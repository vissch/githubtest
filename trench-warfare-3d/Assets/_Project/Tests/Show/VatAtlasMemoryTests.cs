// Phase: tooling (2026-09-23) — the VAT atlases are the largest thing in memory by an order of magnitude (docs/05:
// about 144 MB against a 256 MB budget, where the next biggest item is 34.9 MB). This file guards their lifetime and
// records their size, because the first measurement of them raised two separate questions and only one of the two had
// an answer in the source.
//
// The question this file answers. "Four position atlases were resident where there should have been two, which
// suggests they accumulate across play-mode entries." They do, and the reason is in our source rather than in Unity:
// every atlas texture is created with HideFlags.HideAndDontSave, which exempts it from leaving Play mode, from scene
// unload and from Resources.UnloadUnusedAssets alike, and nothing in Presentation/Units destroyed a Texture2D
// anywhere. VATRenderer.OnDestroy freed its GraphicsBuffers and its NativeArrays and left roughly 70 MB of textures
// behind, so a second Start decoded a second set on top of the first and the editor held both until it was quit.
// VatAsset.Release now frees them and VATRenderer.OnDestroy calls it; these tests fail if either goes away.
//
// The question it does NOT answer, and deliberately does not assert on. The same measurement reported each texture at
// about twice what its format and dimensions imply, and guessed at "a readable texture keeping a CPU copy beside the
// GPU one". That guess is wrong, and the source says so: VatCodec.Decode already passes makeNoLongerReadable, at
// `pos.Apply(false, !keepReadable)` with keepReadable false. So the doubling is still unexplained. The likeliest
// remaining explanation is that it is an EDITOR artefact — the editor keeps a CPU copy it can re-upload after a
// graphics device reset, whatever the flag says — in which case it does not exist in a build at all. Until somebody
// measures a build, this file asserts the size the format implies and merely PRINTS anything else, so that an open
// question cannot masquerade as a green test.
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.Profiling;
using TW.Presentation.Units;

namespace TW.Tests
{
    public class VatAtlasMemoryTests
    {
        /// <summary>A three-frame, four-vertex atlas: a real decoded VatAsset, small enough to cost nothing.</summary>
        static VatAsset Tiny(out Mesh borrowed, string name = "AtlasLifetime")
        {
            var frames = new Vector3[3][];
            var normals = new Vector3[3][];
            for (int f = 0; f < 3; f++)
            {
                frames[f] = new Vector3[4];
                normals[f] = new Vector3[4];
                for (int i = 0; i < 4; i++)
                {
                    frames[f][i] = new Vector3(i * 0.3f, f * 0.4f, 0.1f * i);
                    normals[f][i] = Vector3.up;
                }
            }
            var bytes = VatCodec.Encode(frames, normals, 4, new[] { new Vector2(0, 3) }, new[] { 0.5f });
            borrowed = new Mesh { name = "borrowed" };
            borrowed.SetVertices(frames[0]);
            return VatCodec.Decode(bytes, borrowed, name);
        }

        [Test]
        public void ReleaseDestroysTheAtlasTexturesBecauseNothingElseEverWill()
        {
            var asset = Tiny(out var borrowed);
            Texture2D pos = asset.Positions, nrm = asset.Normals;
            Assert.IsFalse(pos == null, "the decode produced a position texture");
            Assert.IsFalse(nrm == null, "the decode produced a normal texture");

            asset.Release();

            Assert.IsTrue(pos == null,
                "Release must DESTROY the position atlas, not merely drop the reference. Dropping it frees nothing: " +
                "HideFlags.HideAndDontSave means Unity's own cleanup will never take it, so an undestroyed atlas is " +
                "resident until the process exits.");
            Assert.IsTrue(nrm == null, "Release must destroy the normal atlas too.");
            Assert.IsNull(asset.Positions, "Release clears the field so a second call is safe");
            Assert.IsNull(asset.Normals);

            // A decoded figure borrows its mesh from whatever carried the bytes. For a baked figure that is the
            // Resources .asset on disk, and destroying it would empty the file, so Release must leave it alone.
            Assert.IsFalse(borrowed == null,
                "Release destroyed a mesh it does not own. For a baked figure that mesh is the project asset itself, " +
                "so this would have emptied Assets/_Project/Resources/Units/Figure<name>Mesh.asset.");
            Assert.DoesNotThrow(() => asset.Release(), "Release is safe to call twice");
            Object.DestroyImmediate(borrowed);
        }

        [Test]
        public void OnlyTheBoxSoldierOwnsItsMesh()
        {
            var decoded = Tiny(out var borrowed);
            Assert.IsFalse(decoded.OwnsMesh, "a decoded atlas borrows its mesh from whoever supplied it");
            decoded.Release();
            Object.DestroyImmediate(borrowed);

            var box = ProceduralSoldier.Build();
            Assert.IsTrue(box.OwnsMesh, "the box soldier builds its own mesh at runtime, so it must destroy it");
            Mesh mine = box.Mesh;
            box.Release();
            Assert.IsTrue(mine == null, "the box soldier's mesh is HideAndDontSave too, so Release must destroy it");
        }

        [Test]
        public void AtlasTexturesAreExemptFromUnitysOwnCleanup()
        {
            var asset = Tiny(out var borrowed);
            // This is the whole reason Release has to exist. If a future change drops DontSave, Unity would collect
            // these on scene unload and the explicit teardown would merely be belt and braces — but while the flag is
            // on them, an atlas nobody destroys is an atlas nobody can ever reclaim.
            Assert.AreNotEqual(HideFlags.None, asset.Positions.hideFlags & HideFlags.DontSave,
                "the position atlas no longer carries DontSave; re-read VatAsset.Release's reason for existing.");
            Assert.AreNotEqual(HideFlags.None, asset.Normals.hideFlags & HideFlags.DontSave);
            asset.Release();
            Object.DestroyImmediate(borrowed);
        }

        [Test]
        public void EachFigureGetsAnAtlasNamedAfterItself()
        {
            var a = Tiny(out var ma, "FigureSoldier");
            var b = Tiny(out var mb, "FigureSniper");
            Assert.AreNotEqual(a.Positions.name, b.Positions.name,
                "two figures' atlases share a name, so a memory listing cannot tell them apart. That ambiguity is " +
                "what made \"four position atlases where there should be two\" take a day to read (docs/05).");
            StringAssert.Contains("FigureSoldier", a.Positions.name);
            a.Release();
            b.Release();
            Object.DestroyImmediate(ma);
            Object.DestroyImmediate(mb);
        }

        /// <summary>
        /// What one resident set of baked atlases costs, read from the bake's own metadata rather than from a decode,
        /// so this stays a few milliseconds. Positions are RGBA64 (8 bytes a texel) and normals RGBA32 (4), so the
        /// cost is 12 bytes per vertex per frame. docs/05 budgets 256 MB for the atlases; the point of a number here
        /// is that re-baking at twice the frame rate, or adding two more figures, fails loudly instead of quietly
        /// eating the budget.
        /// </summary>
        [Test]
        public void OneResidentSetOfBakedAtlasesFitsTheBudget()
        {
            const long BudgetBytes = 128L * 1024 * 1024;
            long total = 0;
            var lines = new List<string>();
            foreach (var figure in VATRenderer.FigureNames)
            {
                var data = Resources.Load<VatAssetData>("Units/Figure" + figure);
                if (data == null) continue;
                long bytes = (long)data.VertexCount * data.TotalFrames * 12L;
                total += bytes;
                lines.Add($"{figure}: {data.VertexCount} verts x {data.TotalFrames} frames, {data.Rows} rows = " +
                          $"{bytes / 1048576.0:0.0} MB");
            }
            if (lines.Count == 0) Assert.Ignore("no bake in Resources/Units: run TW/VAT/Bake Infantry");

            TestContext.WriteLine(string.Join("\n", lines));
            TestContext.WriteLine($"one resident set: {total / 1048576.0:0.0} MB by format arithmetic, against a " +
                                  $"{BudgetBytes / 1048576} MB budget. Leaking one extra set doubles it.");

            // Printed, never asserted: the profiler reports about twice the arithmetic for these textures and nobody
            // has accounted for it yet (see this file's header). A test must not go green on a number it cannot
            // explain, so the mesh is shown here only as a sanity check that the profiler is being read correctly.
            var probe = Resources.Load<VatAssetData>("Units/Figure" + VATRenderer.FigureNames[0]);
            if (probe != null && probe.Mesh != null)
                TestContext.WriteLine($"for comparison, the profiler puts the bake's MESH at " +
                                      $"{Profiler.GetRuntimeMemorySizeLong(probe.Mesh) / 1048576.0:0.00} MB");

            Assert.That(total, Is.LessThan(BudgetBytes),
                $"the baked atlases now need {total / 1048576.0:0.0} MB resident, over the {BudgetBytes / 1048576} MB " +
                "this test holds them to. Either the bake grew (more frames, more vertices, another figure) or a " +
                "figure stopped sharing one. Do not raise the budget without a build measurement.");
        }
    }
}
