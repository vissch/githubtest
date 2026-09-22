// Phase: B3 (implemented), C1 (clip atlas)
// The atlas contract VATRenderer draws by: one texel column per vertex, one texel row per frame, a row table entry per
// row (positive count loops, negative holds), rows stacked without gaps; the box soldier has a row per AnimRow, the
// baked infantry a row per controller Clip, and the codec round-trips exactly what the baker wrote.
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Presentation;
using TW.Presentation.Units;

namespace TW.Tests
{
    public class VatAssetTests
    {
        static void CheckContract(VatAsset asset, int rows)
        {
            Assert.AreEqual(rows, asset.RowTable.Length);
            Assert.AreEqual(asset.Mesh.vertexCount, asset.Positions.width);
            Assert.AreEqual(asset.TotalFrames, asset.Positions.height);
            Assert.AreEqual(asset.Positions.width, asset.Normals.width);
            Assert.AreEqual(asset.Positions.height, asset.Normals.height);
            Assert.AreEqual(TextureFormat.RGBA64, asset.Positions.format);
            Assert.AreEqual(TextureFormat.RGBA32, asset.Normals.format);
            Assert.AreEqual(rows, asset.RowSeconds.Length);
            float next = 0f;
            for (int r = 0; r < rows; r++)
            {
                var row = asset.RowTable[r];
                Assert.AreEqual(next, row.x, "rows are stacked without gaps, in order");
                Assert.AreNotEqual(0f, row.y);
                Assert.Greater(asset.RowSeconds[r], 0f);
                next = row.x + Mathf.Abs(row.y);
            }
            Assert.AreEqual(asset.TotalFrames, (int)next);
            Assert.Greater(asset.PosSize.y, 1f, "a standing man is in the atlas");
        }

        [Test]
        public void ProceduralSoldier_AtlasMatchesMeshAndAnimRows()
        {
            var asset = ProceduralSoldier.Build();
            CheckContract(asset, (int)AnimRow.Count);
            Assert.IsFalse(asset.ClipAtlas);
            Assert.Less(asset.Mesh.vertexCount, 400, "3,000 of these have to stay under about a million vertices");
        }

        [Test]
        public void BakedFigures_IfPresent_HaveARowPerClipAndHoldTheirOneShots([Values("Soldier", "Sniper")] string figure)
        {
            var data = Resources.Load<VatAssetData>("Units/Figure" + figure);
            if (data == null || !data.Valid) Assert.Ignore("no baked " + figure + " (TW/VAT/Bake Infantry); the renderer falls back to the box soldier");
            var asset = data.ToAsset();
            CheckContract(asset, (int)Clip.Count);
            Assert.IsTrue(asset.ClipAtlas);
            Assert.AreEqual(asset.Mesh.vertexCount, asset.Mesh.colors32.Length, "vertex colours carry albedo and the team mask");
            Assert.Less(asset.Mesh.vertexCount, 1200, "3,000 of these have to stay near three million vertices");
            for (int c = 1; c < (int)Clip.Count; c++)
            {
                bool loop = Clips.Table[c].Loop;
                Assert.AreEqual(loop, asset.Loops(c), ((Clip)c) + " loops in the table and in the bake alike");
            }
            Assert.Less(asset.TotalFrames, 8000, "the atlas stays inside the budget docs/15 section 11 set");
            // the sockets the muzzle flash and the tracer leave from: in front of the man, at the height of his stance
            Assert.AreEqual(3, asset.SocketsPerFrame, "muzzle, barrel and chest are baked with every frame");
            Muzzle(asset, Clip.FireStand, 1.0f, 1.9f);
            Muzzle(asset, Clip.FireKneel, 0.55f, 1.35f);
            Muzzle(asset, Clip.FireProne, 0.0f, 0.65f);
            Assert.IsTrue(asset.Socket((int)Clip.FireStand, 0f, VatAsset.Barrel, out var barrel));
            Assert.Greater(barrel.z, 0.5f, "a man firing points his barrel the way he faces");
            Object.DestroyImmediate(asset.Positions); Object.DestroyImmediate(asset.Normals);
        }

        static void Muzzle(VatAsset asset, Clip clip, float low, float high)
        {
            Assert.IsTrue(asset.Socket((int)clip, 0f, VatAsset.Muzzle, out var m), clip + " has a muzzle");
            Assert.Greater(m.z, 0.35f, clip + ": the muzzle is out in front of him (" + m + ")");
            Assert.That(m.y, Is.InRange(low, high), clip + ": the muzzle is at the height of the stance (" + m + ")");
        }

        [Test]
        public void Codec_RoundTripsFramesWithinQuantisation()
        {
            var frames = new Vector3[3][]; var normals = new Vector3[3][];
            for (int f = 0; f < 3; f++)
            {
                frames[f] = new Vector3[4]; normals[f] = new Vector3[4];
                for (int i = 0; i < 4; i++) { frames[f][i] = new Vector3(i * 0.3f - 0.5f, f * 0.4f + i * 0.1f, 0.2f * i); normals[f][i] = new Vector3(0.6f, 0.8f, 0f); }
            }
            var table = new[] { new Vector2(0, 2), new Vector2(2, -1) };
            var sockets = new Vector3[3][];
            for (int f = 0; f < 3; f++) sockets[f] = new[] { new Vector3(0f, 1f + f, 0.8f), Vector3.forward, new Vector3(0f, 1.3f, f * 0.1f) };
            var bytes = VatCodec.Encode(frames, normals, 4, table, new[] { 0.5f, 0.1f }, sockets);
            var mesh = new Mesh(); mesh.SetVertices(frames[0]);
            var asset = VatCodec.Decode(bytes, mesh, "Test", true);
            Assert.AreEqual(3, asset.TotalFrames); Assert.AreEqual(2, asset.RowTable.Length);
            Assert.IsTrue(asset.Loops(0)); Assert.IsFalse(asset.Loops(1)); Assert.AreEqual(1, asset.Frames(1));
            Assert.AreEqual(0.5f, asset.RowSeconds[0], 1e-6f);
            var px = asset.Positions.GetPixelData<ushort>(0);
            for (int f = 0; f < 3; f++)
                for (int i = 0; i < 4; i++)
                {
                    int k = (f * 4 + i) * 4;
                    var p = asset.PosMin + new Vector3(px[k] / 65535f * asset.PosSize.x, px[k + 1] / 65535f * asset.PosSize.y, px[k + 2] / 65535f * asset.PosSize.z);
                    Assert.Less((p - frames[f][i]).magnitude, 1e-4f, "positions come back to within a tenth of a millimetre");
                }
            // sockets: sampled like the shader samples frames (row 0 loops over frames 0 and 1)
            Assert.AreEqual(3, asset.SocketsPerFrame);
            Assert.IsTrue(asset.Socket(0, 0.25f, VatAsset.Muzzle, out var m));
            Assert.AreEqual(1.5f, m.y, 1e-5f, "half way from frame 0 to frame 1");
            Assert.IsTrue(asset.Socket(1, 1f, VatAsset.Chest, out var c));
            Assert.AreEqual(0.2f, c.z, 1e-5f, "a held row sits on its only frame");
            Object.DestroyImmediate(asset.Positions); Object.DestroyImmediate(asset.Normals); Object.DestroyImmediate(mesh);
        }
    }
}
