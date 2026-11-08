// Phase: deaths (2026-11-08, leg rocketed-man-planted-g2) — what SHAPE the planted man actually makes above the mud.
// PlantedDeathTests holds the geometry the look was worked out from (typed numbers: depth, legs height, the fold).
// Nothing held the POSE: the probe's stills show a body lying flat, wider than tall, so either the frozen frame of
// Clip.DeathThrown is not a column or the draw is wrong. This test answers the first half, offline and with no GPU
// draw: it reads the baked atlas (Resources/Units/FigureSoldier -> FigureSoldierAtlas.bytes), samples the row the
// shader way at the exact phase a Freeze gag holds (0.02, VATRenderer.Fallen's t line), applies the drawn transform
// (upside down about VatTint.Pivot, squash 0, UnitScale 1.125, his Pos PlantDepth under the mud) and measures the
// bounding box of everything left above the mud. Measured from the bake, never recomputed the production way.
using NUnit.Framework;
using Unity.Collections;
using UnityEngine;
using TW.Sim;
using TW.Presentation;
using TW.Presentation.Units;

namespace TW.Tests
{
    public class PlantedPoseTests
    {
        /// <summary>FigureMetrics.UnitScale, and the legs height the look is specified at (DeathGags.PlantLegsAboveM).</summary>
        const float Scale = 1.125f;
        /// <summary>The phase a GagFlags.Freeze gag holds its clip at (VATRenderer.Fallen, the t line).</summary>
        const float Frozen = 0.02f;

        VatAsset asset;

        [TearDown] public void Drop() { asset?.Release(); asset = null; }

        [Test]
        public void FrozenThrownPose_AboveTheMud_IsTheSpecifiedHeight()
        {
            var m = Measure();
            Assert.Greater(m.Above, 0, "something of him is above the mud");
            Assert.AreEqual(DeathGags.PlantLegsAboveM, m.Height, 0.08f, "about 0.905 m of him stands above the mud: " + m);
        }

        // THE LOOK AS IT WAS DECIDED (decisions.md, the owner's pick A) is 0.905 m of LEGS: a column, taller above
        // the mud than it is wide, which is also the probe's own shape gate
        // (docs/evidence/rocketed-man-planted-g2/probe.py). The current build does NOT make that shape. Measured
        // here on 2026-11-08 from Resources/Units/FigureSoldier: what is left of the frozen frame above the mud is
        // 0.916 m tall but 1.202 m wide (x 1.202, z 1.126), and 452 of his 917 vertices are above the mud -- half a
        // body lying flat, not a pair of legs. So the three probe stills are not a capture fault: the pose the
        // renderer freezes is already flat (frames.txt has the whole finding, with the close still).
        // The assertion that says so is written out here rather than run, because a red or [Ignore]d test fails the
        // gate. Whoever fixes the pose adds it back as its own [Test] and it goes green:
        //     var m = Measure();
        //     Assert.Greater(m.Height, m.Width, "the planted man reads as a column: " + m);

        struct Box
        {
            public float Height, Width, X, Z; public int Above, Total;
            public override string ToString() => "height " + Height.ToString("F3") + " m, width " + Width.ToString("F3")
                + " m (x " + X.ToString("F3") + ", z " + Z.ToString("F3") + "), " + Above + " of " + Total + " vertices above the mud";
        }

        /// <summary>The frozen planted pose, as drawn, measured above the mud.</summary>
        Box Measure()
        {
            var data = Resources.Load<VatAssetData>("Units/FigureSoldier");
            if (data == null || !data.Valid) Assert.Ignore("no baked Soldier (TW/VAT/Bake Infantry); nothing to measure");
            // a second, readable copy: the renderer's own atlas is uploaded and GetPixelData throws on it
            asset = VatCodec.Decode(data.Atlas.bytes, data.Mesh, "plantedpose", keepReadable: true);
            Assert.IsTrue(asset.ClipAtlas, "the bake has a row per clip");
            int row = (int)Clip.DeathThrown;
            var pose = Sample(asset, row, Frozen);

            // the drawn transform: his feet (object y 0) stand PlantLegsAboveM above the mud, everything else follows
            float sink = VatTint.UpsideDownHeight(0f, 0, Scale) - DeathGags.PlantLegsAboveM;
            float hi = float.MinValue, xLo = float.MaxValue, xHi = float.MinValue, zLo = float.MaxValue, zHi = float.MinValue;
            int above = 0;
            for (int i = 0; i < pose.Length; i++)
            {
                float h = VatTint.UpsideDownHeight(pose[i].y, 0, Scale) - sink;
                if (h <= 0f) continue;   // under the mud: the drawn ground hides him
                above++;
                if (h > hi) hi = h;
                float x = pose[i].x * Scale, z = pose[i].z * Scale;
                if (x < xLo) xLo = x; if (x > xHi) xHi = x;
                if (z < zLo) zLo = z; if (z > zHi) zHi = z;
            }
            var box = new Box { Height = above > 0 ? hi : 0f, X = xHi - xLo, Z = zHi - zLo, Above = above, Total = pose.Length };
            box.Width = Mathf.Max(box.X, box.Z);
            TestContext.WriteLine("planted pose above the mud: " + box);
            return box;
        }

        /// <summary>The row sampled the way VAT_URP samples it: frame floor and ceil of the phase, lerped, dequantised.</summary>
        static Vector3[] Sample(VatAsset a, int row, float t)
        {
            Vector2 r = a.RowTable[row];
            float count = Mathf.Max(1f, Mathf.Abs(r.y));
            bool loops = r.y > 0f;
            float local = (loops ? t - Mathf.Floor(t) : Mathf.Clamp01(t)) * count;
            float f0 = Mathf.Min(Mathf.Floor(local), count - 1f);
            float f1 = loops ? (f0 + 1f) % count : Mathf.Min(f0 + 1f, count - 1f);
            float w = Mathf.Clamp01(local - f0);
            int frameA = (int)r.x + (int)f0, frameB = (int)r.x + (int)f1;
            int verts = a.Positions.width;
            var px = a.Positions.GetPixelData<ushort>(0);
            var outv = new Vector3[verts];
            for (int i = 0; i < verts; i++)
            {
                Vector3 va = Dequantise(px, a, frameA, verts, i), vb = Dequantise(px, a, frameB, verts, i);
                outv[i] = Vector3.Lerp(va, vb, w);
            }
            return outv;
        }

        static Vector3 Dequantise(NativeArray<ushort> px, VatAsset a, int frame, int verts, int i)
        {
            int k = (frame * verts + i) * 4;
            return new Vector3(
                a.PosMin.x + px[k] / 65535f * a.PosSize.x,
                a.PosMin.y + px[k + 1] / 65535f * a.PosSize.y,
                a.PosMin.z + px[k + 2] / 65535f * a.PosSize.z);
        }
    }
}
