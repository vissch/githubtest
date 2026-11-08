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
        // (docs/evidence/rocketed-man-planted-g2/probe.py). The whole silhouette above the mud is NOT that shape:
        // 0.916 m tall, 1.202 m wide. LegsAboveTheMud_AreAColumn below takes the body apart and says where the
        // width comes from (measured 2026-11-08, printed by the test):
        //     legs  (limbs 4+5) 0.916 m tall, 0.826 m wide, 271 vertices -- a column, as specified
        //     arms  (limbs 2+3) 0.178 m tall, 1.013 m wide,  88 vertices -- flung out, lying flat ON the mud line
        //     body  (limb 0)    0.273 m tall, 0.820 m wide,  81 vertices -- his pelvis, flat at the mud line too
        //     rifle (last 24)   0.434 m tall, 0.114 m wide,  12 vertices -- barely out of the mud, not the cause
        // So his legs do stand up; what breaks the read is his ARMS and pelvis spread 1.2 m across the mud at his
        // waist, where the mud line falls by construction when his legs stand DeathGags.PlantLegsAboveM out of it.
        // No frozen phase and no baked clip fixes that: swept over all 112 baked rows x 20 phases, the best
        // height/width above the mud is Burning at t 0.10 (1.090 / 0.957 = 1.14, but 1.09 m tall breaks the 0.905 m
        // spec) and the best within the spec is Burning at t 0.50 (0.955 / 0.879 = 1.09) -- a 9% margin in
        // geometry, before a pixel of blood or mud is drawn. The sweep is in frames.txt. See the open row in
        // docs/reference/decisions.md: every way past this is the owner's (a deeper plant, a different frozen
        // pose, or cutting the arms off him), so none is built here.

        [Test]
        public void LegsAboveTheMud_AreAColumn_HisArmsAreWhatLiesFlat()
        {
            var pose = FrozenPose();
            var uv = new System.Collections.Generic.List<Vector2>();
            Resources.Load<VatAssetData>("Units/FigureSoldier").Mesh.GetUVs(1, uv);
            int rifleFrom = pose.Length - RifleVerts;   // the bake appends the rifle box last (VATBaker.Box, 24 vertices)
            var legs = Of(pose, i => i < rifleFrom && (Mathf.RoundToInt(uv[i].x) == 4 || Mathf.RoundToInt(uv[i].x) == 5));
            var arms = Of(pose, i => i < rifleFrom && (Mathf.RoundToInt(uv[i].x) == 2 || Mathf.RoundToInt(uv[i].x) == 3));
            var rifle = Of(pose, i => i >= rifleFrom);
            TestContext.WriteLine("legs above the mud:  " + legs);
            TestContext.WriteLine("arms above the mud:  " + arms);
            TestContext.WriteLine("rifle above the mud: " + rifle);
            Assert.Greater(legs.Above, 0, "his legs are above the mud at all");
            Assert.Greater(legs.Height, legs.Width, "his legs alone read as a column: " + legs);
            Assert.Greater(arms.Width, legs.Width, "and his arms, not his rifle, are the wide part: " + arms);
            // The assertion for whoever buries or tucks the arms, written out rather than run (a red test fails
            // the gate), and the thing the probe's pixels measure:
            //     var all = Of(pose, i => true);
            //     Assert.Greater(all.Height, all.Width, "the whole planted man reads as a column: " + all);
        }

        /// <summary>The rifle box VATBaker appends after the skin and the helmet: its 24 vertices are the bake's last.</summary>
        const int RifleVerts = 24;

        struct Box
        {
            public float Height, Width, X, Z; public int Above, Total;
            public override string ToString() => "height " + Height.ToString("F3") + " m, width " + Width.ToString("F3")
                + " m (x " + X.ToString("F3") + ", z " + Z.ToString("F3") + "), " + Above + " of " + Total + " vertices above the mud";
        }

        /// <summary>The box above the mud of the vertices a test picks out (a limb, the rifle), in drawn metres.</summary>
        static Box Of(Vector3[] pose, System.Func<int, bool> pick)
        {
            float sink = VatTint.UpsideDownHeight(0f, 0, Scale) - DeathGags.PlantLegsAboveM;
            float hi = float.MinValue, xLo = float.MaxValue, xHi = float.MinValue, zLo = float.MaxValue, zHi = float.MinValue;
            int above = 0;
            for (int i = 0; i < pose.Length; i++)
            {
                if (!pick(i)) continue;
                float h = VatTint.UpsideDownHeight(pose[i].y, 0, Scale) - sink;
                if (h <= 0f) continue;
                above++;
                if (h > hi) hi = h;
                float x = pose[i].x * Scale, z = pose[i].z * Scale;
                if (x < xLo) xLo = x; if (x > xHi) xHi = x; if (z < zLo) zLo = z; if (z > zHi) zHi = z;
            }
            var b = new Box { Height = above > 0 ? hi : 0f, X = above > 0 ? xHi - xLo : 0f, Z = above > 0 ? zHi - zLo : 0f, Above = above, Total = pose.Length };
            b.Width = Mathf.Max(b.X, b.Z);
            return b;
        }

        /// <summary>The frozen planted pose (row Clip.DeathThrown at the phase a Freeze gag holds), in object space.</summary>
        Vector3[] FrozenPose()
        {
            var data = Resources.Load<VatAssetData>("Units/FigureSoldier");
            if (data == null || !data.Valid) Assert.Ignore("no baked Soldier (TW/VAT/Bake Infantry); nothing to measure");
            // a second, readable copy: the renderer's own atlas is uploaded and GetPixelData throws on it
            asset = VatCodec.Decode(data.Atlas.bytes, data.Mesh, "plantedpose", keepReadable: true);
            Assert.IsTrue(asset.ClipAtlas, "the bake has a row per clip");
            return Sample(asset, (int)Clip.DeathThrown, Frozen);
        }

        /// <summary>The frozen planted pose, as drawn, measured above the mud: his whole silhouette.</summary>
        Box Measure()
        {
            var box = Of(FrozenPose(), i => true);
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
