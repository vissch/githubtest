// Phase: B2 (presentation-only rill drainage; AOSA card C34)
// BattlefieldSurface.BuildDrainage runs on every hollow rescan (every salvo). It used to sort the whole map by height
// and walk it from the top down; it now finds each cell's receiver in index order and passes water on in Kahn's order.
// These tests keep the old walk, verbatim, as the oracle and require the same floats, bit for bit.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim.Terrain;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class DrainageTests
    {
        /// <summary>The pre-C34 BuildDrainage accumulation, unchanged: sort by height, walk from the highest cell down.</summary>
        static float[] SortedWalk(float[] ground, int width, int length)
        {
            int n = width * length;
            var order = new int[n]; var water = new float[n];
            for (int i = 0; i < n; i++) { order[i] = i; water[i] = 1f; }
            var keys = (float[])ground.Clone();
            System.Array.Sort(keys, order);   // lowest first; walk it from the top down
            for (int k = n - 1; k >= 0; k--)
            {
                int i = order[k], x = i % width, z = i / width, to = -1; float drop = 0f;
                for (int dz = -1; dz <= 1; dz++)
                for (int dx = -1; dx <= 1; dx++)
                {
                    if (dx == 0 && dz == 0) continue;
                    int xx = x + dx, zz = z + dz; if (xx < 0 || zz < 0 || xx >= width || zz >= length) continue;
                    float fall = (ground[i] - ground[zz * width + xx]) / (dx != 0 && dz != 0 ? 1.414f : 1f);
                    if (fall > drop) { drop = fall; to = zz * width + xx; }
                }
                if (to >= 0) water[to] += water[i];
            }
            return water;
        }

        /// <summary>The pre-C34 catchment-to-watercourse formula, unchanged.</summary>
        static float OldWatercourse(float water) => Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(Mathf.Log(6f), Mathf.Log(90f), Mathf.Log(water)));

        static int Bits(float f) => System.BitConverter.ToInt32(System.BitConverter.GetBytes(f), 0);

        static void AssertSameWater(float[] ground, int width, int length, string what, bool gulliesExpected = false)
        {
            int n = width * length;
            var expected = SortedWalk(ground, width, length);
            var water = new float[n];
            BattlefieldSurface.Accumulate(ground, width, length, water, new int[n], new int[n], new int[n]);
            int differ = 0, first = -1, rills = 0, gullies = 0;
            for (int i = 0; i < n; i++)
            {
                if (Bits(water[i]) != Bits(expected[i])) { differ++; if (first < 0) first = i; }
                if (expected[i] > 6f) rills++;
                if (expected[i] >= 90f) gullies++;
            }
            Assert.AreEqual(0, differ, $"{what}: {differ} of {n} cells differ from the sorted walk" +
                (first >= 0 ? $", first at cell {first}: {water[first]} vs {expected[first]}" : ""));
            Assert.Greater(rills, 0, $"{what}: no cell gathered more than 6 cells of water, so the test proves little");
            if (gulliesExpected) Assert.Greater(gullies, 0, $"{what}: no cell gathered 90 cells of water, so the test proves little");
        }

        /// <summary>The shelled wood, with a battle's worth of extra shell holes stamped into no man's land.</summary>
        static MapData ShelledMap(int extraCraters)
        {
            var map = BattlefieldGenerator.Create(BattlefieldParams.ShelledForest(34u), Allocator.Persistent);
            var rng = new System.Random(34);
            float w = map.Height.Width, l = map.Height.Length;
            for (int k = 0; k < extraCraters; k++)
            {
                float radius = 1.3f + (float)rng.NextDouble() * 3.9f;
                var center = new float3(4f + (float)rng.NextDouble() * (w - 8f), 0f, 20f + (float)rng.NextDouble() * (l - 60f));
                new CraterStamp { Center = center, Radius = radius, Depth = radius * .38f }.Apply(map);
            }
            return map;
        }

        [Test]
        public void Accumulate_EqualsTheSortedWalk_OnAShelledBattlefield()
        {
            using var map = ShelledMap(400);
            var hf = map.Height; int width = hf.Width, length = hf.Length, n = width * length;
            var raw = new float[n]; var mounded = new float[n];
            for (int i = 0; i < n; i++)
            {
                int x = i % width, z = i / width;
                raw[i] = hf.HeightAtCell(x, z);                                          // centimetre steps: many exact ties
                mounded[i] = raw[i] + BattlefieldSurface.Mound(x + .5f, z + .5f);       // what the surface drains
            }
            AssertSameWater(raw, width, length, "raw heights");
            AssertSameWater(mounded, width, length, "heights plus mounds");
        }

        [Test]
        public void Accumulate_EqualsTheSortedWalk_OnTerracedAndNoisyGround()
        {
            const int width = 97, length = 61; int n = width * length;
            var rng = new System.Random(7);
            var terraced = new float[n]; var noisy = new float[n];
            for (int i = 0; i < n; i++)
            {
                int x = i % width, z = i / width;
                float slope = 12f - x * .09f - z * .05f + Mathf.Sin(x * .31f) * .8f + Mathf.Cos(z * .23f) * .6f;
                terraced[i] = Mathf.Round(slope * 4f) / 4f;                                // flats and ties everywhere
                noisy[i] = slope + (float)rng.NextDouble() * .3f;
            }
            AssertSameWater(terraced, width, length, "terraced ground");
            AssertSameWater(noisy, width, length, "noisy ground", gulliesExpected: true);
        }

        [Test]
        public void Watercourse_EqualsTheLogFormula_ForEveryWholeCatchment()
        {
            // 300 x 800 cells: the largest map (GreyboxMapGenerator) and so the largest catchment there can be
            for (int water = 1; water <= 240000; water++)
                if (Bits(BattlefieldSurface.Watercourse(water)) != Bits(OldWatercourse(water)))
                    Assert.Fail($"catchment {water}: {BattlefieldSurface.Watercourse(water)} vs {OldWatercourse(water)}");
        }

        [Test]
        public void RefreshHollows_Again_GivesTheSameRillsAndHollows()
        {
            // the scratch arrays are kept between rescans: a second rescan over the same ground must change nothing
            using var map = ShelledMap(200);
            var surface = new BattlefieldSurface(map, .6f);
            int width = map.Height.Width, length = map.Height.Length;
            var rills = new float[width * length];
            float any = 0f;
            for (int z = 0; z < length; z++)
            for (int x = 0; x < width; x++) { rills[z * width + x] = surface.Rill(x + .5f, z + .5f); any = Mathf.Max(any, rills[z * width + x]); }
            var hollows = surface.Hollows.ToArray();
            Assert.Greater(hollows.Length, 20, "the shelled map should hold many hollows");
            Assert.Greater(any, 0f, "no rill anywhere: the drainage did not run");

            surface.RefreshHollows();
            for (int z = 0; z < length; z++)
            for (int x = 0; x < width; x++)
                Assert.AreEqual(Bits(rills[z * width + x]), Bits(surface.Rill(x + .5f, z + .5f)), $"rill at {x},{z}");
            Assert.AreEqual(hollows.Length, surface.Hollows.Count, "hollow count");
            for (int i = 0; i < hollows.Length; i++)
            {
                var a = hollows[i]; var b = surface.Hollows[i];
                Assert.IsTrue(a.Center == b.Center && Bits(a.Radius) == Bits(b.Radius) && Bits(a.Depth) == Bits(b.Depth) && Bits(a.Level) == Bits(b.Level), $"hollow {i}");
            }
        }
    }
}
