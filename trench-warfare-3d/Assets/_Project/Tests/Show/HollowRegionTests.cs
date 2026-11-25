// Phase: B2 (presentation-only crater hollows; perf lane, regional hollow rescan)
// BattlefieldSurface.RefreshHollowsChanged / RefreshHollowsRegion rescan only the ground a crater reached (plus the
// window a candidate hollow reads) and must leave every hollow, every cell's nearest hollow and every cell's drainage
// exactly as a whole RefreshHollows does. Craters land anywhere, on the map edges and on each other, alone and in
// batches; after each, two surfaces that rescan regionally are held bit-equal against one that scans whole.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim.Terrain;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class HollowRegionTests
    {
        static int Bits(float f) => System.BitConverter.ToInt32(System.BitConverter.GetBytes(f), 0);

        static void Stamp(MapData map, float x, float z, float radius, float depth, bool dynamic)
        {
            var stamp = new CraterStamp { Center = new float3(x, 0f, z), Radius = radius, Depth = depth };
            if (dynamic) stamp.ApplyDynamic(map); else stamp.Apply(map);
        }

        static void AssertSame(MapData map, BattlefieldSurface a, BattlefieldSurface whole, string what)
        {
            int width = map.Height.Width, length = map.Height.Length;
            Assert.AreEqual(whole.Hollows.Count, a.Hollows.Count, $"{what}: hollow count");
            for (int i = 0; i < whole.Hollows.Count; i++)
            {
                var e = whole.Hollows[i]; var g = a.Hollows[i];
                Assert.IsTrue(Bits(e.Center.x) == Bits(g.Center.x) && Bits(e.Center.y) == Bits(g.Center.y) && Bits(e.Radius) == Bits(g.Radius)
                    && Bits(e.Depth) == Bits(g.Depth) && Bits(e.Level) == Bits(g.Level), $"{what}: hollow {i}");
            }
            int near = 0, drain = 0;
            for (int z = 0; z < length; z++)
            for (int x = 0; x < width; x++)
            {
                if (a.At(x + .5f, z + .5f).Hollow != whole.At(x + .5f, z + .5f).Hollow) near++;
                if (Bits(a.DrainedAt(x, z)) != Bits(whole.DrainedAt(x, z))) drain++;
            }
            Assert.AreEqual(0, near, $"{what}: {near} cells differ in their nearest hollow");
            Assert.AreEqual(0, drain, $"{what}: {drain} cells differ in drainage");
        }

        /// <summary>The cell rectangle holding every height that differs between two copies of the height centimetres.</summary>
        static bool Changed(short[] before, NativeArray<short> after, int width, out int x0, out int z0, out int x1, out int z1)
        {
            x0 = z0 = int.MaxValue; x1 = z1 = -1;
            for (int i = 0; i < before.Length; i++)
            {
                if (before[i] == after[i]) continue;
                int x = i % width, z = i / width;
                x0 = Mathf.Min(x0, x); x1 = Mathf.Max(x1, x); z0 = Mathf.Min(z0, z); z1 = Mathf.Max(z1, z);
            }
            return x1 >= 0;
        }

        [Test]
        public void RegionalRescan_CratersAnywhere_EqualsTheWholeScan()
        {
            using var map = BattlefieldGenerator.Create(BattlefieldParams.ShelledForest(1917u), Allocator.Persistent);
            int width = map.Height.Width, length = map.Height.Length;
            var rng = new System.Random(2511);
            float Next() => (float)rng.NextDouble();
            for (int k = 0; k < 120; k++) { float r = 2f + Next() * 2.4f; Stamp(map, 3f + Next() * (width - 6f), 20f + Next() * (map.SeaStartZ - 40f), r, r * .36f, false); }
            var changed = new BattlefieldSurface(map, .78f);   // RefreshHollowsChanged: finds the area itself
            var region = new BattlefieldSurface(map, .78f);    // RefreshHollowsRegion: the area handed in
            var whole = new BattlefieldSurface(map, .78f);     // RefreshHollows: everything, every time
            Assert.Greater(whole.Hollows.Count, 20, "the shelled map should hold many hollows");
            AssertSame(map, changed, whole, "at the start");
            AssertSame(map, region, whole, "at the start (region)");

            int wholeCells = (width - 6) * (length - 6), smallest = int.MaxValue, regionalRounds = 0;
            var before = new short[width * length];
            for (int round = 0; round < 70; round++)
            {
                map.Height.Cm.CopyTo(before);
                // one crater, a batch of three between two rescans, or a crater on top of an earlier one
                int shells = round % 5 == 4 ? 3 : 1;
                for (int k = 0; k < shells; k++)
                {
                    float r = 1.5f + Next() * 5f, x = Next() * width, z = Next() * length;
                    switch (round % 7)
                    {
                        case 0: x = Next() * 2f; break;                          // west edge
                        case 1: x = width - Next() * 2f; break;                  // east edge
                        case 2: z = Next() * 2f; break;                          // south edge
                        case 3: z = length - Next() * 2f; break;                 // north edge
                        case 4: x = round < 35 ? 1f : width - 1f; z = round < 35 ? 1f : length - 1f; break;   // corners
                    }
                    if (round % 3 == 2 && whole.Hollows.Count > 0)               // overlapping a hollow that is already there
                    {
                        var h = whole.Hollows[(int)(Next() * whole.Hollows.Count)];
                        x = h.Center.x + (Next() - .5f) * 4f; z = h.Center.y + (Next() - .5f) * 4f;
                    }
                    Stamp(map, Mathf.Clamp(x, 0f, width), Mathf.Clamp(z, 0f, length), r, r * (.25f + Next() * .3f), round % 2 == 0);
                }
                bool any = Changed(before, map.Height.Cm, width, out int x0, out int z0, out int x1, out int z1);
                if (!any) { x0 = z0 = x1 = z1 = 0; }
                changed.RefreshHollowsChanged();
                region.RefreshHollowsRegion(x0, z0, x1, z1);
                whole.RefreshHollows();
                AssertSame(map, changed, whole, $"round {round} (changed)");
                AssertSame(map, region, whole, $"round {round} (region)");
                Assert.AreEqual(wholeCells, whole.HollowCellsScanned, "the whole scan visits every cell");
                if (changed.HollowCellsScanned < wholeCells) regionalRounds++;
                smallest = Mathf.Min(smallest, changed.HollowCellsScanned);
            }
            // a rescan over unchanged ground, and another after it
            changed.RefreshHollowsChanged(); region.RefreshHollowsRegion(5, 5, 5, 5); whole.RefreshHollows();
            AssertSame(map, changed, whole, "unchanged ground (changed)");
            AssertSame(map, region, whole, "unchanged ground (region)");
            Assert.AreEqual(0, changed.HollowCellsScanned, "nothing changed: nothing to rescan");
            Assert.Greater(regionalRounds, 50, "most rescans should have been regional");
            Assert.Less(smallest, wholeCells / 10, "a single crater should rescan a small part of the map");
        }
    }
}
