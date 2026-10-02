// Phase: B2 (presentation-only crater hollows and rill drainage; AOSA card C40)
// BattlefieldSurface.RefreshHollows runs after every salvo. C40 made it cheaper without changing a bit of what it finds:
// the strict-minimum test reads whole centimetres before BankDistance, a pool's stamp visits only its own square, and
// the drainage re-derives ground and receivers only where heights changed. These tests keep the pre-C40 scan and
// stamps, verbatim, as the oracle for the hollows and every cell's nearest hollow, and a whole re-derivation
// (ForgetRescan) as the oracle for the drainage, crater after crater on the bench's own map.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim.Terrain;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class HollowRescanTests
    {
        /// <summary>The pre-C40 RefreshHollows scan and pool stamps, unchanged but for reading the surface through its
        /// public members.</summary>
        static List<BattlefieldSurface.Hollow> OldScan(MapData map, BattlefieldSurface surface, out int[] nearHollow)
        {
            var hollows = new List<BattlefieldSurface.Hollow>();
            var hf = map.Height; int width = hf.Width, length = hf.Length;
            nearHollow = new int[width * length];
            for (int i = 0; i < nearHollow.Length; i++) nearHollow[i] = -1;
            for (int z = 3; z < length - 3; z++)
            for (int x = 3; x < width - 3; x++)
            {
                float wx = x + .5f, wz = z + .5f, h = hf.HeightAtCell(x, z);
                if (surface.BankDistance(wx, wz) < 4f) continue;
                bool minimum = true;
                for (int dz = -1; dz <= 1; dz++) for (int dx = -1; dx <= 1; dx++)
                    if ((dx != 0 || dz != 0) && hf.HeightAtCell(x + dx, z + dz) < h + .005f) minimum = false;
                if (!minimum) continue;
                float ring = (hf.Sample(wx - 5f, wz) + hf.Sample(wx + 5f, wz) + hf.Sample(wx, wz - 5f) + hf.Sample(wx, wz + 5f)) * .25f;
                float depth = ring - h; if (depth < .42f) continue;
                bool overlap = false;
                foreach (var other in hollows) if (Vector2.Distance(other.Center, new Vector2(wx, wz)) < 3f) overlap = true;
                if (overlap) continue;
                float radius = 1.5f;
                for (; radius < 5.5f; radius += .25f)
                {
                    float shoulder = (hf.Sample(wx - radius, wz) + hf.Sample(wx + radius, wz) + hf.Sample(wx, wz - radius) + hf.Sample(wx, wz + radius)) * .25f;
                    if (shoulder - h > depth * .86f) break;
                }
                int mark = hollows.Count;
                uint hash = (uint)x * 0x9E3779B1u ^ (uint)z * 0x85EBCA77u; hash ^= hash >> 15; hash *= 0x2C1B3C6Du; hash ^= hash >> 12;
                float roll = (hash & 0xFFFF) / 65535f, fill = .40f + .28f * ((hash >> 16) & 0xFF) / 255f;
                hollows.Add(new BattlefieldSurface.Hollow { Center = new Vector2(wx, wz), Radius = radius, Depth = depth, Level = roll < surface.Flooding ? h + depth * fill : -1000f });
                for (int zz = Mathf.Max(0, z - 8); zz <= Mathf.Min(length - 1, z + 8); zz++)
                for (int xx = Mathf.Max(0, x - 8); xx <= Mathf.Min(width - 1, x + 8); xx++)
                {
                    int i = zz * width + xx;
                    float distance = Vector2.Distance(new Vector2(xx + .5f, zz + .5f), new Vector2(wx, wz));
                    if (distance > radius + 1.5f) continue;
                    if (nearHollow[i] < 0 || distance < Vector2.Distance(new Vector2(xx + .5f, zz + .5f), hollows[nearHollow[i]].Center)) nearHollow[i] = mark;
                }
            }
            return hollows;
        }

        static int Bits(float f) => System.BitConverter.ToInt32(System.BitConverter.GetBytes(f), 0);

        static void Stamp(MapData map, float x, float z, float radius, float depth)
            => new CraterStamp { Center = new float3(x, 0f, z), Radius = radius, Depth = depth }.Apply(map);

        /// <summary>`surface` (rescanned as the view does it) against the old scan, and its drainage against `whole`'s,
        /// which re-derived every cell. Returns how many cells carry a rill.</summary>
        static int AssertSame(MapData map, BattlefieldSurface surface, BattlefieldSurface whole, string what)
        {
            int width = map.Height.Width, length = map.Height.Length;
            var expected = OldScan(map, surface, out var near);
            Assert.AreEqual(expected.Count, surface.Hollows.Count, $"{what}: hollow count");
            for (int i = 0; i < expected.Count; i++)
            {
                var a = expected[i]; var b = surface.Hollows[i];
                Assert.IsTrue(Bits(a.Center.x) == Bits(b.Center.x) && Bits(a.Center.y) == Bits(b.Center.y) && Bits(a.Radius) == Bits(b.Radius)
                    && Bits(a.Depth) == Bits(b.Depth) && Bits(a.Level) == Bits(b.Level), $"{what}: hollow {i}");
            }
            int nearDiffer = 0, drainDiffer = 0, rills = 0, first = -1;
            for (int z = 0; z < length; z++)
            for (int x = 0; x < width; x++)
            {
                if (surface.At(x + .5f, z + .5f).Hollow != near[z * width + x]) nearDiffer++;
                if (Bits(surface.DrainedAt(x, z)) != Bits(whole.DrainedAt(x, z))) { drainDiffer++; if (first < 0) first = z * width + x; }
                if (surface.DrainedAt(x, z) > 0f) rills++;
            }
            Assert.AreEqual(0, nearDiffer, $"{what}: {nearDiffer} cells differ in their nearest hollow from the old scan");
            Assert.AreEqual(0, drainDiffer, $"{what}: {drainDiffer} cells differ in drainage from a whole re-derivation" +
                (first >= 0 ? $", first at {first % width},{first / width}" : ""));
            return rills;
        }

        [Test]
        public void RefreshHollows_CraterAfterCrater_EqualsTheOldScanAndAWholeDrainage()
        {
            // the bench's battlefield (seed 1917), shelled before the battle
            using var map = BattlefieldGenerator.Create(BattlefieldParams.ShelledForest(1917u), Allocator.Persistent);
            int width = map.Height.Width, length = map.Height.Length;
            var rng = new System.Random(40);
            float Next() => (float)rng.NextDouble();
            for (int k = 0; k < 150; k++) { float r = 2f + Next() * 2.4f; Stamp(map, 3f + Next() * (width - 6f), 20f + Next() * (map.SeaStartZ - 40f), r, r * .36f); }
            var surface = new BattlefieldSurface(map, .78f);   // rescanned only where heights changed, as the view does it
            var whole = new BattlefieldSurface(map, .78f);     // the same ground and mounds, re-derived whole every time
            var before = new float[width * length];
            for (int i = 0; i < before.Length; i++) before[i] = surface.DrainedAt(i % width, i / width);
            Assert.Greater(surface.Hollows.Count, 20, "the shelled map should hold many hollows");
            Assert.Greater(AssertSame(map, surface, whole, "at the start"), 0, "no rill anywhere: the drainage did not run");

            // the barrage scenario: 12 HE shells (3 m, 1.2 m deep) in 25 m round a cell of each front trench; one a
            // rescan, and every fourth round a whole barrage between two rescans
            var targets = new float2[2];
            for (int t = 1; t <= 2; t++)
            {
                var trench = map.Trenches[t]; int cell = map.TrenchCells[trench.CellStart + trench.CellCount / 2];
                targets[t - 1] = new float2((cell % map.NavWidth + .5f) * MapData.NavCellSize, (cell / map.NavWidth + .5f) * MapData.NavCellSize);
            }
            for (int round = 0; round < 16; round++)
            {
                int shells = round % 4 == 3 ? 12 : 1;
                for (int k = 0; k < shells; k++)
                {
                    var c = targets[(round + k) % 2]; float angle = Next() * 6.2831853f, reach = Mathf.Sqrt(Next()) * 25f;
                    Stamp(map, Mathf.Clamp(c.x + Mathf.Cos(angle) * reach, 1f, width - 1f), c.y + Mathf.Sin(angle) * reach, 3f, 1.2f);
                }
                surface.RefreshHollows();
                whole.ForgetRescan(); whole.RefreshHollows();
                AssertSame(map, surface, whole, $"round {round}");
            }

            // the edges, the corners, a trench (which keeps its floor), and a rescan with nothing changed
            Stamp(map, .2f, 30f, 3f, 1.2f); Stamp(map, width - .3f, 100f, 4f, 1.5f); Stamp(map, 1f, 1f, 3f, 1.2f);
            Stamp(map, width - 1f, length - 1f, 3f, 1.2f); Stamp(map, width * .5f, length - 2f, 5f, 2f);
            { var trench = map.Trenches[1]; int cell = map.TrenchCells[trench.CellStart + 3]; Stamp(map, (cell % map.NavWidth + .5f) * MapData.NavCellSize, (cell / map.NavWidth + .5f) * MapData.NavCellSize, 3f, 1.2f); }
            surface.RefreshHollows(); whole.ForgetRescan(); whole.RefreshHollows();
            AssertSame(map, surface, whole, "edges and a trench");
            surface.RefreshHollows(); whole.ForgetRescan(); whole.RefreshHollows();
            AssertSame(map, surface, whole, "a rescan over unchanged ground");

            int moved = 0;
            for (int i = 0; i < before.Length; i++) if (Bits(before[i]) != Bits(surface.DrainedAt(i % width, i / width))) moved++;
            Assert.Greater(moved, 0, "the craters moved no water, so the drainage half of the test proves little");
        }
    }
}
