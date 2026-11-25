// Phase: perf pass (2026-11-25) - the pool grid's masks (NightLights.PoolGrid.cs) must only ever be too wide.
// TWPoolMask (TWLightPools.hlsl) walks the set bits of a cell's mask and runs the old per-pool cull test inside, so the
// light is bit-identical exactly when no pool whose cull circle reaches a point in a cell is missing from that cell's mask.
// These pin that against a brute-force test of points, over random pools.
using NUnit.Framework;
using TW.Presentation.Terrain;
using UnityEngine;

namespace TW.Tests
{
    public sealed class PoolGridMaskTests
    {
        const float Cell = 8f, Ox = -24f, Oz = -24f;
        const int Gw = 40, Gh = 60;

        static Vector4[] RandomPools(System.Random rng, int n)
        {
            var pools = new Vector4[NightLights.MaxPools];
            for (int k = 0; k < n; k++)
                pools[k] = new Vector4((float)(rng.NextDouble() * 360.0 - 20.0), (float)(rng.NextDouble() * 12.0), (float)(rng.NextDouble() * 520.0 - 20.0), 1f + (float)(rng.NextDouble() * 19.0));
            return pools;
        }

        [Test]
        public void A_Mask_Holds_Every_Pool_Whose_Cull_Circle_Touches_A_Point_Of_Its_Cell()
        {
            var rng = new System.Random(2611);
            var cells = new uint[Gw * Gh];
            for (int round = 0; round < 40; round++)
            {
                int n = round == 0 ? NightLights.MaxPools : rng.Next(0, NightLights.MaxPools + 1);
                var pools = RandomPools(rng, n);
                NightLights.BuildPoolMasks(pools, n, cells, Gw, Gh, Ox, Oz, Cell, NightLights.PoolMaskReachScale, NightLights.PoolMaskMargin);
                for (int z = 0; z < Gh; z++)
                for (int x = 0; x < Gw; x++)
                {
                    uint mask = cells[z * Gw + x];
                    // corners, edges and interior points of the cell, plus random ones
                    for (int s = 0; s < 40; s++)
                    {
                        float fx = s < 9 ? (s % 3) * 0.5f : (float)rng.NextDouble(), fz = s < 9 ? (s / 3) * 0.5f : (float)rng.NextDouble();
                        float wx = Ox + (x + fx) * Cell, wz = Oz + (z + fz) * Cell;
                        for (int k = 0; k < n; k++)
                        {
                            float dx = pools[k].x - wx, dz = pools[k].z - wz, cull = pools[k].w * NightLights.PoolMaskReachScale;
                            if (dx * dx + dz * dz < cull * cull)   // the shader's cull would keep this pool (3D distance is only larger)
                                Assert.IsTrue((mask & (1u << k)) != 0u, $"pool {k} reaches ({wx:F2},{wz:F2}) but is missing from cell ({x},{z}) in round {round}");
                        }
                    }
                }
            }
        }

        [Test]
        public void A_Mask_Is_Not_Everything_So_The_Grid_Can_Skip_Pools()
        {
            var cells = new uint[Gw * Gh];
            var pools = new Vector4[NightLights.MaxPools];
            pools[0] = new Vector4(20f, 0f, 20f, 6.5f);
            NightLights.BuildPoolMasks(pools, 1, cells, Gw, Gh, Ox, Oz, Cell, NightLights.PoolMaskReachScale, NightLights.PoolMaskMargin);
            Assert.AreNotEqual(0u, cells[(int)((20f - Oz) / Cell) * Gw + (int)((20f - Ox) / Cell)], "the cell under the pool has it");
            Assert.AreEqual(0u, cells[(Gh - 1) * Gw + Gw - 1], "a far corner does not");
        }

        [Test]
        public void No_Bit_At_Or_Above_The_Count_Is_Set()
        {
            var rng = new System.Random(7);
            var cells = new uint[Gw * Gh];
            var pools = RandomPools(rng, NightLights.MaxPools);
            for (int n = 0; n <= NightLights.MaxPools; n++)
            {
                NightLights.BuildPoolMasks(pools, n, cells, Gw, Gh, Ox, Oz, Cell, NightLights.PoolMaskReachScale, NightLights.PoolMaskMargin);
                uint allowed = n >= 32 ? 0xFFFFFFFFu : (1u << n) - 1u;
                for (int i = 0; i < cells.Length; i++) Assert.AreEqual(0u, cells[i] & ~allowed, $"count {n}: a bit past the count in cell {i}");
            }
        }

        [Test]
        public void A_Pool_That_Is_Not_A_Number_Is_In_Every_Cell_And_A_Huge_One_Too()
        {
            var cells = new uint[Gw * Gh];
            var pools = new Vector4[NightLights.MaxPools];
            pools[0] = new Vector4(float.NaN, 0f, 5f, 6.5f);
            pools[1] = new Vector4(5f, 0f, 5f, float.PositiveInfinity);
            pools[2] = new Vector4(5f, 0f, 5f, float.NaN);
            NightLights.BuildPoolMasks(pools, 3, cells, Gw, Gh, Ox, Oz, Cell, NightLights.PoolMaskReachScale, NightLights.PoolMaskMargin);
            for (int i = 0; i < cells.Length; i++) Assert.AreEqual(7u, cells[i], $"cell {i}");
        }

        [Test]
        public void A_Pool_Far_Outside_The_Grid_Sets_Nothing_And_Does_Not_Throw()
        {
            var cells = new uint[Gw * Gh];
            var pools = new Vector4[NightLights.MaxPools];
            pools[0] = new Vector4(-5000f, 0f, 9000f, 20f);
            NightLights.BuildPoolMasks(pools, 1, cells, Gw, Gh, Ox, Oz, Cell, NightLights.PoolMaskReachScale, NightLights.PoolMaskMargin);
            for (int i = 0; i < cells.Length; i++) Assert.AreEqual(0u, cells[i]);
        }
    }
}
