// Phase: B7 (docs/21 phase 2) — the wire belt as BattlefieldComposer.Build actually lays it: every cell with the
// Wire bit gets a frame drawn in it, and a frame sits at its stand's own height (up on the ground, knocked down at
// +.15, flat at +.05). Counted the way AssetScaleReport counts what the composer emits. Review call B3, second
// reading: the earlier [B3] test only classified keys through StandOf, so it could not see a cell Build skips.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim.Terrain;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public sealed class WireBeltTests
    {
        BattlefieldKit kit;

        [OneTimeSetUp] public void BuildKit() => kit = new BattlefieldKit();
        [OneTimeTearDown] public void Tear() { kit?.Dispose(); kit = null; }

        const int BeltZ0 = 12, BeltZ1 = 17, BeltX0 = 2, BeltX1 = 27;

        static MapData BeltMap()
        {
            var map = new MapData(99, new float2(60f, 80f), Allocator.Temp);
            // tilt the ground so a height offset cannot be read off a flat zero
            for (int z = 0; z < map.Height.Length; z++)
            for (int x = 0; x < map.Height.Width; x++)
                map.Height.Cm[map.Height.Index(x, z)] = (short)(x * 4 + z * 3);
            for (int z = BeltZ0; z <= BeltZ1; z++)
            for (int x = BeltX0; x <= BeltX1; x++)
                map.NavLayers[map.NavIndex(x, z)] |= (byte)NavLayer.Wire;
            map.RebuildCost();
            return map;
        }

        static List<(BattlefieldKit.Module m, Matrix4x4 t)> Compose(BattlefieldKit kit, MapData map)
        {
            var emits = new List<(BattlefieldKit.Module, Matrix4x4)>();
            var surface = new BattlefieldSurface(map);
            new BattlefieldComposer(kit, 1917).Build(map, surface, (m, t) => emits.Add((m, t)));
            return emits;
        }

        static bool Cell(MapData map, Vector4 p, out int cx, out int cz)
        {
            cx = (int)math.floor(p.x / MapData.NavCellSize);
            cz = (int)math.floor(p.z / MapData.NavCellSize);
            return cx >= 0 && cz >= 0 && cx < map.NavWidth && cz < map.NavLength;
        }

        [Test, Category("Long")]
        public void EveryWireCellGetsAFrameFromBuild()
        {
            using var map = BeltMap();
            var emits = Compose(kit, map);

            var frames = new Dictionary<int, int>();
            int cells = 0;
            for (int z = 0; z < map.NavLength; z++)
            for (int x = 0; x < map.NavWidth; x++)
                if ((map.NavLayers[map.NavIndex(x, z)] & (byte)NavLayer.Wire) != 0) { frames[z * map.NavWidth + x] = 0; cells++; }
            Assert.AreEqual(156, cells, "[B3c] the harness must lay the 156-cell belt it counts");

            for (int i = 0; i < emits.Count; i++)
            {
                var (m, t) = emits[i];
                if (m != kit.knifeRest && m != kit.wireFence && m != kit.hedgehog && m != kit.stakes && m != kit.wirePost) continue;
                var p = t.GetColumn(3);
                // the two posts of a post-and-wire cell stand at +-.95 of the cell's own point, which can carry one
                // over the cell line: the pair is emitted back to back, so their midpoint is that point again
                if (m == kit.wirePost && i + 1 < emits.Count && emits[i + 1].m == kit.wirePost) { p = (p + emits[++i].t.GetColumn(3)) * .5f; }
                if (!Cell(map, p, out int cx, out int cz)) continue;
                int key = cz * map.NavWidth + cx;
                if (frames.ContainsKey(key)) frames[key]++;
            }

            int empty = 0;
            foreach (var kv in frames) if (kv.Value == 0) empty++;
            // [B3c] a wire cell that Build draws nothing in: every cell with the Wire bit gets at least one frame
            Assert.AreEqual(0, empty, "[B3c] " + empty + " wire cells got no frame from Build");
        }

        [Test, Category("Long")]
        public void AWireFrameSitsAtItsStandsHeight()
        {
            using var map = BeltMap();
            var emits = Compose(kit, map);

            int up = 0, down = 0, flat = 0;
            foreach (var (m, t) in emits)
            {
                if (m != kit.knifeRest) continue;
                var p = t.GetColumn(3);
                // the backdrop lays knife rests outside the field too: only a frame inside a wire cell is the belt's
                if (!Cell(map, p, out int cx, out int cz)) continue;
                if ((map.NavLayers[map.NavIndex(cx, cz)] & (byte)NavLayer.Wire) == 0) continue;
                var stand = BattlefieldComposer.StandOf(cz * map.NavWidth + cx);
                float lift = p.y - map.Height.Sample(p.x, p.z);
                // [B3b] the three lifts: a standing frame on the ground, a knocked-down one at +.15, a flat one at +.05
                switch (stand)
                {
                    case BattlefieldComposer.WireStand.Up: up++; Assert.AreEqual(0f, lift, 1e-3f, "[B3b] a standing wire frame should sit on the ground"); break;
                    case BattlefieldComposer.WireStand.Down: down++; Assert.AreEqual(.15f, lift, 1e-3f, "[B3b] a knocked-down wire frame should sit at +.15"); break;
                    case BattlefieldComposer.WireStand.Flat: flat++; Assert.AreEqual(.05f, lift, 1e-3f, "[B3b] a flat wire frame should sit at +.05"); break;
                }
            }
            Assert.Greater(up, 0, "[B3b] no standing frame in the sample");
            Assert.Greater(down, 4, "[B3b] no knocked-down frame in the sample");
        }
    }
}
