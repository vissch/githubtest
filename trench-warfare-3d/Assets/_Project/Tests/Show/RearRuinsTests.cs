// Phase: B7 (implemented) — the four big ruins stand in the rear with the other buildings (owner, 2026-10-06:
// "lets put them also in the rear with the other buildings"). Built like AssetScaleReport.Measure: the real
// generator, surface and composer, read back through composer.Hamlets.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using UnityEngine;
using TW.Sim.Terrain;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public sealed class RearRuinsTests
    {
        static readonly string[] RuinNames = { "Boilerhouse", "Townhouse", "Guildhall", "Gasholder" };

        static BattlefieldComposer Place(MapData map, BattlefieldSurface surface, BattlefieldKit kit, int seed)
        {
            var composer = new BattlefieldComposer(kit, seed);
            composer.Build(map, surface, (m, mat) => { });
            return composer;
        }

        /// <summary>The front of team 0's rear, exactly as BattlefieldComposer.PlaceRear computes it: a ruin must stand
        /// above (lower z than) this line.</summary>
        static float RearFront(MapData map)
        {
            float front = float.MaxValue;
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                var trench = map.Trenches[t];
                if (trench.OwnerTeam != 0) continue;
                for (int i = trench.CellStart; i < trench.CellStart + trench.CellCount; i++)
                    front = Mathf.Min(front, (map.TrenchCells[i] / map.NavWidth) * MapData.NavCellSize);
            }
            if (front == float.MaxValue) front = map.SizeMeters.y * .12f;
            return front - 4f;
        }

        /// <summary>Places a seed's battlefield and checks every ruin that landed in the rear stands clear of the
        /// trench front, every spawn and the supply road, and clear of every other hamlet; at least <paramref
        /// name="minPlaced"/> of the four must have found room. ShelledForest (90 m) cannot take all four side by
        /// side in the roughly 13 m of usable depth the rear band leaves above the trench front: four ruins
        /// averaging 11-12 m across need about 46 m of width there and the band is only some 27 m wide, so three is
        /// what the ground genuinely holds (PlacementReport: "ruin: spacing"/"ruin: prop" on the one left over).
        /// Nothing is moved off the rear to make the fourth fit.</summary>
        static void CheckGround(BattlefieldKit kit, int seed, System.Func<uint, BattlefieldParams> ground, int minPlaced = 4)
        {
            var map = BattlefieldGenerator.Create(ground((uint)seed), Allocator.Persistent);
            try
            {
                var surface = new BattlefieldSurface(map);
                var composer = Place(map, surface, kit, seed);
                float front = RearFront(map);
                var byName = new Dictionary<string, BattlefieldComposer.Hamlet>();
                foreach (var h in composer.Hamlets)
                {
                    var name = kit.Houses[h.House].Name;
                    if (System.Array.IndexOf(RuinNames, name) >= 0) byName[name] = h;
                }
                Assert.GreaterOrEqual(byName.Count, minPlaced, minPlaced + " of 4 ruins stand in the rear: " + composer.PlacementReport);
                foreach (var (name, h) in byName)
                {
                    Assert.Less(h.Centre.z, front, name + " stands above team 0's rear-trench front");
                    for (int i = 0; i < map.Spawns.Length; i++)
                        Assert.GreaterOrEqual(new Vector2(map.Spawns[i].Pos.x - h.Centre.x, map.Spawns[i].Pos.z - h.Centre.z).magnitude, h.Radius + 2.9f,
                            name + " clear of spawn " + i);
                    if (map.SupplyRoad.IsCreated)
                        for (int i = 0; i < map.SupplyRoad.Length; i++)
                            Assert.GreaterOrEqual(new Vector2(map.SupplyRoad[i].x - h.Centre.x, map.SupplyRoad[i].z - h.Centre.z).magnitude, h.Radius + 1.9f,
                                name + " clear of the supply road point " + i);
                    foreach (var other in composer.Hamlets)
                    {
                        if (other.House == h.House) continue;
                        float gap = new Vector2(other.Centre.x - h.Centre.x, other.Centre.z - h.Centre.z).magnitude;
                        Assert.GreaterOrEqual(gap, h.Radius + other.Radius - .01f, name + " does not overlap " + kit.Houses[other.House].Name);
                    }
                }
            }
            finally { map.Dispose(); }
        }

        [Test]
        public void Three_Of_The_Four_Ruins_Stand_In_The_Rear_On_ShelledForest()
        {
            var kit = new BattlefieldKit(); kit.ResolveKeysAndRules();
            CheckGround(kit, 1917, BattlefieldParams.ShelledForest, minPlaced: 3);
        }

        [Test]
        public void All_Four_Ruins_Stand_In_The_Rear_On_WinterLine()
        {
            var kit = new BattlefieldKit(); kit.ResolveKeysAndRules();
            CheckGround(kit, 1917, BattlefieldParams.WinterLine);
        }

        [Test, Category("Long")]
        public void The_Ruins_Stand_In_The_Rear_Across_Grounds_And_Seeds()
        {
            var kit = new BattlefieldKit(); kit.ResolveKeysAndRules();
            foreach (var (name, make) in AssetScaleReport.Grounds)
                foreach (var seed in new[] { 1917, 2, 7 })
                    CheckGround(kit, seed, make, minPlaced: name == "ShelledForest" ? 3 : 4);
        }
    }
}
