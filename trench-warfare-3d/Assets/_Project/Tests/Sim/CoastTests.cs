// Phase: coastal level — the sea as a battlefield rather than as the scenery behind ShelledForest's far trench.
//
// The defect these were written for: BattlefieldGenerator.SeaMargin was a 36 m const, while SeaLandingSystem puts
// a craft at StandOff (96 m) seaward of the waterline and a gunboat at ShipStandOff plus its spread (90 m to
// 270 m). ShelledForest's water is 17 m deep in Z, so every run-in and every gunboat sat beyond the map edge, over
// clamped heightfield and outside the nav grid. Nothing threw, because CellOf and Height.Sample both clamp, and
// nothing measured it, so the coast quietly could not carry a level.
//
// These read BOTH constants rather than restating either. If somebody moves the boats further out, or trims a
// map's coast, the test says so instead of the picture silently losing its ships.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class CoastTests
    {
        /// <summary>The furthest thing SeaLandingSystem can place on the water, in metres seaward of the waterline.</summary>
        static float FurthestAfloat => SeaLandingSystem.ShipStandOff + SeaLandingSystem.ShipOutMax;

        /// <summary>How much water a map actually has, in metres of Z beyond the waterline.</summary>
        static float WaterReach(MapData map) => map.SizeMeters.y - map.ShoreZ;

        [Test]
        public void TheLandingMapContainsTheWaterItsOwnBoatsUse()
        {
            using var map = BattlefieldGenerator.Create(BattlefieldParams.Landing(1917), Allocator.Persistent);
            Assert.IsTrue(map.HasSea, "a landing map has a sea");
            Assert.GreaterOrEqual(WaterReach(map), FurthestAfloat,
                $"the map has {WaterReach(map):F0} m of water past the waterline but SeaLandingSystem can put a "
                + $"gunboat {FurthestAfloat:F0} m out. Everything beyond the edge sits on clamped height, outside "
                + "the nav grid, and the run-in the level exists to show happens off the map.");
            Assert.GreaterOrEqual(WaterReach(map), SeaLandingSystem.StandOff,
                "a craft appears at StandOff and must appear on the map");
        }

        /// <summary>
        /// ShelledForest is NOT a landing map and is deliberately left short: its coast is the strip of scenery
        /// behind team 1's rear trench, which is all it was built to be. This records that as understood rather
        /// than leaving the next reader to wonder whether the shortfall above applies to it too - and if anyone
        /// ever widens that coast, this is where they will be told to revisit the question.
        /// </summary>
        [Test]
        public void ShelledForestsCoastIsSceneryAndIsKnownToBeTooShortForBoats()
        {
            using var map = BattlefieldGenerator.Create(BattlefieldParams.ShelledForest(1917), Allocator.Persistent);
            Assert.IsTrue(map.HasSea, "ShelledForest still has its coast");
            Assert.Less(WaterReach(map), SeaLandingSystem.StandOff,
                $"ShelledForest now has {WaterReach(map):F0} m of water, past the {SeaLandingSystem.StandOff:F0} m "
                + "a craft appears at. That is a good thing, but it was a scenery coast when this was written - "
                + "check whether it should now be held to the landing map's standard instead.");
        }

        /// <summary>The shipped map's geometry must not move because a NEW map wanted more water.</summary>
        [Test]
        public void AddingTheDialLeftTheShippedMapExactlyWhereItWas()
        {
            var p = BattlefieldParams.ShelledForest(1917);
            Assert.AreEqual(0f, p.SeaMargin, "ShelledForest asks for no particular margin, so it takes the default");
            Assert.AreEqual(BattlefieldGenerator.SeaMargin, BattlefieldGenerator.SeaMarginOf(p),
                "zero must mean the default, or every map built before the dial existed changes shape");
            using var map = BattlefieldGenerator.Create(p, Allocator.Persistent);
            Assert.AreEqual(p.Length + BattlefieldGenerator.SeaMargin, map.SizeMeters.y, 1e-3f,
                "the shipped map is still its layout plus the default coast");
        }

        [Test]
        public void TheMarginSurvivesBeingWrittenDownAndReadBack()
        {
            var p = BattlefieldParams.Landing(1917);
            var q = BattlefieldParams.Deserialize(p.Serialize());
            Assert.AreEqual(p.SeaMargin, q.SeaMargin, 1e-3f, "a replay of a landing map must land on the same coast");
            Assert.AreEqual(p.Sea, q.Sea);
            Assert.AreEqual(p.Length, q.Length, 1e-3f);
            Assert.AreEqual(p.Width, q.Width, 1e-3f);
        }

        /// <summary>
        /// A mission is a BattlefieldParams value, not a saved map (BattlefieldGenerator's own header), so the
        /// preset has to give the same ground on every machine or a landing desyncs before the first shot.
        /// </summary>
        [Test]
        public void TheLandingMapIsTheSameGroundEveryTime()
        {
            using var a = BattlefieldGenerator.Create(BattlefieldParams.Landing(1917), Allocator.Persistent);
            using var b = BattlefieldGenerator.Create(BattlefieldParams.Landing(1917), Allocator.Persistent);
            Assert.AreEqual(a.Hash(0ul), b.Hash(0ul), "same seed and params, same coast");
            using var c = BattlefieldGenerator.Create(BattlefieldParams.Landing(1918), Allocator.Persistent);
            Assert.AreNotEqual(a.Hash(0ul), c.Hash(0ul), "a different seed is a different beach, or the seed is ignored");
        }

        /// <summary>
        /// The beach has to be somewhere a man can stand and fight, not a cliff or a mudflat. Walked rather than
        /// assumed: the sand between the top of the beach and the waterline must fall, and must not fall so fast
        /// that a landed man is climbing.
        /// </summary>
        [Test]
        public void TheBeachIsWalkableFromTheWaterToTheTopOfIt()
        {
            using var map = BattlefieldGenerator.Create(BattlefieldParams.Landing(1917), Allocator.Persistent);
            float x = map.SizeMeters.x * 0.5f;
            float top = map.Height.Sample(x, map.SeaStartZ), water = map.Height.Sample(x, map.ShoreZ);
            Assert.Greater(top, water, "the beach falls from the top of it to the waterline");
            float run = map.ShoreZ - map.SeaStartZ;
            float slope = (top - water) / math.max(run, 1e-3f);
            Assert.Less(slope, 0.5f, $"the beach climbs at {slope:F2} - that is a bank, not sand a man walks up");
            Assert.Greater(slope, 0.01f, $"the beach is flat ({slope:F3}), so the waterline has no edge to read");
        }

        // ---- the shore defences (the owner, 2026-10-08: "The coast's sand") ---------------------------------------
        // What is asserted here is the owner's own words - one wire belt, timber obstacles seaward of it, a trench
        // above the beach the defender starts in, and a path through from every landing - not the generator's
        // arithmetic. Change the noise and these still hold; drop the belt or wall the beach in and they fail.
        static readonly uint[] Seeds = { 1917u, 1918u, 7u, 42u };

        /// <summary>The shore trench: the sea team's line that lies above the beach, behind none of its own. Found by
        /// where it is rather than by its id, so a renumbering cannot quietly make these pass on trench 3.</summary>
        static int ShoreTrench(MapData map)
        {
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                if (map.Trenches[t].OwnerTeam != 1) continue;
                float z = map.NavCellCenter(map.TrenchCells[map.Trenches[t].CellStart]).z;
                if (z > map.SeaStartZ - BattlefieldGenerator.ShoreTrenchInset - 4f && z < map.SeaStartZ) return t;
            }
            return -1;
        }

        static bool IsWire(MapData map, int cell) => ((NavLayer)map.NavLayers[cell] & NavLayer.Wire) != 0;

        /// <summary>BattlefieldGenerator.Connected's traversal rule from one cell, REFUSING every wire cell: where a
        /// man who will not cut can walk. Returns the cells he reaches.</summary>
        static bool[] WalkWithoutCuttingWire(MapData map, int start)
        {
            var seen = new bool[map.NavWidth * map.NavLength];
            var open = new System.Collections.Generic.Stack<int>();
            if (IsWire(map, start) || ((NavLayer)map.NavLayers[start] & NavLayer.Blocked) != 0) return seen;
            seen[start] = true; open.Push(start);
            while (open.Count > 0)
            {
                int cell = open.Pop();
                int cx = cell % map.NavWidth, cz = cell / map.NavWidth;
                for (int d = 0; d < 4; d++)
                {
                    int nx = cx + (d == 0 ? 1 : d == 1 ? -1 : 0), nz = cz + (d == 2 ? 1 : d == 3 ? -1 : 0);
                    if (nx < 0 || nz < 0 || nx >= map.NavWidth || nz >= map.NavLength) continue;
                    int ni = map.NavIndex(nx, nz);
                    if (seen[ni]) continue;
                    byte from = map.NavLayers[cell], to = map.NavLayers[ni];
                    if ((to & (byte)NavLayer.Blocked) != 0) continue;
                    if (IsWire(map, ni)) continue;                       // he does not cut: the gaps are the path
                    const byte traversal = (byte)(NavLayer.Surface | NavLayer.Trench);
                    bool link = ((from | to) & (byte)NavLayer.Link) != 0;
                    if (!link && (from & to & traversal) == 0) continue;
                    seen[ni] = true; open.Push(ni);
                }
            }
            return seen;
        }

        /// <summary>One wire belt on the dry sand, with gaps, and timber standing seaward of it.</summary>
        [Test]
        public void TheSandCarriesOneWireBeltWithGaps_AndTimberSeawardOfIt()
        {
            foreach (uint seed in Seeds)
            {
                using var map = BattlefieldGenerator.Create(BattlefieldParams.Landing(seed), Allocator.Persistent);
                float band0 = map.SeaStartZ + 5f, band1 = map.SeaStartZ + 11f;
                int wired = 0, bare = 0;
                for (int x = 0; x < map.NavWidth; x++)
                {
                    bool any = false;
                    for (int z = 0; z < map.NavLength; z++)
                    {
                        float wz = (z + 0.5f) * MapData.NavCellSize;
                        if (wz < band0 || wz > band1) continue;
                        if (IsWire(map, map.NavIndex(x, z))) { any = true; wired++; }
                    }
                    if (any) continue;
                    bare++;
                }
                Assert.Greater(wired, 0, "seed " + seed + ": no wire on the sand between the obstacles and the trench");
                Assert.Greater(bare, 0, "seed " + seed + ": the belt runs the whole width with no gap, so a landing is walled in");

                int timber = 0;
                for (int i = 0; i < map.Props.Length; i++)
                {
                    var prop = map.Props[i];
                    if (prop.Kind != PropKind.Log || prop.Pos.z <= map.SeaStartZ) continue;
                    Assert.Greater(prop.Pos.z, map.SeaStartZ + BattlefieldGenerator.BeltAt + BattlefieldGenerator.BeltDeep,
                        "seed " + seed + ": timber at " + prop.Pos.z + " stands inland of the belt, not seaward of it");
                    Assert.Greater(map.CellCover[prop.Cell], 0, "a man at the obstacle has something to shelter behind");
                    timber++;
                }
                Assert.Greater(timber, 8, "seed " + seed + ": " + timber + " timber obstacles is not an obstacle belt");
            }
        }

        /// <summary>The defender is dug in above the beach, and starts there.</summary>
        [Test]
        public void TheDefenderHasATrenchAboveTheBeachAndStartsInIt()
        {
            foreach (uint seed in Seeds)
            {
                using var map = BattlefieldGenerator.Create(BattlefieldParams.Landing(seed), Allocator.Persistent);
                int t = ShoreTrench(map);
                Assert.GreaterOrEqual(t, 0, "seed " + seed + ": the sea team has no line above the beach");
                var def = map.Trenches[t];
                Assert.AreEqual((byte)1, def.OwnerTeam);
                Assert.Greater(def.LinkCount, 0, "it has ladders, so his men can get out of it");
                for (int i = 0; i < def.CellCount; i++)
                {
                    float z = map.NavCellCenter(map.TrenchCells[def.CellStart + i]).z;
                    Assert.Less(z, map.SeaStartZ, "seed " + seed + ": the shore trench is cut into the sand, not above it");
                    Assert.Greater(z, map.SeaStartZ - 25f, "seed " + seed + ": the shore trench has fallen back off the shore");
                }

                int spawn = -1;
                for (int i = 0; i < map.Spawns.Length; i++)
                    if (map.Spawns[i].Team == 1) { var c = map.NavCellOf(map.Spawns[i].Pos); spawn = map.NavIndex(c.x, c.y); }
                Assert.GreaterOrEqual(spawn, 0);
                bool inIt = false;
                for (int i = 0; i < def.CellCount; i++) if (map.TrenchCells[def.CellStart + i] == spawn) inIt = true;
                Assert.IsTrue(inIt, "seed " + seed + ": the defender's land arrival is not a cell of his shore trench, so he does not start in it");
            }
        }

        /// <summary>
        /// The path through. A man put down by a ramp must be able to walk to the shore trench WITHOUT crossing
        /// wire: that is what "no landing is walled in" means, and it is the one thing the belt could break.
        /// </summary>
        [Test]
        public void AManLandedByARampCanReachTheShoreTrenchWithoutCuttingWire()
        {
            foreach (uint seed in Seeds)
            {
                using var map = BattlefieldGenerator.Create(BattlefieldParams.Landing(seed), Allocator.Persistent);
                int t = ShoreTrench(map);
                Assert.GreaterOrEqual(t, 0);
                var def = map.Trenches[t];
                float z = map.ShoreZ - map.SeaAway * SeaLandingSystem.RampReach;   // SeaLandingSystem.RampZ()
                for (int k = 0; k < 5; k++)
                {
                    float x = map.SizeMeters.x * (0.1f + 0.2f * k);
                    var c = map.NavCellOf(new float3(x, 0f, z));
                    var reached = WalkWithoutCuttingWire(map, map.NavIndex(c.x, c.y));
                    bool arrived = false;
                    for (int i = 0; i < def.CellCount && !arrived; i++) arrived = reached[map.TrenchCells[def.CellStart + i]];
                    Assert.IsTrue(arrived, "seed " + seed + ": a man landed at x = " + x + " cannot walk off the beach to the "
                        + "shore trench without crossing wire. The belt must have gaps, or that landing is walled in.");
                }
            }
        }

        /// <summary>The shipped map did not move: its coast is still bare scenery.</summary>
        [Test]
        public void ShelledForestsCoastIsStillBare()
        {
            var p = BattlefieldParams.ShelledForest(1917);
            Assert.IsFalse(p.ShoreDefences, "only the coast preset asks for defences");
            using var map = BattlefieldGenerator.Create(p, Allocator.Persistent);
            for (int z = 0; z < map.NavLength; z++)
            {
                if ((z + 0.5f) * MapData.NavCellSize <= map.SeaStartZ) continue;
                for (int x = 0; x < map.NavWidth; x++)
                    Assert.IsFalse(IsWire(map, map.NavIndex(x, z)), "no wire past the top of ShelledForest's beach");
            }
            for (int i = 0; i < map.Props.Length; i++)
                Assert.LessOrEqual(map.Props[i].Pos.z, map.SeaStartZ, "and nothing stands on its sand");
            Assert.AreEqual(4, map.Trenches.Length, "four trenches, as the playtest layout has always had");
        }

        /// <summary>The dial survives being written down and read back, and absent means bare.</summary>
        [Test]
        public void TheShoreDefencesDialSurvivesARoundTrip()
        {
            var p = BattlefieldParams.Landing(1917);
            Assert.IsTrue(BattlefieldParams.Deserialize(p.Serialize()).ShoreDefences, "a replay of a defended shore lands on one");
            Assert.IsFalse(BattlefieldParams.Deserialize(BattlefieldParams.ShelledForest(1917).Serialize()).ShoreDefences);
            // an older blob, written before the field existed, reads back as a bare shore
            var bytes = p.Serialize();
            var older = new byte[bytes.Length - 1];
            System.Array.Copy(bytes, older, older.Length);
            Assert.IsFalse(BattlefieldParams.Deserialize(older).ShoreDefences, "absent means no defences, so an older map keeps its geometry");
        }
    }
}
