// Phase: A3 — the owner, 2026-09-23, playing the game: "we want them to spread out in the trench more, and also
// put their guns over the edge of the trench instead of shooting through the ground."
// Both complaints were measured in a live match before anything was changed (92 men, team 0 front trench):
//   - the firing post was LEVEL WITH THE TRENCH FLOOR (floor 0.73, "fire step" 0.77, parapet 2.43), so a man on it
//     stood 1.66 m below the field and his rifle sat about 0.4 m UNDER the lip: he aimed into the parapet.
//   - 31 of 92 men held no post at all, and the rest stood on cell centres: a 2 m lattice.
// These tests hold both fixes to the geometry, not to the look, so a later change to the trench cannot quietly
// undo them.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;
using TW.Sim.Units;

namespace TW.Tests
{
    public class TrenchSpreadTests
    {
        static MatchSim NewMatch(uint seed = 0xC0FFEE)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.Seed = seed;
            return MatchSim.CreatePlaytest(cfg);
        }

        static void Run(MatchSim m, int ticks)
        {
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            for (int t = 0; t < ticks; t++) m.Step(none);
        }

        /// <summary>The exact height of a nav cell, not a sample at its centre: a centre falls on the seam between
        /// two height cells, where interpolation would blur the very step we are measuring.</summary>
        static float HeightOf(MapData map, int cell)
            => map.Height.HeightAtCell((cell % map.NavWidth) * 2, (cell / map.NavWidth) * 2);

        // ---- the guns ------------------------------------------------------------------------------------------

        [Test]
        public void AFiringPostIsAStepAboveTheTrenchFloor()
        {
            using var m = NewMatch();
            var map = m.Map;
            int checkedCells = 0;
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                var def = map.Trenches[t];
                for (int k = 0; k < def.FireStepCount; k += 7)
                {
                    int fs = map.FireStepCells[def.FireStepStart + k];
                    int x = fs % map.NavWidth, z = fs / map.NavWidth;
                    // the trench floor in the same column: whichever neighbour along Z is this trench and is not the step
                    int floor = -1;
                    foreach (int dz in new[] { -1, 1 })
                    {
                        int n = (z + dz) * map.NavWidth + x;
                        if (n < 0 || n >= map.CellTrenchId.Length) continue;
                        if (map.CellTrenchId[n] == def.Id && n != fs) floor = n;
                    }
                    if (floor < 0) continue;
                    Assert.AreEqual(GreyboxMapGenerator.FireStepRise, HeightOf(map, fs) - HeightOf(map, floor), 0.02f,
                        $"trench {def.Id} column {x}: the firing post must stand a fire step above the trench floor");
                    checkedCells++;
                }
            }
            Assert.Greater(checkedCells, 8, "the map has fire steps to check");
        }

        /// <summary>
        /// The one that matters: a man at a firing post must be able to shoot over the parapet rather than into it.
        /// His rifle sits about 1.25 m above his feet in the fire-step pose, so the ground in front of him has to be
        /// less than that above where he stands. Before the fire step existed the lip was 1.66 m above him.
        /// </summary>
        [Test]
        public void AManAtAFiringPostHasHisRifleOverTheParapet()
        {
            const float RifleAboveFeet = 1.25f;
            using var m = NewMatch();
            var map = m.Map;
            int checkedCells = 0;
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                var def = map.Trenches[t];
                int toward = def.OwnerTeam == 0 ? 1 : -1;      // team 0 faces +Z, team 1 faces -Z
                for (int k = 0; k < def.FireStepCount; k += 7)
                {
                    int fs = map.FireStepCells[def.FireStepStart + k];
                    int x = fs % map.NavWidth, z = fs / map.NavWidth;
                    int lip = (z + toward) * map.NavWidth + x;   // the first cell out of the trench, the parapet
                    if (lip < 0 || lip >= map.CellTrenchId.Length || map.CellTrenchId[lip] == def.Id) continue;
                    float over = HeightOf(map, fs) + RifleAboveFeet - HeightOf(map, lip);
                    Assert.Greater(over, 0f,
                        $"trench {def.Id} column {x}: his rifle is {-over:F2} m BELOW the parapet, so he is shooting into the ground");
                    checkedCells++;
                }
            }
            Assert.Greater(checkedCells, 8, "the map has parapets to check");
        }

        [Test]
        public void AReservePostStaysDownOnTheFloor()
        {
            using var m = NewMatch();
            var map = m.Map;
            var step = new HashSet<int>();
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                var def = map.Trenches[t];
                for (int k = 0; k < def.FireStepCount; k++) step.Add(map.FireStepCells[def.FireStepStart + k]);
            }
            var front = map.Trenches[0];
            int floors = 0;
            for (int k = 0; k < front.CellCount; k++)
            {
                int cell = map.TrenchCells[front.CellStart + k];
                if (step.Contains(cell)) continue;
                floors++;
                int x = cell % map.NavWidth, z = cell / map.NavWidth;
                int lip = (z + (front.OwnerTeam == 0 ? 2 : -2)) * map.NavWidth + x;
                if (lip < 0 || lip >= map.CellTrenchId.Length || map.CellTrenchId[lip] == front.Id) continue;
                Assert.Less(HeightOf(map, cell) + 1.25f, HeightOf(map, lip),
                    "a man in support keeps his head down: the floor is not a fire step");
            }
            Assert.Greater(floors, 8, "the trench has floor as well as fire step");
        }

        // ---- the spread ----------------------------------------------------------------------------------------

        [Test]
        public void AManStandsSomewhereInHisPostCell_NotOnALattice()
        {
            float widest = 0f;
            var seen = new HashSet<int>();
            for (int cell = 0; cell < 4000; cell++)
            {
                var o = TrenchPost.Offset(cell);
                Assert.LessOrEqual(math.abs(o.x), TrenchPost.Jitter + 1e-4f, "never further than half a cell, or he steps into the next post");
                Assert.LessOrEqual(math.abs(o.z), TrenchPost.Jitter + 1e-4f, "never further than half a cell, or he steps into the next post");
                Assert.AreEqual(0f, o.y, "the offset is on the ground plane only");
                widest = math.max(widest, math.abs(o.x));
                seen.Add((int)(o.x * 50f) * 1000 + (int)(o.z * 50f));
            }
            Assert.Greater(widest, TrenchPost.Jitter * 0.8f, "the offsets use the room they are given");
            Assert.Greater(seen.Count, 1500, "the offsets are spread, not a handful of repeated values");
            Assert.AreEqual(TrenchPost.Offset(1234), TrenchPost.Offset(1234), "the same cell always gives the same spot");
            Assert.AreNotEqual(TrenchPost.Offset(1234), TrenchPost.Offset(1235), "neighbouring posts do not share a spot");
        }

        /// <summary>
        /// The measured complaint: at ThinnedInSixteen = 5 a 92-man garrison found 61 posts and a third of it had
        /// nowhere to be, so it collapsed onto the centreline under separation alone. A trench is finite, but it must
        /// not throw away a third of its room to make the posts look irregular.
        /// </summary>
        [Test]
        public void AFullGarrisonFindsPostsForNearlyEveryone()
        {
            using var m = NewMatch();
            var front = m.Map.Trenches[0];
            for (int k = 0; k < 40; k++)
                m.World.Spawn(0, 0, new float3(20f + (k % 20) * 6f, 0f, 20f + (k / 20) * 4f), 100f, 1.2f, false);
            Run(m, 1400);
            int garrison = 0, posted = 0;
            for (int i = 0; i < m.World.HighWater; i++)
            {
                if (!m.World.IsAlive(i) || m.World.TrenchId[i] < 0) continue;
                garrison++;
                if (m.World.PostCell[i] >= 0) posted++;
            }
            Assert.Greater(garrison, 8, "men reached a trench");
            Assert.GreaterOrEqual(posted, (int)(garrison * 0.9f),
                $"{garrison - posted} of {garrison} men have nowhere to be and fall back to separation alone");
            Assert.Greater(front.CellCount, 0);
        }

        /// <summary>
        /// A trench with room in it stands its men apart. TrenchGarrisonSystem offers the widely spaced posts first
        /// (Roomiest = 2 cells = 6 m) and only packs down when those run out, so a light garrison must not end up
        /// shoulder to shoulder. The jitter can take up to 0.55 m off each side of that, hence the margin.
        /// </summary>
        [Test]
        public void ALightGarrisonStandsWellApart()
        {
            using var m = NewMatch();
            for (int k = 0; k < 12; k++) m.World.Spawn(0, 0, new float3(20f + k * 5f, 0f, 20f), 100f, 1.2f, false);
            Run(m, 1200);
            // men STANDING at their posts. A man still walking up the trench passes within arm's length of everyone
            // on his way and says nothing about how the garrison ends up placed.
            var at = new List<float3>();
            for (int i = 0; i < m.World.HighWater; i++)
            {
                int cell = m.World.PostCell[i];
                if (!m.World.IsAlive(i) || m.World.TrenchId[i] < 0 || cell < 0) continue;
                float3 post = m.Map.NavCellCenter(cell) + TrenchPost.Offset(cell);
                if (math.distance(post.xz, m.World.Position[i].xz) < 1f) at.Add(m.World.Position[i]);
            }
            Assert.Greater(at.Count, 5, "men reached the trench and settled onto posts");
            float worst = float.MaxValue;
            for (int a = 0; a < at.Count; a++)
                for (int b = a + 1; b < at.Count; b++)
                    worst = math.min(worst, math.distance(at[a].xz, at[b].xz));
            Assert.Greater(worst, 4.5f, $"two men are standing {worst:F2} m apart in a trench with room to spare");
        }

        [Test]
        public void TheSpreadIsTheSameOnEveryMachine()
        {
            ulong Play()
            {
                using var m = NewMatch();
                for (int k = 0; k < 16; k++) m.World.Spawn(0, 0, new float3(20f + k * 4f, 0f, 20f), 100f, 1.2f, false);
                Run(m, 800);
                return m.World.Hash();
            }
            Assert.AreEqual(Play(), Play(), "same seed, same posts, same spots, same hash");
        }
    }
}
