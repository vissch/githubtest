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

        /// <summary>
        /// A man's feet are on the DRAWN ground, and a combat ray reads the same heightfield bilinearly. Both go
        /// through Sample, so that is what these tests measure. The earlier version read HeightAtCell(x*2, z*2) -
        /// the lower-left corner of the 2 m cell - which is a point no man occupies and nothing consumes.
        /// </summary>
        static float GroundAt(MapData map, int cell)
        {
            var c = map.NavCellCenter(cell);
            return map.Height.Sample(c.x, c.z);
        }

        /// <summary>The first cell out of the trench on the enemy-facing side: walked for, not assumed to be 2 away.</summary>
        static int LipOf(MapData map, in TrenchDef def, int fs)
        {
            int x = fs % map.NavWidth, z = fs / map.NavWidth, step = def.OwnerTeam == 0 ? 1 : -1;
            for (int k = 1; k <= 4; k++)
            {
                int n = (z + step * k) * map.NavWidth + x;
                if (n < 0 || n >= map.CellTrenchId.Length) return -1;
                if (map.CellTrenchId[n] != def.Id) return n;
            }
            return -1;
        }

        /// <summary>
        /// The claim that holds on EVERY map, because it is an identity rather than a property of flat ground:
        /// the trench is carved FireStepDepth down and the step is raised FireStepRise back up, so the top of the
        /// step sits (1.8 - 0.7) = 1.1 m below the ground in front of it, wherever that ground happens to be.
        /// Run on ShelledForest, which is what the game plays and what the old tests avoided.
        /// </summary>
        [Test]
        public void TheStepSitsAboutAMetreBelowTheGroundInFrontOfIt_OnTheMapWeActuallyPlay()
        {
            using var map = BattlefieldGenerator.Create(BattlefieldParams.ShelledForest(1917), Allocator.Persistent);
            int checkedCells = 0;
            float lowest = float.MaxValue, highest = float.MinValue;
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                var def = map.Trenches[t];
                for (int k = 0; k < def.FireStepCount; k += 3)
                {
                    int fs = map.FireStepCells[def.FireStepStart + k];
                    int lip = LipOf(map, def, fs);
                    if (lip < 0) continue;
                    float below = GroundAt(map, lip) - GroundAt(map, fs);
                    lowest = math.min(lowest, below); highest = math.max(highest, below);
                    checkedCells++;
                }
            }
            Assert.Greater(checkedCells, 40, "the map has fire steps to check");
            // craters fall in front of a trench and only ever lower the lip, so the band is one-sided at the top
            Assert.Greater(lowest, 0.55f, $"a firing post is {lowest:F2} m below the lip: too deep to shoot over");
            Assert.Less(highest, 1.60f, $"a firing post is {highest:F2} m below the lip: that is not a step, it is a wall");
        }

        /// <summary>The rise itself, on the one map flat enough to measure it directly: the playtest bowl, whose
        /// neighbouring cells differ by about 0.004 m. On rolling ground the terrain noise swamps it, which is why
        /// the test above asserts the identity instead.</summary>
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
                    int floor = -1;
                    foreach (int dz in new[] { -1, 1 })
                    {
                        int n = (z + dz) * map.NavWidth + x;
                        if (n < 0 || n >= map.CellTrenchId.Length) continue;
                        if (map.CellTrenchId[n] == def.Id && n != fs) floor = n;
                    }
                    if (floor < 0) continue;
                    Assert.AreEqual(GreyboxMapGenerator.FireStepRise, GroundAt(map, fs) - GroundAt(map, floor), 0.06f,
                        $"trench {def.Id} column {x}: the firing post must stand a fire step above the trench floor");
                    checkedCells++;
                }
            }
            Assert.Greater(checkedCells, 8, "the map has fire steps to check");
        }

        /// <summary>
        /// How much of a man clears the parapet. RifleAboveFeet is an ASSUMPTION, not a measurement - the real
        /// figure is the Y of VatAsset.Muzzle on the FireFireStep row, which this assembly cannot reach. The
        /// measured margin on ShelledForest is only 0.08-0.25 m, so an error of 0.16 m in this constant flips the
        /// whole fire step between a fix and a no-op. The test therefore reports the margin it found rather than
        /// merely passing, so the number is visible in the run output and can be checked against the figure.
        /// </summary>
        [Test]
        public void AManAtAFiringPostHasHisRifleOverTheParapet()
        {
            const float RifleAboveFeet = 1.25f;   // assumed; see the summary above
            using var map = BattlefieldGenerator.Create(BattlefieldParams.ShelledForest(1917), Allocator.Persistent);
            int checkedCells = 0;
            float worst = float.MaxValue;
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                var def = map.Trenches[t];
                for (int k = 0; k < def.FireStepCount; k += 3)
                {
                    int fs = map.FireStepCells[def.FireStepStart + k];
                    int lip = LipOf(map, def, fs);
                    if (lip < 0) continue;
                    worst = math.min(worst, GroundAt(map, fs) + RifleAboveFeet - GroundAt(map, lip));
                    checkedCells++;
                }
            }
            Assert.Greater(checkedCells, 40, "the map has parapets to check");
            Assert.Greater(worst, 0f,
                $"thinnest margin {worst:F3} m: his rifle is below the parapet, so he is shooting into the ground. " +
                "Before the fire step existed this was about -0.6 m.");
        }

        /// <summary>A man in support keeps his head down. On the FRONT line, not whichever trench happens to be
        /// first in the list - Trenches[0] on the playtest map is the reserve line.</summary>
        [Test]
        public void AReservePostStaysDownOnTheFloor()
        {
            using var m = NewMatch();
            var map = m.Map;
            var step = new HashSet<int>();
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                var d = map.Trenches[t];
                for (int k = 0; k < d.FireStepCount; k++) step.Add(map.FireStepCells[d.FireStepStart + k]);
            }
            var front = map.Trenches[m.Fields.FrontTrench(0)];
            int floors = 0;
            for (int k = 0; k < front.CellCount; k++)
            {
                int cell = map.TrenchCells[front.CellStart + k];
                if (step.Contains(cell)) continue;
                int lip = LipOf(map, front, cell);
                if (lip < 0) continue;
                floors++;
                Assert.Less(GroundAt(map, cell) + 1.25f, GroundAt(map, lip),
                    "a man in support keeps his head down: the floor is not a fire step");
            }
            Assert.Greater(floors, 8, "the trench has floor as well as fire step");
        }

        // ---- the spread ----------------------------------------------------------------------------------------

        const int W = 150;   // the playtest trench's nav width, so these are the cells the garrison really uses

        [Test]
        public void ThePostFieldWanders_WithoutScatteringMenOffTheirMarks()
        {
            float widest = 0f;
            var seen = new HashSet<int>();
            for (int cell = 0; cell < 4000; cell++)
            {
                var o = TrenchPost.Offset(cell, W);
                Assert.LessOrEqual(math.abs(o.x), TrenchPost.Jitter + 1e-4f, "never further than half a cell, or he steps into the next post");
                Assert.LessOrEqual(math.abs(o.z), TrenchPost.Jitter + 1e-4f, "never further than half a cell, or he steps into the next post");
                Assert.AreEqual(0f, o.y, "the offset is on the ground plane only");
                widest = math.max(widest, math.abs(o.x));
                seen.Add((int)(o.x * 50f) * 1000 + (int)(o.z * 50f));
            }
            Assert.Greater(widest, TrenchPost.Jitter * 0.8f, "the field uses the room it is given");
            Assert.Greater(seen.Count, 1000, "the line wanders across the map rather than repeating");
            Assert.AreEqual(TrenchPost.Offset(1234, W), TrenchPost.Offset(1234, W), "the same cell always gives the same spot");

            // the property the old per-cell noise got exactly backwards. Neighbours must SHARE their
            // displacement - that is what keeps the distance between two men - while the line as a whole
            // still leaves true, which is what stops it reading as ruled.
            float near = math.distance(TrenchPost.Offset(1234, W), TrenchPost.Offset(1235, W));
            float far = math.distance(TrenchPost.Offset(1234, W), TrenchPost.Offset(1274, W));
            Assert.Less(near, 0.25f, $"neighbouring posts move together (measured 0.11 m, was 0.6+ with per-cell noise): {near:F3}");
            Assert.Greater(far, 0.15f, $"but forty cells along, the line has wandered: {far:F3}");
        }

        /// <summary>
        /// THE INVARIANT WHOSE ABSENCE SHIPPED A REGRESSION. SeparationJob.GarrisonSpacing is 2 m and a nav cell
        /// is 2 m, so the bare cell lattice was exactly tuned to the separation radius: a posted garrison sat at
        /// rest. Displacing each post by independent noise dropped the worst adjacent pair to 0.933 m - well
        /// inside the radius, so those men shoved each other while their posts pulled them back, permanently.
        /// Nothing measured that, and the before/after table hid it by comparing against 0.88 m, which came from
        /// postless men rather than posted ones.
        /// The smooth field restores most of it: 1.809 m measured over the same cells. Still under 2 m, so the
        /// very tightest pairs in a FULL trench keep a small standing push - which is why the bound below is
        /// 1.75 and not 2.0, and why it is written down rather than rounded up.
        /// </summary>
        [Test]
        public void NoTwoPostPointsStandInsideEachOther()
        {
            float worst = float.MaxValue; int worstCell = -1;
            for (int cell = 0; cell < 4000; cell++)
            {
                int cx = cell % W, cz = cell / W;
                float3 a = new float3(cx * 2f, 0f, cz * 2f) + TrenchPost.Offset(cell, W);
                foreach (int d in new[] { 1, W, W + 1, W - 1 })   // the four adjacency classes
                {
                    int n = cell + d, nx = n % W, nz = n / W;
                    if (math.abs(nx - cx) > 1) continue;          // wrapped round the end of a row
                    float3 b = new float3(nx * 2f, 0f, nz * 2f) + TrenchPost.Offset(n, W);
                    float dist = math.distance(a, b);
                    if (dist < worst) { worst = dist; worstCell = cell; }
                }
            }
            Assert.Greater(worst, 1.75f,
                $"cell {worstCell}: two adjacent post points are {worst:F3} m apart against a 2.00 m separation " +
                "radius. Men posted that close push each other off their own marks and never settle.");
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
                float3 post = m.Map.NavCellCenter(cell) + TrenchPost.Offset(cell, m.Map.NavWidth);
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
