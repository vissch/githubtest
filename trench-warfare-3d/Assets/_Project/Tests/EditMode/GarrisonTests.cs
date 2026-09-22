// Phase: A3 (implemented) — the garrison holds a trench in posts: at the parapet and back from it, spread out and
// irregular, the same on every machine. (Owner, 2026-09-22: "i do not want them to all line up on similair spots".)
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Units;

namespace TW.Tests
{
    public class GarrisonTests
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

        /// <summary>Men walked into the front trench until they garrison it.</summary>
        static List<int> Garrison(MatchSim m, int men = 16, int ticks = 900)
        {
            for (int k = 0; k < men; k++) m.World.Spawn(0, 0, new float3(20f + k * 3f, 0f, 20f), 100f, 1.2f, false);
            Run(m, ticks);
            var inTrench = new List<int>();
            for (int i = 0; i < m.World.HighWater; i++) if (m.World.IsAlive(i) && m.World.TrenchId[i] >= 0) inTrench.Add(i);
            return inTrench;
        }

        [Test]
        public void EveryGarrisonedManHasHisOwnPost_InHisOwnTrench()
        {
            using var m = NewMatch();
            var men = Garrison(m);
            Assert.Greater(men.Count, 4, "men reach a trench and garrison it");
            var taken = new HashSet<int>();
            foreach (int i in men)
            {
                int cell = m.World.PostCell[i];
                if (cell < 0) continue;                       // the trench was full: he holds where he stands
                Assert.IsTrue(taken.Add(cell), $"slot {i}: two men on one post ({cell})");
                Assert.AreEqual(m.World.TrenchId[i], m.Map.CellTrenchId[cell], "a post is a cell of his own trench");
                Assert.Contains((int)m.World.PostKind[i], new[] { 1, 2 }, "a post is either a firing post or a reserve post");
            }
            Assert.Greater(taken.Count, 3, "most of them got a post");
        }

        [Test]
        public void TheyStandApart_NotInOneLineDownTheMiddle()
        {
            using var m = NewMatch();
            var men = Garrison(m);
            var rows = new HashSet<int>();
            var cols = new HashSet<int>();
            int gaps = 0, last = -99;
            foreach (int i in men)
            {
                var c = m.Map.NavCellOf(m.World.Position[i]);
                rows.Add(c.y); cols.Add(c.x);
                if (last >= 0 && c.x - last > 1) gaps++;
                last = c.x;
            }
            Assert.Greater(rows.Count, 1, "they are not all on one row of cells: some man the parapet, some stand back");
            Assert.Greater(cols.Count, 3, "they are spread along the trench");
            Assert.Greater(gaps, 0, "the posts are thinned, so the line has gaps in it rather than being evenly filled");
        }

        [Test]
        public void AManAtTheParapetMansTheFireStep_AndFacesOverIt()
        {
            using var m = NewMatch();
            var men = Garrison(m);
            // an enemy in front of the trench for them to shoot at
            float3 front = m.World.Position[men[0]] + new float3(0f, 0f, 12f);
            for (int k = 0; k < 6; k++) m.World.Spawn(1, 0, front + new float3(k * 2f, 0f, 0f), 100000f, 0f, false);
            Run(m, 120);
            int onStep = 0, facing = 0;
            foreach (int i in men)
            {
                if (m.World.StanceOf[i] != (byte)Stance.FireStep) continue;
                onStep++;
                Assert.AreEqual(1, m.World.PostKind[i], "only a man at a firing post goes up on the step");
                var def = m.Map.Trenches[m.World.TrenchId[i]];
                float want = def.OwnerTeam == m.World.Team[i] ? def.FacingYaw : def.FacingYaw + SimMath.Pi;
                if (math.abs(SimMath.WrapAngle(m.World.Yaw[i] - want)) < 0.2f) facing++;
            }
            Assert.Greater(onStep, 0, "with a target in front of them, the parapet posts are manned");
            Assert.AreEqual(onStep, facing, "a man on the step faces over the parapet, not at his target's bearing");
        }

        [Test]
        public void ReservePostsKeepTheirHeadsDown()
        {
            using var m = NewMatch();
            var men = Garrison(m);
            float3 front = m.World.Position[men[0]] + new float3(0f, 0f, 12f);
            for (int k = 0; k < 6; k++) m.World.Spawn(1, 0, front + new float3(k * 2f, 0f, 0f), 100000f, 0f, false);
            Run(m, 120);
            foreach (int i in men)
                if (m.World.PostKind[i] == 2)
                    Assert.AreNotEqual((byte)Stance.FireStep, m.World.StanceOf[i], $"slot {i} is in support, not at the parapet");
        }

        [Test]
        public void ThePostsAreTheSameOnEveryMachine()
        {
            ulong Play()
            {
                using var m = NewMatch();
                Garrison(m, 12, 700);
                return m.World.Hash();
            }
            Assert.AreEqual(Play(), Play(), "same seed, same posts, same hash");
        }
    }
}
