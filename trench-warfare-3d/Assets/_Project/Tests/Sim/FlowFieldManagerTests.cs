// Phase: A1 (implemented) — goal table: lazy creation and de-duplication, time-sliced rebuilds, rally goals that
// follow SetRally, and the tracked-vehicle field that crosses trenches where the infantry field needs links.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class FlowFieldManagerTests
    {
        static MatchSim NewMatch()
        {
            var cfg = SimConfig.Default;
            return MatchSim.CreateGreybox(cfg, combat: false);
        }

        static void Step(MatchSim m, params SimCommand[] cmds)
        {
            using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
            m.Step(arr);
        }

        [Test]
        public void Goals_AreCreatedLazily_AndDeduplicated()
        {
            using var m = NewMatch();
            var f = m.Fields;
            Assert.AreEqual(0, f.GoalCount);
            int a = f.GetGoal(GoalKey.Trench(1));
            int b = f.GetGoal(GoalKey.Trench(1));
            int c = f.GetGoal(GoalKey.Trench(1, NavMode.Tracked));
            int d = f.GetGoal(GoalKey.Objective(3));
            Assert.AreEqual(a, b, "same key → same goal");
            Assert.AreNotEqual(a, c, "nav mode is part of the key");
            Assert.AreNotEqual(a, d);
            Assert.AreEqual(3, f.GoalCount);
            Assert.IsFalse(f.IsReady(a), "a new goal is dirty until the manager steps");
            Step(m);
            Assert.IsTrue(f.IsReady(a));
        }

        [Test]
        public void DirtyFields_RebuildAtMostTwoPerTick_InGoalOrder()
        {
            using var m = NewMatch();
            var f = m.Fields;
            for (int k = 0; k < 5; k++) f.GetGoal(GoalKey.Cell(m.Map.NavIndex(10 + k, 50)));
            int Ready() { int n = 0; for (int g = 0; g < f.GoalCount; g++) if (f.IsReady(g)) n++; return n; }
            Assert.AreEqual(0, Ready());
            Step(m); Assert.AreEqual(2, Ready()); Assert.IsTrue(f.IsReady(0) && f.IsReady(1));
            Step(m); Assert.AreEqual(4, Ready());
            Step(m); Assert.AreEqual(5, Ready());
            f.MarkCostDirty(0);
            Assert.AreEqual(5, Ready(), "a dirty field keeps serving its last build");
            for (int g = 0; g < f.GoalCount; g++) Assert.AreEqual(1, f.Dirty[g]);
            Step(m); Step(m); Step(m);
            for (int g = 0; g < f.GoalCount; g++) Assert.AreEqual(0, f.Dirty[g]);
        }

        [Test]
        public void DirtyEveryTick_AllGoalsEventuallyRebuilt_NotJustTheFirstTwo()
        {
            // [N1] the finding's own words: before the fix "only goals 0 and 1 are ever rebuilt" once every goal is
            // dirty every tick, because the single pass always restarted the scan at goal 0.
            using var m = NewMatch();
            var f = m.Fields;
            for (int k = 0; k < 5; k++) f.GetGoal(GoalKey.Cell(m.Map.NavIndex(10 + k, 50)));
            Step(m); Step(m); Step(m);
            for (int g = 0; g < 5; g++) Assert.IsTrue(f.IsReady(g));

            var rebuiltEver = new bool[5];
            for (int tick = 0; tick < 6; tick++)
            {
                f.MarkCostDirty(0);
                var wasDirty = new bool[5];
                for (int g = 0; g < 5; g++) wasDirty[g] = f.Dirty[g] != 0;
                Step(m);
                for (int g = 0; g < 5; g++) if (wasDirty[g] && f.Dirty[g] == 0) rebuiltEver[g] = true;
            }
            for (int g = 0; g < 5; g++) Assert.IsTrue(rebuiltEver[g], $"[N1] goal {g} was never rebuilt while every goal stayed dirty");
        }

        [Test]
        public void DirtyEveryTick_ANewGoalIsNotStarvedByOlderStaleOnes()
        {
            // [N1] pass A serves a never-ready goal before the round-robin pass, so a goal created mid-barrage is
            // not left waiting behind goals that are merely stale.
            using var m = NewMatch();
            var f = m.Fields;
            for (int k = 0; k < 5; k++) f.GetGoal(GoalKey.Cell(m.Map.NavIndex(10 + k, 50)));
            Step(m); Step(m); Step(m);
            for (int g = 0; g < 5; g++) Assert.IsTrue(f.IsReady(g));

            f.MarkCostDirty(0);
            int fresh = f.GetGoal(GoalKey.Cell(m.Map.NavIndex(20, 50)));
            Step(m);
            Assert.IsTrue(f.IsReady(fresh), "[N1] a brand-new goal must be built the same tick it is dirtied, even while 5 older goals are also dirty");
        }

        [Test]
        public void GetGoal_ReturnsMinusOne_WhenTheTableIsFull_NeverThrows()
        {
            // [N3] a full table used to throw InvalidOperationException from inside Step/command handling, which is
            // a crash on one machine and a desync on the other in a lockstep tick.
            using var m = NewMatch();
            var f = m.Fields;
            for (int k = 0; k < FlowFieldManager.MaxCellGoals; k++) Assert.AreNotEqual(-1, f.GetGoal(GoalKey.Cell(m.Map.NavIndex(1 + k, 1))));
            Assert.AreEqual(-1, f.GetGoal(GoalKey.Cell(m.Map.NavIndex(99, 99))), "[N3] the cell reserve is exhausted");

            int remaining = FlowFieldManager.MaxGoals - FlowFieldManager.MaxCellGoals;
            int made = 0;
            for (int refId = 0; made < remaining; refId++)
            {
                made += f.GetGoal(GoalKey.Trench((short)refId)) >= 0 ? 1 : 0;
                if (made >= remaining) break;
                made += f.GetGoal(GoalKey.Trench((short)refId, NavMode.Tracked)) >= 0 ? 1 : 0;
                if (made >= remaining) break;
                made += f.GetGoal(GoalKey.Objective((short)refId)) >= 0 ? 1 : 0;
                if (made >= remaining) break;
                made += f.GetGoal(GoalKey.Objective((short)refId, NavMode.Tracked)) >= 0 ? 1 : 0;
            }
            Assert.AreEqual(FlowFieldManager.MaxGoals, f.GoalCount);
            Assert.AreEqual(-1, f.GetGoal(GoalKey.Trench(9999)), "[N3] the whole table is full: no throw, just -1");
        }

        [Test]
        public void RallyGoal_FollowsSetRally()
        {
            using var m = NewMatch();
            var f = m.Fields;
            int g = f.GetGoal(GoalKey.Rally(0));
            Step(m);
            var c0 = m.Map.NavCellOf(m.World.Rally[0]);
            Assert.AreEqual(0, f.Field(g).Integration[m.Map.NavIndex(c0.x, c0.y)], "field goal is the rally cell");

            var target = new float3(40f, 0f, 300f);
            Step(m, SimCommand.Rally(m.World.Tick, 0, target));
            var c1 = m.Map.NavCellOf(target);
            Assert.AreNotEqual(m.Map.NavIndex(c0.x, c0.y), m.Map.NavIndex(c1.x, c1.y));
            Assert.AreEqual(0, f.Field(g).Integration[m.Map.NavIndex(c1.x, c1.y)], "rebuilt for the new rally point in the same tick");
            Assert.AreNotEqual(0, f.Field(g).Integration[m.Map.NavIndex(c0.x, c0.y)]);
        }

        [Test]
        public void TrackedField_CrossesTrenchesDirectly_AndTheInfantryFieldGoesOverTheWall()
        {
            using var m = NewMatch();
            var map = m.Map;
            var f = m.Fields;
            short hq = f.EnemyHq(0);
            int inf = f.GetGoal(GoalKey.Objective(hq));
            int trk = f.GetGoal(GoalKey.Objective(hq, NavMode.Tracked));
            Step(m);
            var start = map.NavCellOf(map.Spawns[0].Pos);
            int startCell = map.NavIndex(start.x, start.y);

            // walk the tracked field: it may enter trench cells anywhere and must reach the goal
            var tf = f.Field(trk);
            int cell = startCell, steps = 0, trenchEntries = 0;
            while (tf.Integration[cell] != 0 && steps++ < map.NavWidth * map.NavLength)
            {
                byte d = tf.Direction[cell];
                Assert.AreNotEqual(FlowField.NoDirection, d);
                int x = cell % map.NavWidth, z = cell / map.NavWidth;
                var o = FlowField.Offset(d);
                int next = map.NavIndex(x + (int)System.Math.Round(o.x), z + (int)System.Math.Round(o.y));
                if ((map.NavLayers[next] & (byte)NavLayer.Trench) != 0 && (map.NavLayers[cell] & (byte)NavLayer.Trench) == 0) trenchEntries++;
                cell = next;
            }
            Assert.AreEqual(0, tf.Integration[cell], "tracked walk reaches the enemy HQ");
            Assert.GreaterOrEqual(trenchEntries, 2, "the tank crosses both trench lines");

            // infantry: a surface cell right behind a non-link trench cell points straight into the trench (over its wall:
            // until 2026-09-28 it could not, and pointed along the wall to the nearest ladder)
            var def = map.Trenches[0];
            int nonLink = -1;
            for (int c = 0; c < def.CellCount && nonLink < 0; c++)
            {
                int tc = map.TrenchCells[def.CellStart + c];
                if ((map.NavLayers[tc] & (byte)NavLayer.Link) == 0 && tc / map.NavWidth == map.TrenchCells[def.CellStart] / map.NavWidth) nonLink = tc;
            }
            Assert.GreaterOrEqual(nonLink, 0);
            int behind = nonLink - map.NavWidth;   // one cell toward team 0's spawn
            var inff = f.Field(inf);
            Assert.AreEqual(2, inff.Direction[behind], "the infantry field steps north into the trench where it meets it");
            Assert.AreNotEqual(FlowField.Unreachable, inff.Integration[behind]);
        }
    }
}
