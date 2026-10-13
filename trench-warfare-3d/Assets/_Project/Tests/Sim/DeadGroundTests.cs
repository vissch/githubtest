// Phase: A2 (dead ground removed 2026-10-13) — dead ground is gone (2026-10-13, the owner's word of 2026-10-06 and his pick of 2026-10-08): a man on foot in the
// open behind his own front trench is seen and shot at by the far side again, and in its place a shot at him hits half
// as often (CombatTables.ApproachShare, applied by DirectFireSystem with the other hit rules). The rule ends the
// moment he is in his trench or in front of it. The file keeps its name: docs/reference/tasks.md cites it.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class DeadGroundTests
    {
        static void Step(MatchSim m)
        {
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            m.Step(none);
        }

        static float TrenchZ(MatchSim m, short t, float x)
        {
            // the trench's z at this column, as TargetAcquisition measures it
            float sum = 0f; int n = 0;
            var def = m.Map.Trenches[t];
            int col = (int)(x / MapData.NavCellSize);
            for (int c = 0; c < def.CellCount; c++)
            {
                int cell = m.Map.TrenchCells[def.CellStart + c];
                if (cell % m.Map.NavWidth != col) continue;
                sum += m.Map.NavCellCenter(cell).z; n++;
            }
            Assert.Greater(n, 0, "setup: the trench crosses this column");
            return sum / n;
        }

        /// <summary>The battle scene's ground with an enemy machine gunner in his front trench at x 45, and one
        /// unkillable, unmoving rifleman of ours at <paramref name="behindOurs"/> metres behind our front trench
        /// (negative: in front of it). Returns how many ticks of 200 the gunner had him as his target.</summary>
        static int Watched(float behindOurs) => Run(behindOurs, 200, false, out _, out _);

        /// <summary>The same stand-off for <paramref name="ticks"/> ticks: how many of them the gunner had him as his
        /// target, and of <paramref name="shots"/> rounds how many were <paramref name="hits"/>. Same seed and same
        /// slots either way, so the two spots draw the same roll stream. <paramref name="bareGround"/> asserts that
        /// nothing but the ground is between them, which the hit-chance test needs and the sight test does not.</summary>
        static int Run(float behindOurs, int ticks, bool bareGround, out int hits, out int shots)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            var field = BattlefieldParams.ShelledForest(1917u); field.Bombardment = 0f;
            using var m = MatchSim.CreateBattlefield(cfg, field);
            var w = m.World;
            short ours = m.Fields.FrontTrench(0), theirs = m.Fields.FrontTrench(1);
            var e = w.Roster[1 * RosterEntry.SlotCount + 0];
            int gunner = w.Spawn(1, InfantryArchetype.Machinegunner, new float3(45f, 0f, TrenchZ(m, theirs, 45f) + 6f), 100000f, e.Speed, false);
            w.MaxHp[gunner] = 100000f;
            w.GoalId[gunner] = m.Fields.GetGoal(GoalKey.Trench(theirs));
            for (int k = 0; k < 600 && w.TrenchId[gunner] < 0; k++) Step(m);
            Assert.GreaterOrEqual(w.TrenchId[gunner], 0, "setup: the gunner is in his trench");
            float x = w.Position[gunner].x;
            int man = w.Spawn(0, InfantryArchetype.Rifle, new float3(x, 0f, TrenchZ(m, ours, x) - behindOurs), 100000f, 0f, false);
            w.MaxHp[man] = 100000f;
            float range = math.distance(w.Position[man].xz, w.Position[gunner].xz);
            Assert.Less(range, CombatTables.WeaponFor(InfantryArchetype.Machinegunner).RangeMax, "setup: he is inside the gun's range");
            int cell = (int)(w.Position[man].z / MapData.NavCellSize) * m.Map.NavWidth + (int)(w.Position[man].x / MapData.NavCellSize);
            if (bareGround)
            {
                Assert.AreEqual(0, m.Map.CellCover[cell], "setup: nothing stands next to him");
                Assert.AreEqual(0, m.Map.NavLayers[cell] & (byte)NavLayer.Crater, "setup: he is not in a crater");
            }
            int seen = 0; hits = 0; shots = 0;
            for (int k = 0; k < ticks; k++)
            {
                Step(m);
                if (w.TargetSlot[gunner] == man) seen++;
                var evs = w.Events.Events;
                for (int ei = 0; ei < evs.Length; ei++)
                {
                    if (evs[ei].A != gunner) continue;
                    if (evs[ei].Type == SimEventType.Shot) shots++;
                    else if (evs[ei].Type == SimEventType.Hit && evs[ei].B == man) hits++;
                }
            }
            Assert.AreEqual(0f, math.length(w.Velocity[man]), 1e-3f, "setup: he stands still");
            Assert.AreEqual(0u, w.Flags[man] & (uint)UnitFlags.InTrench, "setup: he is in the open, not in a trench");
            return seen;
        }


        /// <summary>Reinforcements walking up from the spawn to their own front trench, against garrisons on both
        /// sides: how many of ten minutes' worth arrive alive, and how many get into the trench (the owner,
        /// 2026-10-06: "the man werent able to enter the trench"). A report, not a verdict: run it on each rule.</summary>
        [Test, Explicit, Category("Long")]
        public void Report_ReinforcementsThatArriveAlive()
        {
            uint[] seeds = { 1917u, 1918u, 7u, 42u, 101u, 404u, 1234u, 9999u };
            TestContext.Out.WriteLine("seed | sent | alive | in trench | killed by fire");
            int totSent = 0, totAlive = 0, totIn = 0, totKilled = 0;
            foreach (uint seed in seeds)
            {
                var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.Seed = seed;
                var field = BattlefieldParams.ShelledForest(seed); field.Bombardment = 0f;
                using var m = MatchSim.CreateBattlefield(cfg, field);
                var w = m.World;
                short ours = m.Fields.FrontTrench(0), theirs = m.Fields.FrontTrench(1);
                int ourGoal = m.Fields.GetGoal(GoalKey.Trench(ours)), theirGoal = m.Fields.GetGoal(GoalKey.Trench(theirs));
                // a garrison of ten in each front trench, half machine gunners
                for (byte team = 0; team < 2; team++)
                {
                    short tr = team == 0 ? ours : theirs;
                    int goal = team == 0 ? ourGoal : theirGoal;
                    for (int k = 0; k < 10; k++)
                    {
                        byte arch = k < 5 ? InfantryArchetype.Machinegunner : InfantryArchetype.Rifle;
                        var e = w.Roster[team * RosterEntry.SlotCount + 0];
                        float x = 30f + k * 6f;
                        int s = w.Spawn(team, arch, new float3(x, 0f, TrenchZ(m, tr, x)), e.Hp, e.Speed, false);
                        w.GoalId[s] = goal;
                    }
                }
                for (int k = 0; k < 600; k++) Step(m);   // let the garrisons settle into their trenches

                int ticks = (int)(600f / m.World.Config.TickSeconds);
                int every = (int)(30f / m.World.Config.TickSeconds);
                var sent = new System.Collections.Generic.List<int2>();   // slot, generation at spawn
                int killedByFire = 0;
                for (int k = 0; k < ticks; k++)
                {
                    if (k % every == 0)
                        for (int n = 0; n < 10; n++)
                        {
                            var e = w.Roster[0 * RosterEntry.SlotCount + 0];
                            float3 at = w.Init.SpawnA + new float3((n - 5) * 2f, 0f, 0f);
                            int s = w.Spawn(0, InfantryArchetype.Rifle, at, e.Hp, e.Speed, false);
                            w.GoalId[s] = ourGoal;
                            sent.Add(new int2(s, w.Generation[s]));
                        }
                    Step(m);
                    var evs = w.Events.Events;
                    for (int ei = 0; ei < evs.Length; ei++)
                        if (evs[ei].Type == SimEventType.Death && evs[ei].B >= 0 && w.Team[evs[ei].A] == 0) killedByFire++;
                }
                int alive = 0, inTrench = 0;
                foreach (var s in sent)
                {
                    if (!w.IsAlive(s.x) || w.Generation[s.x] != s.y) continue;
                    alive++;
                    if (w.TrenchId[s.x] >= 0) inTrench++;
                }
                TestContext.Out.WriteLine($"{seed} | {sent.Count} | {alive} | {inTrench} | {killedByFire}");
                totSent += sent.Count; totAlive += alive; totIn += inTrench; totKilled += killedByFire;
            }
            TestContext.Out.WriteLine($"ALL | {totSent} | {totAlive} | {totIn} | {totKilled}");
        }

        [Test]
        public void AManComingUpBehindHisLines_IsAcquiredByTheEnemyGunner()
        {
            Assert.Greater(Watched(30f), 20, "dead ground is gone: thirty metres behind his own trench he is still seen (48 ticks of 200 when this was measured; 0 while dead ground stood)");
        }

        /// <summary>The rule itself, as a table: half from the trench line back, whole from the line forward.</summary>
        [Test]
        public void TheApproachShare_IsHalfBehindTheLineAndWholeInFrontOfIt()
        {
            Assert.AreEqual(0.5f, CombatTables.ApproachShare(0.01f), 1e-6f, "a step behind the line he is half as easy to hit");
            Assert.AreEqual(0.5f, CombatTables.ApproachShare(30f), 1e-6f);
            Assert.AreEqual(1f, CombatTables.ApproachShare(0f), 1e-6f, "the rule ends on the line itself");
            Assert.AreEqual(1f, CombatTables.ApproachShare(-20f), 1e-6f, "out in front of it he is fair game");
        }

        /// <summary>The owner's pick of 2026-10-08 in a match: he is shot at behind his own lines, and hit distinctly
        /// less often there. Measured 0.66, not 0.50: the halving is exact (the test above), but the man 8 m in front
        /// of the trench is nearer the gun and takes a stance of his own, and both of those move the rate as well. The
        /// band is wide enough for that and still fails if the halving goes.</summary>
        [Test]
        public void BehindHisOwnFrontTrench_AShotHitsHalfAsOften()
        {
            Run(8f, 6000, true, out int behind, out int behindShots);
            Run(-8f, 6000, true, out int ahead, out int aheadShots);
            Assert.Greater(ahead, 0, "setup: in front of his trench he is hit");
            Assert.Greater(behind, 0, "he is shot at behind it too: dead ground is gone");
            Assert.Greater(behindShots, 0); Assert.Greater(aheadShots, 0);
            // per round fired, so the few ticks' difference in how long the gunner held him does not count
            float share = (behind / (float)behindShots) / (ahead / (float)aheadShots);
            TestContext.Out.WriteLine($"approach share: {behind}/{behindShots} behind, {ahead}/{aheadShots} in front = {share:0.000}");
            Assert.That(share, Is.InRange(0.45f, 0.80f),
                $"half as easy to hit: {behind} of {behindShots} rounds behind his trench, {ahead} of {aheadShots} in front = {share:0.00}");
        }

        [Test]
        public void TheSameManInNoMansLand_IsSeen()
        {
            Assert.Greater(Watched(-20f), 100, "twenty metres in front of his trench he is fair game");
        }
    }
}
