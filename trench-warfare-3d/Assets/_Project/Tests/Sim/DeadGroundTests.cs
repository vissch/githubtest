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


        /// <summary>The sign the approach rule measures <paramref name="team"/>'s distances with: his own front
        /// trench's z plus <c>metres * sign</c> is a spot that many metres behind his line (TargetAcquisitionSystem's
        /// HomeSign, which DirectFire reads).</summary>
        static float HomeSign(MatchSim m, int team) => team == 0 ? m.Acquisition.HomeSign.x : m.Acquisition.HomeSign.y;

        /// <summary>A stand-off at a chosen spot either side of one team's front trench: an unkillable, unmoving man
        /// of <paramref name="team"/> <paramref name="behindOurs"/> metres behind that team's own front trench, and an
        /// unkillable, unmoving enemy machine gunner at the same column <paramref name="shooterBehindOurs"/> metres
        /// behind the same line (negative: on the far side of it, where the rule applies). A gunner, not a rifleman,
        /// because a rifleman within 17.6 m of a trench man throws a bomb instead of firing (CombatTables.GrenadeRange).
        /// Counts that gunner's rounds and his hits on the man. <paramref name="inTrenchCells"/> stands the man in his
        /// front trench's own cells instead, which is how the in-trench exemption is reached.</summary>
        static void Near(int team, float behindOurs, float shooterBehindOurs, int ticks, out int hits, out int shots,
                         bool inTrenchCells = false)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            var field = BattlefieldParams.ShelledForest(1917u); field.Bombardment = 0f;
            using var m = MatchSim.CreateBattlefield(cfg, field);
            var w = m.World;
            Step(m);   // TargetAcquisitionSystem builds FrontZ and HomeSign on its first step
            short ours = m.Fields.FrontTrench((byte)team);
            const float x = 45f;
            float fz = TrenchZ(m, ours, x), s = HomeSign(m, team);
            float3 at = new float3(x, 0f, inTrenchCells ? fz : fz + behindOurs * s);
            int man = w.Spawn((byte)team, InfantryArchetype.Rifle, at, 100000f, 0f, false);
            w.MaxHp[man] = 100000f;
            int enemy = 1 - team;
            int gunner = w.Spawn((byte)enemy, InfantryArchetype.Machinegunner,
                new float3(x, 0f, fz + shooterBehindOurs * s), 100000f, 0f, false);
            w.MaxHp[gunner] = 100000f;
            float range = math.distance(w.Position[man].xz, w.Position[gunner].xz);
            Assert.Less(range, CombatTables.WeaponFor(InfantryArchetype.Machinegunner).RangeMax, "setup: he is inside the gun's range");
            hits = 0; shots = 0;
            for (int k = 0; k < ticks; k++)
            {
                Step(m);
                var evs = w.Events.Events;
                for (int ei = 0; ei < evs.Length; ei++)
                {
                    if (evs[ei].A != gunner) continue;
                    if (evs[ei].Type == SimEventType.Shot) shots++;
                    else if (evs[ei].Type == SimEventType.Hit && evs[ei].B == man) hits++;
                }
            }
            bool stood = (w.Flags[man] & (uint)UnitFlags.InTrench) != 0;
            Assert.AreEqual(inTrenchCells, stood, inTrenchCells ? "setup: he stands in his trench" : "setup: he is in the open");
        }

        /// <summary>Hits per round fired of a far-side gunner against one who has broken through the same line, both
        /// at the same <paramref name="gap"/> metres from the same man. About 0.5 when the rule holds; about 1 when it fires on both.</summary>
        static float FarSideOverBrokenThrough(int team, int ticks, float behind, float gap, bool inTrenchCells = false)
        {
            Near(team, behind, behind - gap, ticks, out int far, out int farShots, inTrenchCells);
            Near(team, behind, behind + gap, ticks, out int through, out int throughShots, inTrenchCells);
            Assert.Greater(farShots, 0, "setup: the far-side gunner fires");
            Assert.Greater(throughShots, 0, "setup: the gunner who has broken through fires");
            Assert.Greater(through, 0, "setup: the gunner who has broken through hits him");
            float ratio = (far / (float)farShots) / (through / (float)throughShots);
            TestContext.Out.WriteLine($"team {team} far side {far}/{farShots}, broken through {through}/{throughShots} = {ratio:0.000} (in trench: {inTrenchCells})");
            return ratio;
        }

        /// <summary>N1: the half rule holds only against a shooter on the far side of the man's own front trench, as
        /// the dead-ground rule it replaced did. An enemy who has broken through that line - a flamethrower 9 m behind
        /// it - is past the parapet he would be sheltered by, and hits at the full chance.</summary>
        [Test]
        public void AnEnemyWhoHasBrokenThrough_HitsAtTheFullChance()
        {
            Assert.That(FarSideOverBrokenThrough(0, 4000, 4.5f, 9f), Is.InRange(0.30f, 0.80f),
                "half from the far side of his line, whole from behind it");
        }

        /// <summary>The same for the other team, so the HomeSign.y half of the sign pair is walked: their front
        /// trench runs the other way down the field, and the rule reads their own sign.</summary>
        [Test]
        public void TheOtherTeamsMan_IsHalvedByTheSameRule()
        {
            Assert.That(FarSideOverBrokenThrough(1, 4000, 4.5f, 9f), Is.InRange(0.30f, 0.80f),
                "the rule is measured with the target's own sign");
        }

        /// <summary>The in-trench exemption: a man in his own front trench's cells is not touched by the approach
        /// rule, so a far-side gunner and one who has broken through hit him alike. (The rule's second guard,
        /// <c>TrenchId[t] &lt; 0</c>, is belt and braces: a man in a trench's cells carries UnitFlags.InTrench and the
        /// chance code sends him down the trench-cover branch before the approach rule is reached.) He is below the
        /// rim, so both gunners stand 6 m off: CombatTables.BelowRimRevealRange is 8 m.</summary>
        [Test]
        public void AManInHisTrench_IsNotTouchedByTheApproachRule()
        {
            Assert.That(FarSideOverBrokenThrough(0, 4000, 0f, 6f, inTrenchCells: true), Is.InRange(0.80f, 1.25f),
                "in his trench the rule does not apply: trench cover, from either side alike");
        }

        /// <summary>The machine exemption, which is a structural one: a machine is resolved by the armour rules and
        /// nothing else, so the man's hit chance - and with it the approach rule - is never reached. Shown by the
        /// shape of the rounds: a Tusk 2 m behind its own front trench with a team-0 man 4 m off on the far side of
        /// that line, exactly where the rule would halve a man on foot, takes only close-assault fire (SimEventType
        /// .Shot with Scalar 1, the bundle of grenades), never a round resolved by the chance code (Scalar 0). This is
        /// why the <c>UnitFlags.Vehicle</c> guard beside the rule can never fire, and the ratio tests above have no
        /// machine counterpart: there is no chance to halve.</summary>
        [Test]
        public void AMachineBehindItsOwnFront_IsOnlyEverCloseAssaulted()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            var field = BattlefieldParams.ShelledForest(1917u); field.Bombardment = 0f;
            using var m = MatchSim.CreateBattlefield(cfg, field);
            var w = m.World;
            const float x = 45f;
            int man = w.Spawn(0, InfantryArchetype.Rifle, new float3(x, 0f, 20f), 100000f, 0f, false);
            w.MaxHp[man] = 100000f;
            Step(m);   // a living man, so TargetAcquisitionSystem steps at all and builds FrontZ
            short theirs = m.Fields.FrontTrench(1);
            float fz = TrenchZ(m, theirs, x), s = HomeSign(m, 1);
            int tank = w.Spawn(1, VehicleArchetype.Tusk, new float3(x, 0f, fz + 2f * s), 100000f, 0f, true);
            w.MaxHp[tank] = 100000f;
            w.Position[man] = new float3(x, 0f, fz - 2f * s);   // 4 m off, inside CloseAssaultRange, on the far side
            int bundles = 0, chanceRounds = 0;
            for (int k = 0; k < 600; k++)
            {
                Step(m);
                var evs = w.Events.Events;
                for (int ei = 0; ei < evs.Length; ei++)
                {
                    if (evs[ei].A != man || evs[ei].B != tank || evs[ei].Type != SimEventType.Shot) continue;
                    if (evs[ei].Scalar == 1f) bundles++; else chanceRounds++;
                }
            }
            Assert.Greater(bundles, 0, "setup: he goes for the machine with his grenades");
            Assert.AreEqual(0, chanceRounds, "a machine is never resolved by the man's hit chance, so the approach rule cannot reach it");
        }

        /// <summary>The no-fronts exemption: with no TargetAcquisitionSystem there is no front line to measure from,
        /// so both metres come back 0 and the share is whole. Covered by the table: DirectFire reads the fronts from
        /// that system, and a match built without it has no target acquisition to give anyone a target at all.</summary>
        [Test]
        public void WithNoFrontsToMeasureFrom_TheShareIsWhole()
        {
            Assert.AreEqual(1f, CombatTables.ApproachShare(0f, 0f), 1e-6f, "no front trench: both metres come back 0");
        }

        /// <summary>N6: a front that moved. When a trench changes hands the far side's front trench moves, and the
        /// rule measures from the new line the next tick (TargetAcquisitionSystem.UpdateFronts).</summary>
        [Test]
        public void AFrontThatMovedAfterACapture_IsMeasuredFromTheNewLine()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            var field = BattlefieldParams.ShelledForest(1917u); field.Bombardment = 0f;
            using var m = MatchSim.CreateBattlefield(cfg, field);
            const float x = 45f;
            m.World.Spawn(0, InfantryArchetype.Rifle, new float3(x, 0f, 20f), 100000f, 0f, false);
            Step(m);   // a living man, so TargetAcquisitionSystem steps at all and builds FrontZ
            short theirs = m.Fields.FrontTrench(1);
            int col = (int)(x / MapData.NavCellSize);
            int wdt = m.Map.NavWidth;
            float before = m.Acquisition.FrontZ[0 * wdt + col];
            float captured = TrenchZ(m, theirs, x);
            TestContext.Out.WriteLine($"front trenches: ours {m.Fields.FrontTrench(0)} at {before}, theirs {theirs} at {captured}");
            Assert.AreNotEqual(captured, before, "setup: their front trench is not already ours");
            var ts = m.Fields.Trenches[theirs]; ts.OwnerTeam = 0; m.Fields.Trenches[theirs] = ts;   // a capture, as SectorControl.Captured does
            Step(m);
            TestContext.Out.WriteLine($"after the capture: ours {m.Fields.FrontTrench(0)}, owner {m.Fields.Trenches[theirs].OwnerTeam}, front z {m.Acquisition.FrontZ[0 * wdt + col]}");
            Assert.AreEqual(captured, m.Acquisition.FrontZ[0 * wdt + col], 0.51f, "our front is the trench we took");
            // the same man, measured from the old line and from the new one, with the gunner beyond the new line:
            // whole before the capture (he was out in front of our front trench), half after it
            float s = HomeSign(m, 0);
            float mid = (before + captured) * 0.5f;
            float gunnerZ = captured - 9f * s;   // on the far side of the new line
            Assert.AreEqual(1f, CombatTables.ApproachShare((mid - before) * s, (gunnerZ - before) * s), 1e-6f,
                "before the capture he was out in front of our line");
            Assert.AreEqual(0.5f, CombatTables.ApproachShare((mid - captured) * s, (gunnerZ - captured) * s), 1e-6f,
                "after it he is behind it, with the gun on the far side");
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
                var sentSlots = new System.Collections.Generic.HashSet<int2>();   // the same, for the death count
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
                            sent.Add(new int2(s, w.Generation[s])); sentSlots.Add(new int2(s, w.Generation[s]));
                        }
                    Step(m);
                    var evs = w.Events.Events;
                    for (int ei = 0; ei < evs.Length; ei++)
                        if (evs[ei].Type == SimEventType.Death && evs[ei].B >= 0 && w.Team[evs[ei].A] == 0
                            && sentSlots.Contains(new int2(evs[ei].A, w.Generation[evs[ei].A])))
                            killedByFire++;   // only the men sent, so the garrison's dead are out of the column (N3)
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

        /// <summary>The rule itself, as a table: half when he is behind his own trench line and the shooter is on the
        /// far side of it, whole in every other case.</summary>
        [Test]
        public void TheApproachShare_IsHalfBehindTheLineAndWholeInFrontOfIt()
        {
            Assert.AreEqual(0.5f, CombatTables.ApproachShare(0.01f, -0.01f), 1e-6f, "a step behind the line, shot at from the far side: half as easy to hit");
            Assert.AreEqual(0.5f, CombatTables.ApproachShare(30f, -9f), 1e-6f);
            Assert.AreEqual(1f, CombatTables.ApproachShare(30f, 9f), 1e-6f, "a shooter who has broken through the line hits at the full chance");
            Assert.AreEqual(1f, CombatTables.ApproachShare(0f, -9f), 1e-6f, "the rule ends on the line itself");
            Assert.AreEqual(1f, CombatTables.ApproachShare(-20f, -9f), 1e-6f, "out in front of it he is fair game");
            Assert.AreEqual(1f, CombatTables.ApproachShare(0f, 0f), 1e-6f, "no front trench to measure from");
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
