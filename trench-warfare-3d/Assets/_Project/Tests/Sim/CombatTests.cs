// Phase: A2 / A3 core (implemented) — a garrison shoots an assault coming over open ground and wins the exchange,
// hold-fire keeps a garrison silent and out of sight, fire suppresses and suppression decays, a garrison spreads
// along its trench, an undefended trench is captured and the HQ behind it ends the match, and all of it is
// deterministic.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Nav;

namespace TW.Tests
{
    public class CombatTests
    {
        const int Rifleman = 0, MachineGunner = 2;

        static MatchSim NewMatch(uint seed = 0xC0FFEE)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.Seed = seed;
            return MatchSim.CreateGreybox(cfg);
        }

        static void Step(MatchSim m, params SimCommand[] cmds)
        {
            using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
            m.Step(arr);
        }

        static int Alive(SimWorld w, byte team)
        {
            int n = 0;
            for (int i = 0; i < w.HighWater; i++) if (w.IsAlive(i) && w.Team[i] == team) n++;
            return n;
        }

        static float TrenchZ(MatchSim m, short trench)
            => m.Map.NavCellCenter(m.Map.TrenchCells[m.Map.Trenches[trench].CellStart]).z;

        /// <summary>Riflemen of <paramref name="team"/> placed in a row at world Z, already ordered to assault <paramref name="goalTrench"/>.</summary>
        static void SpawnAssault(MatchSim m, byte team, int count, float z, short goalTrench)
        {
            int goal = m.Fields.GetGoal(GoalKey.Trench(goalTrench));
            for (int k = 0; k < count; k++)
            {
                int slot = m.World.Spawn(team, Rifleman, new float3(120f + k * 3f, 0f, z), 100f, 3f, false);
                m.World.GoalId[slot] = goal;
                m.World.Flags[slot] |= (uint)UnitFlags.Exposed;
            }
        }

        /// <summary>Riflemen of <paramref name="team"/> garrisoned in <paramref name="trench"/>: deployed the normal way and walked in.</summary>
        static void Garrison(MatchSim m, byte team, short trench, int count, int rosterSlot = Rifleman)
        {
            int want = m.Fields.Trenches[trench].GarrisonCount + count;
            for (int k = 0; k < count; k++) Step(m, SimCommand.Deploy(m.World.Tick, team, rosterSlot));
            for (int t = 0; t < 2000 && m.Fields.Trenches[trench].GarrisonCount < want; t++) Step(m);
            Assert.AreEqual(want, m.Fields.Trenches[trench].GarrisonCount, "setup: garrison did not form");
        }

        [Test]
        public void Garrison_ShootsAnAssaultInTheOpen_AndWinsTheExchange()
        {
            using var m = NewMatch();
            Garrison(m, 1, 1, 8);
            Garrison(m, 1, 1, 2, MachineGunner);   // rifles alone kill but barely suppress: 0.5 rounds a second against 8 points of decay
            SpawnAssault(m, 0, 10, TrenchZ(m, 1) - 110f, 1);
            bool shot = false, suppressed = false;
            float maxSuppression = 0f; int mgShots = 0;
            for (int t = 0; t < 900; t++)
            {
                Step(m);
                for (int i = 0; i < m.World.HighWater; i++) if (m.World.IsAlive(i) && m.World.Team[i] == 0) maxSuppression = math.max(maxSuppression, m.World.Suppression[i]);
                var ev = m.World.Events.Events;
                for (int e = 0; e < ev.Length; e++)
                {
                    if (ev[e].Type == SimEventType.Shot) { shot = true; if (m.World.Archetype[ev[e].A] == MachineGunner) mgShots++; }
                    if (ev[e].Type == SimEventType.Suppressed || ev[e].Type == SimEventType.Pinned) suppressed = true;
                }
            }
            Assert.IsTrue(shot, "the garrison must open fire");
            Assert.IsTrue(suppressed, $"machine-gun fire must suppress men in the open (max {maxSuppression:F0}, MG shots {mgShots})");
            int attackers = Alive(m.World, 0), defenders = Alive(m.World, 1);
            Assert.Less(attackers, 10, "an assault over open ground takes losses");
            Assert.Greater(defenders, attackers, "men on the fire-step beat the same number in the open");
            Assert.Greater(m.Fire.Kills[1], 0);
        }

        // ---- the standoff between two dug-in lines -----------------------------------------------------------

        /// <summary>
        /// ShelledForest, because it is the ONLY map on which this question can be asked. Front-to-front
        /// separation, from the generators: greybox 560 m (two trenches, Z=120 and length-120), playtest 200 m,
        /// ShelledForest 100 m - against a 130 m rifle. On the other two the lines cannot reach each other and a
        /// standoff would be a fact about the map rather than about stance.
        /// </summary>
        static MatchSim NewBattle(uint seed = 0xC0FFEE)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.Seed = seed;
            return MatchSim.CreateBattlefield(cfg, TW.Sim.Terrain.BattlefieldParams.ShelledForest(1917));
        }

        /// <summary>
        /// Puts a team in its FRONT line the way the game does. Deploying garrisons the REAR trench - that is
        /// where DefaultGoal sends everyone - so the men are then ordered forward, and the trench chain runs
        /// rear -> front for both sides. Seeding men onto front-line cells directly does NOT work: a man is
        /// garrisoned only when the trench is his goal, so placed men simply walk back out.
        /// </summary>
        static void HoldTheFrontLine(MatchSim m, byte team, int count)
        {
            short rear = m.Fields.RearTrench(team), front = m.Fields.FrontTrench(team);
            int want = m.Fields.Trenches[rear].GarrisonCount + count;
            for (int k = 0; k < count; k++) Step(m, SimCommand.Deploy(m.World.Tick, team, Rifleman));
            for (int t = 0; t < 4000 && m.Fields.Trenches[rear].GarrisonCount < want; t++) Step(m);
            Assert.AreEqual(want, m.Fields.Trenches[rear].GarrisonCount,
                $"setup: team {team} never formed up in its rear trench {rear}");

            Step(m, new SimCommand { Player = team, Type = CommandType.TrenchAdvance, A = rear, B = 0 });
            for (int t = 0; t < 4000 && m.Fields.Trenches[front].GarrisonCount < count; t++) Step(m);
            Assert.Greater(m.Fields.Trenches[front].GarrisonCount, count / 2,
                $"setup: team {team} was ordered from trench {rear} to {front} and only " +
                $"{m.Fields.Trenches[front].GarrisonCount} of {count} arrived");
        }

        static int WithTargets(MatchSim m)
        {
            int n = 0;
            for (int i = 0; i < m.World.HighWater; i++)
                if (m.World.IsAlive(i) && m.World.TargetSlot[i] >= 0) n++;
            return n;
        }

        static int OnTheStep(MatchSim m)
        {
            int n = 0;
            for (int i = 0; i < m.World.HighWater; i++)
                if (m.World.IsAlive(i) && m.World.StanceOf[i] == (byte)Stance.FireStep) n++;
            return n;
        }

        /// <summary>
        /// Two dug-in lines 100 m apart, inside each other's rifle range, do not shoot at each other - and that is
        /// a CIRCLE between two constants in two files that never mention one another:
        ///
        ///   TargetAcquisition.cs:111  an in-trench man is untargetable past BelowRimRevealRange (8 m) unless his
        ///                             stance is FireStep;
        ///   MovementSystem.cs:224     a garrisoned man takes FireStep only if he already has a target.
        ///
        /// He needs a target to stand up and has to be standing up to be one. Nothing holds either line, and each
        /// is a plausible thing to edit alone.
        ///
        /// This does not say the standoff is RIGHT - that is the owner's call and it is open. It says the standoff
        /// is what the code currently means, so that changing it has to be a decision rather than an accident.
        ///
        /// The range is asserted FIRST. On the greybox these two trenches are 560 m apart and this test would have
        /// passed for a reason that has nothing to do with stance.
        /// </summary>
        [Test]
        public void TwoDugInLinesDoNotEngageEachOther_BecauseNeitherCanBecomeATarget()
        {
            using var m = NewBattle();
            short ta = m.Fields.FrontTrench(0), tb = m.Fields.FrontTrench(1);
            float apart = math.abs(TrenchZ(m, ta) - TrenchZ(m, tb));
            float rifle = CombatTables.WeaponFor(Rifleman).RangeMax;
            Assert.Less(apart, rifle,
                $"setup: the front lines are {apart:F0} m apart against a {rifle:F0} m rifle, so a standoff here " +
                "would prove nothing about stance and everything about the map");

            HoldTheFrontLine(m, 0, 8);
            HoldTheFrontLine(m, 1, 8);
            for (int t = 0; t < 400; t++) Step(m);

            Assert.AreEqual(0, OnTheStep(m),
                $"{OnTheStep(m)} men mounted the fire step with nobody in the open to shoot at: the mount " +
                "condition at MovementSystem.cs:224 has changed and the standoff no longer holds");
            // or this passes just as well for two lines that wiped each other out
            Assert.Greater(m.Fields.Trenches[ta].GarrisonCount, 0, "team 0 is still holding its front line");
            Assert.Greater(m.Fields.Trenches[tb].GarrisonCount, 0, "team 1 is still holding its front line");
            Assert.AreEqual(0, WithTargets(m),
                $"{WithTargets(m)} men acquired a target across {apart:F0} m of no man's land. Two dug-in lines are " +
                "meant to be invisible to each other (TargetAcquisition.cs:111); if this fires, BelowRimRevealRange " +
                "or the mount condition moved and trench warfare has become an open-field firefight");
        }

        /// <summary>
        /// The other half of the same contract, and the half that makes the standoff a design rather than a
        /// deadlock: a man in the OPEN is targetable at range whatever his stance, so the garrison that sees him
        /// acquires him - and acquiring is exactly what puts men on the fire step. An attack is what stands a
        /// trench up.
        ///
        /// If this goes red while the test above stays green, the circle has closed for real and no garrison can
        /// engage anything, ever.
        /// </summary>
        [Test]
        public void AnAssaultInTheOpenIsWhatStandsAGarrisonUp()
        {
            using var m = NewMatch();
            Garrison(m, 1, 1, 8);
            for (int t = 0; t < 200; t++) Step(m);
            Assert.AreEqual(0, OnTheStep(m), "a garrison with nobody to shoot at keeps its head down");

            SpawnAssault(m, 0, 10, TrenchZ(m, 1) - 60f, 1);
            // Accumulated over the run, NOT sampled at the end. A target is transient state: at 60 m eight rifles
            // kill ten men in well under 400 ticks, and once they are dead nobody has a target again - so the
            // end-state snapshot reads exactly the same as a garrison that never woke up. The first version of
            // this test measured that snapshot and reported "acquired by nobody" for a fight it had just won.
            int everTargets = 0, everStep = 0;
            for (int t = 0; t < 400; t++)
            {
                Step(m);
                everTargets = math.max(everTargets, WithTargets(m));
                everStep = math.max(everStep, OnTheStep(m));
            }

            Assert.Greater(everTargets, 0,
                "men advancing in the open in front of a garrison were acquired by nobody, so nothing can break " +
                "the standoff and the two constants form a real deadlock");
            Assert.Greater(everStep, 0,
                "somebody acquired them but nobody mounted the fire step, so MovementSystem.cs:224 no longer turns " +
                "a target into a man at the parapet and Stance.FireStep is unreachable in play");
        }

        [Test]
        public void HoldFire_KeepsTheGarrisonSilent_AndOutOfSight()
        {
            using var m = NewMatch();
            Garrison(m, 1, 1, 6);
            Step(m, new SimCommand { Player = 1, Type = CommandType.TrenchHoldFire, A = 1, B = 1 });
            SpawnAssault(m, 0, 6, TrenchZ(m, 1) - 100f, 1);
            // stop the assault 100 m out so nobody reaches the 8 m reveal range
            for (int i = 0; i < m.World.HighWater; i++) if (m.World.Team[i] == 0) { m.World.GoalId[i] = -1; m.World.TrenchId[i] = -1; m.World.Speed[i] = 0f; }
            for (int t = 0; t < 300; t++)
            {
                Step(m);
                var ev = m.World.Events.Events;
                for (int e = 0; e < ev.Length; e++) Assert.AreNotEqual(SimEventType.Shot, ev[e].Type, "nobody can engage: one side holds fire below the rim, the other cannot see it");
            }
            Assert.AreEqual(6, Alive(m.World, 0));
            Assert.AreEqual(6, Alive(m.World, 1));
        }

        [Test]
        public void Suppression_DecaysWhenTheFiringStops()
        {
            using var m = NewMatch();
            int slot = m.World.Spawn(0, Rifleman, new float3(150f, 0f, 300f), 100f, 3f, false);
            m.World.Suppression[slot] = 90f;
            Step(m);
            Assert.AreEqual((byte)Stance.Pinned, m.World.StanceOf[slot], "above 85 a man in the open is pinned");
            for (int t = 0; t < 100; t++) Step(m);   // 5 s at 8 per second
            Assert.Less(m.World.Suppression[slot], 60f);
            Assert.AreNotEqual((byte)Stance.Pinned, m.World.StanceOf[slot]);
            for (int t = 0; t < 200; t++) Step(m);
            Assert.AreEqual(0f, m.World.Suppression[slot]);
        }

        [Test]
        public void Garrison_SpreadsAlongItsTrench()
        {
            using var m = NewMatch();
            Garrison(m, 0, 0, 24);
            for (int t = 0; t < 600; t++) Step(m);
            var w = m.World;
            float minX = float.MaxValue, maxX = float.MinValue, closest = float.MaxValue;
            for (int a = 0; a < w.HighWater; a++)
            {
                if (!w.IsAlive(a) || w.TrenchId[a] != 0) continue;
                minX = math.min(minX, w.Position[a].x); maxX = math.max(maxX, w.Position[a].x);
                Assert.AreEqual(0, m.Map.NavLayers[m.Map.NavIndex(m.Map.NavCellOf(w.Position[a]).x, m.Map.NavCellOf(w.Position[a]).y)] & (byte)TW.Sim.Terrain.NavLayer.Link,
                    "nobody rests on a ladder");
                for (int b = a + 1; b < w.HighWater; b++)
                    if (w.IsAlive(b) && w.TrenchId[b] == 0) closest = math.min(closest, math.distance(w.Position[a], w.Position[b]));
            }
            Assert.Greater(maxX - minX, 14f, "24 men do not stay in one clump by the ladder");
            Assert.Greater(closest, 1.2f, "men stand apart along the trench");
        }

        [Test]
        public void UndefendedTrench_IsCaptured_AndTheHqBehindItEndsTheMatch()
        {
            using var m = NewMatch();
            SpawnAssault(m, 0, 5, TrenchZ(m, 1) - 30f, 1);
            bool trenchCaptured = false;
            for (int t = 0; t < 800 && !trenchCaptured; t++)
            {
                Step(m);
                var ev = m.World.Events.Events;
                for (int e = 0; e < ev.Length; e++) if (ev[e].Type == SimEventType.TrenchCaptured && ev[e].A == 1 && ev[e].B == 0) trenchCaptured = true;
            }
            Assert.IsTrue(trenchCaptured, "five men alone in the enemy trench capture it");
            Assert.AreEqual(0, m.Fields.Trenches[1].OwnerTeam);
            Assert.AreEqual(1, m.Fields.FrontTrench(0), "the captured trench is the new front");
            Assert.AreEqual(-1, m.World.WinnerTeam);

            Step(m, new SimCommand { Player = 0, Type = CommandType.TrenchAdvance, A = 1 });   // the new owner's order is accepted
            for (int t = 0; t < 1500 && m.World.WinnerTeam < 0; t++) Step(m);
            Assert.AreEqual(0, m.World.WinnerTeam, "holding the enemy HQ ends the match");
        }

        [Test]
        public void Hq_CannotBeCaptured_WhileTheMainLineHolds()
        {
            using var m = NewMatch();
            short hq = m.Fields.EnemyHq(0);
            int goal = m.Fields.GetGoal(GoalKey.Objective(hq));
            for (int k = 0; k < 5; k++)
            {
                int slot = m.World.Spawn(0, Rifleman, new float3(140f + k * 3f, 0f, m.Map.SizeMeters.y - 60f), 100f, 3f, false);
                m.World.GoalId[slot] = goal;
            }
            for (int t = 0; t < 900; t++) Step(m);
            Assert.AreEqual(-1, m.World.WinnerTeam, "objectives fall in order: main line first");
        }

        static ulong[] Battle(uint seed)
        {
            using var m = NewMatch(seed);
            var hashes = new ulong[1400];
            for (int t = 0; t < hashes.Length; t++)
            {
                if (t < 40 && t % 2 == 0) Step(m, SimCommand.Deploy(m.World.Tick, 0, t % 3), SimCommand.Deploy(m.World.Tick, 1, (t + 1) % 3));
                else if (t == 700) SpawnAndStep(m);
                else Step(m);
                hashes[t] = m.World.LastHash;
            }
            return hashes;
        }

        static void SpawnAndStep(MatchSim m)
        {
            SpawnAssault(m, 0, 12, TrenchZ(m, 1) - 100f, 1);
            SpawnAssault(m, 1, 12, TrenchZ(m, 0) + 100f, 0);
            Step(m);
        }

        [Test]
        public void Combat_IsDeterministic()
        {
            var a = Battle(7);
            var b = Battle(7);
            for (int i = 0; i < a.Length; i++) Assert.AreEqual(a[i], b[i], $"hash diverged at tick {i}");
            var c = Battle(8);
            bool differs = false;
            for (int i = 0; i < a.Length && !differs; i++) differs = a[i] != c[i];
            Assert.IsTrue(differs, "a different seed changes the dice");
        }
    }
}
