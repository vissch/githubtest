// Phase: A3 (implemented 2026-09-25) — the jetpack trooper: he leaps into an enemy-held trench without a ladder
// and is posted there, nothing hits him in the air, his landing bursts under the garrison, he does not leap again
// until his pack is ready, a man with no enemy trench in reach stays on his feet, and it is the same on every machine.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class JetpackTests
    {
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

        static List<SimEvent> Run(MatchSim m, int ticks, System.Func<bool> done = null, System.Action<SimWorld> each = null)
        {
            var log = new List<SimEvent>();
            for (int t = 0; t < ticks; t++)
            {
                each?.Invoke(m.World);
                Step(m);
                var ev = m.World.Events.Events;
                for (int k = 0; k < ev.Length; k++) log.Add(ev[k]);
                if (done != null && done()) break;
            }
            return log;
        }

        static int Count(List<SimEvent> log, SimEventType type, int a = int.MinValue, int b = int.MinValue)
        {
            int n = 0;
            foreach (var e in log) if (e.Type == type && (a == int.MinValue || e.A == a) && (b == int.MinValue || e.B == b)) n++;
            return n;
        }

        static float TrenchZ(MatchSim m, short trench)
            => m.Map.NavCellCenter(m.Map.TrenchCells[m.Map.Trenches[trench].CellStart + m.Map.Trenches[trench].CellCount / 2]).z;

        /// <summary>The enemy's trench nearest team 0's side, garrisoned by <paramref name="count"/> riflemen.</summary>
        static short EnemyLine(MatchSim m, int count)
        {
            short t = m.Fields.FrontTrench(1);
            Assert.GreaterOrEqual(t, 0, "setup: the enemy holds a fire trench");
            int want = m.Fields.Trenches[t].GarrisonCount + count;
            for (int k = 0; k < count; k++) Step(m, SimCommand.Deploy(m.World.Tick, 1, 0));
            for (int i = 0; i < 2000 && m.Fields.Trenches[t].GarrisonCount < want; i++) Step(m);
            Assert.AreEqual(want, m.Fields.Trenches[t].GarrisonCount, "setup: garrison did not form");
            return t;
        }

        static int Still(MatchSim m, byte team, byte archetype, float x, float z)
        {
            var at = new float3(x, 0f, z);
            Assert.AreEqual(0, (int)(m.Map.LayerAt(at) & NavLayer.Trench), $"setup: ({x},{z}) must be open ground");
            var e = RosterEntry.ForArchetype(archetype);
            return m.World.Spawn(team, archetype, at, e.Hp, 0f, false);
        }

        [Test]
        public void HeLeapsIntoTheEnemyTrenchWithoutALadderAndIsPostedThere()
        {
            using var m = NewMatch();
            short t = EnemyLine(m, 4);
            float z = TrenchZ(m, t) - 20f;
            int him = Still(m, 0, InfantryArchetype.Jetpack, 120f, z);
            var spec = InfantrySpec.For(InfantryArchetype.Jetpack);
            int flight = (int)math.ceil(20f / (spec.JumpSpeed * m.World.Config.TickSeconds));
            bool wasAirborne = false; int hitsInAir = 0;
            var log = Run(m, (int)LeapSystem.CheckEvery + flight + 4, () => m.World.TrenchId[him] == t, w =>
            {
                if ((w.Flags[him] & (uint)UnitFlags.Airborne) != 0) wasAirborne = true;
            });
            foreach (var e in log) if (e.Type == SimEventType.Hit && e.B == him) hitsInAir++;
            Assert.AreEqual(1, Count(log, SimEventType.LeapStarted, him), "one leap");
            Assert.IsTrue(wasAirborne, "he was in the air");
            Assert.AreEqual(0, hitsInAir, "nothing hits a man in the air");
            Assert.AreEqual(t, m.World.TrenchId[him], "in the enemy trench, no ladder");
            Assert.AreNotEqual(0u, m.World.Flags[him] & (uint)UnitFlags.InTrench);
            Assert.AreEqual(-1, m.World.GoalId[him], "arrived: no goal left");
            log.AddRange(Run(m, 20));
            // no post yet: an attacker inside an enemy trench is posted only once his side holds it (TrenchGarrisonSystem)
            Assert.Greater(Count(log, SimEventType.Explosion, LeapSystem.LandingSource), 0, "his landing burst under the garrison");
            Assert.Greater(m.Leap.LeapCooldown[him], 0, "the pack is recharging");
        }

        static uint LeapTick(List<SimEvent> log, int slot)
        {
            foreach (var e in log) if (e.Type == SimEventType.LeapStarted && e.A == slot) return e.Tick;
            return 0;
        }

        [Test]
        public void HeDoesNotLeapAgainUntilThePackIsReady_AndNeverInsideTheTrenchHeHolds()
        {
            using var m = NewMatch();
            short t = EnemyLine(m, 2);
            int him = Still(m, 0, InfantryArchetype.Jetpack, 120f, TrenchZ(m, t) - 15f);
            var log = Run(m, 400, () => m.World.TrenchId[him] == t);
            Assert.AreEqual(t, m.World.TrenchId[him]);
            int leaps = Count(log, SimEventType.LeapStarted, him);
            // [T17] the run used to stop the moment he died, so "no second leap" was satisfied by a corpse. Run on
            // without the early-out and pin that he lives well past touchdown. (Surviving the WHOLE 300-tick cooldown
            // is not pinned: after the landing grace two riflemen are on him at point blank.)
            var grace = Run(m, InfantrySpec.For(InfantryArchetype.Jetpack).LandingGraceTicks);
            Assert.AreEqual(0, Count(grace, SimEventType.Hit, int.MinValue, him), "[T17] nothing hits him during the landing grace");
            log = Run(m, 20);
            Assert.IsTrue(m.World.IsAlive(him), "[T17] he lives at least 60 ticks past touchdown");
            log.AddRange(grace);
            log.AddRange(Run(m, 290));
            Assert.AreEqual(0, Count(log, SimEventType.LeapStarted, him), "no leap while the pack recharges, and none into the trench he already holds");
            Assert.AreEqual(1, leaps);
        }

        /// <summary>[C1] His own landing burst is queued at his feet and resolves the tick after touchdown, while he is
        /// still inside LandingGraceTicks: it must not take a single point off him.</summary>
        [Test]
        public void HisOwnLandingBurstLeavesHimWhole()
        {
            using var m = NewMatch();
            short t = EnemyLine(m, 4);
            float z = TrenchZ(m, t) - 20f;
            int him = Still(m, 0, InfantryArchetype.Jetpack, 120f, z);
            var spec = InfantrySpec.For(InfantryArchetype.Jetpack);
            int flight = (int)math.ceil(20f / (spec.JumpSpeed * m.World.Config.TickSeconds));
            float hpBeforeTouchdown = -1f;
            var log = Run(m, (int)LeapSystem.CheckEvery + flight + 4, () => m.World.TrenchId[him] == t, w =>
            {
                if (m.Movement.LeapTicks[him] > 0) hpBeforeTouchdown = w.Hp[him];
            });
            Assert.Greater(hpBeforeTouchdown, 0f, "[C1] setup: he leapt and was in the air");
            log.AddRange(Run(m, 10));
            Assert.Greater(Count(log, SimEventType.Explosion, LeapSystem.LandingSource), 0, "[C1] his landing burst went off");
            Assert.IsTrue(m.World.IsAlive(him), "[C1] he survived his own landing");
            Assert.AreEqual(hpBeforeTouchdown, m.World.Hp[him], 0.001f, "[C1] his own landing burst took nothing off him");
        }

        /// <summary>[N2] The flight crosses the trench body for several ticks; he is garrisoned (and InTrench) only once
        /// he is down.</summary>
        [Test]
        public void HeIsGarrisonedOnlyWhenHeIsDown()
        {
            using var m = NewMatch();
            short t = EnemyLine(m, 4);
            // far enough back that the flight crosses several metres of the trench body before touchdown: the old
            // arrival rule garrisoned him on the first of those ticks
            int him = Still(m, 0, InfantryArchetype.Jetpack, 120f, TrenchZ(m, t) - 27f);
            // a man of his own standing still straight under the flight path: an airborne man shoves nobody
            int under = Still(m, 0, InfantryArchetype.Rifle, 120f, TrenchZ(m, t) - 14f);
            float3 stood = m.World.Position[under];
            var spec = InfantrySpec.For(InfantryArchetype.Jetpack);
            int flight = (int)math.ceil(27f / (spec.JumpSpeed * m.World.Config.TickSeconds));
            int airTicks = 0;
            Run(m, (int)LeapSystem.CheckEvery + flight + 6, () => m.World.TrenchId[him] == t, w =>
            {
                if (m.Movement.LeapTicks[him] <= 0) return;
                airTicks++;
                Assert.Less(w.TrenchId[him], (short)0, "[N2] not garrisoned while he is in the air");
                Assert.AreEqual(0u, w.Flags[him] & (uint)UnitFlags.InTrench, "[N2] over a trench is not in it");
                Assert.AreNotEqual(0u, w.Flags[him] & (uint)UnitFlags.Airborne, "[N2] Airborne holds for the whole flight");
            });
            Assert.Greater(airTicks, 4, "[N2] setup: he spent several ticks in the air");
            Assert.AreEqual(t, m.World.TrenchId[him], "[N2] and he does end up garrisoned, the ordinary way");
            Assert.AreNotEqual(0u, m.World.Flags[him] & (uint)UnitFlags.InTrench);
            Assert.IsTrue(m.World.IsAlive(under), "[N2] setup: the man under the flight path is still there");
            Assert.AreEqual(0f, math.distance(stood.xz, m.World.Position[under].xz), 0.01f,
                "[N2] a man in the air shoves nobody: the man he flew over has not budged");
        }

        /// <summary>[C3] UnitPose.Airborne: no target, no suppression, no push. A mine, a burning cell, gas and a blast
        /// right under him all reach nothing while he is off the ground.</summary>
        [Test]
        public void NothingOnTheGroundReachesHimInTheAir()
        {
            using var m = NewMatch();
            short t = EnemyLine(m, 4);
            int him = Still(m, 0, InfantryArchetype.Jetpack, 120f, TrenchZ(m, t) - 24f);
            var spec = InfantrySpec.For(InfantryArchetype.Jetpack);
            int flight = (int)math.ceil(24f / (spec.JumpSpeed * m.World.Config.TickSeconds));
            int mine = -2, airTicks = 0;
            float hp0 = -1f, supp0 = -1f, hpMin = float.MaxValue, suppMax = -1f;
            bool burnedInAir = false;
            var log = Run(m, (int)LeapSystem.CheckEvery + flight + 2, () => m.Movement.LeapTicks[him] == 0 && mine >= 0, w =>
            {
                if (m.Movement.LeapTicks[him] <= 0) return;
                airTicks++;
                if (mine == -2)
                {
                    float3 mid = 0.5f * (w.Position[him] + m.Movement.LeapTarget[him]);
                    mine = m.Mines.Place(w, mid, float3.zero, 0f, 1, MineKind.Mine);
                    Assert.GreaterOrEqual(mine, 0, "[C3] setup: a mine on his flight path");
                    hp0 = w.Hp[him]; supp0 = w.Suppression[him];
                    return;
                }
                hpMin = math.min(hpMin, w.Hp[him]); suppMax = math.max(suppMax, w.Suppression[him]);
                if ((w.Flags[him] & (uint)UnitFlags.Burning) != 0) burnedInAir = true;
                if (m.Movement.LeapTicks[him] > 4)
                {
                    m.Burning.IgniteCell(w, w.Position[him], 3f, 1);
                    m.Gas.AddSource(w.Position[him], 100f, 40, 1);
                    m.Blast.Queue(new Impact { Pos = w.Position[him], Damage = 60f, Radius = 6f, Suppression = 60f, Source = 0, Player = 1 });
                }
            });
            Assert.Greater(airTicks, MineSystem.ArmTicks + 6, "[C3] setup: the flight is long enough to arm a mine under him");
            Assert.AreEqual(hp0, hpMin, 0.001f, "[C3] nothing on the ground took hp off him in the air");
            Assert.LessOrEqual(suppMax, supp0 + 0.001f, "[C3] and nothing suppressed him (suppression only decays in the air)");
            Assert.IsFalse(burnedInAir, "[C3] he did not catch fire from a cell he flew over");
            Assert.AreEqual(0, Count(log, SimEventType.MineTriggered, mine, him), "[C3] he did not set the mine off");
            Assert.AreNotEqual((int)MineState.Spent, m.Mines.Mines[mine].State, "[C3] the mine is still there");
        }

        [Test]
        public void WithNoEnemyTrenchInReachHeStaysOnHisFeet()
        {
            using var m = NewMatch();
            short t = EnemyLine(m, 2);
            int him = Still(m, 0, InfantryArchetype.Jetpack, 120f, TrenchZ(m, t) - 60f);
            var log = Run(m, 100);
            Assert.AreEqual(0, Count(log, SimEventType.LeapStarted, him));
            Assert.AreEqual(0u, m.World.Flags[him] & (uint)UnitFlags.Airborne);
        }

        [Test, Category("Long")]
        public void TheLeapIsTheSameOnEveryMachine()
        {
            ulong Play()
            {
                using var m = NewMatch();
                short t = EnemyLine(m, 4);
                for (int k = 0; k < 3; k++) Still(m, 0, InfantryArchetype.Jetpack, 112f + k * 4f, TrenchZ(m, t) - 18f);
                Run(m, 400);
                return m.World.Hash();
            }
            Assert.AreEqual(Play(), Play());
        }
    }
}
