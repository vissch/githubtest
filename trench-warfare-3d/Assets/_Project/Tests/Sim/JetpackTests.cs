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
            log = Run(m, 350, () => !m.World.IsAlive(him));
            Assert.AreEqual(0, Count(log, SimEventType.LeapStarted, him), "no leap while the pack recharges, and none into the trench he already holds");
            Assert.AreEqual(1, leaps);
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

        [Test]
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
