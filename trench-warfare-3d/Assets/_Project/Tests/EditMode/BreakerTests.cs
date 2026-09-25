// Phase: A5b (implemented 2026-09-25) — the Breaker: sent at a garrisoned enemy trench it halts short of it, winds
// up, charges it at more than twice its speed, claws and crits what is in it, backs out with its nose still toward
// the enemy, and does it again, and lives through two cycles; a drive mode moves any tank straight at a point or
// backwards; it lets its own trench alone; and it is the same on every machine.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Sim.Terrain;
using TW.Sim.Units;

namespace TW.Tests
{
    public class BreakerTests
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

        static short EnemyLine(MatchSim m, int count)
        {
            short t = m.Fields.FrontTrench(1);
            int want = m.Fields.Trenches[t].GarrisonCount + count;
            for (int k = 0; k < count; k++) Step(m, SimCommand.Deploy(m.World.Tick, 1, 0));
            for (int i = 0; i < 2000 && m.Fields.Trenches[t].GarrisonCount < want; i++) Step(m);
            Assert.AreEqual(want, m.Fields.Trenches[t].GarrisonCount, "setup: garrison did not form");
            return t;
        }

        static int SpawnBreaker(MatchSim m, byte team, float x, float z)
        {
            var e = RosterEntry.Breaker;
            int slot = m.World.Spawn(team, VehicleArchetype.Breaker, new float3(x, 0f, z), e.Hp, e.Speed, true);
            var c = m.Map.NavCellOf(new float3(x, 0f, z));
            int goalZ = team == 0 ? m.Map.NavLength - 6 : 5;
            m.World.GoalId[slot] = m.Fields.GetGoal(GoalKey.Cell(m.Map.NavIndex(c.x, goalZ), NavMode.Tracked));   // straight up its own column, as TankTests do
            return slot;
        }

        [Test]
        public void ItWindsUpChargesTheTrenchStrikesBacksOutAndLivesToDoItAgain()
        {
            using var m = NewMatch();
            short t = EnemyLine(m, 10);
            float z = TrenchZ(m, t);
            int it = SpawnBreaker(m, 0, 120f, z - 42f);
            var spec = TankSpec.For(VehicleArchetype.Breaker);
            float top = 0f, yawAtWithdraw = 0f, zAtWithdraw = 0f; bool sawWindupHalt = false;
            var log = Run(m, 1400, () => m.Breaker.Cycles >= 2 || !m.World.IsAlive(it), w =>
            {
                float speed = math.length(w.Velocity[it]);
                var phase = (BreakerPhase)m.Breaker.Phase[it];
                if (phase == BreakerPhase.Charge) top = math.max(top, speed);
                if (phase == BreakerPhase.Windup && speed < 0.01f) sawWindupHalt = true;
                if (phase == BreakerPhase.Withdraw && yawAtWithdraw == 0f) { yawAtWithdraw = w.Yaw[it] + 10f; zAtWithdraw = w.Position[it].z; }
            });
            Assert.IsTrue(m.World.IsAlive(it), "it lives through two cycles against ten men");
            Assert.GreaterOrEqual(m.Breaker.Cycles, 2);
            Assert.GreaterOrEqual(Count(log, SimEventType.BreakerPhase, it, (int)BreakerPhase.Windup), 2, "two wind-ups");
            Assert.IsTrue(sawWindupHalt, "it stood still to wind up");
            Assert.Greater(top, RosterEntry.Breaker.Speed * 2f, "the charge is more than twice its speed");
            Assert.Greater(Count(log, SimEventType.CriticalHit, it), 0, "its guns found the mark");
            Assert.Greater(Count(log, SimEventType.VehicleClawed, it), 0, "its claws took men out of the trench");
            Assert.Greater(Count(log, SimEventType.BreakerPhase, it, (int)BreakerPhase.Withdraw), 0);
            Assert.That(m.World.Yaw[it], Is.EqualTo(yawAtWithdraw - 10f).Within(0.3f), "it backs out without turning");
            Assert.AreEqual(0u, m.World.Flags[it] & (uint)UnitFlags.Charging, "and is no longer charging between cycles");
        }

        [Test]
        public void ADriveModeMovesATankStraightAtAPointAndBackwards()
        {
            using var m = NewMatch();
            var e = RosterEntry.Maw;
            int tank = m.World.Spawn(0, VehicleArchetype.Maw, new float3(150f, 0f, 60f), e.Hp, e.Speed, true);
            Step(m);
            var target = new float3(190f, 0f, 60f);   // off to the side of its flow direction
            m.Vehicles.Drive[tank] = VehicleKinematicsSystem.DriveStraight; m.Vehicles.DriveTarget[tank] = target;
            Run(m, 700, () => math.distance(m.World.Position[tank].xz, target.xz) < VehicleKinematicsSystem.DriveArrive + 0.1f);
            Assert.Less(math.distance(m.World.Position[tank].xz, target.xz), VehicleKinematicsSystem.DriveArrive + 0.1f, "it drove to the point");
            float yaw = m.World.Yaw[tank]; float3 before = m.World.Position[tank];
            m.Vehicles.Drive[tank] = VehicleKinematicsSystem.DriveReverse; m.Vehicles.DriveTarget[tank] = new float3(150f, 0f, 60f);
            Run(m, 100);
            Assert.AreEqual(yaw, m.World.Yaw[tank], "reversing does not turn it");
            Assert.Less(math.dot((m.World.Position[tank] - before).xz, math.normalize(SimMath.DirFromYaw(yaw).xz)), -3f, "it went backwards along its nose");
            m.Vehicles.Drive[tank] = VehicleKinematicsSystem.DriveFlow;
            Run(m, 40);
            Assert.Greater(m.World.Position[tank].z, 60f, "and drives on its field again");
        }

        [Test]
        public void ItLetsItsOwnTrenchAlone()
        {
            using var m = NewMatch();
            short own = m.Fields.FrontTrench(0);
            int it = SpawnBreaker(m, 0, 120f, TrenchZ(m, own) - 25f);
            var log = Run(m, 200);
            Assert.AreEqual(0, Count(log, SimEventType.BreakerPhase, it, (int)BreakerPhase.Windup));
        }

        [Test]
        public void TheBreakerIsTheSameOnEveryMachine()
        {
            ulong Play()
            {
                using var m = NewMatch();
                short t = EnemyLine(m, 8);
                SpawnBreaker(m, 0, 120f, TrenchZ(m, t) - 42f);
                Run(m, 900);
                return m.World.Hash();
            }
            Assert.AreEqual(Play(), Play());
        }
    }
}
