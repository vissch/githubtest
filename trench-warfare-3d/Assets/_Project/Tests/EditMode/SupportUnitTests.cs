// Phase: A3 (implemented 2026-09-25) — the medic and the repair engineer: a medic patches the wounded man beside
// him and stops at full, touches neither a vehicle nor a man out of reach; an engineer brings a machine's hull
// back, beats down its fire and mends a thrown track while it is still being shot at; and both are the same on
// every machine.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Terrain;
using TW.Sim.Units;

namespace TW.Tests
{
    public class SupportUnitTests
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

        static List<SimEvent> Run(MatchSim m, int ticks, System.Action<SimWorld> each = null)
        {
            var log = new List<SimEvent>();
            for (int t = 0; t < ticks; t++)
            {
                each?.Invoke(m.World);
                Step(m);
                var ev = m.World.Events.Events;
                for (int k = 0; k < ev.Length; k++) log.Add(ev[k]);
            }
            return log;
        }

        static int Count(List<SimEvent> log, SimEventType type, int a = int.MinValue, int b = int.MinValue)
        {
            int n = 0;
            foreach (var e in log) if (e.Type == type && (a == int.MinValue || e.A == a) && (b == int.MinValue || e.B == b)) n++;
            return n;
        }

        static int Still(MatchSim m, byte team, byte archetype, float x, float z)
        {
            var at = new float3(x, 0f, z);
            Assert.AreEqual(0, (int)(m.Map.LayerAt(at) & NavLayer.Trench), $"setup: ({x},{z}) must be open ground");
            var e = RosterEntry.ForArchetype(archetype);
            return m.World.Spawn(team, archetype, at, e.Hp, 0f, e.IsVehicle);
        }

        [Test]
        public void AMedicPatchesTheWoundedManBesideHimAndStopsAtFull()
        {
            // the patients are medics too: nobody here carries a rifle, so nobody is shot before the assertion
            using var m = NewMatch();
            int medic = Still(m, 0, InfantryArchetype.Medic, 60f, 30f);
            int near = Still(m, 0, InfantryArchetype.Medic, 63f, 30f);
            int far = Still(m, 0, InfantryArchetype.Medic, 90f, 30f);
            int tank = Still(m, 0, VehicleArchetype.Tusk, 60f, 40f);
            float full = RosterEntry.Medic.Hp;
            m.World.Hp[near] = 20f; m.World.Hp[far] = 20f; m.World.Hp[tank] = 900f;
            var log = Run(m, 40);   // 2 s: 50 hp
            Assert.That(m.World.Hp[near], Is.EqualTo(70f).Within(1f), "25 a second, one patient");
            Assert.Greater(Count(log, SimEventType.UnitHealed, medic, near), 5);
            log.AddRange(Run(m, 60));
            Assert.AreEqual(full, m.World.Hp[near], "never past full");
            Assert.AreEqual(20f, m.World.Hp[far], "thirty metres off is out of reach");
            Assert.AreEqual(900f, m.World.Hp[tank], "a medic does nothing for a machine");
            Assert.AreEqual(0, Count(log, SimEventType.UnitHealed, medic, far));
        }

        [Test]
        public void AMedicDoesNotPatchTheOtherSide()
        {
            using var m = NewMatch();   // two medics and nobody armed: nothing shoots the enemy before the assertion
            Still(m, 0, InfantryArchetype.Medic, 60f, 30f);
            int enemy = Still(m, 1, InfantryArchetype.Medic, 63f, 30f);
            m.World.Hp[enemy] = 20f;
            Run(m, 60);
            Assert.AreEqual(20f, m.World.Hp[enemy], "not the other side");
        }

        [Test]
        public void AMedicTakesTheNearestWoundedManFirst()
        {
            using var m = NewMatch();
            int medic = Still(m, 0, InfantryArchetype.Medic, 60f, 30f);
            int a = Still(m, 0, InfantryArchetype.Rifle, 62f, 30f);
            int b = Still(m, 0, InfantryArchetype.Rifle, 66f, 30f);
            m.World.Hp[a] = 80f; m.World.Hp[b] = 20f;
            Run(m, 20);
            Assert.AreEqual(100f, m.World.Hp[a], "the nearer man is whole first");
            Assert.AreEqual(20f, m.World.Hp[b], "the further one waited");
            Run(m, 80);
            Assert.AreEqual(100f, m.World.Hp[b], "and then had his turn");
        }

        [Test]
        public void AnEngineerBringsAMachineBackWhileItIsStillBeingShotAt()
        {
            using var m = NewMatch();
            int tank = Still(m, 0, VehicleArchetype.Tusk, 150f, 100f);
            int engineer = Still(m, 0, InfantryArchetype.Repair, 150f, 95f);
            Step(m);
            const int M = (int)VehicleModule.Count;
            m.World.Hp[tank] = 800f;
            m.Modules.Module[tank * M + (int)VehicleModule.TrackLeft] = 0f;
            m.Modules.Fire[tank] = 0.3f;
            // hit every tick: the crew's own repair (RepairQuietTicks of peace) never gets its chance
            var log = Run(m, 200, w => m.Modules.LastHitTick[tank] = w.Tick);
            Assert.GreaterOrEqual(m.World.Hp[tank], 1150f, "40 hull a second for ten seconds");
            Assert.Greater(Count(log, SimEventType.VehicleHullMended, tank, engineer), 20);
            Assert.GreaterOrEqual(Count(log, SimEventType.VehicleRepaired, tank, (int)VehicleModule.TrackLeft), 1, "the thrown track was mended in eight seconds");
            Assert.Less(m.Modules.Fire[tank], 0.3f, "the fire was beaten down");
            Assert.LessOrEqual(m.World.Hp[tank], m.World.MaxHp[tank]);
        }

        [Test]
        public void AnEngineerOutOfReachDoesNothing()
        {
            using var m = NewMatch();
            int tank = Still(m, 0, VehicleArchetype.Tusk, 150f, 100f);
            Still(m, 0, InfantryArchetype.Repair, 150f, 80f);
            Step(m);
            m.World.Hp[tank] = 800f;
            var log = Run(m, 100, w => m.Modules.LastHitTick[tank] = w.Tick);
            Assert.AreEqual(800f, m.World.Hp[tank]);
            Assert.AreEqual(0, Count(log, SimEventType.VehicleHullMended, tank));
        }

        [Test]
        public void SupportIsTheSameOnEveryMachine()
        {
            ulong Play()
            {
                using var m = NewMatch();
                int medic = Still(m, 0, InfantryArchetype.Medic, 60f, 30f);
                for (int k = 0; k < 4; k++) Still(m, 0, InfantryArchetype.Rifle, 56f + k * 2.5f, 27f);
                int tank = Still(m, 0, VehicleArchetype.Tusk, 80f, 32f);
                Still(m, 0, InfantryArchetype.Repair, 80f, 27f);
                for (int k = 0; k < 6; k++) Still(m, 1, InfantryArchetype.Rifle, 54f + k * 3f, 65f);
                Run(m, 400);
                return m.World.Hash();
            }
            Assert.AreEqual(Play(), Play());
        }
    }
}
