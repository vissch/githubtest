// Phase: A5b (implemented 2026-09-25) — wreck provenance: a machine knocked out by structure with its modules whole
// leaves a record of good quality, one that cooked off leaves scrap, the record survives its slot being re-used, it is
// in the hash, and the sector system says whose ground a cell is.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Units;

namespace TW.Tests
{
    public class WreckRecordTests
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

        static List<SimEvent> Run(MatchSim m, int ticks, System.Func<bool> done = null)
        {
            var log = new List<SimEvent>();
            for (int t = 0; t < ticks; t++)
            {
                Step(m);
                var ev = m.World.Events.Events;
                for (int k = 0; k < ev.Length; k++) log.Add(ev[k]);
                if (done != null && done()) break;
            }
            return log;
        }

        static int Tank(MatchSim m, byte team, byte archetype, float x, float z)
        {
            var e = RosterEntry.ForArchetype(archetype);
            return m.World.Spawn(team, archetype, new float3(x, 0f, z), e.Hp, 0f, true);
        }

        [Test]
        public void AMachineThatDiesWholeLeavesAGoodRecordAndOneThatCooksOffLeavesScrap()
        {
            using var m = NewMatch();
            int whole = Tank(m, 1, VehicleArchetype.Tusk, 150f, 100f);
            int burnt = Tank(m, 1, VehicleArchetype.Maw, 150f, 130f);
            Step(m);
            m.World.Hp[whole] = 0f;      // structure gone, modules untouched
            m.Modules.Fire[burnt] = 0.62f;   // the fuel burns: it cooks off
            // the shelled hull's fire is beaten out every tick (a knocked-out hull's own crew cannot), so it keeps its metal
            var log = new List<SimEvent>();
            for (int t = 0; t < 1500 && (m.World.IsAlive(whole) || m.World.IsAlive(burnt)); t++)
            {
                if (m.World.IsAlive(whole)) m.Modules.Fire[whole] = 0f;
                Step(m);
                var ev = m.World.Events.Events;
                for (int k = 0; k < ev.Length; k++) log.Add(ev[k]);
            }
            Assert.AreEqual(2, m.Deformation.Wrecks.Length, "two wrecks, two records");
            Assert.AreEqual(2, log.FindAll(e => e.Type == SimEventType.WreckRecorded).Count);
            WreckRecord a = default, b = default;
            for (int i = 0; i < 2; i++) { var r = m.Deformation.Wrecks[i]; if (r.Slot == whole) a = r; else b = r; }
            Assert.AreEqual(VehicleArchetype.Tusk, a.Archetype); Assert.AreEqual(1, a.Team);
            Assert.AreEqual((int)VehicleKillCause.Structure, a.Cause);
            Assert.GreaterOrEqual(a.Quality, 0.8f, "whole modules, a structure kill: most of a machine");
            Assert.AreEqual(VehicleArchetype.Maw, b.Archetype);
            Assert.AreEqual((int)VehicleKillCause.Fire, b.Cause, "the fire is what stopped it; the cook-off is its fate");
            Assert.LessOrEqual(b.Quality, 0.15f, "cooked off: scrap");
            Assert.AreEqual(PropKindOf(m, a.PropIndex), TW.Sim.Terrain.PropKind.Wreck);
        }

        [Test]
        public void AShelledHullThatBurnsThroughIsWorthWhatIsLeftOfIt()
        {
            using var m = NewMatch();
            int tank = Tank(m, 1, VehicleArchetype.Tusk, 150f, 100f);
            Step(m);
            m.World.Hp[tank] = 0f;            // a shell stops it
            Step(m);
            m.Modules.Fire[tank] = 0.62f;     // and then it burns: the blow stands in the record, the salvage does not
            Run(m, 1500, () => !m.World.IsAlive(tank));
            var r = m.Deformation.Wrecks[0];
            Assert.AreEqual((int)VehicleKillCause.Structure, r.Cause, "the blow that stopped it");
            Assert.LessOrEqual(r.Quality, 0.5f, "a burnt-out or cooked-off hull is not a machine");
        }

        static TW.Sim.Terrain.PropKind PropKindOf(MatchSim m, int index) => m.Map.Props[index].Kind;

        [Test]
        public void TheRecordSurvivesItsSlotBeingReUsed()
        {
            using var m = NewMatch();
            int tank = Tank(m, 1, VehicleArchetype.Tusk, 150f, 100f);
            Step(m);
            m.World.Hp[tank] = 0f;
            Run(m, 1500, () => !m.World.IsAlive(tank));
            var rec = m.Deformation.Wrecks[0];
            // the freed slot goes to the next man
            int again = m.World.Spawn(0, InfantryArchetype.Rifle, new float3(60f, 0f, 30f), 100f, 3f, false);
            Assert.AreEqual(tank, again, "setup: the slot was re-used");
            Step(m);
            Assert.AreEqual(VehicleArchetype.Tusk, m.Deformation.Wrecks[0].Archetype);
            Assert.AreEqual(1, m.Deformation.Wrecks[0].Team);
            Assert.AreEqual(rec.Generation, m.Deformation.Wrecks[0].Generation);
            Assert.AreNotEqual(rec.Generation, m.World.Generation[again]);
        }

        [Test]
        public void TheRecordIsInTheHashAndTheSectorSystemNamesTheGround()
        {
            using var a = NewMatch(); using var b = NewMatch();
            Step(a); Step(b);
            Assert.AreEqual(a.World.Hash(), b.World.Hash());
            a.Deformation.Wrecks.Add(new WreckRecord { Archetype = VehicleArchetype.Maw, Quality = 0.5f });
            Assert.AreNotEqual(a.World.Hash(), b.World.Hash());
            var def = a.Map.Objectives[0];
            int inside = a.Map.ObjectiveCells[def.CellStart];
            Assert.AreEqual(0, a.Sectors.ObjectiveAt(inside));
            Assert.AreEqual(-1, a.Sectors.ObjectiveAt(-1));
        }
    }
}
