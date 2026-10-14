// Phase: A5b (2026-10-07) — the Mercy carries the wounded. The owner's words: "the behavior of the ambulance should be
// they take badly hurt units from the frontline, can get up to 4 units, once theyre in the ambulance they drive out of
// view, once out of view the units spawn again full health." These cases hold MercyCarrySystem to them: who is taken
// (under BadlyHurt and nobody above it), how many (Seats), that a man aboard has left the field without dying, that he
// comes back at the roster line's hit points at his own side's edge, that he is lost with the hull, that it never
// drives past the foremost trench its side holds, and that it leaves early under fire.
// "Full health" is read from w.Units.Roster[arch].Hp — the table, not the way the system computes it.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Units;

namespace TW.Tests
{
    public class MercyTests
    {
        static MatchSim NewMatch()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            var m = MatchSim.CreatePlaytest(cfg);
            for (int i = 0; i < m.World.HighWater; i++)
                if (m.World.IsAlive(i)) m.World.Despawn(i, -1, default);
            return m;
        }

        static void Step(MatchSim m)
        {
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            m.Step(none);
        }

        static List<SimEvent> Run(MatchSim m, int ticks, System.Action<SimWorld> each = null)
        {
            var log = new List<SimEvent>();
            for (int t = 0; t < ticks; t++)
            {
                Step(m);
                each?.Invoke(m.World);
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

        /// <summary>An ambulance that can drive: its line's speed, not a stand-in standing still.</summary>
        static int Ambulance(MatchSim m, byte team, float x, float z)
        {
            var e = m.World.Units.Roster[VehicleArchetype.Mercy];
            return m.World.Spawn(team, VehicleArchetype.Mercy, new float3(x, 0f, z), e.Hp, e.Speed, true);
        }

        /// <summary>A man of his line at <paramref name="share"/> of his hit points, standing where he is put.</summary>
        static int Man(MatchSim m, byte team, byte archetype, float x, float z, float share)
        {
            var e = m.World.Units.Roster[archetype];
            int i = m.World.Spawn(team, archetype, new float3(x, 0f, z), e.Hp, 0f, false);
            m.World.Hp[i] = e.Hp * share;
            return i;
        }

        static MercyCarrySystem Carry(MatchSim m) => m.World.GetSystem<MercyCarrySystem>();

        // -------------------------------------------------------------------------------------------- who is taken
        [Test]
        public void ItTakesAManUnderThirtyFivePercentAndNotOneAbove()
        {
            using var m = NewMatch();
            int mercy = Ambulance(m, 0, 150f, 50f);
            int hurt = Man(m, 0, InfantryArchetype.Rifle, 156f, 50f, 0.30f);
            int whole = Man(m, 0, InfantryArchetype.Rifle, 144f, 50f, 0.60f);
            var log = Run(m, 200);
            Assert.AreEqual(1, Count(log, SimEventType.UnitBoarded, hurt, mercy), "the man at 30 % goes aboard");
            Assert.AreEqual(0, Count(log, SimEventType.UnitBoarded, whole, mercy), "the man at 60 % is not badly hurt");
            Assert.IsTrue(m.World.IsAlive(whole), "and he stays on the field");
        }

        [Test]
        public void ItNeverCarriesMoreThanFourMen()
        {
            using var m = NewMatch();
            int mercy = Ambulance(m, 0, 150f, 50f);
            // riflemen, not medics: six medics in a heap patch each other back over the badly-hurt line between pickups
            for (int k = 0; k < 6; k++) Man(m, 0, InfantryArchetype.Rifle, 148f + k * 0.8f, 52f, 0.20f);   // all six within reach: no driving
            int worst = 0;
            var log = Run(m, 300, w => worst = math.max(worst, Carry(m).Aboard[mercy]));
            Assert.AreEqual((int)MercyCarrySystem.Seats, Count(log, SimEventType.UnitBoarded, int.MinValue, mercy), "four seats, four men, then it leaves");
            Assert.LessOrEqual(worst, (int)MercyCarrySystem.Seats, "never a fifth man aboard");
        }

        // ------------------------------------------------------------------------------------ aboard he is off the field
        [Test]
        public void AManAboardIsInNoSlotSoHeCannotBeShotAndNoDeathIsRaised()
        {
            using var m = NewMatch();
            int mercy = Ambulance(m, 0, 150f, 50f);
            int hurt = Man(m, 0, InfantryArchetype.Rifle, 156f, 50f, 0.25f);
            var log = new List<SimEvent>();
            bool aboard = false;
            for (int t = 0; t < 200 && !aboard; t++)
            {
                Step(m);
                var ev = m.World.Events.Events;
                for (int k = 0; k < ev.Length; k++) { log.Add(ev[k]); if (ev[k].Type == SimEventType.UnitBoarded && ev[k].A == hurt) aboard = true; }
            }
            Assert.IsTrue(aboard, "setup: he went aboard");
            Assert.IsFalse(m.World.IsAlive(hurt), "he left the field: no slot holds him, so nothing can shoot him");
            Assert.AreEqual(-1, m.World.TrenchId[hurt], "and he is in no trench");
            Assert.AreEqual(0u, m.World.Flags[hurt] & (uint)UnitFlags.Alive);
            Assert.AreEqual(0, Count(log, SimEventType.Death, hurt), "he did not die");
            Assert.AreEqual(1, Carry(m).Aboard[mercy]);
        }

        // ----------------------------------------------------------------------------------------- out and back again
        [Test]
        public void ItReachesItsOwnLineAndEveryManComesBackAtFullHealth()
        {
            // close to its own edge, with both men within reach: the whole cycle fits in a few hundred ticks
            using var m = NewMatch();
            var w = m.World;
            float full = w.Units.Roster[InfantryArchetype.Rifle].Hp;
            int mercy = Ambulance(m, 0, w.Init.SpawnA.x, w.Init.SpawnA.z + 20f);
            for (int k = 0; k < 2; k++) Man(m, 0, InfantryArchetype.Rifle, w.Init.SpawnA.x + k * 0.8f, w.Init.SpawnA.z + 22f, 0.20f);
            var log = Run(m, 600);
            int boarded = Count(log, SimEventType.UnitBoarded, int.MinValue, mercy);
            int returned = Count(log, SimEventType.UnitReturned, int.MinValue, mercy);
            Assert.AreEqual(2, boarded, "both men went aboard");
            Assert.AreEqual(boarded, returned, "and every one of them came back");
            foreach (var e in log)
            {
                if (e.Type != SimEventType.UnitReturned) continue;
                int slot = e.A;
                Assert.AreEqual(full, w.Units.Roster[w.Archetype[slot]].Hp, "he comes back as his own line");
                Assert.AreEqual(full, w.MaxHp[slot], "at the table's hit points");
                Assert.AreEqual(w.MaxHp[slot], w.Hp[slot], "and in full health");
                // his own lane across the width, as a reinforcement comes up, so only the z is the side's edge
                Assert.Less(math.abs(e.Pos.z - w.Init.SpawnA.z), 30f, "from his own side's edge, where his side's men enter");
                Assert.That(e.Pos.x, Is.InRange(0f, w.Init.SizeMeters.x), "on the field");
            }
            Assert.AreEqual(0, Carry(m).Aboard[mercy], "it is empty and fetching again");
            Assert.AreEqual((byte)MercyPhase.Fetch, Carry(m).Phase[mercy]);
        }

        [Test]
        public void DestroyedWithMenAboardTheyAreLostWithIt()
        {
            using var m = NewMatch();
            int mercy = Ambulance(m, 0, 150f, 50f);
            Man(m, 0, InfantryArchetype.Rifle, 156f, 50f, 0.20f);
            var log = Run(m, 200);
            Assert.GreaterOrEqual(Count(log, SimEventType.UnitBoarded, int.MinValue, mercy), 1, "setup: a man is aboard");
            m.World.Despawn(mercy, -1, default);
            var after = Run(m, 400);
            Assert.AreEqual(0, Count(after, SimEventType.UnitReturned), "nothing comes back out of a wreck");
            Assert.AreEqual(0, m.World.AliveCount, "and the side is empty: the man aboard is dead with the hull");
        }

        // --------------------------------------------------------------------------------- it stays behind its own line
        [Test]
        public void ItNeverDrivesPastTheForemostTrenchItsSideHolds()
        {
            using var m = NewMatch();
            var w = m.World;
            var fields = w.GetSystem<TW.Sim.Nav.FlowFieldManager>();
            short front = fields.FrontTrench(0);
            Assert.GreaterOrEqual(front, 0, "setup: team 0 holds a trench");
            float frontZ = m.Map.NavCellCenter(m.Map.TrenchCells[m.Map.Trenches[front].CellStart]).z;
            int mercy = Ambulance(m, 0, 150f, frontZ - 20f);
            Man(m, 0, InfantryArchetype.Rifle, 150f, frontZ + 40f, 0.20f);
            float worst = 0f;
            Run(m, 600, world => worst = math.max(worst, world.Position[mercy].z));
            Assert.Less(worst, frontZ + 2f, $"it stopped at its own front trench (z {frontZ}) and never went for the man beyond it");
        }

        // ------------------------------------------------------------------------------------------- it leaves early
        [Test]
        public void ItLeavesWithOneAboardWhenItsHullIsTakingHits()
        {
            using var m = NewMatch();
            int mercy = Ambulance(m, 0, 150f, 50f);
            Man(m, 0, InfantryArchetype.Rifle, 156f, 50f, 0.20f);
            Man(m, 0, InfantryArchetype.Rifle, 144f, 50f, 0.20f);   // a second man it would fetch if nobody shot at it
            bool shot = false;
            Run(m, 200, w =>
            {
                if (Carry(m).Aboard[mercy] >= 1) { w.Hp[mercy] -= 2f; shot = true; }   // its hull loses hit points: under fire
            });
            Assert.IsTrue(shot, "setup: a man went aboard and the hull was hit after that");
            Assert.AreEqual((byte)MercyPhase.Leave, Carry(m).Phase[mercy], "one aboard and under fire: it leaves");
            Assert.AreEqual(1, Carry(m).Aboard[mercy], "with the one man, not four");
        }
    }
}
