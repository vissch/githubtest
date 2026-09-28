// Phase: A5c (2026-09-28) — the Playground's five units as battle units (owner: "all" of them, spawn-only): the Brute
// (a medium tank), the Croaker (a two-legged walker), the Mercy (an ambulance), the Hopper (a gunship, drawn flying) and
// the Frog (a rifleman of its own id). Defined in Sim/Match/UnitDefinitions.cs like the Skimmer and the Salvo: in the
// match's table, in no faction's slots or pool. These tests hold that, and that each does what it was given: the armed
// ones drive and hurt men in the open, the Mercy heals a wounded man near it (and a shipped machine heals nobody), and
// the Frog fights exactly as a rifleman does.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;

namespace TW.Tests
{
    public class PlaygroundUnitTests
    {
        static readonly byte[] New = { VehicleArchetype.Brute, VehicleArchetype.Croaker, VehicleArchetype.Mercy, VehicleArchetype.Hopper, InfantryArchetype.Frog };

        static MatchSim NewMatch()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            return MatchSim.CreatePlaytest(cfg);
        }

        static List<SimEvent> Run(MatchSim m, int ticks)
        {
            var log = new List<SimEvent>();
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            for (int t = 0; t < ticks; t++)
            {
                m.Step(none);
                var ev = m.World.Events.Events;
                for (int k = 0; k < ev.Length; k++) log.Add(ev[k]);
            }
            return log;
        }

        /// <summary>Spawn one the way TankCapture.Spawn (and so the Unit Sandbox) does: its line from the match's table.</summary>
        static int Spawn(MatchSim m, byte team, byte archetype, float3 at, float speed = -1f)
        {
            var e = m.World.Units.Roster[archetype];
            return m.World.Spawn(team, archetype, at, e.Hp, speed < 0f ? e.Speed : speed, ChassisKind.IsArmoured(m.World.ChassisOf(archetype)));
        }

        /// <summary>Four men of team 0 standing in the open `range` metres south of (30, 200); how many were hurt after `ticks`.</summary>
        static int HurtMen(byte archetype, float range, int ticks, out bool shooterAlive)
        {
            using var m = NewMatch();
            int gun = Spawn(m, 1, archetype, new float3(30f, 0f, 200f), 0f);
            var men = new List<int>();
            for (int k = 0; k < 4; k++) men.Add(m.World.Spawn(0, 0, new float3(26f + k * 2f, 0f, 200f - range), 100f, 0f, false));
            Run(m, ticks);
            int hurt = 0;
            foreach (int i in men) if (!m.World.IsAlive(i) || m.World.Hp[i] < 100f) hurt++;
            shooterAlive = m.World.IsAlive(gun);
            return hurt;
        }

        [Test]
        public void AllFiveAreInTheMatchTableAsTheKindsTheyWereGiven()
        {
            using var m = NewMatch();
            var w = m.World;
            foreach (byte a in New)
            {
                Assert.AreEqual(a, w.Units.Roster[a].Archetype, "archetype " + a);
                Assert.Greater(w.Units.Roster[a].Hp, 0f, "archetype " + a + " can be fielded");
            }
            Assert.AreEqual(ChassisKind.Tracked, w.ChassisOf(VehicleArchetype.Brute));
            Assert.AreEqual(ChassisKind.Legged, w.ChassisOf(VehicleArchetype.Croaker));
            Assert.AreEqual(ChassisKind.Tracked, w.ChassisOf(VehicleArchetype.Mercy));
            Assert.AreEqual(ChassisKind.Tracked, w.ChassisOf(VehicleArchetype.Hopper));
            Assert.IsFalse(ChassisKind.IsArmoured(w.ChassisOf(InfantryArchetype.Frog)), "the Frog is a man");
            var kin = w.GetSystem<TW.Sim.Nav.VehicleKinematicsSystem>();
            Assert.IsTrue(kin.Profiles[VehicleArchetype.Croaker].Walker);
            Assert.AreEqual(2, kin.Profiles[VehicleArchetype.Croaker].Legs, "two legs");
            Assert.AreEqual(0f, kin.Profiles[VehicleArchetype.Hopper].DitchChance, "drawn flying: it never ditches");
            Assert.AreEqual(0f, kin.Profiles[VehicleArchetype.Hopper].BogChance, "nor bogs");
            Assert.AreEqual(0f, w.GetSystem<CombatCatalogueSystem>().Weapon[VehicleArchetype.Mercy].RangeMax, "the ambulance is unarmed");
        }

        [Test]
        public void NoFactionFieldsAnyOfThem()
        {
            for (int f = 0; f < Factions.Count; f++)
            {
                var faction = Factions.Of((byte)f);
                foreach (byte a in New)
                {
                    Assert.IsFalse(FactionRoster.Fields(faction, a), $"{faction} fields archetype {a}");
                    for (int s = 0; s < RosterEntry.SlotCount; s++)
                        Assert.AreNotEqual(a, FactionRoster.Slot(faction, s).Archetype, $"{faction} slot {s} is archetype {a}");
                }
            }
        }

        [TestCase(VehicleArchetype.Brute)]
        [TestCase(VehicleArchetype.Croaker)]
        [TestCase(VehicleArchetype.Hopper)]
        public void EachArmedMachineDrivesAndHurtsMenInTheOpen(byte archetype)
        {
            using (var m = NewMatch())
            {
                var start = new float3(30f, 0f, 200f);
                int unit = Spawn(m, 1, archetype, start);
                Run(m, 150);
                Assert.Greater(math.distance(m.World.Position[unit], start), 5f, "it moves off towards the enemy");
            }
            int hurt = HurtMen(archetype, 80f, 500, out bool alive);
            Assert.Greater(hurt, 0, "men in the open 80 m off are hit");
            Assert.IsTrue(alive);
        }

        [Test]
        public void TheMercyHealsAWoundedManNearItAndAShippedMachineHealsNobody()
        {
            float Healed(byte archetype, float away)
            {
                using var m = NewMatch();
                Spawn(m, 0, archetype, new float3(30f, 0f, 40f), 0f);
                int man = m.World.Spawn(0, 0, new float3(30f + away, 0f, 40f), 100f, 0f, false);
                m.World.Hp[man] = 40f;
                var log = Run(m, 60);
                int events = 0; foreach (var e in log) if (e.Type == SimEventType.UnitHealed && e.B == man) events++;
                Assert.AreEqual(m.World.Hp[man] > 40f, events > 0, "healing raises UnitHealed");
                return m.World.Hp[man] - 40f;
            }
            Assert.Greater(Healed(VehicleArchetype.Mercy, 10f), 20f, "the ambulance patches a man 10 m off");
            Assert.AreEqual(0f, Healed(VehicleArchetype.Mercy, 30f), 1e-3f, "and not one beyond its 14 m");
            Assert.AreEqual(0f, Healed(VehicleArchetype.Tusk, 10f), 1e-3f, "the control: a shipped machine heals nobody");
        }

        [Test]
        public void TheFrogFightsExactlyAsARiflemanDoes()
        {
            using var m = NewMatch();
            var combat = m.World.GetSystem<CombatCatalogueSystem>();
            var frog = combat.Weapon[InfantryArchetype.Frog]; var rifle = combat.Weapon[InfantryArchetype.Rifle];
            Assert.AreEqual(rifle.Damage, frog.Damage); Assert.AreEqual(rifle.RangeMax, frog.RangeMax);
            Assert.AreEqual(rifle.RoundsPerSecond, frog.RoundsPerSecond); Assert.AreEqual(rifle.Accuracy, frog.Accuracy);
            Assert.AreEqual(m.World.Units.Roster[InfantryArchetype.Rifle].Hp, m.World.Units.Roster[InfantryArchetype.Frog].Hp);
            Assert.AreEqual(m.World.Units.Roster[InfantryArchetype.Rifle].Cost, m.World.Units.Roster[InfantryArchetype.Frog].Cost);
            Assert.Greater(HurtMen(InfantryArchetype.Frog, 60f, 500, out _), 0, "and it shoots men");
        }
    }
}
