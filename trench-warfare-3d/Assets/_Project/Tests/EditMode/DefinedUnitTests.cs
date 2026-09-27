// Phase: A5c (2026-09-28) — the first units that exist only as definitions (Sim/Match/UnitDefinitions.cs): the Skimmer,
// a hovercraft with a machine gun, and the Salvo, a half-track rocket truck. The owner's rule for them: real battle units
// the Unit Sandbox can put on the field, and no change to the game as shipped, so they are in the match's table and in no
// faction's slots or pool. These tests hold both halves, and that each one drives and fights on the numbers it was given.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;

namespace TW.Tests
{
    public class DefinedUnitTests
    {
        static readonly byte[] New = { VehicleArchetype.Skimmer, VehicleArchetype.Salvo };

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

        static int Count(List<SimEvent> log, SimEventType type, int a)
        {
            int n = 0;
            foreach (var e in log) if (e.Type == type && e.A == a) n++;
            return n;
        }

        /// <summary>Spawn one the way TankCapture.Spawn (and so the Unit Sandbox) does: its line from the match's table.</summary>
        static int Spawn(MatchSim m, byte team, byte archetype, float3 at, float speed = -1f)
        {
            var e = m.World.Units.Roster[archetype];
            return m.World.Spawn(team, archetype, at, e.Hp, speed < 0f ? e.Speed : speed, ChassisKind.IsArmoured(m.World.ChassisOf(archetype)));
        }

        [Test]
        public void BothAreInTheMatchTableWithTheNumbersTheyWereGiven()
        {
            using var m = NewMatch();
            var w = m.World;

            var sk = w.Units.Roster[VehicleArchetype.Skimmer];
            Assert.AreEqual(VehicleArchetype.Skimmer, sk.Archetype);
            Assert.AreEqual(220, sk.Cost); Assert.AreEqual(1300f, sk.Hp); Assert.AreEqual(3.4f, sk.Speed);
            Assert.IsTrue(sk.IsVehicle);
            Assert.AreEqual(ChassisKind.Tracked, w.ChassisOf(VehicleArchetype.Skimmer), "it drives as a tracked machine");
            var mg = m.Catalogue.Weapon[VehicleArchetype.Skimmer];
            Assert.AreEqual(130f, mg.RangeMax, "a machine gun"); Assert.AreEqual(6f, mg.RoundsPerSecond); Assert.AreEqual(24f, mg.Damage);
            var skHull = m.Catalogue.Tank[VehicleArchetype.Skimmer];
            Assert.AreEqual(0, skHull.GunCount, "no gun for TankGunnery: the machine gun is small arms");
            Assert.AreEqual(8f, skHull.Hull.FrontMm); Assert.AreEqual(2, skHull.Crew);
            var skDrive = m.Vehicles.Profiles[VehicleArchetype.Skimmer];
            Assert.AreEqual(0f, skDrive.BogChance, "it rides over mud"); Assert.AreEqual(0f, skDrive.DitchChance, "and over trenches");
            Assert.AreEqual(1.1f, skDrive.TurnRateRad);

            var sa = w.Units.Roster[VehicleArchetype.Salvo];
            Assert.AreEqual(380, sa.Cost); Assert.AreEqual(2000f, sa.Hp); Assert.AreEqual(1.8f, sa.Speed);
            Assert.AreEqual(ChassisKind.Tracked, w.ChassisOf(VehicleArchetype.Salvo));
            Assert.AreEqual(0f, m.Catalogue.Weapon[VehicleArchetype.Salvo].RangeMax, "no small arms");
            var rockets = m.Catalogue.Tank[VehicleArchetype.Salvo];
            Assert.AreEqual(1, rockets.GunCount);
            Assert.IsTrue(rockets.Gun0.Indirect, "the rockets need no line of sight");
            Assert.AreEqual(16f, rockets.Gun0.ReloadSeconds, "and are long to reload");
            Assert.AreEqual(60f, rockets.Gun0.RangeMin); Assert.AreEqual(380f, rockets.Gun0.RangeMax);
            Assert.AreEqual(7f, rockets.Gun0.HeRadius, "a wide burst"); Assert.AreEqual(360f, rockets.Gun0.HeDamage);
            Assert.IsTrue(rockets.Gun0.FullCircle, "the box turns all the way round on its turntable");
            Assert.AreEqual(0.5f, m.Vehicles.Profiles[VehicleArchetype.Salvo].TurnRateRad);
        }

        [Test]
        public void NoFactionFieldsEitherOfThem()
        {
            for (int f = 0; f < Factions.Count; f++)
            {
                var faction = Factions.Of((byte)f);
                foreach (byte a in New)
                {
                    Assert.IsFalse(FactionRoster.Fields(faction, a), $"{faction} fields archetype {a}");
                    for (int s = 0; s < RosterEntry.SlotCount; s++)
                        Assert.AreNotEqual(a, FactionRoster.Slot(faction, s).Archetype, $"{faction} slot {s} is archetype {a}");
                    for (int k = 0; k < FactionRoster.PoolCount(faction); k++)
                        Assert.AreNotEqual(a, FactionRoster.Pool(faction, k), $"{faction}'s pool holds archetype {a}");
                }
            }
            using var m = NewMatch();
            for (int s = 0; s < m.World.Roster.Length; s++)
                foreach (byte a in New) Assert.AreNotEqual(a, m.World.Roster[s].Archetype, $"the match's roster slot {s} is archetype {a}");
        }

        [Test]
        public void TheSkimmerDrivesAndItsMachineGunKills()
        {
            using var m = NewMatch();
            var start = new float3(30f, 0f, 200f);
            int sk = Spawn(m, 1, VehicleArchetype.Skimmer, start);
            Run(m, 100);
            Assert.Greater(math.distance(m.World.Position[sk], start), 5f, "it drives off towards the enemy");

            using var m2 = NewMatch();
            int gun = Spawn(m2, 1, VehicleArchetype.Skimmer, new float3(30f, 0f, 200f), 0f);
            var men = new List<int>();
            for (int k = 0; k < 4; k++) men.Add(m2.World.Spawn(0, 0, new float3(26f + k * 2f, 0f, 120f), 100f, 0f, false));
            Run(m2, 400);
            int hurt = 0;
            foreach (int i in men) if (!m2.World.IsAlive(i) || m2.World.Hp[i] < 100f) hurt++;
            Assert.Greater(hurt, 0, "men in the open 80 m off are shot");
            Assert.IsTrue(m2.World.IsAlive(gun));
        }

        [Test]
        public void TheSalvoLobsItsRocketsFarAndNeverCloseIn()
        {
            using var m = NewMatch();
            int truck = Spawn(m, 1, VehicleArchetype.Salvo, new float3(30f, 0f, 120f), 0f);
            var close = new List<int>();
            for (int k = 0; k < 4; k++) close.Add(m.World.Spawn(0, 0, new float3(28f + k, 0f, 100f), 100f, 0f, false));   // 20 m: inside 60
            var log = Run(m, 200);
            Assert.AreEqual(0, Count(log, SimEventType.VehicleFired, truck), "nothing inside the minimum range is worth a rocket");

            using var m2 = NewMatch();
            int truck2 = Spawn(m2, 1, VehicleArchetype.Salvo, new float3(30f, 0f, 260f), 0f);
            for (int k = 0; k < 6; k++) m2.World.Spawn(0, 0, new float3(26f + k * 2f, 0f, 110f), 100f, 0f, false);        // 150 m
            var log2 = Run(m2, 20 * 20);
            int fired = Count(log2, SimEventType.VehicleFired, truck2);
            Assert.Greater(fired, 0, "at rocket range it fires");
            Assert.LessOrEqual(fired, 2, "and a 16 s reload allows at most two in 20 s");
        }
    }
}
