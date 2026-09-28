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
using TW.Presentation;

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
            // ------------------------------------------------------------------ the Salvo's rack (format v11)
        struct Landing { public uint Tick; public float3 Pos; }

        /// <summary>A Salvo 150 m from six men in the open, run long enough for one rack to fire and come down.</summary>
        static (List<SimEvent> log, int truck, List<int> men) Rack(MatchSim m)
        {
            int truck = Spawn(m, 1, VehicleArchetype.Salvo, new float3(30f, 0f, 260f), 0f);
            var men = new List<int>();
            for (int k = 0; k < 6; k++) men.Add(m.World.Spawn(0, 0, new float3(26f + k * 2f, 0f, 110f), 100f, 0f, false));
            return (Run(m, 20 * 12), truck, men);
        }

        static List<Landing> Promised(List<SimEvent> log, int truck, out uint fired)
        {
            var list = new List<Landing>(); fired = uint.MaxValue;
            foreach (var e in log)
                if (e.Type == SimEventType.RocketFired && e.A == truck && fired == uint.MaxValue || e.Type == SimEventType.RocketFired && e.A == truck && e.Tick == fired)
                {
                    fired = e.Tick;
                    list.Add(new Landing { Tick = e.Tick + (uint)e.Dir.x + (uint)e.Dir.y, Pos = e.Pos });
                }
            return list;
        }

        /// <summary>Every rocket the event promised bursts on the tick it named and at the point it named, and nothing of
        /// the rack bursts on the tick it is fired: the damage waits for the rockets.</summary>
        [Test]
        public void EachRocketBurstsOnTheTickAndAtThePointItsEventNamed()
        {
            using var m = NewMatch();
            var (log, truck, _) = Rack(m);
            var promised = Promised(log, truck, out uint fired);
            Assert.AreEqual(m.Catalogue.Tank[VehicleArchetype.Salvo].Rockets, promised.Count, "one RocketFired per rocket of the rack");
            int source = SourceId.Unit(VehicleArchetype.Salvo);
            var bursts = new List<SimEvent>();
            foreach (var e in log) if (e.Type == SimEventType.Explosion && e.A == source) bursts.Add(e);
            foreach (var b in bursts) Assert.Greater(b.Tick, fired, "a burst on the tick the rack fired: the damage did not wait for the rockets");
            foreach (var p in promised)
            {
                Assert.GreaterOrEqual(p.Tick, fired + (uint)math.round(TankGunnerySystem.SalvoFlightMin / m.World.Config.TickSeconds), "a rocket lands sooner than it can fly");
                bool found = false;
                foreach (var b in bursts)
                    if (b.Tick == p.Tick && math.distance(b.Pos.xz, p.Pos.xz) < 0.01f) { found = true; break; }
                Assert.IsTrue(found, $"no burst at tick {p.Tick}, ({p.Pos.x:F1}, {p.Pos.z:F1}) as the event said");
            }
            Assert.AreEqual(promised.Count, bursts.Count, "a burst the events never promised");
        }

        /// <summary>The men are hurt when the rockets land, not when the rack fires; and hurt they are.</summary>
        [Test]
        public void TheMenAreHurtWhenTheRocketsLandNotWhenTheRackFires()
        {
            using var m = NewMatch();
            int truck = Spawn(m, 1, VehicleArchetype.Salvo, new float3(30f, 0f, 260f), 0f);
            var men = new List<int>();
            for (int k = 0; k < 6; k++) men.Add(m.World.Spawn(0, 0, new float3(26f + k * 2f, 0f, 110f), 100f, 0f, false));
            uint fired = uint.MaxValue, firstLand = uint.MaxValue, firstHurt = uint.MaxValue;
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            for (int t = 0; t < 20 * 12; t++)
            {
                m.Step(none);
                var ev = m.World.Events.Events;
                for (int k = 0; k < ev.Length; k++)
                    if (ev[k].Type == SimEventType.RocketFired && ev[k].A == truck)
                    {
                        if (fired == uint.MaxValue) fired = ev[k].Tick;
                        if (ev[k].Tick == fired) firstLand = math.min(firstLand, ev[k].Tick + (uint)ev[k].Dir.x + (uint)ev[k].Dir.y);
                    }
                if (firstHurt == uint.MaxValue)
                    foreach (int i in men) if (!m.World.IsAlive(i) || m.World.Hp[i] < 100f) { firstHurt = m.World.Tick - 1; break; }
            }
            Assert.AreNotEqual(uint.MaxValue, fired, "the rack never fired");
            Assert.AreNotEqual(uint.MaxValue, firstHurt, "the rockets hurt nobody in the open 150 m off");
            Assert.AreEqual(firstLand, firstHurt, "the first man is hurt on the tick the first rocket lands");
        }

        /// <summary>The rack is deterministic: the same seed lands the same rockets on the same ticks, and a two-world canary
        /// plays it hash for hash as one world does.</summary>
        [Test]
        public void TheRackIsTheSameEveryRunAndInTheCanary()
        {
            List<Landing> a, b;
            { using var m = NewMatch(); var (log, truck, _) = Rack(m); a = Promised(log, truck, out _); }
            { using var m = NewMatch(); var (log, truck, _) = Rack(m); b = Promised(log, truck, out _); }
            Assert.AreEqual(a.Count, b.Count);
            for (int k = 0; k < a.Count; k++) { Assert.AreEqual(a[k].Tick, b[k].Tick); Assert.AreEqual(a[k].Pos, b[k].Pos); }

            var one = Canary(false); var two = Canary(true);
            for (int t = 0; t < one.Length; t++) Assert.AreEqual(one[t], two[t], $"one world and the canary part at tick {t}");
        }

        static ulong[] Canary(bool canary)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            using var session = new LockstepSession(() => MatchSim.CreatePlaytest(cfg), canary, 0, 0, 0f, cfg.Seed);
            foreach (var m in canary ? new[] { session.Local, session.Peer } : new[] { session.Local })
            {
                m.World.HashInterval = 1;
                Spawn(m, 1, VehicleArchetype.Salvo, new float3(30f, 0f, 260f), 0f);
                for (int k = 0; k < 6; k++) m.World.Spawn(0, 0, new float3(26f + k * 2f, 0f, 110f), 100f, 0f, false);
            }
            var ai = new ScriptedEnemy { Enabled = false };
            const int Ticks = 20 * 12;
            var hashes = new ulong[Ticks];
            int guard = Ticks * 40;
            while (session.Local.World.Tick < Ticks && guard-- > 0)
                if (session.StepOnce(ai)) hashes[session.Local.World.Tick - 1] = session.Local.World.LastHash;
            Assert.IsFalse(session.Desync, "the canary desynced on the rack");
            return hashes;
        }
    }
}
