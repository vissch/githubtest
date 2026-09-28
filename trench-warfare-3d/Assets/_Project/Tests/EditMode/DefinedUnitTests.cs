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
            Assert.AreEqual(180, sk.Cost); Assert.AreEqual(1300f, sk.Hp); Assert.AreEqual(3.4f, sk.Speed);
            Assert.IsTrue(sk.IsVehicle);
            Assert.AreEqual(ChassisKind.Tracked, w.ChassisOf(VehicleArchetype.Skimmer), "it drives as a tracked machine");
            var mg = m.Catalogue.Weapon[VehicleArchetype.Skimmer];
            Assert.AreEqual(130f, mg.RangeMax, "a machine gun"); Assert.AreEqual(6f, mg.RoundsPerSecond); Assert.AreEqual(24f, mg.Damage);
            Assert.AreEqual(12f, mg.PenetrationMm, "that holes a light machine's side and rear");
            Assert.IsTrue(w.Units.Infantry[VehicleArchetype.Skimmer].HuntsArmour);
            Assert.AreEqual(CombatTables.ChargeRevealRange, w.Units.Infantry[VehicleArchetype.Skimmer].LooksDownMetres, "it looks down into a trench");
            Assert.AreEqual(90f, m.Catalogue.Tank[VehicleArchetype.Skimmer].StandOffMetres, "it shoots from out of grenade range");
            var skHull = m.Catalogue.Tank[VehicleArchetype.Skimmer];
            Assert.AreEqual(0, skHull.GunCount, "no gun for TankGunnery: the machine gun is small arms");
            Assert.AreEqual(8f, skHull.Hull.FrontMm); Assert.AreEqual(2, skHull.Crew);
            var skDrive = m.Vehicles.Profiles[VehicleArchetype.Skimmer];
            Assert.AreEqual(0f, skDrive.BogChance, "it rides over mud"); Assert.AreEqual(0f, skDrive.DitchChance, "and over trenches");
            Assert.AreEqual(1.1f, skDrive.TurnRateRad);

            var sa = w.Units.Roster[VehicleArchetype.Salvo];
            Assert.AreEqual(380, sa.Cost); Assert.AreEqual(2000f, sa.Hp); Assert.AreEqual(1.8f, sa.Speed);
            Assert.AreEqual(ChassisKind.Tracked, w.ChassisOf(VehicleArchetype.Salvo));
            Assert.AreEqual(110f, m.Catalogue.Weapon[VehicleArchetype.Salvo].RangeMax, "a hull machine gun for the 60 m its rockets cannot reach");
            Assert.AreEqual(380f, m.Catalogue.Tank[VehicleArchetype.Salvo].StandOffMetres);
            Assert.AreEqual(32f, m.Catalogue.Tank[VehicleArchetype.Salvo].StandOffPatience, "two reloads without a hit and it moves on");
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
            /// <summary>The Salvo is artillery: once it has something in reach it holds there and fires, instead of driving on
        /// into the enemy's lines as every other machine does (the critic found one parked on the enemy's deploy zone).</summary>
        [Test]
        public void TheSalvoHoldsWhereItIsOnceItHasATarget()
        {
            using var m = NewMatch();
            int truck = Spawn(m, 1, VehicleArchetype.Salvo, new float3(30f, 0f, 260f));   // at its own speed, heading south
            // men who outlive the rockets (a target that dies lets it drive on, which is right)
            for (int k = 0; k < 6; k++) m.World.Spawn(0, 0, new float3(26f + k * 2f, 0f, 20f), 1e6f, 0f, false);
            Run(m, 20 * 30);
            float3 held = m.World.Position[truck];
            Assert.GreaterOrEqual(m.Gunnery.GunTarget[truck * TankGunnerySystem.Guns], 0, "it has the men as its target");
            Run(m, 20 * 10);
            Assert.Less(math.distance(held, m.World.Position[truck]), 0.5f, "and it holds while it has them");
            Assert.Greater(math.distance(held.xz, new float2(30f, 20f)), 200f, "from well back: they were in reach where it stood (240 m), so it never closed");

            using var m2 = NewMatch();
            int tusk = m2.World.Spawn(1, VehicleArchetype.Tusk, new float3(30f, 0f, 260f), m2.World.Units.Roster[VehicleArchetype.Tusk].Hp, m2.World.Units.Roster[VehicleArchetype.Tusk].Speed, true);
            var start = m2.World.Position[tusk];
            Run(m2, 20 * 40);
            Assert.Greater(math.distance(start, m2.World.Position[tusk]), 60f, "a machine without StandOff drives on (the rule is the Salvo's alone)");
        }

        /// <summary>Two machines set down on top of each other are pushed apart to their radii, clearance included.</summary>
        [Test]
        public void TheSkimmerIsPushedClearOfAMawItsDrawnSizeApart()
        {
            using var m = NewMatch();
            var maw = m.World.Units.Roster[VehicleArchetype.Maw];
            int a = m.World.Spawn(0, VehicleArchetype.Maw, new float3(40f, 0f, 120f), maw.Hp, 0f, true);
            int b = Spawn(m, 0, VehicleArchetype.Skimmer, new float3(42f, 0f, 120f), 0f);
            Run(m, 20 * 8);
            float want = m.Vehicles.Profiles[VehicleArchetype.Maw].Radius + m.Vehicles.Profiles[VehicleArchetype.Skimmer].Radius;
            Assert.AreEqual(1.2f, m.Vehicles.Profiles[VehicleArchetype.Skimmer].Clearance);
            Assert.GreaterOrEqual(math.distance(m.World.Position[a].xz, m.World.Position[b].xz), want - 0.3f, "they are still inside each other");
            // the Maw is drawn with its sponsons 5.8 m out and the Skimmer 3.45 m out: that far apart, the hulls clear
            Assert.GreaterOrEqual(want, 5.8f + 3.45f - 0.05f);
        }

        // ------------------------------------------------------------------ the balance critic's round (format v13)
        /// <summary>The Skimmer's 12 mm machine gun takes a light machine (a Salvo's 8 mm side) as its mark, and never a
        /// heavy one's front (a Maw's 12 mm); the Tusk's 9 mm gun, whose machine does not hunt armour, takes neither.</summary>
        [Test]
        public void TheSkimmerHuntsLightMachinesButNotAMawsFront()
        {
            using var m = NewMatch();
            int sk = Spawn(m, 1, VehicleArchetype.Skimmer, new float3(40f, 0f, 150f), 0f);
            m.World.Yaw[sk] = SimMath.Pi;
            int salvo = Spawn(m, 0, VehicleArchetype.Salvo, new float3(40f, 0f, 125f), 0f);   // 25 m: in sight on this lumpy map
            m.World.Yaw[salvo] = SimMath.HalfPi;   // its side to the Skimmer
            Clear(m, salvo);
            Run(m, 6);
            int mark = m.World.TargetSlot[sk];
            Assert.AreEqual(salvo, mark, $"the Salvo's side is the Skimmer's mark (it took slot {mark}, archetype {(mark >= 0 ? m.World.Archetype[mark] : -1)}, team {(mark >= 0 ? m.World.Team[mark] : -1)}; sk {sk} salvo {salvo})");
            float before = m.World.Hp[salvo];
            Run(m, 20 * 20);
            Assert.Less(m.World.Hp[salvo], before, "and the machine gun holes it");

            using var m2 = NewMatch();
            int sk2 = Spawn(m2, 1, VehicleArchetype.Skimmer, new float3(40f, 0f, 150f), 0f);
            var maw = m2.World.Units.Roster[VehicleArchetype.Maw];
            int mw = m2.World.Spawn(0, VehicleArchetype.Maw, new float3(40f, 0f, 125f), maw.Hp, 0f, true);
            m2.World.Yaw[mw] = 0f;   // its front to the Skimmer
            Clear(m2, mw);
            Run(m2, 6);
            Assert.AreNotEqual(mw, m2.World.TargetSlot[sk2], "a Maw's 12 mm front is not a mark for 12 mm");
        }

        /// <summary>Take every other team-0 unit off the field (the playtest map has men on it), so the mark is the machine.</summary>
        static void Clear(MatchSim m, int keep)
        {
            for (int i = 0; i < m.World.HighWater; i++)
                if (i != keep && m.World.IsAlive(i) && m.World.Team[i] == 0) m.World.Despawn(i, -1, default);
        }

        /// <summary>Men below the parapet are hidden from everyone beyond 8 m; the Skimmer, driven up, sees them at 20.</summary>
        [Test]
        public void TheSkimmerLooksDownIntoATrenchWithinTwentyMetres()
        {
            using var m = NewMatch();
            var map = m.Map; int found = -1; float3 cell = default;
            for (int c = 0; c < map.CellTrenchId.Length && found < 0; c++)
                if (map.CellTrenchId[c] >= 0) { found = c; cell = new float3((c % map.NavWidth + 0.5f) * TW.Sim.Terrain.MapData.NavCellSize, 0f, (c / map.NavWidth + 0.5f) * TW.Sim.Terrain.MapData.NavCellSize); }
            Assume.That(found >= 0, "the playtest map has a trench");
            int man = m.World.Spawn(0, 0, cell, 1e6f, 0f, false);
            Run(m, 10);
            Assume.That((m.World.Flags[man] & (uint)UnitFlags.InTrench) != 0 && m.World.StanceOf[man] != (byte)Stance.FireStep, "he is below the rim");
            float3 at = cell + new float3(0f, 0f, 15f);
            int sk = Spawn(m, 1, VehicleArchetype.Skimmer, at, 0f);
            int tusk = m.World.Spawn(1, VehicleArchetype.Tusk, cell + new float3(0f, 0f, -15f), 2000f, 0f, true);
            Run(m, 20);
            Assert.AreEqual(man, m.World.TargetSlot[sk], "15 m off, the Skimmer sees him");
            Assert.AreNotEqual(man, m.World.TargetSlot[tusk], "15 m off, a Tusk does not");
        }

        /// <summary>The hold is patient but not for ever: on a mark that loses no hit points (here a man whose hit points
        /// the test puts back every tick, so its rockets hurt him and it never sees it), the Salvo gives the hold up after
        /// StandOffPatience and drives on.</summary>
        [Test]
        public void TheHoldIsGivenUpOnAMarkItIsNotHurting()
        {
            using var m = NewMatch();
            int truck = Spawn(m, 1, VehicleArchetype.Salvo, new float3(30f, 0f, 260f));
            int man = m.World.Spawn(0, 0, new float3(30f, 0f, 20f), 1e6f, 0f, false);
            Run(m, 20 * 3);
            Assert.AreEqual(man, m.Gunnery.GunTarget[truck * TankGunnerySystem.Guns]);
            m.World.MaxHp[man] = 1e6f;
            // make him proof against its rockets: hit points restored every tick, so the hold never sees one come off
            float3 held = m.World.Position[truck];
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            for (int t = 0; t < 20 * 28; t++) { m.World.Hp[man] = 1e6f; m.Step(none); }
            Assert.Less(math.distance(held, m.World.Position[truck]), 0.5f, "inside its patience it holds");
            for (int t = 0; t < 20 * 10; t++) { m.World.Hp[man] = 1e6f; m.Step(none); }
            Assert.Greater(math.distance(held, m.World.Position[truck]), 3f, "after 32 s on a mark that loses nothing it drives on");
        }

        // ------------------------------------------------------------------ critic round 3 (format v14)
        /// <summary>A unit spawned into a dead machine's slot starts with no hold: the old code kept HoldTarget,
        /// HoldTicks, Release and HoldHp from the Salvo that died there.</summary>
        [Test]
        public void ANewUnitInADeadSalvosSlotInheritsNoHold()
        {
            using var m = NewMatch();
            int truck = Spawn(m, 1, VehicleArchetype.Salvo, new float3(30f, 0f, 260f), 0f);
            m.World.Spawn(0, 0, new float3(30f, 0f, 20f), 1e6f, 0f, false);
            Run(m, 20 * 3);
            var g = m.Gunnery;
            Assume.That(g.HoldTarget[truck], Is.GreaterThanOrEqualTo(0), "it is holding");
            m.World.Despawn(truck, -1, default);
            var tusk = m.World.Units.Roster[VehicleArchetype.Tusk];
            int next = m.World.Spawn(1, VehicleArchetype.Tusk, new float3(30f, 0f, 260f), tusk.Hp, 0f, true);
            Assume.That(next, Is.EqualTo(truck), "the Tusk took the Salvo's slot");
            Run(m, 1);
            Assert.AreEqual(-1, g.HoldTarget[next], "no mark"); Assert.AreEqual(0, g.HoldTicks[next], "no count");
            Assert.AreEqual(0, g.Release[next], "no release"); Assert.AreEqual(0f, g.HoldHp[next], "no hit points");
            Assert.AreEqual(-1, g.HoldGoal[next], "no goal");
        }

        /// <summary>A held Salvo sent somewhere else drives there: the hold is only on the goal it was first given. Nothing
        /// in the sim orders a machine yet (players order trenches); anything that re-goals one (an ability, a hero's call,
        /// a move order later) must not be overridden by the hold for 32 s at a time.</summary>
        [Test]
        public void AHeldSalvoSentElsewhereGoes()
        {
            using var m = NewMatch();
            int truck = Spawn(m, 1, VehicleArchetype.Salvo, new float3(30f, 0f, 200f));
            m.World.Spawn(0, 0, new float3(30f, 0f, 20f), 1e6f, 0f, false);
            Run(m, 20 * 3);
            Assume.That(m.Gunnery.HoldTarget[truck], Is.GreaterThanOrEqualTo(0), "it is holding");
            float3 held = m.World.Position[truck];
            // back to its own side's rear: the goal the other team's machines drive to, as an order to retreat would be
            int back = -1;
            for (int i = 0; i < m.World.HighWater && back < 0; i++)
                if (m.World.IsAlive(i) && m.World.Team[i] == 0 && (m.World.Flags[i] & (uint)UnitFlags.Vehicle) != 0) back = m.World.GoalId[i];
            if (back < 0) { int probe = m.World.Spawn(0, VehicleArchetype.Tusk, new float3(30f, 0f, 30f), 2000f, 0f, true); Run(m, 1); back = m.World.GoalId[probe]; m.World.Despawn(probe, -1, default); }
            Assume.That(back, Is.GreaterThanOrEqualTo(0).And.Not.EqualTo(m.World.GoalId[truck]));
            m.World.GoalId[truck] = back;
            Run(m, 20 * 10);
            Assert.Greater(math.distance(held, m.World.Position[truck]), 5f, "sent back, it goes, whatever it has in reach");
        }

        /// <summary>The Salvo dies with its rockets in the air: they still come down, on the ticks and at the points they
        /// were fired for, the same in two runs.</summary>
        [Test]
        public void RocketsInTheAirLandWhenTheSalvoDies()
        {
            List<Landing> a = null, b = null; int burstsA = 0, burstsB = 0;
            for (int run = 0; run < 2; run++)
            {
                using var m = NewMatch();
                int truck = Spawn(m, 1, VehicleArchetype.Salvo, new float3(30f, 0f, 260f), 0f);
                for (int k = 0; k < 6; k++) m.World.Spawn(0, 0, new float3(26f + k * 2f, 0f, 110f), 1e6f, 0f, false);
                var log = new List<SimEvent>();
                using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
                bool killed = false;
                for (int t = 0; t < 20 * 12; t++)
                {
                    m.Step(none);
                    var ev = m.World.Events.Events;
                    for (int k = 0; k < ev.Length; k++) log.Add(ev[k]);
                    if (!killed && m.Gunnery.Rockets.Length > 0) { m.World.Despawn(truck, -1, default); killed = true; }
                }
                Assume.That(killed, "the rack fired");
                var promised = Promised(log, truck, out _);
                int bursts = 0; int source = SourceId.Unit(VehicleArchetype.Salvo);
                foreach (var e in log) if (e.Type == SimEventType.Explosion && e.A == source) bursts++;
                if (run == 0) { a = promised; burstsA = bursts; } else { b = promised; burstsB = bursts; }
            }
            Assert.AreEqual(a.Count, burstsA, "every rocket in the air burst after the Salvo died");
            Assert.AreEqual(a.Count, b.Count); Assert.AreEqual(burstsA, burstsB);
            for (int k = 0; k < a.Count; k++) { Assert.AreEqual(a[k].Tick, b[k].Tick); Assert.AreEqual(a[k].Pos, b[k].Pos); }
        }

        /// <summary>A match with a Salvo's rockets in the air records and replays to the same hashes: the pending
        /// rockets and the hold are in the verified state.</summary>
        [Test]
        public void AMatchWithRocketsInTheAirReplays()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            cfg.LoadoutA = Ten(InfantryArchetype.Rifle);
            cfg.LoadoutB = Ten(VehicleArchetype.Salvo);
            var recorder = new ReplayRecorder(cfg, default, 1);
            bool inTheAir = false;
            using (var m = MatchSim.CreatePlaytest(cfg))
            {
                for (uint t = 0; t < 20 * 30; t++)
                {
                    var cmds = new List<SimCommand>();
                    if (t == 5) cmds.Add(SimCommand.Deploy(t, 1, 0));
                    if (t % 40 == 10) cmds.Add(SimCommand.Deploy(t, 0, 0));
                    using var arr = new NativeArray<SimCommand>(cmds.ToArray(), Allocator.Temp);
                    m.World.HashInterval = 1;
                    m.Step(arr);
                    inTheAir |= m.Gunnery.Rockets.Length > 0;
                    recorder.Record(arr, m.World.LastHash);
                }
            }
            Assert.IsTrue(inTheAir, "the Salvo fired while it was recorded");
            var player = ReplayPlayer.Parse(recorder.Serialize());
            using var again = MatchSim.CreatePlaytest(player.Config);
            again.World.HashInterval = 1;
            Assert.AreEqual(-1, player.Verify(again.World), "the replay re-simulates to the recorded hashes");
        }

        static Unity.Collections.FixedList32Bytes<byte> Ten(byte a)
        {
            var l = new Unity.Collections.FixedList32Bytes<byte>();
            for (int k = 0; k < RosterEntry.SlotCount; k++) l.Add(a);
            return l;
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

            var one = Canary(false, 0, 0, 0f); var two = Canary(true, 0, 0, 0f); var lossy = Canary(true, 2, 1, 0.05f);
            for (int t = 0; t < one.Length; t++)
            {
                Assert.AreEqual(one[t], two[t], $"one world and the canary part at tick {t}");
                Assert.AreEqual(two[t], lossy[t], $"the canary under latency, jitter and loss parts at tick {t}");
            }
        }

        static ulong[] Canary(bool canary, int latency, int jitter, float loss)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            using var session = new LockstepSession(() => MatchSim.CreatePlaytest(cfg), canary, latency, jitter, loss, cfg.Seed);
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
