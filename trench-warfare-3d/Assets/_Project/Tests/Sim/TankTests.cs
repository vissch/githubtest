// Phase: A5b (implemented) — the tanks: armour, getting across trenches, mud, wire and trees, the guns' arcs, what a
// hit does, how a tank burns, bails out, cooks off and leaves a wreck, repairs, and that all of it is deterministic.
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
    public class TankTests
    {
        const int Rifleman = 0, Tank = 6;   // FactionRoster.Slot: slots 6-9 are the machines on both sides

        static MatchSim NewMatch(uint seed = 0xC0FFEE)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.Seed = seed;
            return MatchSim.CreatePlaytest(cfg);
        }

        static void Step(MatchSim m, params SimCommand[] cmds)
        {
            using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
            m.Step(arr);
        }

        static int Count(List<SimEvent> log, SimEventType type, int a = int.MinValue, int b = int.MinValue)
        {
            int n = 0;
            foreach (var e in log) if (e.Type == type && (a == int.MinValue || e.A == a) && (b == int.MinValue || e.B == b)) n++;
            return n;
        }

        /// <summary>Step until done() or maxTicks, keeping every event.</summary>
        static List<SimEvent> Run(MatchSim m, int maxTicks, System.Func<bool> done = null)
        {
            var log = new List<SimEvent>();
            for (int t = 0; t < maxTicks && (done == null || !done()); t++)
            {
                Step(m);
                var ev = m.World.Events.Events;
                for (int k = 0; k < ev.Length; k++) log.Add(ev[k]);
            }
            return log;
        }

        /// <summary>A tank; one of team 0 is sent straight up the map (+Z) in its own column, so it drives (or, stopped,
        /// faces) the way the test lays things out. Its default goal, the enemy HQ, lies off to one side, and an 8-way
        /// flow field would take it there at 45 degrees.</summary>
        static int SpawnTank(MatchSim m, byte team, byte archetype, float3 at, float speed = -1f)
        {
            var entry = archetype == VehicleArchetype.Tusk ? RosterEntry.Tusk : RosterEntry.Maw;
            int tank = m.World.Spawn(team, archetype, at, entry.Hp, speed < 0f ? entry.Speed : speed, true);
            if (team == 0)
            {
                var c = m.Map.NavCellOf(at);
                m.World.GoalId[tank] = m.Fields.GetGoal(GoalKey.Cell(m.Map.NavIndex(c.x, m.Map.NavLength - 6), NavMode.Tracked));
            }
            return tank;
        }

        static void PaintBand(MapData map, float z0, float z1, NavLayer add)
        {
            for (int z = (int)(z0 / MapData.NavCellSize); z <= (int)(z1 / MapData.NavCellSize); z++)
            for (int x = 0; x < map.NavWidth; x++)
                map.SetLayer(x, z, (NavLayer)map.NavLayers[map.NavIndex(x, z)] | add);
            map.RebuildCost();
        }

        // ------------------------------------------------------------------ armour
        [Test]
        public void Armour_FrontStopsWhatTheSideLetsThrough_AndSteepAnglesGlance()
        {
            var maw = TankSpec.Maw.Hull;   // 12 / 8 / 6 / 6
            float3 fromAhead = new float3(0f, 0f, -1f);   // a round travelling toward -Z meets a hull facing +Z nose on
            Assert.IsFalse(Armor.Penetrates(maw, 10f, fromAhead, 0f, out float plate), "10 mm does not hole a 12 mm front");
            Assert.AreEqual(12f, plate);
            Assert.IsTrue(Armor.Penetrates(maw, 20f, fromAhead, 0f, out _), "20 mm holes the front");
            float3 fromLeft = new float3(1f, 0f, 0f);    // travelling toward +X strikes the left side
            Assert.IsTrue(Armor.Penetrates(maw, 10f, fromLeft, 0f, out plate), "10 mm holes an 8 mm side");
            Assert.AreEqual(8f, plate);
            var facing = Armor.FacingOf(fromLeft, 0f, out _, out bool right);
            Assert.AreEqual(ArmourFacing.Side, facing);
            Assert.IsFalse(right, "a round travelling toward +X comes in on the left");
            Assert.AreEqual(ArmourFacing.Rear, Armor.FacingOf(new float3(0f, 0f, 1f), 0f, out _, out _));
            // angling: 14 mm holes the 12 mm front met square, not 40 degrees off it (14 × cos 40° = 10.7)
            Assert.IsTrue(Armor.Penetrates(maw, 14f, fromAhead, 0f, out _));
            Assert.IsFalse(Armor.Penetrates(maw, 14f, fromAhead, 40f * TankSpec.Deg, out _), "a hull turned 40 degrees off the shot");
            Assert.AreEqual(ArmourFacing.Side, Armor.FacingOf(fromAhead, 50f * TankSpec.Deg, out _, out _), "past 45 degrees it shows its side");
            Assert.IsTrue(Armor.PenetratesTop(maw, 15f, out plate) && plate == 6f, "a shell on the deck comes through 6 mm");
            Assert.IsFalse(Armor.PenetratesTop(TankSpec.Tusk.Hull, 5f, out _));
        }

        // ------------------------------------------------------------------ getting across
        [Test]
        public void Maw_BridgesTheFireTrenches_WithoutDitching()
        {
            using var m = NewMatch();
            int tank = SpawnTank(m, 0, VehicleArchetype.Maw, new float3(150f, 0f, 20f));
            var log = Run(m, 3200, () => m.World.Position[tank].z > 160f);
            Assert.Greater(m.World.Position[tank].z, 160f, "across both of its own trenches (60 m and 140 m)");
            Assert.AreEqual(0, Count(log, SimEventType.VehicleDitched, tank), "3 m trenches are inside its 3.5 m");
        }

        [Test]
        public void Tusk_TriesTrenchesTooWideForIt_SometimesDitches_AndClawsOut()
        {
            int ditched = 0, climbed = 0;
            for (uint seed = 1; seed <= 10; seed++)
            {
                using var m = NewMatch(seed);
                int tank = SpawnTank(m, 0, VehicleArchetype.Tusk, new float3(150f, 0f, 20f));
                var log = Run(m, 4000, () => m.World.Position[tank].z > 160f);
                Assert.Greater(m.World.Position[tank].z, 160f, $"seed {seed}: it gets across in the end");
                ditched += Count(log, SimEventType.VehicleDitched, tank) - Count(log, SimEventType.VehicleDitched, tank, -1);
                climbed += Count(log, SimEventType.VehicleDitched, tank, -1);
            }
            Assert.Greater(ditched, 0, "3 m is wider than the Tusk's 2.4 m: some tries end nose down in the trench");
            Assert.AreEqual(ditched, climbed, "every ditched tank clawed its way out");
        }

        [Test]
        public void Mud_BogsATank_ThatThenDrivesOut()
        {
            int bogged = 0;
            for (uint seed = 1; seed <= 6; seed++)
            {
                using var m = NewMatch(seed);
                PaintBand(m.Map, 24f, 56f, NavLayer.Mud);
                int tank = SpawnTank(m, 0, VehicleArchetype.Maw, new float3(150f, 0f, 16f));
                var log = Run(m, 4000, () => m.World.Position[tank].z > 70f);
                Assert.Greater(m.World.Position[tank].z, 70f, $"seed {seed}: through the mud");
                bogged += Count(log, SimEventType.VehicleBogged, tank, 1);
                Assert.AreEqual(Count(log, SimEventType.VehicleBogged, tank, 1), Count(log, SimEventType.VehicleBogged, tank, 0), "each time it stuck it got free");
                Assert.AreEqual(0u, m.World.Flags[tank] & (uint)UnitFlags.Bogged);
            }
            Assert.Greater(bogged, 0, "32 m of mud at 0.8 m/s bogs the Maw more often than not");
        }

        [Test]
        public void Tracks_CrushTheWireTheyRollOver()
        {
            using var m = NewMatch();
            PaintBand(m.Map, 30f, 33f, NavLayer.Wire);
            int tank = SpawnTank(m, 0, VehicleArchetype.Maw, new float3(150f, 0f, 20f));
            var log = Run(m, 1500, () => m.World.Position[tank].z > 45f);
            Assert.Greater(Count(log, SimEventType.WireBreached), 0);
            Assert.Greater(Count(log, SimEventType.VehicleCrushed, tank, 0), 0);
            var c = m.Map.NavCellOf(new float3(m.World.Position[tank].x, 0f, 31.5f));
            Assert.AreEqual(0, m.Map.NavLayers[m.Map.NavIndex(c.x, c.y)] & (byte)NavLayer.Wire, "no wire left where it went through");
            Assert.Greater(m.Vehicles.WireCrushed, 0);
        }

        [Test]
        public void Maw_FlattensTreesUnderItsHull_TheTuskOnlyBrokenOnes()
        {
            using var m = NewMatch();
            int maw = SpawnTank(m, 0, VehicleArchetype.Maw, new float3(100.5f, 0f, 20f));
            int tusk = SpawnTank(m, 0, VehicleArchetype.Tusk, new float3(200.5f, 0f, 20f));
            // beside each path (the next nav cell over, so the field does not route round them) but under the hull
            int t0 = m.Map.AddProp(new PropDef { Pos = new float3(102.5f, 0f, 40f), Kind = PropKind.Tree });
            int t1 = m.Map.AddProp(new PropDef { Pos = new float3(202.5f, 0f, 40f), Kind = PropKind.Tree });
            int t2 = m.Map.AddProp(new PropDef { Pos = new float3(198.5f, 0f, 46f), Kind = PropKind.BrokenTree });
            Assert.IsTrue(t0 >= 0 && t1 >= 0 && t2 >= 0);
            Run(m, 1200, () => m.World.Position[maw].z > 55f && m.World.Position[tusk].z > 55f);
            Assert.AreEqual(PropKind.Log, m.Map.Props[t0].Kind, "a tree under the Maw's hull went over");
            Assert.AreEqual(PropKind.Tree, m.Map.Props[t1].Kind, "the Tusk is too light for a standing tree");
            Assert.AreEqual(PropKind.Log, m.Map.Props[t2].Kind, "but not for a broken one");
        }

        // ------------------------------------------------------------------ guns
        [Test]
        public void Sponsons_OnlyFireInsideTheirArcs()
        {
            using var m = NewMatch();
            int tank = SpawnTank(m, 0, VehicleArchetype.Maw, new float3(150f, 0f, 100f), 0f);
            int behind = m.World.Spawn(1, Rifleman, new float3(150f, 0f, 55f), 100000f, 0f, false);
            var log = Run(m, 300);
            Assert.AreEqual(0, Count(log, SimEventType.VehicleFired, tank), "nothing swings round to a man dead astern");
            m.World.Despawn(behind);

            m.World.Spawn(1, Rifleman, new float3(100f, 0f, 98f), 100000f, 0f, false);   // square off the left side
            log = Run(m, 400);
            Assert.Greater(Count(log, SimEventType.VehicleFired, tank, 0), 0, "the left sponson reaches round to the left");
            Assert.AreEqual(0, Count(log, SimEventType.VehicleFired, tank, 1), "the right one cannot swing through the hull");
        }

        [Test]
        public void TuskTurret_TraversesToATargetBehind_AndFires()
        {
            using var m = NewMatch();
            int tank = SpawnTank(m, 0, VehicleArchetype.Tusk, new float3(150f, 0f, 100f), 0f);
            m.World.Spawn(1, Rifleman, new float3(150f, 0f, 50f), 100000f, 0f, false);
            var log = Run(m, 400);
            Assert.Greater(Count(log, SimEventType.VehicleFired, tank), 0);
            float yaw = m.Gunnery.GunYaw[tank * TankGunnerySystem.Guns];
            Assert.Greater(math.abs(yaw), 2.9f, "the turret faces astern");
            Assert.Greater(Count(log, SimEventType.Explosion), 0, "high explosive against a man");
        }

        // ------------------------------------------------------------------ what a hit does
        [Test]
        public void AFrontalRoundBounces_AFlankShotHoles_AndBreaksSomething()
        {
            using var m = NewMatch();
            int tank = SpawnTank(m, 0, VehicleArchetype.Maw, new float3(150f, 0f, 100f), 0f);
            Step(m);
            float hp = m.World.Hp[tank];
            m.Gunnery.PendingHits.Add(new VehicleHit { Target = tank, Shooter = -1, Kind = VehicleHitKind.ArmourPiercing, PenMm = 10f, Damage = 650f, Pos = m.World.Position[tank], Dir = new float3(0f, 0f, -1f) });
            var log = Run(m, 1);
            Assert.AreEqual(1, m.Modules.Ricochets);
            Assert.Greater(m.World.Hp[tank], hp - 100f, "a bounce only rings the plate");
            Assert.AreEqual(1, Count(log, SimEventType.VehicleArmourHit, tank));

            for (int k = 0; k < 6; k++)
                m.Gunnery.PendingHits.Add(new VehicleHit { Target = tank, Shooter = -1, Kind = VehicleHitKind.ArmourPiercing, PenMm = 40f, Damage = 300f, Pos = m.World.Position[tank], Dir = new float3(1f, 0f, 0f) });
            log = Run(m, 1);
            // 40 mm goes through an 8 mm side every time; once a round sets the racks off the rest strike a hulk
            Assert.Greater(m.Modules.Penetrations, 0);
            Assert.AreEqual(6, Count(log, SimEventType.VehicleArmourHit, tank));
            foreach (var e in log) if (e.Type == SimEventType.VehicleArmourHit) Assert.Greater(e.Scalar, 0f, "holed, not stopped");
            bool knockedOut = (m.World.Flags[tank] & (uint)UnitFlags.KnockedOut) != 0;
            Assert.IsTrue(knockedOut || m.World.Hp[tank] < hp - 250f, "holes cost structure");
            Assert.Greater(Count(log, SimEventType.VehicleModuleHit, tank) + Count(log, SimEventType.VehicleCrewLost, tank), 0, "six holes break something or kill someone");
        }

        [Test]
        public void ABurningTank_IsAbandoned_CooksOff_AndLeavesAWreck()
        {
            using var m = NewMatch();
            int tank = SpawnTank(m, 0, VehicleArchetype.Maw, new float3(150f, 0f, 100f), 0f);
            Step(m);
            int props = m.Map.Props.Length, alive = m.World.AliveCount;
            m.Modules.Fire[tank] = 0.62f;   // the fuel is burning
            var log = Run(m, 1200, () => !m.World.IsAlive(tank));
            Assert.IsFalse(m.World.IsAlive(tank));
            Assert.AreEqual(1, Count(log, SimEventType.VehicleKnockedOut, tank, (int)VehicleKillCause.Fire), "the crew get out when it burns");
            Assert.AreEqual(1, Count(log, SimEventType.VehicleBailedOut, tank, 6), "all six of them");
            Assert.AreEqual(1, Count(log, SimEventType.VehicleCookOff, tank), "the racks go up");
            Assert.AreEqual(1, Count(log, SimEventType.VehicleDestroyed, tank));
            log.AddRange(Run(m, 2));
            Assert.Greater(Count(log, SimEventType.Explosion, VehicleModulesSystem.CookOffSource), 0, "and it bursts");
            Assert.AreEqual(props + 1, m.Map.Props.Length);
            Assert.AreEqual(PropKind.Wreck, m.Map.Props[props].Kind, "a wreck where it stood");
        }

        [Test]
        public void ABrokenTrack_IsMended_WhenTheTankIsLeftAlone()
        {
            using var m = NewMatch();
            int tank = SpawnTank(m, 0, VehicleArchetype.Tusk, new float3(150f, 0f, 100f));
            Step(m);
            m.Modules.Module[tank * (int)VehicleModule.Count + (int)VehicleModule.TrackLeft] = 0f;
            m.Modules.LastHitTick[tank] = m.World.Tick;
            Step(m);
            Assert.AreNotEqual(0u, m.World.Flags[tank] & (uint)UnitFlags.Immobilised, "a broken track: it cannot move");
            float3 at = m.World.Position[tank];
            var log = Run(m, VehicleModulesSystem.RepairQuietTicks + VehicleModulesSystem.RepairTicks + 20, () => (m.World.Flags[tank] & (uint)UnitFlags.Immobilised) == 0);
            Assert.AreEqual(1, Count(log, SimEventType.VehicleRepaired, tank, (int)VehicleModule.TrackLeft));
            Assert.AreEqual(0u, m.World.Flags[tank] & (uint)UnitFlags.Immobilised);
            Assert.Less(math.distance(at, m.World.Position[tank]), 0.5f, "it did not move while the track was off");
        }

        [Test]
        public void AKnockedOutTank_IsNoTarget()
        {
            using var m = NewMatch();
            int tank = SpawnTank(m, 1, VehicleArchetype.Tusk, new float3(150f, 0f, 104f), 0f);
            for (int k = 0; k < 6; k++) m.World.Spawn(0, Rifleman, new float3(146f + k * 1.5f, 0f, 99f), 100000f, 0f, false);   // at the hull
            Step(m);
            m.Modules.Crew[tank] = 1;
            // keep hitting until the last man is dead
            var log = new List<SimEvent>();
            for (int t = 0; t < 40 && (m.World.Flags[tank] & (uint)UnitFlags.KnockedOut) == 0; t++)
            {
                m.Gunnery.PendingHits.Add(new VehicleHit { Target = tank, Shooter = -1, Kind = VehicleHitKind.ArmourPiercing, PenMm = 60f, Damage = 10f, Pos = m.World.Position[tank], Dir = new float3(0f, 0f, 1f) });
                log.AddRange(Run(m, 1));
            }
            Assert.AreNotEqual(0u, m.World.Flags[tank] & (uint)UnitFlags.KnockedOut);
            log = Run(m, 100);
            foreach (var e in log) if (e.Type == SimEventType.Shot) Assert.AreNotEqual(tank, e.B, "nobody throws grenades at a hulk");
            foreach (var e in log) if (e.Type == SimEventType.VehicleFired) Assert.AreNotEqual(tank, e.A, "a hulk fires nothing");
        }

        // ------------------------------------------------------------------ determinism
        [Test]
        public void ATankBattle_IsDeterministic()
        {
            ulong[] Play(uint seed)
            {
                using var m = NewMatch(seed);
                var hashes = new List<ulong>();
                for (uint t = 0; t < 1400; t++)
                {
                    if (t == 0) Step(m, SimCommand.Deploy(t, 0, Tank), SimCommand.Deploy(t, 1, Tank));
                    else if (t < 40 && t % 4 == 0) Step(m, SimCommand.Deploy(t, 0, Rifleman), SimCommand.Deploy(t, 1, Rifleman));
                    else if (t == 700) Step(m, SimCommand.Deploy(t, 0, Tank), SimCommand.Deploy(t, 1, Tank));
                    else Step(m);
                    hashes.Add(m.World.LastHash);
                }
                return hashes.ToArray();
            }
            var a = Play(77); var b = Play(77); var c = Play(78);
            CollectionAssert.AreEqual(a, b, "same seed, same battle");
            CollectionAssert.AreNotEqual(a, c, "another seed, another battle");
        }
    }
}
