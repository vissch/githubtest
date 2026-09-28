// Phase: A4 wrecks (2026-09-28, the seam) — a wreck breaks in stages and then is gone (owner, 2026-09-28): whole wreck
// (blocks, 50 % cover), broken wreck (blocks, 35 %), scrap (does not block, 15 %), cleared (nothing). This file holds
// the stages' rules; the steps that make wrecks take harm add their tests here: blasts (S1), machines grinding wrecks
// and flattening scrap (S2).
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class WreckDecayTests
    {
        static readonly float3 At = new float3(151f, 0f, 241f);

        static MatchSim NewMatch(uint seed = 0xBEEF)
        {
            var cfg = SimConfig.Default; cfg.Seed = seed;
            return MatchSim.CreatePlaytest(cfg);
        }

        static void Step(MatchSim m)
        {
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            m.Step(none);
        }

        static void Shell(MatchSim m, float3 at, float damage, float radius = 10f) => m.Blast.Queue(new Impact { Pos = at, Damage = damage, Radius = radius, Player = -1 });

        static int CountOf(MatchSim m, SimEventType type, out SimEvent last)
        {
            int n = 0; last = default;
            var ev = m.World.Events.Events;
            for (int i = 0; i < ev.Length; i++) if (ev[i].Type == type) { n++; last = ev[i]; }
            return n;
        }

        static bool Blocked(MapData map, int cell) => ((NavLayer)map.NavLayers[cell] & NavLayer.Blocked) != 0;

        [Test]
        public void ABlastWearsAWreckDownThroughEveryStageAndItsCoverAndBlockFollow()
        {
            using var m = NewMatch();
            var map = m.Map;
            int wreck = map.AddProp(new PropDef { Pos = At, Kind = PropKind.Wreck });
            int cell = map.Props[wreck].Cell;
            Assert.AreEqual(600f, map.Props[wreck].Hp, "a wreck of size 1 (the map's own) starts at 600");
            Assert.IsTrue(Blocked(map, cell));

            Shell(m, At, 100f); Step(m);
            Assert.AreEqual(PropKind.Wreck, map.Props[wreck].Kind, "a shell it stands");
            Assert.AreEqual(500f, map.Props[wreck].Hp, 1e-3f);
            Assert.AreEqual(1, CountOf(m, SimEventType.PropWorn, out var worn), "and the picture is told");
            Assert.AreEqual(wreck, worn.A); Assert.AreEqual(0, worn.B, "a blast");
            Assert.AreEqual(500f / 600f, worn.Scalar, 1e-4f, "the share of the stage left");

            Shell(m, At, 700f); Step(m);
            Assert.AreEqual(PropKind.BrokenWreck, map.Props[wreck].Kind);
            Assert.AreEqual(450f, map.Props[wreck].Hp, 1e-3f, "a fresh stage's hit points");
            Assert.IsTrue(Blocked(map, cell), "a broken wreck still blocks");
            Assert.AreEqual(PropRules.CoverPercent(PropKind.BrokenWreck), map.CellCover[cell + 1]);
            Assert.AreEqual(1, CountOf(m, SimEventType.PropChanged, out var changed));
            Assert.AreEqual((int)PropKind.BrokenWreck, changed.B); Assert.AreEqual(0f, changed.Dir.x, "no dead slot on a later stage");

            Shell(m, At, 500f); Step(m);
            Assert.AreEqual(PropKind.Scrap, map.Props[wreck].Kind);
            Assert.IsFalse(Blocked(map, cell), "scrap is walked over");
            Assert.AreEqual(PropRules.CoverPercent(PropKind.Scrap), map.CellCover[cell + 1]);

            Shell(m, At, 300f); Step(m);
            Assert.AreEqual(PropKind.Cleared, map.Props[wreck].Kind, "and then it is gone");
            Assert.AreEqual(0f, map.Props[wreck].Hp);
            Assert.AreEqual(0, map.CellCover[cell + 1], "no cover where it stood");
            Assert.IsFalse(Blocked(map, cell));

            Shell(m, At, 5000f); Step(m);
            Assert.AreEqual(PropKind.Cleared, map.Props[wreck].Kind, "nothing left to break");
            Assert.AreEqual(0, CountOf(m, SimEventType.PropWorn, out _) + CountOf(m, SimEventType.PropChanged, out _));
            Assert.AreEqual(3, m.Deformation.PropsChanged);
        }

        [Test]
        public void OneBlastNeverSkipsAStage()
        {
            using var m = NewMatch();
            int wreck = m.Map.AddProp(new PropDef { Pos = At, Kind = PropKind.Wreck });
            Shell(m, At, 50000f); Step(m);
            Assert.AreEqual(PropKind.BrokenWreck, m.Map.Props[wreck].Kind, "one stage a hit, however hard");
        }

        [Test]
        public void ABurstOnTheDeckCountsAsOnTheWreck()
        {
            using var m = NewMatch();
            int wreck = m.Map.AddProp(new PropDef { Pos = At, Kind = PropKind.Wreck });
            Shell(m, At + new float3(PropRules.WreckBlastReach, 0f, 0f), 100f, 6f); Step(m);
            Assert.AreEqual(500f, m.Map.Props[wreck].Hp, 1e-3f, "full harm WreckBlastReach off its middle");
        }

        [Test]
        public void ABiggerMachineLeavesATougherWreckAndItsOwnCookOffSparesIt()
        {
            using var m = NewMatch();
            var map = m.Map;
            int tank = m.World.Spawn(0, VehicleArchetype.Maw, At, 100f, 1.6f, true);
            Shell(m, At, 50000f); Step(m);   // obliterated: it cooks off, and its burst comes next tick
            Assert.IsFalse(m.World.IsAlive(tank));
            Assert.AreEqual(1, map.Props.Length);
            int wreck = 0;
            Assert.AreEqual(PropRules.WreckSize(m.World.Units.Roster[VehicleArchetype.Maw].Hp), map.Props[wreck].Scale, 1e-5f, "sized from the Maw's hull in the unit table, not the slot's 100 hp");
            Assert.AreEqual(900f, map.Props[wreck].Hp, 1e-3f, "a Maw's wreck: 600 x 1.5");
            // a wreck of the map's own, beside it, put down after the burst that killed the Maw
            int beside = map.AddProp(new PropDef { Pos = At + new float3(4f, 0f, 0f), Kind = PropKind.Wreck });
            Step(m);   // the cook-off's burst
            Assert.AreEqual(900f, map.Props[wreck].Hp, 1e-3f, "its own ammunition does not wear the wreck it made");
            Assert.Less(map.Props[beside].Hp, 600f, "but it wears the one beside it");
            Shell(m, At, 5000f); Step(m);
            Assert.AreEqual(PropKind.BrokenWreck, map.Props[wreck].Kind);
            Assert.AreEqual(675f, map.Props[wreck].Hp, 1e-3f, "and its size carries into every stage");

            using var s = NewMatch();
            s.World.Spawn(0, VehicleArchetype.Skimmer, At, 100f, 1.6f, true);
            Shell(s, At, 50000f); Step(s);
            Assert.AreEqual(600f * PropRules.WreckSizeMin, s.Map.Props[0].Hp, 1e-3f, "a Skimmer's is the smallest");
        }

        [Test]
        public void TheRecordOutlivesItsWreck()
        {
            using var m = NewMatch();
            m.World.Spawn(0, VehicleArchetype.Tusk, At, 100f, 1.6f, true);
            Shell(m, At, 50000f); Step(m); Step(m);
            Assert.AreEqual(1, m.Deformation.Wrecks.Length);
            var before = m.Deformation.Wrecks[0];
            for (int k = 0; k < 3; k++) { Shell(m, At, 50000f); Step(m); }
            Assert.AreEqual(PropKind.Cleared, m.Map.Props[before.PropIndex].Kind, "three stages later the wreck is gone");
            Assert.AreEqual(1, m.Deformation.Wrecks.Length);
            var after = m.Deformation.Wrecks[0];
            Assert.AreEqual(before.Quality, after.Quality, "breaking a wreck does not lower its salvage (owner, 2026-09-28)");
            Assert.AreEqual(before.PropIndex, after.PropIndex, "and the record still names its prop");
        }

        [Test]
        public void AGeneratedWreckBreaksLikeAnyOther()
        {
            using var map = BattlefieldGenerator.Create(BattlefieldParams.ShelledForest(1917), Allocator.Persistent);
            int found = 0;
            for (int i = 0; i < map.Props.Length; i++)
            {
                if (map.Props[i].Kind != PropKind.Wreck) continue;
                found++;
                Assert.AreEqual(PropRules.StartHp(PropKind.Wreck), map.Props[i].Hp, "the generator's wrecks start whole, at size 1");
            }
            Assert.Greater(found, 0);
        }

        [Test]
        public void TwoSimsWearTheSameWrecksThroughABarrage()
        {
            var cfg = SimConfig.Default; cfg.Seed = 99; cfg.StartingSilver = 100000;
            using var a = MatchSim.CreateBattlefield(cfg, BattlefieldParams.ShelledForest(1917));
            using var b = MatchSim.CreateBattlefield(cfg, BattlefieldParams.ShelledForest(1917));
            float3 target = default; bool any = false;
            for (int i = 0; i < a.Map.Props.Length && !any; i++) if (a.Map.Props[i].Kind == PropKind.Wreck) { target = a.Map.Props[i].Pos; any = true; }
            Assert.IsTrue(any, "the field has a wreck to shell");
            float before = 0f; for (int i = 0; i < a.Map.Props.Length; i++) if (PropRules.IsWreckage(a.Map.Props[i].Kind)) before += a.Map.Props[i].Hp;
            for (int t = 0; t < 700; t++)
            {
                var cmds = new System.Collections.Generic.List<SimCommand>();
                if (t == 300 || t == 305) cmds.Add(new SimCommand { Tick = a.World.Tick, Player = (byte)(t == 300 ? 0 : 1), Type = CommandType.SupportFire, A = (int)OffMapAbilityId.HeBarrage, Pos = target });
                using var arr = new NativeArray<SimCommand>(cmds.ToArray(), Allocator.Temp);
                a.Step(arr); b.Step(arr);
                Assert.AreEqual(a.World.LastHash, b.World.LastHash, $"tick {t}");
            }
            float after = 0f; for (int i = 0; i < a.Map.Props.Length; i++) if (PropRules.IsWreckage(a.Map.Props[i].Kind)) after += a.Map.Props[i].Hp;
            Assert.Less(after, before, "the barrage wore the wreckage");
            Assert.AreEqual(a.Map.Hash(SimHash.Offset), b.Map.Hash(SimHash.Offset));
        }

        // ---- S2: machines (VehicleKinematics.Wrecks) ----

        /// <summary>A machine sent straight east along its lane to x 190; what the tracks did to wreckage over `ticks`
        /// (VehicleCrushed b = 3), and the wear events (PropWorn b = 1). A second match, when given, runs in lockstep and
        /// must hash the same every tick.</summary>
        /// <summary>The first row of the playtest map, from the south, open from x 140 to 200 and 6 m either side: no
        /// trench, link, wire, bunker or blocked cell (the machine drives along it and the wreck stands in it).</summary>
        static float Lane(MapData map)
        {
            for (float z = 20f; z < map.SizeMeters.y - 20f; z += 2f)
            {
                bool open = true;
                for (float x = 140f; x <= 200f && open; x += 1f)
                    for (float dz = -6f; dz <= 6f && open; dz += 2f)
                        if ((map.LayerAt(new float3(x, 0f, z + dz)) & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked | NavLayer.Wire | NavLayer.Bunker)) != 0) open = false;
                if (open) return z + 1f;   // the middle of a nav cell's row
            }
            Assert.Fail("no open lane on the playtest map");
            return 0f;
        }

        static (int ground, int worn) DriveEast(MatchSim m, int tank, int ticks, MatchSim twin = null, int twinTank = -1, int prop = -1)
        {
            var target = new float3(190f, 0f, m.World.Position[tank].z);
            m.Vehicles.Drive[tank] = VehicleKinematicsSystem.DriveStraight; m.Vehicles.DriveTarget[tank] = target;
            if (twin != null) { twin.Vehicles.Drive[twinTank] = VehicleKinematicsSystem.DriveStraight; twin.Vehicles.DriveTarget[twinTank] = target; }
            int ground = 0, worn = 0;
            for (int t = 0; t < ticks; t++)
            {
                Step(m);
                var ev = m.World.Events.Events;
                for (int i = 0; i < ev.Length; i++)
                {
                    if (ev[i].Type == SimEventType.VehicleCrushed && ev[i].B == 3 && ev[i].A == tank) ground++;
                    if (ev[i].Type == SimEventType.PropWorn && ev[i].B == 1) worn++;
                }
                if (twin != null) { Step(twin); Assert.AreEqual(m.World.LastHash, twin.World.LastHash, $"tick {t}"); }
                if (prop >= 0 && m.Map.Props[prop].Kind == PropKind.Cleared) break;
            }
            return (ground, worn);
        }

        static int SpawnMachine(MatchSim m, byte archetype, float lane)
        {
            var e = m.World.Units.Roster[archetype];
            int tank = m.World.Spawn(0, archetype, new float3(150f, 0f, lane), e.Hp, e.Speed, true);
            Step(m);
            return tank;
        }

        [Test]
        public void AHeavyMachineGrindsAWreckInItsWayDownToNothing()
        {
            using var m = NewMatch();
            using var twin = NewMatch();
            float lane = Lane(m.Map);
            var at = new float3(166f, 0f, lane);
            int wreck = m.Map.AddProp(new PropDef { Pos = at, Kind = PropKind.Wreck });
            Assert.GreaterOrEqual(wreck, 0, "open ground for the wreck");
            twin.Map.AddProp(new PropDef { Pos = at, Kind = PropKind.Wreck });
            int tank = SpawnMachine(m, VehicleArchetype.Maw, lane), twinTank = SpawnMachine(twin, VehicleArchetype.Maw, lane);
            float hp = m.World.Hp[tank];
            var (ground, worn) = DriveEast(m, tank, 3000, twin, twinTank, wreck);
            Assert.AreEqual(PropKind.Cleared, m.Map.Props[wreck].Kind, "ground to a broken wreck, to scrap, and the scrap flattened");
            Assert.Greater(worn, 0, "wear the picture is told of (PropWorn b = 1)");
            Assert.Greater(ground, 3, "VehicleCrushed b = 3 each time the tracks bit");
            Assert.Greater(m.Vehicles.WrecksGround, 3);
            Assert.AreEqual(hp, m.World.Hp[tank], "ramming costs the machine nothing");
            Assert.Greater(m.World.Position[tank].x, at.x, "and it drove on through where the wreck stood");
        }

        [Test]
        public void ALightMachineIsStoppedByAWreckButDoesNotGrindIt()
        {
            using var m = NewMatch();
            float lane = Lane(m.Map);
            int wreck = m.Map.AddProp(new PropDef { Pos = new float3(166f, 0f, lane), Kind = PropKind.Wreck });
            Assert.GreaterOrEqual(wreck, 0);
            int tank = SpawnMachine(m, VehicleArchetype.Tusk, lane);
            var (ground, worn) = DriveEast(m, tank, 600);
            Assert.AreEqual(0, ground, "the Tusk does not push trees over, nor grind wrecks");
            Assert.AreEqual(0, worn);
            Assert.AreEqual(PropKind.Wreck, m.Map.Props[wreck].Kind);
            Assert.Less(m.World.Position[tank].x, 166f, "it stopped short of it");
        }

        [Test]
        public void AnyMachineFlattensScrapItDrivesOver()
        {
            using var m = NewMatch();
            float lane = Lane(m.Map);
            int scrap = m.Map.AddProp(new PropDef { Pos = new float3(166f, 0f, lane), Kind = PropKind.Scrap });
            Assert.GreaterOrEqual(scrap, 0);
            Assert.AreEqual(250f, m.Map.Props[scrap].Hp, "scrap of size 1");
            int tank = SpawnMachine(m, VehicleArchetype.Tusk, lane);
            var (ground, worn) = DriveEast(m, tank, 900, prop: scrap);
            Assert.Greater(ground, 0, "a light machine goes over scrap and presses it into the mud");
            Assert.IsTrue(m.Map.Props[scrap].Kind == PropKind.Cleared || m.Map.Props[scrap].Hp < 250f, $"worn: {m.Map.Props[scrap].Kind} {m.Map.Props[scrap].Hp}");
        }

        [Test]
        public void AParkedHeavyMachineLeavesTheWreckBesideItAlone()
        {
            using var m = NewMatch();
            float lane = Lane(m.Map);
            int wreck = m.Map.AddProp(new PropDef { Pos = new float3(153f, 0f, lane), Kind = PropKind.Wreck });
            Assert.GreaterOrEqual(wreck, 0);
            int tank = SpawnMachine(m, VehicleArchetype.Maw, lane);
            m.Vehicles.HaltTicks[tank] = 100000;
            int ground = 0;
            for (int t = 0; t < 200; t++)
            {
                Step(m);
                var ev = m.World.Events.Events;
                for (int i = 0; i < ev.Length; i++) if (ev[i].Type == SimEventType.VehicleCrushed && ev[i].B == 3) ground++;
            }
            Assert.AreEqual(0, ground, "standing against a wreck is not grinding it");
            Assert.AreEqual(600f, m.Map.Props[wreck].Hp);
        }

        [Test]
        public void TheNewStagesHaveTheRulesTheOwnerChose()
        {
            Assert.AreEqual(PropKind.BrokenWreck, PropRules.Next(PropKind.Wreck));
            Assert.AreEqual(PropKind.Scrap, PropRules.Next(PropKind.BrokenWreck));
            Assert.AreEqual(PropKind.Cleared, PropRules.Next(PropKind.Scrap));
            Assert.AreEqual(PropKind.Cleared, PropRules.Next(PropKind.Cleared), "cleared is the end");

            Assert.AreEqual(50, PropRules.CoverPercent(PropKind.Wreck));
            Assert.AreEqual(35, PropRules.CoverPercent(PropKind.BrokenWreck));
            Assert.AreEqual(15, PropRules.CoverPercent(PropKind.Scrap));
            Assert.AreEqual(0, PropRules.CoverPercent(PropKind.Cleared));

            Assert.IsTrue(PropRules.Blocks(PropKind.Wreck));
            Assert.IsTrue(PropRules.Blocks(PropKind.BrokenWreck), "a broken wreck still blocks");
            Assert.IsFalse(PropRules.Blocks(PropKind.Scrap), "men and machines go over a scrap pile");
            Assert.IsFalse(PropRules.Blocks(PropKind.Cleared));
            Assert.AreEqual(600f, PropRules.StartHp(PropKind.Wreck)); Assert.AreEqual(450f, PropRules.StartHp(PropKind.BrokenWreck)); Assert.AreEqual(250f, PropRules.StartHp(PropKind.Scrap)); Assert.AreEqual(0f, PropRules.StartHp(PropKind.Cleared));
            Assert.AreEqual(900f, PropRules.StartHp(PropKind.Wreck, 1.5f)); Assert.AreEqual(220f, PropRules.StartHp(PropKind.Tree, 1.5f), "a tree's scale is its drawn size, not its toughness");

            foreach (var k in new[] { PropKind.Wreck, PropKind.BrokenWreck, PropKind.Scrap }) Assert.IsTrue(PropRules.IsWreckage(k), k.ToString());
            foreach (var k in new[] { PropKind.Tree, PropKind.BrokenTree, PropKind.Stump, PropKind.Log, PropKind.Bridge, PropKind.Cleared }) Assert.IsFalse(PropRules.IsWreckage(k), k.ToString());

            // the trees are as they were
            Assert.AreEqual(PropKind.BrokenTree, PropRules.Next(PropKind.Tree));
            Assert.AreEqual(PropKind.Stump, PropRules.Next(PropKind.BrokenTree));
            Assert.AreEqual(PropKind.Stump, PropRules.Next(PropKind.Stump));
        }

        [Test]
        public void TheKindsAndTheEventAreAppendedNotInserted()
        {
            // hashed as numbers and read by the picture: an insert would renumber everything after it
            Assert.AreEqual(4, (int)PropKind.Wreck); Assert.AreEqual(5, (int)PropKind.Bridge);
            Assert.AreEqual(6, (int)PropKind.BrokenWreck); Assert.AreEqual(7, (int)PropKind.Scrap); Assert.AreEqual(8, (int)PropKind.Cleared);
            Assert.AreEqual((int)SimEventType.RocketFired + 1, (int)SimEventType.PropWorn);
        }

        [Test]
        public void AWrecksSizeFollowsItsMachine()
        {
            Assert.AreEqual(1.5f, PropRules.WreckSize(3600f), 1e-5f, "the Maw: as big as it gets");
            Assert.AreEqual(2000f / PropRules.WreckSizeHp, PropRules.WreckSize(2000f), 1e-5f, "the Tusk");
            Assert.AreEqual(PropRules.WreckSizeMin, PropRules.WreckSize(100f), "a test's 100 hp tank is not a wreck of nothing");
            Assert.AreEqual(1f, PropRules.WreckSize(PropRules.WreckSizeHp), 1e-5f);
        }
    }
}
