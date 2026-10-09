// Phase: A4 wrecks (2026-09-28, the seam) — a wreck breaks in stages and then is gone (owner, 2026-09-28): whole wreck
// (blocks, 50 % cover), broken wreck (blocks, 35 %), scrap (does not block, 15 %), cleared (nothing). This file holds
// the stages' rules; the steps that make wrecks take harm add their tests here: blasts (S1), machines grinding wrecks
// and flattening scrap (S2), machine-gun rounds a wreck's cover stopped (S3), guns with nobody to shoot at turning on a
// wreck that shelters their enemies (S4).
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

        // ---- S3: sustained fire (DirectFire.Wrecks) ----

        /// <summary>An enemy (team 1) standing a cell east of a prop at (166, lane), held and too tough to die, and one
        /// `shooter` of team 0 35 m west of him, held; `ticks` of their fire. Returns the wear events on the prop
        /// (PropWorn b = 1) and the shooter's shots. A twin, when given, is set up the same and must hash the same.</summary>
        static (int worn, int shots) ShootAtCover(MatchSim m, byte shooter, PropKind cover, int ticks, MatchSim twin = null)
        {
            float lane = Lane(m.Map);
            int prop = -1;
            foreach (var s in twin != null ? new[] { m, twin } : new[] { m })
            {
                prop = s.Map.AddProp(new PropDef { Pos = new float3(166f, 0f, lane), Kind = cover });
                Assert.GreaterOrEqual(prop, 0);
                int man = s.World.Spawn(1, InfantryArchetype.Rifle, new float3(168f, 0f, lane), 100000f, 0f, false);
                int gun = s.World.Spawn(0, shooter, new float3(133f, 0f, lane), 100000f, 0f, false);
                foreach (int u in new[] { man, gun }) { s.World.GoalId[u] = -1; s.World.TrenchId[u] = -1; }
            }
            int worn = 0, shots = 0;
            for (int t = 0; t < ticks; t++)
            {
                Step(m);
                var ev = m.World.Events.Events;
                for (int i = 0; i < ev.Length; i++)
                {
                    if (ev[i].Type == SimEventType.PropWorn && ev[i].B == 1 && ev[i].A == prop) worn++;
                    if (ev[i].Type == SimEventType.Shot && m.World.Team[ev[i].A] == 0) shots++;
                }
                if (twin != null) { Step(twin); Assert.AreEqual(m.World.LastHash, twin.World.LastHash, $"tick {t}"); }
            }
            return (worn, shots);
        }

        [Test]
        public void AMachineGunWearsTheWreckItsTargetSheltersBehind()
        {
            using var m = NewMatch();
            using var twin = NewMatch();
            var (worn, shots) = ShootAtCover(m, InfantryArchetype.Machinegunner, PropKind.Wreck, 600, twin);
            Assert.Greater(shots, 50, "the gun kept firing at him");
            Assert.Greater(worn, 0, "rounds the wreck's cover stopped wore it (PropWorn b = 1)");
            int prop = m.Map.Props.Length - 1;
            Assert.Less(m.Map.Props[prop].Hp, 600f);
            Assert.AreEqual(worn, m.World.GetSystem<DirectFireSystem>().WreckRounds, "one wear per stopped round");
        }

        [Test]
        public void ARifleDoesNotWearIt()
        {
            using var m = NewMatch();
            var (worn, shots) = ShootAtCover(m, InfantryArchetype.Rifle, PropKind.Wreck, 600);
            Assert.Greater(shots, 5, "the rifle kept firing at him");
            Assert.AreEqual(0, worn, "a rifle's rounds do not wear a wreck (CombatTables.WearsWrecks)");
            Assert.AreEqual(600f, m.Map.Props[m.Map.Props.Length - 1].Hp);
        }

        [Test]
        public void ATreeTakesTheRoundsAndStandsAsItWas()
        {
            using var m = NewMatch();
            var (_, shots) = ShootAtCover(m, InfantryArchetype.Machinegunner, PropKind.Tree, 600);
            Assert.Greater(shots, 50);
            var tree = m.Map.Props[m.Map.Props.Length - 1];
            Assert.AreEqual(PropKind.Tree, tree.Kind);
            Assert.AreEqual(PropRules.StartHp(PropKind.Tree), tree.Hp, "trees do not wear under gunfire");
            Assert.AreEqual(0, m.World.GetSystem<DirectFireSystem>().WreckRounds);
        }

        // ---- S4: a gun with nobody to shoot at fires at the wreck its enemies are behind (DirectFire.Wrecks) ----

        const NavLayer NotOpen = NavLayer.Trench | NavLayer.Link | NavLayer.Blocked | NavLayer.Bunker | NavLayer.Wire;

        /// <summary>Three enemy riflemen (team 1) in a trench beside a wreck, below its rim, and one `gun` of team 0 in the
        /// open `distance` metres off along the field with a clear sight of the wreck; everyone held and too tough to die.
        /// Returns the men; the wreck and the gun come out.</summary>
        static int[] Hidden(MatchSim m, byte gun, float distance, out int wreck, out int shooter)
        {
            var map = m.Map;
            for (int t = 0; t < map.TrenchCells.Length; t++)
            {
                var c = map.NavCellCenter(map.TrenchCells[t]);
                foreach (var off in new[] { new float3(2f, 0f, 0f), new float3(-2f, 0f, 0f), new float3(0f, 0f, 2f), new float3(0f, 0f, -2f) })
                {
                    var at = c + off;
                    if ((map.LayerAt(at) & NotOpen) != 0) continue;
                    foreach (float side in new[] { -1f, 1f })
                    {
                        var g = at + new float3(0f, 0f, side * distance);
                        if (g.z < 6f || g.z > map.SizeMeters.y - 6f || (map.LayerAt(g) & NotOpen) != 0) continue;
                        var eye = new float3(g.x, map.Height.Sample(g.x, g.z) + 1.6f, g.z);
                        var top = new float3(at.x, map.Height.Sample(at.x, at.z) + 1.5f, at.z);
                        if (!HeightfieldRaycast.HasLineOfSight(map.Height, eye, top)) continue;
                        wreck = map.AddProp(new PropDef { Pos = at, Kind = PropKind.Wreck });
                        if (wreck < 0) continue;
                        var men = new int[3];
                        for (int k = 0; k < 3; k++)
                        {
                            float dx = (k - 1) * 1.2f;
                            men[k] = m.World.Spawn(1, InfantryArchetype.Rifle, c + new float3(dx, 0f, 0f), 100000f, 0f, false);
                        }
                        shooter = m.World.Spawn(0, gun, g, 100000f, 0f, false);
                        foreach (int u in men) { m.World.GoalId[u] = -1; m.World.TrenchId[u] = -1; }
                        m.World.GoalId[shooter] = -1; m.World.TrenchId[shooter] = -1;
                        return men;
                    }
                }
            }
            Assert.Fail("no trench beside open ground with a clear sight of it");
            wreck = shooter = -1;
            return null;
        }

        // the machine gun stands inside its own reach and outside a rifle's: 150 m of design, cut with every reach
        // (CombatTables.RangeScale, the owner, 2026-09-28): 120 m, between the rifle's 104 and the gun's 136
        const float GunOff = 150f * CombatTables.RangeScale;
        const int Settle = 10;

        /// <summary>Run `ticks` (counting after the first Settle): the shooter's shots at the wreck and at anyone, and the
        /// most suppression any hidden man took.</summary>
        static (int atWreck, int atMen, float suppressed) Watch(MatchSim m, int wreck, int shooter, int[] men, int ticks, bool pinMen = false, MatchSim twin = null)
        {
            int atWreck = 0, atMen = 0; float suppressed = 0f;
            for (int t = 0; t < ticks; t++)
            {
                if (pinMen) foreach (int u in men) { m.World.Suppression[u] = 100f; if (twin != null) twin.World.Suppression[u] = 100f; }
                Step(m);
                if (twin != null) { Step(twin); Assert.AreEqual(m.World.LastHash, twin.World.LastHash, $"tick {t}"); }
                if (t < Settle) continue;   // a man spawned in a trench is flagged in it by his first move: seen until then
                var ev = m.World.Events.Events;
                for (int i = 0; i < ev.Length; i++)
                {
                    if (ev[i].Type != SimEventType.Shot || ev[i].A != shooter) continue;
                    if (PropTarget.IsProp(ev[i].B)) { Assert.AreEqual(wreck, PropTarget.Decode(ev[i].B), "the wreck they are behind"); atWreck++; }
                    else if (ev[i].B >= 0) atMen++;
                }
                foreach (int u in men) suppressed = math.max(suppressed, m.World.Suppression[u]);
            }
            return (atWreck, atMen, suppressed);
        }

        [Test]
        public void AnIdleMachineGunFiresAtTheWreckHiddenEnemiesAreBehind()
        {
            using var m = NewMatch();
            using var twin = NewMatch();
            var men = Hidden(m, InfantryArchetype.Machinegunner, GunOff, out int wreck, out int gun);
            Hidden(twin, InfantryArchetype.Machinegunner, GunOff, out _, out _);
            var (atWreck, atMen, suppressed) = Watch(m, wreck, gun, men, 400, twin: twin);
            Assert.AreEqual(0, atMen, "below the rim and out of their rifles' range, nobody is seen");
            Assert.Greater(atWreck, 20, "so the gun fires at the wreck they are behind (Shot.b names it)");
            Assert.Greater(suppressed, 20f, "and keeps their heads down");
            Assert.Less(m.Map.Props[wreck].Hp, 600f, "a machine gun's hits wear it");
        }

        [Test]
        public void ARifleFiresAtItButDoesNotWearIt()
        {
            using var m = NewMatch();
            var men = Hidden(m, InfantryArchetype.Rifle, 100f, out int wreck, out int gun);
            var (atWreck, _, _) = Watch(m, wreck, gun, men, 400, pinMen: true);
            Assert.Greater(atWreck, 5, "a rifle keeps them down too");
            Assert.AreEqual(600f, m.Map.Props[wreck].Hp, "but its rounds do not wear a wreck");
        }

        [Test]
        public void ALivingTargetAlwaysWins()
        {
            using var m = NewMatch();
            var men = Hidden(m, InfantryArchetype.Machinegunner, GunOff, out int wreck, out int gun);
            // an enemy in the open, 20 m from the gun, toward nobody's trench: seen at once
            var p = m.World.Position[gun];
            int bait = -1;
            foreach (var off in new[] { new float3(20f, 0f, 0f), new float3(-20f, 0f, 0f) })
                if (bait < 0 && (m.Map.LayerAt(p + off) & NotOpen) == 0) bait = m.World.Spawn(1, InfantryArchetype.Rifle, p + off, 100000f, 0f, false);
            Assert.GreaterOrEqual(bait, 0);
            m.World.GoalId[bait] = -1; m.World.TrenchId[bait] = -1;
            var (atWreck, atMen, _) = Watch(m, wreck, gun, men, 300);
            Assert.Greater(atMen, 20, "the man it can see");
            Assert.AreEqual(0, atWreck, "never the wreck while a living target stands");
        }

        [Test]
        // [N5.2] a knocked-out hull has no target (TargetAcquisition clears it) and the fire job sent it to AtWreck,
        // which tested Airborne, pinned, garrison and weapon but not KnockedOut: a dead hull kept machine-gunning.
        public void AKnockedOutHullDoesNotFireAtAWreck()
        {
            using var m = NewMatch();
            var men = Hidden(m, InfantryArchetype.Machinegunner, GunOff, out int wreck, out _);
            var wp = m.Map.Props[wreck].Pos;
            int hull = -1;
            foreach (var off in new[] { new float3(0f, 0f, 30f), new float3(0f, 0f, -30f), new float3(30f, 0f, 0f), new float3(-30f, 0f, 0f) })
            {
                var at = wp + off;
                if (at.x < 6f || at.z < 6f || at.x > m.Map.SizeMeters.x - 6f || at.z > m.Map.SizeMeters.y - 6f) continue;
                if ((m.Map.LayerAt(at) & NotOpen) != 0) continue;
                var eye = new float3(at.x, m.Map.Height.Sample(at.x, at.z) + 1.6f, at.z);
                var top = new float3(wp.x, m.Map.Height.Sample(wp.x, wp.z) + 1.5f, wp.z);
                if (!HeightfieldRaycast.HasLineOfSight(m.Map.Height, eye, top)) continue;
                var e = m.World.Units.Roster[VehicleArchetype.Tusk];
                hull = m.World.Spawn(0, VehicleArchetype.Tusk, at, e.Hp, e.Speed, true);
                break;
            }
            Assert.GreaterOrEqual(hull, 0, "a spot for the hull in sight of the wreck");
            m.World.GoalId[hull] = -1; m.World.TrenchId[hull] = -1;
            Step(m);
            // knocked out as VehicleModulesSystem reads it: the state sticks while its ticks run down
            m.Modules.State[hull] = (byte)VehicleState.KnockedOut;
            m.Modules.StateTicks[hull] = 400;
            Step(m);   // the modules system (after the fire job) is what puts the KnockedOut flag on the slot
            int shots = 0;
            for (int t = 0; t < 60; t++)
            {
                Step(m);
                var ev = m.World.Events.Events;
                for (int i = 0; i < ev.Length; i++) if (ev[i].Type == SimEventType.Shot && ev[i].A == hull) shots++;
            }
            Assert.AreEqual((byte)VehicleState.KnockedOut, m.Modules.State[hull], "the hulk is still knocked out");
            Assert.AreEqual(0, shots, "[N5.2] a knocked-out hull fired at a wreck");
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
            Assert.AreEqual((int)SimEventType.PounceLanded + 1, (int)SimEventType.PropWorn);   // after hand to hand's five (v25)
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
