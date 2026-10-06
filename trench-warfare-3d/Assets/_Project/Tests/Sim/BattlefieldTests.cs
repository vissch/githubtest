// Phase: A4 (implemented) — the battlefield reacts: a crater below the water table turns wet but never impassable,
// blasts wear trees down to stumps and take their cover and their blocked cell with them, a destroyed vehicle leaves
// a wreck, a shell on wire opens it, and all of it is deterministic.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class BattlefieldTests
    {
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

        static NavLayer LayerAt(MapData map, float x, float z) => map.LayerAt(new float3(x, 0f, z));

        [Test]
        public void Crater_BelowTheWaterTable_TurnsWet_ButNeverImpassable()
        {
            using var m = NewMatch();
            var map = m.Map;
            map.WaterLevel = 0.5f;   // the playtest ground runs from 0 m at the map ends to 2 m in the middle
            float lowZ = 101f, highZ = 241f;
            Assert.AreEqual(NavLayer.None, LayerAt(map, 151f, lowZ) & NavLayer.Mud, "setup: dry before the shell");

            new CraterStamp { Center = new float3(151f, 0f, lowZ), Radius = 4f, Depth = 1.6f }.Apply(map);
            new CraterStamp { Center = new float3(151f, 0f, highZ), Radius = 4f, Depth = 1.2f }.Apply(map);

            var low = LayerAt(map, 151f, lowZ); var high = LayerAt(map, 151f, highZ);
            Assert.AreNotEqual(NavLayer.None, low & NavLayer.Crater);
            Assert.AreNotEqual(NavLayer.None, low & NavLayer.Mud, "a hole that goes under the water table fills");
            Assert.AreEqual(NavLayer.None, low & NavLayer.Blocked, "a shell never makes ground impassable");
            Assert.AreNotEqual(NavLayer.None, high & NavLayer.Crater);
            Assert.AreEqual(NavLayer.None, high & NavLayer.Mud, "a crater on high ground stays dry");
        }

        [Test]
        public void Blasts_WearATreeDownToAStump_AndItsCoverAndBlockedCellGoWithIt()
        {
            using var m = NewMatch();
            var map = m.Map;
            var at = new float3(151f, 0f, 241f);
            int tree = map.AddProp(new PropDef { Pos = at, Kind = PropKind.Tree });
            Assert.GreaterOrEqual(tree, 0);
            int cell = map.Props[tree].Cell;
            Assert.AreNotEqual(NavLayer.None, (NavLayer)map.NavLayers[cell] & NavLayer.Blocked, "a standing tree blocks its cell");
            Assert.AreEqual(PropRules.CoverPercent(PropKind.Tree), map.CellCover[cell + 1], "and covers the cells around it");

            m.Blast.Queue(new Impact { Pos = at, Damage = 300f, Radius = 10f, Player = -1 });
            Step(m);
            Assert.AreEqual(PropKind.BrokenTree, map.Props[tree].Kind);
            Assert.AreNotEqual(NavLayer.None, (NavLayer)map.NavLayers[cell] & NavLayer.Blocked);

            m.Blast.Queue(new Impact { Pos = at, Damage = 300f, Radius = 10f, Player = -1 });
            Step(m);
            Assert.AreEqual(PropKind.Stump, map.Props[tree].Kind);
            Assert.AreEqual(NavLayer.None, (NavLayer)map.NavLayers[cell] & NavLayer.Blocked, "a stump can be walked over");
            Assert.AreEqual(PropRules.CoverPercent(PropKind.Stump), map.CellCover[cell]);

            m.Blast.Queue(new Impact { Pos = at, Damage = 300f, Radius = 10f, Player = -1 });
            Step(m);
            Assert.AreEqual(PropKind.Stump, map.Props[tree].Kind, "nothing left to break");
            Assert.AreEqual(2, m.Deformation.PropsChanged);
        }

        [Test]
        public void ADestroyedVehicle_LeavesAWreck_ThatBlocksAndCovers()
        {
            using var m = NewMatch();
            var map = m.Map;
            var at = new float3(151f, 0f, 241f);
            int tank = m.World.Spawn(0, 4, at, 100f, 1.6f, true);
            Assert.GreaterOrEqual(tank, 0);
            m.Blast.Queue(new Impact { Pos = at, Damage = 50000f, Radius = 6f, Player = -1 });
            Step(m);
            Assert.IsFalse(m.World.IsAlive(tank));
            Assert.AreEqual(1, map.Props.Length);
            Assert.AreEqual(PropKind.Wreck, map.Props[0].Kind);
            Assert.AreNotEqual(NavLayer.None, (NavLayer)map.NavLayers[map.Props[0].Cell] & NavLayer.Blocked);
            Assert.AreEqual(PropRules.CoverPercent(PropKind.Wreck), map.CellCover[map.Props[0].Cell + 1]);
        }

        [Test]
        public void AShellOnWire_OpensIt()
        {
            using var m = NewMatch();
            var map = m.Map;
            new WireBelt { Min = new float3(100f, 0f, 200f), Max = new float3(200f, 0f, 205f) }.Place(map);
            Assert.AreNotEqual(NavLayer.None, LayerAt(map, 151f, 203f) & NavLayer.Wire);
            m.Blast.Queue(new Impact { Pos = new float3(151f, 0f, 203f), Damage = 100f, Radius = 8f, CraterRadius = 3f, CraterDepth = 1f, Player = -1 });
            Step(m);
            Assert.AreEqual(NavLayer.None, LayerAt(map, 151f, 203f) & NavLayer.Wire, "the wire under the crater is gone");
            Assert.AreNotEqual(NavLayer.None, LayerAt(map, 111f, 203f) & NavLayer.Wire, "the rest of the belt stands");
            Assert.Greater(m.Deformation.WireOpened, 0);
        }

        static int Count(MapData map, NavLayer bit)
        {
            int n = 0;
            for (int i = 0; i < map.NavLayers.Length; i++) if ((map.NavLayers[i] & (byte)bit) != 0) n++;
            return n;
        }

        [Test]
        public void Generator_SameParamsSameMap_DifferentSeedDifferentMap()
        {
            using var a = BattlefieldGenerator.Create(BattlefieldParams.ShelledForest(7), Allocator.Persistent);
            using var b = BattlefieldGenerator.Create(BattlefieldParams.ShelledForest(7), Allocator.Persistent);
            using var c = BattlefieldGenerator.Create(BattlefieldParams.ShelledForest(8), Allocator.Persistent);
            Assert.AreEqual(a.Hash(SimHash.Offset), b.Hash(SimHash.Offset));
            Assert.AreNotEqual(a.Hash(SimHash.Offset), c.Hash(SimHash.Offset));
            var round = BattlefieldParams.Deserialize(BattlefieldParams.ShelledForest(7).Serialize());
            using var d = BattlefieldGenerator.Create(round, Allocator.Persistent);
            Assert.AreEqual(a.Hash(SimHash.Offset), d.Hash(SimHash.Offset), "a replay header rebuilds the same map");
        }

        [Test]
        public void Generator_MakesAShelledWoodWithARiver()
        {
            using var map = BattlefieldGenerator.Create(BattlefieldParams.ShelledForest(1917), Allocator.Persistent);
            Assert.Greater(Count(map, NavLayer.Crater), 200, "shell holes");
            Assert.Greater(Count(map, NavLayer.Mud), 900, "mud");
            Assert.Greater(Count(map, NavLayer.Wire), 90, "wire");
            Assert.Greater(Count(map, NavLayer.Blocked), 50, "the river channel and solid props");   // 90 x 240 m: a quarter of the first field's area
            int standing = 0, broken = 0, wrecks = 0;
            for (int i = 0; i < map.Props.Length; i++)
            {
                var kind = map.Props[i].Kind;
                if (kind == PropKind.Tree) standing++; else if (kind == PropKind.Wreck) wrecks++; else if (kind != PropKind.Bridge) broken++;
            }
            Assert.Greater(standing, 20, "some trees are still up");
            Assert.Greater(broken, standing, "most of the wood is broken");
            Assert.Greater(wrecks, 0);
            Assert.AreEqual(4, map.Trenches.Length);
        }

        [Test]
        public void Generator_BothSidesCanAlwaysReachEachOther()
        {
            for (uint seed = 1; seed <= 40; seed++)
            {
                using var map = BattlefieldGenerator.Create(BattlefieldParams.ShelledForest(seed), Allocator.Persistent);
                Assert.IsTrue(BattlefieldGenerator.Connected(map), $"seed {seed}: the river or the wood cut the map in two");
            }
        }

        [Test]
        public void TwoSims_OnAGeneratedBattlefield_StayInSync_ThroughABarrage()
        {
            var cfg = SimConfig.Default; cfg.Seed = 99; cfg.StartingSilver = 100000;
            using var a = MatchSim.CreateBattlefield(cfg, BattlefieldParams.ShelledForest(1917));
            using var b = MatchSim.CreateBattlefield(cfg, BattlefieldParams.ShelledForest(1917));
            for (int t = 0; t < 900; t++)
            {
                var cmds = new System.Collections.Generic.List<SimCommand>();
                if (t < 40 && t % 2 == 0) { cmds.Add(SimCommand.Deploy(a.World.Tick, 0, 0)); cmds.Add(SimCommand.Deploy(a.World.Tick, 1, 0)); }
                if (t == 300) cmds.Add(new SimCommand { Tick = a.World.Tick, Player = 0, Type = CommandType.SupportFire, A = (int)OffMapAbilityId.HeBarrage, Pos = new float3(45f, 0f, 165f) });
                if (t == 320) cmds.Add(new SimCommand { Tick = a.World.Tick, Player = 1, Type = CommandType.SupportFire, A = (int)OffMapAbilityId.HeBarrage, Pos = new float3(42f, 0f, 100f) });
                using var arr = new NativeArray<SimCommand>(cmds.ToArray(), Allocator.Temp);
                a.Step(arr); b.Step(arr);
                Assert.AreEqual(a.World.LastHash, b.World.LastHash, $"tick {t}");
            }
            Assert.Greater(a.Deformation.Applied, 0, "the barrage landed");
        }

        [Test]
        public void AmbientBombardment_FallsAtTheAskedRate_AndNotAtAllWhenQuiet()
        {
            var cfg = SimConfig.Default; cfg.Seed = 5;
            var loud = BattlefieldParams.ShelledForest(3); loud.Bombardment = 60f;
            var quiet = BattlefieldParams.ShelledForest(3); quiet.Bombardment = 0f;
            using var a = MatchSim.CreateBattlefield(cfg, loud);
            using var b = MatchSim.CreateBattlefield(cfg, quiet);
            for (int t = 0; t < 1200; t++) { Step(a); Step(b); }   // one minute
            Assert.That(a.Bombardment.Fired, Is.InRange(35, 90), "about sixty shells in a minute");
            Assert.AreEqual(a.Bombardment.Fired, a.Deformation.Applied + a.Deformation.Queue.Length, "every shell leaves a crater");
            Assert.AreEqual(0, b.Bombardment.Fired);
            Assert.AreEqual(0, b.Deformation.Applied);
        }

        [Test]
        public void EnvironmentEdits_AreDeterministic()
        {
            ulong Run()
            {
                using var m = NewMatch();
                m.Map.WaterLevel = 0.5f;
                for (int k = 0; k < 12; k++) m.Map.AddProp(new PropDef { Pos = new float3(100f + k * 9f, 0f, 230f + (k % 3) * 8f), Kind = PropKind.Tree });
                new WireBelt { Min = new float3(60f, 0f, 200f), Max = new float3(240f, 0f, 204f) }.Place(m.Map);
                for (int t = 0; t < 60; t++)
                {
                    if (t % 5 == 0) m.Blast.Queue(new Impact { Pos = new float3(90f + t * 2f, 0f, 200f + t), Damage = 260f, Radius = 12f, CraterRadius = 3f, CraterDepth = 1.4f, Player = -1 });
                    Step(m);
                }
                return m.Map.Hash(m.World.LastHash);
            }
            Assert.AreEqual(Run(), Run());
        }
    }
}
