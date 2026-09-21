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
