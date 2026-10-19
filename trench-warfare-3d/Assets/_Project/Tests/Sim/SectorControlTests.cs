// Phase: A3 core — the order a side may take objectives in, and that progress drains whenever the cells are
// uncontested, freezing only while both sides are inside. Attacking the other side's line it works up from the
// lowest OrderIndex; winning back its own line it works nearest first, from the highest (the owner's call of
// 2026-10-07). No map: the defs and the states are made by hand, which is all SectorControlSystem.Unlocked reads.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class SectorControlTests
    {
        // Six objectives, three a side, as BattlefieldGenerator lays them out: Main 1, Reserve 2, HQ 3.
        const int Main0 = 0, Reserve0 = 1, Hq0 = 2, Main1 = 3, Reserve1 = 4, Hq1 = 5;

        static NativeList<ObjectiveDef> Objectives()
        {
            var list = new NativeList<ObjectiveDef>(6, Allocator.Temp);
            for (byte side = 0; side < 2; side++)
            {
                list.Add(new ObjectiveDef { Id = (short)(side * 3 + 0), Kind = ObjectiveKind.MainLine,    SideTeam = side, OwnerTeam = side, OrderIndex = 1 });
                list.Add(new ObjectiveDef { Id = (short)(side * 3 + 1), Kind = ObjectiveKind.ReserveLine, SideTeam = side, OwnerTeam = side, OrderIndex = 2 });
                list.Add(new ObjectiveDef { Id = (short)(side * 3 + 2), Kind = ObjectiveKind.HQ,          SideTeam = side, OwnerTeam = side, OrderIndex = 3 });
            }
            return list;
        }

        // Side 0's Main and Reserve are in team 1's hands; its HQ is still its own. Side 1 is all team 1's.
        static NativeArray<ObjectiveState> States()
        {
            var s = new NativeArray<ObjectiveState>(6, Allocator.Temp);
            s[Main0]    = new ObjectiveState { Owner = 1, CapturingTeam = 255 };
            s[Reserve0] = new ObjectiveState { Owner = 1, CapturingTeam = 255 };
            s[Hq0]      = new ObjectiveState { Owner = 0, CapturingTeam = 255 };
            s[Main1]    = new ObjectiveState { Owner = 1, CapturingTeam = 255 };
            s[Reserve1] = new ObjectiveState { Owner = 1, CapturingTeam = 255 };
            s[Hq1]      = new ObjectiveState { Owner = 1, CapturingTeam = 255 };
            return s;
        }

        // [U6] A side wins its own trenches back nearest first: Reserve, then Main.
        [Test]
        public void ASideWinsItsOwnTrenchesBackNearestFirst()
        {
            var objectives = Objectives();
            var states = States();
            try
            {
                Assert.IsTrue(SectorControlSystem.Unlocked(objectives, states, Reserve0, 0),
                    "[U6] team 0 may win back its own Reserve line first: its HQ behind it is still its own");
                Assert.IsFalse(SectorControlSystem.Unlocked(objectives, states, Main0, 0),
                    "[U6] team 0 may not win back its own Main line while its Reserve line behind it is still lost");

                states[Reserve0] = new ObjectiveState { Owner = 0, CapturingTeam = 255 };
                Assert.IsTrue(SectorControlSystem.Unlocked(objectives, states, Main0, 0),
                    "[U6] with its Reserve line back, team 0 may win back its own Main line");
            }
            finally { objectives.Dispose(); states.Dispose(); }
        }

        // Attacking the enemy's line keeps the old order: the lowest OrderIndex first.
        // A guard: green on the old rule too.
        [Test]
        public void AttackingTheEnemyKeepsItsOrder()
        {
            var objectives = Objectives();
            var states = States();
            try
            {
                Assert.IsFalse(SectorControlSystem.Unlocked(objectives, states, Reserve1, 0),
                    "on side 1 team 0 still needs side 1's Main line before its Reserve line");
                Assert.IsTrue(SectorControlSystem.Unlocked(objectives, states, Main1, 0),
                    "side 1's Main line is the first of the enemy's that team 0 may take");

                states[Main1] = new ObjectiveState { Owner = 0, CapturingTeam = 255 };
                Assert.IsTrue(SectorControlSystem.Unlocked(objectives, states, Reserve1, 0),
                    "with side 1's Main line taken, team 0 may go on to its Reserve line");
            }
            finally { objectives.Dispose(); states.Dispose(); }
        }

        // [found-fix-sectorcontrol-capture-progress-freezing] a real greybox map, objective 2: side 1's MainLine
        // (z = length - 120, OrderIndex 1, owner team 1), so team 0 is the attacker and it is Unlocked.
        const int ObjIndex = 2;

        static MatchSim NewMatch() => MatchSim.CreateGreybox(SimConfig.Default, combat: false);

        static void Step(MatchSim m)
        {
            using var arr = new NativeArray<SimCommand>(0, Allocator.Temp);
            m.Step(arr);
        }

        static float3 CellAt(MatchSim m, ObjectiveDef def, int k) => m.Map.NavCellCenter(m.Map.ObjectiveCells[def.CellStart + k]);

        [Test]
        public void ProgressDrainsWhenAnAttackerLeaves()
        {
            using var m = NewMatch();
            var def = m.Map.Objectives[ObjIndex];
            int a = m.World.Spawn(0, 0, CellAt(m, def, 0), 100f, 0f, false);
            int b = m.World.Spawn(0, 0, CellAt(m, def, 2), 100f, 0f, false);
            int c = m.World.Spawn(0, 0, CellAt(m, def, 4), 100f, 0f, false);

            for (int t = 0; t < def.CaptureTicks - 1; t++)
            {
                m.World.Position[a] = CellAt(m, def, 0);
                m.World.Position[b] = CellAt(m, def, 2);
                m.World.Position[c] = CellAt(m, def, 4);
                Step(m);
            }
            var s = m.Sectors.States[ObjIndex];
            Assert.AreEqual(def.CaptureTicks - 1, s.CaptureProgressTicks, "grows a tick at a time while uncontested and above RequiredUnits");
            Assert.AreEqual(0, s.CapturingTeam);
            Assert.AreEqual(1, s.Owner, "not yet flipped");

            // One attacker leaves: two remain, no defenders. Not contested, so progress must drain, not freeze.
            m.World.Position[c] = CellAt(m, def, 4) - new float3(0f, 0f, 60f);
            for (int t = 0; t < 50; t++)
            {
                m.World.Position[a] = CellAt(m, def, 0);
                m.World.Position[b] = CellAt(m, def, 2);
                Step(m);
            }
            s = m.Sectors.States[ObjIndex];
            Assert.AreEqual(def.CaptureTicks - 1 - 50, s.CaptureProgressTicks, "drains at the same rate as the header promises");

            for (int t = 0; t < 350; t++)
            {
                m.World.Position[a] = CellAt(m, def, 0);
                m.World.Position[b] = CellAt(m, def, 2);
                Step(m);
            }
            s = m.Sectors.States[ObjIndex];
            Assert.AreEqual(0, s.CaptureProgressTicks, "drained fully");
            Assert.AreEqual(255, s.CapturingTeam, "the HUD no longer names a capture that is not running");
            Assert.AreEqual(1, s.Owner);

            // The third man comes back: the objective must not flip in one tick, it needs CaptureTicks again.
            for (int t = 0; t < 5; t++)
            {
                m.World.Position[a] = CellAt(m, def, 0);
                m.World.Position[b] = CellAt(m, def, 2);
                m.World.Position[c] = CellAt(m, def, 4);
                Step(m);
            }
            s = m.Sectors.States[ObjIndex];
            Assert.AreEqual(1, s.Owner, "the drain is not a one-tick flip in reverse either");
            Assert.AreEqual(5, s.CaptureProgressTicks);
        }

        [Test]
        public void ContestedCellsFreezeProgress()
        {
            using var m = NewMatch();
            var def = m.Map.Objectives[ObjIndex];
            int a = m.World.Spawn(0, 0, CellAt(m, def, 0), 100f, 0f, false);
            int b = m.World.Spawn(0, 0, CellAt(m, def, 2), 100f, 0f, false);
            int c = m.World.Spawn(0, 0, CellAt(m, def, 4), 100f, 0f, false);

            for (int t = 0; t < 30; t++)
            {
                m.World.Position[a] = CellAt(m, def, 0);
                m.World.Position[b] = CellAt(m, def, 2);
                m.World.Position[c] = CellAt(m, def, 4);
                Step(m);
            }
            var before = m.Sectors.States[ObjIndex].CaptureProgressTicks;

            int d = m.World.Spawn(1, 0, CellAt(m, def, 6), 100f, 0f, false);
            for (int t = 0; t < 20; t++)
            {
                m.World.Position[a] = CellAt(m, def, 0);
                m.World.Position[b] = CellAt(m, def, 2);
                m.World.Position[c] = CellAt(m, def, 4);
                m.World.Position[d] = CellAt(m, def, 6);
                Step(m);
            }
            var s = m.Sectors.States[ObjIndex];
            Assert.AreEqual(before, s.CaptureProgressTicks, "a defender inside contests the cells and freezes progress");
            Assert.AreEqual(0, s.CapturingTeam);
        }
    }
}
