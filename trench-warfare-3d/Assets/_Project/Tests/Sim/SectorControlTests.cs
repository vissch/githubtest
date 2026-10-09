// Phase: A3 core — the order a side may take objectives in. Attacking the other side's line it works up from the
// lowest OrderIndex; winning back its own line it works nearest first, from the highest (the owner's call of
// 2026-10-07). No map: the defs and the states are made by hand, which is all SectorControlSystem.Unlocked reads.
using NUnit.Framework;
using Unity.Collections;
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
    }
}
