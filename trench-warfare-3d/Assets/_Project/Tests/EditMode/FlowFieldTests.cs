// Phase: A1 (implemented) — a path that follows the field from the team 0 spawn must reach the enemy HQ, crossing
// the trench lines on its way. Since 2026-09-28 it crosses them where it meets them (FlowField.CanStepInfantry):
// until then it could only enter a trench through a Link cell, which is what put every company on the same ladders.
using NUnit.Framework;
using Unity.Collections;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class FlowFieldTests
    {
        /// <summary>
        /// Open ground before a goal as wide as the field is crossed straight. Until 2026-09-28 a cell pointed at the
        /// neighbour with the lowest integration and ties went to the lowest direction index: the three cells ahead
        /// tie before a wide goal, so every such cell pointed north-east, and a company crossed the field in file on
        /// one diagonal. A ladder's own column is left out: the way on from there steps aside to the wall beside it,
        /// which is cheaper than the ladder, and where it steps aside is a tie.
        /// </summary>
        [Test]
        public void OpenGroundBeforeAWideGoal_IsCrossedStraight()
        {
            using var map = GreyboxMapGenerator.Create(Allocator.Persistent);
            var field = new FlowField(map.NavWidth, map.NavLength, Allocator.Persistent);
            var def = map.Trenches[0];
            var goals = new NativeList<int>(Allocator.Temp);
            for (int c = 0; c < def.CellCount; c++)   // the trench as a goal, as FlowFieldManager builds it: not its ladders
            {
                int cell = map.TrenchCells[def.CellStart + c];
                if ((map.NavLayers[cell] & (byte)NavLayer.Link) == 0) goals.Add(cell);
            }
            field.Build(map, goals.AsArray());
            goals.Dispose();
            int trenchZ = map.TrenchCells[def.CellStart] / map.NavWidth;
            int straight = 0;
            for (int z = trenchZ - 30; z < trenchZ - 2; z++)
                for (int x = 2; x < map.NavWidth - 2; x++)
                {
                    if (x % 10 == 5) continue;   // a ladder's column
                    Assert.AreEqual(2, field.Direction[map.NavIndex(x, z)], $"cell {x},{z} before the trench at z {trenchZ} points north");
                    straight++;
                }
            Assert.Greater(straight, 1000);
            field.Dispose();
        }

        [Test]
        public void GreyboxField_ReachesGoalAcrossTheTrenches()
        {
            using var map = GreyboxMapGenerator.Create(Allocator.Persistent);
            var field = new FlowField(map.NavWidth, map.NavLength, Allocator.Persistent);
            var goals = new NativeList<int>(Allocator.Temp);
            foreach (var o in map.Objectives) if (o.Kind == ObjectiveKind.HQ && o.SideTeam == 1) for (int c = 0; c < o.CellCount; c++) goals.Add(map.ObjectiveCells[o.CellStart + c]);
            field.Build(map, goals.AsArray());
            goals.Dispose();

            var start = map.NavCellOf(map.Spawns[0].Pos);
            int cell = map.NavIndex(start.x, start.y);
            Assert.AreNotEqual(FlowField.Unreachable, field.Integration[cell], "spawn must reach the goal");
            int steps = 0;
            int trenchEntries = 0;
            while (field.Integration[cell] != 0 && steps++ < map.NavWidth * map.NavLength)
            {
                byte d = field.Direction[cell];
                Assert.AreNotEqual(FlowField.NoDirection, d, $"no direction at cell {cell}");
                int x = cell % map.NavWidth, z = cell / map.NavWidth;
                var o = FlowField.Offset(d);
                int nx = x + (int)System.Math.Round(o.x), nz = z + (int)System.Math.Round(o.y);
                int next = map.NavIndex(nx, nz);
                var from = (NavLayer)map.NavLayers[cell];
                var to = (NavLayer)map.NavLayers[next];
                if ((to & NavLayer.Trench) != 0 && (from & NavLayer.Trench) == 0) trenchEntries++;
                Assert.Less(field.Integration[next], field.Integration[cell], "integration must strictly decrease along the field");
                cell = next;
            }
            Assert.AreEqual(0, field.Integration[cell], "walk must end on a goal cell");
            Assert.GreaterOrEqual(trenchEntries, 1, "the path crosses at least one trench line");
            field.Dispose();
        }

        [Test]
        public void BlockedCells_AreUnreachable()
        {
            using var map = new MapData(99, new Unity.Mathematics.float2(20f, 20f), Allocator.Persistent);
            for (int x = 0; x < map.NavWidth; x++) map.SetLayer(x, 5, NavLayer.Blocked); // wall across the map
            var field = new FlowField(map.NavWidth, map.NavLength, Allocator.Persistent);
            using var goals = new NativeArray<int>(new[] { map.NavIndex(2, 8) }, Allocator.Temp);
            field.Build(map, goals);
            Assert.AreEqual(FlowField.Unreachable, field.Integration[map.NavIndex(2, 2)]);
            Assert.AreNotEqual(FlowField.Unreachable, field.Integration[map.NavIndex(9, 9)]);
            field.Dispose();
        }
    }
}
