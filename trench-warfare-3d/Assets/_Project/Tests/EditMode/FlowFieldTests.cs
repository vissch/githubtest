// Phase: A1 (implemented) — a path that follows the field from the team 0 spawn must reach the enemy HQ and
// may only enter trench cells through Link cells.
using NUnit.Framework;
using Unity.Collections;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class FlowFieldTests
    {
        [Test]
        public void GreyboxField_ReachesGoalThroughLinks()
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
                if ((to & NavLayer.Trench) != 0 && (from & NavLayer.Trench) == 0)
                {
                    trenchEntries++;
                    Assert.IsTrue((to & NavLayer.Link) != 0 || (from & NavLayer.Link) != 0, "trench entered without a link");
                }
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
