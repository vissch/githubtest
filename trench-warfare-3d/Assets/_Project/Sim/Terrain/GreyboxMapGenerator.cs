// Phase: P0 (implemented)
// Procedural flat corridor used by M1, the determinism tests and the platform gate: one fire trench per side,
// links every 20 m, four objectives, spawn points and a straight supply road. No wire, no mud, no craters.
using Unity.Collections;
using Unity.Mathematics;

namespace TW.Sim.Terrain
{
    public static class GreyboxMapGenerator
    {
        public const int MapId = 1;

        public static MapData Create(Allocator allocator, float width = 300f, float length = 800f)
        {
            var map = new MapData(MapId, new float2(width, length), allocator);
            // Slight bowl so LoS tests have something to hit: ground rises 2 m toward the centre of Z.
            for (int z = 0; z < map.Height.Length; z++)
            {
                float t = z / (float)map.Height.Length;
                float h = 2f * (1f - math.abs(t - 0.5f) * 2f);
                for (int x = 0; x < map.Height.Width; x++) map.Height.Set(x, z, h);
            }
            // Team 0 trench at Z = 120 m, team 1 trench at Z = length - 120 m.
            AddFireTrench(map, 0, (short)0, 120f, 0f, next0: 1, next1: -1);
            AddFireTrench(map, 1, (short)1, length - 120f, SimMath.Pi, next0: -1, next1: 0);

            AddLineObjective(map, 0, ObjectiveKind.MainLine, sideTeam: 0, ownerTeam: 0, z: 120f, orderIndex: 1);
            AddLineObjective(map, 1, ObjectiveKind.HQ, sideTeam: 0, ownerTeam: 0, z: 40f, orderIndex: 2);
            AddLineObjective(map, 2, ObjectiveKind.MainLine, sideTeam: 1, ownerTeam: 1, z: length - 120f, orderIndex: 1);
            AddLineObjective(map, 3, ObjectiveKind.HQ, sideTeam: 1, ownerTeam: 1, z: length - 40f, orderIndex: 2);

            map.Spawns.Add(new SpawnPoint { Team = 0, Pos = new float3(width * 0.5f, 0f, 10f), Kind = 0 });
            map.Spawns.Add(new SpawnPoint { Team = 1, Pos = new float3(width * 0.5f, 0f, length - 10f), Kind = 0 });
            map.SupplyRoad.Add(new float3(width * 0.5f, 0f, 0f));
            map.SupplyRoad.Add(new float3(width * 0.5f, 0f, 60f));
            map.SupplyRoad.Add(new float3(width * 0.5f, 0f, length));
            map.SupplyRoad.Add(new float3(width * 0.5f, 0f, length - 60f));
            map.Wind = new float2(0f, -1f);
            map.RebuildCost();
            return map;
        }

        /// <summary>A straight fire trench across the full width at world Z, 2 cells (4 m) deep in Z, with a link every 10 cells.</summary>
        static void AddFireTrench(MapData map, byte team, short id, float z, float facingYaw, short next0, short next1)
        {
            int zc = (int)(z / MapData.NavCellSize);
            var def = new TrenchDef
            {
                Id = id, OwnerTeam = team, Kind = 0, FacingYaw = facingYaw,
                CellStart = map.TrenchCells.Length, FireStepStart = map.FireStepCells.Length, LinkStart = map.LinkCells.Length,
                NextTrenchForTeam0 = next0, NextTrenchForTeam1 = next1,
            };
            for (int x = 2; x < map.NavWidth - 2; x++)
            {
                for (int dz = 0; dz < 2; dz++)
                {
                    int cz = zc + dz;
                    bool link = (x % 10) == 5;
                    map.SetLayer(x, cz, link ? (NavLayer.Trench | NavLayer.Link) : NavLayer.Trench);
                    int idx = map.NavIndex(x, cz);
                    map.TrenchCells.Add(idx);
                    if (link) map.LinkCells.Add(idx);
                }
                // fire-step is the trench cell on the enemy-facing side
                int fsz = team == 0 ? zc + 1 : zc;
                map.FireStepCells.Add(map.NavIndex(x, fsz));
                // carve the heightfield 1.8 m down across the trench footprint
                for (int hz = zc * 2; hz < zc * 2 + 4; hz++)
                    for (int hx = x * 2; hx < x * 2 + 2; hx++)
                        map.Height.Set(hx, hz, map.Height.HeightAtCell(hx, hz) - 1.8f);
            }
            def.CellCount = map.TrenchCells.Length - def.CellStart;
            def.FireStepCount = map.FireStepCells.Length - def.FireStepStart;
            def.LinkCount = map.LinkCells.Length - def.LinkStart;
            map.Trenches.Add(def);
            // parapet cover: one volume per 20 m facing the enemy
            for (float x = 10f; x < map.SizeMeters.x; x += 20f)
                map.StaticCover.Add(new CoverVolume { Center = new float3(x, 0f, z + 2f), Radius = 10f, ArcCenterYaw = facingYaw, ArcHalfWidth = SimMath.HalfPi, Bonus = 0.85f, Height = 1.5f, OwnerSlot = -1 });
        }

        static void AddLineObjective(MapData map, short id, ObjectiveKind kind, byte sideTeam, byte ownerTeam, float z, short orderIndex)
        {
            int zc = (int)(z / MapData.NavCellSize);
            var def = new ObjectiveDef { Id = id, Kind = kind, OwnerTeam = ownerTeam, SideTeam = sideTeam, CellStart = map.ObjectiveCells.Length, RequiredUnits = 3, CaptureTicks = 200, OrderIndex = orderIndex };
            for (int x = 0; x < map.NavWidth; x++) for (int dz = 0; dz < 2; dz++) map.ObjectiveCells.Add(map.NavIndex(x, zc + dz));
            def.CellCount = map.ObjectiveCells.Length - def.CellStart;
            map.Objectives.Add(def);
        }
    }
}
