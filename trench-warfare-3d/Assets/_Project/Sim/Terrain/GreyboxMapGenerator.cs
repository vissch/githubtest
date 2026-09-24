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

        /// <summary>
        /// The fire step: the enemy-facing row of a trench is a ledge this far above its floor, not part of it.
        /// Without it a man at a firing post stands on the floor 1.7 m down and his rifle is about 0.4 m BELOW
        /// the parapet, so he aims into the earth he is meant to be shooting over (measured 2026-09-23).
        /// 0.7 m is about the two feet a real fire step was, and leaves his head and his rifle over the lip.
        /// </summary>
        public const float FireStepRise = 0.7f;

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
            map.BuildTrenchDistance();   // from here on a shell may not dig the ground a trench stands in
            map.ComputeBedrock();
            return map;
        }

        public const int PlaytestMapId = 2;

        /// <summary>The M1.5 playtest map: 300 x 480 m, a reserve and a front trench per side, 200 m of no man's land
        /// (about 45 s at a sprint). Trench chain for team 0: 0 -> 1 -> 2 -> 3 -> enemy HQ, and the reverse for team 1.
        /// Objectives fall in order on each side: main line, reserve line, HQ.</summary>
        public static MapData CreatePlaytest(Allocator allocator)
        {
            const float width = 300f, length = 480f;
            var map = new MapData(PlaytestMapId, new float2(width, length), allocator);
            for (int z = 0; z < map.Height.Length; z++)
            {
                float t = z / (float)map.Height.Length;
                float h = 2f * (1f - math.abs(t - 0.5f) * 2f);
                for (int x = 0; x < map.Height.Width; x++) map.Height.Set(x, z, h);
            }
            AddFireTrench(map, 0, (short)0, 60f, 0f, next0: 1, next1: -1);
            AddFireTrench(map, 0, (short)1, 140f, 0f, next0: 2, next1: 0);
            AddFireTrench(map, 1, (short)2, length - 140f, SimMath.Pi, next0: 3, next1: 1);
            AddFireTrench(map, 1, (short)3, length - 60f, SimMath.Pi, next0: -1, next1: 2);

            AddLineObjective(map, 0, ObjectiveKind.MainLine, sideTeam: 0, ownerTeam: 0, z: 140f, orderIndex: 1);
            AddLineObjective(map, 1, ObjectiveKind.ReserveLine, sideTeam: 0, ownerTeam: 0, z: 60f, orderIndex: 2);
            AddLineObjective(map, 2, ObjectiveKind.HQ, sideTeam: 0, ownerTeam: 0, z: 20f, orderIndex: 3);
            AddLineObjective(map, 3, ObjectiveKind.MainLine, sideTeam: 1, ownerTeam: 1, z: length - 140f, orderIndex: 1);
            AddLineObjective(map, 4, ObjectiveKind.ReserveLine, sideTeam: 1, ownerTeam: 1, z: length - 60f, orderIndex: 2);
            AddLineObjective(map, 5, ObjectiveKind.HQ, sideTeam: 1, ownerTeam: 1, z: length - 20f, orderIndex: 3);

            map.Spawns.Add(new SpawnPoint { Team = 0, Pos = new float3(width * 0.5f, 0f, 6f), Kind = 0 });
            map.Spawns.Add(new SpawnPoint { Team = 1, Pos = new float3(width * 0.5f, 0f, length - 6f), Kind = 0 });
            map.SupplyRoad.Add(new float3(width * 0.5f, 0f, 0f));
            map.SupplyRoad.Add(new float3(width * 0.5f, 0f, 40f));
            map.SupplyRoad.Add(new float3(width * 0.5f, 0f, length));
            map.SupplyRoad.Add(new float3(width * 0.5f, 0f, length - 40f));
            map.Wind = new float2(0f, -1f);
            map.RebuildCost();
            map.BuildTrenchDistance();   // from here on a shell may not dig the ground a trench stands in
            map.ComputeBedrock();
            return map;
        }

        /// <summary>A straight fire trench across the full corridor width at world Z, 2 cells deep in Z, with a link every 10 cells.
        /// It reaches both map edges so that nothing can bypass it round the end (tanks must cross, infantry must use links).</summary>
        /// <summary><paramref name="offsets"/> (optional, one per nav column, in cells along Z) bends the trench: fire bays
        /// set forward and back, joined by a traverse wherever two neighbouring columns differ. Null = a straight line.
        /// <paramref name="ladders"/> (optional) marks the link columns; null = every tenth column.</summary>
        internal static void AddFireTrench(MapData map, byte team, short id, float z, float facingYaw, short next0, short next1, int[] offsets = null, bool[] ladders = null)
        {
            int zBase = (int)(z / MapData.NavCellSize);
            var def = new TrenchDef
            {
                Id = id, OwnerTeam = team, Kind = 0, FacingYaw = facingYaw, WidthMeters = 3f,
                CellStart = map.TrenchCells.Length, FireStepStart = map.FireStepCells.Length, LinkStart = map.LinkCells.Length,
                NextTrenchForTeam0 = next0, NextTrenchForTeam1 = next1,
            };
            for (int x = 0; x < map.NavWidth; x++)
            {
                int zc = zBase + (offsets != null ? offsets[x] : 0);
                // the traverse: where this column's bay sits forward or back of the last one, dig the cells between
                int prev = x > 0 ? zBase + (offsets != null ? offsets[x - 1] : 0) : zc;
                int from = math.min(zc, prev), to = math.max(zc, prev) + 1;
                bool link = ladders != null ? ladders[x] : (x % 10) == 5;
                for (int cz = from; cz <= to; cz++)
                {
                    map.SetLayer(x, cz, link ? (NavLayer.Trench | NavLayer.Link) : NavLayer.Trench);
                    int idx = map.NavIndex(x, cz);
                    map.TrenchCells.Add(idx);
                    map.CellTrenchId[idx] = id;
                    if (link) map.LinkCells.Add(idx);
                    // carve the heightfield 1.8 m down across the cell
                    for (int hz = cz * 2; hz < cz * 2 + 2; hz++)
                        for (int hx = x * 2; hx < x * 2 + 2; hx++)
                            map.Height.Set(hx, hz, map.Height.HeightAtCell(hx, hz) - 1.8f);
                }
                // fire-step is the trench cell on the enemy-facing side - which at a traverse is the end of THIS
                // column's carve, not zc. Where the line jogs backward the carve reaches further forward than zc,
                // and pinning the step to zc left carved floor in front of it: a 0.7 m plinth in the middle of the
                // trench, with the floor between it and the parapet, and a man posted on it standing over a hole.
                int fsz = team == 0 ? to : from;
                map.FireStepCells.Add(map.NavIndex(x, fsz));
                // ...and it is a STEP. The carve above took the whole trench down together, which left the firing
                // post level with the trench floor and every man on it aiming into the parapet. Give the row back
                // the height a fire step has: he stands on it and his head and rifle clear the lip, while the man
                // at a reserve post on the floor behind him still does not.
                for (int hz = fsz * 2; hz < fsz * 2 + 2; hz++)
                    for (int hx = x * 2; hx < x * 2 + 2; hx++)
                        if (hx >= 0 && hz >= 0 && hx < map.Height.Width && hz < map.Height.Length)
                            map.Height.Set(hx, hz, map.Height.HeightAtCell(hx, hz) + FireStepRise);
            }
            def.CellCount = map.TrenchCells.Length - def.CellStart;
            def.FireStepCount = map.FireStepCells.Length - def.FireStepStart;
            def.LinkCount = map.LinkCells.Length - def.LinkStart;
            map.Trenches.Add(def);
            // parapet cover: one volume per 20 m facing the enemy
            for (float x = 10f; x < map.SizeMeters.x; x += 20f)
                map.StaticCover.Add(new CoverVolume { Center = new float3(x, 0f, z + 2f), Radius = 10f, ArcCenterYaw = facingYaw, ArcHalfWidth = SimMath.HalfPi, Bonus = 0.85f, Height = 1.5f, OwnerSlot = -1 });
        }

        internal static void AddLineObjective(MapData map, short id, ObjectiveKind kind, byte sideTeam, byte ownerTeam, float z, short orderIndex, int[] offsets = null)
        {
            int zBase = (int)(z / MapData.NavCellSize);
            var def = new ObjectiveDef { Id = id, Kind = kind, OwnerTeam = ownerTeam, SideTeam = sideTeam, CellStart = map.ObjectiveCells.Length, RequiredUnits = 3, CaptureTicks = 200, OrderIndex = orderIndex };
            for (int x = 0; x < map.NavWidth; x++) for (int dz = 0; dz < 2; dz++) map.ObjectiveCells.Add(map.NavIndex(x, zBase + (offsets != null ? offsets[x] : 0) + dz));   // the line follows the trench's bays
            def.CellCount = map.ObjectiveCells.Length - def.CellStart;
            map.Objectives.Add(def);
        }
    }
}
