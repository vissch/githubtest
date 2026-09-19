// Phase: P0 (implemented) — Map data contract, see docs/02-contracts.md
// Everything the simulation knows about a sector. Produced by GreyboxMapGenerator (P0) or the map authoring
// tool (C1). Immutable during a match except the heightfield, nav layers and cost (deformation, A4).
using Unity.Collections;
using Unity.Mathematics;

namespace TW.Sim.Terrain
{
    public sealed class MapData : System.IDisposable
    {
        public int MapId;
        public float2 SizeMeters;           // X width, Y length (Z axis)
        public const float NavCellSize = 2f;
        public const float HeightCellSize = 1f;
        public const float FieldCellSize = 4f;   // gas/smoke grid

        public Heightfield Height;
        public int NavWidth, NavLength;
        public NativeArray<byte> NavLayers;  // NavLayer bits per nav cell
        public NativeArray<byte> NavCost;    // derived from NavLayers; A4 updates locally

        public NativeList<TrenchDef> Trenches;
        public NativeList<int> TrenchCells;
        public NativeList<int> FireStepCells;
        public NativeList<int> LinkCells;
        public NativeList<ObjectiveDef> Objectives;
        public NativeList<int> ObjectiveCells;
        public NativeList<CoverVolume> StaticCover;
        public NativeList<SpawnPoint> Spawns;
        public NativeList<float3> SupplyRoad;      // polyline from map edge to rally area, team 0 then team 1
        public NativeList<TriggerVolume> Triggers;
        public NativeList<EmplacementDef> Emplacements;
        public float2 Wind;                        // metres per second in XZ, for gas/smoke

        public MapData(int mapId, float2 sizeMeters, Allocator allocator)
        {
            MapId = mapId;
            SizeMeters = sizeMeters;
            Height = new Heightfield((int)(sizeMeters.x / HeightCellSize), (int)(sizeMeters.y / HeightCellSize), HeightCellSize, allocator);
            NavWidth = (int)(sizeMeters.x / NavCellSize);
            NavLength = (int)(sizeMeters.y / NavCellSize);
            NavLayers = new NativeArray<byte>(NavWidth * NavLength, allocator);
            NavCost = new NativeArray<byte>(NavWidth * NavLength, allocator);
            Trenches = new NativeList<TrenchDef>(16, allocator);
            TrenchCells = new NativeList<int>(1024, allocator);
            FireStepCells = new NativeList<int>(512, allocator);
            LinkCells = new NativeList<int>(128, allocator);
            Objectives = new NativeList<ObjectiveDef>(8, allocator);
            ObjectiveCells = new NativeList<int>(512, allocator);
            StaticCover = new NativeList<CoverVolume>(64, allocator);
            Spawns = new NativeList<SpawnPoint>(4, allocator);
            SupplyRoad = new NativeList<float3>(8, allocator);
            Triggers = new NativeList<TriggerVolume>(8, allocator);
            Emplacements = new NativeList<EmplacementDef>(16, allocator);
            Wind = new float2(0f, -1f);
            for (int i = 0; i < NavLayers.Length; i++) NavLayers[i] = (byte)NavLayer.Surface;
            RebuildCost();
        }

        public int NavIndex(int x, int z) => z * NavWidth + x;
        public int2 NavCellOf(float3 worldPos) => new int2(
            math.clamp((int)(worldPos.x / NavCellSize), 0, NavWidth - 1),
            math.clamp((int)(worldPos.z / NavCellSize), 0, NavLength - 1));
        public float3 NavCellCenter(int index)
        {
            int x = index % NavWidth, z = index / NavWidth;
            return new float3((x + 0.5f) * NavCellSize, 0f, (z + 0.5f) * NavCellSize);
        }
        public NavLayer LayerAt(float3 worldPos) { var c = NavCellOf(worldPos); return (NavLayer)NavLayers[NavIndex(c.x, c.y)]; }

        public void SetLayer(int x, int z, NavLayer layer)
        {
            int i = NavIndex(x, z);
            NavLayers[i] = (byte)layer;
            NavCost[i] = NavCosts.For(layer);
        }

        public void RebuildCost()
        {
            for (int i = 0; i < NavLayers.Length; i++) NavCost[i] = NavCosts.For((NavLayer)NavLayers[i]);
        }

        public SimConfig.WorldInit ToWorldInit()
        {
            float3 a = default, b = default;
            for (int i = 0; i < Spawns.Length; i++) { if (Spawns[i].Team == 0) a = Spawns[i].Pos; else b = Spawns[i].Pos; }
            return new SimConfig.WorldInit { SizeMeters = SizeMeters, SpawnA = a, SpawnB = b, GoalZA = SizeMeters.y - 4f, GoalZB = 4f };
        }

        /// <summary>Fold the mutable parts (heightfield, layers, cost) into the tick hash. Called by the deformation system (A4).</summary>
        public ulong Hash(ulong h)
        {
            h = SimHash.Array(Height.Cm, h);
            h = SimHash.Array(NavLayers, h);
            h = SimHash.Array(NavCost, h);
            return h;
        }

        public void Dispose()
        {
            Height.Dispose(); NavLayers.Dispose(); NavCost.Dispose();
            Trenches.Dispose(); TrenchCells.Dispose(); FireStepCells.Dispose(); LinkCells.Dispose();
            Objectives.Dispose(); ObjectiveCells.Dispose(); StaticCover.Dispose(); Spawns.Dispose();
            SupplyRoad.Dispose(); Triggers.Dispose(); Emplacements.Dispose();
        }
    }
}
