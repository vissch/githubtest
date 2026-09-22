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
        public NativeArray<short> CellTrenchId; // trench id per nav cell, -1 outside trenches (derived from TrenchCells)
        public int Version;                  // bumped on every nav/height mutation; hashed instead of the arrays themselves

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

        /// <summary>The water table in metres. Ground below it is wet: more than WetDepth under is Mud, more than
        /// DeepDepth under is Blocked (the generator only; a shell never makes ground impassable). NoWater = dry map.</summary>
        public float WaterLevel = NoWater;
        public const float NoWater = -1000f, WetDepth = 0.15f, DeepDepth = 1.0f;
        // ---- the sea (A3 landings) -------------------------------------------------------------------------
        /// <summary>Which rear edge the sea lies beyond: 0 = the z = 0 edge, 1 = the z = Length edge, NoSea = inland.
        /// The map runs on past SeaStartZ into sand and shallows; the team whose rear it is lands its reinforcements
        /// there (SeaLandingSystem) instead of them appearing at a spawn point.</summary>
        public byte SeaSide = NoSea;
        public const byte NoSea = 255;
        /// <summary>Where the battle layout ends and the beach begins, in metres along Z.</summary>
        public float SeaStartZ;
        /// <summary>Where the water meets the sand, in metres along Z.</summary>
        public float ShoreZ;
        /// <summary>The surface of the sea in metres (the water table, on a map with a river).</summary>
        public float SeaLevel;
        public bool HasSea => SeaSide != NoSea;
        /// <summary>The team whose rear the sea is.</summary>
        public byte SeaTeam => (byte)(SeaSide == 0 ? 0 : 1);
        /// <summary>+1 when the sea lies beyond increasing Z, -1 when it lies beyond decreasing Z.</summary>
        public float SeaAway => SeaSide == 0 ? -1f : 1f;
        /// <summary>Metres out to sea from the waterline; negative up the beach.</summary>
        public float Offshore(float z) => (z - ShoreZ) * SeaAway;

        public NativeList<PropDef> Props;          // trees, stumps, wrecks, bridges (A4); mutable during a match
        public NativeArray<byte> CellCover;        // derived from Props: cover percent per nav cell

        public MapData(int mapId, float2 sizeMeters, Allocator allocator)
        {
            MapId = mapId;
            SizeMeters = sizeMeters;
            Height = new Heightfield((int)(sizeMeters.x / HeightCellSize), (int)(sizeMeters.y / HeightCellSize), HeightCellSize, allocator);
            NavWidth = (int)(sizeMeters.x / NavCellSize);
            NavLength = (int)(sizeMeters.y / NavCellSize);
            NavLayers = new NativeArray<byte>(NavWidth * NavLength, allocator);
            NavCost = new NativeArray<byte>(NavWidth * NavLength, allocator);
            CellTrenchId = new NativeArray<short>(NavWidth * NavLength, allocator);
            CellCover = new NativeArray<byte>(NavWidth * NavLength, allocator);
            Props = new NativeList<PropDef>(256, allocator);
            for (int i = 0; i < CellTrenchId.Length; i++) CellTrenchId[i] = -1;
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
            Version++;
        }

        /// <summary>Call after mutating the heightfield or nav arrays directly (A4) so the tick hash sees the change.</summary>
        public void Touch() => Version++;

        public void RebuildCost()
        {
            for (int i = 0; i < NavLayers.Length; i++) NavCost[i] = NavCosts.For((NavLayer)NavLayers[i]);
            Version++;
        }

        // ---- water ------------------------------------------------------------------------------------------
        /// <summary>Depth of water over the middle of a nav cell; zero or less is dry.</summary>
        public float WaterDepthAtCell(int x, int z) => WaterLevel - Height.Sample((x + 0.5f) * NavCellSize, (z + 0.5f) * NavCellSize);

        /// <summary>Re-apply the water table to a rectangle of nav cells. Trench, link and bunker cells are left alone
        /// (trenches are drained). Returns how many cells changed layer.</summary>
        public int ApplyWater(int x0, int z0, int x1, int z1, bool allowBlock)
        {
            if (WaterLevel <= NoWater) return 0;
            int changed = 0;
            for (int z = math.max(0, z0); z <= math.min(NavLength - 1, z1); z++)
            for (int x = math.max(0, x0); x <= math.min(NavWidth - 1, x1); x++)
            {
                var layer = (NavLayer)NavLayers[NavIndex(x, z)];
                if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Bunker)) != 0) continue;
                float depth = WaterDepthAtCell(x, z);
                var wanted = layer;
                if (depth > WetDepth) wanted |= NavLayer.Mud;
                if (allowBlock && depth > DeepDepth) wanted |= NavLayer.Blocked;
                if (wanted == layer) continue;
                SetLayer(x, z, wanted);
                changed++;
            }
            return changed;
        }

        // ---- props ------------------------------------------------------------------------------------------
        /// <summary>Place a prop; returns its index, or -1 on a trench, link, bunker or blocked cell. Keeping props off
        /// objectives and crossings is the generator's job.</summary>
        public int AddProp(PropDef prop)
        {
            var c = NavCellOf(prop.Pos);
            int cell = NavIndex(c.x, c.y);
            var layer = (NavLayer)NavLayers[cell];
            if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Bunker | NavLayer.Blocked)) != 0) return -1;
            prop.Cell = cell;
            if (prop.Hp <= 0f) prop.Hp = PropRules.StartHp(prop.Kind);
            Props.Add(prop);
            if (PropRules.Blocks(prop.Kind)) SetLayer(c.x, c.y, layer | NavLayer.Blocked);
            StampCover(cell, PropRules.CoverPercent(prop.Kind));
            Version++;
            return Props.Length - 1;
        }

        /// <summary>Change what a prop is (a tree breaks). Returns true when its nav cell opened or closed.</summary>
        public bool SetPropKind(int index, PropKind kind)
        {
            var prop = Props[index];
            bool was = PropRules.Blocks(prop.Kind), now = PropRules.Blocks(kind);
            prop.Kind = kind; prop.Hp = PropRules.StartHp(kind);
            Props[index] = prop;
            int x = prop.Cell % NavWidth, z = prop.Cell / NavWidth;
            var layer = (NavLayer)NavLayers[prop.Cell];
            if (was != now) SetLayer(x, z, now ? layer | NavLayer.Blocked : layer & ~NavLayer.Blocked);   // props never stand on cells that were blocked before them
            RebuildCover();
            return was != now;
        }

        public void RebuildCover()
        {
            for (int i = 0; i < CellCover.Length; i++) CellCover[i] = 0;
            for (int i = 0; i < Props.Length; i++) StampCover(Props[i].Cell, PropRules.CoverPercent(Props[i].Kind));
            Version++;
        }

        void StampCover(int cell, byte percent)
        {
            if (percent == 0) return;
            int cx = cell % NavWidth, cz = cell / NavWidth;
            for (int z = math.max(0, cz - 1); z <= math.min(NavLength - 1, cz + 1); z++)
            for (int x = math.max(0, cx - 1); x <= math.min(NavWidth - 1, cx + 1); x++)
            {
                int i = NavIndex(x, z);
                if (CellCover[i] < percent) CellCover[i] = percent;
            }
        }

        public SimConfig.WorldInit ToWorldInit()
        {
            float3 a = default, b = default;
            for (int i = 0; i < Spawns.Length; i++) { if (Spawns[i].Team == 0) a = Spawns[i].Pos; else b = Spawns[i].Pos; }
            // team 0 advances to the far end of the LAYOUT, not of the map: past it is the enemy's beach and the sea
            float farEnd = HasSea && SeaSide == 1 ? SeaStartZ : SizeMeters.y;
            float nearEnd = HasSea && SeaSide == 0 ? SeaStartZ : 0f;
            return new SimConfig.WorldInit { SizeMeters = SizeMeters, SpawnA = a, SpawnB = b, GoalZA = farEnd - 4f, GoalZB = nearEnd + 4f };
        }

        /// <summary>Fold the mutable parts (heightfield, layers, cost) into the tick hash. Called by the deformation system (A4).</summary>
        public ulong Hash(ulong h)
        {
            h = SimHash.Array(Height.Cm, h);
            h = SimHash.Array(NavLayers, h);
            h = SimHash.Array(NavCost, h);
            h = SimHash.Array(CellTrenchId, h);
            h = SimHash.Array(CellCover, h);
            h = SimHash.Value(WaterLevel, h);
            h = SimHash.Value(SeaSide, h); h = SimHash.Value(SeaStartZ, h); h = SimHash.Value(ShoreZ, h); h = SimHash.Value(SeaLevel, h);
            for (int i = 0; i < Props.Length; i++)   // field by field: the struct has padding bytes
            {
                var p = Props[i];
                h = SimHash.Value(p.Pos, h); h = SimHash.Value(p.Hp, h); h = SimHash.Value(p.Scale, h); h = SimHash.Value(p.Cell, h); h = SimHash.Value((int)p.Kind, h);
            }
            return h;
        }

        public void Dispose()
        {
            Height.Dispose(); NavLayers.Dispose(); NavCost.Dispose(); CellTrenchId.Dispose(); CellCover.Dispose(); Props.Dispose();
            Trenches.Dispose(); TrenchCells.Dispose(); FireStepCells.Dispose(); LinkCells.Dispose();
            Objectives.Dispose(); ObjectiveCells.Dispose(); StaticCover.Dispose(); Spawns.Dispose();
            SupplyRoad.Dispose(); Triggers.Dispose(); Emplacements.Dispose();
        }
    }
}
