// Phase: A1 (implemented) — depends on: MapData (P0)
// Layered Dijkstra flow field on the 2 m nav grid. FlowFieldManager owns one per goal; tests build them standalone.
// Infantry mode: a move between neighbouring cells is legal only if they share a traversal layer (Surface or Trench)
// or one of them is a Link cell, so paths enter and leave trenches only through ladders, ramps and vault points.
// Tracked mode (vehicles): links mean nothing; a trench cell is crossed directly at a high cost when the trench is
// narrow enough for the vehicle (FlowFieldManager.TrenchCrossable) and never entered otherwise; wire is crushed.
using System;
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;
using TW.Sim.Terrain;

namespace TW.Sim.Nav
{
    public enum NavMode : byte { Infantry = 0, Tracked = 1 }

    public struct FlowField : IDisposable
    {
        public const int Unreachable = int.MaxValue;
        public const byte NoDirection = 255;
        public const int TrackedTrenchCost = 20;   // per trench cell while a tracked vehicle crosses it
        public const int TrackedWireCost = 2;      // tracks crush wire

        public int Width, Length;
        public NativeArray<int> Integration;   // accumulated cost to reach the goal (units: cost*10 straight, cost*14 diagonal)
        public NativeArray<byte> Direction;    // 0..7 index into Offsets; 255 = goal cell or unreachable
        readonly bool owns;

        // 8-neighbourhood, fixed order (determinism): E, NE, N, NW, W, SW, S, SE  (x right, z up)

        public FlowField(int width, int length, Allocator allocator)
        {
            Width = width; Length = length;
            Integration = new NativeArray<int>(width * length, allocator);
            Direction = new NativeArray<byte>(width * length, allocator);
            owns = true;
        }

        /// <summary>Non-owning view over slices of a larger buffer (FlowFieldManager). Dispose is a no-op.</summary>
        public FlowField(int width, int length, NativeArray<int> integration, NativeArray<byte> direction)
        {
            Width = width; Length = length;
            Integration = integration; Direction = direction;
            owns = false;
        }

        public bool IsCreated => Integration.IsCreated;

        /// <summary>Unit steering direction (XZ, normalised) at a nav cell. Zero at the goal or when unreachable.</summary>
        public float2 DirectionAt(int cell)
        {
            byte d = Direction[cell];
            if (d == NoDirection) return float2.zero;
            return Offset(d);
        }

        public static float2 Offset(int d)
        {
            switch (d)
            {
                case 0: return new float2(1f, 0f);
                case 1: return new float2(0.70710678f, 0.70710678f);
                case 2: return new float2(0f, 1f);
                case 3: return new float2(-0.70710678f, 0.70710678f);
                case 4: return new float2(-1f, 0f);
                case 5: return new float2(-0.70710678f, -0.70710678f);
                case 6: return new float2(0f, -1f);
                default: return new float2(0.70710678f, -0.70710678f);
            }
        }

        static int2 OffsetInt(int d)
        {
            switch (d)
            {
                case 0: return new int2(1, 0); case 1: return new int2(1, 1); case 2: return new int2(0, 1); case 3: return new int2(-1, 1);
                case 4: return new int2(-1, 0); case 5: return new int2(-1, -1); case 6: return new int2(0, -1); default: return new int2(1, -1);
            }
        }

        /// <summary>Infantry traversal rule: trenches are entered and left only through Link cells.</summary>
        public static bool CanStepInfantry(byte from, byte to)
        {
            if ((to & (byte)NavLayer.Blocked) != 0) return false;
            const byte traversal = (byte)(NavLayer.Surface | NavLayer.Trench);
            if ((from & (byte)NavLayer.Link) != 0 || (to & (byte)NavLayer.Link) != 0) return true;
            return (from & to & traversal) != 0;
        }

        /// <summary>Traversal rule per nav mode. <paramref name="toTrench"/> is the trench id of the destination cell (-1 none).</summary>
        public static bool CanStep(NavMode mode, byte from, byte to, short toTrench, NativeArray<byte> trenchCrossable)
        {
            if (mode == NavMode.Infantry) return CanStepInfantry(from, to);
            if ((to & (byte)NavLayer.Blocked) != 0) return false;
            if ((to & (byte)NavLayer.Trench) != 0) return toTrench >= 0 && toTrench < trenchCrossable.Length && trenchCrossable[toTrench] != 0;
            return true;
        }

        public static int StepCost(NavMode mode, byte layer, byte cost)
        {
            if (mode == NavMode.Tracked)
            {
                if ((layer & (byte)NavLayer.Trench) != 0) return TrackedTrenchCost;
                if ((layer & (byte)NavLayer.Wire) != 0) return TrackedWireCost;
            }
            return cost;
        }

        /// <summary>Synchronous infantry-mode rebuild. Goals are nav cell indices.</summary>
        public void Build(MapData map, NativeArray<int> goals) => Build(map, goals, NavMode.Infantry, default);

        /// <summary>Synchronous full rebuild. <paramref name="trenchCrossable"/> has one byte per TrenchDef and is only read in Tracked mode.</summary>
        public void Build(MapData map, NativeArray<int> goals, NavMode mode, NativeArray<byte> trenchCrossable)
        {
            // Callers hand in Temp-allocated goal lists; the job safety system rejects Temp containers on any job, Run() included,
            // so the goals are copied into a TempJob array for the duration of the build.
            using var jobGoals = new NativeArray<int>(goals.Length, Allocator.TempJob, NativeArrayOptions.UninitializedMemory);
            jobGoals.CopyFrom(goals);
            bool ownCrossable = !trenchCrossable.IsCreated;
            var crossable = ownCrossable ? new NativeArray<byte>(1, Allocator.TempJob) : trenchCrossable;
            var job = new BuildJob
            {
                Width = Width, Length = Length, Mode = mode, Layers = map.NavLayers, Cost = map.NavCost, CellTrenchId = map.CellTrenchId,
                TrenchCrossable = crossable, Goals = jobGoals, Integration = Integration, Direction = Direction,
            };
            job.Run();
            if (ownCrossable) crossable.Dispose();
        }

        [BurstCompile(FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct BuildJob : IJob
        {
            public int Width, Length;
            public NavMode Mode;
            [ReadOnly] public NativeArray<byte> Layers;
            [ReadOnly] public NativeArray<byte> Cost;
            [ReadOnly] public NativeArray<short> CellTrenchId;
            [ReadOnly] public NativeArray<byte> TrenchCrossable;
            [ReadOnly] public NativeArray<int> Goals;
            public NativeArray<int> Integration;
            public NativeArray<byte> Direction;

            public void Execute()
            {
                int n = Width * Length;
                for (int i = 0; i < n; i++) { Integration[i] = Unreachable; Direction[i] = NoDirection; }
                var heap = new MinHeap(n, Allocator.Temp);
                for (int g = 0; g < Goals.Length; g++)
                {
                    int c = Goals[g];
                    if (c < 0 || c >= n) continue;
                    Integration[c] = 0;
                    heap.Push(0, c);
                }
                while (heap.Count > 0)
                {
                    heap.Pop(out int cost, out int cell);
                    if (cost != Integration[cell]) continue; // stale entry
                    int x = cell % Width, z = cell / Width;
                    byte from = Layers[cell];
                    for (int d = 0; d < 8; d++)
                    {
                        int2 o = OffsetInt(d);
                        int nx = x + o.x, nz = z + o.y;
                        if (nx < 0 || nz < 0 || nx >= Width || nz >= Length) continue;
                        int nc = nz * Width + nx;
                        byte to = Layers[nc];
                        if (!CanStep(Mode, from, to, CellTrenchId[nc], TrenchCrossable)) continue;
                        // corner cutting through blocked cells is not allowed on diagonals
                        if ((d & 1) == 1)
                        {
                            int ax = z * Width + nx, az = nz * Width + x;
                            if ((Layers[ax] & (byte)NavLayer.Blocked) != 0 || (Layers[az] & (byte)NavLayer.Blocked) != 0) continue;
                        }
                        int step = StepCost(Mode, to, Cost[nc]) * ((d & 1) == 1 ? 14 : 10);
                        int nCost = cost + step;
                        if (nCost < Integration[nc])
                        {
                            Integration[nc] = nCost;
                            heap.Push(nCost, nc);
                        }
                    }
                }
                // direction = neighbour with lowest integration; ties resolved by lowest d (fixed order)
                for (int cell = 0; cell < n; cell++)
                {
                    int best = Integration[cell];
                    if (best == Unreachable || best == 0) continue;
                    int x = cell % Width, z = cell / Width;
                    byte from = Layers[cell];
                    byte bestD = NoDirection;
                    for (int d = 0; d < 8; d++)
                    {
                        int2 o = OffsetInt(d);
                        int nx = x + o.x, nz = z + o.y;
                        if (nx < 0 || nz < 0 || nx >= Width || nz >= Length) continue;
                        int nc = nz * Width + nx;
                        if (!CanStep(Mode, from, Layers[nc], CellTrenchId[nc], TrenchCrossable)) continue;
                        int v = Integration[nc];
                        if (v < best) { best = v; bestD = (byte)d; }
                    }
                    Direction[cell] = bestD;
                }
                heap.Dispose();
            }
        }

        /// <summary>Binary min-heap of (cost, cell). Fixed tie order: lower cell index first.</summary>
        struct MinHeap : IDisposable
        {
            NativeList<long> items;
            public int Count => items.Length;
            public MinHeap(int capacity, Allocator a) { items = new NativeList<long>(capacity, a); }
            static long Key(int cost, int cell) => ((long)cost << 32) | (uint)cell;
            public void Push(int cost, int cell)
            {
                items.Add(Key(cost, cell));
                int i = items.Length - 1;
                while (i > 0)
                {
                    int p = (i - 1) >> 1;
                    if (items[p] <= items[i]) break;
                    long tmp = items[p]; items[p] = items[i]; items[i] = tmp;
                    i = p;
                }
            }
            public void Pop(out int cost, out int cell)
            {
                long top = items[0];
                cost = (int)(top >> 32); cell = (int)(top & 0xFFFFFFFF);
                int last = items.Length - 1;
                items[0] = items[last];
                items.RemoveAt(last);
                int i = 0;
                while (true)
                {
                    int l = 2 * i + 1, r = l + 1, m = i;
                    if (l < items.Length && items[l] < items[m]) m = l;
                    if (r < items.Length && items[r] < items[m]) m = r;
                    if (m == i) break;
                    long tmp = items[m]; items[m] = items[i]; items[i] = tmp;
                    i = m;
                }
            }
            public void Dispose() => items.Dispose();
        }

        public ulong Hash(ulong h) { h = SimHash.Array(Integration, h); return SimHash.Array(Direction, h); }

        public void Dispose()
        {
            if (!owns) return;
            if (Integration.IsCreated) Integration.Dispose();
            if (Direction.IsCreated) Direction.Dispose();
        }
    }
}
