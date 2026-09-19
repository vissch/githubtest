// Phase: A1 (initial implementation)
// Uniform 1 m bucket grid keyed by integer cell. Built single-threaded in slot order so that per-bucket
// iteration order (and therefore every float sum over neighbours) is identical on every peer.
using System;
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;

namespace TW.Sim.Nav
{
    public struct SpatialHash : IDisposable
    {
        public float CellSize;
        public int Width, Length;
        public NativeParallelMultiHashMap<int, int> Map;

        public SpatialHash(float2 sizeMeters, float cellSize, int capacity, Allocator allocator)
        {
            CellSize = cellSize;
            Width = (int)math.ceil(sizeMeters.x / cellSize) + 1;
            Length = (int)math.ceil(sizeMeters.y / cellSize) + 1;
            Map = new NativeParallelMultiHashMap<int, int>(capacity, allocator);
        }

        public bool IsCreated => Map.IsCreated;

        public int Key(float3 p)
        {
            int x = math.clamp((int)(p.x / CellSize), 0, Width - 1);
            int z = math.clamp((int)(p.z / CellSize), 0, Length - 1);
            return z * Width + x;
        }

        public int KeyXZ(int x, int z) => z * Width + x;

        public void Rebuild(NativeArray<float3> positions, NativeArray<uint> flags, int count)
        {
            new BuildJob { Hash = this, Positions = positions, Flags = flags, Count = count }.Run();
        }

        [BurstCompile(FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct BuildJob : IJob
        {
            public SpatialHash Hash;
            [ReadOnly] public NativeArray<float3> Positions;
            [ReadOnly] public NativeArray<uint> Flags;
            public int Count;
            public void Execute()
            {
                Hash.Map.Clear();
                for (int i = 0; i < Count; i++)
                {
                    if ((Flags[i] & (uint)UnitFlags.Alive) == 0) continue;
                    Hash.Map.Add(Hash.Key(Positions[i]), i);
                }
            }
        }

        public void Dispose() { if (Map.IsCreated) Map.Dispose(); }
    }
}
