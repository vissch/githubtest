// Phase: A1 (initial implementation)
// Soft repulsion between infantry within 2r. Each slot reads its own 3x3 hash buckets and writes only its own
// velocity adjustment, so the job is parallel and deterministic.
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;

namespace TW.Sim.Nav
{
    [BurstCompile(FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
    public struct SeparationJob : IJobParallelFor
    {
        public const float Radius = 0.5f;       // infantry collision radius (m)
        public const float Strength = 4f;       // m/s per metre of overlap

        [ReadOnly] public SpatialHash Hash;
        [ReadOnly] public NativeArray<float3> Position;
        [ReadOnly] public NativeArray<uint> Flags;
        public NativeArray<float3> Push;        // output: additive velocity for this tick

        public void Execute(int i)
        {
            Push[i] = float3.zero;
            if ((Flags[i] & (uint)UnitFlags.Alive) == 0) return;
            float3 p = Position[i];
            int cx = math.clamp((int)(p.x / Hash.CellSize), 0, Hash.Width - 1);
            int cz = math.clamp((int)(p.z / Hash.CellSize), 0, Hash.Length - 1);
            float3 sum = float3.zero;
            float diameter = Radius * 2f;
            for (int dz = -1; dz <= 1; dz++)
            for (int dx = -1; dx <= 1; dx++)
            {
                int x = cx + dx, z = cz + dz;
                if (x < 0 || z < 0 || x >= Hash.Width || z >= Hash.Length) continue;
                if (Hash.Map.TryGetFirstValue(Hash.KeyXZ(x, z), out int j, out var it))
                {
                    do
                    {
                        if (j == i) continue;
                        float3 d = p - Position[j];
                        d.y = 0f;
                        float dist = SimMath.Length(d);
                        if (dist < diameter)
                        {
                            float3 n = dist > 1e-4f ? d / dist : new float3(((i & 1) == 0) ? 1f : -1f, 0f, 0f);
                            sum += n * (diameter - dist) * Strength;
                        }
                    } while (Hash.Map.TryGetNextValue(out j, ref it));
                }
            }
            Push[i] = sum;
        }
    }
}
