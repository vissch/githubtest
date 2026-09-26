// Phase: A5 (implemented; docs/21 phase 5) — how much thick cloud lies on a line of sight.
// Samples the 4 m field grid every 2 m along the segment (the midpoint of each step) and adds up the metres that
// fall in a cell above Thick, at most MaxSteps steps (128 m; past that a screen has done its work). It is the one
// answer TargetAcquisition (blind past SmokeBlindMetres) and DirectFire (accuracy per metre) ask of the smoke, and
// GasSmokeSystem.MetresThrough asks it of either field. Pure, Burst-compiled, deterministic: fixed steps, no rays.
using Unity.Burst;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Terrain;

namespace TW.Sim.Combat
{
    [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
    public static class SmokeLos
    {
        /// <summary>A cell above this concentration is thick: it counts against sight and aim.</summary>
        public const float Thick = 10f;
        public const float Step = 2f;
        public const int MaxSteps = 64;

        /// <summary>Metres of the segment a-b (XZ) that lie in thick cells of a field-grid of width x length cells.</summary>
        public static float MetresThrough(NativeArray<float> grid, int width, int length, float3 a, float3 b)
        {
            float3 d = b - a; d.y = 0f;
            float dist = SimMath.Length(d);
            if (dist < 1e-3f) return 0f;
            int steps = math.min(MaxSteps, (int)math.ceil(dist / Step));
            float metres = 0f;
            for (int k = 0; k < steps; k++)
            {
                float s0 = k * Step, s1 = math.min(dist, s0 + Step);
                float3 p = a + d * ((s0 + s1) * 0.5f / dist);
                int cx = math.clamp((int)(p.x / MapData.FieldCellSize), 0, width - 1);
                int cz = math.clamp((int)(p.z / MapData.FieldCellSize), 0, length - 1);
                if (grid[cz * width + cx] > Thick) metres += s1 - s0;
            }
            return metres;
        }
    }
}
