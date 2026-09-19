// Phase: A2 (initial implementation) — depends on: Heightfield (P0)
// Amanatides-Woo traversal of the 1 m heightfield. A ray is blocked when its height at a cell drops below the
// cell's terrain height plus a small margin. Smoke attenuation (GasSmokeField) and CoverVolume checks are added
// in A2 by the DirectFire system; this function only answers "does the terrain itself block the line".
using Unity.Burst;
using Unity.Mathematics;
using TW.Sim.Terrain;

namespace TW.Sim.Combat
{
    [BurstCompile(FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
    public static class HeightfieldRaycast
    {
        public const float Margin = 0.05f;

        /// <summary>Eye height above ground for each stance (metres). Index = Stance.</summary>
        public static float EyeHeight(Stance s)
        {
            switch (s)
            {
                case Stance.Prone: case Stance.Pinned: return 0.4f;
                case Stance.Crouch: return 1.0f;
                case Stance.FireStep: return 1.5f;   // head above the parapet
                default: return 1.6f;
            }
        }

        /// <summary>True when unobstructed terrain line exists from <paramref name="from"/> to <paramref name="to"/> (world positions incl. Y).</summary>
        public static bool HasLineOfSight(in Heightfield hf, float3 from, float3 to)
        {
            float2 a = new float2(from.x, from.z) / hf.CellSize;
            float2 b = new float2(to.x, to.z) / hf.CellSize;
            float2 d = b - a;
            float total = SimMath.Length(d);
            if (total < 1e-4f) return true;
            float2 dir = d / total;

            int x = (int)math.floor(a.x), z = (int)math.floor(a.y);
            int ex = (int)math.floor(b.x), ez = (int)math.floor(b.y);
            int stepX = dir.x > 0f ? 1 : -1, stepZ = dir.y > 0f ? 1 : -1;
            float tDeltaX = dir.x != 0f ? math.abs(1f / dir.x) : float.MaxValue;
            float tDeltaZ = dir.y != 0f ? math.abs(1f / dir.y) : float.MaxValue;
            float tMaxX = dir.x != 0f ? ((dir.x > 0f ? (x + 1) - a.x : a.x - x) * tDeltaX) : float.MaxValue;
            float tMaxZ = dir.y != 0f ? ((dir.y > 0f ? (z + 1) - a.y : a.y - z) * tDeltaZ) : float.MaxValue;

            float t = 0f;
            int guard = hf.Width + hf.Length + 2;
            while (guard-- > 0)
            {
                // advance to the next cell boundary
                float tNext;
                if (tMaxX < tMaxZ) { tNext = tMaxX; }
                else { tNext = tMaxZ; }
                if (tNext > total) tNext = total;
                // test the current cell at its far edge along the ray (lowest ray height inside the cell when descending,
                // so also test the near edge): use the min of ray heights at t and tNext.
                float yNear = from.y + (to.y - from.y) * (t / total);
                float yFar = from.y + (to.y - from.y) * (tNext / total);
                float rayY = math.min(yNear, yFar);
                float ground = hf.HeightAtCell(x, z);
                bool isEndpointCell = (x == ex && z == ez) || t == 0f;
                if (!isEndpointCell && rayY < ground + Margin) return false;
                if (x == ex && z == ez) break;
                if (tNext >= total) break;
                t = tNext;
                if (tMaxX < tMaxZ) { x += stepX; tMaxX += tDeltaX; }
                else { z += stepZ; tMaxZ += tDeltaZ; }
                if (x < 0 || z < 0 || x >= hf.Width || z >= hf.Length) break;
            }
            return true;
        }
    }
}
