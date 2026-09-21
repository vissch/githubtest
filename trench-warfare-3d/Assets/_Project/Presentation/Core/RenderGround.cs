// Phase: B2 (presentation ground shared by terrain, unit placement and combat effects)
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Terrain;

namespace TW.Presentation
{
    public struct RenderGroundGrid
    {
        [ReadOnly] public NativeArray<float> Heights;
        public int Width, Length;
        public float Step;
        public float Sample(float x, float z, float fallback)
        {
            if (!Heights.IsCreated) return fallback;
            float fx = math.clamp(x / Step, 0f, Width - 1f), fz = math.clamp(z / Step, 0f, Length - 1f);
            int x0 = (int)math.floor(fx), z0 = (int)math.floor(fz), x1 = math.min(x0 + 1, Width - 1), z1 = math.min(z0 + 1, Length - 1);
            float tx = fx - x0, tz = fz - z0;
            float a = Heights[z0 * Width + x0], b = Heights[z0 * Width + x1], c = Heights[z1 * Width + x0], d = Heights[z1 * Width + x1];
            // Match the terrain's triangle diagonal exactly, rather than bilinear interpolation through the quad.
            return tx + tz <= 1f ? a + (b - a) * tx + (c - a) * tz : d + (c - d) * (1f - tx) + (b - d) * (1f - tz);
        }
    }
    public static class RenderGround
    {
        public static MapData Map;
        public static RenderGroundGrid Grid;
        public static float Sample(MapData map, float x, float z)
        {
            float original = map.Height.Sample(x, z);
            return ReferenceEquals(Map, map) ? Grid.Sample(x, z, original) : original;
        }
    }
}
