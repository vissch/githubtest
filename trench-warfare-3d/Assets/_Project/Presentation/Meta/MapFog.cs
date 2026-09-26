// Phase: B6 / docs/21 phase 6 (implemented) — the fog of the unexplored map: an alpha mask over the continent, dense
// wherever nothing is known and clear in a soft hole round every country node the player can reach (available,
// current or complete), with a wisp of the continent's own noise so the sheet is not flat. Pure arithmetic over a
// texel grid so a test can read it; StrategicMapView bakes it into the fog sheet's texture and rebakes it when a
// node's state changes.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Meta
{
    public static class MapFog
    {
        /// <summary>Alpha of the fog where nothing is known.</summary>
        public const float Density = 0.82f;
        /// <summary>Clear this far round a reachable node (metres), then a soft edge <see cref="HoleSoft"/> wide.</summary>
        public const float HoleRadius = 11f, HoleSoft = 9f;
        /// <summary>The sheet drapes this high over the land; a locked pin's stub stays under it.</summary>
        public const float Sheet = 1.1f;
        public const float TexelsPerMetre = 2f;
        /// <summary>The wisps: this share of the density waves with the continent's noise.</summary>
        public const float NoiseDepth = 0.25f;
        const uint NoiseSalt = 0xF06u;

        /// <summary>The texel grid over a world rectangle: row-major from the -x/-z corner, texel centres sampled.</summary>
        public readonly struct Grid
        {
            public readonly int W, H;
            public readonly float OriginX, OriginZ, SpanX, SpanZ;

            public Grid(float originX, float originZ, float spanX, float spanZ)
            {
                OriginX = originX; OriginZ = originZ; SpanX = spanX; SpanZ = spanZ;
                W = Mathf.Max(1, Mathf.RoundToInt(spanX * TexelsPerMetre));
                H = Mathf.Max(1, Mathf.RoundToInt(spanZ * TexelsPerMetre));
            }

            public int Count => W * H;
            public float X(int i) => OriginX + (i + 0.5f) * SpanX / W;
            public float Z(int j) => OriginZ + (j + 0.5f) * SpanZ / H;

            /// <summary>The texel under a world position, clamped to the grid.</summary>
            public int Index(float x, float z)
            {
                int i = Mathf.Clamp(Mathf.FloorToInt((x - OriginX) / SpanX * W), 0, W - 1);
                int j = Mathf.Clamp(Mathf.FloorToInt((z - OriginZ) / SpanZ * H), 0, H - 1);
                return j * W + i;
            }
        }

        /// <summary>How clear a point <paramref name="d"/> metres from a hole's centre is: 1 inside the radius, 0 past
        /// the soft edge, smooth between.</summary>
        public static float Clear(float d) => 1f - Mathf.SmoothStep(0f, 1f, Mathf.Clamp01((d - HoleRadius) / HoleSoft));

        /// <summary>The fog alpha per texel of the grid for holes at these world XZ positions. Allocates: called when
        /// a state changes, not per frame.</summary>
        public static float[] Alpha(Grid grid, IReadOnlyList<Vector2> holes, uint seed)
        {
            var a = new float[grid.Count];
            for (int j = 0; j < grid.H; j++)
            {
                float z = grid.Z(j);
                for (int i = 0; i < grid.W; i++)
                {
                    float x = grid.X(i), clear = 0f;
                    for (int k = 0; k < holes.Count; k++)
                    {
                        float dx = holes[k].x - x, dz = holes[k].y - z;
                        float c = Clear(Mathf.Sqrt(dx * dx + dz * dz));
                        if (c > clear) clear = c;
                    }
                    float wisp = 1f - NoiseDepth + NoiseDepth * ContinentMesh.Fbm(x, z, seed ^ NoiseSalt);
                    a[j * grid.W + i] = Density * wisp * (1f - clear);
                }
            }
            return a;
        }
    }
}
