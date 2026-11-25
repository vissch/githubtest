// Phase: perf pass (2026-11-25) - depends on: NightLights.Pools.cs, TWLightPools.hlsl (TWPoolMask).
// A coarse world-space grid of 32-bit masks, so a Toon, Tank or VAT pixel visits only the pools that can reach its cell.
// Cell (x, z) holds bit k when pool k's CULL circle, widened by a safety margin, touches that cell. The shader reads the
// cell's mask (TWPoolMask), ANDs it with the first `count` bits, and walks only the set bits in ascending order, running
// its own, untouched cull test inside. So the mask can only skip a pool the old loop would have rejected anyway: the
// light is bit-identical (the proof is written in TWLightPools.hlsl). Pixels outside the grid, and a frame with no grid
// set (_TWPoolGridSize.x = 0), take every pool as before.
// The texture is R32_UInt, point filtered, clamped, no mips: an integer format, so a 32-bit mask arrives exactly (a float
// texture holds 24 bits of integer at most), read with Load, never sampled.
using UnityEngine;
using UnityEngine.Experimental.Rendering;

namespace TW.Presentation.Terrain
{
    public sealed partial class NightLights
    {
        /// <summary>The ground loop's widest cull, in pool reaches: TW_STREAK_REACH in TWLightPools.hlsl (the glint's own is
        /// 1.6, the figures' 1.25, both inside it). The mask uses this for every loop, so it holds for all of them.</summary>
        public const float PoolMaskReachScale = 2.6f;
        /// <summary>Metres added to every cull circle: float rounding in the shader's distance and a pixel on a cell edge.</summary>
        public const float PoolMaskMargin = 0.25f;
        /// <summary>Cell edge (m), and the most cells along either side of the grid (128 x 128 x 4 bytes = 64 KB).</summary>
        public const float PoolGridCell = 8f, PoolGridPad = 24f;
        public const int PoolGridMaxSide = 128;

        static readonly int PoolGridId = Shader.PropertyToID("_TWPoolGrid"), PoolGridSizeId = Shader.PropertyToID("_TWPoolGridSize"), PoolGridTexId = Shader.PropertyToID("_TWPoolGridTex");
        Texture2D poolGridTex;
        uint[] poolGridCells;
        int poolGridW, poolGridH, poolGridKeyN = -1;
        float poolGridOx, poolGridOz, poolGridCellM;
        readonly Vector4[] poolGridKey = new Vector4[MaxPools];
        bool poolGridOff;

        /// <summary>Fills cells (gw x gh, row z * gw + x, cell (x, z) covering [ox + x*cell, ox + (x+1)*cell) in x and the
        /// same in z) with the pools whose circle of radius |w| * reachScale + margin around (x, z) touches the cell. A pool
        /// with a non-finite position or reach sets its bit in every cell (the shader's own test on NaN keeps the pool, so
        /// the mask must too). Only the first min(count, 32) pools get a bit.</summary>
        public static void BuildPoolMasks(Vector4[] pools, int count, uint[] cells, int gw, int gh, float ox, float oz, float cell, float reachScale, float margin)
        {
            int total = gw * gh;
            for (int i = 0; i < total; i++) cells[i] = 0u;
            if (count > MaxPools) count = MaxPools;
            for (int k = 0; k < count; k++)
            {
                uint bit = 1u << k;
                Vector4 p = pools[k];
                float r = Mathf.Abs(p.w) * reachScale + margin;
                if (!Finite(p.x) || !Finite(p.z) || !Finite(r) || r > 1e6f)
                {
                    for (int i = 0; i < total; i++) cells[i] |= bit;
                    continue;
                }
                int x0 = Mathf.Max(0, Mathf.FloorToInt((p.x - r - ox) / cell)), x1 = Mathf.Min(gw - 1, Mathf.FloorToInt((p.x + r - ox) / cell));
                int z0 = Mathf.Max(0, Mathf.FloorToInt((p.z - r - oz) / cell)), z1 = Mathf.Min(gh - 1, Mathf.FloorToInt((p.z + r - oz) / cell));
                float r2 = r * r;
                for (int z = z0; z <= z1; z++)
                {
                    float cz = oz + z * cell;
                    float dz = Mathf.Max(Mathf.Max(cz - p.z, p.z - (cz + cell)), 0f);
                    for (int x = x0; x <= x1; x++)
                    {
                        float cx = ox + x * cell;
                        float dx = Mathf.Max(Mathf.Max(cx - p.x, p.x - (cx + cell)), 0f);
                        if (dx * dx + dz * dz <= r2) cells[z * gw + x] |= bit;
                    }
                }
            }
        }

        static bool Finite(float v) => !float.IsNaN(v) && !float.IsInfinity(v);

        /// <summary>Builds and hands the grid for the n pools just set in poolAt, for the map this scene stands on. Called
        /// per camera, straight after the pool arrays; unchanged pools are not rebuilt or re-uploaded. Any trouble (no map,
        /// no R32_UInt) leaves _TWPoolGridSize.x at 0, which is the full loop.</summary>
        void UploadPoolGrid(int n)
        {
            if (poolGridOff) return;
            if (poolGridTex == null)
            {
                var map = Host != null && Host.Local != null ? Host.Local.Map : null;
                if (map == null) { Shader.SetGlobalVector(PoolGridSizeId, Vector4.zero); return; }
                if (!SystemInfo.IsFormatSupported(GraphicsFormat.R32_UInt, FormatUsage.Sample)) { poolGridOff = true; Shader.SetGlobalVector(PoolGridSizeId, Vector4.zero); return; }
                float sx = map.SizeMeters.x + 2f * PoolGridPad, sz = map.SizeMeters.y + 2f * PoolGridPad;
                float cell = Mathf.Max(PoolGridCell, Mathf.Max(sx, sz) / PoolGridMaxSide);
                poolGridW = Mathf.Clamp(Mathf.CeilToInt(sx / cell), 1, PoolGridMaxSide);
                poolGridH = Mathf.Clamp(Mathf.CeilToInt(sz / cell), 1, PoolGridMaxSide);
                poolGridCellM = cell; poolGridOx = -PoolGridPad; poolGridOz = -PoolGridPad;
                poolGridCells = new uint[poolGridW * poolGridH];
                poolGridTex = new Texture2D(poolGridW, poolGridH, GraphicsFormat.R32_UInt, TextureCreationFlags.None)
                { name = "TWPoolGrid", filterMode = FilterMode.Point, wrapMode = TextureWrapMode.Clamp, hideFlags = HideFlags.HideAndDontSave };
                poolGridKeyN = -1;
            }
            bool same = n == poolGridKeyN;
            for (int k = 0; same && k < n; k++)   // exact floats: Vector4 == is approximate
                same = poolAt[k].x == poolGridKey[k].x && poolAt[k].y == poolGridKey[k].y && poolAt[k].z == poolGridKey[k].z && poolAt[k].w == poolGridKey[k].w;
            if (!same)
            {
                BuildPoolMasks(poolAt, n, poolGridCells, poolGridW, poolGridH, poolGridOx, poolGridOz, poolGridCellM, PoolMaskReachScale, PoolMaskMargin);
                poolGridTex.SetPixelData(poolGridCells, 0);
                poolGridTex.Apply(false, false);
                for (int k = 0; k < n; k++) poolGridKey[k] = poolAt[k];
                poolGridKeyN = n;
            }
            Shader.SetGlobalTexture(PoolGridTexId, poolGridTex);
            Shader.SetGlobalVector(PoolGridId, new Vector4(poolGridOx, poolGridOz, 1f / poolGridCellM, 0f));
            Shader.SetGlobalVector(PoolGridSizeId, new Vector4(poolGridW, poolGridH, 0f, 0f));
        }

        /// <summary>No grid once the lights are gone: size 0 is the full loop.</summary>
        void ReleasePoolGrid()
        {
            Shader.SetGlobalVector(PoolGridSizeId, Vector4.zero);
            if (poolGridTex != null) { Destroy(poolGridTex); poolGridTex = null; }
            poolGridCells = null; poolGridKeyN = -1;
        }
    }
}
