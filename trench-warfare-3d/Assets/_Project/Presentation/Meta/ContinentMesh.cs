// Phase: B6 / docs/21 phase 6 (implemented) — the strategic map's continent: a 96 x 64 m height field from four octaves
// of value noise over a basin that falls away to the sea at the edges, faceted (flat-shaded triangles) and cut into
// three height bands, each a submesh with its own Toon colour. Deterministic from its seed, so the same map every
// time and a test can ask where a country node stands. The sea is a plate below zero. Plain code, no scene.
using UnityEngine;

namespace TW.Presentation.Meta
{
    public static class ContinentMesh
    {
        public const int Width = 96, Length = 64;
        public const float Cell = 1f, Relief = 1.5f;
        public const uint Seed = 1917;
        /// <summary>Band floors in raw height: lowland from the sea, upland, highland.</summary>
        public const float Upland = 1.2f, Highland = 2.4f;
        public const float BasinHeight = 3f, BasinEdge = 0.2f, NoiseAmplitude = 1.2f;

        public static int Index(int w, int x, int z) => z * (w + 1) + x;

        /// <summary>Raw heights at the (w + 1) x (l + 1) grid corners; above zero is land.</summary>
        public static float[] Heights(uint seed, int w = Width, int l = Length)
        {
            var h = new float[(w + 1) * (l + 1)];
            for (int z = 0; z <= l; z++)
                for (int x = 0; x <= w; x++)
                {
                    float u = x / (float)w, v = z / (float)l;
                    float dx = (u - 0.5f) * 2f, dz = (v - 0.5f) * 2f;
                    float r = Mathf.Min(1.4f, Mathf.Sqrt(dx * dx + dz * dz));
                    float basin = BasinHeight * (1f - Mathf.Pow(r, 1.6f)) - BasinEdge;
                    h[Index(w, x, z)] = basin + (Fbm(x * Cell, z * Cell, seed) - 0.5f) * NoiseAmplitude;
                }
            return h;
        }

        static float Hash(int x, int z, uint seed)
        {
            uint k = unchecked((uint)x * 374761393u + (uint)z * 668265263u + seed * 2246822519u);
            k = (k ^ (k >> 13)) * 1274126177u;
            k ^= k >> 16;
            return (k & 0xFFFFFF) / 16777216f;
        }

        static float Value(float x, float z, float cell, uint seed)
        {
            float fx = x / cell, fz = z / cell;
            int ix = Mathf.FloorToInt(fx), iz = Mathf.FloorToInt(fz);
            float tx = Mathf.SmoothStep(0f, 1f, fx - ix), tz = Mathf.SmoothStep(0f, 1f, fz - iz);
            float a = Mathf.Lerp(Hash(ix, iz, seed), Hash(ix + 1, iz, seed), tx);
            float b = Mathf.Lerp(Hash(ix, iz + 1, seed), Hash(ix + 1, iz + 1, seed), tx);
            return Mathf.Lerp(a, b, tz);
        }

        /// <summary>Four octaves at 22 / 10 / 5 / 2.5 m, weights summing to one: 0..1.</summary>
        public static float Fbm(float x, float z, uint seed) =>
            0.5f * Value(x, z, 22f, seed) + 0.25f * Value(x, z, 10f, seed ^ 0x9E3779B9u) + 0.15f * Value(x, z, 5f, seed ^ 0x7F4A7C15u) + 0.10f * Value(x, z, 2.5f, seed ^ 0x3C6EF372u);

        /// <summary>The raw height at normalised map coordinates (0..1 across, 0..1 along), bilinear.</summary>
        public static float HeightAt(float[] h, int w, int l, float u, float v)
        {
            float fx = Mathf.Clamp01(u) * w, fz = Mathf.Clamp01(v) * l;
            int ix = Mathf.Min(w - 1, Mathf.FloorToInt(fx)), iz = Mathf.Min(l - 1, Mathf.FloorToInt(fz));
            float tx = fx - ix, tz = fz - iz;
            float a = Mathf.Lerp(h[Index(w, ix, iz)], h[Index(w, ix + 1, iz)], tx);
            float b = Mathf.Lerp(h[Index(w, ix, iz + 1)], h[Index(w, ix + 1, iz + 1)], tx);
            return Mathf.Lerp(a, b, tz);
        }

        public static int Band(float rawHeight) => rawHeight >= Highland ? 2 : rawHeight >= Upland ? 1 : 0;

        /// <summary>The land: flat-shaded triangles, three submeshes by band, y = raw height x Relief. Triangles wholly under
        /// the sea are left out (the sea plate covers them).</summary>
        public static Mesh Land(float[] h, int w = Width, int l = Length, string name = "Continent")
        {
            // Two passes over the same grid: the first counts what survives the cull and how it falls into the bands, the
            // second fills exact-size arrays. One List<> without a capacity grew by doubling to 36,864 entries, so the
            // throw-away intermediate arrays cost as much again as the mesh it keeps (about 1.6 MB), once per scene.
            int kept = 0;
            var perBand = new int[3];
            for (int z = 0; z < l; z++)
                for (int x = 0; x < w; x++)
                {
                    float h00 = h[Index(w, x, z)], h10 = h[Index(w, x + 1, z)], h01 = h[Index(w, x, z + 1)], h11 = h[Index(w, x + 1, z + 1)];
                    if (!(h00 < 0f && h01 < 0f && h11 < 0f)) { kept++; perBand[Band((h00 + h01 + h11) / 3f)]++; }
                    if (!(h00 < 0f && h11 < 0f && h10 < 0f)) { kept++; perBand[Band((h00 + h11 + h10) / 3f)]++; }
                }
            var v = new Vector3[kept * 3]; var n = new Vector3[kept * 3]; var s = new Vector3[kept * 3]; var uv = new Vector2[kept * 3];
            var tris = new[] { new int[perBand[0] * 3], new int[perBand[1] * 3], new int[perBand[2] * 3] };
            var cursor = new int[3];
            int vi = 0;
            for (int z = 0; z < l; z++)
                for (int x = 0; x < w; x++)
                {
                    float h00 = h[Index(w, x, z)], h10 = h[Index(w, x + 1, z)], h01 = h[Index(w, x, z + 1)], h11 = h[Index(w, x + 1, z + 1)];
                    var p00 = new Vector3(x * Cell, h00 * Relief, z * Cell); var p10 = new Vector3((x + 1) * Cell, h10 * Relief, z * Cell);
                    var p01 = new Vector3(x * Cell, h01 * Relief, (z + 1) * Cell); var p11 = new Vector3((x + 1) * Cell, h11 * Relief, (z + 1) * Cell);
                    Emit(v, n, s, uv, tris, cursor, ref vi, p00, p01, p11, (h00 + h01 + h11) / 3f);
                    Emit(v, n, s, uv, tris, cursor, ref vi, p00, p11, p10, (h00 + h11 + h10) / 3f);
                }
            var mesh = new Mesh { name = name, hideFlags = HideFlags.HideAndDontSave };
            if (v.Length > 65535) mesh.indexFormat = UnityEngine.Rendering.IndexFormat.UInt32;
            mesh.SetVertices(v); mesh.SetNormals(n); mesh.SetUVs(0, uv); mesh.SetUVs(3, s);
            mesh.subMeshCount = 3;
            for (int b = 0; b < 3; b++) mesh.SetTriangles(tris[b], b);
            mesh.RecalculateBounds();
            return mesh;
        }

        static void Emit(Vector3[] v, Vector3[] n, Vector3[] s, Vector2[] uv, int[][] tris, int[] cursor, ref int i, Vector3 a, Vector3 b, Vector3 c, float rawMean)
        {
            if (a.y < 0f && b.y < 0f && c.y < 0f) return;
            var normal = Vector3.Cross(b - a, c - a).normalized;
            if (normal.y < 0f) normal = -normal;
            v[i] = a; v[i + 1] = b; v[i + 2] = c;
            n[i] = normal; n[i + 1] = normal; n[i + 2] = normal;
            s[i] = Vector3.up; s[i + 1] = Vector3.up; s[i + 2] = Vector3.up;
            uv[i] = new Vector2(a.x, a.z) * 0.05f; uv[i + 1] = new Vector2(b.x, b.z) * 0.05f; uv[i + 2] = new Vector2(c.x, c.z) * 0.05f;
            int band = Band(rawMean);
            var t = tris[band];
            int k = cursor[band];
            t[k] = i; t[k + 1] = i + 1; t[k + 2] = i + 2;
            cursor[band] = k + 3;
            i += 3;
        }

        /// <summary>The sea: one plate just below zero, reaching past the land by <paramref name="margin"/>.</summary>
        public static Mesh Sea(int w = Width, int l = Length, float margin = 14f)
        {
            return MetaMeshes.Box(new Vector3(w * Cell + margin * 2f, 0.3f, l * Cell + margin * 2f), new Vector3(w * Cell * 0.5f, -0.17f, l * Cell * 0.5f), "ContinentSea");
        }

        /// <summary>The fog sheet: a grid draped <paramref name="lift"/> above the land (and at the lift over the sea),
        /// reaching <paramref name="margin"/> past the land on every side, uv 0..1 across that span so a MapFog grid
        /// over the same rectangle lands on it texel for texel.</summary>
        public static Mesh FogSheet(float[] h, float lift, float margin, float step = 2f, int w = Width, int l = Length, string name = "ContinentFog")
        {
            float spanX = w * Cell + 2f * margin, spanZ = l * Cell + 2f * margin;
            int nx = Mathf.Max(1, Mathf.CeilToInt(spanX / step)), nz = Mathf.Max(1, Mathf.CeilToInt(spanZ / step));
            var v = new Vector3[(nx + 1) * (nz + 1)]; var n = new Vector3[v.Length]; var uv = new Vector2[v.Length];
            for (int j = 0; j <= nz; j++)
                for (int i = 0; i <= nx; i++)
                {
                    float x = -margin + spanX * i / nx, z = -margin + spanZ * j / nz;
                    bool overLand = x >= 0f && z >= 0f && x <= w * Cell && z <= l * Cell;
                    float raw = overLand ? HeightAt(h, w, l, x / (w * Cell), z / (l * Cell)) : -1f;
                    int k = j * (nx + 1) + i;
                    v[k] = new Vector3(x, Mathf.Max(0f, raw) * Relief + lift, z);
                    n[k] = Vector3.up;
                    uv[k] = new Vector2((float)i / nx, (float)j / nz);
                }
            var tris = new int[nx * nz * 6];
            int t = 0;
            for (int j = 0; j < nz; j++)
                for (int i = 0; i < nx; i++)
                {
                    int a = j * (nx + 1) + i, b = a + 1, c = a + nx + 1, d = c + 1;
                    tris[t++] = a; tris[t++] = c; tris[t++] = d;
                    tris[t++] = a; tris[t++] = d; tris[t++] = b;
                }
            var mesh = new Mesh { name = name, hideFlags = HideFlags.HideAndDontSave };
            if (v.Length > 65535) mesh.indexFormat = UnityEngine.Rendering.IndexFormat.UInt32;
            mesh.vertices = v; mesh.normals = n; mesh.uv = uv; mesh.triangles = tris;
            mesh.RecalculateBounds();
            return mesh;
        }

        /// <summary>Where a map position (0..1, 0..1) stands in the world: on the land, or at sea level over water.</summary>
        public static Vector3 WorldOf(float[] h, Vector2 mapPos, int w = Width, int l = Length)
        {
            float raw = HeightAt(h, w, l, mapPos.x, mapPos.y);
            return new Vector3(mapPos.x * w * Cell, Mathf.Max(0f, raw) * Relief, mapPos.y * l * Cell);
        }
    }
}
