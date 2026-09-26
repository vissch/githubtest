// Phase: B6 / docs/21 phase 6 (implemented) — the strategic map's continent: a 96 x 64 m height field from four octaves
// of value noise over a basin that falls away to the sea at the edges, faceted (flat-shaded triangles) and cut into
// three height bands, each a submesh with its own Toon colour. Deterministic from its seed, so the same map every
// time and a test can ask where a country node stands. The sea is a plate below zero. Plain code, no scene.
using System.Collections.Generic;
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
            var v = new List<Vector3>(); var n = new List<Vector3>(); var s = new List<Vector3>(); var uv = new List<Vector2>();
            var tris = new[] { new List<int>(), new List<int>(), new List<int>() };
            for (int z = 0; z < l; z++)
                for (int x = 0; x < w; x++)
                {
                    float h00 = h[Index(w, x, z)], h10 = h[Index(w, x + 1, z)], h01 = h[Index(w, x, z + 1)], h11 = h[Index(w, x + 1, z + 1)];
                    var p00 = new Vector3(x * Cell, h00 * Relief, z * Cell); var p10 = new Vector3((x + 1) * Cell, h10 * Relief, z * Cell);
                    var p01 = new Vector3(x * Cell, h01 * Relief, (z + 1) * Cell); var p11 = new Vector3((x + 1) * Cell, h11 * Relief, (z + 1) * Cell);
                    Tri(v, n, s, uv, tris, p00, p01, p11, (h00 + h01 + h11) / 3f);
                    Tri(v, n, s, uv, tris, p00, p11, p10, (h00 + h11 + h10) / 3f);
                }
            var mesh = new Mesh { name = name, hideFlags = HideFlags.HideAndDontSave };
            if (v.Count > 65535) mesh.indexFormat = UnityEngine.Rendering.IndexFormat.UInt32;
            mesh.SetVertices(v); mesh.SetNormals(n); mesh.SetUVs(0, uv); mesh.SetUVs(3, s);
            mesh.subMeshCount = 3;
            for (int b = 0; b < 3; b++) mesh.SetTriangles(tris[b], b);
            mesh.RecalculateBounds();
            return mesh;
        }

        static void Tri(List<Vector3> v, List<Vector3> n, List<Vector3> s, List<Vector2> uv, List<int>[] tris, Vector3 a, Vector3 b, Vector3 c, float rawMean)
        {
            if (a.y < 0f && b.y < 0f && c.y < 0f) return;
            var normal = Vector3.Cross(b - a, c - a).normalized;
            if (normal.y < 0f) normal = -normal;
            int i = v.Count;
            v.Add(a); v.Add(b); v.Add(c);
            n.Add(normal); n.Add(normal); n.Add(normal);
            s.Add(Vector3.up); s.Add(Vector3.up); s.Add(Vector3.up);
            uv.Add(new Vector2(a.x, a.z) * 0.05f); uv.Add(new Vector2(b.x, b.z) * 0.05f); uv.Add(new Vector2(c.x, c.z) * 0.05f);
            var t = tris[Band(rawMean)];
            t.Add(i); t.Add(i + 1); t.Add(i + 2);
        }

        /// <summary>The sea: one plate just below zero, reaching past the land by <paramref name="margin"/>.</summary>
        public static Mesh Sea(int w = Width, int l = Length, float margin = 14f)
        {
            return MetaMeshes.Box(new Vector3(w * Cell + margin * 2f, 0.3f, l * Cell + margin * 2f), new Vector3(w * Cell * 0.5f, -0.17f, l * Cell * 0.5f), "ContinentSea");
        }

        /// <summary>Where a map position (0..1, 0..1) stands in the world: on the land, or at sea level over water.</summary>
        public static Vector3 WorldOf(float[] h, Vector2 mapPos, int w = Width, int l = Length)
        {
            float raw = HeightAt(h, w, l, mapPos.x, mapPos.y);
            return new Vector3(mapPos.x * w * Cell, Mathf.Max(0f, raw) * Relief, mapPos.y * l * Cell);
        }
    }
}
