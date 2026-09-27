// Phase: B6 / docs/21 phase 6 (implemented) — the few meshes the campaign views need that no kit provides: a plate,
// a ring, a tapered cylinder (chimneys, map pins), a ribbon along points (the front line). Built once, drawn with the
// Toon shader (a smoothed normal in TEXCOORD3 for its outline hull, as BattlefieldProps.Combine writes it). Plain
// code, no scene: the tests can build them.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Meta
{
    public static class MetaMeshes
    {
        public static Material Toon(Color color, float outline = 1.2f)
        {
            var shader = Shader.Find("TW/Toon (URP)");
            var m = new Material(shader != null ? shader : Shader.Find("Universal Render Pipeline/Lit")) { enableInstancing = true, hideFlags = HideFlags.HideAndDontSave };
            m.SetColor("_BaseColor", color);
            if (m.HasProperty("_OutlineWidth")) m.SetFloat("_OutlineWidth", outline);
            return m;
        }

        /// <summary>An unlit, alpha-blended material on URP's own Unlit shader (kept in player builds by
        /// Resources/ShaderKeep/KeepUnlitTransparent, the way CombatFx makes its smoke), or null when that shader is
        /// not there: the caller then draws nothing.</summary>
        public static Material Transparent(Color color)
        {
            var shader = Shader.Find("Universal Render Pipeline/Unlit");
            if (shader == null) return null;
            var m = new Material(shader) { hideFlags = HideFlags.HideAndDontSave, color = color };
            m.SetFloat("_Surface", 1f);
            m.SetFloat("_Blend", 0f);
            m.SetFloat("_ZWrite", 0f);
            m.SetInt("_SrcBlend", (int)UnityEngine.Rendering.BlendMode.SrcAlpha);
            m.SetInt("_DstBlend", (int)UnityEngine.Rendering.BlendMode.OneMinusSrcAlpha);
            m.EnableKeyword("_SURFACE_TYPE_TRANSPARENT");
            m.SetOverrideTag("RenderType", "Transparent");
            m.renderQueue = (int)UnityEngine.Rendering.RenderQueue.Transparent;
            if (m.HasProperty("_BaseColor")) m.SetColor("_BaseColor", color);
            return m;
        }

        sealed class Builder
        {
            public readonly List<Vector3> V = new List<Vector3>(), N = new List<Vector3>(), S = new List<Vector3>();
            public readonly List<Vector2> UV = new List<Vector2>();
            public readonly List<int> T = new List<int>();

            public int Add(Vector3 p, Vector3 n, Vector3 smooth, Vector2 uv) { V.Add(p); N.Add(n); S.Add(smooth); UV.Add(uv); return V.Count - 1; }

            public void Quad(Vector3 a, Vector3 b, Vector3 c, Vector3 d, Vector3 n, Vector3 centre)
            {
                int i = Add(a, n, Smooth(a, centre), new Vector2(0, 0)), j = Add(b, n, Smooth(b, centre), new Vector2(1, 0));
                int k = Add(c, n, Smooth(c, centre), new Vector2(1, 1)), l = Add(d, n, Smooth(d, centre), new Vector2(0, 1));
                T.Add(i); T.Add(j); T.Add(k); T.Add(i); T.Add(k); T.Add(l);
            }

            public Mesh Finish(string name)
            {
                var mesh = new Mesh { name = name, hideFlags = HideFlags.HideAndDontSave };
                if (V.Count > 65535) mesh.indexFormat = UnityEngine.Rendering.IndexFormat.UInt32;
                mesh.SetVertices(V); mesh.SetNormals(N); mesh.SetUVs(0, UV); mesh.SetUVs(3, S); mesh.SetTriangles(T, 0);
                mesh.RecalculateBounds();
                return mesh;
            }
        }

        static Vector3 Smooth(Vector3 p, Vector3 centre) { var d = p - centre; return d.sqrMagnitude > 1e-6f ? d.normalized : Vector3.up; }

        /// <summary>An axis-aligned box of the given size about <paramref name="centre"/>, hard normals, a radial smooth normal.</summary>
        public static Mesh Box(Vector3 size, Vector3 centre, string name = "MetaBox")
        {
            var b = new Builder();
            Vector3 h = size * 0.5f;
            Vector3 p000 = centre + new Vector3(-h.x, -h.y, -h.z), p100 = centre + new Vector3(h.x, -h.y, -h.z), p110 = centre + new Vector3(h.x, h.y, -h.z), p010 = centre + new Vector3(-h.x, h.y, -h.z);
            Vector3 p001 = centre + new Vector3(-h.x, -h.y, h.z), p101 = centre + new Vector3(h.x, -h.y, h.z), p111 = centre + new Vector3(h.x, h.y, h.z), p011 = centre + new Vector3(-h.x, h.y, h.z);
            b.Quad(p010, p110, p111, p011, Vector3.up, centre);        // top
            b.Quad(p001, p101, p100, p000, Vector3.down, centre);      // bottom
            b.Quad(p000, p100, p110, p010, Vector3.back, centre);      // -z
            b.Quad(p101, p001, p011, p111, Vector3.forward, centre);   // +z
            b.Quad(p001, p000, p010, p011, Vector3.left, centre);      // -x
            b.Quad(p100, p101, p111, p110, Vector3.right, centre);     // +x
            return b.Finish(name);
        }

        /// <summary>A cylinder from radius <paramref name="rBottom"/> at y = 0 to <paramref name="rTop"/> at y = height, capped.</summary>
        public static Mesh Cylinder(float rBottom, float rTop, float height, int segments = 12, string name = "MetaCylinder")
        {
            var b = new Builder();
            var centre = new Vector3(0f, height * 0.5f, 0f);
            for (int s = 0; s < segments; s++)
            {
                float a0 = s * Mathf.PI * 2f / segments, a1 = (s + 1) * Mathf.PI * 2f / segments;
                Vector3 d0 = new Vector3(Mathf.Cos(a0), 0f, Mathf.Sin(a0)), d1 = new Vector3(Mathf.Cos(a1), 0f, Mathf.Sin(a1));
                Vector3 p0 = d0 * rBottom, p1 = d1 * rBottom, p2 = d1 * rTop + Vector3.up * height, p3 = d0 * rTop + Vector3.up * height;
                Vector3 n = ((d0 + d1) * 0.5f + Vector3.up * ((rBottom - rTop) / Mathf.Max(0.01f, height))).normalized;
                b.Quad(p0, p1, p2, p3, n, centre);
                if (rTop > 0.001f) { int c = b.Add(Vector3.up * height, Vector3.up, Vector3.up, Vector2.one * 0.5f); int i = b.Add(p3, Vector3.up, Vector3.up, Vector2.zero), j = b.Add(p2, Vector3.up, Vector3.up, Vector2.one); b.T.Add(c); b.T.Add(i); b.T.Add(j); }
                { int c = b.Add(Vector3.zero, Vector3.down, Vector3.down, Vector2.one * 0.5f); int i = b.Add(p1, Vector3.down, Vector3.down, Vector2.zero), j = b.Add(p0, Vector3.down, Vector3.down, Vector2.one); b.T.Add(c); b.T.Add(i); b.T.Add(j); }
            }
            return b.Finish(name);
        }

        /// <summary>A flat ring in the XZ plane at y = 0, facing up.</summary>
        public static Mesh Ring(float rInner, float rOuter, int segments = 32, string name = "MetaRing")
        {
            var b = new Builder();
            for (int s = 0; s < segments; s++)
            {
                float a0 = s * Mathf.PI * 2f / segments, a1 = (s + 1) * Mathf.PI * 2f / segments;
                Vector3 d0 = new Vector3(Mathf.Cos(a0), 0f, Mathf.Sin(a0)), d1 = new Vector3(Mathf.Cos(a1), 0f, Mathf.Sin(a1));
                b.Quad(d0 * rInner, d1 * rInner, d1 * rOuter, d0 * rOuter, Vector3.up, Vector3.zero);
            }
            return b.Finish(name);
        }

        /// <summary>A flat disc at y = 0, facing up.</summary>
        public static Mesh Disc(float r, int segments = 24, string name = "MetaDisc")
        {
            var b = new Builder();
            for (int s = 0; s < segments; s++)
            {
                float a0 = s * Mathf.PI * 2f / segments, a1 = (s + 1) * Mathf.PI * 2f / segments;
                int c = b.Add(Vector3.zero, Vector3.up, Vector3.up, Vector2.one * 0.5f);
                int i = b.Add(new Vector3(Mathf.Cos(a0), 0f, Mathf.Sin(a0)) * r, Vector3.up, Vector3.up, Vector2.zero);
                int j = b.Add(new Vector3(Mathf.Cos(a1), 0f, Mathf.Sin(a1)) * r, Vector3.up, Vector3.up, Vector2.one);
                b.T.Add(c); b.T.Add(j); b.T.Add(i);
            }
            return b.Finish(name);
        }

        /// <summary>A ribbon of the given width along the points, lying flat (facing up), lifted by <paramref name="lift"/>.
        /// Dashed: every second piece of each segment (pieces of <paramref name="dash"/> metres) is left out.</summary>
        public static Mesh Ribbon(IList<Vector3> points, float width, float lift = 0.05f, float dash = 0f, string name = "MetaRibbon")
        {
            var b = new Builder();
            float half = width * 0.5f;
            for (int i = 0; i + 1 < points.Count; i++)
            {
                Vector3 a = points[i], c = points[i + 1];
                Vector3 along = c - a; float len = along.magnitude; if (len < 1e-4f) continue;
                along /= len;
                Vector3 side = Vector3.Cross(Vector3.up, along).normalized * half;
                if (dash <= 0f) { Quad(b, a, c, side, lift); continue; }
                int pieces = Mathf.Max(1, Mathf.CeilToInt(len / dash));
                for (int k = 0; k < pieces; k += 2)
                {
                    float t0 = k * dash / len, t1 = Mathf.Min(1f, (k + 1) * dash / len);
                    Quad(b, Vector3.Lerp(a, c, t0), Vector3.Lerp(a, c, t1), side, lift);
                }
            }
            return b.Finish(name);
        }

        static void Quad(Builder b, Vector3 a, Vector3 c, Vector3 side, float lift)
        {
            var up = Vector3.up * lift;
            b.Quad(a - side + up, a + side + up, c + side + up, c - side + up, Vector3.up, (a + c) * 0.5f);
        }
    }
}
