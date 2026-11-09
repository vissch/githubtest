// Phase: CelFx kit (2026-11-09) - a primitive kind: one mesh, one shader, one pool of records and one indirect draw.
// THE EXTENSION POINT. A new kind (Bolt, Beam, Spike burst, Cone, ...) is one shader under Shaders/Cel/ that includes
// CelFx.hlsl and reads CelRecord, plus one small class here or in your own folder:
//
//     public sealed class BoltKind : CelKind
//     {
//         public override string Name => "Bolt";
//         public override Shader FindShader() => <Shader.Find of "TW/Cel/Bolt", the name as a literal>;
//         public override Mesh BuildMesh() => CelMeshes.Strip(8);
//     }
//
// CelFxRenderer finds every non-abstract CelKind in the TW.* assemblies when it starts (no list to edit), gives the
// built-ins the first ids and the rest ids in name order, and draws each live pool once a frame through FrameBudget.
// Recipes name kinds by Name. Keep the shader in Resources/ShaderKeep (a keeper material) so a player build has it.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public abstract class CelKind
    {
        /// <summary>The name recipes use (CelLayer.Kind). Unique.</summary>
        public abstract string Name { get; }
        /// <summary>The shader, found by its name written out as a literal (ShaderInclusionTests and CelFxTests read the
        /// source for the call, so a name built at runtime would hide the shader from the build check).</summary>
        public abstract Shader FindShader();
        /// <summary>The mesh one record draws (built once, owned by the renderer).</summary>
        public abstract Mesh BuildMesh();
        /// <summary>Records in the pool; past it the oldest is overwritten. Scaled by FxQuality's chunk factor.</summary>
        public virtual int Capacity => 1024;
        /// <summary>Lower draws first among the kinds (after the queue the shader asks for).</summary>
        public virtual int Order => 100;
        /// <summary>Set anything the material needs once, after it is made.</summary>
        public virtual void Configure(Material material) { }
    }

    /// <summary>The built-in kinds, in id order: their ids never move.</summary>
    public static class CelKinds
    {
        public const int Streak = 0, Lobe = 1, Glow = 2, BuiltIn = 3;
        public const string StreakName = "Streak", LobeName = "Lobe", GlowName = "Glow";
        /// <summary>Layer kinds that are not GPU primitives: chunks go to DebrisRenderer (CelFx.Chunks), lights to NightLights (SceneHooks.Flash).</summary>
        public const string ChunksName = "Chunks", LightName = "Light";
        public const int Chunks = -10, Light = -11;
    }

    public sealed class StreakKind : CelKind
    {
        public override string Name => CelKinds.StreakName;
        public override Shader FindShader() => Shader.Find("TW/Cel/Streak");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel Streak");
        public override int Capacity => 2048;
        public override int Order => 20;
    }

    public sealed class LobeKind : CelKind
    {
        public override string Name => CelKinds.LobeName;
        public override Shader FindShader() => Shader.Find("TW/Cel/Lobe");
        public override Mesh BuildMesh() => CelMeshes.Icosphere(3, "Cel Lobe");
        public override int Capacity => 768;
        public override int Order => 0;
    }

    public sealed class GlowKind : CelKind
    {
        public override string Name => CelKinds.GlowName;
        public override Shader FindShader() => Shader.Find("TW/Cel/Glow");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel Glow");
        public override int Capacity => 512;
        public override int Order => 10;
    }

    /// <summary>Meshes the kinds are built from. Everything is generated; nothing is imported.</summary>
    public static class CelMeshes
    {
        /// <summary>A quad with uv 0..1 (positions = uv - 0.5 in XY): the shader places it.</summary>
        public static Mesh Quad01(string name)
        {
            var m = new Mesh { name = name, hideFlags = HideFlags.HideAndDontSave };
            m.SetVertices(new List<Vector3> { new Vector3(-0.5f, -0.5f, 0f), new Vector3(0.5f, -0.5f, 0f), new Vector3(-0.5f, 0.5f, 0f), new Vector3(0.5f, 0.5f, 0f) });
            m.SetUVs(0, new List<Vector2> { new Vector2(0f, 0f), new Vector2(1f, 0f), new Vector2(0f, 1f), new Vector2(1f, 1f) });
            m.SetTriangles(new[] { 0, 2, 1, 1, 2, 3 }, 0);
            m.bounds = new Bounds(Vector3.zero, Vector3.one * 2f);
            return m;
        }

        /// <summary>A strip of `segments` quads along uv.x 0..1, uv.y 0..1 across: beams, bolts, ribbons.</summary>
        public static Mesh Strip(int segments, string name = "Cel Strip")
        {
            segments = Mathf.Max(1, segments);
            var v = new List<Vector3>(); var uv = new List<Vector2>(); var t = new List<int>();
            for (int i = 0; i <= segments; i++)
            {
                float x = i / (float)segments;
                v.Add(new Vector3(x - 0.5f, -0.5f, 0f)); uv.Add(new Vector2(x, 0f));
                v.Add(new Vector3(x - 0.5f, 0.5f, 0f)); uv.Add(new Vector2(x, 1f));
            }
            for (int i = 0; i < segments; i++)
            {
                int a = i * 2;
                t.Add(a); t.Add(a + 1); t.Add(a + 2); t.Add(a + 2); t.Add(a + 1); t.Add(a + 3);
            }
            var m = new Mesh { name = name, hideFlags = HideFlags.HideAndDontSave };
            m.SetVertices(v); m.SetUVs(0, uv); m.SetTriangles(t, 0);
            m.bounds = new Bounds(Vector3.zero, Vector3.one * 2f);
            return m;
        }

        /// <summary>A unit icosphere, `subdivisions` times split (2: 162 vertices, 320 triangles), normals outward.</summary>
        public static Mesh Icosphere(int subdivisions, string name = "Cel Icosphere")
        {
            float p = (1f + Mathf.Sqrt(5f)) * 0.5f;
            var v = new List<Vector3>
            {
                new Vector3(-1, p, 0), new Vector3(1, p, 0), new Vector3(-1, -p, 0), new Vector3(1, -p, 0),
                new Vector3(0, -1, p), new Vector3(0, 1, p), new Vector3(0, -1, -p), new Vector3(0, 1, -p),
                new Vector3(p, 0, -1), new Vector3(p, 0, 1), new Vector3(-p, 0, -1), new Vector3(-p, 0, 1),
            };
            for (int i = 0; i < v.Count; i++) v[i] = v[i].normalized;
            var f = new List<int>
            {
                0, 11, 5, 0, 5, 1, 0, 1, 7, 0, 7, 10, 0, 10, 11, 1, 5, 9, 5, 11, 4, 11, 10, 2, 10, 7, 6, 7, 1, 8,
                3, 9, 4, 3, 4, 2, 3, 2, 6, 3, 6, 8, 3, 8, 9, 4, 9, 5, 2, 4, 11, 6, 2, 10, 8, 6, 7, 9, 8, 1,
            };
            var mid = new Dictionary<long, int>();
            for (int s = 0; s < subdivisions; s++)
            {
                var next = new List<int>(f.Count * 4);
                for (int i = 0; i < f.Count; i += 3)
                {
                    int a = f[i], b = f[i + 1], c = f[i + 2];
                    int ab = Mid(a, b), bc = Mid(b, c), ca = Mid(c, a);
                    next.AddRange(new[] { a, ab, ca, b, bc, ab, c, ca, bc, ab, bc, ca });
                }
                f = next;
            }
            int Mid(int a, int b)
            {
                long key = a < b ? ((long)a << 32) | (uint)b : ((long)b << 32) | (uint)a;
                if (mid.TryGetValue(key, out int i)) return i;
                v.Add(((v[a] + v[b]) * 0.5f).normalized);
                mid[key] = v.Count - 1;
                return v.Count - 1;
            }
            // every face outward the way Unity winds a front face (its normal, cross(b - a, c - a), points out)
            for (int i = 0; i < f.Count; i += 3)
            {
                Vector3 a = v[f[i]], b = v[f[i + 1]], c = v[f[i + 2]];
                if (Vector3.Dot(Vector3.Cross(b - a, c - a), a + b + c) < 0f) { int x = f[i + 1]; f[i + 1] = f[i + 2]; f[i + 2] = x; }
            }
            var m = new Mesh { name = name, hideFlags = HideFlags.HideAndDontSave };
            m.SetVertices(v); m.SetNormals(v); m.SetTriangles(f, 0);
            m.bounds = new Bounds(Vector3.zero, Vector3.one * 4f);
            return m;
        }
    }
}
