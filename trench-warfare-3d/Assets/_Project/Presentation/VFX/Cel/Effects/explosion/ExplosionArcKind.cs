// Phase: Explosion reference revision 2026-11-14. Directed explosion arcs: one unchanged main strip plus one short procedural daughter branch.
// CelFxRenderer discovers this concrete CelKind automatically. The existing ExpFx keeper retains its shader.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class ExpArcKind : CelKind
    {
        public const string KindName = "ExpArc";
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/ExpFx");
        public override int Capacity => 128;
        public override int Order => 18;
        public override void Configure(Material material) { material.renderQueue = 3018; }

        public override Mesh BuildMesh()
        {
            var vertices = new List<Vector3>(84);
            var uv = new List<Vector2>(84);
            var triangles = new List<int>(240);
            AddStrip(32, 0f, vertices, uv, triangles); // Finer sampling keeps the closing hook's small bend smooth.
            AddStrip(8, 2f, vertices, uv, triangles);  // Dedicated branch flag: uv.y = 2 or 3.
            var mesh = new Mesh { name = "Cel ExpArc Fork", hideFlags = HideFlags.HideAndDontSave };
            mesh.SetVertices(vertices);
            mesh.SetUVs(0, uv);
            mesh.SetTriangles(triangles, 0);
            mesh.bounds = new Bounds(Vector3.zero, Vector3.one * 2f);
            return mesh;
        }

        static void AddStrip(int segments, float uvOffset, List<Vector3> vertices,
            List<Vector2> uv, List<int> triangles)
        {
            int start = vertices.Count;
            for (int i = 0; i <= segments; i++)
            {
                float x = i / (float)segments;
                vertices.Add(new Vector3(x - 0.5f, -0.5f, 0f)); uv.Add(new Vector2(x, uvOffset));
                vertices.Add(new Vector3(x - 0.5f, 0.5f, 0f)); uv.Add(new Vector2(x, uvOffset + 1f));
            }
            for (int i = 0; i < segments; i++)
            {
                int a = start + i * 2;
                triangles.Add(a); triangles.Add(a + 1); triangles.Add(a + 2);
                triangles.Add(a + 2); triangles.Add(a + 1); triangles.Add(a + 3);
            }
        }
    }
}
