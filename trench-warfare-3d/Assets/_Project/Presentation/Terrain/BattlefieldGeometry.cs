// Phase: B2 (reusable worn solid primitive for the procedural field kit)
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Terrain
{
    public static class BattlefieldGeometry
    {
        public static Mesh WornBox(float bevel, float wear, int seed)
        {
            float inner = .5f - Mathf.Clamp(bevel, .005f, .2f);
            var vertices = new List<Vector3>(); var uv = new List<Vector2>(); var triangles = new List<int>();
            Vector3 Axis(int axis, float value) { var p = Vector3.zero; p[axis] = value; return p; }
            void Face(params Vector3[] corners)
            {
                int start = vertices.Count; var center = Vector3.zero;
                foreach (var p in corners) center += p;
                bool reverse = Vector3.Dot(Vector3.Cross(corners[1] - corners[0], corners[2] - corners[0]), center) < 0f;
                for (int i = 0; i < corners.Length; i++)
                {
                    var p = corners[i];
                    // The same position always wears identically across adjacent faces, so no cracks open in the solid.
                    p.y += (Mathf.PerlinNoise((p.x + 1f) * 4f + seed, (p.z + 1f) * 4f) - .5f) * wear * (p.y > 0f ? 1f : .15f);
                    vertices.Add(p); uv.Add(new Vector2(p.x + .5f, p.y + .5f));
                }
                for (int i = 1; i < corners.Length - 1; i++)
                { triangles.Add(start); triangles.Add(start + (reverse ? i + 1 : i)); triangles.Add(start + (reverse ? i : i + 1)); }
            }
            for (int a = 0; a < 3; a++) for (int sign = -1; sign <= 1; sign += 2)
            {
                int b = (a + 1) % 3, c = (a + 2) % 3; var center = Axis(a, sign * .5f);
                Face(center + Axis(b, -inner) + Axis(c, -inner), center + Axis(b, inner) + Axis(c, -inner), center + Axis(b, inner) + Axis(c, inner), center + Axis(b, -inner) + Axis(c, inner));
            }
            for (int a = 0; a < 3; a++) for (int b = a + 1; b < 3; b++)
            for (int sa = -1; sa <= 1; sa += 2) for (int sb = -1; sb <= 1; sb += 2)
            {
                int c = 3 - a - b;
                Face(Axis(a, sa * .5f) + Axis(b, sb * inner) + Axis(c, -inner), Axis(a, sa * .5f) + Axis(b, sb * inner) + Axis(c, inner),
                     Axis(a, sa * inner) + Axis(b, sb * .5f) + Axis(c, inner), Axis(a, sa * inner) + Axis(b, sb * .5f) + Axis(c, -inner));
            }
            for (int x = -1; x <= 1; x += 2) for (int y = -1; y <= 1; y += 2) for (int z = -1; z <= 1; z += 2)
                Face(new Vector3(x * .5f, y * inner, z * inner), new Vector3(x * inner, y * .5f, z * inner), new Vector3(x * inner, y * inner, z * .5f));
            var mesh = new Mesh { name = "Worn bevel box", hideFlags = HideFlags.HideAndDontSave };
            mesh.SetVertices(vertices); mesh.SetUVs(0, uv); mesh.SetTriangles(triangles, 0); mesh.RecalculateNormals(); mesh.RecalculateBounds();
            return mesh;
        }
    }
}
