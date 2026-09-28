// Phase: C2 (the mid-distance figure, 2026-09-28) — VatDecimate, which reduces a figure's mesh for the mid tier by
// collapsing vertices onto other vertices: it reaches its target with triangles in the full mesh's own vertices (so the
// mid mesh draws with the full figure's atlas), none flat or turned over, the shape holds, no vertex stands in across a
// limb, and a locked hard-edged box keeps every triangle on its own vertices. With a bake present, the baked mid mesh
// is checked against its full figure: the same vertices, fewer triangles, no separate atlas.
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;
using UnityEngine;
using TW.Editor;
using TW.Presentation.Units;

namespace TW.Tests
{
    public class VatDecimateTests
    {
        /// <summary>A UV sphere, rings x segments, with a seam: the first column duplicated at the last (as a UV seam is).</summary>
        static (Vector3[] pos, int[] tris) Sphere(int rings, int segments, float radius = 1f)
        {
            var pos = new List<Vector3>(); var tris = new List<int>();
            for (int r = 0; r <= rings; r++)
                for (int s = 0; s <= segments; s++)
                {
                    float th = Mathf.PI * r / rings, ph = 2f * Mathf.PI * s / segments;
                    pos.Add(radius * new Vector3(Mathf.Sin(th) * Mathf.Cos(ph), Mathf.Cos(th), Mathf.Sin(th) * Mathf.Sin(ph)));
                }
            int W = segments + 1;
            for (int r = 0; r < rings; r++)
                for (int s = 0; s < segments; s++)
                {
                    int a = r * W + s, b = a + 1, c = a + W, d = c + 1;
                    tris.AddRange(new[] { a, b, c, b, d, c });   // wound outward
                }
            return (pos.ToArray(), tris.ToArray());
        }

        static HashSet<int> Used(VatDecimate.Result r) => new HashSet<int>(r.Triangles);

        [Test]
        public void ReachesItsTargetInTheFullMeshVertices()
        {
            var (pos, tris) = Sphere(16, 24);
            var r = VatDecimate.Reduce(pos, tris, 120);
            var used = Used(r);
            Assert.AreEqual(used.Count, r.Used);
            Assert.LessOrEqual(used.Count, 150, "about the target (seam corners that were never collapsed keep their own vertex)");
            Assert.Greater(used.Count, 60);
            Assert.AreEqual(0, r.Triangles.Length % 3);
            Assert.IsTrue(r.Triangles.All(t => t >= 0 && t < pos.Length), "indices into the full mesh: the mid mesh is the full vertex buffer (and atlas) with these triangles");
            for (int t = 0; t < r.Triangles.Length; t += 3)
            {
                Vector3 a = pos[r.Triangles[t]], b = pos[r.Triangles[t + 1]], c = pos[r.Triangles[t + 2]];
                Assert.Greater(Vector3.Cross(b - a, c - a).magnitude, 1e-6f, "no flat triangle at " + t / 3);
                Assert.Greater(Vector3.Dot(Vector3.Cross(b - a, c - a), (a + b + c) / 3f), 0f, "every face still faces out (none turned over), triangle " + t / 3);
            }
        }

        [Test]
        public void TheShapeHolds()
        {
            var (pos, tris) = Sphere(16, 24);
            var r = VatDecimate.Reduce(pos, tris, 120);
            for (int t = 0; t < r.Triangles.Length; t += 3)
            {
                Vector3 centre = (pos[r.Triangles[t]] + pos[r.Triangles[t + 1]] + pos[r.Triangles[t + 2]]) / 3f;
                Assert.Greater(centre.magnitude, 0.75f, "a face sank into the sphere, triangle " + t / 3);
            }
            var kept = Used(r).Select(k => pos[k]).ToArray();
            Assert.Greater(kept.Max(p => p.y), 0.95f); Assert.Less(kept.Min(p => p.y), -0.95f);   // a pole may collapse one ring down
            Assert.Greater(kept.Max(p => p.x), 0.9f); Assert.Less(kept.Min(p => p.x), -0.9f);
        }

        /// <summary>Critic r1: the group check was untested (a test passed with it deleted). Every vertex's stand-in is
        /// of its own group; the control, with one group, does collapse across the line.</summary>
        [Test]
        public void NoVertexStandsInAcrossALimb()
        {
            var (pos, tris) = Sphere(12, 16);
            var group = new int[pos.Length];
            for (int i = 0; i < pos.Length; i++) group[i] = pos[i].y > 0.2f ? 1 : 0;
            var r = VatDecimate.Reduce(pos, tris, 40, null, group);
            for (int i = 0; i < pos.Length; i++) Assert.AreEqual(group[i], group[r.Into[i]], "vertex " + i + " is drawn by " + r.Into[i]);
            Assert.IsTrue(Used(r).Any(k => group[k] == 1) && Used(r).Any(k => group[k] == 0), "both limbs keep vertices");

            var none = VatDecimate.Reduce(pos, tris, 40);
            Assert.IsTrue(Enumerable.Range(0, pos.Length).Any(i => group[i] != group[none.Into[i]]), "the control collapses across it");
        }

        /// <summary>Critic r1: welding merged the rifle box's 24 hard-edged vertices into its 8 corners, so its faces shaded
        /// with each other's normals. A locked box keeps every triangle, on its own vertices.</summary>
        [Test]
        public void ALockedHardEdgedBoxKeepsItsOwnVertices()
        {
            var (sp, st) = Sphere(10, 14);
            var box = new List<Vector3>(); var bt = new List<int>();
            Vector3[] dirs = { Vector3.right, Vector3.left, Vector3.up, Vector3.down, Vector3.forward, Vector3.back };
            foreach (var d in dirs)
            {
                Vector3 u = Vector3.Cross(d, Mathf.Abs(d.y) > 0.5f ? Vector3.forward : Vector3.up), v = Vector3.Cross(d, u);
                int o = box.Count;
                foreach (var (x, y) in new[] { (-1, -1), (1, -1), (1, 1), (-1, 1) }) box.Add(new Vector3(3f, 0f, 0f) + 0.2f * (d + x * u + y * v));
                bt.AddRange(new[] { o, o + 1, o + 2, o, o + 2, o + 3 });
            }
            var pos = sp.Concat(box).ToArray();
            var tris = st.Concat(bt.Select(t => t + sp.Length)).ToArray();
            var locked = pos.Select((p, i) => i >= sp.Length).ToArray();
            var group = pos.Select((p, i) => i >= sp.Length ? 9 : 0).ToArray();
            var r = VatDecimate.Reduce(pos, tris, 50, locked, group);
            var outTris = new HashSet<(int, int, int)>();
            for (int t = 0; t < r.Triangles.Length; t += 3) outTris.Add((r.Triangles[t], r.Triangles[t + 1], r.Triangles[t + 2]));
            for (int t = 0; t < bt.Count; t += 3)
                Assert.IsTrue(outTris.Contains((bt[t] + sp.Length, bt[t + 1] + sp.Length, bt[t + 2] + sp.Length)), "box triangle " + t / 3 + " kept on its own vertices");
            Assert.Less(Used(r).Count(k => k < sp.Length), sp.Length / 2, "while the sphere was reduced");
        }

        [Test]
        public void TheBakedMidMeshIsTheFullFigureWithFewerTriangles()
        {
            for (int k = 0; k < VATRenderer.FigureNames.Length; k++)
            {
                var full = Resources.Load<VatAssetData>("Units/Figure" + VATRenderer.FigureNames[k]);
                var mid = Resources.Load<Mesh>("Units/Figure" + VATRenderer.MidNames[k] + "Mesh");
                if (full == null || mid == null) Assert.Ignore("no mid bake in Resources/Units: run TW/VAT/Bake Infantry");
                Assert.AreEqual(full.Mesh.vertexCount, mid.vertexCount, VATRenderer.MidNames[k] + ": the same vertices, so the atlas columns line up (the renderer refuses one that differs)");
                int fullTris = full.Mesh.triangles.Length / 3, midTris = mid.triangles.Length / 3;
                int used = new HashSet<int>(mid.triangles).Count;
                Assert.Less(midTris, fullTris * 0.75f, VATRenderer.MidNames[k] + ": fewer triangles");
                Assert.Less(used, full.VertexCount * 0.6f, VATRenderer.MidNames[k] + ": the GPU shades under 60 % of the vertices");
                Assert.Greater(used, 264, "and more than the far box");
                Assert.IsNull(Resources.Load<VatAssetData>("Units/Figure" + VATRenderer.MidNames[k]), "no separate mid atlas (critic r1: it duplicated 30 MB)");
            }
        }
    }
}
