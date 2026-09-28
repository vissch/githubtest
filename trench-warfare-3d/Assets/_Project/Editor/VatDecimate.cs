// Phase: C2 (figures; the mid-distance figure, owner decision 2026-09-28) — reduces a baked figure's mesh for the mid
// tier by collapsing vertices ONTO other vertices of the same mesh (quadric error, half-edge collapse), never moving
// one, and returns only a smaller triangle list in the FULL mesh's vertex indices. The mid figure is the full figure's
// vertex buffer and atlas with these triangles: the shader reads the atlas by SV_VertexID, so the mid tier costs no
// memory beyond its index buffer, and the GPU shades only the vertices the triangles still name (critic r1: a separate
// mid atlas duplicated 30 MB the GPU already held). Vertices at one position (UV seams, the rifle box's hard edges) are
// welded: the skin's seam twins are drawn by one of them (only the albedo sample differs), while a locked part keeps
// every one of its own vertices, so the box's per-face normals survive. A vertex only collapses onto one of its own group (the limb), so the shell that takes a limb
// off (VatPad) still cuts at the same seams; locked vertices (the rifle box, the helmet brim) are never removed. The
// error is summed over several poses of the bake (a bent knee is not the rest pose), and a collapse that turns a face
// over in any of them is refused.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Editor
{
    public static class VatDecimate
    {
        public struct Result
        {
            public int[] Triangles;   // the mid triangles, as indices into the FULL mesh's vertices
            public int[] Into;        // for each full vertex, the full vertex that stands in for it (itself when kept)
            public int Used;          // how many distinct vertices the triangles name
        }

        /// <param name="poses">The full figure's vertices in one or more poses (poses[0] the rest pose, all the same length).</param>
        /// <param name="tris">Its triangles.</param>
        /// <param name="target">How many welded positions to keep, locked ones included (it stops at it, or when nothing legal is left).</param>
        /// <param name="locked">Vertices never removed (null: none).</param>
        /// <param name="group">A group per vertex (the limb); a vertex only collapses onto one of its own group (null: one group).</param>
        public static Result Reduce(Vector3[][] poses, int[] tris, int target, bool[] locked = null, int[] group = null)
        {
            var pos = poses[0];
            int n = pos.Length, P = poses.Length;
            // ---- weld by position and group: one representative decides for all of them
            var rep = new int[n];
            var at = new Dictionary<(int, int, int, int), int>();
            for (int i = 0; i < n; i++)
            {
                var key = (Mathf.RoundToInt(pos[i].x * 1e4f), Mathf.RoundToInt(pos[i].y * 1e4f), Mathf.RoundToInt(pos[i].z * 1e4f), group != null ? group[i] : 0);
                if (at.TryGetValue(key, out int r)) rep[i] = r; else { at[key] = i; rep[i] = i; }
            }
            var isLocked = new bool[n];
            if (locked != null) for (int i = 0; i < n; i++) if (locked[i]) isLocked[rep[i]] = true;

            // ---- faces: corners as representatives (what collapses) and as the vertex drawn: a locked corner keeps its own
            // vertex (the rifle box's hard edges keep their per-face normals); a skin corner is drawn by its representative,
            // so a UV seam's twin vertices cost one (the Tripo figures are full of seams: keeping every twin left 613 of 917
            // vertices in use; the twins differ only in the albedo sample, which cannot be told apart beyond 70 m)
            int Corner(int i) => locked != null && locked[i] ? i : rep[i];
            var faces = new List<int[]>(tris.Length / 3); var corners = new List<int[]>(tris.Length / 3);
            for (int t = 0; t + 2 < tris.Length; t += 3)
            {
                int a = rep[tris[t]], b = rep[tris[t + 1]], c = rep[tris[t + 2]];
                if (a == b || b == c || a == c) continue;
                faces.Add(new[] { a, b, c }); corners.Add(new[] { Corner(tris[t]), Corner(tris[t + 1]), Corner(tris[t + 2]) });
            }
            var q = new double[P][][];
            for (int p = 0; p < P; p++) { q[p] = new double[n][]; for (int i = 0; i < n; i++) q[p][i] = new double[10]; }
            var facesOf = new List<int>[n];
            for (int i = 0; i < n; i++) facesOf[i] = new List<int>();
            for (int f = 0; f < faces.Count; f++)
            {
                var fc = faces[f];
                for (int p = 0; p < P; p++)
                {
                    var ps = poses[p];
                    Vector3 nrm = Vector3.Cross(ps[fc[1]] - ps[fc[0]], ps[fc[2]] - ps[fc[0]]);
                    float area = nrm.magnitude;
                    if (area <= 1e-12f) continue;
                    nrm /= area;
                    var k = Plane(nrm.x, nrm.y, nrm.z, -Vector3.Dot(nrm, ps[fc[0]]), area);
                    for (int j = 0; j < 3; j++) Add(q[p][fc[j]], k);
                }
                for (int j = 0; j < 3; j++) facesOf[fc[j]].Add(f);
            }
            // borders (an edge with one face) are held by a steep plane through the edge, so the silhouette holds
            var edgeCount = new Dictionary<long, int>();
            foreach (var fc in faces) for (int j = 0; j < 3; j++) { long e = Edge(fc[j], fc[(j + 1) % 3]); edgeCount[e] = edgeCount.TryGetValue(e, out int c) ? c + 1 : 1; }
            foreach (var fc in faces)
                for (int j = 0; j < 3; j++)
                {
                    int a = fc[j], b = fc[(j + 1) % 3];
                    if (edgeCount[Edge(a, b)] != 1) continue;
                    for (int p = 0; p < P; p++)
                    {
                        var ps = poses[p];
                        Vector3 fn = Vector3.Cross(ps[fc[1]] - ps[fc[0]], ps[fc[2]] - ps[fc[0]]);
                        Vector3 side = Vector3.Cross(ps[b] - ps[a], fn).normalized;
                        if (side.sqrMagnitude < 1e-12f) continue;
                        var k = Plane(side.x, side.y, side.z, -Vector3.Dot(side, ps[a]), 10.0 * (ps[b] - ps[a]).sqrMagnitude);
                        Add(q[p][a], k); Add(q[p][b], k);
                    }
                }

            var alive = new bool[n];
            int live = 0;
            for (int i = 0; i < n; i++) if (rep[i] == i && (facesOf[i].Count > 0 || isLocked[i])) { alive[i] = true; live++; }
            var faceAlive = new bool[faces.Count];
            for (int f = 0; f < faces.Count; f++) faceAlive[f] = true;
            var into = new int[n];
            for (int i = 0; i < n; i++) into[i] = rep[i];

            // ---- greedy: the cheapest legal collapse, until the target
            while (live > target)
            {
                int bu = -1, bv = -1; double best = double.MaxValue;
                for (int f = 0; f < faces.Count; f++)
                {
                    if (!faceAlive[f]) continue;
                    var fc = faces[f];
                    for (int j = 0; j < 3; j++)
                        for (int dir = 0; dir < 2; dir++)
                        {
                            int u = dir == 0 ? fc[j] : fc[(j + 1) % 3], v = dir == 0 ? fc[(j + 1) % 3] : fc[j];
                            if (isLocked[u] || (group != null && group[u] != group[v])) continue;
                            double cost = 0;
                            for (int p = 0; p < P; p++) cost += Error(q[p][u], poses[p][v]) + Error(q[p][v], poses[p][v]);
                            if (cost < best && !Flips(u, v, poses, faces, facesOf, faceAlive)) { best = cost; bu = u; bv = v; }
                        }
                }
                if (bu < 0) break;   // nothing legal left
                // collapse bu onto bv: every corner on bu now names bv's own vertex
                foreach (int f in facesOf[bu])
                {
                    if (!faceAlive[f]) continue;
                    var fc = faces[f];
                    for (int j = 0; j < 3; j++) if (fc[j] == bu) { fc[j] = bv; corners[f][j] = bv; }
                    if (fc[0] == fc[1] || fc[1] == fc[2] || fc[0] == fc[2] || Duplicate(f, faces, facesOf[bv], faceAlive))
                    {
                        faceAlive[f] = false;
                        for (int j = 0; j < 3; j++) if (fc[j] != bv && alive[fc[j]] && !isLocked[fc[j]] && !AnyAlive(facesOf[fc[j]], faceAlive)) { alive[fc[j]] = false; live--; }   // orphaned
                    }
                    else facesOf[bv].Add(f);
                }
                facesOf[bu].Clear();
                for (int p = 0; p < P; p++) Add(q[p][bv], q[p][bu]);
                alive[bu] = false; into[bu] = bv; live--;
                if (!AnyAlive(facesOf[bv], faceAlive) && !isLocked[bv] && alive[bv]) { alive[bv] = false; live--; }
            }

            // ---- the result, in the full mesh's own vertices
            int Final(int v) { while (into[v] != v) v = into[v]; return v; }
            var outTris = new List<int>();
            var used = new HashSet<int>();
            for (int f = 0; f < faces.Count; f++)
                if (faceAlive[f]) foreach (int v in corners[f]) { outTris.Add(v); used.Add(v); }
            var stand = new int[n];
            for (int i = 0; i < n; i++) { int r = Final(rep[i]); stand[i] = r == rep[i] ? i : r; }
            return new Result { Triangles = outTris.ToArray(), Into = stand, Used = used.Count };
        }

        /// <summary>One-pose form (the tests' spheres).</summary>
        public static Result Reduce(Vector3[] pos, int[] tris, int target, bool[] locked = null, int[] group = null) =>
            Reduce(new[] { pos }, tris, target, locked, group);

        static long Edge(int a, int b) => a < b ? ((long)a << 32) | (uint)b : ((long)b << 32) | (uint)a;

        static bool AnyAlive(List<int> fs, bool[] faceAlive) { foreach (int f in fs) if (faceAlive[f]) return true; return false; }

        /// <summary>After a collapse, face f names the same three vertices as another live face of v (a fin): drop it.</summary>
        static bool Duplicate(int f, List<int[]> faces, List<int> ofV, bool[] faceAlive)
        {
            var a = faces[f];
            foreach (int g in ofV)
            {
                if (g == f || !faceAlive[g]) continue;
                var b = faces[g];
                if ((a[0] == b[0] || a[0] == b[1] || a[0] == b[2]) && (a[1] == b[0] || a[1] == b[1] || a[1] == b[2]) && (a[2] == b[0] || a[2] == b[1] || a[2] == b[2])) return true;
            }
            return false;
        }

        /// <summary>Moving u onto v turns one of u's faces over (or crushes it flat) in any pose: not allowed.</summary>
        static bool Flips(int u, int v, Vector3[][] poses, List<int[]> faces, List<int>[] facesOf, bool[] faceAlive)
        {
            foreach (int f in facesOf[u])
            {
                if (!faceAlive[f]) continue;
                var fc = faces[f];
                if (fc[0] == v || fc[1] == v || fc[2] == v) continue;   // this face goes away
                foreach (var pos in poses)
                {
                    Vector3 a = pos[fc[0]], b = pos[fc[1]], c = pos[fc[2]];
                    Vector3 before = Vector3.Cross(b - a, c - a);
                    if (before.sqrMagnitude < 1e-14f) continue;   // flat in this pose already
                    Vector3 a2 = fc[0] == u ? pos[v] : a, b2 = fc[1] == u ? pos[v] : b, c2 = fc[2] == u ? pos[v] : c;
                    Vector3 after = Vector3.Cross(b2 - a2, c2 - a2);
                    if (after.sqrMagnitude < 1e-14f || Vector3.Dot(before.normalized, after.normalized) < 0.2f) return true;
                }
            }
            return false;
        }

        // a symmetric 4x4 quadric in 10 doubles: aa ab ac ad bb bc bd cc cd dd
        static double[] Plane(double a, double b, double c, double d, double w) =>
            new[] { w * a * a, w * a * b, w * a * c, w * a * d, w * b * b, w * b * c, w * b * d, w * c * c, w * c * d, w * d * d };
        static void Add(double[] q, double[] k) { for (int i = 0; i < 10; i++) q[i] += k[i]; }
        static double Error(double[] q, Vector3 p)
        {
            double x = p.x, y = p.y, z = p.z;
            return q[0] * x * x + 2 * q[1] * x * y + 2 * q[2] * x * z + 2 * q[3] * x + q[4] * y * y + 2 * q[5] * y * z + 2 * q[6] * y + q[7] * z * z + 2 * q[8] * z + q[9];
        }
    }
}
