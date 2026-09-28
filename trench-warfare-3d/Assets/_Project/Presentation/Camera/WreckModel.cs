// Phase: wrecks (2026-09-28, implemented) — a machine's carcass: its root part (the Hull, a walker's Body) cut into
// chunks for the stages its wreck breaks through (owner, 2026-09-28: a wreck breaks further, then disappears). Cut
// at load from the battle model already in Resources/Vehicles, the same mesh the live machine draws, so no machine
// needs a model of its own for it and a new machine has one the day it is added.
//
// The cut is by triangle: every triangle goes to the cell of a grid its middle falls in (bands up the hull, the
// keel, the waist and the crown, then two across and two or three along), and the vertices it uses are copied for
// that chunk, so no vertex is shared by two chunks. The chunk rides in UV4.x (1..24; 0 is every other mesh), which
// TW/Tank reads against the instance's _Chunks mask: a gone chunk collapses. A chunk's open edge shows the hull's
// inside, which is soot-dark on a wreck: a burnt-out shell, not a sawn one. One carcass per LOD, cut the same way,
// so what is gone near is gone far.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public sealed class Carcass
    {
        public Mesh Mesh;
        /// <summary>Chunks 1..Chunks; index k - 1 in the arrays below. A chunk with no triangles has Radius 0.</summary>
        public int Chunks, Bands;
        public Vector3[] Centre;
        public float[] Radius;
        public byte[] Band;
        /// <summary>Bits (chunk k is bit k - 1) of the keel band, the band a scrap pile is left of, and of all chunks.</summary>
        public uint Keel, All;
        /// <summary>The chunks of each band, top band first within the order they come away in (seeded, stable).</summary>
        public int[][] ByBand;
    }

    public static class WreckModel
    {
        /// <summary>The most chunks a carcass may have: TW/Tank reads the mask from one float's whole-number range.</summary>
        public const int MaxChunks = 24;
        /// <summary>A hull at least this tall is cut in three bands (keel, waist, crown), else two; at least this long,
        /// in three rows along it, else two.</summary>
        public const float TallBands = 3.5f, LongRows = 6f;

        public static int BandsFor(float height) => height >= TallBands ? 3 : 2;
        public static int RowsFor(float length) => length >= LongRows ? 3 : 2;

        /// <summary>The chunk (0-based) a point of the hull falls in: band up the hull, then the column across it, then
        /// the row along it. Pure, so WreckModelTests hold the grid without a mesh.</summary>
        public static int ChunkOf(Vector3 p, Bounds b, int bands, int rows)
        {
            Vector3 u = new Vector3(
                b.size.x > 1e-4f ? (p.x - b.min.x) / b.size.x : 0.5f,
                b.size.y > 1e-4f ? (p.y - b.min.y) / b.size.y : 0.5f,
                b.size.z > 1e-4f ? (p.z - b.min.z) / b.size.z : 0.5f);
            int band = Mathf.Clamp((int)(u.y * bands), 0, bands - 1);
            int col = u.x < 0.5f ? 0 : 1;
            int row = Mathf.Clamp((int)(u.z * rows), 0, rows - 1);
            return band * (2 * rows) + col * rows + row;
        }

        /// <summary>The carcass of a root mesh (a copy; the live machine's mesh is left alone). seed orders the chunks'
        /// coming away within a band; the same seed cuts the same carcass. like: a carcass whose grid this one must share
        /// (the far LOD cut on the near LOD's bounds, bands and rows, so chunk k is the same piece of hull at both).</summary>
        public static Carcass Build(Mesh hull, uint seed, Carcass like = null, Bounds likeBounds = default)
        {
            if (hull == null || !hull.isReadable) return null;
            var b = like != null ? likeBounds : hull.bounds;
            int bands = like != null ? like.Bands : BandsFor(b.size.y), rows = like != null ? like.Chunks / (2 * like.Bands) : RowsFor(b.size.z), chunks = bands * 2 * rows;
            var tris = hull.triangles;
            var pos = new List<Vector3>(); hull.GetVertices(pos);
            var nrm = new List<Vector3>(); hull.GetNormals(nrm);
            var col = new List<Color32>(); hull.GetColors(col);
            var tan = new List<Vector4>(); hull.GetTangents(tan);
            var uv = new List<Vector4>[4];
            for (int ch = 0; ch < 4; ch++)
            {
                uv[ch] = new List<Vector4>();
                if (hull.HasVertexAttribute((UnityEngine.Rendering.VertexAttribute)((int)UnityEngine.Rendering.VertexAttribute.TexCoord0 + ch))) hull.GetUVs(ch, uv[ch]);
            }
            var outPos = new List<Vector3>(pos.Count * 2); var outNrm = new List<Vector3>(pos.Count * 2); var outCol = new List<Color32>(pos.Count * 2); var outTan = new List<Vector4>(pos.Count * 2);
            var outUv = new List<Vector4>[4]; for (int ch = 0; ch < 4; ch++) outUv[ch] = new List<Vector4>(uv[ch].Count > 0 ? pos.Count * 2 : 0);
            var outChunk = new List<Vector2>(pos.Count * 2);
            var outTris = new List<int>(tris.Length);
            var remap = new Dictionary<long, int>(pos.Count * 2);
            var sum = new Vector3[chunks]; var count = new int[chunks];
            var lo = new Vector3[chunks]; var hi = new Vector3[chunks];
            for (int k = 0; k < chunks; k++) { lo[k] = Vector3.positiveInfinity; hi[k] = Vector3.negativeInfinity; }
            for (int t = 0; t < tris.Length; t += 3)
            {
                Vector3 mid = (pos[tris[t]] + pos[tris[t + 1]] + pos[tris[t + 2]]) / 3f;
                int c = ChunkOf(mid, b, bands, rows);
                for (int j = 0; j < 3; j++)
                {
                    int v = tris[t + j];
                    long key = (long)v * 64 + c;
                    if (!remap.TryGetValue(key, out int nv))
                    {
                        nv = outPos.Count; remap[key] = nv;
                        outPos.Add(pos[v]);
                        if (nrm.Count > 0) outNrm.Add(nrm[v]);
                        if (col.Count > 0) outCol.Add(col[v]);
                        if (tan.Count > 0) outTan.Add(tan[v]);
                        for (int ch = 0; ch < 4; ch++) if (uv[ch].Count > 0) outUv[ch].Add(uv[ch][v]);
                        outChunk.Add(new Vector2(c + 1, 0f));
                        sum[c] += pos[v]; count[c]++;
                        lo[c] = Vector3.Min(lo[c], pos[v]); hi[c] = Vector3.Max(hi[c], pos[v]);
                    }
                    outTris.Add(nv);
                }
            }
            var mesh = new Mesh { name = hull.name + " carcass", hideFlags = HideFlags.HideAndDontSave, indexFormat = outPos.Count > 65000 ? UnityEngine.Rendering.IndexFormat.UInt32 : UnityEngine.Rendering.IndexFormat.UInt16 };
            mesh.SetVertices(outPos);
            if (outNrm.Count == outPos.Count) mesh.SetNormals(outNrm);
            if (outCol.Count == outPos.Count) mesh.SetColors(outCol);
            if (outTan.Count == outPos.Count) mesh.SetTangents(outTan);
            for (int ch = 0; ch < 4; ch++) if (outUv[ch].Count == outPos.Count) mesh.SetUVs(ch, outUv[ch]);
            mesh.SetUVs(4, outChunk);
            mesh.SetTriangles(outTris, 0);
            mesh.bounds = hull.bounds;

            var carcass = new Carcass { Mesh = mesh, Chunks = chunks, Bands = bands, Centre = new Vector3[chunks], Radius = new float[chunks], Band = new byte[chunks], ByBand = new int[bands][] };
            var rng = new DebrisRng(new Vector3(seed * 0.37f, 1f, seed * 0.11f), seed);
            for (int band = 0; band < bands; band++)
            {
                var list = new List<int>();
                for (int k = 0; k < chunks; k++)
                {
                    if (k / (2 * rows) != band) continue;
                    carcass.Band[k] = (byte)band;
                    if (count[k] == 0) continue;
                    carcass.Centre[k] = sum[k] / count[k];
                    carcass.Radius[k] = (hi[k] - lo[k]).magnitude * 0.5f;
                    carcass.All |= 1u << k;
                    if (band == 0) carcass.Keel |= 1u << k;
                    list.Add(k + 1);
                }
                // seeded shuffle: which chunk of a band comes away first differs from wreck type to wreck type
                for (int i = list.Count - 1; i > 0; i--) { int j = (int)(rng.Next() * (i + 1)) % (i + 1); (list[i], list[j]) = (list[j], list[i]); }
                carcass.ByBand[band] = list.ToArray();
            }
            return carcass;
        }
    }
}
