// Phase: wrecks (2026-09-28, implemented) — a machine's carcass (WreckModel) and which of its chunks are gone at each
// stage (WreckStageRules): the grid bands the hull keel, waist and crown; every triangle is in one chunk and keeps its
// vertex data; no vertex is shared by two chunks (or masking one would drag its neighbour's edge); the far LOD cut on the
// near LOD's grid names the same chunks; the mask fits a float; chunks only ever go, from the top down; scrap is the
// keel alone and cleared is nothing. Built on a lattice mesh made here, so no model and no GPU are needed.
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class WreckModelTests
    {
        /// <summary>Horizontal sheets of shared-vertex quads through a 4 x 4.5 x 7 m hull: every chunk boundary is crossed
        /// by quads whose vertices are shared, which is what the cut has to split.</summary>
        static Mesh Lattice(float height = 4.5f, float length = 7f)
        {
            var v = new List<Vector3>(); var t = new List<int>(); var uv = new List<Vector2>();
            const int n = 8;
            for (int sheet = 0; sheet < 6; sheet++)
            {
                float y = height * (sheet + 0.5f) / 6f;
                int first = v.Count;
                for (int j = 0; j <= n; j++)
                    for (int i = 0; i <= n; i++) { v.Add(new Vector3(-2f + 4f * i / n, y, -length * 0.5f + length * j / n)); uv.Add(new Vector2(i / (float)n, j / (float)n)); }
                for (int j = 0; j < n; j++)
                    for (int i = 0; i < n; i++)
                    {
                        int a = first + j * (n + 1) + i, b = a + 1, c = a + n + 1, d = c + 1;
                        t.Add(a); t.Add(c); t.Add(b); t.Add(b); t.Add(c); t.Add(d);
                    }
            }
            var m = new Mesh();
            m.SetVertices(v); m.SetUVs(0, uv); m.SetTriangles(t, 0); m.RecalculateNormals(); m.RecalculateBounds();
            return m;
        }

        static int ChunkOfVertex(List<Vector4> chunk, int v) => Mathf.RoundToInt(chunk[v].x);

        [Test]
        public void TheGridBandsTheHullKeelWaistAndCrown()
        {
            var b = new Bounds(new Vector3(0f, 2.25f, 0f), new Vector3(4f, 4.5f, 7f));
            Assert.AreEqual(3, WreckModel.BandsFor(4.5f)); Assert.AreEqual(2, WreckModel.BandsFor(2f));
            Assert.AreEqual(3, WreckModel.RowsFor(7f)); Assert.AreEqual(2, WreckModel.RowsFor(4f));
            int keel = WreckModel.ChunkOf(new Vector3(-1f, 0.2f, -3f), b, 3, 3), crown = WreckModel.ChunkOf(new Vector3(-1f, 4.4f, -3f), b, 3, 3);
            Assert.AreEqual(0, keel / 6, "the bottom is the keel band");
            Assert.AreEqual(2, crown / 6, "the top is the crown band");
            Assert.AreNotEqual(WreckModel.ChunkOf(new Vector3(-1f, 2f, 0f), b, 3, 3), WreckModel.ChunkOf(new Vector3(1f, 2f, 0f), b, 3, 3), "left and right are two chunks");
        }

        [Test]
        public void EveryTriangleIsInOneChunkAndKeepsItsVertexData()
        {
            var hull = Lattice();
            var c = WreckModel.Build(hull, 7u);
            Assert.IsNotNull(c);
            Assert.AreEqual(3 * 2 * 3, c.Chunks);
            Assert.LessOrEqual(c.Chunks, WreckModel.MaxChunks);
            var tris = c.Mesh.triangles; var pos = new List<Vector3>(); c.Mesh.GetVertices(pos);
            var chunk = new List<Vector4>(); c.Mesh.GetUVs(4, chunk);
            var uv0 = new List<Vector2>(); c.Mesh.GetUVs(0, uv0);
            Assert.AreEqual(hull.triangles.Length, tris.Length, "no triangle lost or added");
            Assert.AreEqual(pos.Count, uv0.Count, "the atlas coordinates came along");
            for (int k = 0; k < tris.Length; k += 3)
            {
                int id = ChunkOfVertex(chunk, tris[k]);
                Assert.AreEqual(id, ChunkOfVertex(chunk, tris[k + 1])); Assert.AreEqual(id, ChunkOfVertex(chunk, tris[k + 2]));
                Vector3 mid = (pos[tris[k]] + pos[tris[k + 1]] + pos[tris[k + 2]]) / 3f;
                Assert.AreEqual(WreckModel.ChunkOf(mid, hull.bounds, c.Bands, 3) + 1, id, "the chunk its middle falls in");
            }
            // the original triangles, in order, at the same places
            var orig = hull.triangles; var origPos = new List<Vector3>(); hull.GetVertices(origPos);
            for (int k = 0; k < tris.Length; k++) Assert.Less(Vector3.Distance(origPos[orig[k]], pos[tris[k]]), 1e-5f);
        }

        [Test]
        public void NoVertexIsSharedByTwoChunks()
        {
            var c = WreckModel.Build(Lattice(), 7u);
            var tris = c.Mesh.triangles; var chunk = new List<Vector4>(); c.Mesh.GetUVs(4, chunk);
            var owner = new Dictionary<int, int>();
            for (int k = 0; k < tris.Length; k += 3)
            {
                int id = ChunkOfVertex(chunk, tris[k]);
                for (int j = 0; j < 3; j++)
                {
                    if (owner.TryGetValue(tris[k + j], out int was)) Assert.AreEqual(was, id, "a vertex used by two chunks: masking one would drag the other's edge");
                    owner[tris[k + j]] = id;
                }
            }
        }

        [Test]
        public void TheFarCarcassCutOnTheNearGridNamesTheSameChunks()
        {
            var near = Lattice();
            var nearC = WreckModel.Build(near, 7u);
            var far = Lattice(4.9f, 7.6f);   // a far LOD with its tracks fused in: a little bigger all round
            var farC = WreckModel.Build(far, 7u, nearC, near.bounds);
            Assert.AreEqual(nearC.Chunks, farC.Chunks); Assert.AreEqual(nearC.Bands, farC.Bands);
            var p = new Vector3(1f, 4f, 2.5f);
            Assert.AreEqual(WreckModel.ChunkOf(p, near.bounds, nearC.Bands, 3), WreckModel.ChunkOf(p, near.bounds, farC.Bands, 3), "a point of the hull is the same chunk near and far");
        }

        [Test]
        public void TheMaskFitsAFloat()
        {
            var c = WreckModel.Build(Lattice(), 7u);
            Assert.Less(c.All, 1u << 24, "TW/Tank reads the mask from one float's whole numbers");
            Assert.AreEqual(c.All, (uint)(float)c.All);
            Assert.AreNotEqual(0u, c.Keel); Assert.AreEqual(0u, c.Keel & ~c.All);
        }

        [Test]
        public void ChunksOnlyEverGoFromTheTopDown()
        {
            var c = WreckModel.Build(Lattice(), 7u);
            uint was = 0u;
            foreach (int stage in new[] { WreckStageRules.Whole, WreckStageRules.Broken })
                for (float share = 1f; share >= -0.001f; share -= 0.05f)
                {
                    uint now = WreckStageRules.Hidden(c, stage, share);
                    Assert.AreEqual(was, was & now, $"stage {stage} at {share:0.00}: a chunk came back");
                    Assert.AreEqual(0u, now & c.Keel, "the keel stands until the scrap");
                    was = now;
                }
            uint topBand = 0u; foreach (int k in c.ByBand[c.Bands - 1]) topBand |= 1u << (k - 1);
            Assert.AreEqual(0u, WreckStageRules.Hidden(c, WreckStageRules.Whole, 1f), "a fresh wreck is whole");
            Assert.LessOrEqual(CountBits(WreckStageRules.Hidden(c, WreckStageRules.Whole, 0f)), (int)(WreckStageRules.WholeShed * c.ByBand[c.Bands - 1].Length), "at most half its crown while it is whole");
            Assert.AreEqual(topBand, WreckStageRules.Hidden(c, WreckStageRules.Broken, 1f) & topBand, "a broken wreck has lost its crown");
            Assert.AreEqual(c.All & ~c.Keel, WreckStageRules.Hidden(c, WreckStageRules.Scrap, 1f), "scrap is the keel alone");
            Assert.AreEqual(c.All, WreckStageRules.Hidden(c, WreckStageRules.Cleared, 1f), "and cleared is nothing");
        }

        [Test]
        public void EveryWreckKindHasItsStage()
        {
            Assert.AreEqual(WreckStageRules.Whole, WreckStageRules.StageOf(PropKind.Wreck));
            Assert.AreEqual(WreckStageRules.Broken, WreckStageRules.StageOf(PropKind.BrokenWreck));
            Assert.AreEqual(WreckStageRules.Scrap, WreckStageRules.StageOf(PropKind.Scrap));
            Assert.AreEqual(WreckStageRules.Cleared, WreckStageRules.StageOf(PropKind.Cleared));
            Assert.AreEqual(-1, WreckStageRules.StageOf(PropKind.Tree));
            Assert.AreEqual(0b0110u, WreckStageRules.Newly(0b0001u, 0b0111u));
        }

        static int CountBits(uint x) { int n = 0; while (x != 0u) { n += (int)(x & 1u); x >>= 1; } return n; }
    }
}
