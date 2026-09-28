// Phase: C2 (2026-09-28, owner Q-B3) — the mid figures: Figure<Name>MidMesh, drawn by VATRenderer between MidDistance and
// LodDistance from the full figure's atlas (Tools/midfigure.py -> VATBaker.WriteMidFigures). What would go wrong
// silently: a mid mesh whose columns no longer match the atlas after a rebake (every man a tangle of wrong vertices); a
// mid vertex playing another vertex's column than the one it was cut from; the rifle losing faces; a mid mesh that is
// not cheaper than the figure; the mid tier chosen at the wrong distance, or chosen with no mid meshes loaded.
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Units;

namespace TW.Tests
{
    public class MidFigureTests
    {
        const int Rifle = 24;   // VATBaker's rifle box: the figure's last 24 vertices

        [Test]
        public void EveryFigureHasAMidMesh_MadeOfItsOwnVertices_ThatFitsItsAtlas()
        {
            foreach (var name in VATRenderer.FigureNames)
            {
                var data = Resources.Load<VatAssetData>("Units/Figure" + name);
                Assert.NotNull(data, $"{name}: no baked figure");
                var full = data.Mesh;
                var mid = Resources.Load<Mesh>("Units/Figure" + name + "MidMesh");
                Assert.NotNull(mid, $"{name}: no Resources/Units/Figure{name}MidMesh (TW/VAT/Write Mid Figures)");
                int tris = mid.triangles.Length / 3;
                Assert.LessOrEqual(tris, 250, $"{name}: the mid figure is 250 triangles at most (owner, 2026-09-28)");
                Assert.Less(mid.vertexCount * 3, full.vertexCount, $"{name}: the mid figure has under a third of the figure's vertices");

                var uv = new List<Vector2>(); mid.GetUVs(1, uv);
                var fullUv = new List<Vector2>(); full.GetUVs(1, fullUv);
                var fullPos = full.vertices; var pos = mid.vertices;
                var columns = new HashSet<int>();
                for (int i = 0; i < mid.vertexCount; i++)
                {
                    float y = uv[i].y;
                    int col = Mathf.RoundToInt(y) - 1;
                    Assert.AreEqual(col + 1f, y, 1e-3f, $"{name} mid vertex {i}: UV1.y carries a whole column + 1");
                    Assert.That(col, Is.InRange(0, full.vertexCount - 1), $"{name} mid vertex {i}: column {col} is outside the figure");
                    // where it stands is where its column stands in the idle frame: a rebake that reordered the figure's
                    // vertices would make every mid vertex play a stranger's path
                    Assert.Less((pos[i] - fullPos[col]).magnitude, 1e-4f, $"{name} mid vertex {i} does not stand on its column {col}: rerun Tools/midfigure.py and TW/VAT/Write Mid Figures");
                    Assert.AreEqual(fullUv[col].x, uv[i].x, $"{name} mid vertex {i}: its limb is its column's");
                    columns.Add(col);
                }
                for (int c = full.vertexCount - Rifle; c < full.vertexCount; c++) Assert.IsTrue(columns.Contains(c), $"{name}: the rifle box lost its vertex {c}");
                var t = mid.triangles;
                for (int k = 0; k < t.Length; k += 3)
                    Assert.Greater(Vector3.Cross(pos[t[k + 1]] - pos[t[k]], pos[t[k + 2]] - pos[t[k]]).sqrMagnitude, 0f, $"{name}: mid triangle {k / 3} has no area");

                // and the renderer takes it: every column inside the decoded atlas
                var atlas = data.ToAsset();
                try { Assert.AreSame(mid, VATRenderer.MidMesh(name, atlas), $"{name}: VATRenderer refuses its mid mesh"); }
                finally { atlas.Release(); }
            }
        }

        /// <summary>The writer builds from the committed source, and refuses one whose columns no longer stand where they
        /// were cut (what a rebake from a changed model does to every column): shifted by one, each lands on a neighbour.</summary>
        [Test]
        public void TheWriterRefusesASourceTheFigureNoLongerMatches()
        {
            string name = VATRenderer.FigureNames[0];
            var full = Resources.Load<VatAssetData>("Units/Figure" + name).Mesh;
            string json = System.IO.File.ReadAllText(System.IO.Path.Combine(Application.dataPath, "..", TW.Editor.VATBaker.MidSourceFolder, $"Figure{name}Mid.json"));
            var built = TW.Editor.VATBaker.BuildMid(json, full, "probe");
            Assert.NotNull(built, "the committed source fits the committed figure");
            Object.DestroyImmediate(built);
            string shifted = System.Text.RegularExpressions.Regex.Replace(json, "\"columns\":\\[([0-9,]+)\\]",
                m => "\"columns\":[" + string.Join(",", System.Array.ConvertAll(m.Groups[1].Value.Split(','), c => (int.Parse(c) + 1) % full.vertexCount)) + "]");
            Assert.AreNotEqual(json, shifted);
            Assert.IsNull(TW.Editor.VATBaker.BuildMid(shifted, full, "probe"), "a source whose columns moved must not be written");
        }

        [Test]
        public void AMidMeshWithAColumnPastTheAtlas_IsRefused()
        {
            var data = Resources.Load<VatAssetData>("Units/Figure" + VATRenderer.FigureNames[0]);
            var atlas = data.ToAsset();
            try
            {
                Assert.NotNull(VATRenderer.MidMesh(VATRenderer.FigureNames[0], atlas), "the real one fits");
                // the same mesh against an atlas a column narrower: its rifle's last vertex no longer fits
                var narrow = new Texture2D(atlas.Positions.width - 1, 1, TextureFormat.RGBA64, false, true);
                var real = atlas.Positions;
                atlas.Positions = narrow;
                try { Assert.IsNull(VATRenderer.MidMesh(VATRenderer.FigureNames[0], atlas), "a column past the atlas must not draw"); }
                finally { atlas.Positions = real; Object.DestroyImmediate(narrow); }
            }
            finally { atlas.Release(); }
        }

        [Test]
        public void GroupOf_FullFigureInside_MidPastMidDistance_OnlyWhenMidsLoaded()
        {
            const float mid = 90f * 90f;
            // two figures (soldier 0, sniper 1), their mids are groups 2 and 3
            Assert.AreEqual(0, VATRenderer.GroupOf(0, 2, 2, 50f * 50f, mid));
            Assert.AreEqual(1, VATRenderer.GroupOf(1, 2, 2, 50f * 50f, mid));
            Assert.AreEqual(2, VATRenderer.GroupOf(0, 2, 2, 120f * 120f, mid));
            Assert.AreEqual(3, VATRenderer.GroupOf(1, 2, 2, 120f * 120f, mid));
            Assert.AreEqual(1, VATRenderer.GroupOf(1, 2, 0, 120f * 120f, mid), "no mid meshes: the full figure all the way to LodDistance");
            var r = new GameObject("vat").AddComponent<VATRenderer>();
            try { Assert.Less(r.MidDistance, r.LodDistance, "the mid band lies inside the near tier"); }
            finally { Object.DestroyImmediate(r.gameObject); }
        }
    }
}
