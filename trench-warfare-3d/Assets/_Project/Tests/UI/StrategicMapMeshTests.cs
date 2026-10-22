// Phase: B6 / docs/21 phase 6 (implemented) — the strategic map's continent: the same seed builds the same heights,
// every country node of the campaign graph stands on land while the corners are sea, the land mesh has its three
// bands and stays under the 16-bit index limit, and a map position lands where the pins expect it. No scene.
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Meta;
using TW.Perf;
using TW.UI;

namespace TW.Tests
{
    public class StrategicMapMeshTests
    {
        [Test]
        public void The_Same_Seed_Builds_The_Same_Continent()
        {
            var a = ContinentMesh.Heights(ContinentMesh.Seed);
            var b = ContinentMesh.Heights(ContinentMesh.Seed);
            Assert.That(a.Length, Is.EqualTo((ContinentMesh.Width + 1) * (ContinentMesh.Length + 1)));
            Assert.That(a, Is.EqualTo(b));
            var c = ContinentMesh.Heights(ContinentMesh.Seed + 1);
            Assert.That(c, Is.Not.EqualTo(a), "another seed is another continent");
        }

        [Test]
        public void Every_Country_Node_Stands_On_Land_And_The_Corners_Are_Sea()
        {
            var h = ContinentMesh.Heights(ContinentMesh.Seed);
            foreach (var n in CampaignGraph.Nodes)
            {
                float raw = ContinentMesh.HeightAt(h, ContinentMesh.Width, ContinentMesh.Length, n.MapPos.x, n.MapPos.y);
                Assert.That(raw, Is.GreaterThan(0.2f), n.Id + " is in the sea at " + n.MapPos);
            }
            foreach (var corner in new[] { new Vector2(0f, 0f), new Vector2(1f, 0f), new Vector2(0f, 1f), new Vector2(1f, 1f) })
                Assert.That(ContinentMesh.HeightAt(h, ContinentMesh.Width, ContinentMesh.Length, corner.x, corner.y), Is.LessThan(0f), "corner " + corner);
            Assert.That(ContinentMesh.HeightAt(h, ContinentMesh.Width, ContinentMesh.Length, 0.5f, 0.5f), Is.GreaterThan(ContinentMesh.Highland), "the middle is high ground");
        }

        [Test]
        public void The_Land_Mesh_Has_Three_Bands()
        {
            var h = ContinentMesh.Heights(ContinentMesh.Seed);
            var mesh = ContinentMesh.Land(h);
            try
            {
                Assert.That(mesh.subMeshCount, Is.EqualTo(3));
                for (int b = 0; b < 3; b++) Assert.That(mesh.GetTriangles(b).Length, Is.GreaterThan(0), "band " + b + " has no triangles");
                Assert.That(mesh.vertexCount, Is.LessThanOrEqualTo(65535));
                Assert.That(mesh.bounds.max.y, Is.GreaterThan(ContinentMesh.Highland * ContinentMesh.Relief));
            }
            finally { Object.DestroyImmediate(mesh); }
            Assert.That(ContinentMesh.Band(0.5f), Is.Zero);
            Assert.That(ContinentMesh.Band(ContinentMesh.Upland), Is.EqualTo(1));
            Assert.That(ContinentMesh.Band(ContinentMesh.Highland + 0.1f), Is.EqualTo(2));
        }

        [Test]
        public void A_Map_Position_Lands_Where_The_Pins_Expect_It()
        {
            var h = ContinentMesh.Heights(ContinentMesh.Seed);
            var p = ContinentMesh.WorldOf(h, new Vector2(0.5f, 0.5f));
            Assert.That(p.x, Is.EqualTo(ContinentMesh.Width * 0.5f).Within(1e-3f));
            Assert.That(p.z, Is.EqualTo(ContinentMesh.Length * 0.5f).Within(1e-3f));
            Assert.That(p.y, Is.GreaterThan(0f));
            var sea = ContinentMesh.WorldOf(h, new Vector2(0f, 0f));
            Assert.That(sea.y, Is.Zero, "over the sea a pin stands at sea level");
        }

        [Test]
        public void The_Fog_Sheet_Drapes_Over_The_Land_And_Reaches_Over_The_Sea()
        {
            var h = ContinentMesh.Heights(ContinentMesh.Seed);
            var mesh = ContinentMesh.FogSheet(h, MapFog.Sheet, StrategicMapView.FogMargin);
            try
            {
                var b = mesh.bounds;
                Assert.That(b.min.x, Is.EqualTo(-StrategicMapView.FogMargin).Within(1e-3f));
                Assert.That(b.max.x, Is.EqualTo(ContinentMesh.Width * ContinentMesh.Cell + StrategicMapView.FogMargin).Within(1e-3f));
                Assert.That(b.min.z, Is.EqualTo(-StrategicMapView.FogMargin).Within(1e-3f));
                Assert.That(b.min.y, Is.EqualTo(MapFog.Sheet).Within(1e-3f), "over the sea the sheet lies at its lift");
                Assert.That(b.max.y, Is.GreaterThan(ContinentMesh.Highland * ContinentMesh.Relief + MapFog.Sheet), "over the high ground it rises with the land");
                Assert.That(mesh.vertexCount, Is.LessThanOrEqualTo(65535));
                float minU = 1f, maxU = 0f, minV = 1f, maxV = 0f;
                foreach (var t in mesh.uv) { minU = Mathf.Min(minU, t.x); maxU = Mathf.Max(maxU, t.x); minV = Mathf.Min(minV, t.y); maxV = Mathf.Max(maxV, t.y); }
                Assert.That(minU, Is.Zero); Assert.That(maxU, Is.EqualTo(1f)); Assert.That(minV, Is.Zero); Assert.That(maxV, Is.EqualTo(1f));
            }
            finally { Object.DestroyImmediate(mesh); }
        }

        [Test]
        public void The_Fog_Opens_Over_The_Reachable_Nodes_And_Stays_Over_The_Rest()
        {
            var h = ContinentMesh.Heights(ContinentMesh.Seed);
            var grid = StrategicMapView.FogGrid;
            var lowlands = ContinentMesh.WorldOf(h, CampaignGraph.Find("lowlands").MapPos);
            var citadel = ContinentMesh.WorldOf(h, CampaignGraph.Find("the-citadel").MapPos);
            Assert.That(Vector2.Distance(new Vector2(lowlands.x, lowlands.z), new Vector2(citadel.x, citadel.z)), Is.GreaterThan(MapFog.HoleRadius + MapFog.HoleSoft), "the finale is out of the first hole's reach");

            var a = MapFog.Alpha(grid, new List<Vector2> { new Vector2(lowlands.x, lowlands.z) }, ContinentMesh.Seed);
            Assert.That(a.Length, Is.EqualTo(grid.Count));
            Assert.That(a[grid.Index(lowlands.x, lowlands.z)], Is.LessThan(0.01f), "clear over the reachable node");
            Assert.That(a[grid.Index(citadel.x, citadel.z)], Is.GreaterThan(MapFog.Density * (1f - MapFog.NoiseDepth) - 1e-4f), "fogged over the locked one");
            float max = 0f; foreach (var v in a) max = Mathf.Max(max, v);
            Assert.That(max, Is.LessThanOrEqualTo(MapFog.Density + 1e-4f), "never denser than the density");
            float edge = a[grid.Index(lowlands.x + MapFog.HoleRadius + MapFog.HoleSoft * 0.5f, lowlands.z)];
            Assert.That(edge, Is.GreaterThan(0.05f).And.LessThan(MapFog.Density), "the hole has a soft edge");

            var none = MapFog.Alpha(grid, new List<Vector2>(), ContinentMesh.Seed);
            float min = 1f; foreach (var v in none) min = Mathf.Min(min, v);
            Assert.That(min, Is.GreaterThanOrEqualTo(MapFog.Density * (1f - MapFog.NoiseDepth) - 1e-4f), "with nothing reachable the whole map is fogged");
            Assert.That(MapFog.Clear(0f), Is.EqualTo(1f)); Assert.That(MapFog.Clear(MapFog.HoleRadius + MapFog.HoleSoft), Is.Zero);
        }

        [Test]
        public void The_Fog_Is_Thinner_Over_The_Sea_And_Along_The_Front()
        {
            var h = ContinentMesh.Heights(ContinentMesh.Seed);
            var grid = StrategicMapView.FogGrid;
            var none = new List<Vector2>();
            var plain = MapFog.Alpha(grid, none, ContinentMesh.Seed);
            var charted = MapFog.Alpha(grid, none, ContinentMesh.Seed, h);
            int sea = grid.Index(-10f, -10f), land = grid.Index(ContinentMesh.Width * 0.5f, ContinentMesh.Length * 0.5f);
            Assert.That(ContinentMesh.HeightAt(h, ContinentMesh.Width, ContinentMesh.Length, 0f, 0f), Is.LessThan(0f), "the corner is sea");
            Assert.AreEqual(plain[sea] * MapFog.SeaShare, charted[sea], 1e-4f, "over the sea the fog is the charted share");
            Assert.AreEqual(plain[land], charted[land], 1e-4f, "over the high ground it is what it was");
            var front = new List<Vector2>();
            foreach (var id in CampaignGraph.FrontLine) { var n = CampaignGraph.Find(id); var w = ContinentMesh.WorldOf(h, n.MapPos); front.Add(new Vector2(w.x, w.z)); }
            Assert.GreaterOrEqual(front.Count, 2);
            var known = MapFog.Alpha(grid, none, ContinentMesh.Seed, h, front);
            var mid = (front[0] + front[1]) * 0.5f;
            int on = grid.Index(mid.x, mid.y);
            Assert.That(MapFog.ToPolyline(front, mid.x, mid.y), Is.LessThan(0.6f));
            Assert.That(known[on], Is.LessThanOrEqualTo(charted[on] * (1f - MapFog.FrontShare) + 1e-3f), "on the front line the fog is thinned by the front's share");
            // "far from the front" by construction: the plate's centre is river-line, a node ON the front line
            int far = -1;
            for (int j = 0; j < grid.H && far < 0; j++)
                for (int i = 0; i < grid.W && far < 0; i++)
                {
                    float x = grid.X(i), z = grid.Z(j);
                    bool landHere = ContinentMesh.HeightAt(h, ContinentMesh.Width, ContinentMesh.Length, x / (ContinentMesh.Width * ContinentMesh.Cell), z / (ContinentMesh.Length * ContinentMesh.Cell)) > 0f;
                    if (landHere && MapFog.ToPolyline(front, x, z) > MapFog.FrontBand + MapFog.FrontSoft + 1f) far = j * grid.W + i;
                }
            Assert.GreaterOrEqual(far, 0, "some land lies well away from the front");
            Assert.AreEqual(charted[far], known[far], 1e-4f, "far from it, untouched");
        }

        // [found-strategic-map-builds-continent-mesh-and-fog-20261017-2] a List with no capacity doubles as the grid
        // grows, so the full continent threw away about 1.6 MB of intermediate arrays. Exact-size arrays cost the
        // same number of allocations at a quarter grid and at the full one.
        [Test]
        public void The_Land_Mesh_Costs_The_Same_Allocations_At_Any_Size()
        {
            if (!AllocProbe.Available) Assert.Ignore("no profiler to count allocations with");
            var full = ContinentMesh.Heights(ContinentMesh.Seed);
            const int qw = 48, ql = 32;
            var quarter = ContinentMesh.Heights(ContinentMesh.Seed, qw, ql);
            // A warm-up call at each size, so the count below is the steady cost and not first-call compilation.
            Object.DestroyImmediate(ContinentMesh.Land(full));
            Object.DestroyImmediate(ContinentMesh.Land(quarter, qw, ql));
            Mesh a = null, b = null;
            try
            {
                int small = AllocProbe.Count(() => b = ContinentMesh.Land(quarter, qw, ql));
                int big = AllocProbe.Count(() => a = ContinentMesh.Land(full));
                TestContext.WriteLine("Land allocations: quarter grid " + small + ", full grid " + big);
                Assert.That(big, Is.LessThanOrEqualTo(small + 2), "[found-strategic-map-builds-continent-mesh-and-fog-20261017-2] the cost grows with the grid: " + big + " against " + small);
                Assert.That(big, Is.LessThanOrEqualTo(24), "[found-strategic-map-builds-continent-mesh-and-fog-20261017-2] the full grid allocates " + big + " times");
            }
            finally { if (a != null) Object.DestroyImmediate(a); if (b != null) Object.DestroyImmediate(b); }
        }

        // The List version of Land, copied as it stood, is the proof that exact-size arrays did not move a vertex.
        [Test]
        public void The_Land_Mesh_Is_The_Mesh_It_Was()
        {
            const int qw = 48, ql = 32;
            var h = ContinentMesh.Heights(ContinentMesh.Seed, qw, ql);
            var mesh = ContinentMesh.Land(h, qw, ql);
            var reference = ReferenceLand(h, qw, ql);
            try
            {
                Assert.That(mesh.vertices, Is.EqualTo(reference.vertices));
                Assert.That(mesh.normals, Is.EqualTo(reference.normals));
                Assert.That(mesh.uv, Is.EqualTo(reference.uv));
                var s = new List<Vector3>(); var sRef = new List<Vector3>();
                mesh.GetUVs(3, s); reference.GetUVs(3, sRef);
                Assert.That(s, Is.EqualTo(sRef));
                for (int band = 0; band < 3; band++)
                    Assert.That(mesh.GetTriangles(band), Is.EqualTo(reference.GetTriangles(band)), "band " + band);
            }
            finally { Object.DestroyImmediate(mesh); Object.DestroyImmediate(reference); }
        }

        /// <summary>The List version of ContinentMesh.Land as it stood before the exact-size arrays, kept here so a
        /// change of shape fails even when the allocation count still looks cheap.</summary>
        static Mesh ReferenceLand(float[] h, int w, int l)
        {
            var v = new List<Vector3>(); var n = new List<Vector3>(); var s = new List<Vector3>(); var uv = new List<Vector2>();
            var tris = new[] { new List<int>(), new List<int>(), new List<int>() };
            for (int z = 0; z < l; z++)
                for (int x = 0; x < w; x++)
                {
                    float h00 = h[ContinentMesh.Index(w, x, z)], h10 = h[ContinentMesh.Index(w, x + 1, z)];
                    float h01 = h[ContinentMesh.Index(w, x, z + 1)], h11 = h[ContinentMesh.Index(w, x + 1, z + 1)];
                    const float c = ContinentMesh.Cell, r = ContinentMesh.Relief;
                    var p00 = new Vector3(x * c, h00 * r, z * c); var p10 = new Vector3((x + 1) * c, h10 * r, z * c);
                    var p01 = new Vector3(x * c, h01 * r, (z + 1) * c); var p11 = new Vector3((x + 1) * c, h11 * r, (z + 1) * c);
                    ReferenceTri(v, n, s, uv, tris, p00, p01, p11, (h00 + h01 + h11) / 3f);
                    ReferenceTri(v, n, s, uv, tris, p00, p11, p10, (h00 + h11 + h10) / 3f);
                }
            var mesh = new Mesh { name = "ReferenceContinent", hideFlags = HideFlags.HideAndDontSave };
            if (v.Count > 65535) mesh.indexFormat = UnityEngine.Rendering.IndexFormat.UInt32;
            mesh.SetVertices(v); mesh.SetNormals(n); mesh.SetUVs(0, uv); mesh.SetUVs(3, s);
            mesh.subMeshCount = 3;
            for (int b = 0; b < 3; b++) mesh.SetTriangles(tris[b], b);
            mesh.RecalculateBounds();
            return mesh;
        }

        static void ReferenceTri(List<Vector3> v, List<Vector3> n, List<Vector3> s, List<Vector2> uv, List<int>[] tris, Vector3 a, Vector3 b, Vector3 c, float rawMean)
        {
            if (a.y < 0f && b.y < 0f && c.y < 0f) return;
            var normal = Vector3.Cross(b - a, c - a).normalized;
            if (normal.y < 0f) normal = -normal;
            int i = v.Count;
            v.Add(a); v.Add(b); v.Add(c);
            n.Add(normal); n.Add(normal); n.Add(normal);
            s.Add(Vector3.up); s.Add(Vector3.up); s.Add(Vector3.up);
            uv.Add(new Vector2(a.x, a.z) * 0.05f); uv.Add(new Vector2(b.x, b.z) * 0.05f); uv.Add(new Vector2(c.x, c.z) * 0.05f);
            var t = tris[ContinentMesh.Band(rawMean)];
            t.Add(i); t.Add(i + 1); t.Add(i + 2);
        }

        // [found-fog-rebakes-full-45k-texel-grid-on-every-pin-20261017-0] selecting a pin flips Current<->Available,
        // which moves no fog hole: only locking or unlocking a node may rebake the 45,632-texel grid.
        [Test]
        public void Only_A_Lock_Changing_Dirties_The_Fog()
        {
            Assert.That(StrategicMapView.FogAffects(NodeState.Current, NodeState.Available), Is.False, "[found-fog-rebakes-full-45k-texel-grid-on-every-pin-20261017-0] a click that only moves the selection rebakes the fog");
            Assert.That(StrategicMapView.FogAffects(NodeState.Available, NodeState.Current), Is.False, "[found-fog-rebakes-full-45k-texel-grid-on-every-pin-20261017-0] a click that only moves the selection rebakes the fog");
            Assert.That(StrategicMapView.FogAffects(NodeState.Locked, NodeState.Available), Is.True, "[found-fog-rebakes-full-45k-texel-grid-on-every-pin-20261017-0] unlocking a node must rebake the fog");
            Assert.That(StrategicMapView.FogAffects(NodeState.Available, NodeState.Locked), Is.True, "[found-fog-rebakes-full-45k-texel-grid-on-every-pin-20261017-0] unlocking a node must rebake the fog");
            Assert.That(StrategicMapView.FogAffects(NodeState.Available, NodeState.Complete), Is.False, "[found-fog-rebakes-full-45k-texel-grid-on-every-pin-20261017-0] a click that only moves the selection rebakes the fog");
            Assert.That(StrategicMapView.FogAffects(NodeState.Current, NodeState.Complete), Is.False, "[found-fog-rebakes-full-45k-texel-grid-on-every-pin-20261017-0] a click that only moves the selection rebakes the fog");
            Assert.That(StrategicMapView.FogAffects(NodeState.Locked, NodeState.Locked), Is.False, "[found-fog-rebakes-full-45k-texel-grid-on-every-pin-20261017-0] a click that only moves the selection rebakes the fog");
            Assert.That(StrategicMapView.FogAffects(NodeState.Available, NodeState.Available), Is.False, "[found-fog-rebakes-full-45k-texel-grid-on-every-pin-20261017-0] a click that only moves the selection rebakes the fog");
        }
    }
}
