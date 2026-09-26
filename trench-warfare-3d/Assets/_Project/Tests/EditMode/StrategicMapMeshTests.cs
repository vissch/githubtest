// Phase: B6 / docs/21 phase 6 (implemented) — the strategic map's continent: the same seed builds the same heights,
// every country node of the campaign graph stands on land while the corners are sea, the land mesh has its three
// bands and stays under the 16-bit index limit, and a map position lands where the pins expect it. No scene.
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Meta;
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
            Assert.AreEqual(charted[land], known[land], 1e-4f, "far from it, untouched");
        }
    }
}
