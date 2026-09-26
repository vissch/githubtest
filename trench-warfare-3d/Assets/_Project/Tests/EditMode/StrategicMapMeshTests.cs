// Phase: B6 / docs/21 phase 6 (implemented) — the strategic map's continent: the same seed builds the same heights,
// every country node of the campaign graph stands on land while the corners are sea, the land mesh has its three
// bands and stays under the 16-bit index limit, and a map position lands where the pins expect it. No scene.
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
    }
}
