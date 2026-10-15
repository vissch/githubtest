// Phase: units-one-size — the owner, 2026-10-13: "the units... should allways be one size". Overturns the
// 2026-09-26 "readability grow is a men-only exception" row (docs/reference/decisions.md): every unit (infantry,
// machines, walkers, heroes, riders) is drawn at one world size at every camera zoom.
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Sim.Nav;
using TW.Presentation;
using TW.Presentation.Units;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public sealed class UnitOneSizeTests
    {
        static readonly float[] Zooms = { 6f, 120f, 600f };   // TacticalCamera.ZoomMin, a middle value, ZoomMax

        [Test]
        public void Infantry_Is_The_Same_Drawn_Height_At_Every_Zoom()
        {
            var go = new GameObject("vat");
            var vat = go.AddComponent<VATRenderer>();
            try
            {
                foreach (var zoom in Zooms)
                    Assert.AreEqual(2.00f, vat.ScaleAt(zoom) * FigureMetrics.BakeHeightM, 0.01f, $"infantry at zoom {zoom}");
            }
            finally { Object.DestroyImmediate(go); }
        }

        [Test]
        public void Rider_Scale_Is_The_Same_At_Every_Zoom()
        {
            var go = new GameObject("vat");
            var vat = go.AddComponent<VATRenderer>();
            var tanks = go.AddComponent<TankRenderer>();
            try
            {
                float first = -1f;
                foreach (var zoom in Zooms)
                {
                    float riderScale = tanks.RiderScaleAt(vat, zoom);
                    if (first < 0f) first = riderScale;
                    Assert.AreEqual(first, riderScale, 1e-4f, $"rider at zoom {zoom}");
                }
            }
            finally { Object.DestroyImmediate(go); }
        }

        [Test]
        public void A_Tank_Is_The_Same_Drawn_Height_At_Every_Zoom()
        {
            var model = TankModel.Load("Maw", VehicleArchetype.Maw, "Hull", VehicleSize.Tank);
            Assert.NotNull(model, "Maw did not load from Resources/Vehicles/Maw");
            float first = -1f;
            foreach (var zoom in Zooms)
            {
                float height = MachineScale.DrawnHeightM(model, zoom);
                if (first < 0f) first = height;
                Assert.AreEqual(first, height, 1e-4f, $"Maw at zoom {zoom}");
            }
        }

        [Test]
        public void A_Walker_Is_The_Same_Drawn_Height_At_Every_Zoom()
        {
            var model = TankModel.Load("Pincer", VehicleArchetype.Pincer, "Body", VehicleSize.Walker);
            Assert.NotNull(model, "Pincer did not load from Resources/Vehicles/Pincer");
            float first = -1f;
            foreach (var zoom in Zooms)
            {
                float height = MachineScale.DrawnHeightM(model, zoom);
                if (first < 0f) first = height;
                Assert.AreEqual(first, height, 1e-4f, $"Pincer at zoom {zoom}");
            }
        }

        /// <summary>Proves the three assertions above can fail: run against a stand-in whose "drawn" size grows with
        /// the zoom (the grow men used to get before 2026-10-13, FigureMetrics.GrowFromZoom) and expect them to go red.</summary>
        [Test]
        public void OneSizeAssertions_Catches_A_Grow_That_Came_Back()
        {
            static float Grown(float size, float zoom) => size * (1f + zoom / FigureMetrics.GrowFromZoom);

            Assert.Throws<AssertionException>(() =>
            {
                float first = -1f;
                foreach (var zoom in Zooms)
                {
                    float height = Grown(3f, zoom);
                    if (first < 0f) first = height;
                    Assert.AreEqual(first, height, 1e-4f, $"Maw at zoom {zoom}");
                }
            }, "a zoom-dependent tank height must fail the one-size assertion");

            Assert.Throws<AssertionException>(() =>
            {
                float first = -1f;
                foreach (var zoom in Zooms)
                {
                    float height = Grown(2.6f, zoom);
                    if (first < 0f) first = height;
                    Assert.AreEqual(first, height, 1e-4f, $"Pincer at zoom {zoom}");
                }
            }, "a zoom-dependent walker height must fail the one-size assertion");

            Assert.Throws<AssertionException>(() =>
            {
                float first = -1f;
                foreach (var zoom in Zooms)
                {
                    float riderScale = Grown(FigureMetrics.UnitScale, zoom);
                    if (first < 0f) first = riderScale;
                    Assert.AreEqual(first, riderScale, 1e-4f, $"rider at zoom {zoom}");
                }
            }, "a zoom-dependent rider scale must fail the one-size assertion");
        }
    }
}
