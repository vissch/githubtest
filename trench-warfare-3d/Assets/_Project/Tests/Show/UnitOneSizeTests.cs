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
            try
            {
                float first = -1f;
                foreach (var zoom in Zooms)
                {
                    float riderScale = Mathf.Min(vat.ScaleAt(zoom) / FigureMetrics.UnitScale * 1.15f, 2.2f);
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
                float height = model.Height;   // VehicleSize.Tank is baked into the vertices at load time, not the zoom
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
                float height = model.Height;
                if (first < 0f) first = height;
                Assert.AreEqual(first, height, 1e-4f, $"Pincer at zoom {zoom}");
            }
        }
    }
}
