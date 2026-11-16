// Phase: CelFx orbital laser (2026-11-09) - arc direction, far simplification, persistence and allocation guards.
using System;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.VFX.Cel;

namespace TW.Tests
{
    public sealed class OrbitalLaserTests
    {
        GameObject holder;
        CelFxRenderer renderer;

        void CreateRenderer()
        {
            SceneStatics.ResetSession();
            holder = new GameObject("OrbitalLaserTests") { hideFlags = HideFlags.HideAndDontSave };
            renderer = holder.AddComponent<CelFxRenderer>();
            renderer.Init();
            CelFx.Adopt(renderer);
            CelFx.Clock = 100f;
        }

        [TearDown]
        public void TearDown()
        {
            if (renderer != null) renderer.Shutdown();
            if (holder != null) UnityEngine.Object.DestroyImmediate(holder);
            SceneStatics.ResetSession();
        }

        [Test]
        public void Far_Uses_Only_The_Beam_And_Lasting_Scorch()
        {
            CreateRenderer();
            CelFx.ZoomOverride = 240f;
            OrbitalLaserRecipe.Play(Vector3.zero, Vector3.right, 1917u);
            int orbital = renderer.KindId(OrbitalLaserKind.KindName);
            int ground = renderer.KindId(OrbitalGroundKind.KindName);
            int debris = renderer.KindId(OrbitalDebrisKind.KindName);
            Assert.That(debris, Is.GreaterThanOrEqualTo(0));
            Assert.That(renderer.SpawnedOf(orbital), Is.EqualTo(2));
            Assert.That(renderer.SpawnedOf(ground), Is.Zero);
            Assert.That(renderer.SpawnedOf(debris), Is.Zero);
            Assert.That(renderer.SpawnedOf(CelKinds.Lobe), Is.Zero);
            Assert.That(renderer.SpawnedOf(CelKinds.Streak), Is.Zero);
            renderer.Submit(108f);
            Assert.That(renderer.DrawCalls, Is.EqualTo(1), "the black scorch stays after the eight-second clip");
        }

        [Test]
        public void The_Sweep_Travels_Around_The_Left_Arc_From_Back_To_Front()
        {
            Vector3 start = OrbitalLaserRecipe.SweepOffset(0f, Vector3.right);
            Vector3 middle = OrbitalLaserRecipe.SweepOffset(1.5f, Vector3.right);
            Vector3 end = OrbitalLaserRecipe.SweepOffset(OrbitalLaserRecipe.BeamSeconds, Vector3.right);
            Assert.That(start.z, Is.GreaterThan(OrbitalLaserRecipe.RadiusForward * 0.85f));
            Assert.That(middle.x - OrbitalLaserRecipe.SweepCentreRight, Is.LessThan(-2.8f));
            Assert.That(end.z, Is.LessThan(-OrbitalLaserRecipe.RadiusForward * 0.95f));
            for (int i = 0; i <= 80; i++)
            {
                Vector3 p = OrbitalLaserRecipe.SweepOffset(i * 0.1f, Vector3.right);
                Assert.That(p.y, Is.Zero);
                Assert.That(p.x - 2.55f, Is.LessThan(-1.8f), "the reference sweep stays left of the dummy");
            }
        }

        [Test]
        public void One_Close_Strike_On_Low_Quality_Does_Not_Overwrite_Its_Own_Beam()
        {
            FxTier previous = FxQuality.Tier;
            try
            {
                FxQuality.Set(FxTier.Low);
                CreateRenderer();
                CelFx.ZoomOverride = 16f;
                OrbitalLaserRecipe.Play(Vector3.zero, Vector3.right, 1917u);
                int orbital = renderer.KindId(OrbitalLaserKind.KindName);
                int ground = renderer.KindId(OrbitalGroundKind.KindName);
                int debris = renderer.KindId(OrbitalDebrisKind.KindName);
                Assert.That(renderer.SpawnedOf(orbital), Is.GreaterThan(0));
                Assert.That(renderer.SpawnedOf(orbital), Is.LessThanOrEqualTo(renderer.CapacityOf(orbital)),
                    "all records are queued up front; overflowing here erases the beam and impact before the first frame");
                Assert.That(renderer.SpawnedOf(ground), Is.EqualTo(2), "the close strike retains its crater and scorch");
                Assert.That(renderer.SpawnedOf(ground), Is.LessThanOrEqualTo(renderer.CapacityOf(ground)));
                Assert.That(renderer.SpawnedOf(debris), Is.GreaterThan(0), "the close strike includes fragments and rim stones");
                Assert.That(renderer.SpawnedOf(debris), Is.LessThanOrEqualTo(renderer.CapacityOf(debris)),
                    "future fragments and permanent stones must fit together before the first frame");
                Assert.That(renderer.SpawnedOf(CelKinds.Streak), Is.LessThanOrEqualTo(renderer.CapacityOf(CelKinds.Streak)));
            }
            finally { FxQuality.Set(previous); }
        }

        [Test]
        public void The_Close_Scorch_And_Rim_Stones_Persist_Until_Clear()
        {
            CreateRenderer();
            CelFx.ZoomOverride = 16f;
            OrbitalLaserRecipe.Play(Vector3.zero, Vector3.right, 1917u);
            int ground = renderer.KindId(OrbitalGroundKind.KindName);
            int debris = renderer.KindId(OrbitalDebrisKind.KindName);
            Assert.That(renderer.SpawnedOf(ground), Is.EqualTo(2));
            Assert.That(renderer.SpawnedOf(debris), Is.GreaterThan(0));
            renderer.Submit(108f);
            Assert.That(renderer.DrawCalls, Is.EqualTo(2),
                "after transient effects expire, the ground scar and rim-stone pool remain");
            renderer.Clear();
            renderer.Submit(108f);
            Assert.That(renderer.DrawCalls, Is.Zero, "clearing the effect also removes its permanent ground layers");
        }

        [Test]
        public void A_Warmed_Orbital_Play_Allocates_Nothing()
        {
            CreateRenderer();
            CelFx.ZoomOverride = 16f;
            Action play = () =>
            {
                for (uint i = 0; i < 20; i++) OrbitalLaserRecipe.Play(Vector3.zero, Vector3.right, i);
            };
            play();
            Assert.That(OrbitalLaserLightingTests.CountAllocations(play), Is.Zero);
        }
    }
}
