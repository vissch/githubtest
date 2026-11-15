// Phase: CelFx red_lasers (2026-11-15) - the volley's pool and parts, the far simplification, that the smoke outlives the
// bolts and then nothing is left, the clip's launch tables, the lens handed over and put back, and the allocation guard.
using System;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.VFX.Cel;

namespace TW.Tests
{
    public sealed class RedLasersTests
    {
        static readonly Vector3 Muzzle = new Vector3(12.9f, 1.36f, 0.1f), Target = new Vector3(0f, 1.3f, 0f), Foot = Vector3.zero;

        GameObject holder;
        CelFxRenderer renderer;

        void CreateRenderer()
        {
            SceneStatics.ResetSession();
            holder = new GameObject("RedLasersTests") { hideFlags = HideFlags.HideAndDontSave };
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

        static long Allocated(Action act, int times)
        {
            act();
            long before = GC.GetAllocatedBytesForCurrentThread();
            for (int i = 0; i < times; i++) act();
            return GC.GetAllocatedBytesForCurrentThread() - before;
        }

        int Spawned(float zoom)
        {
            CelFx.ZoomOverride = zoom;
            int kind = renderer.KindId(RedLasersKind.KindName);
            int before = renderer.SpawnedOf(kind);
            RedLasersRecipe.Play(Muzzle, Target, Foot, 1917u);
            int n = renderer.SpawnedOf(kind) - before;
            renderer.Clear();
            return n;
        }

        [Test]
        public void A_Close_Volley_Has_Every_Part_And_Fits_Half_Its_Pool()
        {
            CreateRenderer();
            int kind = renderer.KindId(RedLasersKind.KindName);
            Assert.That(kind, Is.GreaterThanOrEqualTo(0));
            int close = Spawned(CelFx.CloseZoom);
            Assert.That(close, Is.GreaterThan(300), "53 bolts, 6 misses, the hit's stars and blooms, the burns, the smoke, the lights");
            Assert.That(close, Is.LessThanOrEqualTo(renderer.CapacityOf(kind) / 2),
                "everything is queued up front: two volleys at once must not overwrite each other's first");
        }

        [Test]
        public void Going_Out_It_Keeps_The_Bolts_And_The_Hit_And_Drops_Most_Pieces()
        {
            CreateRenderer();
            int close = Spawned(CelFx.CloseZoom), mid = Spawned(CelFx.MidZoom), far = Spawned(240f);
            Assert.That(far, Is.GreaterThanOrEqualTo(10), "a third of the bolts and the hit's stars");
            Assert.That(mid, Is.GreaterThan(far));
            Assert.That(close, Is.GreaterThan(mid));
        }

        [Test]
        public void The_Volley_Draws_While_It_Runs_The_Smoke_Outlives_It_And_Nothing_Stays()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            RedLasersRecipe.Play(Muzzle, Target, Foot, 1917u);
            // the clip's frame f falls f / 24 - 0.5 s after the fire
            renderer.Submit(100f + 60f / 24f - 0.5f);
            Assert.That(renderer.DrawCalls, Is.EqualTo(2), "the lasers' pool and the sparks' Streaks: two draws");
            renderer.Submit(100f + 130f / 24f - 0.5f);
            Assert.That(renderer.DrawCalls, Is.EqualTo(1), "frame 130: only the smoke over the second ground hit is left");
            renderer.Submit(100f + 150f / 24f - 0.5f);
            Assert.That(renderer.DrawCalls, Is.Zero, "frame 150: the clip is empty from frame 136");
        }

        [Test]
        public void The_Launch_Tables_Are_The_Clips()
        {
            foreach (var lane in new[] { RedLasersRecipe.LaunchA, RedLasersRecipe.LaunchB })
            {
                for (int i = 1; i < lane.Length; i++)
                    Assert.That(lane[i], Is.GreaterThan(lane[i - 1] + 1.5f), "a barrel fires about every 3.2 frames");
                Assert.That(lane[0], Is.InRange(13f, 18f));
                Assert.That(lane[lane.Length - 1], Is.InRange(92f, 94f), "the last bolts land on frame 100");
            }
            Assert.That(RedLasersRecipe.LaunchA.Length + RedLasersRecipe.LaunchB.Length, Is.InRange(50, 56), "15 a second for 3.3 s");
        }

        [Test]
        public void Its_Spark_Recipe_And_Palettes_Are_Valid()
        {
            Assert.That(RedLasersRecipe.Sparks.Validate(), Is.Empty);
            Assert.That(CelPalette.Find("red_lasers_bolt"), Is.GreaterThan(0));
            Assert.That(CelPalette.Find("red_lasers_spark"), Is.GreaterThan(0));
            Assert.That(CelPalette.Find("red_lasers_smoke"), Is.GreaterThan(0));
        }

        [Test]
        public void The_Lens_Is_Handed_Over_And_Put_Back_With_The_Session()
        {
            RedLasersRecipe.Lens(0.4f, 0.5f);
            Vector4 lens = Shader.GetGlobalVector("_TWRedLaserLens");
            Assert.That(lens.x, Is.EqualTo(1.2f).Within(1e-4f));
            Assert.That(lens.y, Is.EqualTo(2.5f).Within(1e-4f));
            Assert.That(SceneStatics.Registered, Does.Contain(nameof(RedLasersRecipe)));
            SceneStatics.ResetSession();
            Assert.That(Shader.GetGlobalVector("_TWRedLaserLens").x, Is.Zero, "no vignette is assumed once the session is over");
        }

        [Test]
        public void A_Warmed_Play_Allocates_Nothing()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            uint seed = 0;
            long bytes = Allocated(() => RedLasersRecipe.Play(Muzzle, Target, Foot, seed++), 20);
            Assert.That(bytes, Is.Zero);
        }
    }
}
