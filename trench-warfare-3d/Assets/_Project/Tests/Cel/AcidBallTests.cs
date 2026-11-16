// Phase: CelFx acid ball (2026-11-15) - the ball's path, its pool's capacity, the far simplification, that it leaves
// nothing behind, the lens handed over and put back, and the allocation guard.
using System;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.VFX.Cel;

namespace TW.Tests
{
    public sealed class AcidBallTests
    {
        static readonly Vector3 From = new Vector3(-6.9f, 4.8f, -9.6f), Target = new Vector3(0f, 1.3f, 0f);

        GameObject holder;
        CelFxRenderer renderer;

        void CreateRenderer()
        {
            SceneStatics.ResetSession();
            holder = new GameObject("AcidBallTests") { hideFlags = HideFlags.HideAndDontSave };
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

        [Test]
        public void The_Ball_Leaves_Where_It_Starts_And_Lands_Where_It_Is_Sent()
        {
            Assert.That((AcidBallRecipe.Along(From, Target, 0f) - From).magnitude, Is.LessThan(1e-4f));
            Assert.That((AcidBallRecipe.Along(From, Target, AcidBallRecipe.Flight) - Target).magnitude, Is.LessThan(1e-3f));
            Vector3 half = AcidBallRecipe.Along(From, Target, AcidBallRecipe.Flight * 0.5f);
            Vector3 line = (From + Target) * 0.5f;
            Assert.That(half.y - line.y, Is.InRange(0.05f, 0.6f), "the clip's ball flies nearly straight: a shallow arc");
        }

        [Test]
        public void A_Close_Ball_Has_Every_Layer_And_Fits_Its_Pool()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            AcidBallRecipe.Play(From, Target, Vector3.zero, Vector3.right, 1917u);
            int kind = renderer.KindId(AcidBallKind.KindName);
            Assert.That(kind, Is.GreaterThanOrEqualTo(0));
            Assert.That(renderer.SpawnedOf(kind), Is.GreaterThan(150), "ball, burst, crown, ring, drops, smoke, bubbles, cloud and specks");
            Assert.That(renderer.SpawnedOf(kind), Is.LessThanOrEqualTo(renderer.CapacityOf(kind) / 2),
                "everything is queued up front: two balls at once must not overwrite each other's first");
        }

        [Test]
        public void A_Far_Ball_Is_The_Ball_The_Burst_The_Ring_The_Puddle_Its_Light_And_A_Little_Smoke()
        {
            CreateRenderer();
            CelFx.ZoomOverride = 240f;
            AcidBallRecipe.Play(From, Target, Vector3.zero, Vector3.right, 1917u);
            int kind = renderer.KindId(AcidBallKind.KindName);
            int far = renderer.SpawnedOf(kind);   // SpawnedOf counts every record ever spawned
            Assert.That(far, Is.InRange(10, 24), "five lights, the puddle, the ring, the ball, the burst, six puffs, three of the cloud");
            renderer.Clear();
            CelFx.ZoomOverride = CelFx.MidZoom;
            AcidBallRecipe.Play(From, Target, Vector3.zero, Vector3.right, 1917u);
            int mid = renderer.SpawnedOf(kind) - far;
            renderer.Clear();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            AcidBallRecipe.Play(From, Target, Vector3.zero, Vector3.right, 1917u);
            int close = renderer.SpawnedOf(kind) - far - mid;
            Assert.That(mid, Is.GreaterThan(far));
            Assert.That(close, Is.GreaterThan(mid * 2));
        }

        [Test]
        public void The_Ball_Draws_While_It_Runs_And_Leaves_Nothing_Behind()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            AcidBallRecipe.Play(From, Target, Vector3.zero, Vector3.right, 1917u);
            renderer.Submit(101.5f);
            Assert.That(renderer.DrawCalls, Is.EqualTo(1), "one pool, one draw");
            renderer.Submit(108f);
            Assert.That(renderer.DrawCalls, Is.Zero, "the stain dries and the cloud clears: nothing stays after the clip's eight seconds");
        }

        [Test]
        public void The_Lens_Is_Handed_Over_And_Put_Back_With_The_Session()
        {
            AcidBallRecipe.Lens(0.4f, 0.5f);
            Vector4 lens = Shader.GetGlobalVector("_TWAcidLens");
            Assert.That(lens.x, Is.EqualTo(1.2f).Within(1e-4f));
            Assert.That(lens.y, Is.EqualTo(2.5f).Within(1e-4f));
            Assert.That(SceneStatics.Registered, Does.Contain(nameof(AcidBallRecipe)));
            SceneStatics.ResetSession();
            Assert.That(Shader.GetGlobalVector("_TWAcidLens").x, Is.Zero, "no vignette is assumed once the session is over");
        }

        [Test]
        public void A_Warmed_Play_Allocates_Nothing()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            uint seed = 0;
            long bytes = Allocated(() => AcidBallRecipe.Play(From, Target, Vector3.zero, Vector3.right, seed++), 20);
            Assert.That(bytes, Is.Zero);
        }
    }
}
