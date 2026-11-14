// Phase: CelFx neon acid (2026-11-14) - the jet's arc, its pool's capacity, the far simplification, that it leaves
// nothing behind, the lens handed over and put back, and the allocation guard.
using System;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.VFX.Cel;

namespace TW.Tests
{
    public sealed class NeonAcidTests
    {
        static readonly Vector3 Nozzle = new Vector3(-9f, 6f, -8f), Target = new Vector3(0f, 1.3f, 0f);

        GameObject holder;
        CelFxRenderer renderer;

        void CreateRenderer()
        {
            SceneStatics.ResetSession();
            holder = new GameObject("NeonAcidTests") { hideFlags = HideFlags.HideAndDontSave };
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
        public void The_Jet_Leaves_The_Nozzle_And_Lands_On_The_Target()
        {
            Assert.That((NeonAcidRecipe.Along(Nozzle, Target, 0f) - Nozzle).magnitude, Is.LessThan(1e-4f));
            Assert.That((NeonAcidRecipe.Along(Nozzle, Target, NeonAcidRecipe.Flight) - Target).magnitude, Is.LessThan(1e-3f));
            Vector3 half = NeonAcidRecipe.Along(Nozzle, Target, NeonAcidRecipe.Flight * 0.5f);
            Assert.That(half.y, Is.GreaterThan((Nozzle.y + Target.y) * 0.5f + 1f), "the clip's jet is an arc, not a line");
        }

        [Test]
        public void A_Close_Jet_Has_Every_Layer_And_Fits_Its_Pool()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            NeonAcidRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, 1917u);
            int kind = renderer.KindId(NeonAcidKind.KindName);
            Assert.That(kind, Is.GreaterThanOrEqualTo(0));
            Assert.That(renderer.SpawnedOf(kind), Is.GreaterThan(200), "strands, drops, splash, bubbles and smoke");
            Assert.That(renderer.SpawnedOf(kind), Is.LessThanOrEqualTo(renderer.CapacityOf(kind) / 2),
                "everything is queued up front: two jets at once must not overwrite each other's first");
        }

        [Test]
        public void A_Far_Jet_Is_The_Band_The_Puddle_Its_Light_And_A_Little_Smoke()
        {
            CreateRenderer();
            CelFx.ZoomOverride = 240f;
            NeonAcidRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, 1917u);
            int kind = renderer.KindId(NeonAcidKind.KindName);
            int far = renderer.SpawnedOf(kind);   // SpawnedOf counts every record ever spawned
            Assert.That(far, Is.InRange(5, 12), "three lights (under the jet, at the foot, after it), the puddle, the band, five puffs");
            renderer.Clear();
            CelFx.ZoomOverride = CelFx.MidZoom;
            NeonAcidRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, 1917u);
            int mid = renderer.SpawnedOf(kind) - far;
            renderer.Clear();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            NeonAcidRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, 1917u);
            int close = renderer.SpawnedOf(kind) - far - mid;
            Assert.That(mid, Is.GreaterThan(far));
            Assert.That(close, Is.GreaterThan(mid * 2));
        }

        [Test]
        public void The_Jet_Draws_While_It_Runs_And_Leaves_Nothing_Behind()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            NeonAcidRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, 1917u);
            renderer.Submit(101.5f);
            Assert.That(renderer.DrawCalls, Is.EqualTo(1), "one pool, one draw");
            renderer.Submit(108f);
            Assert.That(renderer.DrawCalls, Is.Zero, "the puddle dries and the smoke clears: nothing stays after the clip's eight seconds");
        }

        [Test]
        public void The_Lens_Is_Handed_Over_And_Put_Back_With_The_Session()
        {
            NeonAcidRecipe.Lens(0.4f, 0.5f);
            Vector4 lens = Shader.GetGlobalVector("_TWAcidLens");
            Assert.That(lens.x, Is.EqualTo(1.2f).Within(1e-4f));
            Assert.That(lens.y, Is.EqualTo(2.5f).Within(1e-4f));
            Assert.That(SceneStatics.Registered, Does.Contain(nameof(NeonAcidRecipe)));
            SceneStatics.ResetSession();
            Assert.That(Shader.GetGlobalVector("_TWAcidLens").x, Is.Zero, "no vignette is assumed once the session is over");
        }

        [Test]
        public void A_Warmed_Play_Allocates_Nothing()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            uint seed = 0;
            long bytes = Allocated(() => NeonAcidRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, seed++), 20);
            Assert.That(bytes, Is.Zero);
        }
    }
}
