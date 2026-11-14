// Phase: CelFx fireball (2026-11-14) - the comet's arc, the pool's capacity, the far simplification, that the
// effect leaves nothing behind, the lens handed over and put back, and the allocation guard. Copied from
// neon_acid's tests in a01 and corrected to the fireball's own parts in a10, the first run of this lane's gate.
using System;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.VFX.Cel;

namespace TW.Tests
{
    public sealed class FireballTests
    {
        static readonly Vector3 Nozzle = new Vector3(-9f, 6f, -8f), Target = new Vector3(0f, 1.3f, 0f);

        GameObject holder;
        CelFxRenderer renderer;

        void CreateRenderer()
        {
            SceneStatics.ResetSession();
            holder = new GameObject("FireballTests") { hideFlags = HideFlags.HideAndDontSave };
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
        public void The_Comet_Starts_Off_Screen_And_Lands_On_The_Target()
        {
            Assert.That((FireballRecipe.Along(Nozzle, Target, 0f) - Nozzle).magnitude, Is.LessThan(1e-4f));
            Assert.That((FireballRecipe.Along(Nozzle, Target, FireballRecipe.Flight) - Target).magnitude, Is.LessThan(1e-3f));
            Vector3 half = FireballRecipe.Along(Nozzle, Target, FireballRecipe.Flight * 0.5f);
            Assert.That(half.y, Is.GreaterThan((Nozzle.y + Target.y) * 0.5f), "the clip's comet droops a little, it does not dive");
            Assert.That(half.y, Is.LessThan((Nozzle.y + Target.y) * 0.5f + 0.5f), "the clip's comet is near level, not an artillery arc");
        }

        [Test]
        public void A_Close_Jet_Has_Every_Layer_And_Fits_Its_Pool()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            FireballRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, 1917u);
            int kind = renderer.KindId(FireballKind.KindName);
            Assert.That(kind, Is.GreaterThanOrEqualTo(0));
            Assert.That(renderer.SpawnedOf(kind), Is.GreaterThan(200), "strands, drops, splash, bubbles and smoke");
            Assert.That(renderer.SpawnedOf(kind), Is.LessThanOrEqualTo(renderer.CapacityOf(kind) / 2),
                "everything is queued up front: two jets at once must not overwrite each other's first");
        }

        [Test]
        public void A_Far_Fireball_Is_The_Comet_Its_Tail_The_Burst_And_Its_Light()
        {
            CreateRenderer();
            CelFx.ZoomOverride = 240f;
            FireballRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, 1917u);
            int kind = renderer.KindId(FireballKind.KindName);
            int far = renderer.SpawnedOf(kind);   // SpawnedOf counts every record ever spawned
            // a10: this bound was neon_acid's (5..12 for a band, a puddle and three lights) and had never been run
            // against the fireball, whose far play is a different set of parts: two ground lights, the comet's head
            // and the ten records of its tail, the burst's eight lobes and the eight smoke puffs they cool into -
            // 29 records - with the hit's splat, the spark rays and the falling embers all left out at this distance.
            Assert.That(far, Is.InRange(20, 40),
                "two ground lights, the comet's head and tail, the burst's lobes and their smoke; no splat, no rays, no embers");
            renderer.Clear();
            CelFx.ZoomOverride = CelFx.MidZoom;
            FireballRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, 1917u);
            int mid = renderer.SpawnedOf(kind) - far;
            renderer.Clear();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            FireballRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, 1917u);
            int close = renderer.SpawnedOf(kind) - far - mid;
            Assert.That(mid, Is.GreaterThan(far));
            Assert.That(close, Is.GreaterThan(mid * 2));
        }

        [Test]
        public void The_Jet_Draws_While_It_Runs_And_Leaves_Nothing_Behind()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            FireballRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, 1917u);
            renderer.Submit(101.5f);
            Assert.That(renderer.DrawCalls, Is.EqualTo(1), "one pool, one draw");
            renderer.Submit(108f);
            Assert.That(renderer.DrawCalls, Is.Zero, "the puddle dries and the smoke clears: nothing stays after the clip's eight seconds");
        }

        [Test]
        public void The_Lens_Is_Handed_Over_And_Put_Back_With_The_Session()
        {
            FireballRecipe.Lens(0.4f, 0.5f);
            Vector4 lens = Shader.GetGlobalVector("_TWAcidLens");
            Assert.That(lens.x, Is.EqualTo(1.2f).Within(1e-4f));
            Assert.That(lens.y, Is.EqualTo(2.5f).Within(1e-4f));
            Assert.That(SceneStatics.Registered, Does.Contain(nameof(FireballRecipe)));
            SceneStatics.ResetSession();
            Assert.That(Shader.GetGlobalVector("_TWAcidLens").x, Is.Zero, "no vignette is assumed once the session is over");
        }

        [Test]
        public void A_Warmed_Play_Allocates_Nothing()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            uint seed = 0;
            long bytes = Allocated(() => FireballRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, seed++), 20);
            Assert.That(bytes, Is.Zero);
        }
    }
}
