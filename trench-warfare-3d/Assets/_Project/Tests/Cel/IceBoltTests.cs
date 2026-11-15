// Phase: CelFx ice_bolt (2026-11-15) - the volley's speed, its pool's capacity, the far simplification, that it leaves
// nothing behind, the lens handed over and put back, the Range registration and the allocation guard.
using System;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.VFX;
using TW.Presentation.VFX.Cel;

namespace TW.Tests
{
    public sealed class IceBoltTests
    {
        static readonly Vector3 Muzzle = new Vector3(8f, 1.7f, 10f), Target = new Vector3(0f, 1.3f, 0f);

        GameObject holder;
        CelFxRenderer renderer;

        void CreateRenderer()
        {
            SceneStatics.ResetSession();
            holder = new GameObject("IceBoltTests") { hideFlags = HideFlags.HideAndDontSave };
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
        public void The_Darts_Cross_The_Corridor_At_The_Clips_Speed()
        {
            // the clip's darts cross the 12.9 m corridor's visible 0.92 in six frames: ~3.7 shares a second
            Assert.That(IceBoltRecipe.HeadSpeed, Is.InRange(3.3f, 4.1f));
            Assert.That(IceBoltRecipe.DartWindow, Is.InRange(0.05f, 0.2f), "a dart is about a tenth of the corridor long");
        }

        [Test]
        public void Both_Recipes_Validate()
        {
            CreateRenderer();
            Assert.That(IceBoltRecipe.Volley.Validate(renderer), Is.Empty);
            Assert.That(IceBoltRecipe.Hit.Validate(renderer), Is.Empty);
        }

        [Test]
        public void A_Close_Volley_Has_Every_Layer_And_Fits_Its_Pool()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            IceBoltRecipe.Play(Muzzle, Target, 1917u);
            int kind = renderer.KindId(IceBoltKind.KindName);
            Assert.That(kind, Is.GreaterThanOrEqualTo(0));
            Assert.That(renderer.SpawnedOf(kind), Is.GreaterThan(250), "darts, ribbons, blooms, stars, chips, shards and frost");
            Assert.That(renderer.SpawnedOf(kind), Is.LessThanOrEqualTo(renderer.CapacityOf(kind) / 2),
                "everything is queued up front: two volleys at once must not overwrite each other's first");
        }

        [Test]
        public void Going_Out_Spawns_Fewer_But_Never_Nothing()
        {
            CreateRenderer();
            int kind = renderer.KindId(IceBoltKind.KindName);
            CelFx.ZoomOverride = 240f;
            IceBoltRecipe.Play(Muzzle, Target, 1917u);
            int far = renderer.SpawnedOf(kind);   // SpawnedOf counts every record ever spawned
            renderer.Clear();
            CelFx.ZoomOverride = CelFx.MidZoom;
            IceBoltRecipe.Play(Muzzle, Target, 1917u);
            int mid = renderer.SpawnedOf(kind) - far;
            renderer.Clear();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            IceBoltRecipe.Play(Muzzle, Target, 1917u);
            int close = renderer.SpawnedOf(kind) - far - mid;
            Assert.That(far, Is.GreaterThan(0), "the darts and the bloom still read far out");
            Assert.That(mid, Is.GreaterThan(far));
            Assert.That(close, Is.GreaterThan(mid));
        }

        [Test]
        public void The_Ice_Draws_While_It_Runs_And_Leaves_Nothing_Behind()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            IceBoltRecipe.Play(Muzzle, Target, 1917u);
            renderer.Submit(101.5f);
            Assert.That(renderer.DrawCalls, Is.EqualTo(1), "one pool, one draw");
            renderer.Submit(108f);
            Assert.That(renderer.DrawCalls, Is.Zero, "the frost clears: nothing stays after the clip's eight seconds");
        }

        [Test]
        public void The_Lens_Is_Handed_Over_And_Put_Back_With_The_Session()
        {
            IceBoltRecipe.Lens(0.4f, 0.5f);
            Vector4 lens = Shader.GetGlobalVector("_TWIceLens");
            Assert.That(lens.x, Is.EqualTo(1.2f).Within(1e-4f));
            Assert.That(lens.y, Is.EqualTo(2.5f).Within(1e-4f));
            Assert.That(SceneStatics.Registered, Does.Contain(nameof(IceBoltRecipe)));
            SceneStatics.ResetSession();
            Assert.That(Shader.GetGlobalVector("_TWIceLens").x, Is.Zero, "no vignette is assumed once the session is over");
        }

        [Test]
        public void The_Range_Knows_It()
        {
            Assert.That(CelRange.Has("ice_bolt"), "[CelRangeEffect(\"ice_bolt\")] on IceBoltRecipe.Range");
        }

        [Test]
        public void A_Warmed_Play_Allocates_Nothing()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            uint seed = 0;
            long bytes = Allocated(() => IceBoltRecipe.Play(Muzzle, Target, seed++), 20);
            Assert.That(bytes, Is.Zero);
        }
    }
}
