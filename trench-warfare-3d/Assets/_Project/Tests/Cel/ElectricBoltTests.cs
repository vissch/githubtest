// Phase: CelFx electric bolt (2026-11-15) - the stream's pool and parts, the far simplification, that it leaves nothing
// behind, the lens handed over and put back, and the allocation guard.
using System;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.VFX.Cel;

namespace TW.Tests
{
    public sealed class ElectricBoltTests
    {
        static readonly Vector3 From = new Vector3(10f, 1.1f, 8.5f), Aim = new Vector3(0.1f, 1.0f, 0.1f), Target = new Vector3(0f, 1.3f, 0f), Foot = Vector3.zero;

        GameObject holder;
        CelFxRenderer renderer;

        void CreateRenderer()
        {
            SceneStatics.ResetSession();
            holder = new GameObject("ElectricBoltTests") { hideFlags = HideFlags.HideAndDontSave };
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
            int kind = renderer.KindId(ElectricBoltKind.KindName);
            int before = renderer.SpawnedOf(kind);
            ElectricBoltRecipe.Play(From, Aim, Target, Foot, 1917u);
            int n = renderer.SpawnedOf(kind) - before;
            renderer.Clear();
            return n;
        }

        [Test]
        public void A_Close_Stream_Has_Every_Part_And_Fits_Half_Its_Pool()
        {
            CreateRenderer();
            int kind = renderer.KindId(ElectricBoltKind.KindName);
            Assert.That(kind, Is.GreaterThanOrEqualTo(0));
            int close = Spawned(CelFx.CloseZoom);
            Assert.That(close, Is.GreaterThan(250), "43 bolts, the arcs, the flares, the hit's blooms and rays, the lights");
            Assert.That(close, Is.LessThanOrEqualTo(renderer.CapacityOf(kind) / 2),
                "everything is queued up front: two streams at once must not overwrite each other's first");
        }

        [Test]
        public void Going_Out_It_Keeps_The_Big_Shapes_And_Drops_Most_Pieces()
        {
            CreateRenderer();
            int close = Spawned(CelFx.CloseZoom), mid = Spawned(CelFx.MidZoom), far = Spawned(240f);
            Assert.That(far, Is.GreaterThanOrEqualTo(10), "a few bolts, the upper arc, the hit and the ground light");
            Assert.That(mid, Is.GreaterThan(far));
            Assert.That(close, Is.GreaterThan(mid));
        }

        [Test]
        public void The_Stream_Draws_While_It_Runs_And_Leaves_Nothing_Behind()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            ElectricBoltRecipe.Play(From, Aim, Target, Foot, 1917u);
            renderer.Submit(102f);
            Assert.That(renderer.DrawCalls, Is.EqualTo(2), "the bolts' pool and the sparks' Streaks: two draws");
            renderer.Submit(106f);
            Assert.That(renderer.DrawCalls, Is.Zero, "no smoke, no scorch: nothing stays after the last arc");
        }

        [Test]
        public void Its_Spark_Recipe_Is_Valid()
        {
            Assert.That(ElectricBoltRecipe.Sparks.Validate(), Is.Empty);
            Assert.That(CelPalette.Find("electric_bolt"), Is.GreaterThan(0));
        }

        [Test]
        public void The_Lens_Is_Handed_Over_And_Put_Back_With_The_Session()
        {
            ElectricBoltRecipe.Lens(0.4f, 0.5f);
            Vector4 lens = Shader.GetGlobalVector("_TWBoltLens");
            Assert.That(lens.x, Is.EqualTo(1.2f).Within(1e-4f));
            Assert.That(lens.y, Is.EqualTo(2.5f).Within(1e-4f));
            Assert.That(SceneStatics.Registered, Does.Contain(nameof(ElectricBoltRecipe)));
            SceneStatics.ResetSession();
            Assert.That(Shader.GetGlobalVector("_TWBoltLens").x, Is.Zero, "no vignette is assumed once the session is over");
        }

        [Test]
        public void A_Warmed_Play_Allocates_Nothing()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            uint seed = 0;
            long bytes = Allocated(() => ElectricBoltRecipe.Play(From, Aim, Target, Foot, seed++), 20);
            Assert.That(bytes, Is.Zero);
        }
    }
}
