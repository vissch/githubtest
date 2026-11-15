// Phase: CelFx acid beam (2026-11-15) - the beam's line, its pool's capacity, the far simplification, that it leaves
// nothing behind, the lens handed over and put back, and the allocation guard.
using System;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.VFX.Cel;

namespace TW.Tests
{
    public sealed class AcidBeamTests
    {
        static readonly Vector3 Nozzle = new Vector3(-13f, 1.2f, -3f), Target = new Vector3(0f, 1.3f, 0f);

        GameObject holder;
        CelFxRenderer renderer;

        void CreateRenderer()
        {
            SceneStatics.ResetSession();
            holder = new GameObject("AcidBeamTests") { hideFlags = HideFlags.HideAndDontSave };
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
        public void The_Beam_Is_A_Straight_Line_From_The_Nozzle_To_The_Target()
        {
            float flight = AcidBeamRecipe.FlightOf(Nozzle, Target);
            Assert.That(flight, Is.EqualTo((Target - Nozzle).magnitude / AcidBeamRecipe.Speed).Within(1e-4f));
            Assert.That((AcidBeamRecipe.Along(Nozzle, Target, 0f) - Nozzle).magnitude, Is.LessThan(1e-4f));
            Assert.That((AcidBeamRecipe.Along(Nozzle, Target, flight) - Target).magnitude, Is.LessThan(1e-3f));
            Vector3 half = AcidBeamRecipe.Along(Nozzle, Target, flight * 0.5f);
            Assert.That((half - (Nozzle + Target) * 0.5f).magnitude, Is.LessThan(1e-3f), "the clip's beam is a straight line, not an arc");
        }

        [Test]
        public void A_Close_Beam_Has_Every_Layer_And_Fits_Half_Its_Pool()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            AcidBeamRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, 1917u);
            int kind = renderer.KindId(AcidBeamKind.KindName);
            Assert.That(kind, Is.GreaterThanOrEqualTo(0));
            Assert.That(renderer.SpawnedOf(kind), Is.GreaterThan(400), "the beam, blobs, beads, sparks, leaves, starbursts and smoke");
            Assert.That(renderer.SpawnedOf(kind), Is.LessThanOrEqualTo(renderer.CapacityOf(kind) / 2),
                "everything is queued up front: two beams at once must not overwrite each other's first");
        }

        [Test]
        public void A_Far_Beam_Keeps_Its_Big_Shapes_And_Drops_Most_Pieces()
        {
            CreateRenderer();
            CelFx.ZoomOverride = 240f;
            AcidBeamRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, 1917u);
            int kind = renderer.KindId(AcidBeamKind.KindName);
            int far = renderer.SpawnedOf(kind);   // SpawnedOf counts every record ever spawned
            renderer.Clear();
            CelFx.ZoomOverride = CelFx.MidZoom;
            AcidBeamRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, 1917u);
            int mid = renderer.SpawnedOf(kind) - far;
            renderer.Clear();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            AcidBeamRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, 1917u);
            int close = renderer.SpawnedOf(kind) - far - mid;
            Assert.That(far, Is.GreaterThanOrEqualTo(5), "the beam, the puddle, its lights and a little smoke");
            Assert.That(mid, Is.GreaterThan(far));
            Assert.That(close, Is.GreaterThan(mid * 2));
        }

        [Test]
        public void The_Beam_Draws_While_It_Runs_And_Leaves_Nothing_Behind()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            AcidBeamRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, 1917u);
            renderer.Submit(101.5f);
            Assert.That(renderer.DrawCalls, Is.EqualTo(1), "one pool, one draw");
            renderer.Submit(108f);
            Assert.That(renderer.DrawCalls, Is.Zero, "the puddle dries and the smoke clears: nothing stays after the clip's eight seconds");
        }

        [Test]
        public void The_Lens_Is_Handed_Over_And_Put_Back_With_The_Session()
        {
            AcidBeamRecipe.Lens(0.4f, 0.5f);
            Vector4 lens = Shader.GetGlobalVector("_TWAcidBeamLens");
            Assert.That(lens.x, Is.EqualTo(1.2f).Within(1e-4f));
            Assert.That(lens.y, Is.EqualTo(2.5f).Within(1e-4f));
            Assert.That(SceneStatics.Registered, Does.Contain(nameof(AcidBeamRecipe)));
            SceneStatics.ResetSession();
            Assert.That(Shader.GetGlobalVector("_TWAcidBeamLens").x, Is.Zero, "no vignette is assumed once the session is over");
        }

        [Test]
        public void A_Warmed_Play_Allocates_Nothing()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            uint seed = 0;
            long bytes = Allocated(() => AcidBeamRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, seed++), 20);
            Assert.That(bytes, Is.Zero);
        }
    }
}
