// Phase: CelFx ice_ball (2026-11-15) - the ball's flight to where it holds, the pool's capacity, the far
// simplification, that the effect leaves nothing behind, the lens handed over and put back, and the allocation
// guard. Adapted from the fireball's tests to the ice ball's own parts.
using System;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.VFX.Cel;

namespace TW.Tests
{
    public sealed class IceBallTests
    {
        static readonly Vector3 Nozzle = new Vector3(-9f, 6f, -8f), Target = new Vector3(0f, 1.3f, 0f);

        GameObject holder;
        CelFxRenderer renderer;

        void CreateRenderer()
        {
            SceneStatics.ResetSession();
            holder = new GameObject("IceBallTests") { hideFlags = HideFlags.HideAndDontSave };
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
        public void The_Ball_Flies_Nearly_Straight_To_Where_It_Holds_Short_Of_The_Target()
        {
            Vector3 hold = IceBallRecipe.HoldSpot(Nozzle, Target);
            Assert.That((IceBallRecipe.Along(Nozzle, hold, 0f) - Nozzle).magnitude, Is.LessThan(1e-4f));
            Assert.That((IceBallRecipe.Along(Nozzle, hold, IceBallRecipe.Flight) - hold).magnitude, Is.LessThan(1e-3f));
            Vector3 flat = Target - hold;
            flat.y = 0f;
            Assert.That(flat.magnitude, Is.EqualTo(1.7f).Within(1e-3f), "it hangs 1.7 m short of the target, on its own line of flight");
            Vector3 half = IceBallRecipe.Along(Nozzle, hold, IceBallRecipe.Flight * 0.5f);
            Assert.That(half.y, Is.GreaterThan((Nozzle.y + hold.y) * 0.5f));
            Assert.That(half.y, Is.LessThan((Nozzle.y + hold.y) * 0.5f + 0.25f), "the clip's ball flies nearly straight");
            Assert.That(IceBallRecipe.Hit, Is.EqualTo(IceBallRecipe.Flight + IceBallRecipe.Hold + IceBallRecipe.Lunge).Within(1e-5f));
        }

        [Test]
        public void A_Close_Ball_Has_Every_Layer_And_Fits_Its_Pool()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            IceBallRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, 1917u);
            int kind = renderer.KindId(IceBallKind.KindName);
            Assert.That(kind, Is.GreaterThanOrEqualTo(0));
            Assert.That(renderer.SpawnedOf(kind), Is.GreaterThan(120), "the lights, the swirl, the comet, its chips, the crystal, the mist, the shards and the glints");
            Assert.That(renderer.SpawnedOf(kind), Is.LessThanOrEqualTo(renderer.CapacityOf(kind) / 2),
                "everything is queued up front: two balls at once must not overwrite each other's first");
        }

        [Test]
        public void A_Far_Ball_Is_The_Comet_The_Crystal_The_Mist_The_Swirl_And_Its_Light()
        {
            CreateRenderer();
            CelFx.ZoomOverride = 240f;
            IceBallRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, 1917u);
            int kind = renderer.KindId(IceBallKind.KindName);
            int far = renderer.SpawnedOf(kind);   // SpawnedOf counts every record ever spawned
            // three ground lights, the swirl, the comet (ball and tail are one strip), the crystal and the mist
            // (one card each, the mist with fewer lobes this far off). No chips, no shards, no glints.
            Assert.That(far, Is.InRange(5, 9), "three ground lights, the swirl, the comet, the crystal and the mist; no chips, shards or glints");
            renderer.Clear();
            CelFx.ZoomOverride = CelFx.MidZoom;
            IceBallRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, 1917u);
            int mid = renderer.SpawnedOf(kind) - far;
            renderer.Clear();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            IceBallRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, 1917u);
            int close = renderer.SpawnedOf(kind) - far - mid;
            Assert.That(mid, Is.GreaterThan(far));
            Assert.That(close, Is.GreaterThan(mid * 2));
        }

        [Test]
        public void The_Ball_Draws_While_It_Runs_And_Leaves_Nothing_Behind()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            IceBallRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, 1917u);
            renderer.Submit(101.5f);
            Assert.That(renderer.DrawCalls, Is.EqualTo(1), "one pool, one draw");
            renderer.Submit(100f + IceBallRecipe.Length + 0.2f);
            Assert.That(renderer.DrawCalls, Is.Zero, "the mist thins away: nothing stays after the effect's length");
        }

        [Test]
        public void The_Lens_Is_Handed_Over_And_Put_Back_With_The_Session()
        {
            IceBallRecipe.Lens(0.4f, 0.5f);
            Vector4 lens = Shader.GetGlobalVector("_TWAcidLens");
            Assert.That(lens.x, Is.EqualTo(1.2f).Within(1e-4f));
            Assert.That(lens.y, Is.EqualTo(2.5f).Within(1e-4f));
            Assert.That(SceneStatics.Registered, Does.Contain(nameof(IceBallRecipe)));
            SceneStatics.ResetSession();
            Assert.That(Shader.GetGlobalVector("_TWAcidLens").x, Is.Zero, "no vignette is assumed once the session is over");
        }

        [Test]
        public void A_Warmed_Play_Allocates_Nothing()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            uint seed = 0;
            long bytes = Allocated(() => IceBallRecipe.Play(Nozzle, Target, Vector3.zero, Vector3.right, seed++), 20);
            Assert.That(bytes, Is.Zero);
        }
    }
}
