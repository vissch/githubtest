// Phase: CelFx swirling rockets (2026-11-15) - the paths read off the clip (four strike the target, two fly into
// another rocket's flame, none jumps), the pool's capacity, the LOD tiers, that the effect leaves nothing behind, the
// lens handed over and put back, and the allocation guard. After FireballTests.
using System;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.VFX.Cel;

namespace TW.Tests
{
    public sealed class SwirlingRocketsTests
    {
        static readonly Vector3 Target = new Vector3(0f, 1.3f, 0f);

        GameObject holder;
        CelFxRenderer renderer;

        void CreateRenderer()
        {
            SceneStatics.ResetSession();
            holder = new GameObject("SwirlingRocketsTests") { hideFlags = HideFlags.HideAndDontSave };
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
        public void Four_Rockets_Strike_The_Target_And_Two_Fly_Into_Another_Ones_Flame()
        {
            int hits = 0;
            for (int j = 0; j < SwirlingRocketsRecipe.Rockets; j++)
            {
                var path = SwirlingRocketsPaths.All[j];
                Assert.That(path.P.Length, Is.EqualTo((path.Last - path.First + 1) * 3), "one point a clip frame");
                Vector3 end = SwirlingRocketsRecipe.PathPoint(Target, Vector3.right, j, path.Last);
                if (path.Hits)
                {
                    hits++;
                    Assert.That((end - Target).magnitude, Is.LessThan(1.2f), "rocket " + j + " lands on the board");
                }
                else
                {
                    Vector3 there = SwirlingRocketsRecipe.PathPoint(Target, Vector3.right, path.Partner, path.Last);
                    Assert.That((end - there).magnitude, Is.LessThan(0.6f), "rocket " + j + " ends inside its partner's flame");
                }
                for (int f = path.First + 1; f <= path.Last; f++)
                {
                    float step = (SwirlingRocketsRecipe.PathPoint(Target, Vector3.right, j, f) - SwirlingRocketsRecipe.PathPoint(Target, Vector3.right, j, f - 1)).magnitude;
                    Assert.That(step, Is.LessThan(1.3f), "rocket " + j + " jumps at frame " + f + " (the clip's fastest entry is 1.1 m a frame)");
                }
            }
            Assert.That(hits, Is.EqualTo(4));
        }

        [Test]
        public void A_Close_Play_Has_Every_Layer_And_Fits_Its_Pool()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            SwirlingRocketsRecipe.Play(Target, Vector3.zero, Vector3.right, 1917u);
            int kind = renderer.KindId(SwirlingRocketsKind.KindName);
            Assert.That(kind, Is.GreaterThanOrEqualTo(0));
            Assert.That(renderer.SpawnedOf(kind), Is.GreaterThan(700), "the trails, the bodies, the spark dashes, the burst, its embers and the lights");
            Assert.That(renderer.SpawnedOf(kind), Is.LessThanOrEqualTo(renderer.CapacityOf(kind) / 2),
                "everything is queued up front: two plays at once must not overwrite each other's first");
        }

        [Test]
        public void A_Far_Play_Is_The_Trails_The_Burst_And_The_Light()
        {
            CreateRenderer();
            CelFx.ZoomOverride = 240f;
            SwirlingRocketsRecipe.Play(Target, Vector3.zero, Vector3.right, 1917u);
            int kind = renderer.KindId(SwirlingRocketsKind.KindName);
            int far = renderer.SpawnedOf(kind);
            // a trail record a stretch of two frames (~140), four lights and the cloud; no bodies, sparks or embers
            Assert.That(far, Is.InRange(100, 200));
            renderer.Clear();
            CelFx.ZoomOverride = CelFx.MidZoom;
            SwirlingRocketsRecipe.Play(Target, Vector3.zero, Vector3.right, 1917u);
            int mid = renderer.SpawnedOf(kind) - far;
            renderer.Clear();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            SwirlingRocketsRecipe.Play(Target, Vector3.zero, Vector3.right, 1917u);
            int close = renderer.SpawnedOf(kind) - far - mid;
            Assert.That(mid, Is.GreaterThan(far));
            Assert.That(close, Is.GreaterThan(mid));
        }

        [Test]
        public void It_Draws_While_It_Runs_And_Leaves_Nothing_Behind()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            SwirlingRocketsRecipe.Play(Target, Vector3.zero, Vector3.right, 1917u);
            renderer.Submit(102.5f);
            Assert.That(renderer.DrawCalls, Is.EqualTo(1), "one pool, one draw");
            renderer.Submit(100f + SwirlingRocketsRecipe.Length + 0.6f);
            Assert.That(renderer.DrawCalls, Is.Zero, "the smoke clears and the embers go out: nothing stays");
        }

        [Test]
        public void The_Lens_Is_Handed_Over_And_Put_Back_With_The_Session()
        {
            SwirlingRocketsRecipe.Lens(0.4f, 0.5f);
            Vector4 lens = Shader.GetGlobalVector("_TWAcidLens");
            Assert.That(lens.x, Is.EqualTo(1.2f).Within(1e-4f));
            Assert.That(lens.y, Is.EqualTo(2.5f).Within(1e-4f));
            Assert.That(SceneStatics.Registered, Does.Contain(nameof(SwirlingRocketsRecipe)));
            SceneStatics.ResetSession();
            Assert.That(Shader.GetGlobalVector("_TWAcidLens").x, Is.Zero, "no vignette is assumed once the session is over");
        }

        [Test]
        public void A_Warmed_Play_Allocates_Nothing()
        {
            CreateRenderer();
            CelFx.ZoomOverride = CelFx.CloseZoom;
            uint seed = 0;
            long bytes = Allocated(() => SwirlingRocketsRecipe.Play(Target, Vector3.zero, Vector3.right, seed++), 20);
            Assert.That(bytes, Is.Zero);
        }
    }
}
