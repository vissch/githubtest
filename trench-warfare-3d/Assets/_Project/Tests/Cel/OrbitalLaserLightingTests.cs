// Phase: CelFx orbital laser reference match (2026-11-14) - diffuse-light lifecycle, fixed capacity and emission allocations.
using System;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.Profiling;
using TW.Presentation;
using TW.Presentation.VFX.Cel;

namespace TW.Tests
{
    public sealed class OrbitalLaserLightingTests
    {
        GameObject holder;
        CelFxRenderer renderer;

        [SetUp]
        public void SetUp() => OrbitalLaserLighting.ResetSession();

        [TearDown]
        public void TearDown()
        {
            if (renderer != null) renderer.Shutdown();
            if (holder != null) UnityEngine.Object.DestroyImmediate(holder);
            OrbitalLaserLighting.ResetSession();
        }

        void CreateRenderer()
        {
            holder = new GameObject("OrbitalLaserLightingTests") { hideFlags = HideFlags.HideAndDontSave };
            renderer = holder.AddComponent<CelFxRenderer>();
            renderer.Init();
        }

        // Unity's Boehm runtime can return a constant zero from GC.GetAllocatedBytesForCurrentThread.
        // Count the real profiler allocation samples, as TW.Perf.AllocProbe does, without a new assembly reference.
        internal static int CountAllocations(Action action)
        {
            var recorder = Recorder.Get("GC.Alloc");
            Assert.That(recorder != null && recorder.isValid, Is.True, "GC.Alloc recorder must be available for an allocation verdict");
            recorder.FilterToCurrentThread();
            recorder.enabled = false;
            try
            {
                recorder.enabled = true;
                action();
                recorder.enabled = false;
                return recorder.sampleBlockCount;
            }
            finally { recorder.enabled = false; }
        }

        [Test]
        public void Allocation_Recorder_Detects_A_Known_Allocation()
        {
            byte[] keep = null;
            Action allocate = () => keep = new byte[4096];
            allocate();
            int allocated = CountAllocations(allocate);
            GC.KeepAlive(keep);
            Assert.That(allocated, Is.GreaterThanOrEqualTo(1), "the instrument must detect real allocations");
        }

        [Test]
        public void Session_Reset_Disables_The_Light_Path()
        {
            OrbitalLaserLighting.Add(Vector3.zero, 10f, 3.4f, Color.cyan, 6f);
            Assert.That(Shader.GetGlobalFloat("_OrbitalLightCount"), Is.EqualTo(1f));
            Assert.That(SceneStatics.Registered, Does.Contain(nameof(OrbitalLaserLighting)));
            SceneStatics.ResetSession();
            Assert.That(Shader.GetGlobalFloat("_OrbitalLightCount"), Is.Zero,
                "an inactive orbital effect must add no light to any surface");
        }

        [Test]
        public void A_Full_Light_Pool_Replaces_The_Oldest_Record()
        {
            for (int i = 0; i < OrbitalLaserLighting.Capacity + 2; i++)
                OrbitalLaserLighting.Add(new Vector3(i, 2f, 3f), 10f + i, 3.4f, Color.cyan, 6f);
            var positions = Shader.GetGlobalVectorArray("_OrbitalLightPositions");
            var timing = Shader.GetGlobalVectorArray("_OrbitalLightTiming");
            Assert.That(Shader.GetGlobalFloat("_OrbitalLightCount"), Is.EqualTo(OrbitalLaserLighting.Capacity));
            Assert.That(positions.Length, Is.EqualTo(OrbitalLaserLighting.Capacity));
            Assert.That(positions[0].x, Is.EqualTo(2f), "the two oldest lights were replaced");
            Assert.That(positions[6].x, Is.EqualTo(8f));
            Assert.That(positions[7].x, Is.EqualTo(9f));
            Assert.That(timing[0].x, Is.EqualTo(12f), "other overlapping effects retain their own clock");
            Assert.That(timing[7].x, Is.EqualTo(19f));
        }

        [TestCase(false)]
        [TestCase(true)]
        public void Renderer_Clear_Or_Shutdown_Drops_The_Light(bool shutdown)
        {
            CreateRenderer();
            OrbitalLaserLighting.Add(Vector3.zero, 10f, 3.4f, Color.cyan, 6f);
            if (shutdown) renderer.Shutdown(); else renderer.Clear();
            Assert.That(Shader.GetGlobalFloat("_OrbitalLightCount"), Is.Zero,
                "removing the effect must also remove its illumination");
        }

        [Test]
        public void The_Renderer_Disable_Callback_Clears_A_Live_Light()
        {
            CreateRenderer();
            OrbitalLaserLighting.Add(Vector3.zero, 10f, 3.4f, Color.cyan, 6f);
            renderer.enabled = false;
            // This is an EditMode fixture; the runtime renderer deliberately does not ExecuteAlways.
            // Invoke the actual callback directly: SendMessage refuses disabled runtime behaviours in EditMode.
            var disable = typeof(CelFxRenderer).GetMethod("OnDisable",
                System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
            Assert.That(disable, Is.Not.Null, "the renderer must retain its lifecycle cleanup callback");
            disable.Invoke(renderer, null);
            Assert.That(Shader.GetGlobalFloat("_OrbitalLightCount"), Is.Zero);
        }

        [Test]
        public void Expired_Lights_Do_Not_Return_When_The_Render_Clock_Rewinds()
        {
            CreateRenderer();
            OrbitalLaserLighting.Add(Vector3.zero, 10f, 3.4f, Color.cyan, 6f);
            renderer.Submit(12f);
            Assert.That(Shader.GetGlobalFloat("_OrbitalLightCount"), Is.EqualTo(1f));
            renderer.Submit(14f);
            Assert.That(Shader.GetGlobalFloat("_OrbitalLightCount"), Is.Zero);
            renderer.Submit(11f);
            Assert.That(Shader.GetGlobalFloat("_OrbitalLightCount"), Is.Zero);
        }

        [Test]
        public void Expiry_Preserves_The_Other_Overlapping_Lights()
        {
            OrbitalLaserLighting.Add(Vector3.zero, 10f, 1f, Color.red, 6f);
            OrbitalLaserLighting.Add(Vector3.right, 10f, 3f, Color.cyan, 7f);
            OrbitalLaserLighting.Prune(11.5f);
            Assert.That(Shader.GetGlobalFloat("_OrbitalLightCount"), Is.EqualTo(1f));
            var positions = Shader.GetGlobalVectorArray("_OrbitalLightPositions");
            var colours = Shader.GetGlobalVectorArray("_OrbitalLightColours");
            Assert.That(positions[0].x, Is.EqualTo(1f));
            Assert.That(positions[0].w, Is.EqualTo(7f));
            Assert.That(colours[0], Is.EqualTo(new Vector4(0f, 1f, 1f, 0f)));
        }

        [Test]
        public void Warmed_Light_Emission_Allocates_Nothing()
        {
            Action emit = () =>
            {
                for (int i = 0; i < 100; i++)
                    OrbitalLaserLighting.Add(Vector3.zero, 10f + i, 3.4f, Color.cyan, 6f);
            };
            emit();
            Assert.That(CountAllocations(emit), Is.Zero);
        }

        [Test]
        public void Warmed_Expiry_Maintenance_Allocates_Nothing()
        {
            Action emitAndExpire = () =>
            {
                for (int i = 0; i < 100; i++)
                {
                    OrbitalLaserLighting.Add(Vector3.zero, 10f + i, 1f, Color.cyan, 6f);
                    OrbitalLaserLighting.Prune(12f + i);
                }
            };
            emitAndExpire();
            Assert.That(CountAllocations(emitAndExpire), Is.Zero);
        }
    }
}
