// Phase: CelFx delayed scheduling - receiver clock causality, GPU record identity and measured queue allocations.
using System;
using System.Collections.Generic;
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.VFX.Cel;

namespace TW.Tests
{
    public sealed class CelDelayedPlayTests
    {
        CelFxRenderer renderer;
        GameObject holder;
        FxTier previousQuality;

        [SetUp]
        public void SetUp()
        {
            previousQuality = FxQuality.Tier;
            FxQuality.Set(FxTier.High);
            SceneStatics.ResetSession();
            holder = new GameObject("CelDelayedPlayTests") { hideFlags = HideFlags.HideAndDontSave };
            renderer = holder.AddComponent<CelFxRenderer>();
            renderer.Init();
            CelFx.Adopt(renderer);
            CelFx.ZoomOverride = 10f;
            CelFx.Clock = 100f;
        }

        [TearDown]
        public void TearDown()
        {
            try
            {
                renderer.Shutdown();
                UnityEngine.Object.DestroyImmediate(holder);
            }
            finally
            {
                SceneStatics.ResetSession();
                FxQuality.Set(previousQuality);
            }
        }

        static CelFxRecipe TimedLight(float start = 0f) => new CelFxRecipe("delayed-light-test", 1f,
            new CelLayer { Kind = CelKinds.LightName, Start = start, Life = 0.5f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Palette = "fire" });

        [Test]
        public void Delayed_Light_Waits_For_Exact_Due_Time_Without_Moving_Clock()
        {
            var times = new List<float>();
            SceneHooks.Flash = (at, color, power, reach, life) => times.Add(CelFx.Now);
            CelFx.Play(TimedLight(0.25f), Vector3.zero, Vector3.forward, Vector3.zero, 19u, 2f);
            Assert.That(CelFx.Now, Is.EqualTo(100f));
            Assert.That(CelFx.PendingCount, Is.EqualTo(1));
            Assert.That(times, Is.Empty);
            CelFx.Clock = 102.249f; CelFx.Pump();
            Assert.That(times, Is.Empty);
            CelFx.Clock = 102.25f; CelFx.Pump();
            Assert.That(times, Is.EqualTo(new[] { 102.25f }));
            CelFx.Pump();
            Assert.That(times.Count, Is.EqualTo(1));
            TestContext.WriteLine("Delayed receiver invoked at 102.25; no invocation at 100 or 102.249.");
        }

        [Test]
        public void A_Later_Delayed_Play_Does_Not_Drain_An_Earlier_Pending_Light()
        {
            var times = new List<float>();
            SceneHooks.Flash = (at, color, power, reach, life) => times.Add(CelFx.Now);
            var recipe = TimedLight();
            CelFx.Play(recipe, Vector3.zero, Vector3.forward, Vector3.zero, 1u, 2f);
            CelFx.Play(recipe, Vector3.zero, Vector3.forward, Vector3.zero, 2u, 4f);
            Assert.That(times, Is.Empty);
            Assert.That(CelFx.PendingCount, Is.EqualTo(2));
            CelFx.Clock = 101.999f; CelFx.Pump();
            Assert.That(times, Is.Empty);
            CelFx.Clock = 102f; CelFx.Pump();
            Assert.That(times, Is.EqualTo(new[] { 102f }));
            CelFx.Clock = 104f; CelFx.Pump();
            Assert.That(times, Is.EqualTo(new[] { 102f, 104f }));
            TestContext.WriteLine("Independent receiver times: 102, 104; both remained queued at 100.");
        }

        [Test]
        public void Default_Play_Still_Fires_A_Zero_Start_Light_Immediately()
        {
            int flashes = 0;
            float observed = -1f;
            SceneHooks.Flash = (at, color, power, reach, life) => { flashes++; observed = CelFx.Now; };
            CelFx.Play(TimedLight(), Vector3.zero, Vector3.forward, Vector3.zero, 19u);
            Assert.That(flashes, Is.EqualTo(1));
            Assert.That(observed, Is.EqualTo(100f));
            Assert.That(CelFx.PendingCount, Is.EqualTo(0));
        }

        CelRecord RecordAt(int kind, int slot)
        {
            const BindingFlags flags = BindingFlags.Instance | BindingFlags.NonPublic;
            var pools = (Array)typeof(CelFxRenderer).GetField("pools", flags).GetValue(renderer);
            object pool = pools.GetValue(kind);
            int start = (int)pool.GetType().GetField("Start").GetValue(pool);
            object records = typeof(CelFxRenderer).GetField("records", flags).GetValue(renderer);
            return (CelRecord)records.GetType().GetProperty("Item").GetValue(records, new object[] { start + slot });
        }

        [Test]
        public void Delayed_Play_Preserves_The_Pinned_Clock_GPU_Record_Bytes()
        {
            var recipe = new CelFxRecipe("delayed-gpu-test", 2f, new CelLayer {
                Kind = CelKinds.LobeName, Start = 0.08f, Duration = 0.14f, Close = 3, Mid = 2, Far = 1,
                Life = 1.8f, LifeJitter = 0.04f, Shape = CelShape.Box, ShapeSize = Vector3.one,
                Aim = CelAim.Radial, Speed = 0.8f, Palette = "fire", SizeJitter = 0.2f });
            CelFx.Clock = 102f;
            CelFx.Play(recipe, Vector3.one, Vector3.forward, Vector3.up, 1917u);
            int count = renderer.SpawnedOf(CelKinds.Lobe);
            Assert.That(count, Is.GreaterThan(0), "the adopted renderer must contain real GPU records");
            Assert.That(count, Is.EqualTo(3), "High quality at zoom 10 emits all three records");
            var expected = new CelRecord[count];
            for (int i = 0; i < count; i++) expected[i] = RecordAt(CelKinds.Lobe, i);
            CelFx.Clock = 100f;
            CelFx.Play(recipe, Vector3.one, Vector3.forward, Vector3.up, 1917u, 2f);
            Assert.That(renderer.SpawnedOf(CelKinds.Lobe), Is.EqualTo(count * 2));
            for (int i = 0; i < count; i++)
            {
                var actual = RecordAt(CelKinds.Lobe, count + i);
                Assert.That(actual, Is.EqualTo(expected[i]), "all CelRecord fields including Born and Seed");
                TestContext.WriteLine("GPU record " + i + ": Born=" + actual.Born.ToString("R") + ", Seed=" + actual.Seed.ToString("R"));
            }
            Assert.That(CelFx.Now, Is.EqualTo(100f));
        }

        [Test]
        public void Delayed_Play_And_Pump_Have_Zero_Warmed_GCAlloc_Samples_With_Positive_Control()
        {
            Assert.That(TW.Perf.AllocProbe.Available, Is.True);
            byte[] keep = null;
            Assert.That(TW.Perf.AllocProbe.PerCall(() => keep = new byte[4096], 4, 20), Is.EqualTo(1));
            GC.KeepAlive(keep);
            Assert.That(TW.Perf.AllocProbe.PerCall(() => { }, 4, 20), Is.EqualTo(0));
            var recipe = TimedLight();
            int flashes = 0;
            SceneHooks.Flash = (at, color, power, reach, life) => flashes++;
            double allocations = TW.Perf.AllocProbe.PerCall(() => {
                CelFx.Clock = 100f;
                CelFx.Play(recipe, Vector3.zero, Vector3.forward, Vector3.zero, 1917u, 2f);
                CelFx.Clock = 102f;
                CelFx.Pump();
            }, 20, 200);
            Assert.That(flashes, Is.EqualTo(220));
            Assert.That(allocations, Is.EqualTo(0));
            TestContext.WriteLine("Delayed Play/Pump GC.Alloc samples per call: " + allocations + "; positive control: 1. This covers the queue and no-allocation receiver only, not whole-frame rendering.");
        }

        [Test]
        public void Artillery_Contact_Receivers_Stay_Queued_Until_Their_Shell_Arrivals()
        {
            ArtilleryRecipe.Init();
            float[] contactFrames = { 18f, 22f, 24f, 32f, 60f, 62f, 64f, 70f };
            var times = new List<float>();
            SceneHooks.Flash = (at, color, power, reach, life) => times.Add(CelFx.Now);
            for (int i = 0; i < contactFrames.Length; i++)
                ArtilleryRecipe.PlayAt(ArtilleryRecipe.Background, Vector3.zero, Vector3.forward,
                    1917u + (uint)i * 101u, contactFrames[i] / 24f - ArtilleryRecipe.LeadIn);
            Assert.That(times, Is.Empty, "preparing all eight future impacts must not light the field");
            Assert.That(CelFx.PendingCount, Is.EqualTo(8));
            for (int i = 0; i < contactFrames.Length; i++)
            {
                float due = 100f + (contactFrames[i] / 24f - ArtilleryRecipe.LeadIn);
                CelFx.Clock = due - 0.0001f; CelFx.Pump();
                Assert.That(times.Count, Is.EqualTo(i));
                CelFx.Clock = due; CelFx.Pump();
                Assert.That(times.Count, Is.EqualTo(i + 1));
                Assert.That(times[i], Is.EqualTo(due));
                TestContext.WriteLine("Artillery contact frame=" + contactFrames[i] + ", receiver clock=" + times[i].ToString("R") + "; requested shot t0=.2 fires at f5=.208333, so visual birth retains +8.333ms.");
            }
        }
    }
}
