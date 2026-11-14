// Phase: CelFx kit (2026-11-09) - the kit's rules held by tests: a spawn and a frame allocate nothing, a recipe never
// spends more going out, every cel shader is found and kept for a build, every draw goes through FrameBudget, and the
// statics are put back when Play ends.
using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text.RegularExpressions;
using NUnit.Framework;
using UnityEditor;
using UnityEngine;
using UnityEngine.TestTools;
using TW.Presentation;
using TW.Presentation.VFX;
using TW.Presentation.VFX.Cel;

namespace TW.Tests
{
    public sealed class FlamethrowerCelFxTests
    {
        const string CelCode = "Assets/_Project/Presentation/VFX/Cel";
        CelFxRenderer renderer;
        GameObject holder;

        [SetUp]
        public void SetUp()
        {
            SceneStatics.ResetSession();
            holder = new GameObject("CelFxTests") { hideFlags = HideFlags.HideAndDontSave };
            renderer = holder.AddComponent<CelFxRenderer>();
            renderer.Init();
            CelFx.ZoomOverride = 10f;
            CelFx.Clock = 100f;
        }

        [TearDown]
        public void TearDown()
        {
            if (renderer != null) renderer.Shutdown();
            if (holder != null) UnityEngine.Object.DestroyImmediate(holder);
            SceneStatics.ResetSession();
        }

        static CelRecord AnyRecord(float born) => new CelRecord
        {
            Pos = Vector3.one, Born = born, Vel = Vector3.up, Life = 1f, Axis = Vector3.up, Seed = 0.5f,
            Size = new Vector4(1f, 1f, 1f, 0f), Erode = new Vector4(0f, 0.5f, 1f, 0.1f), Tone = new Vector4(CelPalette.Fire, 2f, 1f, 2f),
        };

        static long Allocated(Action act, int times)
        {
            act();   // warm: first-call JIT and lazy statics
            long before = GC.GetAllocatedBytesForCurrentThread();
            for (int i = 0; i < times; i++) act();
            return GC.GetAllocatedBytesForCurrentThread() - before;
        }

        [Test]
        public void The_Record_Is_128_Bytes_As_The_Shader_Reads_It()
        {
            Assert.That(Marshal.SizeOf<CelRecord>(), Is.EqualTo(CelRecord.Bytes));
            string hlsl = File.ReadAllText("Assets/_Project/Shaders/Cel/CelFx.hlsl");
            Assert.That(hlsl, Does.Contain("struct CelRecord"), "CelFx.hlsl holds the record's layout");
        }

        [Test]
        public void A_Spawn_Allocates_Nothing()
        {
            var r = AnyRecord(100f);
            long bytes = Allocated(() => renderer.Spawn(CelKinds.Lobe, r), 2000);
            Assert.That(bytes, Is.EqualTo(0), "Spawn copies a struct into the ring; " + bytes + " B for 2000 spawns");
        }

        CelRecord StoredRecord(int kind, int offset)
        {
            const BindingFlags flags = BindingFlags.Instance | BindingFlags.NonPublic;
            var pools = (Array)typeof(CelFxRenderer).GetField("pools", flags).GetValue(renderer);
            var pool = pools.GetValue(kind);
            int start = (int)pool.GetType().GetField("Start").GetValue(pool);
            object records = typeof(CelFxRenderer).GetField("records", flags).GetValue(renderer);
            return (CelRecord)records.GetType().GetProperty("Item").GetValue(records, new object[] { start + offset });
        }

        sealed class RangeStaleZoom : MonoBehaviour, IZoomSource
        {
            public float CurrentZoom => 30f;
        }

        [TestCase(120f)]
        [TestCase(240f)]
        public void A_Far_Flamethrower_Range_Shot_Uses_Its_Declared_Zoom_At_Spawn(float shotZoom)
        {
            var switchedOff = new List<Camera>();
            var keepQuality = FxQuality.Tier;
            GameObject eye = null;
            try
            {
                // Reproduce the restored beach camera after CaptureRig finishes the pre-shot still.
                foreach (var other in Camera.allCameras)
                    if (other.CompareTag("MainCamera")) { other.enabled = false; switchedOff.Add(other); }
                eye = new GameObject("Range stale Mid camera", typeof(Camera));
                eye.hideFlags = HideFlags.HideAndDontSave;
                eye.tag = "MainCamera";
                eye.AddComponent<RangeStaleZoom>();
                ViewEye.Invalidate();
                Assert.That(ViewEye.Zoom, Is.EqualTo(30f), "prime the cached Mid zoom before the Far shot");
                CelFx.ZoomOverride = float.NaN;
                FxQuality.Set(FxTier.High);
                CelFx.Adopt(renderer);

                CelRange.Begin(Vector3.zero, Vector3.up, new Vector3(0f, 1f, 12f), Vector3.forward * 12f);
                var shot = CelRange.Fire("flamethrower", 0f);
                Assert.That(shot, Is.Not.Null);
                shot.Lod = RangeLod.Far;
                shot.Zoom = shotZoom;
                Assert.That(CelRange.Tick(), Is.EqualTo(1));

                Assert.That(ViewEye.Zoom, Is.EqualTo(30f), "the effect must not change the restored camera");
                Assert.That(CelFx.Zoom, Is.EqualTo(shotZoom), "the shot zoom controls both counts and shader LOD");
                int flame = renderer.KindId("FlameJet");
                Assert.That(renderer.SpawnedOf(flame), Is.EqualTo(1), "Far retains one jet and no Mid-only target curls");
                Assert.That(StoredRecord(flame, 0).Extra.w, Is.GreaterThan(0f), "the retained record is the main jet");
                Assert.That(renderer.SpawnedOf(renderer.KindId("FlameSmoke")), Is.EqualTo(1));
                Assert.That(renderer.SpawnedOf(renderer.KindId("FlameWash")), Is.EqualTo(2));
                int spawned = 0;
                for (int kind = 0; kind < renderer.KindCount; kind++) spawned += renderer.SpawnedOf(kind);
                Assert.That(spawned, Is.EqualTo(4), "the authored Far budget excludes sparks, ribbons and detached puffs");
            }
            finally
            {
                CelRange.Clear();
                FxQuality.Set(keepQuality);
                if (eye != null) UnityEngine.Object.DestroyImmediate(eye);
                foreach (var other in switchedOff) if (other != null) other.enabled = true;
                ViewEye.Invalidate();
            }
        }

        [TestCase(1f)]
        [TestCase(-1f)]
        [TestCase(2f)]
        [TestCase(-1.1f)]
        public void Two_Flame_Emitters_Keep_Their_Own_Frame_And_Clock(float mode)
        {
            CelFx.Adopt(renderer);
            var recipe = new CelFxRecipe("emitter isolation", 2f, new CelLayer
            {
                Kind = "FlameJet", Close = 1, Mid = 1, Far = 1, LifeJitter = 0f,
                Palette = "white", Extra = new Vector4(0f, 0f, 0f, mode), Buoyancy = 0.7f,
                Offset = new Vector3(0.2f, 0.3f, 0.4f),
            });
            int kind = renderer.KindId("FlameJet");
            var originA = new Vector3(-4f, 1f, 2f);
            var targetA = new Vector3(7f, 3f, 2f);
            var originB = new Vector3(40f, 2f, -30f);
            var targetB = new Vector3(40f, 5f, -18f);
            CelFx.Play(recipe, originA, targetA - originA, targetA, 1917);
            var first = StoredRecord(kind, 0);
            CelFx.Clock = 100.5f;
            CelFx.Play(recipe, originB, targetB - originB, targetB, 1918);
            var second = StoredRecord(kind, 1);
            Assert.That(StoredRecord(kind, 0), Is.EqualTo(first), "a later emitter must not rewrite an active record");
            Assert.That(first.Axis, Is.EqualTo(targetA));
            Assert.That(first.Motion, Is.EqualTo(new Vector4(originA.x, originA.y, originA.z, 0.7f)));
            Assert.That(first.Tone.z, Is.EqualTo(100f));
            Assert.That(second.Axis, Is.EqualTo(targetB));
            Assert.That(second.Motion, Is.EqualTo(new Vector4(originB.x, originB.y, originB.z, 0.7f)));
            Assert.That(second.Tone.z, Is.EqualTo(100.5f));
        }

        [UnityTest]
        public IEnumerator A_Second_Emitter_Does_Not_Recolor_An_Already_Rendered_Jet()
            => RenderedEmitterIsolation(false);

        [UnityTest]
        public IEnumerator A_Second_Emitter_Does_Not_Recolor_An_Already_Rendered_Travelling_Front()
            => RenderedEmitterIsolation(true);

        [UnityTest]
        public IEnumerator A_Second_Emitter_Does_Not_Recolor_An_Independent_Flame_Stream()
            => RenderedEmitterIsolation(false, independent: true);

        [UnityTest]
        public IEnumerator A_Second_Emitter_Does_Not_Recolor_Overlapping_Independent_Flame_Tongues()
            => RenderedEmitterIsolation(false, independent: true, tonguesOnly: true);

        IEnumerator RenderedEmitterIsolation(bool frontOnly, bool independent = false, bool tonguesOnly = false)
        {
            FlamethrowerRecipe.AddPalettes();
            CelFx.Adopt(renderer);
            var recipe = new CelFxRecipe("rendered emitter isolation", 3f,
                new CelLayer { Kind = "FlameJet", Close = 1, Mid = 1, Far = 1, Life = 3f, LifeJitter = 0f,
                    Aim = CelAim.Target, Spread = 0f, Size = new CelCurve(0.3f, 2.3f, 1.4f), SizeJitter = 0f,
                    Erode = new CelCurve(0.8f, 0.3f, 0.85f), Palette = FlamethrowerRecipe.Flame, Core = 3.5f,
                    Extra = new Vector4(9f, 2.2f, 0.5f, 1f) },
                new CelLayer { Kind = "FlameJet", Close = 1, Mid = 1, Far = 1, Life = 1.2f, LifeJitter = 0f,
                    Aim = CelAim.Dir, Speed = 8f, SpeedJitter = 0f, Spread = 0f, Buoyancy = 0.6f, Stretch = 0.5f,
                    Size = new CelCurve(1.8f, 5.5f, 0.8f), SizeJitter = 0f, Erode = new CelCurve(0.75f, 0.3f, 1f),
                    Palette = FlamethrowerRecipe.Flame, Core = 3.5f, Extra = new Vector4(6f, 0.8f, 0.5f, -1f) },
                new CelLayer { Kind = "FlameJet", Close = 1, Mid = 1, Far = 1, Life = 0.85f, LifeJitter = 0f,
                    Offset = new Vector3(0f, 0.8f, 0f), Aim = CelAim.Dir, Speed = 12f, SpeedJitter = 0f, Spread = 0f,
                    Size = new CelCurve(0.3f, 1.5f, 0.5f), SizeJitter = 0f, Erode = new CelCurve(0f, 0.3f, 1f),
                    Palette = FlamethrowerRecipe.Flame, Core = 3.5f, Extra = new Vector4(0.9f, 3f, 0.8f, -3f) });
            if (independent)
            {
                // Exercise the sustained modes after the main handoff, with two overlapping
                // tongues born at different times. A tongues-only case prevents the main
                // stream from hiding missing or incorrectly clipped independent geometry.
                recipe.Layers[0].Extra = new Vector4(9f, 2.2f, 0.5f, 2f);
                var tongues = recipe.Layers[1];
                tongues.Extra = new Vector4(6f, 0.8f, 0.5f, -1.1f);
                tongues.Start = 0.2f; tongues.Duration = 0.4f; tongues.Life = 1.5f;
                tongues.Close = 2; tongues.Mid = 2; tongues.Far = 2;
                recipe.Layers.RemoveAt(2);
                if (tonguesOnly) recipe.Layers.RemoveAt(0);
            }
            // Also render the front alone, so the body cannot hide a missing or broken front in this assertion.
            if (frontOnly) recipe.Layers.RemoveRange(0, 2);
            float captureClock = independent ? 101.1f : 100.6f;
            var cameraObject = new GameObject("FlameEmitterIsolationCamera") { hideFlags = HideFlags.HideAndDontSave };
            var camera = cameraObject.AddComponent<Camera>();
            camera.enabled = false;
            camera.transform.position = new Vector3(2004f, 3f, 1982f);
            camera.transform.LookAt(new Vector3(2004f, 1.5f, 2000f));
            camera.fieldOfView = 40f; camera.nearClipPlane = 0.1f; camera.farClipPlane = 40f;
            camera.clearFlags = CameraClearFlags.SolidColor; camera.backgroundColor = Color.black;
            camera.allowHDR = false; camera.allowMSAA = false;
            var rt = new RenderTexture(256, 256, 24, RenderTextureFormat.ARGB32, RenderTextureReadWrite.Linear);
            var pixels = new Texture2D(256, 256, TextureFormat.RGB24, false, true);
            var previousTarget = RenderTexture.active;
            bool wasEnabled = renderer.enabled;
            // This test owns submission. Automatic LateUpdate and a second manual
            // Submit in the same frame would enqueue duplicate indirect draws;
            // additive halo passes make those duplicates visible.
            renderer.enabled = false;
            try
            {
                rt.Create(); camera.targetTexture = rt;
                Color32[] Capture()
                {
                    renderer.Submit(CelFx.Clock);
                    camera.Render();
                    RenderTexture.active = rt;
                    pixels.ReadPixels(new Rect(0, 0, 256, 256), 0, 0);
                    pixels.Apply();
                    return pixels.GetPixels32();
                }
                FlamethrowerRecipe.Play(recipe, new Vector3(2000f, 1f, 2000f), new Vector3(2008f, 2f, 2000f), 1917);
                CelFx.Clock = captureClock;
                yield return null;
                var before = Capture();
                yield return null;
                // A no-op frame verifies that the harness itself does not change
                // the image before introducing another emitter. Keep exact equality.
                var unchanged = Capture();
                yield return null;
                // Reconstruct the second emitter's delayed start, then compare at the
                // same fixed clock. Both new-mode emitters have live delayed tongues here.
                CelFx.Clock = independent ? 100.4f : 101f;
                FlamethrowerRecipe.Play(recipe, new Vector3(2200f, 1f, 2050f), new Vector3(2200f, 3f, 2062f), 1918);
                CelFx.Clock = captureClock;
                var after = Capture();
                // Finish the submitted frame before destroying its GPU resources,
                // including when an assertion below fails.
                yield return null;
                int visible = 0, controlChanged = 0, changed = 0;
                for (int i = 0; i < before.Length; i++)
                {
                    if (before[i].r + before[i].g + before[i].b > 30) visible++;
                    if (!before[i].Equals(unchanged[i])) controlChanged++;
                    if (!before[i].Equals(after[i])) changed++;
                }
                Assert.That(visible, Is.GreaterThan(50), tonguesOnly ? "overlapping independent tongues must render without a main stream"
                    : frontOnly ? "the travelling front must be visible" : "the comparison must contain a visible jet");
                Assert.That(controlChanged, Is.Zero, "an unchanged emitter and fixed clock must render identically on separate frames");
                Assert.That(changed, Is.Zero, "an offscreen emitter must not alter the active stream, independent tongues, or travelling front");
            }
            finally
            {
                camera.targetTexture = null; RenderTexture.active = previousTarget;
                UnityEngine.Object.DestroyImmediate(cameraObject);
                UnityEngine.Object.DestroyImmediate(pixels);
                rt.Release(); UnityEngine.Object.DestroyImmediate(rt);
                renderer.enabled = wasEnabled;
            }
        }

        [Test]
        public void Preparing_Flame_Records_Allocates_Nothing()
        {
            int kind = renderer.KindId("FlameJet");
            var record = AnyRecord(100f);
            record.Extra.w = -1f;
            long bytes = Allocated(() => renderer.Spawn(kind, record, Vector3.zero, Vector3.right * 10f, 100f), 2000);
            Assert.That(bytes, Is.EqualTo(0), "emitter packing is a value-copy operation at spawn");
        }

        [Test]
        public void Preparing_A_Travelling_Front_Binds_The_Emitter_And_Preserves_Its_Travel()
        {
            var record = AnyRecord(102f);
            record.Pos = new Vector3(3f, 4f, 5f);
            record.Vel = new Vector3(9f, 2f, 1f);
            record.Life = 0.85f;
            record.Motion = new Vector4(0f, 0f, 0f, 0.7f);
            record.Extra.w = -3f;
            var origin = new Vector3(-4f, 1f, 2f);
            var target = new Vector3(7f, 3f, 2f);
            var expected = record;
            expected.Axis = target;
            expected.Motion = new Vector4(origin.x, origin.y, origin.z, record.Motion.w);
            expected.Tone.z = 100f;
            int kind = renderer.KindId("FlameJet");
            renderer.Spawn(kind, record, origin, target, 100f);
            Assert.That(StoredRecord(kind, 0), Is.EqualTo(expected),
                "the front keeps Pos, Vel, Born, Life and buoyancy while binding its own emitter frame");
        }

        [TestCase(-2f)]
        [TestCase(-2.1f)]
        public void Preparation_Preserves_Fireball_Motion_And_Default_Kinds(float mode)
        {
            var record = AnyRecord(100f);
            record.Extra.w = mode;
            record.Motion = new Vector4(0.4f, 0.5f, 0.6f, 0.7f);
            int flame = renderer.KindId("FlameJet");
            renderer.Spawn(flame, record, Vector3.one * 5f, Vector3.one * 10f, 101f);
            renderer.Spawn(CelKinds.Lobe, record, Vector3.one * 5f, Vector3.one * 10f, 101f);
            Assert.That(StoredRecord(flame, 0), Is.EqualTo(record), "fireball discs still use gravity, drag and spin");
            Assert.That(StoredRecord(CelKinds.Lobe, 0), Is.EqualTo(record), "the default preparation hook is inert");
            record.Extra.w = -1f;
            renderer.Spawn(flame, record);
            Assert.That(StoredRecord(flame, 1), Is.EqualTo(record), "direct Spawn preserves caller-prepared data");
        }

        [TestCase("FlameSmoke", -1f)]
        [TestCase("FlameJet", -2.2f)]
        public void ImpactFire_Preparation_Keeps_Independent_Emitter_Clocks_Heights_And_Motion(string kindName, float mode)
        {
            CelFx.Adopt(renderer);
            var recipe = new CelFxRecipe("impact fire phase isolation", 8f, new CelLayer
            {
                Kind = kindName, Start = 1.4f, Duration = 2.1f, Close = 4, Mid = 4, Far = 4,
                Life = 3f, LifeJitter = 0f, Palette = "white", Extra = new Vector4(1.1f, kindName == "FlameJet" ? 1f : 0.12f, -0.8f, mode),
            });
            int kind = renderer.KindId(kindName);
            var firstTarget = new Vector3(0f, 3f, 10f);
            var secondTarget = new Vector3(20f, 8f, 10f);
            CelFx.Play(recipe, Vector3.zero, Vector3.forward, firstTarget, 1917);
            int firstCount = renderer.SpawnedOf(kind);
            Assert.That(firstCount, Is.GreaterThanOrEqualTo(2));
            var first = StoredRecord(kind, 0);
            Assert.That(first.Tone.z, Is.EqualTo(100f));
            Assert.That(first.Extra.z, Is.EqualTo(firstTarget.y));
            Assert.That(StoredRecord(kind, 1).Extra.z, Is.EqualTo(firstTarget.y), "different births share one impact height");
            Assert.That(StoredRecord(kind, 1).Tone.z, Is.EqualTo(100f), "different births share one emitter phase");
            Assert.That(StoredRecord(kind, 1).Born, Is.GreaterThan(first.Born));
            CelFx.Clock = 102f;
            CelFx.Play(recipe, Vector3.right * 20f, Vector3.forward, secondTarget, 1918);
            Assert.That(StoredRecord(kind, 0), Is.EqualTo(first));
            Assert.That(StoredRecord(kind, firstCount).Tone.z, Is.EqualTo(102f));
            Assert.That(StoredRecord(kind, firstCount).Extra.z, Is.EqualTo(secondTarget.y));
            Assert.That(StoredRecord(kind, firstCount + 1).Tone.z, Is.EqualTo(102f));
            Assert.That(StoredRecord(kind, firstCount + 1).Extra.z, Is.EqualTo(secondTarget.y));
            Assert.That(StoredRecord(kind, 1).Tone.z, Is.EqualTo(100f), "later emissions do not overwrite earlier clocks");
            Assert.That(StoredRecord(kind, 1).Extra.z, Is.EqualTo(firstTarget.y), "later emissions do not overwrite earlier heights");

            var record = AnyRecord(100f);
            record.Motion = new Vector4(0.2f, 0.3f, 0.4f, 0.5f);
            record.Axis = new Vector3(0.2f, 0.8f, 0.4f);
            record.Extra = new Vector4(0.35f, 1f, -0.8f, mode);
            var prepared = record;
            CelKind implementation = kindName == "FlameSmoke" ? (CelKind)new FlameSmokeKind() : new FlameJetKind();
            implementation.PrepareRecord(ref prepared, Vector3.zero, Vector3.one, 108f);
            var expected = record; expected.Tone.z = 108f; expected.Extra.z = 1f;
            Assert.That(prepared, Is.EqualTo(expected), "only emitter phase and impact height are packed; rotation axis, seed and physics remain intact");
        }

        [Test]
        public void FlameSmoke_Preparation_Allocates_Nothing()
        {
            int kind = renderer.KindId("FlameSmoke");
            var record = AnyRecord(100f); record.Extra.w = -1f;
            Assert.That(Allocated(() => renderer.Spawn(kind, record, Vector3.zero, Vector3.one, 100f), 2000), Is.Zero);
        }

        [Test]
        public void TimeScaled_Impact_Fire_Composes_Local_Scales_And_Phase_Without_Changing_Other_Modes()
        {
            var source = new CelFxRecipe("impact fire phase scaling", 8f,
                new CelLayer { Kind = "FlameSmoke", Start = 1.4f, Extra = new Vector4(1f, 0.1f, -0.8f, -1f) },
                new CelLayer { Kind = CelKinds.LobeName, Opacity = 0.7f, Extra = new Vector4(1f, 0.1f, -0.8f, 0.3f) },
                new CelLayer { Kind = "FlameJet", Start = 0.85f, Extra = new Vector4(0.35f, 1f, 0.18f, -2.2f) },
                new CelLayer { Kind = "FlameJet", Opacity = 0.7f, Extra = new Vector4(0.35f, 3f, 0.18f, -2f) },
                new CelLayer { Kind = "FlameJet", Opacity = 0.6f, Extra = new Vector4(0.55f, 3f, 0.25f, -2.1f) });
            var originalExtra = new Vector4[source.Layers.Count];
            var originalOpacity = new float[source.Layers.Count];
            var originalStart = new float[source.Layers.Count];
            for (int i = 0; i < source.Layers.Count; i++)
            {
                originalExtra[i] = source.Layers[i].Extra;
                originalOpacity[i] = source.Layers[i].Opacity;
                originalStart[i] = source.Layers[i].Start;
            }

            var scaled = FlamethrowerRecipe.TimeScaled(source, 1.2f, 0.16f);
            Assert.That(scaled.Layers[0].Extra.w, Is.EqualTo(-1.2f).Within(1e-6f));
            Assert.That(scaled.Layers[0].Opacity, Is.EqualTo(1.16f).Within(1e-6f));
            Assert.That(scaled.Layers[2].Extra.y, Is.EqualTo(1.2f).Within(1e-6f), "target support stores its local time scale separately from its mode");
            Assert.That(scaled.Layers[2].Extra.w, Is.EqualTo(-2.2f), "time scaling must not change the target mode selector");
            Assert.That(scaled.Layers[2].Opacity, Is.EqualTo(1.16f).Within(1e-6f));

            var twice = FlamethrowerRecipe.TimeScaled(scaled, 2f, 0.3f);
            Assert.That(twice.Layers[0].Extra.w, Is.EqualTo(-2.4f).Within(1e-6f));
            Assert.That(twice.Layers[0].Opacity, Is.EqualTo(1.62f).Within(1e-6f));
            Assert.That(twice.Layers[2].Extra.y, Is.EqualTo(2.4f).Within(1e-6f));
            Assert.That(twice.Layers[2].Extra.w, Is.EqualTo(-2.2f));
            Assert.That(twice.Layers[2].Opacity, Is.EqualTo(1.62f).Within(1e-6f), "nested delays compose in the same clock for smoke and target support");
            foreach (int i in new[] { 1, 3, 4 })
            {
                Assert.That(scaled.Layers[i].Extra, Is.EqualTo(originalExtra[i]), "ordinary lobe and fireball payloads retain their meanings");
                Assert.That(twice.Layers[i].Extra, Is.EqualTo(originalExtra[i]));
                Assert.That(scaled.Layers[i].Opacity, Is.EqualTo(originalOpacity[i]));
                Assert.That(twice.Layers[i].Opacity, Is.EqualTo(originalOpacity[i]));
            }
            for (int i = 0; i < source.Layers.Count; i++)
            {
                Assert.That(source.Layers[i].Extra, Is.EqualTo(originalExtra[i]), "scaling does not mutate the source payload");
                Assert.That(source.Layers[i].Opacity, Is.EqualTo(originalOpacity[i]));
                Assert.That(source.Layers[i].Start, Is.EqualTo(originalStart[i]));
            }
            foreach (int i in new[] { 0, 2 })
            {
                var record = AnyRecord(100f);
                record.Extra = twice.Layers[i].Extra;
                record.Tone.z = twice.Layers[i].Opacity;
                CelKind implementation = i == 0 ? (CelKind)new FlameSmokeKind() : new FlameJetKind();
                implementation.PrepareRecord(ref record, Vector3.zero, Vector3.one, 100f);
                Assert.That(record.Tone.z, Is.EqualTo(100.62f).Within(1e-5f));
                Assert.That(record.Extra.z, Is.EqualTo(1f), "time scaling changes phase, not world impact height");
                Assert.That(record.Extra.w, Is.EqualTo(twice.Layers[i].Extra.w));
                Assert.That(record.Extra.y, Is.EqualTo(twice.Layers[i].Extra.y));
            }
        }

        [Test]
        public void A_Frame_Allocates_Nothing()
        {
            for (int i = 0; i < 50; i++) { renderer.Spawn(CelKinds.Streak, AnyRecord(100f)); renderer.Spawn(CelKinds.Glow, AnyRecord(100f)); }
            long bytes = Allocated(() => { renderer.Spawn(CelKinds.Lobe, AnyRecord(100f)); renderer.Submit(100.2f); }, 200);
            Assert.That(bytes, Is.EqualTo(0), "Submit uploads and draws without garbage; " + bytes + " B over 200 frames");
        }

        [Test]
        public void Playing_A_Recipe_Allocates_Nothing()
        {
            CelFx.Adopt(renderer);
            long bytes = Allocated(() => CelDemo.Play(new Vector3(0f, 1f, 0f)), 50);
            Assert.That(bytes, Is.EqualTo(0), "CelFx.Play spawns from the recipe with no garbage; " + bytes + " B for 50 plays");
            CelFx.Pump();
        }

        [Test]
        public void A_Pool_Is_A_Ring_With_A_Cap()
        {
            int cap = renderer.CapacityOf(CelKinds.Glow);
            for (int i = 0; i < cap + 37; i++) renderer.Spawn(CelKinds.Glow, AnyRecord(100f));
            renderer.Submit(100f);
            Assert.That(renderer.Held, Is.EqualTo(cap), "past the cap the oldest is overwritten; the pool never grows");
        }

        [Test]
        public void A_Pool_Is_Drawn_Once_While_It_Holds_A_Live_Record_And_Not_After()
        {
            renderer.Spawn(CelKinds.Lobe, AnyRecord(100f));
            renderer.Spawn(CelKinds.Lobe, AnyRecord(100f));
            renderer.Submit(100.5f);
            Assert.That(renderer.DrawCalls, Is.EqualTo(1), "one indirect draw per kind, however many records");
            renderer.Submit(102f);
            Assert.That(renderer.DrawCalls, Is.EqualTo(0), "every record past its life: the pool is not drawn");
        }

        [Test]
        public void The_Built_In_Kinds_Keep_Their_Ids_And_Every_Name_Is_Unique()
        {
            var kinds = CelFxRenderer.FindKinds();
            Assert.That(kinds[CelKinds.Streak].Name, Is.EqualTo(CelKinds.StreakName));
            Assert.That(kinds[CelKinds.Lobe].Name, Is.EqualTo(CelKinds.LobeName));
            Assert.That(kinds[CelKinds.Glow].Name, Is.EqualTo(CelKinds.GlowName));
            var names = new HashSet<string>();
            foreach (var k in kinds) Assert.That(names.Add(k.Name), Is.True, "two kinds called " + k.Name);
            Assert.That(renderer.KindId(CelKinds.ChunksName), Is.EqualTo(CelKinds.Chunks));
            Assert.That(renderer.KindId(CelKinds.LightName), Is.EqualTo(CelKinds.Light));
        }

        /// <summary>Every recipe in the TW assemblies: a static CelFxRecipe field anywhere (the effect agents' files included).</summary>
        static List<CelFxRecipe> AllRecipes()
        {
            var list = new List<CelFxRecipe>();
            foreach (var asm in AppDomain.CurrentDomain.GetAssemblies())
            {
                if (!asm.GetName().Name.StartsWith("TW.", StringComparison.Ordinal)) continue;
                Type[] types;
                try { types = asm.GetTypes(); } catch (ReflectionTypeLoadException e) { types = e.Types; }
                foreach (var t in types)
                {
                    if (t == null || t.ContainsGenericParameters) continue;
                    foreach (var f in t.GetFields(BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic))
                        if (f.FieldType == typeof(CelFxRecipe) && f.GetValue(null) is CelFxRecipe r) list.Add(r);
                }
            }
            return list;
        }

        // Combined recipe/LOD validation lives once in CelFxTests.


        [Test]
        public void The_Lod_Blend_Follows_The_Zoom_Bands()
        {
            Assert.That(CelFx.LodBlend(7.5f), Is.EqualTo(0f), "T3 is Close");
            Assert.That(CelFx.LodBlend(16f), Is.EqualTo(0f), "T2 is Close");
            Assert.That(CelFx.LodBlend(30f), Is.EqualTo(1f), "T1 is Mid");
            Assert.That(CelFx.LodBlend(120f), Is.EqualTo(2f), "O120 is Far");
            Assert.That(CelFx.LodBlend(600f), Is.EqualTo(2f));
            Assert.That(CelFx.InkScale(2f), Is.LessThan(CelFx.InkScale(0f)), "the ink thins going out");
        }

        [Test]
        public void A_Recipe_Whose_Counts_Rise_Fails_Validation()
        {
            // the check above must be able to fail
            var wrong = new CelFxRecipe("rising", 1f, new CelLayer { Close = 2, Mid = 5, Far = 1, Palette = "fire" });
            Assert.That(wrong.Validate(renderer), Is.Not.Empty);
            Assert.That(wrong.Layers[0].CountAt(1f, 1f), Is.GreaterThan(wrong.Layers[0].CountAt(0f, 1f)), "and CountAt shows the rise");
        }

        static List<string> CelShaderNames()
        {
            var names = new SortedSet<string>();
            foreach (var file in Directory.GetFiles("Assets/_Project/Presentation/VFX", "*.cs", SearchOption.AllDirectories))
                foreach (Match m in Regex.Matches(File.ReadAllText(file), "Shader\\.Find\\(\"(TW/Cel/[^\"]+)\"\\)")) names.Add(m.Groups[1].Value);
            return new List<string>(names);
        }

        [Test]
        public void Every_Cel_Shader_Is_Found_And_Kept_For_A_Build()
        {
            var names = CelShaderNames();
            Assert.That(names, Is.SupersetOf(new[] { "TW/Cel/Streak", "TW/Cel/Lobe", "TW/Cel/Glow" }), "the scan finds the built-ins");
            var kept = new HashSet<string>();
            foreach (var guid in AssetDatabase.FindAssets("t:Material", new[] { "Assets/_Project/Resources" }))
            {
                var mat = AssetDatabase.LoadAssetAtPath<Material>(AssetDatabase.GUIDToAssetPath(guid));
                if (mat != null && mat.shader != null) kept.Add(mat.shader.name);
            }
            var missing = new List<string>();
            foreach (var n in names)
            {
                var s = Shader.Find(n);
                if (s == null) missing.Add(n + " (not found: a compile error?)");
                else if (!s.isSupported) missing.Add(n + " (not supported)");
                else if (!kept.Contains(n)) missing.Add(n + " (no keeper material in Resources/ShaderKeep)");
            }
            Assert.That(missing, Is.Empty, string.Join("\n", missing));
            foreach (var kind in CelFxRenderer.FindKinds()) Assert.That(kind.FindShader(), Is.Not.Null, kind.Name + "'s shader");
        }

        [Test]
        public void Every_Cel_Draw_Goes_Through_FrameBudget()
        {
            var direct = new Regex(@"\bGraphics\s*\.\s*(?:Render|Draw)\w*\s*\(|\.\s*(?:DrawMesh|DrawProcedural|DrawRenderer)\w*\s*\(");
            int scanned = 0;
            foreach (var file in Directory.GetFiles(CelCode, "*.cs", SearchOption.AllDirectories))
            {
                scanned++;
                string code = Regex.Replace(File.ReadAllText(file), @"//[^\n]*", "");
                Assert.That(direct.IsMatch(code), Is.False, file + " draws around FrameBudget");
            }
            Assert.That(scanned, Is.GreaterThan(4));
            Assert.That(File.ReadAllText(CelCode + "/CelFxRenderer.cs"), Does.Contain("FrameBudget.DrawIndirect("));
        }

        [Test]
        public void The_Statics_Register_And_Are_Put_Back_When_Play_Ends()
        {
            RuntimeHelpers.RunClassConstructor(typeof(CelFx).TypeHandle);
            RuntimeHelpers.RunClassConstructor(typeof(CelPalette).TypeHandle);
            Assert.That(SceneStatics.Registered, Does.Contain(nameof(CelFx)));
            Assert.That(SceneStatics.Registered, Does.Contain(nameof(CelPalette)));
            CelFx.Clock = 5f; CelFx.ZoomOverride = 50f; CelFx.Chunks = (p, a, n, s, sc, c, l, b, salt) => { };
            int v = CelPalette.Version;
            SceneStatics.ResetSession();
            Assert.That(float.IsNaN(CelFx.Clock) && float.IsNaN(CelFx.ZoomOverride), Is.True, "the clock and the zoom are unpinned");
            Assert.That(CelFx.Chunks, Is.Null, "the lent debris is dropped");
            Assert.That(CelPalette.Version, Is.Not.EqualTo(v), "the palette is sent again to the next renderer");
            Assert.That(CelPalette.Find("fire"), Is.EqualTo(CelPalette.Fire), "and its rows keep their ids");
        }

        [Test]
        public void Chunk_And_Light_Layers_Wait_For_Their_Frame()
        {
            CelFx.Adopt(renderer);
            int thrown = 0;
            CelFx.Chunks = (p, a, n, s, sc, c, l, b, salt) => thrown += n;
            CelDemo.Play(new Vector3(0f, 1f, 0f));
            Assert.That(thrown, Is.EqualTo(0), "the clods are due 0.22 s in");
            Assert.That(CelFx.PendingCount, Is.GreaterThan(0));
            CelFx.Clock = 100.3f;
            CelFx.Pump();
            Assert.That(thrown, Is.GreaterThan(0), "and thrown once due");
        }
    }
}
