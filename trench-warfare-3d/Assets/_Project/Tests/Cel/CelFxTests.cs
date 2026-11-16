// Phase: CelFx kit (2026-11-09) - the kit's rules held by tests: a spawn and a frame allocate nothing, a recipe never
// spends more going out, every cel shader is found and kept for a build, every draw goes through FrameBudget, and the
// statics are put back when Play ends.
using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text.RegularExpressions;
using NUnit.Framework;
using UnityEditor;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.VFX.Cel;

namespace TW.Tests
{
    public sealed class CelFxTests
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

        [TestCase(1f)]
        [TestCase(-1f)]
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

        [TestCase(false)]
        [TestCase(true)]
        public void A_Second_Emitter_Does_Not_Recolor_An_Already_Rendered_Jet(bool frontOnly)
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
            // Also render the front alone, so the body cannot hide a missing or broken front in this assertion.
            if (frontOnly) recipe.Layers.RemoveRange(0, 2);
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
                CelFx.Clock = 100.6f;
                var before = Capture();
                CelFx.Clock = 101f;
                FlamethrowerRecipe.Play(recipe, new Vector3(2200f, 1f, 2050f), new Vector3(2200f, 3f, 2062f), 1918);
                CelFx.Clock = 100.6f;
                var after = Capture();
                int visible = 0, changed = 0;
                for (int i = 0; i < before.Length; i++)
                {
                    if (before[i].r + before[i].g + before[i].b > 30) visible++;
                    if (!before[i].Equals(after[i])) changed++;
                }
                Assert.That(visible, Is.GreaterThan(50), frontOnly ? "the travelling front must be visible" : "the comparison must contain a visible jet");
                Assert.That(changed, Is.Zero, "an offscreen emitter must not alter the active stream or travelling front");
            }
            finally
            {
                camera.targetTexture = null; RenderTexture.active = previousTarget;
                UnityEngine.Object.DestroyImmediate(cameraObject);
                UnityEngine.Object.DestroyImmediate(pixels);
                rt.Release(); UnityEngine.Object.DestroyImmediate(rt);
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

        [Test]
        public void Preparation_Preserves_Fireball_Motion_And_Default_Kinds()
        {
            var record = AnyRecord(100f);
            record.Extra.w = -2f;
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

        [Test]
        public void FlameSmoke_Preparation_Keeps_Independent_Emitter_Starts_And_Motion()
        {
            CelFx.Adopt(renderer);
            var recipe = new CelFxRecipe("smoke phase isolation", 8f, new CelLayer
            {
                Kind = "FlameSmoke", Start = 1.4f, Duration = 2.1f, Close = 4, Mid = 4, Far = 4,
                Life = 3f, LifeJitter = 0f, Palette = "white", Extra = new Vector4(1.1f, 0.12f, -0.8f, -1f),
            });
            int kind = renderer.KindId("FlameSmoke");
            CelFx.Play(recipe, Vector3.zero, Vector3.forward, Vector3.forward * 10f, 1917);
            int firstCount = renderer.SpawnedOf(kind);
            Assert.That(firstCount, Is.GreaterThanOrEqualTo(2));
            var first = StoredRecord(kind, 0);
            Assert.That(first.Tone.z, Is.EqualTo(100f));
            Assert.That(StoredRecord(kind, 1).Tone.z, Is.EqualTo(100f), "different births share one emitter phase");
            Assert.That(StoredRecord(kind, 1).Born, Is.GreaterThan(first.Born));
            CelFx.Clock = 102f;
            CelFx.Play(recipe, Vector3.right * 20f, Vector3.forward, Vector3.right * 20f + Vector3.forward * 10f, 1918);
            Assert.That(StoredRecord(kind, 0), Is.EqualTo(first));
            Assert.That(StoredRecord(kind, firstCount).Tone.z, Is.EqualTo(102f));

            var record = AnyRecord(100f);
            record.Motion = new Vector4(0.2f, 0.3f, 0.4f, 0.5f);
            record.Extra.w = -1f;
            var prepared = record;
            new FlameSmokeKind().PrepareRecord(ref prepared, Vector3.zero, Vector3.one, 108f);
            var expected = record; expected.Tone.z = 108f;
            Assert.That(prepared, Is.EqualTo(expected), "only the unused cutout opacity slot becomes the phase origin");
        }

        [Test]
        public void FlameSmoke_Preparation_Allocates_Nothing()
        {
            int kind = renderer.KindId("FlameSmoke");
            var record = AnyRecord(100f); record.Extra.w = -1f;
            Assert.That(Allocated(() => renderer.Spawn(kind, record, Vector3.zero, Vector3.one, 100f), 2000), Is.Zero);
        }

        [Test]
        public void TimeScaled_FlameSmoke_Composes_Emitter_Phase_Delay_Without_Changing_Other_Lobes()
        {
            var source = new CelFxRecipe("smoke phase scaling", 8f,
                new CelLayer { Kind = "FlameSmoke", Start = 1.4f, Extra = new Vector4(1f, 0.1f, -0.8f, -1f) },
                new CelLayer { Kind = CelKinds.LobeName, Opacity = 0.7f, Extra = new Vector4(1f, 0.1f, -0.8f, 0.3f) });
            var scaled = FlamethrowerRecipe.TimeScaled(source, 1.2f, 0.16f);
            Assert.That(scaled.Layers[0].Extra.w, Is.EqualTo(-1.2f).Within(1e-6f));
            Assert.That(scaled.Layers[0].Opacity, Is.EqualTo(1.16f).Within(1e-6f));
            Assert.That(scaled.Layers[1].Opacity, Is.EqualTo(0.7f));
            Assert.That(scaled.Layers[1].Extra.w, Is.EqualTo(0.3f));
            var twice = FlamethrowerRecipe.TimeScaled(scaled, 2f, 0.3f);
            Assert.That(twice.Layers[0].Extra.w, Is.EqualTo(-2.4f).Within(1e-6f));
            Assert.That(twice.Layers[0].Opacity, Is.EqualTo(1.62f).Within(1e-6f));
            Assert.That(source.Layers[0].Opacity, Is.EqualTo(1f), "scaling does not mutate the source");
            var record = AnyRecord(100f); record.Tone.z = twice.Layers[0].Opacity;
            new FlameSmokeKind().PrepareRecord(ref record, Vector3.zero, Vector3.one, 100f);
            Assert.That(record.Tone.z, Is.EqualTo(100.62f).Within(1e-5f));
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

        [Test]
        public void Every_Recipe_Is_Valid_And_Never_Spends_More_Going_Out()
        {
            var recipes = AllRecipes();
            Assert.That(recipes.Exists(r => r.Name == CelDemo.Recipe.Name), Is.True, "the scan finds the demo, so it is not blind");
            var bad = new List<string>();
            float[] zooms = { 6f, 7.5f, 10f, 12f, 16f, 20f, 25f, 30f, 45f, 60f, 90f, 120f, 240f, 600f };
            foreach (var recipe in recipes)
            {
                bad.AddRange(recipe.Validate(renderer));
                foreach (FxTier tier in Enum.GetValues(typeof(FxTier)))
                {
                    float q = CelFx.QualityShare(tier);
                    for (int i = 0; i < recipe.Layers.Count; i++)
                    {
                        int last = int.MaxValue;
                        foreach (float z in zooms)
                        {
                            int n = recipe.Layers[i].CountAt(CelFx.LodBlend(z), q);
                            if (n > last) bad.Add(recipe.Name + "[" + i + "] rises from " + last + " to " + n + " at zoom " + z + " (" + tier + ")");
                            last = n;
                        }
                    }
                }
            }
            Assert.That(bad, Is.Empty, string.Join("\n", bad));
        }

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
