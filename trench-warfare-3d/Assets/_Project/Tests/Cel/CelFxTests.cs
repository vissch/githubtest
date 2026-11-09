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
