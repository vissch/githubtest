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
        public void A_Gpu_Birth_End_Changes_Only_Late_Opacity_And_Preserves_The_Random_Stream()
        {
            var layer = new CelLayer
            {
                Start = 0.25f, Duration = 2.6f, TimeJitter = 0.04f,
                Life = 0.26f, LifeJitter = 0.1f, Shape = CelShape.Box,
                ShapeSize = new Vector3(0.2f, 1.05f, 1.8f), Aim = CelAim.Target,
                Arrive = 0.846154f, Spread = 0f, SizeJitter = 0.25f,
                Size = new CelCurve(0.15f, 0.15f), Opacity = 0.73f,
            };
            var originalRng = new CelRng(1917u * 31u + 1u);
            var boundedRng = new CelRng(1917u * 31u + 1u);
            int unchanged = 0, hidden = 0;
            for (int j = 0; j < 38; j++)
            {
                layer.BirthEnd = -1f;
                CelFx.Make(layer, j, 38, Vector3.zero, Vector3.forward, Vector3.right, Vector3.up,
                    new Vector3(10f, 2f, 5f), 100f, ref originalRng, out CelRecord original);
                layer.BirthEnd = 1.25f;
                CelFx.Make(layer, j, 38, Vector3.zero, Vector3.forward, Vector3.right, Vector3.up,
                    new Vector3(10f, 2f, 5f), 100f, ref boundedRng, out CelRecord bounded);
                var expected = original;
                if (original.Born > 101.5f) { expected.Tone.z = 0f; hidden++; }
                else unchanged++;
                Assert.That(bounded, Is.EqualTo(expected), "record " + j + ": every other packed field must be identical");
            }
            Assert.That(unchanged, Is.GreaterThan(0));
            Assert.That(hidden, Is.GreaterThan(0));
            for (int i = 0; i < 16; i++)
                Assert.That(boundedRng.Next(), Is.EqualTo(originalRng.Next()), "suppression must not skip random draws");
        }

        [Test]
        public void A_Gpu_Birth_Exactly_On_The_End_Remains_Visible_And_The_Default_Is_Unbounded()
        {
            var layer = new CelLayer { Start = 0.25f, Duration = 1f, LifeJitter = 0f, SizeJitter = 0f, Opacity = 0.42f };
            Assert.That(layer.BirthEnd, Is.LessThan(0f));
            var rng = new CelRng(73u);
            CelFx.Make(layer, 2, 3, Vector3.zero, Vector3.forward, Vector3.right, Vector3.up,
                Vector3.one, 100f, ref rng, out CelRecord unbounded);
            Assert.That(unbounded.Tone.z, Is.EqualTo(0.42f));
            layer.BirthEnd = 0.5f;
            CelFx.Make(layer, 1, 3, Vector3.zero, Vector3.forward, Vector3.right, Vector3.up,
                Vector3.one, 100f, ref rng, out CelRecord boundary);
            CelFx.Make(layer, 2, 3, Vector3.zero, Vector3.forward, Vector3.right, Vector3.up,
                Vector3.one, 100f, ref rng, out CelRecord after);
            Assert.That(boundary.Born, Is.EqualTo(100.75f));
            Assert.That(boundary.Tone.z, Is.EqualTo(0.42f));
            Assert.That(after.Tone.z, Is.Zero);
        }

        [Test]
        public void The_Reference_Hold_Birth_End_Suppresses_Only_The_Last_Jittered_Tracer()
        {
            var layer = ExplosionRecipe.Hold.Layers[0];
            float previous = layer.BirthEnd;
            try
            {
                layer.BirthEnd = 2.6f;
                int count = layer.CountAt(0f, 1.25f);
                Assert.That(count, Is.EqualTo(38));
                var rng = new CelRng(1917u * 31u + 1u);
                for (int j = 0; j < count; j++)
                {
                    CelFx.Make(layer, j, count, Vector3.zero, Vector3.forward, Vector3.right, Vector3.up,
                        new Vector3(10f, 2f, 5f), 100f, ref rng, out CelRecord record);
                    Assert.That(record.Tone.z, Is.EqualTo(j == 37 ? 0f : layer.Opacity), "reference tracer " + j);
                    if (j == 37) Assert.That(record.Born - 100f, Is.EqualTo(2.639317f).Within(0.00002f));
                }
            }
            finally { layer.BirthEnd = previous; }
        }

        [Test]
        public void A_Target_Frame_Is_Opt_In_And_Changes_Only_Metadata_After_All_Random_Draws()
        {
            var layer = new CelLayer
            {
                Start = 0.75f, Duration = 0.1f, TimeJitter = 0.04f, Life = 2.45f,
                Shape = CelShape.Box, ShapeSize = new Vector3(4.5f, 3.5f, 0.5f),
                Offset = new Vector3(0.2f, 0.3f, -0.4f), Aim = CelAim.Radial, Speed = 0.32f,
                Size = new CelCurve(0.37f, 0.67f, 0.35f), SizeJitter = 0.25f, Core = 2.75f,
            };
            Assert.That(layer.PackTargetFrame, Is.False, "existing kinds retain their normal Axis and Core by default");
            var target = new Vector3(12f, 3f, 7f);
            var standardRng = new CelRng(2128u * 31u + 1u);
            var payloadRng = new CelRng(2128u * 31u + 1u);
            for (int j = 0; j < 65; j++)
            {
                layer.PackTargetFrame = false;
                CelFx.Make(layer, j, 65, Vector3.zero, Vector3.forward, Vector3.right, Vector3.up,
                    target, 100f, ref standardRng, out CelRecord standard);
                layer.PackTargetFrame = true;
                CelFx.Make(layer, j, 65, Vector3.zero, Vector3.forward, Vector3.right, Vector3.up,
                    target, 100f, ref payloadRng, out CelRecord payload);
                Assert.That(standard.Axis.sqrMagnitude, Is.EqualTo(1f).Within(0.000002f));
                Assert.That(standard.Tone.y, Is.EqualTo(2.75f));
                var expected = standard;
                expected.Axis = target;
                expected.Tone.y = 100.75f;
                Assert.That(payload, Is.EqualTo(expected), "record " + j + ": position, birth, motion, seed, size, alpha and every other field stay unchanged");
            }
            for (int j = 0; j < 16; j++)
                Assert.That(payloadRng.Next(), Is.EqualTo(standardRng.Next()), "packing metadata must not skip or add RNG draws");
        }

        [TestCase(CelAnchor.Origin)]
        [TestCase(CelAnchor.Target)]
        public void A_Target_Frame_Ignores_Spawn_Offset_And_Shares_The_Late_Start_Birth(CelAnchor anchor)
        {
            var layer = new CelLayer
            {
                PackTargetFrame = true, Anchor = anchor, Shape = CelShape.Point,
                Start = 0.75f, Duration = 1.5f, TimeJitter = 0f, Life = 4f, LifeJitter = 0f,
            };
            var target = new Vector3(12f, 3f, 7f);
            var rng = new CelRng(491u);
            for (int j = 0; j < 3; j++)
            {
                layer.Offset = new Vector3(2f + j, 1f + j, -3f);
                CelFx.Make(layer, j, 3, Vector3.zero, Vector3.forward, Vector3.right, Vector3.up,
                    target, 100f, ref rng, out CelRecord record);
                Assert.That(record.Pos, Is.Not.EqualTo(target), "the spawn offset really changes this record's position");
                Assert.That(record.Axis, Is.EqualTo(target), "the common target excludes every per-layer spawn offset");
                Assert.That(record.Tone.y, Is.EqualTo(100.75f), "bank birth includes Start, independent of each staggered record birth");
                Assert.That(record.Born, Is.EqualTo(100.75f + j * 0.75f));
            }
        }

        [Test]
        public void A_Default_Line_Still_Stores_Its_End_Offset_And_Core()
        {
            var layer = new CelLayer { Shape = CelShape.Line, Aim = CelAim.None, Core = 3.25f };
            var origin = new Vector3(-4f, 1f, 3f);
            var target = new Vector3(12f, 3f, 7f);
            var rng = new CelRng(817u);
            Assert.That(layer.PackTargetFrame, Is.False);
            CelFx.Make(layer, 0, 1, origin, Vector3.forward, Vector3.right, Vector3.up,
                target, 100f, ref rng, out CelRecord record);
            Assert.That(record.Axis, Is.EqualTo(target - origin));
            Assert.That(record.Tone.y, Is.EqualTo(3.25f));
        }

        [Test]
        public void A_Target_Frame_Clamps_The_Common_Birth_Without_Removing_Staggered_Births()
        {
            var layer = new CelLayer { PackTargetFrame = true, Start = -0.25f, Duration = 1f, TimeJitter = 0f };
            var rng = new CelRng(79u);
            for (int j = 0; j < 3; j++)
            {
                CelFx.Make(layer, j, 3, Vector3.zero, Vector3.forward, Vector3.right, Vector3.up,
                    Vector3.one, 100f, ref rng, out CelRecord record);
                Assert.That(record.Tone.y, Is.EqualTo(100f));
                Assert.That(record.Born, Is.EqualTo(Mathf.Max(100f, 99.75f + j * 0.5f)));
            }
        }

        [Test]
        public void The_Main_Bank_And_Existing_Bridge_Share_Their_Target_Frame_Only()
        {
            var bank = ExplosionRecipe.MainCloud.Layers[0];
            var bridge = ExplosionRecipe.MainCloudBridge.Layers[0];
            Assert.That(bank.PackTargetFrame, Is.True);
            Assert.That(bridge.PackTargetFrame, Is.True);
            Assert.That(bank.Close, Is.EqualTo(52));
            Assert.That(bank.CountAt(0f, 1.25f), Is.EqualTo(65), "the reference capture is Epic, not High");
            Assert.That(bridge.Close, Is.EqualTo(1));
            for (int j = 1; j < ExplosionRecipe.MainCloud.Layers.Count; j++)
                Assert.That(ExplosionRecipe.MainCloud.Layers[j].PackTargetFrame, Is.False, "brown fragments and every other Main layer retain the default contract");
            var target = new Vector3(12f, 3f, 7f);
            var bankRng = new CelRng(2128u * 31u + 1u);
            var bridgeRng = new CelRng(3460u * 31u + 1u);
            CelFx.Make(bank, 64, 65, Vector3.zero, Vector3.forward, Vector3.right, Vector3.up,
                target, 100f, ref bankRng, out CelRecord b);
            CelFx.Make(bridge, 0, 1, Vector3.zero, Vector3.forward, Vector3.right, Vector3.up,
                target, 100f, ref bridgeRng, out CelRecord a);
            Assert.That(a.Axis, Is.EqualTo(b.Axis));
            Assert.That(a.Axis, Is.EqualTo(target));
            Assert.That(a.Tone.y, Is.EqualTo(b.Tone.y));
            Assert.That(a.Pos, Is.Not.EqualTo(b.Pos), "the existing distinct geometry must not collapse to the shared metadata anchor");
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
                var checks = new Dictionary<string, List<CelLayer>>();
                for (int i = 0; i < recipe.Layers.Count; i++)
                {
                    var layer = recipe.Layers[i];
                    string key = string.IsNullOrEmpty(layer.LodGroup) ? "layer " + i : "group " + layer.LodGroup;
                    if (!checks.TryGetValue(key, out var members)) checks.Add(key, members = new List<CelLayer>());
                    members.Add(layer);
                }
                foreach (FxTier tier in Enum.GetValues(typeof(FxTier)))
                {
                    float q = CelFx.QualityShare(tier);
                    foreach (var check in checks)
                    {
                        int last = int.MaxValue;
                        foreach (float z in zooms)
                        {
                            int n = 0;
                            foreach (var layer in check.Value) n += layer.CountAt(CelFx.LodBlend(z), q);
                            if (n > last) bad.Add(recipe.Name + "[" + check.Key + "] rises from " + last + " to " + n + " at zoom " + z + " (" + tier + ")");
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

        [Test]
        public void Explicit_Replacement_Groups_Validate_Their_Combined_Budget()
        {
            var close = new CelLayer { Name = "ribbon", LodGroup = "streak", Close = 50, Mid = 0, Far = 0, Palette = "fire" };
            var mid = new CelLayer { Name = "simple", LodGroup = "streak", Close = 0, Mid = 10, Far = 0, Palette = "fire" };
            var recipe = new CelFxRecipe("replacement", 1f, close, mid);
            Assert.That(recipe.Validate(renderer), Is.Empty);
            mid.Mid = 51;
            Assert.That(recipe.Validate(renderer), Has.Some.Contains("combined counts rise"));
            mid.Mid = 10;
            mid.Far = 11;
            Assert.That(recipe.Validate(renderer), Has.Some.Contains("combined counts rise"));
        }

        [Test]
        public void Lod_Groups_Do_Not_Hide_Ungrouped_Rises_Or_Invalid_Membership()
        {
            var layer = new CelLayer { Name = "dirt", Close = 3, Mid = 4, Far = 0, Palette = "fire" };
            var recipe = new CelFxRecipe("invalid", 1f, layer);
            Assert.That(recipe.Validate(renderer), Has.Some.Contains("counts rise"));
            layer.LodGroup = "solo";
            Assert.That(recipe.Validate(renderer), Has.Some.Contains("at least two"));
            var other = new CelLayer { Name = "dirt", LodGroup = "solo", Close = 5, Mid = 0, Far = 0, Palette = "fire" };
            recipe.Layers.Add(other);
            Assert.That(recipe.Validate(renderer), Has.Some.Contains("unique layer names"));
            other.Name = "other";
            layer.LodGroup = other.LodGroup = "bad group";
            Assert.That(recipe.Validate(renderer), Has.Some.Contains("LodGroup must use"));
        }

        [Test]
        public void Lod_Group_Validation_Checks_Rounded_Quality_Counts()
        {
            // Raw totals 2 -> 2 look equal, but Low quality rounds one Close record into two Mid records.
            var recipe = new CelFxRecipe("rounded", 1f,
                new CelLayer { Name = "close", LodGroup = "shape", Close = 2, Mid = 0, Far = 0, Palette = "fire" },
                new CelLayer { Name = "mid-a", LodGroup = "shape", Close = 0, Mid = 1, Far = 0, Palette = "fire" },
                new CelLayer { Name = "mid-b", LodGroup = "shape", Close = 0, Mid = 1, Far = 0, Palette = "fire" });
            Assert.That(recipe.Validate(renderer), Has.Some.Contains("rounded counts rise"));
        }

        [Test]
        public void Delayed_Play_Schedules_Light_And_Gpu_Birth_Without_Advancing_Clock()
        {
            CelFx.Adopt(renderer);
            int flashes = 0;
            var previousFlash = SceneHooks.Flash;
            SceneHooks.Flash = (at, color, power, radius, life) => flashes++;
            try
            {
                var recipe = new CelFxRecipe("delayed", 1f,
                    new CelLayer { Name = "light", Kind = CelKinds.LightName, Close = 1, Mid = 1, Far = 0, Palette = "fire", Life = 1f },
                    new CelLayer { Name = "gpu", Kind = CelKinds.LobeName, Close = 1, Mid = 1, Far = 0, Palette = "fire", Life = 1f, LifeJitter = 0f });
                CelFx.Play(recipe, Vector3.zero, Vector3.forward, Vector3.one, 17u, 0.25f);
                Assert.That(CelFx.Clock, Is.EqualTo(100f));
                Assert.That(flashes, Is.Zero, "scheduling the light must not fire it early");
                Assert.That(CelFx.PendingCount, Is.EqualTo(1));
                var field = typeof(CelFxRenderer).GetField("records", BindingFlags.Instance | BindingFlags.NonPublic);
                var records = (System.Collections.IEnumerable)field.GetValue(renderer);
                int scheduled = 0;
                foreach (CelRecord record in records)
                    if (record.Life > 0f) { Assert.That(record.Born, Is.EqualTo(100.25f)); scheduled++; }
                Assert.That(scheduled, Is.GreaterThan(0), "the test inspected real spawned GPU records");
                CelFx.Clock = 100.24f; CelFx.Pump();
                Assert.That(flashes, Is.Zero);
                CelFx.Clock = 100.25f; CelFx.Pump();
                Assert.That(flashes, Is.EqualTo(1));
                CelFx.Pump();
                Assert.That(flashes, Is.EqualTo(1));
            }
            finally { SceneHooks.Flash = previousFlash; }
        }

        [Test]
        public void Flash_Card_Is_Independent_And_Keeps_Delayed_Fallback()
        {
            CelFx.Adopt(renderer);
            var oldFlash = SceneHooks.Flash;
            var oldFire = SceneHooks.FlashWithCard;
            int fireCalls = 0, flashCalls = 0;
            SceneHooks.Flash = (at, color, power, reach, life) => flashCalls++;
            SceneHooks.FlashWithCard = (at, color, power, reach, life, card) =>
            {
                fireCalls++;
                Assert.That(power, Is.EqualTo(4f));
                Assert.That(reach, Is.EqualTo(26f));
                Assert.That(card, Is.EqualTo(0.22f));
            };
            try
            {
                var recipe = new CelFxRecipe("controlled-light", 1f,
                    new CelLayer { Kind = CelKinds.LightName, Close = 1, Mid = 1,
                        Palette = "fire", Core = 4f, Extra = new Vector4(26f, 0.22f, 0f, 0f) });
                CelFx.Play(recipe, Vector3.zero, Vector3.forward, Vector3.one, 9u, 0.25f);
                Assert.That(fireCalls, Is.Zero);
                CelFx.Clock = 100.25f; CelFx.Pump();
                Assert.That(fireCalls, Is.EqualTo(1));
                Assert.That(flashCalls, Is.Zero);
                SceneHooks.FlashWithCard = null;
                CelFx.Play(recipe, Vector3.zero, Vector3.forward, Vector3.one, 11u);
                Assert.That(flashCalls, Is.EqualTo(1), "stages without explicit-card support retain the light");
            }
            finally { SceneHooks.Flash = oldFlash; SceneHooks.FlashWithCard = oldFire; }
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
