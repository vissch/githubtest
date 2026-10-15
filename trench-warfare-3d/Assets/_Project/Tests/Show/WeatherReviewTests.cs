// Phase: review fixes (rv-77-p3-u19a) — the weather findings W4, W5, W6 and W7. Both are source contracts: Storm's meshes
// are made in Start and freed in OnDestroy, which only run in play (and Destroy throws in edit mode), and the rain's
// wrap is a deal between Rain.cs and Rain_URP.shader that nothing here can run. So each test pins the one line the
// finding is about and the number on the other side it has to agree with.
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Text.RegularExpressions;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class WeatherReviewTests
    {
        const string StormPath = "Assets/_Project/Presentation/Terrain/Storm.cs";
        const string RainPath = "Assets/_Project/Presentation/Terrain/Rain.cs";
        const string RainShaderPath = "Assets/_Project/Shaders/Rain_URP.shader";

        // [W4] every Mesh Storm builds has to be held in a field and destroyed in OnDestroy: the "Lightning glow"
        // card was a local, so each scene load left one mesh behind for good.
        [Test]
        public void TheLightningGlowCardIsDestroyedWithTheStorm()
        {
            string src = File.ReadAllText(StormPath);
            Assert.That(Regex.IsMatch(src, @"var\s+\w+\s*=\s*new Mesh"), Is.False,
                "[W4] a Mesh kept in a local can never be destroyed; hold it in a field");

            var made = Regex.Matches(src, @"(\w+)\s*=\s*new Mesh\b");
            Assert.That(made.Count, Is.GreaterThan(0), "[W4] Storm must still build its meshes here");

            var body = Regex.Match(src, @"void OnDestroy\(\)\s*\{([^}]*)\}");
            Assert.That(body.Success, Is.True, "[W4] Storm must have an OnDestroy to free its meshes in");

            foreach (Match m in made)
            {
                string field = m.Groups[1].Value;
                Assert.That(Regex.IsMatch(src, @"Mesh\s+[\w\s,]*\b" + field + @"\b[\w\s,]*;"), Is.True,
                    "[W4] " + field + " must be a Mesh field of Storm, not a local");
                Assert.That(body.Groups[1].Value, Does.Match(@"Destroy\(\s*" + field + @"\s*\)"),
                    "[W4] OnDestroy never destroys " + field + ": one mesh leaks per scene load");
            }
        }

        // [W5] the CPU wraps the fall at a whole number of boxes, but the shader scales the Y offset per streak, so
        // the wrap is only seamless when that scale times the wrap count is whole. The finding's expected value: the
        // jump a streak makes at a wrap is zero boxes, for every streak.
        [Test]
        public void TheFallWrapsAtAWholeNumberOfBoxesForEveryStreak()
        {
            string rain = File.ReadAllText(RainPath);
            var wrapConst = Regex.Match(rain, @"const float Wraps = ([0-9.]+)f;");
            Assert.That(wrapConst.Success, Is.True, "[W5] Rain must name the number of boxes it wraps the fall at");
            float wraps = float.Parse(wrapConst.Groups[1].Value, CultureInfo.InvariantCulture);

            Assert.That(rain, Does.Match(@"Mathf\.Repeat\(fallen\[k\], Box\[k\] \* Wraps\)"),
                "[W5] the wrap must use the same Wraps the shader is told about");
            Assert.That(rain, Does.Match(@"SetVector\(OffsetId, new Vector4\(fallen\.x, fallen\.y, fallen\.z, Wraps\)\)"),
                "[W5] _Offset's free w must carry the wrap count, or the shader cannot quantise against it");

            string shader = File.ReadAllText(RainShaderPath);
            Assert.That(shader, Does.Match(@"float w = max\(1\.0, _Offset\.w\);"),
                "[W5] the shader must read the wrap count off _Offset.w (guarded against 0)");
            var scale = Regex.Match(shader, @"float ys = round\(\(([0-9.]+) \+ ([0-9.]+) \* v\.random\.x\) \* w\) / w;");
            Assert.That(scale.Success, Is.True,
                "[W5] the per-streak Y scale must be rounded to steps of 1/w, or every flake jumps once per wrap");
            Assert.That(shader, Does.Match(@"velocity = float3\(_Fall\.x, -_Fall\.y \* ys, _Fall\.z\)"),
                "[W5] the fall speed must use the quantised ys, or the slant stops matching the motion");
            Assert.That(shader, Does.Match(@"_Offset\.xyz \* float3\(1\.0, ys, 1\.0\)"),
                "[W5] the Y offset must be scaled by the quantised ys, or every flake jumps once per wrap");

            float a = float.Parse(scale.Groups[1].Value, CultureInfo.InvariantCulture);
            float b = float.Parse(scale.Groups[2].Value, CultureInfo.InvariantCulture);
            for (int i = 0; i < 64; i++)
            {
                float r = i / 63f;
                float s = Mathf.Round((a + b * r) * wraps) / wraps;      // what the shader's text says it does
                float jump = Mathf.Abs(wraps * s - Mathf.Round(wraps * s));
                Assert.That(jump, Is.LessThan(1e-4f),
                    "[W5] streak " + i + " jumps " + jump + " boxes at a wrap; it must jump none");
            }
        }

        // [W6] the wading scan must take the slots in turn: the cursor used to move inside the same walk that adds
        // n, so after the first hit the walk restarted and skipped slots. Expected values from the finding's worked
        // example: 20 alive units all rippling, cursor 0, three a step -> 0, 1, 2 and the cursor left at 3.
        [Test]
        public void TheWadingScanTakesEverySlotInTurn()
        {
            var into = new int[3];
            int found = WaterRings.RipplesThisStep(20, 400, 3, i => true, into, 0, out int next);
            Assert.That(found, Is.EqualTo(3), "[W6] three rippling slots a step");
            Assert.That(new[] { into[0], into[1], into[2] }, Is.EqualTo(new[] { 0, 1, 2 }),
                "[W6] the scan skipped slots: it must take them in turn");
            Assert.That(next, Is.EqualTo(3), "[W6] the cursor must come to rest one past the last slot taken");

            var rippling = new HashSet<int> { 5, 6, 7, 12 };
            found = WaterRings.RipplesThisStep(20, 400, 3, i => rippling.Contains(i), into, 0, out next);
            Assert.That(found, Is.EqualTo(3), "[W6] three of the four rippling slots this step");
            Assert.That(new[] { into[0], into[1], into[2] }, Is.EqualTo(new[] { 5, 6, 7 }),
                "[W6] the scan must take the rippling slots in turn, not every other one");
            Assert.That(next, Is.EqualTo(8), "[W6] the cursor must come to rest one past the last slot taken");
        }

        // [W7] LampOut used to clear glow card j for lantern j, but MoreFires appends cards with no lantern, so a
        // lantern added after it points at a burning tree's card. Here lantern 2's card is 4; cards 2 and 3 are
        // MoreFires'. Expected: card 4 goes clear, cards 0-3 stay lit.
        [Test]
        public void LampOutClearsTheCardOfTheLampThatWentOut()
        {
            var go = new GameObject("W7 night lights");
            var mesh = new Mesh();
            var lights = new List<Light>();
            try
            {
                var nl = go.AddComponent<NightLights>();
                for (int i = 0; i < 3; i++)
                {
                    var lgo = new GameObject("lamp " + i);
                    lgo.transform.position = new Vector3(i * 20f, 0f, 0f);
                    var l = lgo.AddComponent<Light>();
                    l.enabled = true;
                    lights.Add(l);
                }
                var verts = new Vector3[20]; var cols = new Color[20];      // five glow cards, four vertices each
                for (int i = 0; i < 20; i++) cols[i] = Color.white;
                mesh.vertices = verts; mesh.colors = cols;

                Set(nl, "lanterns", lights);
                Set(nl, "lanternHome", new List<Vector3> { lights[0].transform.position, lights[1].transform.position, lights[2].transform.position });
                Set(nl, "lanternCard", new List<int> { 0, 1, 4 });
                Set(nl, "nightGlows", mesh);

                var lampOut = typeof(NightLights).GetMethod("LampOut",
                    System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
                Assert.That(lampOut, Is.Not.Null, "[W7] NightLights.LampOut moved");
                lampOut.Invoke(nl, new object[] { new Vector3(40f, 0f, 0f), 1f });

                var after = mesh.colors;
                Assert.That(lights[2].enabled, Is.False, "[W7] the lamp whose post went must go out");
                for (int k = 0; k < 4; k++)
                    Assert.That(after[4 * 4 + k], Is.EqualTo(Color.clear), "[W7] card 4 is that lamp's card and must go clear");
                for (int c = 0; c < 4; c++)
                for (int k = 0; k < 4; k++)
                    Assert.That(after[c * 4 + k], Is.EqualTo(Color.white),
                        "[W7] card " + c + " belongs to another lamp or to MoreFires and must stay lit");
            }
            finally
            {
                foreach (var l in lights) if (l != null) Object.DestroyImmediate(l.gameObject);
                Object.DestroyImmediate(mesh);
                Object.DestroyImmediate(go);
            }
        }

        static void Set(object target, string field, object value)
        {
            var f = target.GetType().GetField(field,
                System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
            Assert.That(f, Is.Not.Null, "[W7] NightLights." + field + " moved");
            f.SetValue(target, value);
        }

        // [W8] shipFired used to start at all zeros, which reads as "fired at time zero": at load, Time.timeSinceLevelLoad
        // is itself near zero, so every gunboat's gun kicked and showed a bow ring for its first fraction of a second.
        [Test]
        public void AGunboatHasNotFiredWhenTheLevelLoads()
        {
            var go = new GameObject("W8 landing craft") { hideFlags = HideFlags.HideAndDontSave };
            try
            {
                var view = go.AddComponent<LandingCraftView>();
                var f = typeof(LandingCraftView).GetField("shipFired",
                    System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
                Assert.That(f, Is.Not.Null, "[W8] LandingCraftView.shipFired moved");
                var shipFired = (float[])f.GetValue(view);
                Assert.That(shipFired.Length, Is.EqualTo(TW.Sim.Match.SeaLandingSystem.Ships));
                foreach (var t in shipFired)
                    Assert.That(t, Is.LessThanOrEqualTo(-0.45f),
                        "[W8] an unfired gunboat must already be past the .45 s kick window at load");
            }
            finally
            {
                Object.DestroyImmediate(go);
            }
        }

        // [W9] Atmosphere.OnDestroy cleared only the colours, leaving _TWWind, _TWMist, _TWField and _TWFieldFog
        // (and QuietFog's _TWPresence, pointed at a texture it just destroyed) standing for the next scene.
        [Test]
        public void AtmosphereAndQuietFogLeaveNoGlobalsBehind()
        {
            int windId = Shader.PropertyToID("_TWWind"), mistId = Shader.PropertyToID("_TWMist");
            int fieldId = Shader.PropertyToID("_TWField"), fieldFogId = Shader.PropertyToID("_TWFieldFog");
            int presenceId = Shader.PropertyToID("_TWPresence");
            var stray = new Vector4(1f, 2f, 3f, 4f);
            Shader.SetGlobalVector(windId, stray); Shader.SetGlobalVector(mistId, stray);
            Shader.SetGlobalVector(fieldId, stray); Shader.SetGlobalVector(fieldFogId, stray);
            var throwaway = new Texture2D(1, 1);
            Shader.SetGlobalTexture(presenceId, throwaway);

            GameObject atmoGo = null, fogGo = null;
            try
            {
                atmoGo = new GameObject("W9 atmosphere") { hideFlags = HideFlags.HideAndDontSave };
                var atmo = atmoGo.AddComponent<Atmosphere>();
                var atmoDestroy = typeof(Atmosphere).GetMethod("OnDestroy",
                    System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
                Assert.That(atmoDestroy, Is.Not.Null, "[W9] Atmosphere.OnDestroy moved");
                atmoDestroy.Invoke(atmo, null);

                fogGo = new GameObject("W9 quiet fog") { hideFlags = HideFlags.HideAndDontSave };
                var fog = fogGo.AddComponent<QuietFog>();
                var fogDestroy = typeof(QuietFog).GetMethod("OnDestroy",
                    System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
                Assert.That(fogDestroy, Is.Not.Null, "[W9] QuietFog.OnDestroy moved");
                fogDestroy.Invoke(fog, null);

                Assert.That(Shader.GetGlobalVector(windId), Is.EqualTo(Vector4.zero), "[W9] _TWWind must not outlive Atmosphere");
                Assert.That(Shader.GetGlobalVector(mistId), Is.EqualTo(Vector4.zero), "[W9] _TWMist must not outlive Atmosphere");
                Assert.That(Shader.GetGlobalVector(fieldId), Is.EqualTo(Vector4.zero), "[W9] _TWField must not outlive Atmosphere");
                Assert.That(Shader.GetGlobalVector(fieldFogId), Is.EqualTo(Vector4.zero), "[W9] _TWFieldFog must not outlive Atmosphere");
                Assert.That(Shader.GetGlobalTexture(presenceId), Is.Not.EqualTo(throwaway),
                    "[W9] _TWPresence must not point at the texture QuietFog just destroyed");
            }
            finally
            {
                Shader.SetGlobalVector(windId, Vector4.zero); Shader.SetGlobalVector(mistId, Vector4.zero);
                Shader.SetGlobalVector(fieldId, Vector4.zero); Shader.SetGlobalVector(fieldFogId, Vector4.zero);
                Shader.SetGlobalTexture(presenceId, Texture2D.whiteTexture);
                if (atmoGo != null) Object.DestroyImmediate(atmoGo);
                if (fogGo != null) Object.DestroyImmediate(fogGo);
                Object.DestroyImmediate(throwaway);
            }
        }

        // [W10] BiomeProfile.ForGround claimed a missing case is a compile error, but only WinterLine was named and
        // ShelledForest/Landing fell through `default` silently; this test is the real guard the comment promises.
        [Test]
        public void EveryGroundNamesItsOwnLook()
        {
            const string path = "Assets/_Project/Presentation/Terrain/BiomeProfile.cs";
            string src = File.ReadAllText(path);
            foreach (var g in System.Enum.GetValues(typeof(TW.Presentation.Ground)))
                Assert.That(src, Does.Match(@"case\s+TW\.Presentation\.Ground\." + g + @"\s*:"),
                    "[W10] Ground." + g + " must be named in BiomeProfile.ForGround's switch");
            Assert.That(src, Does.Not.Contain("compile error"),
                "[W10] the comment must not promise a compile error the switch cannot give");
        }
    }
}
