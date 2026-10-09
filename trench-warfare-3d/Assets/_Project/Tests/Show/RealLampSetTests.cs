// Phase: night lights (2026-10-07, the owner's word: "The 8 lamps nearest your view stay real, the other 35 painted
// only") — RealLampSet, the choice behind NightLights' fixed lamps, and its wiring (NightLights.RealLamps.cs): the eight
// nearest the view are chosen, a tie or a moving view never pops a lamp, the knob at 43 is the night as it was, a lamp
// by water in the picture keeps its reflection, and nothing but the fixed lamps is switched (flashes, crater glows,
// the star shell and the machines' lights are left alone).
using System.Collections.Generic;
using System.Reflection;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Terrain;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class RealLampSetTests
    {
        const float Dt = 1f / 60f;

        /// <summary>43 lamps on a ragged grid, 13 m apart: the field's count, no two as far from an ordinary focus as each other.</summary>
        static List<Vector3> Field(int n = 43)
        {
            var at = new List<Vector3>();
            for (int i = 0; i < n; i++) at.Add(new Vector3(8f + (i % 6) * 13f + (i / 6) * 1.7f, 1.5f + (i % 3) * 0.2f, 10f + (i / 6) * 13f + (i % 6) * 0.9f));
            return at;
        }

        static List<int> Nearest(List<Vector3> at, Vector3 focus, int k, IReadOnlyList<bool> isOut = null)
        {
            var idx = new List<int>();
            for (int i = 0; i < at.Count; i++) if (isOut == null || !isOut[i]) idx.Add(i);
            idx.Sort((a, b) =>
            {
                float da = (at[a].x - focus.x) * (at[a].x - focus.x) + (at[a].z - focus.z) * (at[a].z - focus.z);
                float db = (at[b].x - focus.x) * (at[b].x - focus.x) + (at[b].z - focus.z) * (at[b].z - focus.z);
                int c = da.CompareTo(db); return c != 0 ? c : a.CompareTo(b);
            });
            if (idx.Count > k) idx.RemoveRange(k, idx.Count - k);
            idx.Sort();
            return idx;
        }

        static List<int> ChosenOf(RealLampSet s)
        {
            var l = new List<int>();
            for (int i = 0; i < s.Count; i++) if (s.Chosen(i)) l.Add(i);
            return l;
        }

        [Test]
        public void TheDefaults_AreTheOwnersEight_AndFortyThreeIsEveryLamp()
        {
            Assert.AreEqual(8, RealLampSet.DefaultBudget, "the owner, 2026-10-07: the 8 lamps nearest your view stay real");
            Assert.AreEqual(43, RealLampSet.Everything, "the lamps on the field he decided on: that many or more is the night as it was");
            Assert.AreEqual(8, Knobs.Get("lights.realLamps", RealLampSet.DefaultBudget));
            Assert.AreEqual(4, Knobs.Get("lights.waterLamps", RealLampSet.DefaultExtra));
            Assert.LessOrEqual(RealLampSet.DefaultBudget, 8, "the renderer gives an object eight lights: more real lamps than that light nothing more");
        }

        [Test]
        public void TheEightNearestTheView_KeepTheirRealLight_TheRestArePaintedOnly()
        {
            var at = Field();
            foreach (var focus in new[] { new Vector3(40f, 0f, 45f), new Vector3(5f, 0f, 5f), new Vector3(80f, 0f, 100f), new Vector3(-30f, 0f, 60f) })
            {
                var s = new RealLampSet();
                s.Step(at, null, null, focus, 8, 0, Dt);
                CollectionAssert.AreEqual(Nearest(at, focus, 8), ChosenOf(s), $"focus {focus}");
                Assert.AreEqual(8, s.LitCount, "the first step is a cut: the eight are lit at once and no other");
                for (int i = 0; i < at.Count; i++)
                {
                    Assert.AreEqual(s.Chosen(i) ? 1f : 0f, s.Weight(i), $"lamp {i}");
                    Assert.AreEqual(s.Chosen(i) ? 1f : 0f, s.Level(i), $"lamp {i}");
                }
            }
        }

        [Test]
        public void HeightDoesNotCount_ALampOnAMoundIsAsNearAsOneInATrench()
        {
            var at = new List<Vector3> { new Vector3(10f, 30f, 0f), new Vector3(11f, 0f, 0f), new Vector3(12f, 0f, 0f) };
            var s = new RealLampSet();
            s.Step(at, null, null, Vector3.zero, 1, 0, Dt);
            CollectionAssert.AreEqual(new[] { 0 }, ChosenOf(s), "distance is measured along the ground");
        }

        [Test]
        public void ATie_GoesByTheList_AndAShakingView_NeverSwapsTwoLamps()
        {
            // seven lamps close by, then two exactly as far off as each other for the eighth place, then the rest far away
            var at = new List<Vector3>();
            for (int i = 0; i < 7; i++) at.Add(new Vector3(i - 3f, 1.5f, 0f));
            at.Add(new Vector3(-20f, 1.5f, 0f)); at.Add(new Vector3(20f, 1.5f, 0f));
            for (int i = 0; i < 34; i++) at.Add(new Vector3(60f + i, 1.5f, 60f));
            var s = new RealLampSet();
            s.Step(at, null, null, Vector3.zero, 8, 0, Dt);
            Assert.IsTrue(s.Chosen(7), "the earlier of two equal lamps"); Assert.IsFalse(s.Chosen(8));
            // the view shakes a metre either way (CameraShake, a hand on the mouse): the eighth lamp keeps its place
            for (int f = 0; f < 600; f++)
            {
                float shake = Mathf.Sin(f * 1.7f) * 0.9f;
                s.Step(at, null, null, new Vector3(shake, 0f, Mathf.Cos(f * 0.9f) * 0.5f), 8, 0, Dt);
                Assert.IsTrue(s.Chosen(7), $"frame {f}"); Assert.IsFalse(s.Chosen(8), $"frame {f}");
                Assert.AreEqual(1f, s.Weight(7), $"frame {f}"); Assert.AreEqual(0f, s.Weight(8), $"frame {f}");
            }
            // only when the other lamp is nearer by more than KeepMetres does it take the place
            s.Step(at, null, null, new Vector3(RealLampSet.KeepMetres * 0.5f - 0.05f, 0f, 0f), 8, 0, Dt);
            Assert.IsTrue(s.Chosen(7), "nearer by a hair under KeepMetres: no swap");
            s.Step(at, null, null, new Vector3(RealLampSet.KeepMetres * 0.5f + 0.05f, 0f, 0f), 8, 0, Dt);
            Assert.IsTrue(s.Chosen(8), "nearer by more: the place changes hands"); Assert.IsFalse(s.Chosen(7));
        }

        [Test]
        public void AsTheSetChanges_OneLampFadesOutAsTheOtherFadesIn_AndTheirLightAddsUpToOne()
        {
            var at = new List<Vector3> { new Vector3(-10f, 1.5f, 0f), new Vector3(10f, 1.5f, 0f) };
            var s = new RealLampSet();
            s.Step(at, null, null, new Vector3(-5f, 0f, 0f), 1, 0, Dt);
            Assert.AreEqual(1f, s.Level(0)); Assert.AreEqual(0f, s.Level(1));
            var focus = new Vector3(5f, 0f, 0f);   // the view has moved ten metres: a pan, not a cut
            float last0 = 1f, last1 = 0f; int steps = 0;
            for (; steps < 200 && s.Weight(1) < 1f; steps++)
            {
                s.Step(at, null, null, focus, 1, 0, Dt);
                Assert.AreEqual(1f, s.Level(0) + s.Level(1), 1e-5f, $"step {steps}: the two together are one lamp's light");
                Assert.LessOrEqual(s.Level(0), last0); Assert.GreaterOrEqual(s.Level(1), last1);
                Assert.LessOrEqual(last0 - s.Level(0), 1.5f * Dt / RealLampSet.FadeSeconds + 1e-5f, $"step {steps}: no pop (the eased curve's steepest is 1.5 x the even rate)");
                last0 = s.Level(0); last1 = s.Level(1);
            }
            Assert.AreEqual(Mathf.CeilToInt(RealLampSet.FadeSeconds / Dt), steps, 1, "the fade takes FadeSeconds");
            Assert.AreEqual(0f, s.Weight(0)); Assert.AreEqual(1f, s.Weight(1));
            Assert.AreEqual(1, s.LitCount, "and then one lamp is lit again, not two");
        }

        [Test]
        public void APanAcrossTheField_NeverPopsALamp_AndSettlesOnTheEightNearest()
        {
            var at = Field();
            var s = new RealLampSet();
            var focus = new Vector3(10f, 0f, 12f);
            s.Step(at, null, null, focus, 8, 0, Dt);
            var before = new float[at.Count];
            int changes = 0; var was = ChosenOf(s);
            for (int f = 0; f < 600; f++)   // ten seconds at 9 m/s, up the field and across it
            {
                for (int i = 0; i < at.Count; i++) before[i] = s.Weight(i);
                focus += new Vector3(0.05f, 0f, 0.14f);
                s.Step(at, null, null, focus, 8, 0, Dt);
                Assert.AreEqual(8, s.ChosenCount, $"frame {f}: eight lamps hold a place at every moment");
                for (int i = 0; i < at.Count; i++)
                {
                    Assert.That(s.Weight(i), Is.InRange(0f, 1f));
                    Assert.LessOrEqual(Mathf.Abs(s.Weight(i) - before[i]), Dt / RealLampSet.FadeSeconds + 1e-5f, $"frame {f}, lamp {i}: a step of the fade, never a jump");
                }
                var now = ChosenOf(s);
                if (!Same(now, was)) { changes++; was = now; }
            }
            Assert.Greater(changes, 5, "the pan really crossed lamps");
            for (int f = 0; f < 60; f++) s.Step(at, null, null, focus, 8, 0, Dt);   // the view rests: a second is longer than the fade
            Assert.AreEqual(8, s.LitCount, "only the eight are lit once the fades are done");
            // a lamp holds its place against one nearer by less than KeepMetres, so the eight are the nearest to within that
            var nearest = Nearest(at, focus, 8); float worstKept = 0f, bestLeft = float.MaxValue;
            for (int i = 0; i < at.Count; i++)
            {
                float d = Mathf.Sqrt((at[i].x - focus.x) * (at[i].x - focus.x) + (at[i].z - focus.z) * (at[i].z - focus.z));
                if (s.Chosen(i)) worstKept = Mathf.Max(worstKept, d); else bestLeft = Mathf.Min(bestLeft, d);
                Assert.AreEqual(s.Chosen(i) ? 1f : 0f, s.Weight(i), $"lamp {i} settled");
            }
            Assert.LessOrEqual(worstKept, bestLeft + RealLampSet.KeepMetres + 1e-3f, "the eight nearest, give or take the margin a lamp keeps its place by");
            Assert.GreaterOrEqual(nearest.Count, 8);
        }

        static bool Same(List<int> a, List<int> b)
        {
            if (a.Count != b.Count) return false;
            for (int i = 0; i < a.Count; i++) if (a[i] != b[i]) return false;
            return true;
        }

        [Test]
        public void ACut_SetsEveryLampAtOnce_AJumpOnTheMapIsNotAFade()
        {
            var at = Field();
            var s = new RealLampSet();
            var a = new Vector3(10f, 0f, 12f); var b = a + new Vector3(0f, 0f, RealLampSet.CutMetres + 40f);
            s.Step(at, null, null, a, 8, 0, Dt);
            s.Step(at, null, null, b, 8, 0, Dt);
            CollectionAssert.AreEqual(Nearest(at, b, 8), ChosenOf(s));
            Assert.AreEqual(8, s.LitCount, "the whole picture changed: nothing left burning from the last one");
            for (int i = 0; i < at.Count; i++) Assert.AreEqual(s.Chosen(i) ? 1f : 0f, s.Weight(i));
            // a step with no time in it (a second camera in the same frame, a held clock) changes no weight short of a cut
            var c = b + new Vector3(0f, 0f, -20f);
            var w = new float[at.Count]; for (int i = 0; i < at.Count; i++) w[i] = s.Weight(i);
            s.Step(at, null, null, c, 8, 0, 0f);
            for (int i = 0; i < at.Count; i++) Assert.AreEqual(w[i], s.Weight(i), $"lamp {i}");
            // Reset: the next step is a cut again (NightLights.Rewind, a capture that wants the same picture twice)
            s.Reset();
            s.Step(at, null, null, c, 8, 0, 0f);
            CollectionAssert.AreEqual(Nearest(at, c, 8), ChosenOf(s));
            Assert.AreEqual(8, s.LitCount);
        }

        [Test]
        public void TheKnobAtFortyThreeOrMore_IsTheNightAsItWas_EveryLampWhole()
        {
            foreach (int n in new[] { 43, 49 })       // the field's count, and the most the caps allow
            foreach (int budget in new[] { 43, 44, 1000 })
            {
                var at = Field(n);
                var s = new RealLampSet();
                var focus = new Vector3(10f, 0f, 12f);
                for (int f = 0; f < 120; f++)
                {
                    focus += new Vector3(0.3f, 0f, 0.8f);
                    s.Step(at, null, null, focus, budget, 4, Dt);
                    Assert.AreEqual(n, s.LitCount, $"{n} lamps, budget {budget}, frame {f}");
                    for (int i = 0; i < n; i++) Assert.AreEqual(1f, s.Level(i), $"lamp {i}: exactly 1, so its light is the same number as before the rule");
                }
            }
            // and from eight to everything is at once: turning the rule off never leaves a lamp half lit
            var back = new RealLampSet(); var lamps = Field();
            back.Step(lamps, null, null, Vector3.zero, 8, 0, Dt);
            back.Step(lamps, null, null, Vector3.zero, 43, 0, Dt);
            Assert.AreEqual(43, back.LitCount);
            for (int i = 0; i < 43; i++) Assert.AreEqual(1f, back.Level(i));
            // the rule holds only where the painted pools are drawn: by day, or with look.pools 0, a painted-only lamp would light nothing
            Assert.AreEqual(8, NightLights.BudgetFor(8, true, 1f));
            Assert.AreEqual(RealLampSet.Everything, NightLights.BudgetFor(8, false, 1f), "by day: every lamp");
            Assert.AreEqual(RealLampSet.Everything, NightLights.BudgetFor(8, true, 0f), "look.pools 0, the old night: every lamp");
            Assert.AreEqual(0, NightLights.BudgetFor(-3, true, 1f));
        }

        [Test]
        public void ALampByWaterInThePicture_StaysRealBeyondTheEight_UpToItsOwnCap()
        {
            var at = Field();
            var focus = new Vector3(40f, 0f, 45f);
            var eight = Nearest(at, focus, 8);
            var byDist = new List<int>();   // every lamp, nearest first
            for (int i = 0; i < 43; i++) byDist.Add(i);
            byDist.Sort((a, b) => ((at[a] - focus).x * (at[a] - focus).x + (at[a] - focus).z * (at[a] - focus).z).CompareTo((at[b] - focus).x * (at[b] - focus).x + (at[b] - focus).z * (at[b] - focus).z));
            // flagged: one of the eight (it needs no extra place), and the 10th, 14th, 20th, 25th, 30th and 40th nearest
            var extra = new bool[43];
            extra[byDist[2]] = true;
            foreach (int r in new[] { 9, 13, 19, 24, 29, 39 }) extra[byDist[r]] = true;

            var s = new RealLampSet();
            s.Step(at, null, extra, focus, 8, 4, Dt);
            var want = new List<int>(eight) { byDist[9], byDist[13], byDist[19], byDist[24] }; want.Sort();
            CollectionAssert.AreEqual(want, ChosenOf(s), "the eight, and the four flagged lamps nearest the view after them");
            Assert.AreEqual(12, s.LitCount);

            var none = new RealLampSet();
            none.Step(at, null, extra, focus, 8, 0, Dt);
            CollectionAssert.AreEqual(eight, ChosenOf(none), "lights.waterLamps 0: the eight alone");

            var two = new RealLampSet();
            two.Step(at, null, extra, focus, 8, 2, Dt);
            Assert.AreEqual(10, two.ChosenCount);
            Assert.IsTrue(two.Chosen(byDist[9]) && two.Chosen(byDist[13]) && !two.Chosen(byDist[19]));

            // the flag going (the lamp leaves the picture) fades its light out, as any other lamp's
            extra[byDist[9]] = false;
            s.Step(at, null, extra, focus, 8, 4, Dt);
            Assert.IsFalse(s.Chosen(byDist[9])); Assert.That(s.Weight(byDist[9]), Is.InRange(0.9f, 0.999f), "one step of the fade, not out at once");
            Assert.IsTrue(s.Chosen(byDist[29]), "and the next flagged lamp takes the place");
        }

        [Test]
        public void ByWater_IsTheWaterSheetWithinSixMetres_NotDryGround()
        {
            var map = new MapData(0, new float2(60f, 60f), Allocator.Persistent);
            try
            {
                // level ground at 1 m, a pond 10 m square sunk to 0 m at x 30-40, z 20-30; the water stands at 0.5 m
                float cell = map.Height.CellSize;
                for (int z = 0; z < map.Height.Length; z++)
                for (int x = 0; x < map.Height.Width; x++)
                {
                    float wx = x * cell, wz = z * cell;
                    map.Height.Cm[map.Height.Index(x, z)] = (short)(wx >= 30f && wx <= 40f && wz >= 20f && wz <= 30f ? 0 : 100);
                }
                Assert.IsFalse(NightLights.ByWater(map, new Vector3(35f, 1f, 25f)), "no water level set: no lamp is by water");
                map.WaterLevel = 0.5f;
                Assert.IsTrue(NightLights.ByWater(map, new Vector3(35f, 1f, 25f)), "a lamp standing in the pond");
                Assert.IsTrue(NightLights.ByWater(map, new Vector3(27f, 2.5f, 25f)), "three metres from its edge");
                Assert.IsTrue(NightLights.ByWater(map, new Vector3(25f, 2.5f, 25f)), "five metres from its edge");
                Assert.IsFalse(NightLights.ByWater(map, new Vector3(21f, 2.5f, 25f)), "nine metres off: its light does not reach the water");
                Assert.IsFalse(NightLights.ByWater(map, new Vector3(5f, 2.5f, 50f)), "dry ground");
                Assert.IsFalse(NightLights.ByWater(null, Vector3.zero));
            }
            finally { map.Dispose(); }
        }

        // ---- the wiring: NightLights' own lights, built by its Start, with a field of lamps hung by hand

        const BindingFlags Any = BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public;
        static T Private<T>(NightLights n, string name) where T : class
        {
            var f = typeof(NightLights).GetField(name, Any);
            Assert.IsNotNull(f, "NightLights." + name + " (renamed? this test reads it by name)");
            return f.GetValue(n) as T;
        }
        static object Call(NightLights n, string name, params object[] args)
        {
            var m = typeof(NightLights).GetMethod(name, Any);
            Assert.IsNotNull(m, "NightLights." + name + " (renamed? this test calls it by name)");
            return m.Invoke(n, args);
        }
        static void Set(NightLights n, string name, object value)
        {
            var f = typeof(NightLights).GetField(name, Any);
            Assert.IsNotNull(f, "NightLights." + name);
            f.SetValue(n, value);
        }

        struct Seen { public bool On; public float Intensity, Range; public Color Color; public Vector3 At; }

        [Test]
        public void OnlyTheFixedLampsAreSwitched_FlashesCraterGlowsTheStarShellAndMachineLightsAreLeftAlone()
        {
            bool night = SceneMood.Night;
            var go = new GameObject("night lights under test") { hideFlags = HideFlags.DontSave };
            var camGo = new GameObject("lens under test") { hideFlags = HideFlags.DontSave };
            NightLights lights = null;
            try
            {
                Knobs.Clear();
                SceneMood.Night = true;
                lights = go.AddComponent<NightLights>();
                Call(lights, "Start");   // the flash pool, the crater glows, the star shell, the machines' lights
                var cam = camGo.AddComponent<Camera>();
                cam.enabled = false;
                cam.transform.SetPositionAndRotation(new Vector3(40f, 40f, 0f), Quaternion.Euler(45f, 0f, 0f));

                // 43 lamps, as Build hangs them
                var at = Field();
                var lanterns = Private<List<Light>>(lights, "lanterns");
                var lanternBase = Private<List<float>>(lights, "lanternBase"); var level = Private<List<float>>(lights, "lanternLevel");
                var home = Private<List<Vector3>>(lights, "lanternHome"); var phase = Private<List<float>>(lights, "lanternPhase");
                for (int i = 0; i < at.Count; i++)
                {
                    var l = (Light)Call(lights, "MakeLight", "Lantern " + i, lights.Lantern, 5f + 0.01f * i, 10f);
                    l.transform.position = at[i];
                    lanterns.Add(l); lanternBase.Add(l.intensity); level.Add(l.intensity); home.Add(at[i]); phase.Add(i);
                }
                Call(lights, "BuiltLamps", new object[] { null });
                Set(lights, "poolStrength", 1f);   // the painted pools are drawn (look.pools): the rule holds
                Assert.AreEqual(43, lights.FixedLamps);

                // every other light NightLights owns, lit, each with its own numbers
                var others = new List<Light>(); var seen = new List<Seen>();
                var kinds = new Dictionary<string, int>();
                foreach (var l in go.GetComponentsInChildren<Light>(true))
                {
                    if (lanterns.Contains(l)) continue;
                    l.enabled = true; l.intensity = 3f + others.Count; l.range = 7f + others.Count * 0.5f;
                    l.transform.position = new Vector3(40f + others.Count, 2f, 44f);   // in among the lamps the view is on
                    others.Add(l);
                    seen.Add(new Seen { On = l.enabled, Intensity = l.intensity, Range = l.range, Color = l.color, At = l.transform.position });
                    string kind = l.name.TrimEnd('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ' ');
                    kinds.TryGetValue(kind, out int k); kinds[kind] = k + 1;
                }
                Assert.AreEqual(NightLights.PoolSize, kinds["Flash"], "the flash pool (gun and shell flashes)");
                Assert.AreEqual(3, kinds["Crater glow"]);
                Assert.AreEqual(1, kinds["Star shell"]);
                Assert.AreEqual(NightLights.DefaultMachinePool, kinds["Machine light"], "the machines' fires and cook-offs");

                var focus = new Vector3(40f, 0f, 45f);
                Call(lights, "RealLampsFor", cam, focus);
                Assert.AreEqual(8, lights.RealLampsLit, "eight fixed lamps keep their Light");
                var nearest = Nearest(at, focus, 8);
                for (int i = 0; i < 43; i++)
                {
                    Assert.AreEqual(nearest.Contains(i), lanterns[i].enabled, $"lamp {i}");
                    if (lanterns[i].enabled) Assert.AreEqual(level[i], lanterns[i].intensity, $"lamp {i}: a real lamp burns exactly as it did");
                }
                // the view moves and jumps, the knob goes to none and back: no other light is touched
                foreach (var f2 in new[] { focus + new Vector3(6f, 0f, 9f), new Vector3(5f, 0f, 5f), new Vector3(80f, 0f, 100f) })
                    Call(lights, "RealLampsFor", cam, f2);
                Set(lights, "realBudget", 0);
                Call(lights, "RealLampsFor", cam, new Vector3(-200f, 0f, 0f));
                Assert.AreEqual(0, lights.RealLampsLit, "lights.realLamps 0: every fixed lamp painted only");
                for (int k = 0; k < others.Count; k++)
                {
                    var l = others[k]; var s = seen[k];
                    Assert.AreEqual(s.On, l.enabled, l.name); Assert.AreEqual(s.Intensity, l.intensity, l.name); Assert.AreEqual(s.Range, l.range, l.name);
                    Assert.AreEqual(s.Color, l.color, l.name); Assert.AreEqual(s.At, l.transform.position, l.name);
                }

                // 43: the night as it was, every lamp on with its own light
                Set(lights, "realBudget", 43);
                Call(lights, "RealLampsFor", cam, focus);
                Assert.AreEqual(43, lights.RealLampsLit);
                for (int i = 0; i < 43; i++) Assert.AreEqual(level[i], lanterns[i].intensity, $"lamp {i}: the same number as before the rule");

                // by day, and with the pools off, the rule stands aside whatever the knob says
                Set(lights, "realBudget", 8);
                SceneMood.Night = false;
                Call(lights, "RealLampsFor", cam, focus);
                Assert.AreEqual(43, lights.RealLampsLit, "by day there are no painted pools: every lamp keeps its light");
                SceneMood.Night = true; Set(lights, "poolStrength", 0f);
                Call(lights, "RealLampsFor", cam, focus);
                Assert.AreEqual(43, lights.RealLampsLit, "look.pools 0: every lamp keeps its light");
                Set(lights, "poolStrength", 1f);

                // a lamp whose post goes down is out for good: dark at once, never chosen again, its place goes to the next
                Call(lights, "RealLampsFor", cam, focus);
                int gone = nearest[0];
                Assert.IsTrue(lanterns[gone].enabled);
                Call(lights, "LampOut", at[gone], 0.5f);
                Assert.IsTrue(lights.LampIsOut(gone)); Assert.IsFalse(lanterns[gone].enabled, "dark the moment its post goes");
                Call(lights, "RealLampsFor", cam, focus);
                Assert.IsFalse(lanterns[gone].enabled); Assert.IsFalse(lights.RealLamps.Chosen(gone));
                Assert.AreEqual(8, lights.RealLamps.ChosenCount, "the ninth nearest has its place");
                Set(lights, "realBudget", 43);
                Call(lights, "RealLampsFor", cam, focus);
                Assert.IsFalse(lanterns[gone].enabled, "and the knob at 43 does not light it again: it was out before the rule too");
                Assert.AreEqual(42, lights.RealLampsLit);
                for (int k = 0; k < others.Count; k++) Assert.AreEqual(seen[k].Intensity, others[k].intensity, others[k].name);
            }
            finally
            {
                SceneMood.Night = night;
                SceneHooks.Flash = null; SceneHooks.FireLight = null; SceneHooks.MachineGlows = null; SceneHooks.MachineLight = null; SceneHooks.LampOut = null;
                if (lights != null)
                {
                    var owned = Private<List<Object>>(lights, "owned");
                    if (owned != null) foreach (var o in owned) if (o != null) Object.DestroyImmediate(o);
                }
                Object.DestroyImmediate(camGo);
                Object.DestroyImmediate(go);
                Knobs.Clear();
            }
        }

        [Test]
        public void ThePaintedPools_GoFirstToTheFlamesInThePicture_NotToThoseUnderTheCamera()
        {
            Assert.AreEqual(1f, NightLights.DefaultPoolsByView, "on by default");
            Assert.AreEqual(1f, Knobs.Get("look.poolsByView", NightLights.DefaultPoolsByView));
            // the play view: the lens 30 m above the ground it looks at and 64 m back from it, the ground 2 m above y = 0
            var lens = new Vector3(40f, 32f, 10f);
            var forward = Quaternion.Euler(25f, 0f, 0f) * Vector3.forward;
            Vector3 focus = NightLights.FocusOn(lens, forward, 2f);
            Assert.AreEqual(2f, focus.y, 1e-3f, "on the ground, not on the plane under it");
            Assert.AreEqual(10f + 30f / Mathf.Tan(25f * Mathf.Deg2Rad), focus.z, 1e-2f);
            Assert.AreEqual(40f, focus.x, 1e-3f);
            Assert.AreEqual(lens, NightLights.FocusOn(lens, forward, 50f), "a lens under the ground looks at where it stands");

            // three lamps: far up the picture, near the bottom of the picture, and under the camera (out of the picture)
            var far = new Vector3(42f, 3.5f, focus.z + 40f); var near = new Vector3(38f, 3.5f, focus.z - 25f); var under = new Vector3(40f, 3.5f, 12f);
            float r = NightLights.PoolReach;
            float Key(bool byView, bool seen, Vector3 p) => NightLights.PoolKey(byView, seen, (p - lens).sqrMagnitude, (p - focus).sqrMagnitude, r);
            // as before (look.poolsByView 0): by the distance from the lens alone, so the lamp nobody sees came first
            Assert.Less(Key(false, false, under), Key(false, true, near));
            Assert.Less(Key(false, true, near), Key(false, true, far));
            Assert.AreEqual(NightLights.PoolRank((near - lens).sqrMagnitude, r), Key(false, true, near), "exactly the old rank");
            // by the view: every lamp in the picture before any lamp out of it, the nearer to the lens (the larger on screen) first
            Assert.Less(Key(true, true, near), Key(true, true, far), "of two lamps in the picture, the one at the bottom of it first");
            Assert.Less(Key(true, true, far), Key(true, false, under), "a lamp deep in the picture still comes before one under the camera");
            Assert.AreEqual(Key(false, true, near), Key(true, true, near), "in the picture, the rank is the old one: what had a pool keeps it");
            // out of the picture, the nearer to the picture's middle first: its pool is the likelier to show at the edge
            var beside = new Vector3(40f + 45f, 3.5f, focus.z); var behind = new Vector3(40f, 3.5f, -60f);
            Assert.Less(Key(true, false, beside), Key(true, false, behind));
            // and a burning wreck's wide pool still counts as nearer than a lamp at the same distance, seen or not
            Assert.Less(NightLights.PoolKey(true, true, 1600f, 400f, 13f), NightLights.PoolKey(true, true, 1600f, 400f, r));
            Assert.Less(NightLights.PoolKey(true, false, 1600f, 400f, 13f), NightLights.PoolKey(true, false, 1600f, 400f, r));
            Assert.Greater(NightLights.OutOfPicture, 600f * 600f, "more than any distance on a field, squared");
        }

        /// <summary>The lamps chosen at each still of a view walking 3 m forward over `map`, a quarter metre a still, by
        /// the focus NightLights itself works out for the camera (ViewFocus, called by name); and the largest step the
        /// focus took between two stills.</summary>
        static List<string> WalkTheView(MapData map, List<Vector3> at, out float largestStep)
        {
            var hostGo = new GameObject("host under test") { hideFlags = HideFlags.DontSave }; hostGo.SetActive(false);
            var go = new GameObject("night lights under test") { hideFlags = HideFlags.DontSave }; go.SetActive(false);
            var camGo = new GameObject("lens under test") { hideFlags = HideFlags.DontSave };
            var host = hostGo.AddComponent<SimHost>();
            var local = typeof(SimHost).GetProperty("Local");
            try
            {
                // a host whose match has this map and nothing else: a focus that reads the ground reads Host.Local.Map
                Assert.IsNotNull(local, "SimHost.Local (renamed? this test sets it by name)");
                var match = (TW.Sim.Match.MatchSim)System.Runtime.Serialization.FormatterServices.GetUninitializedObject(typeof(TW.Sim.Match.MatchSim));
                match.Map = map;
                local.GetSetMethod(true).Invoke(host, new object[] { match });
                var lights = go.AddComponent<NightLights>();
                lights.Host = host;
                var cam = camGo.AddComponent<Camera>();
                cam.enabled = false;
                var rot = Quaternion.Euler(25f, 0f, 0f);   // the play view's pitch, looking up the field
                var s = new RealLampSet(); var sets = new List<string>();
                largestStep = 0f; Vector3 before = Vector3.zero;
                for (int k = 0; k <= 12; k++)
                {
                    cam.transform.SetPositionAndRotation(new Vector3(40f, 30f, -6f + 0.25f * k), rot);
                    var focus = (Vector3)Call(lights, "ViewFocus", cam);
                    if (k > 0) largestStep = Mathf.Max(largestStep, new Vector2(focus.x - before.x, focus.z - before.z).magnitude);
                    before = focus;
                    s.Step(at, null, null, focus, 8, 0, 0.1f);
                    sets.Add(string.Join(",", ChosenOf(s)));
                }
                return sets;
            }
            finally
            {
                if (local != null) local.GetSetMethod(true).Invoke(host, new object[] { null });   // the host never built that match: it must not dispose it
                Object.DestroyImmediate(camGo); Object.DestroyImmediate(go); Object.DestroyImmediate(hostGo);
            }
        }

        [Test]
        public void TheFocus_DoesNotStepWithTheGroundUnderIt_ATrenchEdgeLightsNoNinthLamp()
        {
            // review NL.1 (the orange sandbags): the focus was worked out from one sample of the carved ground, so it leapt
            // about 4 m as that sample crossed a trench 2 m deep, and a ninth lamp was chosen for the stills it sat in.
            // Level ground 2 m above y = 0, 120 m square; in one of the two fields a trench across it, its floor at y = 0
            // under z 59 to 60: the view's y = 0 point (64.3 m ahead of the lens at pitch 25) walks through it.
            MapData Ground(bool trench)
            {
                var map = new MapData(0, new float2(120f, 120f), Allocator.Persistent);
                float cell = map.Height.CellSize;
                for (int z = 0; z < map.Height.Length; z++)
                for (int x = 0; x < map.Height.Width; x++)
                    map.Height.Cm[map.Height.Index(x, z)] = (short)(trench && z * cell >= 59f && z * cell <= 60f ? 0 : 200);
                return map;
            }
            // seven lamps under the view, then two for the eighth place: lamp 7 at z 36 and lamp 8 at z 80, so lamp 8 is
            // the nearer once the focus is past z 58 and takes the place from lamp 7 once it is past z 59 (KeepMetres);
            // the other 34 far off
            var at = new List<Vector3>();
            for (int i = 0; i < 7; i++) at.Add(new Vector3(37f + i, 3.5f, 58f));
            at.Add(new Vector3(40f, 3.5f, 36f)); at.Add(new Vector3(40f, 3.5f, 80f));
            for (int i = 0; i < 34; i++) at.Add(new Vector3(100f + (i % 6) * 3f, 3.5f, 100f + (i / 6) * 3f));
            MapData level = Ground(false), dug = Ground(true);
            try
            {
                Assert.AreEqual(2f, RenderGround.Sample(level, 40f, 59.5f), 0.01f, "the level field");
                Assert.Less(RenderGround.Sample(dug, 40f, 59.5f), 0.6f, "the trench is in the walk's way");
                // this field can show the fault: a focus worked out from the ground sampled under the view (as it was)
                // keeps lamp 7 over level ground, and is thrown 4 m on by the trench, onto lamp 8
                List<string> Stepping(MapData map)
                {
                    var s = new RealLampSet(); var sets = new List<string>();
                    Vector3 forward = Quaternion.Euler(25f, 0f, 0f) * Vector3.forward;
                    for (int k = 0; k <= 12; k++)
                    {
                        var lens = new Vector3(40f, 30f, -6f + 0.25f * k);
                        Vector3 p = NightLights.FocusOf(lens, forward);
                        s.Step(at, null, null, NightLights.FocusOn(lens, forward, RenderGround.Sample(map, p.x, p.z)), 8, 0, 0.1f);
                        sets.Add(string.Join(",", ChosenOf(s)));
                    }
                    return sets;
                }
                CollectionAssert.AreNotEqual(Stepping(level), Stepping(dug), "this field does not show the fault: a focus that steps with the ground should light another lamp over the trench");
                // the game's own focus, over both fields
                var onLevel = WalkTheView(level, at, out float levelStep);
                var overTrench = WalkTheView(dug, at, out float trenchStep);
                CollectionAssert.AreEqual(onLevel, overTrench, "the trench under the view changes no lamp: the same eight at every still as over level ground");
                Assert.AreEqual(0.25f, levelStep, 0.01f, "over level ground the focus moves as the lens does");
                Assert.AreEqual(0.25f, trenchStep, 0.01f, "and over the trench too: it does not leap as the ground under it steps");
            }
            finally { level.Dispose(); dug.Dispose(); }
        }
    }
}
