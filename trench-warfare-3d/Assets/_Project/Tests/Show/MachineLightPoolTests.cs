// Phase: A5c (2026-09-29, the Dust Front lessons: vehicle weight) — MachineLightSlots, the choice behind NightLights'
// machine light pool (lights.machinePool), and the lamps' two pure rules (colour, card size). A field of forty burning
// machines must never light more than the pool holds, and the light a cook-off or a fire has must not be taken by a
// lesser one: the shared flash pool, which a barrage empties, is what this pool exists to stand apart from.
using System.Collections.Generic;
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class MachineLightPoolTests
    {
        const int Fire = 2, CookOff = 3, Furnace = 1;
        const BindingFlags Any = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;

        static void Same(Color want, Color got, string what)
        {
            Assert.AreEqual(want.r, got.r, 1e-5f, what); Assert.AreEqual(want.g, got.g, 1e-5f, what);
            Assert.AreEqual(want.b, got.b, 1e-5f, what); Assert.AreEqual(want.a, got.a, 1e-5f, what);
        }

        static int Lit(MachineLightSlots s, float now)
        {
            int n = 0;
            for (int i = 0; i < s.Count; i++) if (s.Level(i, now) > 0f) n++;
            return n;
        }

        [Test]
        public void FortyBurningMachines_NeverLightMoreThanThePoolHolds_AndTheBrightestKeepThem()
        {
            var s = new MachineLightSlots(4);
            float now = 0f;
            for (int frame = 0; frame < 120; frame++, now += 1f / 60f)
            {
                s.Sweep(now);
                // machine 39 burns brightest; they ask dimmest first one frame and brightest first the next, ending on
                // brightest first, so a pool that let any fire take any other's light would end with the four dimmest
                for (int q = 0; q < 40; q++) { int m = (frame & 1) == 0 ? q : 39 - q; s.Request(m * 4 + 1, Fire, 3f + m * 0.1f, 0f, now); }
                Assert.LessOrEqual(Lit(s, now), 4, $"frame {frame}");
            }
            Assert.AreEqual(4, Lit(s, now), "the pool is full while they burn");
            // equal priority: a fire takes a dimmer fire's light, never one as bright as itself, so the four held at
            // the end are the four brightest
            var keys = new System.Collections.Generic.List<int>();
            for (int i = 0; i < s.Count; i++) keys.Add(s.KeyOf(i));
            keys.Sort();
            CollectionAssert.AreEqual(new[] { 36 * 4 + 1, 37 * 4 + 1, 38 * 4 + 1, 39 * 4 + 1 }, keys);
        }

        [Test]
        public void ACookOffTakesAFiresLight_AFurnaceCannot_AndAFireKeepsItsOwnAgainstAnEqualFire()
        {
            var s = new MachineLightSlots(2);
            Assert.AreEqual(0, s.Request(1, Fire, 6f, 0f, 0f));
            Assert.AreEqual(1, s.Request(5, Fire, 4f, 0f, 0f));
            Assert.AreEqual(-1, s.Request(9, Furnace, 9f, 0f, 0f), "a furnace, however bright, ranks below a fire");
            Assert.AreEqual(-1, s.Request(13, Fire, 4f, 0f, 0f), "an equal fire does not take a fire's light");
            int took = s.Request(17, CookOff, 30f, 1.2f, 0f);
            Assert.AreEqual(1, took, "the cook-off takes the dimmer fire's light");
            Assert.AreEqual(17, s.KeyOf(1)); Assert.AreEqual(CookOff, s.PriorityOf(1));
            Assert.AreEqual(1, s.KeyOf(0), "the brighter fire kept its own");
            Assert.AreEqual(0, s.Request(1, Fire, 6.5f, 0f, 0.1f), "the same fire asking again keeps its slot");
        }

        [Test]
        public void AHeldLightStaysWholeAFrameOn_ThenFades_AndIsFreed()
        {
            var s = new MachineLightSlots(1);
            s.Request(7, Fire, 8f, 0f, 1f);
            Assert.AreEqual(8f, s.Level(0, 1f + 1f / 60f), 1e-5f, "asked in LateUpdate, lit whole in the next frame's Update");
            Assert.AreEqual(8f, s.Level(0, 1f + MachineLightSlots.HoldGrace), 1e-4f);
            float half = 1f + MachineLightSlots.HoldGrace + MachineLightSlots.HoldFade * 0.5f;
            Assert.AreEqual(4f, s.Level(0, half), 1e-3f, "half-way through its fade, half its light");
            float gone = 1f + MachineLightSlots.HoldGrace + MachineLightSlots.HoldFade + 0.01f;
            Assert.AreEqual(0f, s.Level(0, gone));
            s.Sweep(gone);
            Assert.AreEqual(-1, s.KeyOf(0), "a light gone out frees its slot");
            Assert.AreEqual(0, s.Request(11, Furnace, 1f, 0f, gone), "which even a furnace may then have");
        }

        [Test]
        public void AFlashFallsAwayOverItsLife_AndAskedAgainStartsAgain()
        {
            var s = new MachineLightSlots(1);
            s.Request(2, CookOff, 30f, 1.2f, 0f);
            Assert.AreEqual(30f, s.Level(0, 0f), 1e-4f);
            Assert.AreEqual(30f * 0.25f, s.Level(0, 0.6f), 1e-3f, "(1 - age) squared, as the shared flash pool falls");
            Assert.AreEqual(0f, s.Level(0, 1.2f));
            s.Request(2, CookOff, 30f, 1.2f, 1f);
            Assert.AreEqual(30f, s.Level(0, 1f), 1e-4f, "asked again, it starts again");
        }

        [Test]
        public void NoPool_NoLight()
        {
            var s = new MachineLightSlots(0);
            Assert.AreEqual(-1, s.Request(1, CookOff, 30f, 1.2f, 0f));
            s.Sweep(1f);   // nothing to sweep, nothing thrown
        }

        [Test]
        public void ABurningWreckAsksForTheFireLight_AndFortyOfThemNeverLightMoreThanThePool()
        {
            var tr = new GameObject("WreckLightUnderTest");
            var camGo = new GameObject("MainCamera");
            try
            {
                var r = tr.AddComponent<TankRenderer>();
                camGo.tag = "MainCamera";
                camGo.AddComponent<Camera>();
                camGo.transform.position = new Vector3(0f, 0f, -5f);
                var cam = Camera.main;
                Assert.NotNull(cam, "a MainCamera-tagged camera in the scene");

                var viewType = r.GetType().GetNestedType("View", BindingFlags.NonPublic | BindingFlags.Public);
                Assert.NotNull(viewType, "TankRenderer has no View");
                var wrecks = (System.Collections.IList)r.GetType().GetField("wrecks", Any).GetValue(r);
                var emit = r.GetType().GetMethod("EmitMachineLights", Any);
                Assert.NotNull(emit, "TankRenderer has no EmitMachineLights");

                object NewWreck(int slot)
                {
                    object v = System.Activator.CreateInstance(viewType, true);
                    viewType.GetField("Model", Any).SetValue(v, new TankModel());
                    viewType.GetField("World", Any).SetValue(v, new[] { Matrix4x4.identity });
                    viewType.GetField("Pos", Any).SetValue(v, cam.transform.position);
                    viewType.GetField("Slot", Any).SetValue(v, slot);
                    viewType.GetField("Fire", Any).SetValue(v, 1f);
                    viewType.GetField("Dead", Any).SetValue(v, true);
                    viewType.GetField("CookOff", Any).SetValue(v, false);
                    viewType.GetField("DiedAt", Any).SetValue(v, 0f);
                    return v;
                }

                SceneHooks.MachineGlows = null;
                var asks = new List<(int key, int priority, float peak)>();
                SceneHooks.MachineLight = (key, priority, at, color, peak, reach, seconds) => asks.Add((key, priority, peak));

                wrecks.Clear();
                wrecks.Add(NewWreck(0));
                emit.Invoke(r, new object[] { 0f });

                Assert.AreEqual(1, asks.Count, "one burning wreck asks once");
                Assert.AreEqual(0 * 4 + 1, asks[0].key);
                Assert.AreEqual(Fire, asks[0].priority);
                float wantPeak = TankRenderer.FireLightPeak(1f) * 0.82f;
                Assert.AreEqual(wantPeak, asks[0].peak, 1e-3f, "FireLightPeak(1) at the flicker's middle, now = 0");

                wrecks.Clear();
                for (int k = 0; k < 40; k++) wrecks.Add(NewWreck(k));
                var slots = new MachineLightSlots(4);
                SceneHooks.MachineLight = (key, priority, at, color, peak, reach, seconds) => slots.Request(key, priority, peak, seconds, 0f);
                emit.Invoke(r, new object[] { 0f });
                Assert.LessOrEqual(Lit(slots, 0f), 4, "forty burning wrecks never light more than the pool holds");
            }
            finally
            {
                SceneHooks.MachineGlows = null; SceneHooks.MachineLight = null;
                Object.DestroyImmediate(tr); Object.DestroyImmediate(camGo);
            }
        }

        [Test]
        public void TheLamps_WearTheSidesColourTheTrailersRedOrTheLanternsAmber_AndHoldOnScreenFarOff()
        {
            Same(TankRenderer.TeamA, TankRenderer.LampColour(0, 0f), "side A");
            Same(TankRenderer.TeamB, TankRenderer.LampColour(1, 0f), "side B");
            Same(TankRenderer.TrailerRed, TankRenderer.LampColour(0, 1f), "the trailer's red");
            Same(TankRenderer.LampAmber, TankRenderer.LampColour(0, 2f), "the lanterns' amber, side A");
            Same(TankRenderer.LampAmber, TankRenderer.LampColour(1, 2f), "and side B: warm light carries no side");
            Same(TankRenderer.LampAmber, TankRenderer.LampColour(1, 3f), "clamped");
            var go = new GameObject("lanterns");
            try { Same(go.AddComponent<TW.Presentation.Terrain.NightLights>().Lantern, TankRenderer.LampAmber, "the same amber as the trench's lanterns"); }
            finally { Object.DestroyImmediate(go); }
            Assert.AreEqual(1.2f, TankRenderer.LampCard(1.2f, 20f), 1e-6f, "close by, its own size");
            Assert.AreEqual(2.25f, TankRenderer.LampCard(1.2f, 150f), 1e-4f, "far off, 0.015 of its distance");
            float px20 = TankRenderer.LampCard(1.2f, 20f) / 20f, px150 = TankRenderer.LampCard(1.2f, 150f) / 150f;
            Assert.GreaterOrEqual(px150, 0.015f - 1e-6f, "never under 0.015 radians across, however far");
            Assert.Greater(px20, px150, "and nearer, larger on the screen");
        }
    }
}
