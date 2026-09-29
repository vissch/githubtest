// Phase: A5c (2026-09-29, the Dust Front lessons: vehicle weight) — MachineLightSlots, the choice behind NightLights'
// machine light pool (lights.machinePool), and the lamps' two pure rules (colour, card size). A field of forty burning
// machines must never light more than the pool holds, and the light a cook-off or a fire has must not be taken by a
// lesser one: the shared flash pool, which a barrage empties, is what this pool exists to stand apart from.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class MachineLightPoolTests
    {
        const int Fire = 2, CookOff = 3, Furnace = 1;

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
        public void TheLamps_WearTheSidesColourOrTheTrailersRed_AndHoldOnScreenFarOff()
        {
            Same(TankRenderer.TeamA, TankRenderer.LampColour(0, 0f), "side A");
            Same(TankRenderer.TeamB, TankRenderer.LampColour(1, 0f), "side B");
            Same(TankRenderer.TrailerRed, TankRenderer.LampColour(0, 1f), "the trailer's red");
            Same(TankRenderer.TrailerRed, TankRenderer.LampColour(1, 2f), "clamped");
            Assert.AreEqual(0.45f, TankRenderer.LampCard(0.45f, 20f), 1e-6f, "close by, its own size");
            Assert.AreEqual(0.9f, TankRenderer.LampCard(0.45f, 150f), 1e-4f, "far off, 0.006 of its distance");
            float px20 = TankRenderer.LampCard(0.45f, 20f) / 20f, px150 = TankRenderer.LampCard(0.45f, 150f) / 150f;
            Assert.Greater(px150, 0.005f, "never under 0.006 radians across, however far");
            Assert.Greater(px20, px150, "and nearer, larger on the screen");
        }
    }
}
