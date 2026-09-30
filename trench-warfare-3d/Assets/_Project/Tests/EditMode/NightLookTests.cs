// Phase: A5d (2026-09-29, the owner's night look) — the knobs that draw today's night at 0, and what they do past it.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class NightLookTests
    {
        [Test]
        public void LiftAtZero_LeavesTheHazeAndTheFogEndAsToday()
        {
            var haze = new Color(0.075f, 0.105f, 0.17f);
            Assert.AreEqual(haze, Atmosphere.Lifted(haze, Atmosphere.LiftHaze, 0f), "the colour");
            Assert.AreEqual(412.5f, Atmosphere.LiftedFogEnd(42.5f, 412.5f, 50f, Atmosphere.LiftReach, 0f), 1e-4f, "the fog's end");
        }

        [Test]
        public void FullLift_ClosesTheFogWithinItsReach_AndLightensTheHaze()
        {
            // at full lift the fog closes reach x the distance looked at past its start, far short of today's 230 m more
            Assert.AreEqual(42.5f + Atmosphere.LiftReach * 50f, Atmosphere.LiftedFogEnd(42.5f, 412.5f, 50f, Atmosphere.LiftReach, 1f), 1e-4f);
            var haze = new Color(0.075f, 0.105f, 0.17f);
            var lifted = Atmosphere.Lifted(haze, Atmosphere.LiftHaze, 1f);
            Assert.Greater(lifted.grayscale, haze.grayscale * 2f, "the distance lifts: lighter than the night haze");
            float Sat(Color c) { Color.RGBToHSV(c, out _, out float s, out _); return s; }
            Assert.Less(Sat(lifted), Sat(haze), "and greyer");
            Assert.Greater(lifted.b, lifted.r, "but still blue: the colour edit keeps it");
        }

        [Test]
        public void TheReach_GrowsWithTheViewDistance_WithinItsCap()
        {
            Assert.AreEqual(1f, Atmosphere.ReachFor(1f, 20f), 1e-6f, "nearer than zoom 30: the knob's own");
            Assert.AreEqual(1f, Atmosphere.ReachFor(1f, Atmosphere.ReachAt), 1e-6f);
            Assert.AreEqual(2f, Atmosphere.ReachFor(1f, 2f * Atmosphere.ReachAt), 1e-5f, "zoom 60: twice, as the sweep chose");
            Assert.AreEqual(Atmosphere.ReachGrowMax, Atmosphere.ReachFor(1f, 1000f), 1e-6f, "capped");
        }

        [Test]
        public void TheNightLook_IsOnByDefault()
        {
            Assert.AreEqual(1f, Atmosphere.DefaultLift); Assert.AreEqual(1f, Atmosphere.DefaultWet); Assert.AreEqual(1f, NightLights.DefaultPools);
            Assert.That(NightLights.DefaultPoolSoft, Is.InRange(0.01f, 0.5f), "softened, but not so far that the pools' warm light spreads thin");
            Assert.Greater(NightLights.DefaultMoreFires, 0, "more fires burn in no man's land than carry a real light");
            Assert.Greater(NightLights.FlareGlowWarm.r, NightLights.FlareGlowWarm.b, "the star shell's glow warm, never a stray blue orb (the owner's rule)");
            Assert.GreaterOrEqual(NightLights.FlareNeutral.r, NightLights.FlareNeutral.b, "and the light it throws not blue");
            Assert.AreEqual(1f, NightLights.DefaultThroughHaze, "the lamps shine through the lifted haze");
            Assert.AreEqual(1f, Atmosphere.DefaultInkFade, "the ink fades with the haze, or distant wire is bare black outlines");
            Assert.Greater(Atmosphere.DefaultGlowHue, 0f, "a distant lamp's glow keeps its colour in the haze, not a grey-white oval");
            Assert.Less(Atmosphere.CurtainPale, 1.9f, "the rain curtains less pale over the lifted haze than the 1.9 that made them white stripes");
            Assert.Less(Atmosphere.DefaultPuddleSky, 1f, "still water mirrors less of the pale sky at night: dark glass, not a pale slab");
            Assert.Greater(Atmosphere.DefaultWaterDim, 0f, "the flooded ground's water sheet is darker at night");
            Assert.Greater(Atmosphere.DefaultGrade, 0f, "the night grade grounds the shadows in umber");
            Assert.Greater(Atmosphere.GradeUmber.x, Atmosphere.GradeUmber.z, "umber: warmer than the blue it replaces");
            Assert.Greater(Atmosphere.LiftHaze.grayscale, 0.35f, "the haze lighter than the lit ground, so the distance's silhouettes stand dark against it");
        }

        [Test]
        public void ThePools_TurnAFlameOrange_NotPale()
        {
            var w = NightLights.PoolWarmth;
            Assert.AreEqual(1f, w.x, 1e-6f);
            Assert.Less(w.y, 1f); Assert.Less(w.z, w.y, "blue goes most: orange, not the pale sand the lantern colour alone made");
            Assert.AreEqual(32, NightLights.MaxPools, "TW_MAX_POOLS in TWLightPools.hlsl");
        }

        [Test]
        public void ABurningWreck_WinsItsPoolOverALampAtTheSameDistance()
        {
            float d2 = 40f * 40f;
            Assert.AreEqual(d2, NightLights.PoolRank(d2, NightLights.PoolReach), 1e-3f, "a lantern ranks by its distance");
            Assert.AreEqual(d2, NightLights.PoolRank(d2, 3f), 1e-3f, "a small flame is not counted farther off");
            Assert.Less(NightLights.PoolRank(d2, 13f), NightLights.PoolRank(30f * 30f, NightLights.PoolReach), "a wreck 13 m across at 40 m beats a lamp at 30 m");
        }

        [Test]
        public void FirePools_AreKept_UntilTheCap_AndNothingForAColdOne()
        {
            var go = new GameObject("NightLookTests lights");
            try
            {
                var lights = go.AddComponent<NightLights>();
                lights.AddFirePool(Vector3.zero, Color.white, 0f, 8f, 1f);
                Assert.AreEqual(0, lights.FirePools, "no strength: no pool");
                for (int k = 0; k < NightLights.MaxFirePools + 10; k++) lights.AddFirePool(new Vector3(k, 0f, 0f), Color.white, 1f, 8f, 1f);
                Assert.AreEqual(NightLights.MaxFirePools, lights.FirePools, "a field of fires stops at the cap");
            }
            finally { Object.DestroyImmediate(go); }
        }
    }
}
