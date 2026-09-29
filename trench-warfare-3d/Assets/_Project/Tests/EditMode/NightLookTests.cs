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
        }

        [Test]
        public void ThePools_TurnAFlameOrange_NotPale()
        {
            var w = NightLights.PoolWarmth;
            Assert.AreEqual(1f, w.x, 1e-6f);
            Assert.Less(w.y, 1f); Assert.Less(w.z, w.y, "blue goes most: orange, not the pale sand the lantern colour alone made");
            Assert.AreEqual(32, NightLights.MaxPools, "TW_MAX_POOLS in TWLightPools.hlsl");
        }
    }
}
