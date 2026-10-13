// Phase: review-fix (2026-10-14) — pure maths for the landing craft's wake rings and its ride between ticks.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class LandingCraftRideTests
    {
        [Test]
        public void AOneCraftWakeDoesNotWrapTheRingBuffer()
        {
            // the time a lone craft afloat takes to overwrite every one of the 16 ring slots
            float ringsPerSecond = LandingCraftView.WakeRingsPerStep / LandingCraftView.WakeInterval;
            float wrapSeconds = WaterRings.Count / ringsPerSecond;
            Assert.GreaterOrEqual(wrapSeconds, 1.9f,
                "[W1] one craft afloat wraps all 16 ring slots before the shortest ring has faded");
        }

        [Test]
        public void TheHullRidesBetweenTicks()
        {
            // prev at z 100, now at z 100.35: SeaLandingSystem.RunSpeed (7 m/s) times a 0.05 s tick
            var prev = new Vector3(0f, 0f, 100f);
            var now = new Vector3(0f, 0f, 100.35f);
            Assert.AreEqual(100f, LandingCraftView.Ride(prev, now, 0f).z, 1e-4f, "[W2] the hull is drawn at the raw tick position");
            Assert.AreEqual(100.175f, LandingCraftView.Ride(prev, now, .5f).z, 1e-4f, "[W2] the hull is drawn at the raw tick position");
            Assert.AreEqual(100.35f, LandingCraftView.Ride(prev, now, 1f).z, 1e-4f, "[W2] the hull is drawn at the raw tick position");

            float yaw = LandingCraftView.RideYaw(350f * Mathf.Deg2Rad, 10f * Mathf.Deg2Rad, .5f);
            Assert.AreEqual(0f, yaw, 1e-4f, "[W2] the yaw turns the short way round, not the long way through 180");
        }
    }
}
