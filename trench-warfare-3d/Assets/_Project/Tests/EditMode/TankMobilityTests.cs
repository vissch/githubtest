// Phase: A5b (2026-09-24) — a tank's track fails by degrees, the way a walker's legs always have.
// Owner: "insect vehicles that run slower when they loose a leg" (already true, 16 % a leg) and the same grading for
// armour, which used to be a switch: whole, or thrown and the tank stuck where it stood.
// These are the curves as a table, so a tuning pass shows up here rather than in a playtest.
using NUnit.Framework;
using TW.Sim.Units;

namespace TW.Tests
{
    public class TankMobilityTests
    {
        const float Tol = 0.005f;

        [Test]
        public void ASoundTrackCostsNothing()
        {
            Assert.AreEqual(1f, VehicleModulesSystem.TrackHealthFactor(1f), Tol);
            Assert.AreEqual(1f, VehicleModulesSystem.TrackHealthFactor(VehicleModulesSystem.TrackFullAbove), Tol);
            Assert.AreEqual(1f, VehicleModulesSystem.TrackFactor(1f, 1f), Tol);
        }

        [Test]
        public void ATornTrackDragsInsteadOfStopping()
        {
            // the middle of the range, and the value a repair leaves behind
            Assert.AreEqual(0.783f, VehicleModulesSystem.TrackHealthFactor(VehicleModulesSystem.RepairTo), 0.01f,
                "a mended track is 0.6 health, which is 78 % of the speed: a repair is not a new tank");
            Assert.AreEqual(0.567f, VehicleModulesSystem.TrackHealthFactor(0.4f), 0.01f);
            Assert.AreEqual(VehicleModulesSystem.TrackWorstFactor, VehicleModulesSystem.TrackHealthFactor(VehicleModulesSystem.TrackThrownBelow), Tol,
                "at the threshold it is at its worst but still moving");
        }

        [Test]
        public void ATrackTornPastItsThresholdIsOff()
        {
            Assert.AreEqual(0f, VehicleModulesSystem.TrackHealthFactor(VehicleModulesSystem.TrackThrownBelow - 0.01f), Tol);
            Assert.AreEqual(0f, VehicleModulesSystem.TrackHealthFactor(0f), Tol);
            Assert.AreEqual(0f, VehicleModulesSystem.TrackFactor(1f, 0f), Tol, "one track off stops the tank");
        }

        [Test]
        public void TheWorseTrackSetsThePaceAndAnUnevenPairCostsMore()
        {
            float even = VehicleModulesSystem.TrackFactor(0.4f, 0.4f);
            float uneven = VehicleModulesSystem.TrackFactor(1f, 0.4f);
            Assert.AreEqual(VehicleModulesSystem.TrackHealthFactor(0.4f), even, Tol, "a matched pair runs at its own health");
            Assert.Less(uneven, even, "one good track and one torn one crabs: slower than two equally torn ones");
            Assert.Greater(uneven, even * (1f - VehicleModulesSystem.TrackMismatch - Tol), "but only by the mismatch");
        }

        [Test]
        public void TheEngineFailsByDegreesToo()
        {
            Assert.AreEqual(1f, VehicleModulesSystem.EngineFactor(1f), Tol);
            Assert.AreEqual(1f, VehicleModulesSystem.EngineFactor(VehicleModulesSystem.EngineFullAbove), Tol);
            Assert.AreEqual(0.89f, VehicleModulesSystem.EngineFactor(0.6f), 0.01f);
            Assert.AreEqual(0.67f, VehicleModulesSystem.EngineFactor(0.4f), 0.01f);
            Assert.AreEqual(VehicleModulesSystem.EngineWorstFactor, VehicleModulesSystem.EngineFactor(VehicleModulesSystem.StalledBelow), Tol);
            Assert.AreEqual(0f, VehicleModulesSystem.EngineFactor(VehicleModulesSystem.StalledBelow - 0.01f), Tol, "below that it is stalled");
        }

        [Test]
        public void BothCurvesRiseWithHealthAndNeverJump()
        {
            float lastTrack = -1f, lastEngine = -1f;
            for (int i = 0; i <= 100; i++)
            {
                float v = i / 100f;
                float t = VehicleModulesSystem.TrackHealthFactor(v), e = VehicleModulesSystem.EngineFactor(v);
                Assert.GreaterOrEqual(t, lastTrack, "a healthier track is never slower");
                Assert.GreaterOrEqual(e, lastEngine, "nor a healthier engine");
                lastTrack = t; lastEngine = e;
            }
            // the only step either curve is allowed is the one at its failure threshold
            float justOff = VehicleModulesSystem.TrackHealthFactor(VehicleModulesSystem.TrackThrownBelow - 1e-4f);
            float justOn = VehicleModulesSystem.TrackHealthFactor(VehicleModulesSystem.TrackThrownBelow);
            Assert.AreEqual(0f, justOff, Tol);
            Assert.AreEqual(VehicleModulesSystem.TrackWorstFactor, justOn, Tol);
        }

        [Test]
        public void AWalkerStillLimpsAtSixteenPerCentALegAndStopsAtItsFloor()
        {
            // the rule the owner already liked, now named rather than inline
            Assert.AreEqual(0.16f, VehicleModulesSystem.LegLoss, 1e-4f);
            Assert.AreEqual(0.84f, 1f - VehicleModulesSystem.LegLoss * 1, 1e-4f, "one leg gone");
            Assert.AreEqual(0.68f, 1f - VehicleModulesSystem.LegLoss * 2, 1e-4f, "two");
            Assert.AreEqual(0.25f, VehicleModulesSystem.LegFloor, 1e-4f, "and it never crawls slower than this");
            float sixLegsGone = 1f - VehicleModulesSystem.LegLoss * 6;
            Assert.Less(sixLegsGone, VehicleModulesSystem.LegFloor, "six legs gone would go under the floor, so the floor holds");
        }
    }
}
