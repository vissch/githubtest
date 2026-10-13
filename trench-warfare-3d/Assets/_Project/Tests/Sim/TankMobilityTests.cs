// Phase: A5b (2026-09-24) — a tank's track fails by degrees, the way a walker's legs always have.
// Owner: "insect vehicles that run slower when they loose a leg" (already true, 16 % a leg) and the same grading for
// armour, which used to be a switch: whole, or thrown and the tank stuck where it stood.
// These are the curves as a table, so a tuning pass shows up here rather than in a playtest.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Sim.Units;

namespace TW.Tests
{
    public class TankMobilityTests
    {
        const float Tol = 0.005f;

        static MatchSim NewMatch(uint seed = 0xC0FFEE)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.Seed = seed;
            return MatchSim.CreatePlaytest(cfg);
        }

        static void Step(MatchSim m)
        {
            using var arr = new NativeArray<SimCommand>(0, Allocator.Temp);
            m.Step(arr);
        }

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

        // [T26] the old test only did arithmetic on two constants and never ran the sim's own SpeedFactor
        // calculation (code review 2026-10-04, T26). This drives a real Pincer (six legs, the most any walker
        // has — no archetype has eight) through VehicleModulesSystem and reads Vehicles.SpeedFactor back.
        static int SpawnPincer(MatchSim m)
        {
            var entry = RosterEntry.Pincer;
            return m.World.Spawn(0, VehicleArchetype.Pincer, new float3(30f, 0f, 30f), entry.Hp, 0f, true);
        }

        [Test]
        public void AWalkerStillLimpsAtSixteenPerCentALegAndStopsAtItsFloor()
        {
            // the rule the owner already liked, now named rather than inline
            Assert.AreEqual(0.16f, VehicleModulesSystem.LegLoss, 1e-4f);
            Assert.AreEqual(0.25f, VehicleModulesSystem.LegFloor, 1e-4f, "and it never crawls slower than this");

            using var m = NewMatch();
            int crab = SpawnPincer(m);
            Step(m);
            Assert.AreEqual(6, VehicleProfile.Pincer.Legs, "a Pincer has six legs");

            m.Modules.LegsLost[crab] = 0b000001; // one leg, left side
            Step(m);
            Assert.AreEqual(0.84f, m.Vehicles.SpeedFactor[crab], 1e-3f, "[T26] one leg gone, full crew, sound engine");

            m.Modules.LegsLost[crab] = 0b000011; // two legs, left side
            Step(m);
            Assert.AreEqual(0.68f, m.Vehicles.SpeedFactor[crab], 1e-3f, "[T26] two legs gone");

            m.Modules.LegsLost[crab] = 0b111111; // all six legs, three a side
            Step(m);
            Assert.AreEqual(0.25f, m.Vehicles.SpeedFactor[crab], 1e-3f, "[T26] every leg gone stops at the floor, not at zero");
        }
    }
}
