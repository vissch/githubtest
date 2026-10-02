// Phase: coastal level — the sea as a battlefield rather than as the scenery behind ShelledForest's far trench.
//
// The defect these were written for: BattlefieldGenerator.SeaMargin was a 36 m const, while SeaLandingSystem puts
// a craft at StandOff (96 m) seaward of the waterline and a gunboat at ShipStandOff plus its spread (90 m to
// 270 m). ShelledForest's water is 17 m deep in Z, so every run-in and every gunboat sat beyond the map edge, over
// clamped heightfield and outside the nav grid. Nothing threw, because CellOf and Height.Sample both clamp, and
// nothing measured it, so the coast quietly could not carry a level.
//
// These read BOTH constants rather than restating either. If somebody moves the boats further out, or trims a
// map's coast, the test says so instead of the picture silently losing its ships.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class CoastTests
    {
        /// <summary>The furthest thing SeaLandingSystem can place on the water, in metres seaward of the waterline.</summary>
        static float FurthestAfloat => SeaLandingSystem.ShipStandOff + SeaLandingSystem.ShipOutMax;

        /// <summary>How much water a map actually has, in metres of Z beyond the waterline.</summary>
        static float WaterReach(MapData map) => map.SizeMeters.y - map.ShoreZ;

        [Test]
        public void TheLandingMapContainsTheWaterItsOwnBoatsUse()
        {
            using var map = BattlefieldGenerator.Create(BattlefieldParams.Landing(1917), Allocator.Persistent);
            Assert.IsTrue(map.HasSea, "a landing map has a sea");
            Assert.GreaterOrEqual(WaterReach(map), FurthestAfloat,
                $"the map has {WaterReach(map):F0} m of water past the waterline but SeaLandingSystem can put a "
                + $"gunboat {FurthestAfloat:F0} m out. Everything beyond the edge sits on clamped height, outside "
                + "the nav grid, and the run-in the level exists to show happens off the map.");
            Assert.GreaterOrEqual(WaterReach(map), SeaLandingSystem.StandOff,
                "a craft appears at StandOff and must appear on the map");
        }

        /// <summary>
        /// ShelledForest is NOT a landing map and is deliberately left short: its coast is the strip of scenery
        /// behind team 1's rear trench, which is all it was built to be. This records that as understood rather
        /// than leaving the next reader to wonder whether the shortfall above applies to it too - and if anyone
        /// ever widens that coast, this is where they will be told to revisit the question.
        /// </summary>
        [Test]
        public void ShelledForestsCoastIsSceneryAndIsKnownToBeTooShortForBoats()
        {
            using var map = BattlefieldGenerator.Create(BattlefieldParams.ShelledForest(1917), Allocator.Persistent);
            Assert.IsTrue(map.HasSea, "ShelledForest still has its coast");
            Assert.Less(WaterReach(map), SeaLandingSystem.StandOff,
                $"ShelledForest now has {WaterReach(map):F0} m of water, past the {SeaLandingSystem.StandOff:F0} m "
                + "a craft appears at. That is a good thing, but it was a scenery coast when this was written - "
                + "check whether it should now be held to the landing map's standard instead.");
        }

        /// <summary>The shipped map's geometry must not move because a NEW map wanted more water.</summary>
        [Test]
        public void AddingTheDialLeftTheShippedMapExactlyWhereItWas()
        {
            var p = BattlefieldParams.ShelledForest(1917);
            Assert.AreEqual(0f, p.SeaMargin, "ShelledForest asks for no particular margin, so it takes the default");
            Assert.AreEqual(BattlefieldGenerator.SeaMargin, BattlefieldGenerator.SeaMarginOf(p),
                "zero must mean the default, or every map built before the dial existed changes shape");
            using var map = BattlefieldGenerator.Create(p, Allocator.Persistent);
            Assert.AreEqual(p.Length + BattlefieldGenerator.SeaMargin, map.SizeMeters.y, 1e-3f,
                "the shipped map is still its layout plus the default coast");
        }

        [Test]
        public void TheMarginSurvivesBeingWrittenDownAndReadBack()
        {
            var p = BattlefieldParams.Landing(1917);
            var q = BattlefieldParams.Deserialize(p.Serialize());
            Assert.AreEqual(p.SeaMargin, q.SeaMargin, 1e-3f, "a replay of a landing map must land on the same coast");
            Assert.AreEqual(p.Sea, q.Sea);
            Assert.AreEqual(p.Length, q.Length, 1e-3f);
            Assert.AreEqual(p.Width, q.Width, 1e-3f);
        }

        /// <summary>
        /// A mission is a BattlefieldParams value, not a saved map (BattlefieldGenerator's own header), so the
        /// preset has to give the same ground on every machine or a landing desyncs before the first shot.
        /// </summary>
        [Test]
        public void TheLandingMapIsTheSameGroundEveryTime()
        {
            using var a = BattlefieldGenerator.Create(BattlefieldParams.Landing(1917), Allocator.Persistent);
            using var b = BattlefieldGenerator.Create(BattlefieldParams.Landing(1917), Allocator.Persistent);
            Assert.AreEqual(a.Hash(0ul), b.Hash(0ul), "same seed and params, same coast");
            using var c = BattlefieldGenerator.Create(BattlefieldParams.Landing(1918), Allocator.Persistent);
            Assert.AreNotEqual(a.Hash(0ul), c.Hash(0ul), "a different seed is a different beach, or the seed is ignored");
        }

        /// <summary>
        /// The beach has to be somewhere a man can stand and fight, not a cliff or a mudflat. Walked rather than
        /// assumed: the sand between the top of the beach and the waterline must fall, and must not fall so fast
        /// that a landed man is climbing.
        /// </summary>
        [Test]
        public void TheBeachIsWalkableFromTheWaterToTheTopOfIt()
        {
            using var map = BattlefieldGenerator.Create(BattlefieldParams.Landing(1917), Allocator.Persistent);
            float x = map.SizeMeters.x * 0.5f;
            float top = map.Height.Sample(x, map.SeaStartZ), water = map.Height.Sample(x, map.ShoreZ);
            Assert.Greater(top, water, "the beach falls from the top of it to the waterline");
            float run = map.ShoreZ - map.SeaStartZ;
            float slope = (top - water) / math.max(run, 1e-3f);
            Assert.Less(slope, 0.5f, $"the beach climbs at {slope:F2} - that is a bank, not sand a man walks up");
            Assert.Greater(slope, 0.01f, $"the beach is flat ({slope:F3}), so the waterline has no edge to read");
        }
    }
}
