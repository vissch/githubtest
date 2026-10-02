// Phase: snow level — ground that has frozen, rather than ShelledForest wearing a blizzard.
//
// Until BattlefieldParams.WinterLine existed, the snow level was the shelled wood repainted: a river running
// across no man's land and a water table under it, on a field whose own BiomeProfile sets Flooding = 0.25 because
// "what water there is has frozen" (docs/18, W2). No shader could fix that, because the water is in the MAP.
//
// These assert the ground, not the look. Biome lives in Presentation and TW.Sim cannot see it without closing a
// reference cycle, so pairing this map with Biome.Winter belongs to the mission card.
using NUnit.Framework;
using Unity.Collections;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class WinterMapTests
    {
        [Test]
        public void TheWinterLineHasNoRiverAcrossNoMansLand()
        {
            var p = BattlefieldParams.WinterLine(1917);
            Assert.IsFalse(p.River, "a frozen field does not have a flowing river through the middle of it");
            using var map = BattlefieldGenerator.Create(p, Allocator.Persistent);
            Assert.LessOrEqual(map.WaterLevel, MapData.NoWater,
                $"the winter map still has a water table at {map.WaterLevel}, so puddles, fords and the wet ground "
                + "term all come back on a field that is supposed to have frozen");
        }

        /// <summary>
        /// The river is gated on BOTH the flag and the water table (`p.River && p.WaterLevel > NoWater`), so this
        /// pins the belt-and-braces rather than trusting one of them: turning River back on must still produce no
        /// river while the map is dry, or somebody will flip the flag and wonder why nothing happened.
        /// </summary>
        [Test]
        public void ADryMapHasNoRiverEvenIfSomebodyTurnsTheFlagBackOn()
        {
            var p = BattlefieldParams.WinterLine(1917);
            p.River = true;
            using var wet = BattlefieldGenerator.Create(BattlefieldParams.WinterLine(1917), Allocator.Persistent);
            using var dry = BattlefieldGenerator.Create(p, Allocator.Persistent);
            Assert.AreEqual(wet.Hash(0ul), dry.Hash(0ul),
                "River on a NoWater map must be inert; if this fails the two gates have come apart and a winter "
                + "field can grow a river from a flag alone");
        }

        /// <summary>Frozen ground does not churn: the half-speed mire is a thaw, not a winter.</summary>
        [Test]
        public void TheWinterLineIsNotAMudBath()
        {
            var winter = BattlefieldParams.WinterLine(1917);
            var autumn = BattlefieldParams.ShelledForest(1917);
            Assert.Less(winter.Mud, autumn.Mud,
                $"winter churns {winter.Mud:F2} of its open ground against the shelled wood's {autumn.Mud:F2}");
            Assert.Less(winter.Forest, autumn.Forest,
                "the snowfield reads as open ground with broken timber standing in it, not as a wood");
        }

        [Test]
        public void TheWinterLineIsInlandAndHasNoCoast()
        {
            var p = BattlefieldParams.WinterLine(1917);
            Assert.IsFalse(p.Sea, "the winter line is a mountain sector, not a beach");
            using var map = BattlefieldGenerator.Create(p, Allocator.Persistent);
            Assert.IsFalse(map.HasSea);
            Assert.AreEqual(p.Length, map.SizeMeters.y, 1e-3f, "no sea, so no coast is added past the layout");
        }

        /// <summary>
        /// A mission is a BattlefieldParams value rather than a saved map, so the preset has to give the same
        /// ground on every machine.
        /// </summary>
        [Test]
        public void TheWinterLineIsTheSameGroundEveryTime()
        {
            using var a = BattlefieldGenerator.Create(BattlefieldParams.WinterLine(1917), Allocator.Persistent);
            using var b = BattlefieldGenerator.Create(BattlefieldParams.WinterLine(1917), Allocator.Persistent);
            Assert.AreEqual(a.Hash(0ul), b.Hash(0ul), "same seed and params, same ground");
            using var c = BattlefieldGenerator.Create(BattlefieldParams.WinterLine(1918), Allocator.Persistent);
            Assert.AreNotEqual(a.Hash(0ul), c.Hash(0ul), "a different seed is a different sector");
        }

        [Test]
        public void TheWinterLineSurvivesBeingWrittenDownAndReadBack()
        {
            var p = BattlefieldParams.WinterLine(1917);
            var q = BattlefieldParams.Deserialize(p.Serialize());
            Assert.AreEqual(p.WaterLevel, q.WaterLevel, 1e-3f, "a replay of a frozen field must still be frozen");
            Assert.AreEqual(p.River, q.River);
            Assert.AreEqual(p.Mud, q.Mud, 1e-3f);
            Assert.AreEqual(p.Sea, q.Sea);
        }

        /// <summary>
        /// The two new presets must be different maps, and both must differ from the one that shipped. Cheap, and
        /// it catches the copy-paste that leaves a new preset quietly identical to its neighbour.
        /// </summary>
        [Test]
        public void TheThreeBattlefieldsAreThreeDifferentPlaces()
        {
            using var wood = BattlefieldGenerator.Create(BattlefieldParams.ShelledForest(1917), Allocator.Persistent);
            using var beach = BattlefieldGenerator.Create(BattlefieldParams.Landing(1917), Allocator.Persistent);
            using var snow = BattlefieldGenerator.Create(BattlefieldParams.WinterLine(1917), Allocator.Persistent);
            Assert.AreNotEqual(wood.Hash(0ul), beach.Hash(0ul));
            Assert.AreNotEqual(wood.Hash(0ul), snow.Hash(0ul));
            Assert.AreNotEqual(beach.Hash(0ul), snow.Hash(0ul));
        }
    }
}
