// Phase: snow level — the winter map becomes a level that can be chosen, rather than a preset nothing calls.
//
// WinterMapTests asserts the GROUND is right. These assert it can be REACHED: that a mission card naming the
// winter line hands that ground to SimHost, and that the ground is paired with the winter look in the one place
// the pairing is written down. Until this existed, BattlefieldParams.WinterLine was called by its own tests and
// by nothing else, and SimHost built the shelled wood whatever the mission said.
//
// The pairing is the part most likely to be got wrong later - someone adds a ground and forgets the look, or
// forgets the ground and the new level silently comes up as the wood - so both maps are walked exhaustively from
// Enum.GetValues rather than spot-checked, and a new value with no case added fails here.
using System;
using NUnit.Framework;
using Unity.Collections;
using TW.Presentation;
using TW.Presentation.Terrain;
using TW.Sim.Terrain;
using TW.UI;
using UnityEngine;

namespace TW.Tests
{
    public class WinterLevelTests
    {
        static ulong HashOf(Ground g, uint seed)
        {
            using var map = BattlefieldGenerator.Create(MatchLaunch.Field(g, seed), Allocator.Persistent);
            return map.Hash(0ul);
        }

        [Test]
        public void ChoosingTheWinterLineBuildsTheFrozenGroundAndNotTheWood()
        {
            using var direct = BattlefieldGenerator.Create(BattlefieldParams.WinterLine(1917), Allocator.Persistent);
            Assert.AreEqual(direct.Hash(0ul), HashOf(Ground.WinterLine, 1917),
                "asking for the winter line through the launch path gave different ground from calling the preset "
                + "directly, so the mission and the tests are looking at two different maps");
            Assert.AreNotEqual(HashOf(Ground.ShelledForest, 1917), HashOf(Ground.WinterLine, 1917),
                "the winter line came out as the shelled wood: the ground is being ignored somewhere in the chain");
        }

        /// <summary>
        /// The reason ShelledForest is pinned to zero. Every Request and every MissionCard asset serialized before
        /// Ground existed reads back with the default, and must still be the map it was already using - otherwise
        /// adding this enum would silently move every shipped mission onto a different battlefield.
        /// </summary>
        [Test]
        public void TheGroundNobodyChoseIsStillTheShelledWood()
        {
            Assert.AreEqual(Ground.ShelledForest, default(Ground), "the zero value must stay the shelled wood");
            using var wood = BattlefieldGenerator.Create(BattlefieldParams.ShelledForest(1917), Allocator.Persistent);
            Assert.AreEqual(wood.Hash(0ul), HashOf(default(Ground), 1917));
        }

        [Test]
        public void EveryGroundIsADifferentPlaceAndNoneOfThemFallsBackToTheWoodByAccident()
        {
            var grounds = (Ground[])Enum.GetValues(typeof(Ground));
            Assert.GreaterOrEqual(grounds.Length, 3, "the three fields in scope are the wood, the winter line and the beach");
            for (int i = 0; i < grounds.Length; i++)
                for (int j = i + 1; j < grounds.Length; j++)
                    Assert.AreNotEqual(HashOf(grounds[i], 1917), HashOf(grounds[j], 1917),
                        $"{grounds[i]} and {grounds[j]} generate the same ground, so one of them has no case in "
                        + "MatchLaunch.Field and is quietly falling through to the default");
        }

        /// <summary>
        /// Walks the look map the same way. A ground added without a case here does not fail to compile - the
        /// switch has a default - so this is what catches it: exactly one ground is the winter one, and adding a
        /// fourth field that should be snowy without saying so will show up as two.
        /// </summary>
        [Test]
        public void ExactlyOneGroundWearsTheWinterLook()
        {
            int winter = 0;
            foreach (Ground g in Enum.GetValues(typeof(Ground)))
                if (BiomeProfile.ForGround(g) == Biome.Winter) winter++;
            Assert.AreEqual(1, winter, "one field is the frozen one; if this is 0 the winter level comes up in night mud");
            Assert.AreEqual(Biome.Winter, BiomeProfile.ForGround(Ground.WinterLine));
            Assert.AreEqual(Biome.NightMud, BiomeProfile.ForGround(Ground.ShelledForest));
            Assert.AreEqual(Biome.NightMud, BiomeProfile.ForGround(Ground.Landing),
                "the coast is the same war at the same hour: the sea and the sand make it a different place, not the weather");
        }

        [Test]
        public void AWinterCardHandsTheWinterGroundAllTheWayToTheHost()
        {
            var card = ScriptableObject.CreateInstance<MissionCard>();
            try
            {
                card.Ground = Ground.WinterLine;
                Assert.AreEqual(Ground.WinterLine, card.ToRequest(card.DefaultDifficulty).Ground,
                    "the card knows which ground it is and the request does not carry it, so the host builds the wood");
            }
            finally { ScriptableObject.DestroyImmediate(card); }
        }

        [Test]
        public void ACardMadeBeforeAnyOfThisStillAsksForTheWood()
        {
            var card = ScriptableObject.CreateInstance<MissionCard>();
            try { Assert.AreEqual(Ground.ShelledForest, card.ToRequest(card.DefaultDifficulty).Ground); }
            finally { ScriptableObject.DestroyImmediate(card); }
        }
    }
}
