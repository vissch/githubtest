// Phase: A5d (2026-09-29) — comic sound words stay rare (the owner's word), and each moment gets its word.
using NUnit.Framework;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim;
using TW.UI;

namespace TW.Tests
{
    public class ComicWordsTests
    {
        static SimEvent E(SimEventType t, float scalar = 0f, float dirY = 0f) => new SimEvent { Type = t, Scalar = scalar, Dir = new float3(0f, dirY, 0f) };

        [Test]
        public void EachMoment_GetsItsWord_AndOrdinaryEventsNone()
        {
            Assert.AreEqual(ComicWords.Moment.NearMiss, ComicWords.Classify(E(SimEventType.NearMiss)));
            Assert.AreEqual(ComicWords.Moment.BigBurst, ComicWords.Classify(E(SimEventType.Explosion, 7f)));
            Assert.AreEqual(ComicWords.Moment.None, ComicWords.Classify(E(SimEventType.Explosion, 3f)), "a small burst: no word");
            Assert.AreEqual(ComicWords.Moment.None, ComicWords.Classify(E(SimEventType.Explosion, 8f, 2f)), "the cook-off's own blast has its own event");
            Assert.AreEqual(ComicWords.Moment.Ricochet, ComicWords.Classify(E(SimEventType.VehicleArmourHit, -40f)));
            Assert.AreEqual(ComicWords.Moment.None, ComicWords.Classify(E(SimEventType.VehicleArmourHit, 40f)), "holed: no clang");
            Assert.AreEqual(ComicWords.Moment.CookOff, ComicWords.Classify(E(SimEventType.VehicleCookOff, 6f)));
            Assert.AreEqual(ComicWords.Moment.None, ComicWords.Classify(E(SimEventType.Shot)));
            Assert.AreEqual(ComicWords.Moment.Blow, ComicWords.Classify(E(SimEventType.MeleeBlow, 60f)), "a blow that landed");
            Assert.AreEqual(ComicWords.Moment.Parry, ComicWords.Classify(E(SimEventType.MeleeBlow, 0f)), "one turned aside");
            Assert.AreEqual(ComicWords.Moment.None, ComicWords.Classify(E(SimEventType.MeleeBlow, -1f)), "a miss says nothing");
            Assert.AreEqual(ComicWords.Moment.Pounce, ComicWords.Classify(E(SimEventType.PounceLanded, 3f)));
            Assert.AreEqual("THWACK!", ComicWords.WordFor(ComicWords.Moment.Blow));
            Assert.Greater((int)ComicWords.Moment.Pounce, (int)ComicWords.Moment.Blow, "a crab landing outweighs a rifle butt");
            Assert.AreEqual("CRACK!", ComicWords.WordFor(ComicWords.Moment.NearMiss));
        }

        [Test]
        public void AFightOfNearMissesEveryFrame_GivesOneWordPerGap()
        {
            var p = new ComicWords.Picker { Gap = 8f };
            int words = 0;
            for (int f = 0; f < 60 * 60; f++)                      // a minute at 60 frames, a near miss every frame
            {
                p.Offer(ComicWords.Moment.NearMiss, Vector3.zero);
                if (p.Take(f / 60f, 0, out _, out _)) words++;
            }
            Assert.LessOrEqual(words, 8, "rare: at most one word each 8 s");
            Assert.GreaterOrEqual(words, 7, "but the fight does get its words");
        }

        [Test]
        public void TheWeightiestMomentWins_AndNoMoreThanTwoShow()
        {
            var p = new ComicWords.Picker { Gap = 1f };
            p.Offer(ComicWords.Moment.NearMiss, Vector3.zero);
            p.Offer(ComicWords.Moment.CookOff, Vector3.one);
            p.Offer(ComicWords.Moment.BigBurst, Vector3.zero);
            Assert.IsTrue(p.Take(10f, 0, out var m, out var at));
            Assert.AreEqual(ComicWords.Moment.CookOff, m); Assert.AreEqual(Vector3.one, at);
            p.Offer(ComicWords.Moment.CookOff, Vector3.zero);
            Assert.IsFalse(p.Take(20f, ComicWords.MaxLive, out _, out _), "two showing: no third");
        }

        [Test]
        public void AWord_IsOnlyWhereThePlayerSeesIt()
        {
            Assert.IsTrue(ComicWords.Seen(new Vector3(0.5f, 0.5f, 30f), 40f));
            Assert.IsFalse(ComicWords.Seen(new Vector3(0.5f, 0.5f, -1f), 40f), "behind the camera");
            Assert.IsFalse(ComicWords.Seen(new Vector3(0.02f, 0.5f, 30f), 40f), "at the frame's edge");
            Assert.IsFalse(ComicWords.Seen(new Vector3(0.5f, 0.5f, 30f), 400f), "too far off to read");
        }
    }
}
