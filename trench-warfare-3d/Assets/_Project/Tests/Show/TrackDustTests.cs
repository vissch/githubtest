// Phase: A5c (2026-09-29, the Dust Front lessons: vehicle weight) — TrackDust.Weigh, each track's dust. The gate it is
// to replace (TankRenderer.Effects: the hull's |Speed| over 0.25 m/s) is written out here and shown missing a pivot.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class TrackDustTests
    {
        const float MawGauge = 1.7f;
        static bool OldGate(float speed) => Mathf.Abs(speed) > 0.25f;   // TankRenderer.Effects, 2026-09-29

        [Test]
        public void APivotOnTheSpotThrowsDustFromBothTracks_WhichTheHullSpeedGateMissed()
        {
            float speed = 0f, yawRate = 0.4f;   // the Maw turning in place at 0.4 rad/s: each track runs 0.68 m/s
            Assert.IsFalse(OldGate(speed), "the old gate: a hull standing still throws no dust");
            var (l, r) = TrackDust.Weigh(speed, yawRate, MawGauge);
            Assert.Greater(l, 0.2f, "left track"); Assert.Greater(r, 0.2f, "right track");
            Assert.AreEqual(l, r, 1e-6f, "a pivot on the spot churns both alike");
        }

        [Test]
        public void StandingStillThrowsNone_DrivingStraightBothAlike_AndFullSpeedIsAll()
        {
            Assert.AreEqual((0f, 0f), TrackDust.Weigh(0f, 0f, MawGauge));
            Assert.AreEqual((0f, 0f), TrackDust.Weigh(0.2f, 0f, MawGauge), "under the old gate's 0.25 m/s: none, as before");
            var (l, r) = TrackDust.Weigh(1.6f, 0f, MawGauge);
            Assert.AreEqual(l, r, 1e-6f); Assert.Greater(l, 0.5f);
            Assert.AreEqual((1f, 1f), TrackDust.Weigh(-3f, 0f, MawGauge), "reversing hard is all a track can throw");
        }

        [Test]
        public void InATurnTheOuterTrackThrowsMore()
        {
            var (l, r) = TrackDust.Weigh(1.2f, 0.3f, MawGauge);   // turning with the left track outside (left = speed + turn)
            Assert.Greater(l, r, "the outer track runs faster over the ground");
            Assert.Greater(r, 0f, "and the inner one still turns");
        }
    }
}
