// Phase: SHOW fix (found-tone-down-the-teal-ground-band-under-machine-20261016-2) — the disc's side ring had a
// fixed strength with no camera fade, so at close range it painted brighter than the unit it marks.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class TankDiscRingTests
    {
        // [found-tone-down-the-teal-ground-band-under-machine-20261016-2]
        [Test]
        public void TheTealBandIsDimmerThanTheMachineAtCloseRange()
        {
            // 16.2 m, 0.06997 and 0.42488 all come from the finding's own still (Captures/units/29-MarkIV-a.json),
            // not from this code. Old code (fixed 1.4 glow, no fade): 0.522, red.
            Assert.That(TankRenderer.BandLuma(TankRenderer.TeamA, TankRenderer.RingStrength(16.2f), 0.06997f), Is.LessThan(0.42488f), "[found-tone-down-the-teal-ground-band-under-machine-20261016-2] the teal band is still brighter than the brightest pixel of the 29-MarkIV-a still");

            Assert.That(TankRenderer.RingStrength(54f), Is.EqualTo(1f).Within(0.001f), "[found-tone-down-the-teal-ground-band-under-machine-20261016-2] the standard view's side ring must not change");
        }
    }
}
