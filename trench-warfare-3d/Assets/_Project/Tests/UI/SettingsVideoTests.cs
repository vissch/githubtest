// Phase: B6 (test added 2026-10-08 for code review finding L6) — the video page's resolution row must be picked by
// refresh rate as well as size: the monitor lists 1920x1080 at 60 and at 144 Hz as two rows, and a size-only match
// always landed on the first of them. Reflection on purpose: the pre-fix tree has no PickResolution, and this file
// must still compile against it so the other findings' tests can run there.
using System.Collections.Generic;
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.UI;

namespace TW.Tests
{
    public class SettingsVideoTests
    {
        static Resolution Row(int w, int h, int hz)
        {
            var r = new Resolution { width = w, height = h };
            r.refreshRateRatio = new RefreshRate { numerator = (uint)hz, denominator = 1 };
            return r;
        }

        // [L6] the refresh rate was listed in the dropdown and stored in the settings, but the row was matched by
        // size alone, so a 144 Hz choice came back as the 60 Hz row of the same size.
        [Test]
        public void TheResolutionRowIsPickedByRefreshRateAndNotBySizeAlone()
        {
            var pick = typeof(SettingsScreen).GetMethod("PickResolution", BindingFlags.NonPublic | BindingFlags.Static);
            Assert.That(pick, Is.Not.Null, "[L6] nothing matches a row by refresh rate: it is picked by size alone");

            var rows = new List<Resolution> { Row(1920, 1080, 60), Row(1920, 1080, 144), Row(2560, 1440, 60) };
            Assert.That(pick.Invoke(null, new object[] { rows, 1920, 1080, 144 }), Is.EqualTo(1), "[L6] 1920x1080 at 144 Hz must pick the 144 Hz row");
            Assert.That(pick.Invoke(null, new object[] { rows, 1920, 1080, 60 }), Is.EqualTo(0), "[L6] 1920x1080 at 60 Hz must pick the 60 Hz row");
            Assert.That(pick.Invoke(null, new object[] { rows, 2560, 1440, 144 }), Is.EqualTo(2), "[L6] with no matching rate the highest rate of that size is taken");
            Assert.That(pick.Invoke(null, new object[] { rows, 1920, 1080, 0 }), Is.EqualTo(1), "[L6] rate 0 means 'current': the best row of that size");
            Assert.That(pick.Invoke(null, new object[] { rows, 800, 600, 60 }), Is.EqualTo(-1), "[L6] a size no row has must report no row");
        }
    }
}
