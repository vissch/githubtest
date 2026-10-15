// Phase: zoom-hit-marks-size (2026-10-15, the owner: "ground hit marks are about 40% smaller at zoom 30 since
// zoom-scale landed; restore their size when zoomed out"). Cause: gore-by-zoom (6eb17009a) turned HitZoomBoost
// into a thin forward to GoreScale.Size (1.0 at zoom 30), and the ground marks read it — before that the same
// line gave 1.6 at zoom 30. HitMarkGrow restores the marks' own pre-landing curve; the gore pieces stay on
// GoreScale exactly as landed.
using NUnit.Framework;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class HitMarkSizeTests
    {
        [Test]
        public void GroundMarks_KeepTheirPreZoomScaleSize()
        {
            float close = CombatFx.HitMarkGrow(6f);
            float play = CombatFx.HitMarkGrow(30f);
            float far = CombatFx.HitMarkGrow(120f);
            float farthest = CombatFx.HitMarkGrow(600f);

            Assert.AreEqual(1.0f, close, 1.0f * 0.05f, "close in, true to size");
            Assert.AreEqual(1.6f, play, 1.6f * 0.05f, "zoom 30: the play zoom, as before zoom-scale");
            Assert.AreEqual(1.6f, far, 1.6f * 0.05f, "zoom 120: already at the cap");
            Assert.AreEqual(1.6f, farthest, 1.6f * 0.05f, "zoom 600: still at the cap");
        }

        [Test]
        public void AtThePlayZoom_MarksAreNotOnTheGoreCurve()
        {
            Assert.Greater(CombatFx.HitMarkGrow(30f), GoreScale.Size(30f) * 1.3f,
                "the marks' own curve (1.6 at zoom 30) must not collapse to GoreScale's (1.0)");
        }
    }
}
