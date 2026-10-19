// Phase: water-entry-splash-r6 (2026-10-19) — the water's own clock, pinned. A capture shoots the same throw twice
// seconds apart to measure its noise; without this the lapping, the drift and the raindrops move the sheet by
// themselves and nothing is measurable through it. Pure arithmetic: no scene, no shader.
using NUnit.Framework;
using TW.Presentation;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class WaterRingsClockTests
    {
        [TearDown]
        public void TearDown() => WaterRings.PinnedClock = 0f;

        [Test]
        public void ThePinnedClockWinsAndTheSessionGivesItBack()
        {
            WaterRings.PinnedClock = 0f;
            Assert.AreEqual(5f, WaterRings.ClockToPush(5f), 1e-4f, "unpinned: the live second goes to the shader");

            WaterRings.PinnedClock = 120f;
            Assert.AreEqual(120f, WaterRings.ClockToPush(5f), 1e-4f, "pinned: the sheet is frozen at that second");

            SceneStatics.ResetSession();
            Assert.AreEqual(0f, WaterRings.PinnedClock, 1e-4f, "no scene is live: the water gets its clock back");
            Assert.AreEqual(5f, WaterRings.ClockToPush(5f), 1e-4f);
        }
    }
}
