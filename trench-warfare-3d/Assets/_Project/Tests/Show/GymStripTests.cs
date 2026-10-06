// Phase: tooling (the gym sees, 2026-10-06) — The gym's time strip (Perf/GymStrip) and the clear-day light (BiomeProfile.ClearDay).
//
// The bug these guard. The first unattended filming (unit-look/baseline) photographed every entry once per zoom
// band, three of those bands so far out that the whole stage was a dot, and flagged 0 of 350 entries: a reviewer
// could not tell an effect that was never drawn from a still that missed it. The strip is five frames in time at
// one close zoom, and the flag is a measurement against a noise floor rather than an opinion — so the thing worth
// testing is that the flag can fail BOTH WAYS: silent on an entry that draws, raised on one that does not.
using NUnit.Framework;
using TW.Perf;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class GymStripTests
    {
        [Test]
        public void StripsTheTabsThatAreMomentsInTime()
        {
            Assert.IsTrue(GymStrip.Strips(GymTab.Deaths));
            Assert.IsTrue(GymStrip.Strips(GymTab.Abilities));
            Assert.IsTrue(GymStrip.Strips(GymTab.Events));
            Assert.IsTrue(GymStrip.Strips(GymTab.Units));
            Assert.IsFalse(GymStrip.Strips(GymTab.Clips), "a clip is a pose: it keeps its zoom bands");
            Assert.IsFalse(GymStrip.Strips(GymTab.Scenes), "a scene is a whole stage: it keeps its overviews");
        }

        static GymEntry Entry(GymTab tab, GymExpect expect) => new GymEntry { Tab = tab, Expect = expect, Name = "x" };

        [Test]
        public void OnlyAnEntryThatPromisesSomethingCanBeFlagged()
        {
            Assert.IsTrue(GymStrip.Expects(Entry(GymTab.Deaths, GymExpect.Fires)));
            Assert.IsTrue(GymStrip.Expects(Entry(GymTab.Abilities, GymExpect.FactionSeat)));
            Assert.IsTrue(GymStrip.Expects(Entry(GymTab.Events, GymExpect.Preview)));
            Assert.IsFalse(GymStrip.Expects(Entry(GymTab.Abilities, GymExpect.Rejected)), "the sim refuses it: drawing nothing is the right answer");
            Assert.IsFalse(GymStrip.Expects(Entry(GymTab.Events, GymExpect.Covered)));
            Assert.IsFalse(GymStrip.Expects(Entry(GymTab.Events, GymExpect.Excluded)));
            Assert.IsFalse(GymStrip.Expects(Entry(GymTab.Clips, GymExpect.Fires)), "a clip is never a strip, so it is never flagged this way");
        }

        [Test]
        public void MomentsAreFourOrMoreAscendingAndKeepTheOldPhotographedOne()
        {
            foreach (float life in new[] { 0f, 0.4f, 3f, 12f, 40f })
            {
                var m = GymStrip.Moments(life);
                Assert.GreaterOrEqual(m.Length, 4, "life " + life);
                for (int i = 1; i < m.Length; i++)
                    Assert.GreaterOrEqual(m[i], m[i - 1] + 0.25f - 1e-4f, "moments must be ascending and apart, life " + life);
                Assert.GreaterOrEqual(m[0], 0f);
                Assert.Less(m[m.Length - 1], life + 2.01f + 1f, "the strip must not run far past the entry's life");
            }
            // the baseline photographed at `life`; that frame has to still be in the strip, or the two runs cannot be compared
            var mid = GymStrip.Moments(12f);
            Assert.Contains(12f, mid, "the old photographed moment is still one of the frames");
            Assert.AreEqual(GymStrip.Frames, GymStrip.Names.Length);
            Assert.AreEqual(GymStrip.Frames, mid.Length + 1, "the 'before' frame plus one per moment");
        }

        [Test]
        public void TheThresholdNeverFallsBelowTheFloorOfVisibility()
        {
            Assert.AreEqual(0.004f, GymStrip.Threshold(0f), 1e-6f);
            Assert.AreEqual(0.004f, GymStrip.Threshold(0.001f), 1e-6f, "3 x 0.001 is under the floor");
            Assert.AreEqual(0.030f, GymStrip.Threshold(0.010f), 1e-6f);
        }

        [Test]
        public void TheFlagFailsBothWays()
        {
            const float floor = 0.002f;   // a measured idle stage: rain and flicker repaint 0.2 % of a close frame
            // an entry that draws: a shell burst repaints a fifth to a third of the close frame
            Assert.IsNull(GymStrip.NothingDrawn(new[] { 0.21f, 0.33f, 0.30f, 0.12f }, floor), "this one draws and must not be flagged");
            // an entry that does not: every frame is inside the noise
            string flag = GymStrip.NothingDrawn(new[] { 0.001f, 0.002f, 0.001f, 0.001f }, floor);
            Assert.IsNotNull(flag, "nothing was drawn and it must be flagged");
            StringAssert.Contains("nothing drawn", flag);
            // nothing measured is not evidence of nothing drawn
            Assert.IsNull(GymStrip.NothingDrawn(new float[0], floor));
            Assert.IsNull(GymStrip.NothingDrawn(null, floor));
            Assert.IsNull(GymStrip.NothingDrawn(new[] { float.NaN, float.NaN }, floor), "unreadable diffs say nothing either way");
        }

        [Test]
        public void ClearDayIsTheNightFieldsGroundInDaylight()
        {
            var day = BiomeProfile.ClearDay();
            var night = BiomeProfile.NightMud();
            Assert.IsFalse(day.Dark, "a figure cannot be judged in the dark");
            Assert.AreEqual(0f, day.Rain, 1e-6f);
            Assert.IsFalse(day.WantsRain);
            Assert.Greater(day.Exposure, night.Exposure);
            Assert.AreEqual(night.Id, day.Id, "same ground, only the light changes");
            Assert.AreEqual(night.Flooding, day.Flooding, 1e-6f);
            Assert.AreEqual(0f, day.SnowCoverage, 1e-6f);
            Assert.AreEqual(0f, day.HeatStrength, 1e-6f);
        }
    }
}
