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
using TW.Sim.Match;
using UnityEngine;

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


        // ------------------------------------------------------- framing the subject and the sim's truth (look-04)
        // The bug these guard. Round 2's strips were shot at one fixed 16 m band aimed at the stage, so a 2 m man was
        // a handful of pixels and Deaths/Shot.jpg had no man in any of its five cells; and Events/VehicleDestroyed.jpg
        // showed the same undamaged tank five times because the event had only been replayed into the effects while
        // the machine never died in the sim - and the gym called that entry PASSED.

        [Test]
        public void TheZoomFramesTheSubjectAndStaysInTheBand()
        {
            float man = GymStrip.ZoomFor(GymStrip.SubjectHeight(false, false), 25f);
            float tank = GymStrip.ZoomFor(GymStrip.SubjectHeight(true, false), 25f);
            float walker = GymStrip.ZoomFor(GymStrip.SubjectHeight(true, true), 25f);
            Assert.AreEqual(6f, man, 0.5f, "a man wants the closest zoom the camera allows");
            Assert.AreEqual(15f, walker, 1f, "a walker is tall: it needs the wide end of the band");
            Assert.Greater(walker, tank); Assert.Greater(tank, man);
            foreach (float z in new[] { man, tank, walker })
            {
                Assert.GreaterOrEqual(z, GymStrip.ZoomMin);
                Assert.LessOrEqual(z, GymStrip.ZoomMax);
            }
            // a subject taller than the band allows is clamped, not shot from a mile off
            Assert.AreEqual(GymStrip.ZoomMax, GymStrip.ZoomFor(40f, 25f), 1e-4f);
            Assert.AreEqual(GymStrip.ZoomMin, GymStrip.ZoomFor(0.2f, 25f), 1e-4f);
            // the frame is 1.1547 * zoom metres tall: at the zoom it picks, the subject fills about a third of it
            float frac = GymStrip.SubjectHeight(true, true) * Mathf.Cos(25f * Mathf.Deg2Rad) / (1.1547f * walker);
            Assert.AreEqual(GymStrip.Share, frac, 0.03f, "the walker should fill about a third of the cell's height");
        }

        [Test]
        public void AnAircraftIsShotWideAndAimedUp()
        {
            foreach (var id in new[] { OffMapAbilityId.StrafeRun, OffMapAbilityId.BomberRun, OffMapAbilityId.ParaDrop, OffMapAbilityId.ReconFlight })
                Assert.IsTrue(GymStrip.Flyer(new GymEntry { Tab = GymTab.Abilities, Id = (int)id, Name = id.ToString() }), id + " flies");
            Assert.IsFalse(GymStrip.Flyer(new GymEntry { Tab = GymTab.Abilities, Id = (int)OffMapAbilityId.HeBarrage, Name = "HeBarrage" }));
            Assert.IsFalse(GymStrip.Flyer(new GymEntry { Tab = GymTab.Deaths, Id = (int)OffMapAbilityId.StrafeRun, Name = "Blast" }), "only an ability flies");
            // PlaneLow is 25 m up: a frame 1.1547 * zoom tall aimed at FlyerAimY has to reach it
            Assert.Greater(GymStrip.FlyerZoom * 1.1547f * 0.5f + GymStrip.FlyerAimY, 25f, "the aircraft must be inside the frame");
        }

        [Test]
        public void TheHudAndTheAimingDiscAreHiddenUnlessTheRunAsksForThem()
        {
            var e = Entry(GymTab.Abilities, GymExpect.Fires);
            Assert.IsFalse(GymStrip.ShowsOverlays(e, false), "an aiming disc repaints the frame the measurement is reading");
            Assert.IsTrue(GymStrip.ShowsOverlays(e, true), "hud=1 asks for them back");
        }

        [Test]
        public void AnEntryWhoseStagingNeverHappenedIsNotJudged()
        {
            var ev = Entry(GymTab.Events, GymExpect.Preview);
            StringAssert.Contains("not staged", GymStrip.NotStaged(ev, true, false, false, false, false), "replayed to the effects only");
            StringAssert.Contains("not staged", GymStrip.NotStaged(ev, false, false, false, false, false), "the machine neither died nor caught fire");
            Assert.IsNull(GymStrip.NotStaged(ev, false, false, false, false, true), "the sim really destroyed it: judge the pictures");

            var death = Entry(GymTab.Deaths, GymExpect.Fires);
            StringAssert.Contains("not staged", GymStrip.NotStaged(death, false, true, false, false, false), "the victim lived");
            Assert.IsNull(GymStrip.NotStaged(death, false, true, true, false, false), "he died: judge the pictures");

            var ability = Entry(GymTab.Abilities, GymExpect.Fires);
            StringAssert.Contains("not staged", GymStrip.NotStaged(ability, false, false, false, false, false), "the sim refused it");
            Assert.IsNull(GymStrip.NotStaged(ability, false, false, false, true, false), "it fired: judge the pictures");
            // an ability the catalogue EXPECTS the sim to refuse is not "not staged": drawing nothing is its right answer
            Assert.IsNull(GymStrip.NotStaged(Entry(GymTab.Abilities, GymExpect.Rejected), false, false, false, false, false));
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

        // ----------------------------------------------------------- the camera's side (look-07, 2026-10-06)
        // round3/FLAGS.md claimed no infantryman carries a weapon from pictures all taken from BEHIND him at a third
        // of the cell's height. These four guard the arithmetic that lets the gym stand in front of a man and get
        // close enough to see what he holds.

        [Test]
        public void NoViewsOptionIsTodaysSingleStandardView()
        {
            CollectionAssert.AreEqual(new[] { GymStrip.GymView.Std }, GymStrip.ParseViews(null));
            CollectionAssert.AreEqual(new[] { GymStrip.GymView.Std }, GymStrip.ParseViews(""));
            CollectionAssert.AreEqual(new[] { GymStrip.GymView.Std }, GymStrip.ParseViews("sideways,up"));
            // and the standard view is always first, so the frame that is diffed and flagged never changes
            Assert.AreEqual(GymStrip.GymView.Std, GymStrip.ParseViews("side,front")[0]);
        }

        [Test]
        public void TheFrontViewLooksHimInTheFace()
        {
            Assert.AreEqual(210f, GymStrip.ViewYaw(GymStrip.GymView.Front, 30f), 1e-3f);
            Assert.AreEqual(120f, GymStrip.ViewYaw(GymStrip.GymView.Side, 30f), 1e-3f);
            Assert.AreEqual(30f, GymStrip.ViewYaw(GymStrip.GymView.Back, 30f), 1e-3f);
            Assert.AreEqual(170f, GymStrip.ViewYaw(GymStrip.GymView.Front, 350f), 1e-3f, "wrapped into 0..360");
        }

        [Test]
        public void TheStandardViewKeepsTodaysPose()
        {
            Assert.IsNaN(GymStrip.ViewYaw(GymStrip.GymView.Std, 30f), "NaN means: do not pin a yaw, shoot as before");
            Assert.AreEqual(25f, GymStrip.ViewPitch(GymStrip.GymView.Std), 1e-6f);
            Assert.AreEqual(GymStrip.Share, GymStrip.ViewShare(GymStrip.GymView.Std), 1e-6f);
            Assert.AreEqual("", GymStrip.ViewSuffix(GymStrip.GymView.Std), "its file names are unchanged");
        }

        [Test]
        public void AManFillsHalfAFrontViewsHeight()
        {
            // The rig's frame is 1.1547 * zoom metres tall (CaptureRig.Pose keeps distance * tan30 / tan(fov/2)), so
            // a 2 m man foreshortened by the pitch fills height * cos(pitch) / (1.1547 * zoom) of it. With ZoomMin's
            // 6 m floor that is 0.286 - the share FLAGS.md read a silhouette from.
            float pitch = GymStrip.ViewPitch(GymStrip.GymView.Front), share = GymStrip.ViewShare(GymStrip.GymView.Front);
            float zoom = GymStrip.ZoomFor(2f, pitch, share, GymStrip.CloseZoomFloor);
            float frac = 2f * Mathf.Cos(pitch * Mathf.Deg2Rad) / (1.1547f * zoom);
            Assert.GreaterOrEqual(frac, 0.5f, "a man must fill half a close view's height");
            float old = 2f * Mathf.Cos(pitch * Mathf.Deg2Rad) / (1.1547f * GymStrip.ZoomFor(2f, pitch, share));
            Assert.Less(old, 0.3f, "and the old floor could not frame him: that is what this is for");
        }

    }
}
