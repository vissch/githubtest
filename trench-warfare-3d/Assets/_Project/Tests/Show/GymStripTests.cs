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
            // look-08: past a floor of 0.005 the line is the floor plus 1 % of the frame, not three times the floor
            Assert.AreEqual(0.020f, GymStrip.Threshold(0.010f), 1e-6f);
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


        // ---------------------------------------------------------------- look-08: a line that can still fail
        [Test]
        public void ThresholdLeavesRoundThreesShotUnflagged()
        {
            // Round 3's Deaths/Shot, shot at 6 m where the idle floor is 0.2466: the strip moved 0.2725, 0.2594,
            // 0.2525, 0.2530 of the frame - clearly more than idle - and the old rule (three times the floor, 0.7398)
            // flagged it "nothing drawn". On the old Threshold the first assert below fails.
            var shot = new[] { 0.2725f, 0.2594f, 0.2525f, 0.2530f };
            Assert.IsNull(GymStrip.NothingDrawn(shot, 0.2466f), "a strip that moved a quarter of the frame drew something");
            Assert.IsNotNull(GymStrip.NothingDrawn(new[] { 0.2470f }, 0.2466f), "and a strip sitting AT the floor is still flagged");
            Assert.AreEqual(0.2566f, GymStrip.Threshold(0.2466f), 1e-4f, "the floor plus 1 % of the frame");
        }

        [Test]
        public void ThresholdKeepsTheOldThreeTimesRuleAtATinyFloor()
        {
            Assert.AreEqual(0.004f, GymStrip.Threshold(0.0005f), 1e-6f, "never under 0.4 % of the frame");
            Assert.AreEqual(0.009f, GymStrip.Threshold(0.003f), 1e-6f, "3 x floor is the tighter of the two here");
            Assert.AreEqual(0.03f, GymStrip.Threshold(0.02f), 1e-6f, "floor + 0.01 takes over once 3 x floor runs away");
            foreach (float f in new[] { 0f, 0.001f, 0.01f, 0.1f, 0.5f })
                Assert.LessOrEqual(GymStrip.Threshold(f), 3f * f > 0.004f ? 3f * f : 0.004f, "never looser than the old rule");
        }

        [Test]
        public void AStripHidesTheMachinesGroundRing()
        {
            var maw = new GymEntry { Tab = GymTab.Units, Name = "Maw", Expect = GymExpect.Fires };
            Assert.IsFalse(GymStrip.ShowsSelection(maw, false), "the cyan ring is an instrument, not the machine");
            Assert.IsTrue(GymStrip.ShowsSelection(maw, true), "hud=1 puts it back");
            var clip = new GymEntry { Tab = GymTab.Clips, Name = "Walk", Expect = GymExpect.Fires };
            Assert.IsTrue(GymStrip.ShowsSelection(clip, false), "a clip or a scene is the game as it is played");
            var ring = new GymEntry { Tab = GymTab.Units, Name = "SelectionRing", Expect = GymExpect.Fires };
            Assert.IsTrue(GymStrip.ShowsSelection(ring, false), "an entry about the ring keeps its subject");
            Assert.IsFalse(GymStrip.AboutSelection(maw));
            Assert.IsTrue(GymStrip.AboutSelection(ring));
        }


        // look-08, fault 2: round 3 passed Units/Medic, Shield, Engineer, Officer, Para, Vehicle and Jetpack with the
        // man behind a hull or a walker leg, because subject_in_frame only asks whether his POSITION projects into
        // the cell. On the old code there was no VisibleShare and no `hidden` flag at all, so these two tests did
        // not compile against it - the fault was that nothing measured this.
        [Test]
        public void VisibleShareIsTheSubjectsOwnPixelsOverALoneRiflemans()
        {
            // a man alone on a bare stage repaints 0.0600 of the frame when he stops being drawn, over a 0.0100 floor
            Assert.AreEqual(1f, GymStrip.VisibleShare(0.06f, 0.06f, 0.01f), 1e-4f, "nothing in front of him: all of him");
            Assert.AreEqual(0.5f, GymStrip.VisibleShare(0.035f, 0.06f, 0.01f), 1e-4f, "half of him behind a hull");
            Assert.AreEqual(0f, GymStrip.VisibleShare(0.01f, 0.06f, 0.01f), 1e-4f, "at the floor: not one pixel of him got through");
            Assert.AreEqual(0f, GymStrip.VisibleShare(0.004f, 0.06f, 0.01f), 1e-4f, "under the floor is still none of him, never negative");
            Assert.AreEqual(1f, GymStrip.VisibleShare(0.09f, 0.06f, 0.01f), 1e-4f, "more than the reference is still all of him");
            Assert.IsTrue(float.IsNaN(GymStrip.VisibleShare(float.NaN, 0.06f, 0.01f)), "a vehicle has no hide hook: no number");
            Assert.IsTrue(float.IsNaN(GymStrip.VisibleShare(0.06f, float.NaN, 0.01f)), "no reference measured: no number");
            Assert.IsFalse(float.IsNaN(GymStrip.VisibleShare(0.02f, 0.0100f, 0.01f)), "a reference at the floor does not divide by zero");
        }

        [Test]
        public void HiddenFlagsASubjectUnderSixtyPercentAndNeverANaN()
        {
            Assert.IsNull(GymStrip.Hidden(1f), "fully visible");
            Assert.IsNull(GymStrip.Hidden(GymStrip.MinVisible), "exactly at the line is not flagged");
            Assert.IsNotNull(GymStrip.Hidden(0.59f), "just under the line is flagged");
            Assert.IsNotNull(GymStrip.Hidden(0f), "behind a hull");
            StringAssert.Contains("hidden", GymStrip.Hidden(0.2f));
            Assert.IsNull(GymStrip.Hidden(float.NaN), "nothing measured is not evidence of hiding");
            Assert.AreEqual(0.60f, GymStrip.MinVisible, 1e-6f, "the share stated in round4/FLAGS.md");
        }
    }
}
