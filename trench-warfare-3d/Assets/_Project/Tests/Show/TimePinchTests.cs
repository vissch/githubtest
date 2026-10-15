// Phase: punch-quarter-second (implemented) — measures PunchPinch's curve from its timing data alone, no scene.
// Phase: punch-quarter-second-b (implemented) — PunchClock's interrupt handling (a lightning freeze and thaw, a
// pause, another writer, no interruption at all) and the lockstep gate on Fire(), both driven with no Unity frame.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class TimePinchTests
    {
        const float Step = 1f / 60f;

        [TearDown]
        public void TearDown() => Time.timeScale = 1f;

        [Test]
        public void Punch_Window_Is_Under_A_Quarter_Second()
        {
            float window = 0f;
            for (float t = 0f; t <= 0.5f; t += 0.0005f)
                if (PunchPinch.ScaleAt(t) < 0.999f) window = t;
            Assert.Less(window, 0.250f, $"the punch's window is {window:F4} s");
        }

        [Test]
        public void The_Curve_Is_A_Hold_Then_A_Ramp_Back()
        {
            Assert.AreEqual(PunchPinch.SlowScale, PunchPinch.ScaleAt(0f));
            Assert.AreEqual(PunchPinch.SlowScale, PunchPinch.ScaleAt(PunchPinch.HoldSeconds));
            Assert.AreEqual(1f, PunchPinch.ScaleAt(PunchPinch.Duration));
            Assert.AreEqual(PunchPinch.HoldSeconds + PunchPinch.RampSeconds, PunchPinch.Duration);

            float prev = PunchPinch.ScaleAt(PunchPinch.HoldSeconds);
            for (float t = PunchPinch.HoldSeconds; t <= PunchPinch.Duration; t += 0.001f)
            {
                float s = PunchPinch.ScaleAt(t);
                Assert.GreaterOrEqual(s, prev - 1e-5f, "the ramp never falls");
                prev = s;
            }
        }

        static PunchClock NewClock() => new PunchClock { Since = -1f, Written = -1f };

        /// <summary>Runs `frames` steps of a clock, each frame asking Step for the scale and feeding whatever it
        /// writes back as the next frame's scaleNow (as the real Time.timeScale would see its own write).</summary>
        static float Drive(ref PunchClock clock, int frames, float scaleNow, bool frozen)
        {
            for (int i = 0; i < frames; i++)
                if (clock.Step(Step, scaleNow, frozen, out var write)) scaleNow = write;
            return scaleNow;
        }

        [Test]
        public void Lightning_During_Punch_Restores_Time_Scale()
        {
            foreach (float thawTo in new[] { 0.25f, 1f })
            {
                var clock = NewClock();
                Assert.IsTrue(clock.TryFire(1f, lockstep: false, out float scale));
                Assert.AreEqual(PunchPinch.SlowScale, scale);

                // a few frames into the punch before the strike
                scale = Drive(ref clock, 3, scale, frozen: false);
                Assert.IsTrue(clock.Running);

                // the strike: Storm.Freeze holds the world at 0 for a while, the clock just waits
                for (int i = 0; i < 60; i++)
                    Assert.IsFalse(clock.Step(Step, 0f, frozen: true, out _), "a frozen frame must not write");
                Assert.IsTrue(clock.Running, "the window must survive the freeze, waiting to resume");

                // Storm.Thaw hands the field back at thawTo (today's bug: its own 0.25; the fix: the punch's own
                // pre-punch scale, 1f)
                scale = Drive(ref clock, 120, thawTo, frozen: false);

                Assert.AreEqual(1f, scale, 1e-4f, $"thaw to {thawTo} must still end the game at 1x");
                Assert.IsFalse(clock.Running, "the window must have ended");
            }
        }

        [Test]
        public void No_Interruption_Leaves_The_Game_Slow()
        {
            foreach (int interruptFrame in new[] { 0, 1, 5, 10, 20 })
            foreach (float foreign in new[] { 0f, 0.25f, 0.5f, 1f })
            {
                var clock = NewClock();
                Assert.IsTrue(clock.TryFire(1f, lockstep: false, out float scale));

                for (int i = 0; i < 300; i++)
                {
                    bool frozen = foreign <= 0.001f && i == interruptFrame;
                    float feed = i == interruptFrame && !frozen ? foreign : scale;
                    if (clock.Step(Step, feed, frozen, out float write)) scale = write;
                    else if (!clock.Running) scale = feed;   // the window dropped: the stranger's value stands

                    // mid-punch (Running, or parked under a freeze) the scale is legitimately between 0.25 and 1
                    // (the ramp, or paused wherever the freeze caught it): only once the window has ended is a
                    // value stuck in that band a bug, unless it is the foreign writer's own value
                    if (clock.Running) continue;
                    bool strandedLow = scale >= PunchPinch.SlowScale - PunchClock.Eps && scale < 1f - PunchClock.Eps;
                    bool isForeignsOwn = System.Math.Abs(scale - foreign) <= PunchClock.Eps;
                    Assert.IsFalse(strandedLow && !isForeignsOwn,
                        $"frame {i}, interrupt {interruptFrame}, foreign {foreign}: scale stuck at {scale}");
                }

                Assert.IsFalse(clock.Running);
            }
        }

        [Test]
        public void Lockstep_Match_Never_Fires_The_Punch()
        {
            var clock = NewClock();
            Assert.IsFalse(clock.TryFire(1f, lockstep: true, out _));
            Assert.IsFalse(clock.Running);

            Assert.IsTrue(clock.TryFire(1f, lockstep: false, out float write));
            Assert.AreEqual(PunchPinch.SlowScale, write);

            var go = new GameObject("TimePinchTest");
            try
            {
                var pinch = go.AddComponent<TimePinch>();

                Time.timeScale = 1f;
                pinch.Fire(true);
                Assert.AreEqual(1f, Time.timeScale, 1e-4f, "lockstep must never fire the punch");

                pinch.Fire(false);
                Assert.AreEqual(PunchPinch.SlowScale, Time.timeScale, 1e-4f);

                Object.DestroyImmediate(pinch);
                Assert.AreEqual(1f, Time.timeScale, 1e-4f, "destroying mid-punch must hand the scale back");
            }
            finally
            {
                Object.DestroyImmediate(go);
            }
        }
    }
}
