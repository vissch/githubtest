// Phase: punch-quarter-second (implemented) — measures PunchPinch's curve from its timing data alone, no scene.
using NUnit.Framework;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class TimePinchTests
    {
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
    }
}
