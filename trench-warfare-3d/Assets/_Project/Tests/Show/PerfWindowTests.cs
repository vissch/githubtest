// Phase: tooling (code review 2026-10-04, F2 + F4) - which frames of the bench's window are samples, and which
// main-thread number may be split into tick and idle frames. Pins Perf/WindowSamples.cs: a wrong rule makes every
// report carry the bench's own set-up cost, or put a tick's cost in the idle series.
using NUnit.Framework;
using TW.Perf;

namespace TW.Tests.Show
{
    /// <summary>The window's sampling rules (code review F2 and F4).</summary>
    public class PerfWindowTests
    {
        [Test]
        public void TheWindowsSetUpFrameIsNotASample()   // [F2]
        {
            Assert.AreEqual(1, WindowSamples.SetupFrames, "[F2] the window's own set-up frame must be dropped");
            Assert.IsFalse(WindowSamples.Counts(1), "[F2] the window's first frame is the bench's set-up frame, not a sample");
            Assert.IsTrue(WindowSamples.Counts(2), "[F2] the second window frame is the first real sample");
        }

        [Test]
        public void OnlyAOneFrameOldMainThreadNumberIsSplitTickOrIdle()   // [F4]
        {
            Assert.IsTrue(WindowSamples.CanSplit(WindowSamples.MainMs.Recorder), "[F4] the Main Thread recorder is one frame late: it may be split");
            Assert.IsFalse(WindowSamples.CanSplit(WindowSamples.MainMs.FrameTiming), "[F4] FrameTimingManager is about four frames late: its value must not be split tick/idle");
        }

        [Test]
        public void AFrameMarkerIsNotDividedBySimTicks()   // [F3]
        {
            Assert.IsFalse(WindowSamples.PerTick("TW.Host.Update"), "[F3] the host's frame runs once a frame, not once a tick");
            Assert.IsFalse(WindowSamples.PerTick("TW.Hud.Refresh"), "[F3] the HUD refresh runs once a frame, not once a tick");
            Assert.IsTrue(WindowSamples.PerTick("TW.Sim.Step"), "[F3] the sim's step is the one thing that runs once a tick");
            // devbase-1 (docs/reference/aosa/runs/1): TW.Host.Update summed 3143.4367 ms over 1773 frames and 400 ticks
            Assert.AreEqual(1.77, WindowSamples.PerFrameMs(3143.4367, 1773), 0.01, "[F3] TW.Host.Update is 1.77 ms a frame");
            Assert.AreEqual(7.86, WindowSamples.PerTickMs(3143.4367, 400), 0.01, "[F3] dividing the same total by ticks reads 7.86, the wrong number the report showed");
            Assert.AreEqual(0.0, WindowSamples.PerFrameMs(3143.4367, 0), 1e-9, "[F3] no frame recorded is 0, not a divide by zero");
            Assert.AreEqual(0.0, WindowSamples.PerTickMs(3143.4367, 0), 1e-9, "[F3] no tick stepped is 0, not a divide by zero");
        }
    }
}
