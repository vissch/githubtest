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
    }
}
