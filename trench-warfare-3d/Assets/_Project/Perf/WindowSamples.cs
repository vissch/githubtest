// Phase: tooling (code review 2026-10-04, F2 + F4) - which frames of PerfBench's measurement window are samples, and
// which main-thread number may be split into tick and idle frames. Pure rules, no Unity calls: PerfWindowTests pins it.
namespace TW.Perf
{
    public static class WindowSamples
    {
        /// <summary>F2: frames at the start of the window that are not samples. The window's first frame is the bench's
        /// own set-up frame (the recorder arrays were allocated, the world hashed and the scenario's orders issued on
        /// it), and every recorder reports the frame before this one, so its gc_bytes and ms are the bench's own cost:
        /// gc_bytes could never read 0 B and every scenario opened with a hitch nobody in the game pays.</summary>
        public const int SetupFrames = 1;

        /// <summary>True when a window frame counts as a sample. <paramref name="windowFrame"/> is 1-based: frame 1 is
        /// the first frame of the window.</summary>
        public static bool Counts(int windowFrame) => windowFrame > SetupFrames;

        /// <summary>Where a frame's main-thread milliseconds came from.</summary>
        public enum MainMs
        {
            /// <summary>The Main Thread profiler recorder: one frame late, exactly the lag the tick/idle split assumes.</summary>
            Recorder,
            /// <summary>FrameTimingManager: about four frames late, so a tick's cost lands in a later frame.</summary>
            FrameTiming,
        }

        /// <summary>F4: only a one-frame-old number may be put in the tick or the idle series. FrameTimingManager
        /// delivers about four frames late, so splitting its value by "did the previous frame step a tick" put a tick's
        /// cost in the idle frames: tick and idle p50 came out within 1% although a tick steps 3,000 men.</summary>
        public static bool CanSplit(MainMs source) => source == MainMs.Recorder;

        /// <summary>F3: true when a marker's window total may be divided by the sim's ticks. Only the sim's own
        /// markers (its step, its hash, its systems) run once a tick; the host's frame, the HUD and presentation run
        /// once a frame, so dividing them by ticks made a higher frame rate look worse "per tick" (devbase-1:
        /// TW.Host.Update 7.86 "per tick" against 1.77 ms a frame).</summary>
        public static bool PerTick(string marker) =>
            marker != null && marker.StartsWith("TW.Sim.", System.StringComparison.Ordinal);

        /// <summary>A marker's window total over the frames it covered. 0 when no frame was recorded.</summary>
        public static double PerFrameMs(double totalMs, int framesRecorded) =>
            framesRecorded > 0 ? totalMs / framesRecorded : 0.0;

        /// <summary>A marker's window total over the sim ticks it covered. 0 when no tick was stepped.</summary>
        public static double PerTickMs(double totalMs, int ticks) => ticks > 0 ? totalMs / ticks : 0.0;
    }
}
