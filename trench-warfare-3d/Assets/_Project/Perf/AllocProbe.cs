// Phase: tooling (perf pass, 2026-09-23) — counts the managed allocations a block of code makes, exactly.
// Why this exists: every allocation test in the project measured with System.GC.GetAllocatedBytesForCurrentThread(),
// and under Unity's Boehm GC that call is a stub. Measured in this editor on 2026-09-23: a 1 MB array, 10,000
// `new object()` and 2,000 formatted strings all read 0 bytes. Every "measured at 0 B" finding recorded in docs/05 and
// in TickAllocationTests was therefore void — including the one that cleared AnimationController.Tick, which builds a
// string for every man on every tick.
// This counts GC.Alloc samples on the calling thread through a Recorder, which is what the test framework's
// Is.Not.AllocatingGCMemory() does internally. Same measurement run: 1 MB array -> 1, 10,000 objects -> 10,000.
// It counts allocations, not bytes: the number that decides whether a hot path is clean is "how many", and a count is
// exact where a byte figure from heap deltas moves in whole blocks and misses small strings entirely.
// Needs the profiler, so it works in the editor and development players and returns -1 in a release player.
using System;
using UnityEngine.Profiling;

namespace TW.Perf
{
    public static class AllocProbe
    {
        static Recorder recorder;
        static bool busy;

        /// <summary>False in a release player, where there is no profiler to count with.</summary>
        public static bool Available => Get() != null;

        static Recorder Get()
        {
            if (recorder == null)
            {
                var r = Recorder.Get("GC.Alloc");
                if (r == null || !r.isValid) return null;
                r.FilterToCurrentThread();
                recorder = r;
            }
            return recorder;
        }

        /// <summary>Managed allocations made by `block` on this thread; -1 when no profiler is available.</summary>
        public static int Count(Action block)
        {
            if (busy) throw new InvalidOperationException("AllocProbe.Count is not re-entrant: every call shares one Recorder");
            var r = Get();
            if (r == null) { block(); return -1; }
            busy = true;
            try
            {
                r.enabled = false;
                r.enabled = true;    // a fresh enable starts the count from zero
                block();
                r.enabled = false;
                return r.sampleBlockCount;
            }
            finally { r.enabled = false; busy = false; }
        }

        /// <summary>Allocations per call, averaged over `reps` calls, after `warm` calls have grown every buffer the
        /// code will ever grow and JIT-compiled everything it runs. Without the warm-up this counts one-time capacity
        /// growth and first-call compilation and calls them a leak.</summary>
        public static double PerCall(Action step, int warm, int reps)
        {
            for (int i = 0; i < warm; i++) step();
            int n = Count(() => { for (int i = 0; i < reps; i++) step(); });
            return n < 0 ? -1 : n / (double)Math.Max(1, reps);
        }
    }
}
