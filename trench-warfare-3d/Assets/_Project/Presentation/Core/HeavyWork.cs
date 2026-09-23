// Phase: tooling (perf pass, 2026-09-23) — at most one heavy, deferrable job a frame.
// A shell landing set off three whole-map jobs in the SAME frame: the terrain rescanned its hollows (11 ms in the
// editor), the props re-laid the whole battlefield (26 ms) and the terrain rebuilt its chunks (27 ms), which with the
// rest of the frame made the 64-85 ms hitches measured on 2026-09-23. None of them has to happen in the frame the shell
// lands: the burst, the smoke and the debris cover the ground for longer than a frame or two. Each asks here first; a
// job that is refused stays pending and runs next frame, so the work is the same and the results identical, only no
// longer stacked.
using UnityEngine;

namespace TW.Presentation
{
    public static class HeavyWork
    {
        static int claimedFrame = -1;

        /// <summary>True for the first heavy job asking this frame; false for every later one, which should wait.</summary>
        public static bool TryClaim()
        {
            int f = Time.frameCount;
            if (claimedFrame == f) return false;
            claimedFrame = f;
            return true;
        }

        /// <summary>Whether a heavy job has already run this frame (for work that may share a frame but not stack).</summary>
        public static bool ClaimedThisFrame => claimedFrame == Time.frameCount;
    }
}
