// Phase: B6 (implemented) — [I3] lock / hold-fire are absolute flags the sim applies 3 ticks later and not at all
// while paused, so a second click computed from stale sim state resent the same value and the toggle looked stuck.
// This remembers what we last asked for per trench and per flag, and shows that instead of the sim's own value
// while the request is still outstanding, so a click always flips what the player is looking at.
using System.Collections.Generic;

namespace TW.UI
{
    public sealed class TrenchFlagRequests
    {
        struct Entry { public int Value; public uint IssuedTick; }

        readonly Dictionary<int, Entry> outstanding = new Dictionary<int, Entry>();

        static int Key(int trench, bool holdFire) => trench * 2 + (holdFire ? 1 : 0);

        /// <summary>The value asked for while it is outstanding, else simValue: dropped once the sim catches up
        /// (simValue equals it) or after 30 ticks (rejected or lost). Ticks, not seconds: a tactical pause stops
        /// the clock, which is exactly when the request must stay outstanding.</summary>
        public int Shown(int trench, bool holdFire, int simValue, uint tick)
        {
            int key = Key(trench, holdFire);
            if (!outstanding.TryGetValue(key, out var e)) return simValue;
            if (e.Value == simValue || tick - e.IssuedTick > 30) { outstanding.Remove(key); return simValue; }
            return e.Value;
        }

        /// <summary>Records and returns the value to send: the opposite of what is currently shown.</summary>
        public int Next(int trench, bool holdFire, int simValue, uint tick)
        {
            int shown = Shown(trench, holdFire, simValue, tick);
            int next = shown != 0 ? 0 : 1;
            outstanding[Key(trench, holdFire)] = new Entry { Value = next, IssuedTick = tick };
            return next;
        }
    }
}
