// Phase: C4 / docs/21 phase 5 (implemented 2026-09-26) — the sim's clock for the picture: the tick the host has stepped
// to plus the fraction of the next it has accumulated, in seconds. Everything the picture times against the sim (the
// strafe aircraft, the beam's sweep, a torch's, a pool's, a pyre's life) reads this and nothing reads Time.time for
// it, so a paused or slowed match holds them with the bursts and the fire; only cosmetic cadence (a flicker, a card
// every so often) keeps the wall clock. One seam for the two clocks (critique round 2).
namespace TW.Presentation
{
    public static class SimClock
    {
        /// <summary>The sim's clock in seconds: the tick plus the fraction of the next; 0 without a match.</summary>
        public static float Seconds(SimHost host)
            => host != null && host.Local != null ? (host.Local.World.Tick + host.Alpha) * host.Local.World.Config.TickSeconds : 0f;

        /// <summary>Ticks to seconds at the match's rate; 0 without a match.</summary>
        public static float Of(SimHost host, uint ticks)
            => host != null && host.Local != null ? ticks * host.Local.World.Config.TickSeconds : 0f;
    }
}
