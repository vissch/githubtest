// Phase: tooling (perf pass, 2026-09-23) — named profiler markers for where a frame and a tick actually go.
// docs/05 budgets the sim per system ("flow field 0.60 ms", "separation + movement 0.60 ms") and has never been able to
// check one line of it, because until now the only marker in the project was the HUD's. These are the names the
// Profiler window, ProfilerRecorder and TW.Perf.PerfBench all read. They live in Sim.Core because every assembly can
// see it; nothing here reads or writes sim state, and a marker has no effect on any hash.
// Cost: Begin/End compile out of a release player (ENABLE_PROFILER is undefined there); in the editor and a development
// build a pair costs tens of nanoseconds. Never put one inside a per-unit loop.
using Unity.Profiling;

namespace TW.Sim
{
    public static class PerfMarkers
    {
        // ---- the sim: one world, one tick
        public static readonly ProfilerMarker SimStep = new ProfilerMarker("TW.Sim.Step");
        public static readonly ProfilerMarker SimHash = new ProfilerMarker("TW.Sim.Hash");

        /// <summary>One marker per registered system, named after its type (TW.Sim.Sys.MovementSystem). Built once, when
        /// the system is added, so no name is ever formatted per tick.</summary>
        public static ProfilerMarker ForSystem(object system) => new ProfilerMarker("TW.Sim.Sys." + system.GetType().Name);

        // ---- the host: what a frame does around the sim
        public static readonly ProfilerMarker HostUpdate = new ProfilerMarker("TW.Host.Update");
        public static readonly ProfilerMarker HostEnemyAi = new ProfilerMarker("TW.Host.EnemyAi");
        public static readonly ProfilerMarker HostStepLocal = new ProfilerMarker("TW.Host.StepLocal");
        public static readonly ProfilerMarker HostStepPeer = new ProfilerMarker("TW.Host.StepPeer");
        public static readonly ProfilerMarker PresentCapture = new ProfilerMarker("TW.Present.Capture");
        public static readonly ProfilerMarker PresentInterpolate = new ProfilerMarker("TW.Present.Interpolate");
        public static readonly ProfilerMarker AnimTick = new ProfilerMarker("TW.Anim.Tick");
        public static readonly ProfilerMarker AnimAdvance = new ProfilerMarker("TW.Anim.Advance");
        public static readonly ProfilerMarker EventsCollect = new ProfilerMarker("TW.Events.Collect");
        public static readonly ProfilerMarker EventsDispatch = new ProfilerMarker("TW.Events.Dispatch");
    }
}
