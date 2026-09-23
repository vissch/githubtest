// Phase: tooling (perf pass, 2026-09-23) — named profiler markers for where a frame and a tick actually go.
// docs/05 budgets the sim per system ("flow field 0.60 ms", "separation + movement 0.60 ms") and has never been able to
// check one line of it, because until now the only marker in the project was the HUD's. These are the names the
// Profiler window, ProfilerRecorder and TW.Perf.PerfBench all read. They live in Sim.Core because every assembly can
// see it; nothing here reads or writes sim state, and a marker has no effect on any hash.
// Cost: Begin/End compile out of a release player (ENABLE_PROFILER is undefined there); in the editor and a development
// build a pair costs tens of nanoseconds. Never put one inside a per-unit loop.
using System.Collections.Generic;
using Unity.Profiling;

namespace TW.Sim
{
    public static class PerfMarkers
    {
        // every marker below, by name, in declaration order: PerfBench records all of them without a list of its own
        static readonly List<string> names = new List<string>();
        public static IReadOnlyList<string> Names => names;
        static ProfilerMarker M(string name) { names.Add(name); return new ProfilerMarker(name); }

        // ---- the sim: one world, one tick
        public static readonly ProfilerMarker SimStep = M("TW.Sim.Step");
        public static readonly ProfilerMarker SimHash = M("TW.Sim.Hash");

        /// <summary>One marker per registered system, named after its type (TW.Sim.Sys.MovementSystem). Built once, when
        /// the system is added, so no name is ever formatted per tick.</summary>
        public static ProfilerMarker ForSystem(object system) => new ProfilerMarker("TW.Sim.Sys." + system.GetType().Name);

        // ---- the host: what a frame does around the sim
        public static readonly ProfilerMarker HostUpdate = M("TW.Host.Update");
        public static readonly ProfilerMarker HostEnemyAi = M("TW.Host.EnemyAi");
        public static readonly ProfilerMarker HostStepLocal = M("TW.Host.StepLocal");
        public static readonly ProfilerMarker HostStepPeer = M("TW.Host.StepPeer");
        public static readonly ProfilerMarker PresentCapture = M("TW.Present.Capture");
        public static readonly ProfilerMarker PresentInterpolate = M("TW.Present.Interpolate");
        public static readonly ProfilerMarker AnimTick = M("TW.Anim.Tick");
        public static readonly ProfilerMarker AnimAdvance = M("TW.Anim.Advance");
        public static readonly ProfilerMarker EventsCollect = M("TW.Events.Collect");
        public static readonly ProfilerMarker EventsDispatch = M("TW.Events.Dispatch");

        // ---- presentation, per frame: one at the top of each Update/LateUpdate that matters, sections where it is heavy
        public static readonly ProfilerMarker VatLate = M("TW.VAT.Late");
        public static readonly ProfilerMarker TankLate = M("TW.Tank.Late");
        public static readonly ProfilerMarker FxUpdate = M("TW.Fx.Update");
        public static readonly ProfilerMarker FxShake = M("TW.Fx.Shake");
        public static readonly ProfilerMarker DebrisLate = M("TW.Debris.Late");
        public static readonly ProfilerMarker CameraLate = M("TW.Camera.Late");
        public static readonly ProfilerMarker DestructionUpdate = M("TW.Destruction.Update");
        public static readonly ProfilerMarker PropsUpdate = M("TW.Props.Update");
        public static readonly ProfilerMarker PropsCompose = M("TW.Props.Compose");
        public static readonly ProfilerMarker TerrainUpdate = M("TW.Terrain.Update");
        public static readonly ProfilerMarker TerrainHollows = M("TW.Terrain.Hollows");
        public static readonly ProfilerMarker TerrainRepaint = M("TW.Terrain.Repaint");
        public static readonly ProfilerMarker TerrainApply = M("TW.Terrain.Apply");
        public static readonly ProfilerMarker TerrainChunks = M("TW.Terrain.Chunks");
        public static readonly ProfilerMarker AtmosphereLate = M("TW.Atmosphere.Late");
        public static readonly ProfilerMarker NightLightsUpdate = M("TW.NightLights.Update");
        public static readonly ProfilerMarker RainLate = M("TW.Rain.Late");
        public static readonly ProfilerMarker FogUpdate = M("TW.Fog.Update");
        public static readonly ProfilerMarker SmallLifeUpdate = M("TW.SmallLife.Update");
        public static readonly ProfilerMarker StormUpdate = M("TW.Storm.Update");
        public static readonly ProfilerMarker WaterRingsUpdate = M("TW.WaterRings.Update");
        public static readonly ProfilerMarker LandingUpdate = M("TW.Landing.Update");
        public static readonly ProfilerMarker HudLate = M("TW.Hud.Late");
    }
}
