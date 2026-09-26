// Phase: tooling (perf pass, 2026-09-23) - one benchmark for the editor and the Windows player.
// Every performance number this project had was taken in the editor, on a laptop far above the target, with a
// stress run whose battle, camera and sky differed from one run to the next. This measures one fixed battle the
// same way in both places:
//   1. SimHost's stress preset deploys `stress` riflemen a side (SimHost.StressOverride, read in Awake).
//      S04: `knobs=stress.spread=1` makes the player's army fill the posts of each of its trenches and send the rest
//      over the top; the default is the preset as it was, the whole army in the rear trench (hash_start differs).
//   2. The match fast-forwards to `settle_ticks`, then PAUSES there while `warm` frames render, so late shader
//      variants, Burst and pools are warm and the window always opens on the same tick: a deterministic sim means
//      two runs then measure the same fight (hash_start in the report proves it).
//   3. The camera is held at the standard view over the middle of the army (CaptureRig's pose arithmetic) with the
//      shake off and the weather pinned, and the window records `ticks` sim ticks at 1x.
//   4. The report (json) has p50/p95/p99 of frame, main thread, render thread and GPU time, draw/SetPass/geometry
//      counts, GC, every TW.* profiler marker, the renderers' own counters, memory, and the machine and build.
// Player:  TrenchWarfare.exe -twbench "stress=1500 ticks=400 quality=5 out=C:\...\run.json"
// Editor:  CaptureRig.Bench("stress=1500 ticks=400 out=...") with GreyboxCorridor open (TW_BENCH env var, which
//          survives the domain reload on entering play).
// Markers compile out of a release player, so a release run reports frame totals and a development build attributes
// them; never compare numbers across the two.
// AOSA (2026-09-25): `scenario=` stages barrages, armour or a vfx stack in the window (BenchScenarios), issued as sim
// commands only after hash_start; `knobs=a=1|b=2` sets TW.Presentation.Knobs before the scene loads, and the report
// carries Knobs.ToJson() as config.knobs; window.hash_end fixes the state the window ended on (docs/reference/aosa).
// C33 (2026-09-25): the `shot=` still repeats. Three runs of one build, same hashes, differed on 7.5-10% of the still's
// pixels, because everything before it ran on real time: the men's idle poses (SimHost's animTime), every shader's
// _Time (rain, fog, water, flames, wind), lantern flicker, and whichever effects the fast-forward happened to leave in
// flight. So the lead-in now runs on a HELD clock: in the menu Time.time is walked to a fixed value (AlignClock), then
// every frame from the match's load to the window's first is Time.captureDeltaTime = 1/64 s, UnityEngine.Random starts
// from one seed, the terrain's millisecond budgets are off and the camera is the bench's own from the first settle
// frame. The sim steps the same ticks on the same frames in every run, so every frame before the window is the same
// frame. The clock is released before the window's first frame: the window runs on real time exactly as before.
// `shot_tick=N` instead takes the still N ticks into the window and keeps the clock held through it: an image run of
// the battle running, whose timings are not real time.
// C51 (2026-09-25): `ground=WinterLine` (or `winter`) launches the stress battle on another battlefield, the way a
// mission card does (MatchLaunch.Request.Ground): its map (MatchLaunch.Field) and its look (BiomeProfile.ForGround),
// so a shadow, fog or colour card can be judged in daylight. Absent, the request is exactly the one it always was
// (ShelledForest, night). An unknown name stops the run with exit 2 before the match loads; it never falls back.
// C64 (2026-09-26): each hitch also keeps the frame's largest TW marker (name and ms, HitchAttribution.Largest over the
// recorders' values for the same frame the hitch's dt measured) and the frame's gc_bytes: window.hitch_records, with
// window.hitch_carriers per marker (hitches it was the largest in, median share of the hitch ms). hitches_over_33ms is
// unchanged. A release player has no markers and no gc_bytes: its records say null.
// C66 (2026-09-26): HudController.Refresh's parts carry their own TW.Hud.<Part> markers (HudParts below), recorded
// beside TW.Hud.Refresh, so they land in series and per_tick_ms.
// C69 (2026-09-26): the engine's own markers (EngineMarkers below: the script run, physics, the UI Toolkit panel update
// and repaint, GC.Collect, the wait for present) are series `script:<name>`, per frame, as cmp.py reads them. A name
// whose recorder is not valid goes to unavailable; one that is valid but was not registered yet when the recorders
// opened is listed in script_markers.unregistered_at_open, because its zeros may mean "never ran" or "no such marker".
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Text;
using Unity.Profiling;
using Unity.Profiling.LowLevel.Unsafe;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Presentation.Terrain;
using TW.Presentation.Units;

namespace TW.Perf
{
    /// <summary>Order -32000: its Update runs before BattlefieldProps' (so the editor's Scene-view prop draw is off for
    /// the frame) and its LateUpdate poses the camera before anything culls against it or shakes it.</summary>
    [DefaultExecutionOrder(-32000)]
    public sealed class PerfBench : MonoBehaviour
    {
        public const string CommandLineArg = "-twbench", EnvVar = "TW_BENCH";
        public BenchOptions Options = new BenchOptions();
        /// <summary>The run in progress, or null.</summary>
        public static PerfBench Running { get; private set; }
        /// <summary>Where the last report went and how it ended (0 ok, 2 bad bench option, 3 settle timed out, 4 desync,
        /// 5 write failed).</summary>
        public static string LastResultPath = "";
        public static int LastExitCode = -1;

        [RuntimeInitializeOnLoadMethod(RuntimeInitializeLoadType.BeforeSceneLoad)]
        static void Boot()
        {
            string raw = null;
            var args = Environment.GetCommandLineArgs();
            for (int i = 0; i + 1 < args.Length; i++) if (args[i] == CommandLineArg) { raw = args[i + 1]; break; }
            if (raw == null)
            {
                raw = Environment.GetEnvironmentVariable(EnvVar);
                if (!string.IsNullOrEmpty(raw)) Environment.SetEnvironmentVariable(EnvVar, null);   // one run per request
            }
            if (string.IsNullOrEmpty(raw)) return;
            var o = BenchOptions.Parse(raw);
            // presentation knobs before the scene loads, so every Awake/Start that reads one sees the bench's value
            if (!string.IsNullOrEmpty(o.Knobs)) Knobs.Parse(o.Knobs);
            SimHost.StressOverride = o.Stress;
            if (o.Canary >= 0) SimHost.CanaryOverride = o.Canary == 1;
            // a player boots to the main menu as it always does, and the bench launches the match from there the way the
            // Skirmish button does (MatchLaunch.Start): jumping straight to the battle scene left the menu shell in its
            // menu state, drawn over the whole battle (found by the first player screenshot, 2026-09-23)
            var go = new GameObject("PerfBench");
            DontDestroyOnLoad(go);
            Running = go.AddComponent<PerfBench>();
            Running.Options = o;
            Debug.Log("[PerfBench] armed: " + raw);
            if (o.GroundUnknown) Debug.LogError("[PerfBench] " + UnknownGroundWhy(o));   // the run stops on its first frame (WaitHost)
        }

        enum Stage { WaitHost, Settle, Approach, Warm, Window, Done }
        Stage stage;
        SimHost host;
        TacticalCamera tactical;
        Camera cam;
        Vector2 focus;
        int waitFrames, warmLeft, alivePeak;
        float settleDeadline, keepShake = 1f;
        uint lastTick, t0;
        ulong hashStart, hashEnd;
        uint hashEndTick;
        bool hashEndTaken;
        readonly BenchScenarios.Log scenarioLog = new BenchScenarios.Log();
        int aliveStart;
        double startRealtime;
        bool prevTicked, allFocused = true;
        int gcCollectionsStart;
        long monoStart, monoMax;

        void OnDestroy() { ReleaseClock(); if (Running == this) Running = null; }

        void Update()
        {
            // the editor's prop editor hands BattlefieldProps the Scene view's camera every editor tick, so every prop
            // is drawn twice while a Scene view is open; a player never pays that, so the bench does not either
            if (stage >= Stage.Warm && stage < Stage.Done) BattlefieldProps.EditorCamera = null;
        }

        void LateUpdate()
        {
            switch (stage)
            {
                case Stage.WaitHost: WaitHost(); break;
                case Stage.Settle: Follow(); Settle(); break;
                case Stage.Approach: Follow(); Approach(); break;
                case Stage.Warm: Pose(); HideHud(); Warm(); break;
                case Stage.Window: Pose(); HideHud(); Sample(); break;
            }
        }

        // ------------------------------------------------------------------------------------------------ the run
        bool launched;

        static string UnknownGroundWhy(BenchOptions o) =>
            "unknown ground '" + o.GroundRaw + "': valid are " + BenchOptions.GroundNames() + "; nothing was run";

        void WaitHost()
        {
            // C51: a ground that names no battlefield is refused before anything loads, never run as the wood
            if (Options.GroundUnknown) { Finish(2, UnknownGroundWhy(Options)); return; }
            if (host == null) host = FindFirstObjectByType<SimHost>();
            if (host == null && !launched && UnityEngine.SceneManagement.SceneManager.GetActiveScene().name == MatchLaunch.MenuScene)
            {
                if (!AlignClock()) return;   // some cheap menu frames, walking Time.time to the same value in every run
                HoldClock();                  // before the battle loads: its first frame is already on the held clock
                launched = true;
                MatchLaunch.Start(new MatchLaunch.Request { Title = "PerfBench", Ground = Options.Ground });   // C51: ShelledForest unless ground= said
                return;
            }
            if (host == null || host.Local == null) return;
            // C51: the editor path builds whatever its scene says and never sees the request, so a ground= it did not
            // get is an error, not a mislabelled report (checked only when ground= was given: the default is unchanged)
            if (Options.GroundRaw.Length > 0 && (host.Ground != Options.Ground || !host.GeneratedBattlefield))
            {
                Finish(2, "ground=" + Options.GroundRaw + " asked for " + Options.Ground + ", but the match was built on " +
                          (host.GeneratedBattlefield ? host.Ground.ToString() : "the playtest map") + " (ground= needs the player's menu launch)");
                return;
            }
            HoldClock();   // the editor path (no menu): the scene was already running, so this clock is held but not aligned
            if (++waitFrames < 3) return;   // ShellBoot applies settings.json after the scene loads; ours come after it
            if (Options.Quality >= 0 && Options.Quality < QualitySettings.names.Length) QualitySettings.SetQualityLevel(Options.Quality, true);
            QualitySettings.vSyncCount = Options.VSync ? 1 : 0;
            Application.targetFrameRate = -1;
            if (!Application.isEditor) Screen.SetResolution(Options.Width, Options.Height, FullScreenMode.Windowed);
            EventPump.ProfileSubscribers = Options.Subscribers;
            host.TimeScale = Options.FastForward;
            settleDeadline = Time.realtimeSinceStartup + 900f;
            TakeCamera();
            stage = Stage.Settle;
            Debug.Log($"[PerfBench] settling to tick {Options.SettleTicks} at {Options.FastForward}x with {Options.Stress} a side");
        }

        void Settle()
        {
            var w = host.Local.World;
            alivePeak = Mathf.Max(alivePeak, w.AliveCount);
            if (Time.realtimeSinceStartup > settleDeadline) { Finish(3, "settle timed out at tick " + w.Tick); return; }
            // stop fast-forwarding a little short, so the last ticks arrive one at a time and the pause lands on the tick
            if (w.Tick + 40 < (uint)Options.SettleTicks) return;
            host.TimeScale = 1f;
            host.DropBacklog();
            stage = Stage.Approach;
        }

        void Approach()
        {
            var w = host.Local.World;
            alivePeak = Mathf.Max(alivePeak, w.AliveCount);
            if (w.Tick < (uint)Options.SettleTicks) return;
            host.TimeScale = 0f;   // paused on the tick: the warm-up renders a frozen battle
            host.DropBacklog();
            focus = ArmyCentre(w);   // the window's view: the army's centre on the settle tick, as it always was
            if (Options.Weather >= 0f) Atmosphere.PinnedClock = Options.Weather;
            warmLeft = Mathf.Max(1, Options.Warm);
            stage = Stage.Warm;
        }

        void Warm()
        {
            if (warmLeft == 10 && Options.ShotTick < 0) Shoot("warm");
            if (--warmLeft > 0) return;
            var w = host.Local.World;
            OpenRecorders();
            t0 = w.Tick; lastTick = t0;
            hashStart = w.Hash();
            aliveStart = w.AliveCount;
            // the scenario, after hash_start and before the match resumes: nothing it does can reach the battle the
            // window opened on, and its orders land on the same tick in every run (its few allocations happen before
            // the GC and mono baselines below)
            if (Options.Scenario != BenchScenario.None) BenchScenarios.Issue(Options.Scenario, host, focus, scenarioLog);
            startRealtime = Time.realtimeSinceStartupAsDouble;
            gcCollectionsStart = GC.CollectionCount(0);
            monoStart = monoMax = UnityEngine.Profiling.Profiler.GetMonoUsedSizeLong();
            host.DropBacklog();
            // the window runs on real time, as it always has; only an image run (shot_tick) keeps the clock held
            if (Options.ShotTick < 0) ReleaseClock();
            host.TimeScale = 1f;
            stage = Stage.Window;
        }

        static Vector2 ArmyCentre(TW.Sim.SimWorld w)
        {
            double x = 0, z = 0; int n = 0;
            for (int i = 0; i < w.HighWater; i++)
            {
                if ((w.Flags[i] & (uint)TW.Sim.UnitFlags.Alive) == 0) continue;
                x += w.Position[i].x; z += w.Position[i].z; n++;
            }
            return n > 0 ? new Vector2((float)(x / n), (float)(z / n)) : new Vector2(45f, 120f);
        }

        static readonly int CloseId = Shader.PropertyToID("_TWClose");

        /// <summary>The standard view held on one point: CaptureRig.Rig.Pose's arithmetic (itself TacticalCamera's, with
        /// the battle-following angle neutralised so a run is repeatable). Every frame, because CameraShake and
        /// anything else that nudges the camera would otherwise accumulate while TacticalCamera is off.</summary>
        void Pose()
        {
            if (tactical == null || cam == null || host == null || host.Local == null) return;
            var tc = tactical;
            tc.Zoom = Mathf.Clamp(Options.Zoom, tc.ZoomMin, tc.ZoomMax);
            tc.Focus = focus;
            SceneHooks.CloseUp = 1f - Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(tc.DetailFullZoom, tc.DetailGoneZoom, tc.Zoom));
            Shader.SetGlobalFloat(CloseId, SceneHooks.CloseUp);
            float close = 1f - Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(tc.ZoomMin, tc.CloseZoom, tc.Zoom));
            float fov = Mathf.Lerp(tc.Fov, tc.CloseFov, close);
            cam.fieldOfView = fov; cam.nearClipPlane = 0.2f;
            float pitch = Mathf.Clamp(Options.Pitch, tc.PitchMin, tc.PitchMax);
            var rot = Quaternion.Euler(pitch, tc.BaseYaw + Options.Yaw, 0f);
            float distance = tc.Zoom * Mathf.Tan(30f * Mathf.Deg2Rad) / Mathf.Tan(fov * 0.5f * Mathf.Deg2Rad);
            float ground = host.Local.Map.Height.Sample(focus.x, focus.y);
            var aim = new Vector3(focus.x, ground * close + 1.1f * close, focus.y);
            cam.transform.SetPositionAndRotation(aim - rot * Vector3.forward * distance, rot);
        }

        /// <summary>The camera is the bench's from the first settle frame (C33): TacticalCamera's follow and the shake run
        /// on unscaled time, which the held clock may not hold, and what the camera sees decides which effects are
        /// spawned and how many Random draws they take.</summary>
        void TakeCamera()
        {
            tactical = FindFirstObjectByType<TacticalCamera>();
            cam = tactical != null ? tactical.GetComponent<Camera>() : null;
            if (cam == null) cam = Camera.main;
            if (tactical != null) tactical.enabled = false;   // it re-places the camera every frame; the bench holds it
            keepShake = CameraShake.Strength;
            CameraShake.Strength = 0f;
        }

        /// <summary>The lead-in's view: the standard view over the army's centre on this frame's tick. The window's
        /// focus is the same arithmetic on the settle tick (Approach), so the window's view is unchanged.</summary>
        void Follow()
        {
            if (host == null || host.Local == null) return;
            focus = ArmyCentre(host.Local.World);
            Pose();
        }

        // ------------------------------------------------------------------------------------------- the held clock
        /// <summary>Seconds a frame while the clock is held: a power of two, so Time.time adds up exactly.</summary>
        public const float HeldStep = 1f / 64f;
        /// <summary>Time.time is walked to a multiple of this before the match loads.</summary>
        public const double ClockGrid = 64.0;
        /// <summary>The largest walking step: under Time.maximumDeltaTime (0.333 s), which might clamp a longer one.</summary>
        public const float AlignMaxStep = 0.25f;
        const int RandomSeed = 0x7E5EED;
        double alignTarget = -1.0;
        int alignFrames, heldFrame0, stillFrame = -1, framesShot, lastShotFrame = -1;
        bool clockHeld, clockAligned;
        string stillWhere = "";
        uint stillTick;
        double stillTime, stillUnscaled;

        /// <summary>Where Time.time is walked to: the first multiple of ClockGrid at least a second ahead of now.</summary>
        public static double AlignTarget(double now) => ClockGrid * Math.Ceiling((now + 1.0) / ClockGrid);

        /// <summary>The next frame's captureDeltaTime towards target, or 0 once the clock is there (to 0.1 ms; the last
        /// step lands within a float's rounding of it, far below anything a frame can show).</summary>
        public static float AlignStep(double now, double target)
        {
            double left = target - now;
            return left <= 1e-4 ? 0f : (float)Math.Min(left, AlignMaxStep);
        }

        /// <summary>Menu frames only. Time.time cannot be set, and it reaches the battle at whatever the splash and the
        /// menu took, so everything that reads it absolutely (URP's _Time in every shader, lantern flicker) started each
        /// run somewhere else. True when it is there, or when it cannot get there (the report then says so).</summary>
        bool AlignClock()
        {
            if (alignTarget < 0.0) alignTarget = AlignTarget(Time.timeAsDouble);
            float step = AlignStep(Time.timeAsDouble, alignTarget);
            if (step <= 0f || Time.timeScale != 1f || ++alignFrames > 2000) { clockAligned = step <= 0f; return true; }
            Time.captureDeltaTime = step;
            return false;
        }

        /// <summary>From here to the window every frame is HeldStep long whatever the machine does, UnityEngine.Random
        /// starts from one seed and the terrain's millisecond budgets are off, so every frame of the lead-in is the same
        /// in every run (the sim already was). Nothing here reaches the sim: it steps on ticks, never on frames.</summary>
        void HoldClock()
        {
            if (clockHeld) return;
            Time.captureDeltaTime = HeldStep;
            UnityEngine.Random.InitState(RandomSeed);
            GreyboxTerrainView.Unmetered = true;
            heldFrame0 = Time.frameCount;
            clockHeld = true;
        }

        /// <summary>Real time again from the next frame (the window's first, or wherever the run stopped).</summary>
        void ReleaseClock()
        {
            if (!clockHeld) return;
            Time.captureDeltaTime = 0f;
            GreyboxTerrainView.Unmetered = false;
            clockHeld = false;
        }

        void Capture(string path)
        {
            try { ScreenCapture.CaptureScreenshot(path); }
            catch (Exception e) { warnings.Add("screenshot failed: " + e.Message); }
            framesShot++; lastShotFrame = Time.frameCount;
        }

        /// <summary>`shot_hud=0` on an image run: every UI Toolkit panel's root is set to display none, each frame (a panel
        /// rebuilt later is caught too), and CombatFx's world-space overlays (strike target discs, aiming circle, banner) are
        /// not drawn. The HUD still runs; it is only not drawn.</summary>
        void HideHud()
        {
            if (Options.ShotHud || Options.ShotTick < 0) return;
            TW.Presentation.Tactical.CombatFx.ShowOverlays = false;   // the strike discs and the banner are HUD too, drawn in the world (C56)
            foreach (var doc in FindObjectsByType<UnityEngine.UIElements.UIDocument>(FindObjectsSortMode.None))
                if (doc.rootVisualElement != null) doc.rootVisualElement.style.display = UnityEngine.UIElements.DisplayStyle.None;
        }

        /// <summary>The `shot=` still, once, written at the end of this frame. The report's `still` block says which frame
        /// of the held clock it was, so two runs can be checked for having shot the same frame.</summary>
        void Shoot(string where)
        {
            if (string.IsNullOrEmpty(Options.Shot)) return;
            if (stillFrame >= 0)
            {
                // shot_frames: one more held frame per call after the still, until there are ShotFrames of them
                if (framesShot >= Options.ShotFrames || Time.frameCount == lastShotFrame) return;
                string full = Path.GetFullPath(Options.Shot);
                Capture(Path.Combine(Path.GetDirectoryName(full), Path.GetFileNameWithoutExtension(full) + ".f" + framesShot + ".png"));
                return;
            }
            try { string dir = Path.GetDirectoryName(Path.GetFullPath(Options.Shot)); if (!string.IsNullOrEmpty(dir)) Directory.CreateDirectory(dir); }
            catch (Exception e) { warnings.Add("screenshot folder: " + e.Message); }
            Capture(Path.GetFullPath(Options.Shot));
            stillWhere = where;
            stillTick = host != null && host.Local != null ? host.Local.World.Tick : 0u;
            stillFrame = Time.frameCount - heldFrame0;
            stillTime = Time.timeAsDouble;
            stillUnscaled = Time.unscaledTimeAsDouble;
        }

        // --------------------------------------------------------------------------------------------- recording
        sealed class Series
        {
            public readonly string Key; public readonly double[] V; public int N;
            public Series(string key, int capacity) { Key = key; V = new double[capacity]; }
            public void Add(double x) { if (N < V.Length) V[N++] = x; }
        }

        sealed class Rec
        {
            public readonly Series S; public ProfilerRecorder R; public readonly double Scale; public readonly bool Marker;
            public Rec(Series s, ProfilerRecorder r, double scale, bool marker) { S = s; R = r; Scale = scale; Marker = marker; }
        }

        readonly List<Rec> recs = new List<Rec>();
        readonly List<Series> series = new List<Series>();
        readonly List<string> unavailable = new List<string>(), warnings = new List<string>();
        readonly FrameTiming[] timing = new FrameTiming[1];
        Series dt, cpu, main, render, gpu, mainTick, mainIdle, ticksPerFrame;
        Series vatDrawn, vatNear, vatVerts, vatShadows, propDraws, propVerts, debrisAlive, debrisDraws, paintMs, alive;
        Series budgetDraws, budgetVerts, budgetIndirect;
        VATRenderer vat; BattlefieldProps props; DebrisRenderer debris; GreyboxTerrainView terrain;
        readonly int[] hitchFrame = new int[64]; readonly uint[] hitchTick = new uint[64]; readonly double[] hitchMs = new double[64];
        // C64: per hitch, the largest TW marker's name and ms (null and NaN without one) and the frame's gc_bytes (NaN
        // where the counter is unavailable: a release player)
        readonly string[] hitchMarker = new string[64]; readonly double[] hitchMarkerMs = new double[64], hitchGc = new double[64];
        double[] recNow = new double[0]; bool[] recIsMarker = new bool[0];   // sized once in OpenRecorders: no allocation in the window
        Rec gcRec;
        readonly List<string> scriptRecorded = new List<string>(), scriptUnregistered = new List<string>();
        int hitches, frames;
        ProfilerRecorder mainRec, gpuRec;

        /// <summary>C66: HudController.Refresh's parts, each wrapped in its own marker inside TW.Hud.Refresh (the names
        /// are HudController's; TW.Perf does not reference TW.UI, so they are repeated here).</summary>
        public static readonly string[] HudParts =
        {
            "TW.Hud.Hotkeys", "TW.Hud.LegacyInterim", "TW.Hud.Minimap", "TW.Hud.Dialogue", "TW.Hud.Selection",
            "TW.Hud.BindGauges", "TW.Hud.Cards", "TW.Hud.Clusters", "TW.Hud.Objectives", "TW.Hud.Tooltip",
        };

        /// <summary>C69: the engine's own markers, recorded as `script:<name>` series. The names are the ones Unity 6000.0's
        /// development player carries (read from its UnityPlayer.dll and UnityEngine.UIElementsModule.dll); the last two
        /// are UI Toolkit's managed markers inside the panel update, for C68, and may not resolve.</summary>
        static void EngineMarkers(Action<ProfilerCategory, string> add)
        {
            add(ProfilerCategory.Scripts, "Update.ScriptRunBehaviourUpdate");
            add(ProfilerCategory.Scripts, "PreLateUpdate.ScriptRunBehaviourLateUpdate");
            add(ProfilerCategory.Physics, "FixedUpdate.PhysicsFixedUpdate");
            add(ProfilerCategory.Gui, "PreLateUpdate.UIElementsUpdatePanels");
            add(ProfilerCategory.Gui, "PostLateUpdate.UIElementsRepaintPanels");
            add(ProfilerCategory.Memory, "GC.Collect");
            add(ProfilerCategory.Render, "Gfx.WaitForPresentOnGfxThread");
            add(ProfilerCategory.Gui, "Panel.Layout");
            add(ProfilerCategory.Gui, "RenderChain.Process");
        }

        Series New(string key)
        {
            var s = new Series(key, Mathf.Clamp(Options.Ticks * 50, 2000, 200000));
            series.Add(s);
            return s;
        }

        Rec Counter(ProfilerCategory category, string name, string key, double scale)
        {
            var r = ProfilerRecorder.StartNew(category, name);
            if (!r.Valid) { r.Dispose(); unavailable.Add(name); return null; }
            var rec = new Rec(New(key), r, scale, false);
            recs.Add(rec);
            return rec;
        }

        /// <summary>C69: an engine marker as series `script:<name>` (ns to ms). Not a TW marker: it is kept out of
        /// per_tick_ms and out of the hitch pick, since the script-run markers hold every TW marker under them.</summary>
        void EngineMarker(ProfilerCategory category, string name)
        {
            var r = ProfilerRecorder.StartNew(category, name);
            if (!r.Valid) { r.Dispose(); unavailable.Add(name); return; }
            recs.Add(new Rec(New("script:" + name), r, 1e-6, false));
            scriptRecorded.Add(name);
        }

        /// <summary>Which recorded engine names the profiler had not registered when the recorders opened. Runs once,
        /// before the window's GC baselines, and only when an engine recorder is valid (never on a release player).</summary>
        void CheckRegistered()
        {
            if (scriptRecorded.Count == 0) return;
            try
            {
                var handles = new List<ProfilerRecorderHandle>();
                ProfilerRecorderHandle.GetAvailable(handles);
                var known = new HashSet<string>(StringComparer.Ordinal);
                foreach (var h in handles) known.Add(ProfilerRecorderHandle.GetDescription(h).Name ?? "");
                foreach (var n in scriptRecorded) if (!known.Contains(n)) scriptUnregistered.Add(n);
            }
            catch (Exception e) { warnings.Add("could not list the profiler's markers: " + e.Message); }
        }

        void Marker(string name)
        {
            var r = ProfilerRecorder.StartNew(ProfilerCategory.Scripts, name);
            if (!r.Valid) { r.Dispose(); unavailable.Add(name); return; }
            recs.Add(new Rec(New(name), r, 1e-6, true));
        }

        void OpenRecorders()
        {
            FrameTimingManager.CaptureFrameTimings();
            dt = New("dt_ms"); cpu = New("cpu_frame_ms"); main = New("main_ms"); render = New("render_ms"); gpu = New("gpu_ms");
            mainTick = New("main_ms_tick_frames"); mainIdle = New("main_ms_idle_frames"); ticksPerFrame = New("ticks_per_frame");
            mainRec = ProfilerRecorder.StartNew(ProfilerCategory.Internal, "Main Thread");
            gpuRec = ProfilerRecorder.StartNew(ProfilerCategory.Render, "GPU Frame Time");
            Counter(ProfilerCategory.Render, "SetPass Calls Count", "setpass", 1);
            Counter(ProfilerCategory.Render, "Draw Calls Count", "draw_calls", 1);
            Counter(ProfilerCategory.Render, "Batches Count", "batches", 1);
            Counter(ProfilerCategory.Render, "Triangles Count", "triangles", 1);
            Counter(ProfilerCategory.Render, "Vertices Count", "vertices", 1);
            gcRec = Counter(ProfilerCategory.Memory, "GC Allocated In Frame", "gc_bytes", 1);
            Counter(ProfilerCategory.Memory, "GC Allocation In Frame Count", "gc_count", 1);

            foreach (var n in TW.Sim.PerfMarkers.Names) Marker(n);
            foreach (var s in host.Local.World.Systems) Marker("TW.Sim.Sys." + s.GetType().Name);
            Marker("TW.Hud.Refresh");
            foreach (var n in HudParts) Marker(n);   // C66
            if (Options.Subscribers) for (int k = 0; k < host.Events.SubscriberCount; k++) Marker(host.Events.SubscriberMarker(k));
            EngineMarkers(EngineMarker);   // C69
            CheckRegistered();
            recNow = new double[recs.Count]; recIsMarker = new bool[recs.Count];
            for (int k = 0; k < recs.Count; k++) recIsMarker[k] = recs[k].Marker;

            vat = FindFirstObjectByType<VATRenderer>(); props = FindFirstObjectByType<BattlefieldProps>();
            debris = FindFirstObjectByType<DebrisRenderer>();
            terrain = FindFirstObjectByType<GreyboxTerrainView>();
            vatDrawn = New("vat_drawn"); vatNear = New("vat_near"); vatVerts = New("vat_vertices"); vatShadows = New("vat_shadows_on");
            propDraws = New("props_draw_calls"); propVerts = New("props_vertices"); debrisAlive = New("debris_alive"); debrisDraws = New("debris_draw_calls");
            paintMs = New("terrain_paint_ms"); alive = New("alive");
            // RenderGround's own count of what our code submitted, the last complete frame (acceptance rules 3 and 7)
            budgetDraws = New("frame_budget_draws"); budgetVerts = New("frame_budget_vertices"); budgetIndirect = New("frame_budget_indirect");
#if UNITY_EDITOR
            if (UnityEditor.SceneView.sceneViews.Count > 0)
                warnings.Add(UnityEditor.SceneView.sceneViews.Count + " Scene view(s) open: every Graphics.RenderMesh* call also draws into them, so editor GPU and draw numbers are inflated");
#endif
        }

        void Sample()
        {
            var w = host.Local.World;
            uint tick = w.Tick;
            int stepped = (int)(tick - lastTick);
            lastTick = tick;
            frames++;
            allFocused &= Application.isFocused;

            double frameDt = Time.unscaledDeltaTime * 1000.0;
            dt.Add(frameDt);
            ticksPerFrame.Add(stepped);
            bool hitch = frameDt > 33.34 && hitches < hitchMs.Length;

            double mainMs = -1, gpuMs = -1;
            FrameTimingManager.CaptureFrameTimings();
            if (FrameTimingManager.GetLatestTimings(1, timing) > 0)
            {
                if (timing[0].cpuFrameTime > 0) cpu.Add(timing[0].cpuFrameTime);
                if (timing[0].cpuMainThreadFrameTime > 0) mainMs = timing[0].cpuMainThreadFrameTime;
                if (timing[0].cpuRenderThreadFrameTime > 0) render.Add(timing[0].cpuRenderThreadFrameTime);
                if (timing[0].gpuFrameTime > 0) gpuMs = timing[0].gpuFrameTime;
            }
            if (mainMs < 0 && mainRec.Valid && mainRec.LastValue > 0) mainMs = mainRec.LastValue * 1e-6;
            if (gpuMs < 0 && gpuRec.Valid && gpuRec.LastValue > 0) gpuMs = gpuRec.LastValue * 1e-6;
            if (mainMs >= 0)
            {
                main.Add(mainMs);
                (prevTicked ? mainTick : mainIdle).Add(mainMs);   // the recorders report the frame before this one
            }
            if (gpuMs >= 0) gpu.Add(gpuMs);
            prevTicked = stepped > 0;

            for (int k = 0; k < recs.Count; k++)
            {
                var r = recs[k];
                double v = r.R.Valid ? r.R.LastValue * r.Scale : 0.0;
                if (r.R.Valid) r.S.Add(v);
                if (hitch && k < recNow.Length) recNow[k] = v;
            }
            if (hitch)
            {
                // the recorders hold the frame before this one, the same frame unscaledDeltaTime measured (C64)
                hitchFrame[hitches] = frames; hitchTick[hitches] = tick; hitchMs[hitches] = frameDt;
                int top = HitchAttribution.Largest(recNow, recIsMarker, Math.Min(recNow.Length, recs.Count));
                hitchMarker[hitches] = top >= 0 ? recs[top].S.Key : null;
                hitchMarkerMs[hitches] = top >= 0 ? recNow[top] : double.NaN;
                hitchGc[hitches] = gcRec != null && gcRec.R.Valid ? gcRec.R.LastValue : double.NaN;
                hitches++;
            }

            if (vat != null) { vatDrawn.Add(vat.DrawnInfantry); vatNear.Add(vat.DrawnNear); vatVerts.Add(vat.VerticesThisFrame); vatShadows.Add(vat.ShadowsThisFrame ? 1 : 0); }
            if (props != null) { propDraws.Add(props.DrawCalls); propVerts.Add(props.SubmittedVertices); }
            if (debris != null) { debrisAlive.Add(debris.Alive); debrisDraws.Add(debris.DrawCalls); }
            if (terrain != null) paintMs.Add(terrain.LastPaintMilliseconds);
            alive.Add(w.AliveCount);
            budgetDraws.Add(FrameBudget.DrawCalls); budgetVerts.Add(FrameBudget.Vertices); budgetIndirect.Add(FrameBudget.IndirectDraws);
            long mono = UnityEngine.Profiling.Profiler.GetMonoUsedSizeLong();
            if (mono > monoMax) monoMax = mono;

            if (Options.ShotTick >= 0 && tick >= t0 + (uint)Options.ShotTick) Shoot("window");
            if (host.Desync) { Finish(4, "DESYNC during the window at tick " + tick); return; }
            if (tick >= t0 + (uint)Options.Ticks) Finish(0, "ok");
        }

        // ------------------------------------------------------------------------------------------------ report
        void Finish(int code, string why)
        {
            if (stage == Stage.Done) return;
            var endStage = stage;
            stage = Stage.Done;
            // hash_end: the same full-state hash as hash_start (SimWorld.Hash on the player's world; the one-world build
            // has HashInterval 0, so LastHash is not it), on the tick the window closed
            if (endStage == Stage.Window && host != null && host.Local != null)
            {
                var we = host.Local.World;
                hashEnd = we.Hash(); hashEndTick = we.Tick; hashEndTaken = true;
            }
            string path = "";
            try
            {
                path = Path.GetFullPath(Options.Out);
                string dir = Path.GetDirectoryName(path);
                if (!string.IsNullOrEmpty(dir)) Directory.CreateDirectory(dir);
                File.WriteAllText(path, Report(code, why, endStage));
            }
            catch (Exception e) { Debug.LogError("[PerfBench] could not write the report: " + e.Message); code = 5; }
            foreach (var r in recs) r.R.Dispose();
            recs.Clear();
            if (mainRec.Valid) mainRec.Dispose();
            if (gpuRec.Valid) gpuRec.Dispose();
            ReleaseClock();
            CameraShake.Strength = keepShake;
            Atmosphere.PinnedClock = -1f;
            EventPump.ProfileSubscribers = false;
            SimHost.StressOverride = -1;
            if (Options.Canary >= 0) SimHost.CanaryOverride = null;
            if (tactical != null) tactical.enabled = true;
            // quitting: the match runs on as it was; staying (quit=0): it holds on the window's last tick, so whatever is
            // inspected next (a fingerprint, a still) sees the state the report describes
            if (host != null) { host.DropBacklog(); host.TimeScale = Options.Quit ? 1f : 0f; }
            LastResultPath = path; LastExitCode = code;
            Debug.Log($"[PerfBench] {why}: exit {code}, {frames} frames, report {path}");
            if (!Options.Quit) return;
#if UNITY_EDITOR
            UnityEditor.EditorApplication.ExitPlaymode();
#else
            Application.Quit(code);
#endif
        }

        static readonly CultureInfo Inv = CultureInfo.InvariantCulture;
        static string Q(string s) => "\"" + (s ?? "").Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n").Replace("\r", "") + "\"";
        static string N(double v) => double.IsNaN(v) || double.IsInfinity(v) ? "null" : v.ToString("0.####", Inv);

        static void Stats(StringBuilder sb, Series s, bool last)
        {
            sb.Append("    ").Append(Q(s.Key)).Append(": ");
            if (s.N == 0) { sb.Append("null").Append(last ? "\n" : ",\n"); return; }
            var v = new double[s.N];
            Array.Copy(s.V, v, s.N);
            Array.Sort(v);
            double sum = 0; for (int i = 0; i < v.Length; i++) sum += v[i];
            double P(double q) => v[Math.Min(v.Length - 1, (int)(v.Length * q))];
            sb.Append("{ \"p50\": ").Append(N(P(.5))).Append(", \"p95\": ").Append(N(P(.95))).Append(", \"p99\": ").Append(N(P(.99)))
              .Append(", \"mean\": ").Append(N(sum / v.Length)).Append(", \"max\": ").Append(N(v[v.Length - 1]))
              .Append(", \"sum\": ").Append(N(sum)).Append(", \"n\": ").Append(v.Length).Append(" }").Append(last ? "\n" : ",\n");
        }

        /// <summary>Appends `, "key": ["a", "b"]` to an open object.</summary>
        static void Strings(StringBuilder sb, string key, List<string> items)
        {
            sb.Append(", ").Append(Q(key)).Append(": [");
            for (int i = 0; i < items.Count; i++) sb.Append(i > 0 ? ", " : "").Append(Q(items[i]));
            sb.Append("]");
        }

        /// <summary>Knobs.ToJson() as it stands when the report is written, so `read` holds every knob the game read.</summary>
        string KnobsJson()
        {
            try
            {
                string j = Knobs.ToJson();
                return string.IsNullOrWhiteSpace(j) ? "null" : j.Trim();
            }
            catch (Exception e) { warnings.Add("Knobs.ToJson failed: " + e.Message); return "null"; }
        }

        string Report(int code, string why, Stage endStage)
        {
            var sb = new StringBuilder(16384);
            var w = host != null && host.Local != null ? host.Local.World : null;
            sb.Append("{\n  \"schema\": \"tw-perf/1\",\n");
            sb.Append("  \"run\": { \"label\": ").Append(Q(Options.Label)).Append(", \"utc\": ").Append(Q(DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ssZ", Inv)))
              .Append(", \"context\": ").Append(Q(Application.isEditor ? "editor" : "player"))
              .Append(", \"build\": ").Append(Q(Application.isEditor ? "editor" : Debug.isDebugBuild ? "development" : "release"))
              .Append(", \"args\": ").Append(Q(Options.Raw)).Append(", \"exit\": ").Append(code).Append(", \"why\": ").Append(Q(why))
              .Append(", \"ended_in\": ").Append(Q(endStage.ToString())).Append(" },\n");
            string buildInfo = null;
            try
            {
                string p = Path.Combine(Path.GetDirectoryName(Application.dataPath) ?? "", "build-info.json");
                if (!Application.isEditor && File.Exists(p)) buildInfo = File.ReadAllText(p).Trim();
            }
            catch (Exception) { }
            sb.Append("  \"build_info\": ").Append(string.IsNullOrEmpty(buildInfo) ? "null" : buildInfo).Append(",\n");
            sb.Append("  \"machine\": { \"cpu\": ").Append(Q(SystemInfo.processorType)).Append(", \"cores\": ").Append(SystemInfo.processorCount)
              .Append(", \"ram_mb\": ").Append(SystemInfo.systemMemorySize).Append(", \"gpu\": ").Append(Q(SystemInfo.graphicsDeviceName))
              .Append(", \"gpu_api\": ").Append(Q(SystemInfo.graphicsDeviceVersion)).Append(", \"vram_mb\": ").Append(SystemInfo.graphicsMemorySize)
              .Append(", \"os\": ").Append(Q(SystemInfo.operatingSystem)).Append(", \"refresh_hz\": ").Append(N(Screen.currentResolution.refreshRateRatio.value))
              .Append(", \"battery\": ").Append(Q(SystemInfo.batteryStatus.ToString())).Append(" },\n");
            string backend =
#if ENABLE_IL2CPP
                "il2cpp";
#else
                "mono";
#endif
            sb.Append("  \"config\": { \"quality\": ").Append(Q(QualitySettings.names[QualitySettings.GetQualityLevel()]))
              .Append(", \"vsync\": ").Append(QualitySettings.vSyncCount).Append(", \"target_fps\": ").Append(Application.targetFrameRate)
              .Append(", \"screen\": [").Append(Screen.width).Append(", ").Append(Screen.height).Append("], \"fullscreen\": ").Append(Q(Screen.fullScreenMode.ToString()))
              .Append(", \"backend\": ").Append(Q(backend)).Append(", \"gc_incremental\": ").Append(UnityEngine.Scripting.GarbageCollector.isIncremental ? "true" : "false")
              .Append(", \"frame_timing_stats\": ").Append(FrameTimingManager.IsFeatureEnabled() ? "true" : "false")
              .Append(", \"knobs_arg\": ").Append(Q(Options.Knobs)).Append(", \"knobs\": ").Append(KnobsJson());
            // C51: the battlefield this ran on, the look it got and its map size (another ground is another map)
            var size = host != null && host.Local != null ? (Vector2)host.Local.Map.SizeMeters : Vector2.zero;
            string biome = host != null ? Atmosphere.Profile.Name : "";
            sb.Append(", \"ground\": ").Append(Q(Options.GroundUnknown ? "unknown" : host != null ? host.Ground.ToString() : Options.Ground.ToString()))
              .Append(", \"ground_arg\": ").Append(Q(Options.GroundRaw))
              .Append(", \"biome\": ").Append(Q(biome))
              .Append(", \"night\": ").Append(SceneMood.Night ? "true" : "false")
              .Append(", \"map_m\": [").Append(N(size.x)).Append(", ").Append(N(size.y)).Append("] },\n");
            sb.Append("  \"scenario\": { \"scene\": ").Append(Q(UnityEngine.SceneManagement.SceneManager.GetActiveScene().name))
              .Append(", \"stress_per_side\": ").Append(Options.Stress)
              .Append(", \"seed\": ").Append(host != null ? host.Seed : 0).Append(", \"battlefield_seed\": ").Append(host != null ? host.BattlefieldSeed : 0)
              .Append(", \"generated\": ").Append(host != null && host.GeneratedBattlefield ? "true" : "false")
              .Append(", \"bombardment_per_min\": ").Append(N(host != null ? host.BombardmentNow : 0))
              .Append(", \"canary\": ").Append(host != null && host.Peer != null ? "true" : "false")
              .Append(", \"view\": { \"focus\": [").Append(N(focus.x)).Append(", ").Append(N(focus.y)).Append("], \"zoom\": ").Append(N(Options.Zoom))
              .Append(", \"yaw\": ").Append(N(Options.Yaw)).Append(", \"pitch\": ").Append(N(Options.Pitch)).Append(" }, \"weather_clock\": ").Append(N(Options.Weather))
              .Append(", \"name\": ").Append(Q(BenchOptions.ScenarioName(Options.Scenario))).Append(", \"requested\": ").Append(Q(Options.ScenarioRaw));
            Strings(sb, "commands", scenarioLog.Commands);
            Strings(sb, "writes", scenarioLog.Writes);
            Strings(sb, "presentation", scenarioLog.Presentation);
            sb.Append(" },\n");
            // C33: two runs shot the same frame when every field here but path matches
            sb.Append("  \"still\": { \"path\": ").Append(Q(Options.Shot)).Append(", \"at\": ").Append(Q(stillWhere))
              .Append(", \"tick\": ").Append(stillFrame >= 0 ? stillTick.ToString(Inv) : "null")
              .Append(", \"held_frame\": ").Append(stillFrame >= 0 ? stillFrame.ToString(Inv) : "null")
              .Append(", \"time\": ").Append(stillFrame >= 0 ? stillTime.ToString("0.#########", Inv) : "null")
              .Append(", \"unscaled_time\": ").Append(stillFrame >= 0 ? stillUnscaled.ToString("0.#########", Inv) : "null")
              .Append(", \"clock_aligned\": ").Append(clockAligned ? "true" : "false")
              .Append(", \"held_step\": ").Append(HeldStep.ToString("0.######", Inv))
              .Append(", \"window_clock\": ").Append(Q(Options.ShotTick >= 0 ? "held" : "real")).Append(", \"frames\": ").Append(framesShot.ToString(Inv)).Append(", \"hud\": ").Append(Options.ShotHud || Options.ShotTick < 0 ? "true" : "false").Append(" },\n");
            double seconds = startRealtime > 0 ? Time.realtimeSinceStartupAsDouble - startRealtime : 0;
            sb.Append("  \"window\": { \"tick_start\": ").Append(t0).Append(", \"tick_end\": ").Append(w != null ? w.Tick : 0)
              .Append(", \"hash_start\": ").Append(Q(hashStart.ToString("X16")))
              .Append(", \"hash_end\": ").Append(hashEndTaken ? Q(hashEnd.ToString("X16")) : "null")
              .Append(", \"hash_end_tick\": ").Append(hashEndTaken ? hashEndTick.ToString(Inv) : "null")
              .Append(", \"alive_peak_before\": ").Append(alivePeak)
              .Append(", \"alive_start\": ").Append(aliveStart).Append(", \"alive_end\": ").Append(w != null ? w.AliveCount : 0)
              .Append(", \"frames\": ").Append(frames).Append(", \"seconds\": ").Append(N(seconds))
              .Append(", \"fps_mean\": ").Append(N(seconds > 0 ? frames / seconds : 0))
              .Append(", \"desync\": ").Append(host != null && host.Desync ? "true" : "false").Append(", \"focused\": ").Append(allFocused ? "true" : "false")
              .Append(", \"gc_collections\": ").Append(GC.CollectionCount(0) - gcCollectionsStart)
              .Append(", \"mono_used_mb\": [").Append(N(monoStart / 1048576.0)).Append(", ").Append(N(monoMax / 1048576.0)).Append("]")
              .Append(", \"hitches_over_33ms\": [");
            for (int i = 0; i < hitches; i++) sb.Append(i > 0 ? ", " : "").Append("[").Append(hitchFrame[i]).Append(", ").Append(hitchTick[i]).Append(", ").Append(N(hitchMs[i])).Append("]");
            sb.Append("]");
            // C64: the same hitches with their carrier; null where a release player has no marker or no gc_bytes
            sb.Append(", \"hitch_records\": [");
            for (int i = 0; i < hitches; i++)
                sb.Append(i > 0 ? ", " : "").Append("{ \"frame\": ").Append(hitchFrame[i]).Append(", \"tick\": ").Append(hitchTick[i])
                  .Append(", \"ms\": ").Append(N(hitchMs[i])).Append(", \"marker\": ").Append(hitchMarker[i] != null ? Q(hitchMarker[i]) : "null")
                  .Append(", \"marker_ms\": ").Append(N(hitchMarkerMs[i])).Append(", \"gc_bytes\": ").Append(N(hitchGc[i])).Append(" }");
            sb.Append("], \"hitch_carriers\": [");
            var carriers = HitchAttribution.Summarise(hitchMarker, hitchMarkerMs, hitchMs, hitches);
            for (int i = 0; i < carriers.Count; i++)
                sb.Append(i > 0 ? ", " : "").Append("{ \"marker\": ").Append(Q(carriers[i].Marker)).Append(", \"hitches\": ").Append(carriers[i].Hitches)
                  .Append(", \"median_share\": ").Append(N(carriers[i].MedianShare)).Append(" }");
            sb.Append("] },\n");
            sb.Append("  \"series\": {\n");
            for (int i = 0; i < series.Count; i++) Stats(sb, series[i], i == series.Count - 1);
            sb.Append("  },\n");
            // per sim tick, per world: the number docs/05 budgets (a marker's window total over the ticks it covered)
            int ticks = w != null ? (int)(w.Tick - t0) : 0;
            sb.Append("  \"per_tick_ms\": {");
            bool first = true;
            foreach (var r in recs)
            {
                if (!r.Marker || ticks <= 0) continue;
                double total = 0; for (int i = 0; i < r.S.N; i++) total += r.S.V[i];
                sb.Append(first ? "\n    " : ",\n    ").Append(Q(r.S.Key)).Append(": ").Append(N(total / ticks));
                first = false;
            }
            sb.Append(first ? "},\n" : "\n  },\n");
            // C69: the engine markers recorded as script:<name>, and those not yet registered when the recorders opened
            sb.Append("  \"script_markers\": { \"recorded\": [");
            for (int i = 0; i < scriptRecorded.Count; i++) sb.Append(i > 0 ? ", " : "").Append(Q(scriptRecorded[i]));
            sb.Append("], \"unregistered_at_open\": [");
            for (int i = 0; i < scriptUnregistered.Count; i++) sb.Append(i > 0 ? ", " : "").Append(Q(scriptUnregistered[i]));
            sb.Append("] },\n");
            sb.Append("  \"unavailable\": [");
            for (int i = 0; i < unavailable.Count; i++) sb.Append(i > 0 ? ", " : "").Append(Q(unavailable[i]));
            sb.Append("],\n  \"warnings\": [");
            if (!allFocused) warnings.Add("the window lost focus during the run");
            if (aliveStart >= alivePeak && alivePeak > 0) warnings.Add("nobody had died before the window: the armies may not be in contact yet");
            if (Options.Scenario == BenchScenario.None && Options.ScenarioRaw.Length > 0 && Options.ScenarioRaw.ToLowerInvariant() != "none")
                warnings.Add("unknown scenario '" + Options.ScenarioRaw + "': ran none");
            warnings.AddRange(scenarioLog.Warnings);
            if (!Options.GroundUnknown && Options.Ground != Ground.ShelledForest)
                warnings.Add("ground " + Options.Ground + ": another map (" + N(size.x) + " x " + N(size.y) + " m) and look (" + biome +
                             "), so hash_start is not the night bench's: compare only with runs on the same ground");
            if (Options.ShotTick >= 0)
                warnings.Add("shot_tick: the window ran on the held clock (1/64 s a frame) so its still repeats; its timings are not real time: an image run, never a perf sample");
            if (Options.ShotTick >= 0 && !string.IsNullOrEmpty(Options.Shot) && stillFrame < 0)
                warnings.Add("shot_tick " + Options.ShotTick + " was never reached inside the window: no still");
            if (!clockAligned && !Application.isEditor && !string.IsNullOrEmpty(Options.Shot))
                warnings.Add("the clock was not aligned before the match loaded: this still may not repeat");
            if (hashEndTaken && code == 0 && hashEndTick != t0 + (uint)Options.Ticks)
                warnings.Add("hash_end was taken at tick " + hashEndTick + ", " + (hashEndTick - (t0 + (uint)Options.Ticks)) + " past the window's last tick (a slow last frame stepped more than one tick): compare it only with a run whose hash_end_tick is the same");
            for (int i = 0; i < warnings.Count; i++) sb.Append(i > 0 ? ", " : "").Append(Q(warnings[i]));
            sb.Append("]\n}\n");
            return sb.ToString();
        }
    }
}
