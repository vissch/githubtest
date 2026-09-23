// Phase: tooling (perf pass, 2026-09-23) — one benchmark for the editor and the Windows player.
// Every performance number this project had was taken in the editor, on a laptop far above the target, with a
// stress run whose battle, camera and sky differed from one run to the next. This measures one fixed battle the
// same way in both places:
//   1. SimHost's stress preset deploys `stress` riflemen a side (SimHost.StressOverride, read in Awake).
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
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Text;
using Unity.Profiling;
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
        /// <summary>Where the last report went and how it ended (0 ok, 3 settle timed out, 4 desync, 5 write failed).</summary>
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
        ulong hashStart;
        int aliveStart;
        double startRealtime;
        bool prevTicked, allFocused = true;
        int gcCollectionsStart;
        long monoStart, monoMax;

        void OnDestroy() { if (Running == this) Running = null; }

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
                case Stage.Settle: Settle(); break;
                case Stage.Approach: Approach(); break;
                case Stage.Warm: Pose(); Warm(); break;
                case Stage.Window: Pose(); Sample(); break;
            }
        }

        // ------------------------------------------------------------------------------------------------ the run
        bool launched;

        void WaitHost()
        {
            if (host == null) host = FindFirstObjectByType<SimHost>();
            if (host == null && !launched && UnityEngine.SceneManagement.SceneManager.GetActiveScene().name == MatchLaunch.MenuScene)
            {
                launched = true;
                MatchLaunch.Start(new MatchLaunch.Request { Title = "PerfBench" });
                return;
            }
            if (host == null || host.Local == null) return;
            if (++waitFrames < 3) return;   // ShellBoot applies settings.json after the scene loads; ours come after it
            if (Options.Quality >= 0 && Options.Quality < QualitySettings.names.Length) QualitySettings.SetQualityLevel(Options.Quality, true);
            QualitySettings.vSyncCount = Options.VSync ? 1 : 0;
            Application.targetFrameRate = -1;
            if (!Application.isEditor) Screen.SetResolution(Options.Width, Options.Height, FullScreenMode.Windowed);
            EventPump.ProfileSubscribers = Options.Subscribers;
            host.TimeScale = Options.FastForward;
            settleDeadline = Time.realtimeSinceStartup + 900f;
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
            tactical = FindFirstObjectByType<TacticalCamera>();
            cam = tactical != null ? tactical.GetComponent<Camera>() : null;
            if (cam == null) cam = Camera.main;
            focus = ArmyCentre(w);
            if (tactical != null) tactical.enabled = false;   // it re-places the camera every frame; the bench holds it
            keepShake = CameraShake.Strength;
            CameraShake.Strength = 0f;
            if (Options.Weather >= 0f) Atmosphere.PinnedClock = Options.Weather;
            warmLeft = Mathf.Max(1, Options.Warm);
            stage = Stage.Warm;
        }

        void Warm()
        {
            if (warmLeft == 10 && !string.IsNullOrEmpty(Options.Shot))
            {
                try { string dir = Path.GetDirectoryName(Path.GetFullPath(Options.Shot)); if (!string.IsNullOrEmpty(dir)) Directory.CreateDirectory(dir); ScreenCapture.CaptureScreenshot(Path.GetFullPath(Options.Shot)); }
                catch (Exception e) { warnings.Add("screenshot failed: " + e.Message); }
            }
            if (--warmLeft > 0) return;
            var w = host.Local.World;
            OpenRecorders();
            t0 = w.Tick; lastTick = t0;
            hashStart = w.Hash();
            aliveStart = w.AliveCount;
            startRealtime = Time.realtimeSinceStartupAsDouble;
            gcCollectionsStart = GC.CollectionCount(0);
            monoStart = monoMax = UnityEngine.Profiling.Profiler.GetMonoUsedSizeLong();
            host.DropBacklog();
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
        VATRenderer vat; BattlefieldProps props; DebrisRenderer debris; GreyboxTerrainView terrain;
        readonly int[] hitchFrame = new int[64]; readonly uint[] hitchTick = new uint[64]; readonly double[] hitchMs = new double[64];
        int hitches, frames;
        ProfilerRecorder mainRec, gpuRec;

        Series New(string key)
        {
            var s = new Series(key, Mathf.Clamp(Options.Ticks * 50, 2000, 200000));
            series.Add(s);
            return s;
        }

        void Counter(ProfilerCategory category, string name, string key, double scale)
        {
            var r = ProfilerRecorder.StartNew(category, name);
            if (!r.Valid) { r.Dispose(); unavailable.Add(name); return; }
            recs.Add(new Rec(New(key), r, scale, false));
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
            Counter(ProfilerCategory.Memory, "GC Allocated In Frame", "gc_bytes", 1);
            Counter(ProfilerCategory.Memory, "GC Allocation In Frame Count", "gc_count", 1);

            foreach (var n in TW.Sim.PerfMarkers.Names) Marker(n);
            foreach (var s in host.Local.World.Systems) Marker("TW.Sim.Sys." + s.GetType().Name);
            Marker("TW.Hud.Refresh");
            if (Options.Subscribers) for (int k = 0; k < host.Events.SubscriberCount; k++) Marker(host.Events.SubscriberMarker(k));

            vat = FindFirstObjectByType<VATRenderer>(); props = FindFirstObjectByType<BattlefieldProps>();
            debris = FindFirstObjectByType<DebrisRenderer>();
            terrain = FindFirstObjectByType<GreyboxTerrainView>();
            vatDrawn = New("vat_drawn"); vatNear = New("vat_near"); vatVerts = New("vat_vertices"); vatShadows = New("vat_shadows_on");
            propDraws = New("props_draw_calls"); propVerts = New("props_vertices"); debrisAlive = New("debris_alive"); debrisDraws = New("debris_draw_calls");
            paintMs = New("terrain_paint_ms"); alive = New("alive");
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
            if (frameDt > 33.34 && hitches < hitchMs.Length) { hitchFrame[hitches] = frames; hitchTick[hitches] = tick; hitchMs[hitches] = frameDt; hitches++; }

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
                if (r.R.Valid) r.S.Add(r.R.LastValue * r.Scale);
            }

            if (vat != null) { vatDrawn.Add(vat.DrawnInfantry); vatNear.Add(vat.DrawnNear); vatVerts.Add(vat.VerticesThisFrame); vatShadows.Add(vat.ShadowsThisFrame ? 1 : 0); }
            if (props != null) { propDraws.Add(props.DrawCalls); propVerts.Add(props.SubmittedVertices); }
            if (debris != null) { debrisAlive.Add(debris.Alive); debrisDraws.Add(debris.DrawCalls); }
            if (terrain != null) paintMs.Add(terrain.LastPaintMilliseconds);
            alive.Add(w.AliveCount);
            long mono = UnityEngine.Profiling.Profiler.GetMonoUsedSizeLong();
            if (mono > monoMax) monoMax = mono;

            if (host.Desync) { Finish(4, "DESYNC during the window at tick " + tick); return; }
            if (tick >= t0 + (uint)Options.Ticks) Finish(0, "ok");
        }

        // ------------------------------------------------------------------------------------------------ report
        void Finish(int code, string why)
        {
            if (stage == Stage.Done) return;
            var endStage = stage;
            stage = Stage.Done;
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
            CameraShake.Strength = keepShake;
            Atmosphere.PinnedClock = -1f;
            EventPump.ProfileSubscribers = false;
            SimHost.StressOverride = -1;
            if (Options.Canary >= 0) SimHost.CanaryOverride = null;
            if (tactical != null) tactical.enabled = true;
            if (host != null) host.TimeScale = 1f;
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
              .Append(", \"frame_timing_stats\": ").Append(FrameTimingManager.IsFeatureEnabled() ? "true" : "false").Append(" },\n");
            sb.Append("  \"scenario\": { \"scene\": ").Append(Q(UnityEngine.SceneManagement.SceneManager.GetActiveScene().name))
              .Append(", \"stress_per_side\": ").Append(Options.Stress)
              .Append(", \"seed\": ").Append(host != null ? host.Seed : 0).Append(", \"battlefield_seed\": ").Append(host != null ? host.BattlefieldSeed : 0)
              .Append(", \"generated\": ").Append(host != null && host.GeneratedBattlefield ? "true" : "false")
              .Append(", \"bombardment_per_min\": ").Append(N(host != null ? host.BombardmentNow : 0))
              .Append(", \"canary\": ").Append(host != null && host.Peer != null ? "true" : "false")
              .Append(", \"view\": { \"focus\": [").Append(N(focus.x)).Append(", ").Append(N(focus.y)).Append("], \"zoom\": ").Append(N(Options.Zoom))
              .Append(", \"yaw\": ").Append(N(Options.Yaw)).Append(", \"pitch\": ").Append(N(Options.Pitch)).Append(" }, \"weather_clock\": ").Append(N(Options.Weather)).Append(" },\n");
            double seconds = startRealtime > 0 ? Time.realtimeSinceStartupAsDouble - startRealtime : 0;
            sb.Append("  \"window\": { \"tick_start\": ").Append(t0).Append(", \"tick_end\": ").Append(w != null ? w.Tick : 0)
              .Append(", \"hash_start\": ").Append(Q(hashStart.ToString("X16"))).Append(", \"alive_peak_before\": ").Append(alivePeak)
              .Append(", \"alive_start\": ").Append(aliveStart).Append(", \"alive_end\": ").Append(w != null ? w.AliveCount : 0)
              .Append(", \"frames\": ").Append(frames).Append(", \"seconds\": ").Append(N(seconds))
              .Append(", \"fps_mean\": ").Append(N(seconds > 0 ? frames / seconds : 0))
              .Append(", \"desync\": ").Append(host != null && host.Desync ? "true" : "false").Append(", \"focused\": ").Append(allFocused ? "true" : "false")
              .Append(", \"gc_collections\": ").Append(GC.CollectionCount(0) - gcCollectionsStart)
              .Append(", \"mono_used_mb\": [").Append(N(monoStart / 1048576.0)).Append(", ").Append(N(monoMax / 1048576.0)).Append("]")
              .Append(", \"hitches_over_33ms\": [");
            for (int i = 0; i < hitches; i++) sb.Append(i > 0 ? ", " : "").Append("[").Append(hitchFrame[i]).Append(", ").Append(hitchTick[i]).Append(", ").Append(N(hitchMs[i])).Append("]");
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
            sb.Append("  \"unavailable\": [");
            for (int i = 0; i < unavailable.Count; i++) sb.Append(i > 0 ? ", " : "").Append(Q(unavailable[i]));
            sb.Append("],\n  \"warnings\": [");
            if (!allFocused) warnings.Add("the window lost focus during the run");
            if (aliveStart >= alivePeak && alivePeak > 0) warnings.Add("nobody had died before the window: the armies may not be in contact yet");
            for (int i = 0; i < warnings.Count; i++) sb.Append(i > 0 ? ", " : "").Append(Q(warnings[i]));
            sb.Append("]\n}\n");
            return sb.ToString();
        }
    }
}
