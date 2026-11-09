// Phase: CelFx job (2026-11-09, lane/show/cel-vfx) - the Range stage and its capture. The owner's six reference clips
// (C:/Users/PC/Downloads/vfx_h3/final/finalfinal, 8 s, 1344x768, 24 fps) are painted over one plate of this game's
// night field with a wooden target dummy (plates/start.png). That plate is an image edit of a real game frame: the
// footage session's beach still dim_novfx/1_side_track/05.png (2026-11-08, Footage.cs PartBeach, shot 1_side_track,
// still 05: no effects, no flare, look.menClose 0), with a dummy painted where the frog rifleman ran.
// stage=beach (the default) replays that shot: the SHELLED WOOD 1917 mission launched as the menu launches it, the
// same settle, the same runner and props cleared, then the camera fixed at the still's exact pose (logged by a
// re-run of the footage) with the plate's crop and scale built into its projection, the runner gone and a
// TargetDummy fitted to the plate's dummy (792,314)-(890,480) where he stood. Frames are drawn at 2x and averaged
// 2x2, as the plate was made (the edit's 2752x1536, halved). stage=corridor is the first Range: GreyboxCorridor at
// night, a dummy on open ground and a camera solved to the plate's dummy box. Either way the effect is fired through
// CelRange at 0.8 s, then fired in the battle and photographed through CaptureRig at T1 (zoom 30), O120 and O240.
//   Unity.exe -batchmode -projectPath <project> -executeMethod TW.Editor.RangeStage.CommandLine
//             -twrange "effect=old_shell out=C:/x/run lod=all run=<id>" -logFile <log>
//   Tools/tw eval 'return TW.Editor.RangeStage.Run("effect=none out=C:/x/run lod=close");'
// Options: stage=beach|corridor (beach) effect=<CelRange id> out=<folder> lod=close|mid|far|all (close) seed=<int> (1917) t0=<s> (0.8)
// frames=<n> (192) peaks=<s,s,..> (seconds after the fire for the mid/far stills; 0.1,0.3,0.6,1.2) run=<id>
// minutes=<watchdog> (12) quit=1 (exit when done; CommandLine adds it).
// Writes into out: frames/f_0000.png.. (close), close.json (per-frame draws), dummy.json (the dummy's projected and
// rendered screen box, the target, the places), lod/<band>_<k>.png + lod/t1_series_NN.png, lod.json, and
// range_run.json last (ok, run id, counts): the caller checks that one for freshness. C:/Users/PC/Documents/celfx/
// tools/capture.ps1 drives it. Time is stepped at 24 fps (Time.captureFramerate) and the presentation's Random is
// seeded, so two runs of one build draw (nearly) the same frames.
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Text;
using Unity.Mathematics;
using UnityEditor;
using UnityEditor.SceneManagement;
using UnityEngine;
using TW.Perf;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Presentation.VFX;
using TW.Sim.Terrain;

namespace TW.Editor
{
    public static class RangeStage
    {
        const string Request = "TW.Range.Request";
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        public const string Entering = "entering Play for the range: ";
        public const string InSession = "range running in this Play session: ";

        // the shot, as solved against plates/start.png (1344x768): the game's close camera (CloseFov 42, ClosePitch 13)
        public const int W = 1344, H = 768, Fps = 24, Frames = 192;
        /// <summary>An unnamed layer the dummy is drawn on, so one render can leave it out (its drawn box).</summary>
        public const int DummyLayer = 30;
        public const float T0 = 0.8f, Fov = 42f, Pitch = 13f, CloseZoom = 7.5f;
        /// <summary>The close camera's heading (degrees; option heading=).</summary>
        public static float Heading = DefaultHeading;
        /// <summary>Picked from five headings (2026-11-09): crates, a sandbag line, warm ruins and a water ribbon behind, like the plate.</summary>
        public const float DefaultHeading = -120f;
        /// <summary>The dummy's box in the plate, pixels from the top left (x0, y0, x1, y1), measured by the orchestrator.</summary>
        public static readonly Vector4 PlateBox = new Vector4(792f, 314f, 890f, 480f);
        /// <summary>Where the clips' two shot lines would meet the frame edge, extended past it (pixels from the top
        /// left): the tracers enter at the lower-left corner, the bolts and beams at the left edge above the head.</summary>
        public static readonly Vector2 MuzzleScreen = new Vector2(-150f, 835f), MuzzleHighScreen = new Vector2(-150f, 175f);

        // ------------------------------------------------------------------------------------- the beach shot (default)
        public const string MenuScene = "Assets/_Project/Scenes/MainMenu.unity";
        public const string Catalog = "Assets/_Project/UI/Missions/MissionCatalog.asset";
        /// <summary>The footage's frame rate: the battle is replayed to the still at it, then filmed at 24.</summary>
        public const int BeachFps = 30;
        /// <summary>The still's camera, logged from the footage re-run (C:/Users/PC/Documents/celfx/ref/rerun1/footage.log,
        /// SNAPLOG 1_side_track/05.png): a 2560x1440 render, fov 32, near 0.2.</summary>
        public static readonly Vector3 BeachCamPos = new Vector3(28.8478928f, 7.710133f, 267.163757f);
        public static readonly Quaternion BeachCamRot = new Quaternion(-6.08345E-09f, -0.9902681f, 0.13917312f, -4.32859935E-08f);
        public const float BeachFov = 32f, BeachSrcW = 2560f, BeachSrcH = 1440f, BeachHeading = 180f;
        /// <summary>The runner at the still (sim position) and where the footage cleared the kit props (centre, and the camera it cleared from).</summary>
        public static readonly Vector2 BeachHero = new Vector2(26.6862526f, 248.878021f);
        public static readonly Vector2 BeachClearAt = new Vector2(23.8939133f, 248.98851f), BeachClearFrom = new Vector2(26.2911034f, 267.252472f);
        /// <summary>Time.time at the footage's still 00 (the wind clock; each later still is 4 frames at 30 fps on).</summary>
        public const float BeachClock0 = 18.6647587f;
        /// <summary>plates/start.png pixel (u, v, 1) -> game-frame pixel (x, y), both from the top left: ECC (affine, dummy
        /// masked, correlation 0.98) of the game still onto the plate. The plate is a crop at 1.86 game px per plate px.</summary>
        public static readonly float[] PlateToGame = { 1.860913174109612f, 0.0019546899804882417f, 28.35248406418461f, 0.00019452836568234488f, 1.8590595200657845f, 4.865661710500717f };

        /// <summary>The still's lens, cropped and scaled the way the plate was: a 2560x1440 fov-32 projection followed by the
        /// plate's affine in normalized device coordinates, so a 1344x768 render lands on start.png pixel for pixel.</summary>
        public static Matrix4x4 BeachProjection(float near, float far)
        {
            var p = Matrix4x4.Perspective(BeachFov, BeachSrcW / BeachSrcH, near, far);
            var w = PlateToGame;
            float a = w[0], b = w[1], c = w[2], d = w[3], e = w[4], f = w[5], det = a * e - b * d;
            float hw = BeachSrcW * 0.5f, hh = BeachSrcH * 0.5f, pw = W * 0.5f, ph = H * 0.5f;
            // game px x = hw (xg + 1), y = hh (1 - yg); plate px (u, v) = W^-1 (x - c, y - f); plate ndc xp = u/pw - 1, yp = 1 - v/ph
            var m = Matrix4x4.identity;
            m.m00 = e * hw / (pw * det); m.m01 = b * hh / (pw * det); m.m03 = (e * (hw - c) - b * (hh - f)) / (pw * det) - 1f;
            m.m10 = d * hw / (ph * det); m.m11 = a * hh / (ph * det); m.m13 = 1f - (a * (hh - f) - d * (hw - c)) / (ph * det);
            return m * p;
        }

        /// <summary>The footage's mission: the first SHELLED WOOD card of the catalog, as the mission list orders it.</summary>
        public static TW.UI.MissionCard BeachCard()
        {
            var cat = AssetDatabase.LoadAssetAtPath<TW.UI.MissionCatalog>(Catalog);
            if (cat == null || cat.Cards == null) return null;
            foreach (var c in cat.Cards) if (c != null && c.Ground == Ground.ShelledForest) return c;
            return null;
        }

        static string StageOf(string options) => (Gym.Opt(options ?? "", "stage") ?? "beach").ToLowerInvariant();

        public static string Run(string options = "")
        {
            if (EditorApplication.isPlaying) { StartRunner(options); return InSession + options; }
            string scene = StageOf(options) == "corridor" ? Scene : MenuScene;
            if (EditorSceneManager.GetActiveScene().path != scene)
            {
                if (!EditorSceneManager.SaveCurrentModifiedScenesIfUserWantsTo()) return "the open scene has unsaved changes: save or discard them first";
                EditorSceneManager.OpenScene(scene);
            }
            SessionState.SetString(Request, options ?? "");
            EditorApplication.EnterPlaymode();
            return Entering + options;
        }

        /// <summary>-executeMethod entry: options from -twrange "&lt;options&gt;"; the editor exits when the run ends
        /// (0 done, 1 it could not run or the watchdog stopped it).</summary>
        public static void CommandLine()
        {
            var args = System.Environment.GetCommandLineArgs();
            string opts = "";
            for (int i = 0; i < args.Length - 1; i++) if (args[i] == "-twrange") opts = args[i + 1];
            string said = Run(opts + " quit=1");
            if (!said.StartsWith(Entering, System.StringComparison.Ordinal) && !said.StartsWith(InSession, System.StringComparison.Ordinal))
            {
                Debug.LogError("Range: the run never started: " + said);
                EditorApplication.Exit(1);
            }
        }

        [InitializeOnLoadMethod]
        static void Hook()
        {
            EditorApplication.playModeStateChanged -= OnPlayMode;
            EditorApplication.playModeStateChanged += OnPlayMode;
            RegisterOldLooks();
        }

        static void OnPlayMode(PlayModeStateChange change)
        {
            if (change != PlayModeStateChange.EnteredPlayMode) return;
            string req = SessionState.GetString(Request, null);
            if (req == null) return;
            SessionState.EraseString(Request);   // a crash must not leave the editor looping
            StartRunner(req);
        }

        static void StartRunner(string options)
        {
            if (StageOf(options) == "beach")
            {
                // the beach run loads its mission, so it lives on its own object that survives the scene load
                var go = new GameObject("RangeRun");
                Object.DontDestroyOnLoad(go);
                go.AddComponent<RangeRun>().Options = options ?? "";
                return;
            }
            var host = Object.FindFirstObjectByType<SimHost>();
            if (host == null) { Debug.LogError("Range: no SimHost in the open scene"); if (Gym.Opt(options, "quit") == "1") EditorApplication.Exit(1); return; }
            var run = host.gameObject.GetComponent<RangeRun>();
            if (run == null) run = host.gameObject.AddComponent<RangeRun>();
            run.Options = options ?? "";
        }

        // ------------------------------------------------------------------------------------------- today's looks
        static FieldInfo booksField;

        static FlipbookFx Books(CombatFx fx)
        {
            if (fx == null) return null;
            if (booksField == null) booksField = typeof(CombatFx).GetField("books", BindingFlags.Instance | BindingFlags.NonPublic);
            var b = booksField?.GetValue(fx) as FlipbookFx;
            return b != null && b.Ready ? b : null;
        }

        sealed class Rounds { public int Next; public CombatFx Fx; }

        const int BulletRounds = 12;
        const float BulletEvery = 0.12f;

        /// <summary>A number in [0, 1) from the seed and a counter: the old looks' jitter, the same every run.</summary>
        static float Hash01(int seed, int k, int salt)
        {
            uint h = (uint)seed * 747796405u + (uint)k * 2891336453u + (uint)salt * 277803737u;
            h ^= h >> 16; h *= 2246822519u; h ^= h >> 13; h *= 3266489917u; h ^= h >> 16;
            return (h & 0xFFFFFF) / 16777216f;
        }

        /// <summary>The placeholders the new looks are judged against: today's shell burst (the sim's own BlastSystem,
        /// a metre short of the dummy on the shooter's side) and today's rifle fire (CombatFx tracers, hit flashes,
        /// puffs and spurts of dirt, drawn by CombatFx's own books).</summary>
        static void RegisterOldLooks()
        {
            CelRange.Register("old_shell", s =>
            {
                var back = s.Direction; back.y = 0f; back = back.sqrMagnitude > 1e-4f ? back.normalized : Vector3.forward;
                Debug.Log("Range old_shell: " + VfxLab.Shell(s.Ground.x - back.x, s.Ground.z - back.z));
            });
            CelRange.Register("old_bullets", s => { s.State = new Rounds { Fx = Object.FindFirstObjectByType<CombatFx>() }; OldRound(s); }, (s, dt) => OldRound(s));
        }

        static void OldRound(RangeShot s)
        {
            if (!(s.State is Rounds r) || r.Fx == null) return;
            float since = Time.time - s.FiredAt;
            var books = Books(r.Fx);
            while (r.Next < BulletRounds && r.Next * BulletEvery <= since + 1e-4f)
            {
                int k = r.Next++;
                float a = Hash01(s.Seed, k, 1), b = Hash01(s.Seed, k, 2), c = Hash01(s.Seed, k, 3);
                bool miss = k % 3 == 2;
                Vector3 right = Vector3.Cross(Vector3.up, s.Direction).normalized;
                Vector3 to = miss
                    ? s.Ground + right * ((a - 0.5f) * 2.4f) + s.Direction * ((b - 0.3f) * 1.6f)
                    : s.Target + right * ((a - 0.5f) * 0.5f) + Vector3.up * ((b - 0.5f) * 0.6f);
                if (miss) to.y = s.Ground.y;
                Vector3 from = s.Muzzle + Vector3.up * ((c - 0.5f) * 0.15f);
                r.Fx.AddTracer(from, to, 1, 1f);   // side 1: the red-orange halo (side 0 is green), nearer the clips
                SceneHooks.Sparks?.Invoke(to, 3);
                if (books == null) continue;
                if (miss)
                    books.Add(FlipbookFx.Book.Spurt, to, 1.3f + 0.6f * c, 0.5f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored, grow: 0.3f, alpha: 0.85f, pop: 0.3f);
                else
                {
                    books.Add(FlipbookFx.Book.Flash, to - s.Direction * 0.15f, 1.8f, 0.09f, roll: a * 6.2832f, glow: 3.2f * SceneTints.Now.Glow, pop: 0.5f);
                    books.Add(FlipbookFx.Book.Puff, to - s.Direction * 0.15f, 1.6f, 0.7f, b < 0.5f ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                        velocity: -s.Direction * 1.3f + Vector3.up * 1.1f, grow: 1.0f, roll: (c - 0.5f), alpha: 0.85f, pop: 0.4f);
                }
            }
        }

        internal static string Inv(float v) => v.ToString("0.####", CultureInfo.InvariantCulture);
        internal static string V2(Vector2 v) => "[" + Inv(v.x) + ", " + Inv(v.y) + "]";
        internal static string V3(Vector3 v) => "[" + Inv(v.x) + ", " + Inv(v.y) + ", " + Inv(v.z) + "]";
    }

    /// <summary>The Range run: a coroutine on the SimHost's object, in Play. Ordered after CaptureRig's rig so its
    /// LateUpdate renders after every renderer has submitted the frame.</summary>
    [DefaultExecutionOrder(30001)]
    public sealed class RangeRun : MonoBehaviour
    {
        public string Options = "";
        static readonly CultureInfo Inv = CultureInfo.InvariantCulture;

        string outDir, effect, runId, lod;
        int seed, frames; float t0;
        float deadline = float.MaxValue; bool finished, quit;
        SimHost host; TacticalCamera tc; Camera cam; TargetDummy dummy;
        float wallStart;
        float wet;
        /// <summary>The field's wetness on the Range (the plate's matte mud); negative keeps the biome's own.</summary>
        public const float DefaultWet = 0.3f;

        // the stage: "beach" (the plate's own shot, the default) or "corridor" (the first Range)
        string stage = "beach";
        bool Beach => stage == "beach";
        float closeUp = 1f;              // SceneHooks.CloseUp while filming (the beach: what the footage's free camera set)
        float poseFov = RangeStage.Fov;
        bool poseCustom; Matrix4x4 poseProj;
        int superSample = 1;             // frames drawn at this many times the size and box-averaged down (the beach: 2)
        bool clockRun; float clockOffset; // the wind clock (Atmosphere.PinnedClock) held to the footage's
        int pendingFps;                  // the frame rate to switch to once frame 0 is drawn
        Vector3 heroFoot; int hero = -1;

        // the close film, driven from Update (the effect's tick) and LateUpdate (the render) so frame k is drawn at
        // exactly k/24 s on the shot clock
        bool filming, begun; int frame;
        RangeShot shot; Vector3 muzzle, muzzleHigh, target, ground;
        Texture2D tex, grabbed, grabbedWith;
        bool grab;
        readonly StringBuilder perFrame = new StringBuilder();
        bool ticking;   // the shot clock runs (mid/far)
        int closeWritten, lodWritten;

        void Update()
        {
            if (!finished && Time.realtimeSinceStartup > deadline) { Debug.LogError("Range: the run passed its wall-clock limit; stopping"); Finish(1); return; }
            if (clockRun) TW.Presentation.Terrain.Atmosphere.PinnedClock = Time.time + clockOffset;
            if (filming)
            {
                HoldPose();
                SceneHooks.CloseUp = closeUp; Shader.SetGlobalFloat("_TWClose", closeUp);
                if (!begun)
                {
                    begun = true;
                    UnityEngine.Random.InitState(seed);
                    CelRange.Begin(muzzle, muzzleHigh, target, ground);
                    shot = CelRange.Fire(effect, t0);
                    if (shot != null) { shot.Lod = RangeLod.Close; shot.Zoom = RangeStage.CloseZoom; shot.Seed = seed; shot.Eye = cam.transform.position; }
                }
                CelRange.Tick();
            }
            else if (ticking) CelRange.Tick();
        }

        void LateUpdate()
        {
            // a render asked for by the coroutine: made here, after every renderer has submitted the frame (a render
            // from the coroutine runs before the LateUpdates and draws nothing procedural, CaptureRig.Run)
            // the dummy's box as drawn: the frame twice in this one LateUpdate, with and without the dummy's layer, so
            // nothing else (grass sway, fog drift) changes between the two
            if (grab)
            {
                int mask = cam.cullingMask;
                grabbedWith = RenderTex();
                cam.cullingMask = mask & ~(1 << RangeStage.DummyLayer);
                grabbed = RenderTex();
                cam.cullingMask = mask;
                grab = false; return;
            }
            if (!filming || !begun) return;
            HoldPose();
            string path = Path.Combine(outDir, "frames", "f_" + frame.ToString("0000", Inv) + ".png");
            Render(path);
            closeWritten++;
            if (pendingFps > 0) { Time.captureFramerate = pendingFps; pendingFps = 0; }   // the beach: replayed at 30, filmed at 24
            if (perFrame.Length > 0) perFrame.Append(",\n");
            perFrame.Append("    {\"f\": ").Append(frame).Append(", \"t\": ").Append(RangeStage.Inv(CelRange.Now))
                .Append(", \"draws\": ").Append(FrameBudget.DrawCalls).Append(", \"verts\": ").Append(FrameBudget.Vertices)
                .Append(", \"indirect\": ").Append(FrameBudget.IndirectDraws).Append("}");
            frame++;
            if (frame >= frames) filming = false;
        }

        // the close camera's pose, held every frame of the film: something in the battle scene took the camera back
        // about 10 s into Play (the first film's last 12 frames were the whole map from far above)
        Vector3 posePos; Quaternion poseRot; bool poseSet, poseWarned;

        void HoldPose()
        {
            if (!poseSet) return;
            if (tc != null && tc.enabled) tc.enabled = false;
            var t = cam.transform;
            if (!poseWarned && ((t.position - posePos).sqrMagnitude > 1e-6f || Quaternion.Angle(t.rotation, poseRot) > 0.01f || Mathf.Abs(cam.fieldOfView - poseFov) > 0.01f))
            {
                poseWarned = true;
                var who = new StringBuilder();
                foreach (var b in cam.GetComponents<Behaviour>()) who.Append(b.GetType().Name).Append(b.enabled ? "+ " : "- ");
                Debug.LogWarning("Range: something moved the camera during the film (frame " + frame + ", to " + t.position + ", fov " + cam.fieldOfView
                    + ", tc " + (tc != null ? tc.enabled + " zoom " + tc.Zoom : "none") + ", main is ours " + (Camera.main == cam) + ", on it: " + who + "); held at the Range pose");
            }
            t.SetPositionAndRotation(posePos, poseRot);
            if (poseCustom) { cam.fieldOfView = poseFov; cam.projectionMatrix = poseProj; return; }   // the beach: the footage never set the zoom
            cam.fieldOfView = RangeStage.Fov; cam.aspect = RangeStage.W / (float)RangeStage.H;
            if (tc != null) tc.Zoom = RangeStage.CloseZoom;
        }

        IEnumerator Start()
        {
            wallStart = Time.realtimeSinceStartup;
            quit = Gym.Opt(Options, "quit") == "1";
            outDir = Gym.Opt(Options, "out");
            effect = Gym.Opt(Options, "effect") ?? "none";
            runId = Gym.Opt(Options, "run") ?? "";
            lod = Gym.Opt(Options, "lod") ?? "close";
            seed = int.TryParse(Gym.Opt(Options, "seed"), out int sd) ? sd : 1917;
            frames = int.TryParse(Gym.Opt(Options, "frames"), out int fr) ? Mathf.Max(1, fr) : RangeStage.Frames;
            t0 = float.TryParse(Gym.Opt(Options, "t0"), NumberStyles.Float, Inv, out float tt) ? tt : RangeStage.T0;
            float minutes = float.TryParse(Gym.Opt(Options, "minutes"), NumberStyles.Float, Inv, out float mm) ? mm : 12f;
            var peaks = new List<float>();
            foreach (var p in (Gym.Opt(Options, "peaks") ?? "0.1,0.3,0.6,1.2").Split(','))
                if (float.TryParse(p, NumberStyles.Float, Inv, out float pv)) peaks.Add(pv);
            deadline = Time.realtimeSinceStartup + minutes * 60f;
            RangeStage.Heading = float.TryParse(Gym.Opt(Options, "heading"), NumberStyles.Float, Inv, out float hd) ? hd : RangeStage.DefaultHeading;
            wet = float.TryParse(Gym.Opt(Options, "wet"), NumberStyles.Float, Inv, out float wt) ? wt : DefaultWet;
            int spotPick = int.TryParse(Gym.Opt(Options, "spot"), out int sp) ? sp : 0;
            stage = (Gym.Opt(Options, "stage") ?? "beach").ToLowerInvariant();
            if (string.IsNullOrEmpty(outDir)) { Debug.LogError("Range: out=<folder> is required"); Finish(1); yield break; }
            if (stage != "beach" && stage != "corridor") { Debug.LogError("Range: stage=beach|corridor, not '" + stage + "'"); Finish(1); yield break; }
            if (!CelRange.Has(effect)) { Debug.LogError("Range: no effect '" + effect + "'; known: " + string.Join(", ", CelRange.Ids())); Finish(1); yield break; }
            Directory.CreateDirectory(Path.Combine(outDir, "frames"));
            bool doClose = lod == "close" || lod == "all", doMid = lod == "mid" || lod == "all", doFar = lod == "far" || lod == "all";

            if (Beach)
            {
                superSample = int.TryParse(Gym.Opt(Options, "ss"), out int ssv) ? Mathf.Clamp(ssv, 1, 2) : 2;
                bool ok = false;
                yield return BeachRun(peaks, doClose, doMid, doFar, r => ok = r);
                if (!ok) yield break;
                WriteRunRecord();
                Finish(0);
                yield break;
            }

            Time.captureFramerate = RangeStage.Fps;
            UnityEngine.Random.InitState(seed);
            host = GetComponent<SimHost>();
            float until = Time.realtimeSinceStartup + 60f;
            while ((host.Local == null || host.Local.World.Tick < 60) && Time.realtimeSinceStartup < until) yield return null;
            if (host.Local == null) { Debug.LogError("Range: the match never started"); Finish(1); yield break; }
            var night = FindFirstObjectByType<TW.Presentation.Terrain.NightLights>();
            if (night != null) night.Rewind();
            var director = GymDirector.Attach(host);
            if (!director.Quiet()) Debug.LogWarning("Range: could not stop the ambient shells (worlds unaligned)");
            Calm();
            tc = FindFirstObjectByType<TacticalCamera>();
            cam = Camera.main;
            if (tc == null || cam == null) { Debug.LogError("Range: no TacticalCamera or Camera.main"); Finish(1); yield break; }
            // settle: about 13 s into Play something in the battle scene re-frames the camera once (zoom 14.9, the field's
            // fog changes with it); the first films caught it in their last frames. Wait past it, then pose.
            int settle = int.TryParse(Gym.Opt(Options, "settle"), out int st) ? st : 16 * RangeStage.Fps;
            for (int f = 0; f < settle; f++) yield return null;

            var spots = Spots(director.Stage, 5);
            if (spots.Count == 0) { Debug.LogError("Range: no open ground for the dummy"); Finish(1); yield break; }
            int found = spots.Count;
            for (int k = 0; spots.Count < 5; k++) { Debug.LogWarning("Range: only " + spots.Count + " spots; the battle stills share one"); spots.Add(found > 1 ? spots[1 + k % (found - 1)] : spots[0]); }
            Debug.Log("Range: effect " + effect + ", lod " + lod + ", spots " + string.Join(" ", spots.ConvertAll(s => s.ToString("0", Inv))));

            if (spotPick > 0 && spotPick < spots.Count) { var t = spots[0]; spots[0] = spots[spotPick]; spots[spotPick] = t; }
            if (doClose) yield return Close(spots[0], night);
            if (doMid || doFar)
            {
                if (doClose) for (int f = 0; f < 10 * RangeStage.Fps; f++) yield return null;   // let the close shot's smoke go
                yield return Lod(spots, peaks, doMid, doFar);
            }
            WriteRunRecord();
            Finish(0);
        }

        void WriteRunRecord()
        {
            var sb = new StringBuilder("{\n");
            sb.Append("  \"run\": \"").Append(Gym.Esc(runId)).Append("\",\n");
            sb.Append("  \"stage\": \"").Append(stage).Append("\", \"effect\": \"").Append(Gym.Esc(effect)).Append("\", \"lod\": \"").Append(lod).Append("\", \"seed\": ").Append(seed).Append(",\n");
            sb.Append("  \"close_frames\": ").Append(closeWritten).Append(", \"lod_stills\": ").Append(lodWritten).Append(",\n");
            sb.Append("  \"sha\": \"").Append(Gym.ShortSha()).Append("\", \"wall_seconds\": ").Append(RangeStage.Inv(Time.realtimeSinceStartup - wallStart)).Append(",\n");
            sb.Append("  \"ok\": true\n}\n");
            File.WriteAllText(Path.Combine(outDir, "range_run.json"), sb.ToString());
            Debug.Log("Range: done, " + closeWritten + " close frames and " + lodWritten + " lod stills in " + outDir);
        }

        /// <summary>No weather, no shake, no lightning, no overlays, a pinned sky: the shot is the effect and nothing else.</summary>
        void Calm()
        {
            var sky = FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; sky.Lightning = false; }
            // the plate's mud is matte; the night biome's own wet sheen (0.85 / 0.75) is cut to its value (option wet=)
            if (sky != null && wet >= 0f) { sky.Wetness = wet; sky.WetGlint = wet * 0.6f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;
            TW.Presentation.Terrain.Storm.Hold = true;
            CameraShake.Strength = 0f; CameraShake.Reset();
            CombatFx.ShowOverlays = false;
        }

        // ---------------------------------------------------------------------------------------------- the beach shot
        /// <summary>The plate's own shot (Footage.cs PartBeach + BeachShot "1_side_track" with fx=0 men=0, still 05),
        /// replayed step for step at the footage's 30 fps, then filmed from that still's exact camera.</summary>
        IEnumerator BeachRun(List<float> peaks, bool doClose, bool doMid, bool doFar, System.Action<bool> done)
        {
            done(false);
            RangeStage.Heading = RangeStage.BeachHeading;
            Time.captureFramerate = RangeStage.BeachFps;
            UnityEngine.Random.InitState(seed);
            var card = RangeStage.BeachCard();
            if (card == null) { Debug.LogError("Range: no SHELLED WOOD card in " + RangeStage.Catalog); Finish(1); yield break; }
            var req = card.ToRequest(card.DefaultDifficulty, card.BattlefieldSeed);
            Debug.Log("Range: beach: launching mission " + card.Id + " (" + card.Title + "), battlefield seed " + req.BattlefieldSeed + ", ground " + req.Ground + ", difficulty " + req.Difficulty);
            MatchLaunch.Start(req);
            yield return null;
            float until = Time.realtimeSinceStartup + 120f;
            while (Time.realtimeSinceStartup < until)
            {
                host = FindFirstObjectByType<SimHost>();
                if (host != null && host.Local != null && host.Local.World.Tick > 40 && FindFirstObjectByType<TW.UI.HudController>() != null) break;
                yield return null;
            }
            if (host == null || host.Local == null) { Debug.LogError("Range: the beach match never started"); Finish(1); yield break; }
            var map = host.Local.Map;
            Debug.Log("Range: beach: match up, tick " + host.Local.World.Tick + ", shore " + RangeStage.Inv(map.ShoreZ) + ", sand from " + RangeStage.Inv(map.SeaStartZ) + ", map " + map.SizeMeters + ", t " + RangeStage.Inv(Time.time));
            var director = GymDirector.Attach(host);
            director.Quiet();
            yield return null;
            VfxLab.Rain(0f);
            float was = host.TimeScale; host.TimeScale = 3f;
            for (int f = 0; f < 4 * RangeStage.BeachFps; f++) yield return null;
            host.TimeScale = was;
            Knobs.Set("look.menClose", "0");
            CameraShake.Strength = 0f; CameraShake.Reset();

            // BeachShot: the stage cleared, the foes gone, the camera on the still's pose
            director.Clear(); ClearFoes();
            for (int i = 0; i < 3 + 4; i++) yield return null;
            tc = FindFirstObjectByType<TacticalCamera>();
            cam = Camera.main;
            if (tc == null || cam == null) { Debug.LogError("Range: no TacticalCamera or Camera.main"); Finish(1); yield break; }
            tc.enabled = false;
            closeUp = tc.CloseUpAt(tc.NearZoom);
            cam.nearClipPlane = 0.2f;
            posePos = RangeStage.BeachCamPos; poseRot = RangeStage.BeachCamRot; poseFov = RangeStage.BeachFov;
            poseProj = RangeStage.BeachProjection(cam.nearClipPlane, cam.farClipPlane); poseCustom = true; poseSet = true;
            cam.transform.SetPositionAndRotation(posePos, poseRot); cam.fieldOfView = poseFov;   // placed before the hold starts watching it
            Debug.Log("Range: beach: closeUp " + RangeStage.Inv(closeUp) + ", tc zoom " + RangeStage.Inv(tc.Zoom) + ", far " + RangeStage.Inv(cam.farClipPlane));
            for (int i = 0; i < Mathf.RoundToInt(1.5f * RangeStage.BeachFps); i++) { PoseBeach(); yield return null; }

            // the runner: his footprints are in the plate, and the dummy stands where he is at the still
            float zs = Mathf.Lerp(map.SeaStartZ, map.ShoreZ, 0.45f);
            hero = Runner(director, new Vector2(20f, zs), new Vector2(map.SizeMeters.x - 8f, zs), 3.2f);
            if (hero < 0) { Debug.LogError("Range: no runner"); Finish(1); yield break; }
            int runFrames = Mathf.RoundToInt(1.3f * RangeStage.BeachFps);
            for (int i = 0; i < runFrames; i++) { if (i % 5 == 0) ClearFoes(); PoseBeach(); yield return null; }
            ClearProps(RangeStage.BeachClearAt, 7f, 3f, RangeStage.BeachClearFrom);

            // the footage's stills 00..05, four frames each; still 05 is film frame 0
            for (int s = 0; s <= 5; s++)
            {
                ClearFoes();
                // the wind clock: at this still's frame (3 frames on) it reads what the footage's did
                clockOffset = RangeStage.BeachClock0 + s * 4f / RangeStage.BeachFps - (Time.time + 3f / RangeStage.BeachFps); clockRun = true;
                if (s == 3)
                {
                    var hp = host.Local.World.Position[hero];
                    Debug.Log("Range: beach: runner at " + RangeStage.Inv(hp.x) + ", " + RangeStage.Inv(hp.z) + " at still 03 (the footage: 25.7296, 248.957)");
                    int sl = hero;
                    host.WriteWorlds(m => { if (m.World.IsAlive(sl)) m.World.Despawn(sl); });
                }
                if (s == 4) PlaceBeachDummy(map);
                if (s == 4) grab = true;   // the drawn box, at still 04's frame (the runner gone, the dummy in)
                for (int i = 0; i < 3; i++)
                {
                    PoseBeach();
                    if (s == 5 && i == 2) { StartFilm(); }   // frame 0 is drawn on the next frame: the still's
                    yield return null;
                }
                if (s < 5) { PoseBeach(); yield return null; }
            }
            var without = grabbed; var with = grabbedWith;
            var drawn = without != null && with != null ? DrawnBox(without, with) : new Vector4(-1, -1, -1, -1);
            if (without != null) Object.Destroy(without); if (with != null) Object.Destroy(with);
            WriteDummy(drawn);
            if (doClose)
            {
                int n = 0;
                while (filming) { if (n++ % 4 == 0) ClearFoes(); yield return null; }
                File.WriteAllText(Path.Combine(outDir, "close.json"), "{\n  \"stage\": \"beach\", \"effect\": \"" + Gym.Esc(effect) + "\", \"fps\": " + RangeStage.Fps + ", \"t0\": " + RangeStage.Inv(t0)
                    + ", \"zoom\": " + RangeStage.Inv(RangeStage.CloseZoom) + ",\n  \"frames\": [\n" + perFrame + "\n  ]\n}\n");
            }
            else { filming = false; Time.captureFramerate = RangeStage.Fps; pendingFps = 0; }
            CelRange.Clear();
            ReleaseFlare();
            if (dummy != null) { Object.Destroy(dummy); dummy = null; }
            poseSet = false; poseCustom = false; clockRun = false;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            cam.ResetProjectionMatrix(); cam.ResetAspect();
            tc.enabled = true;
            superSample = 1;   // the battle stills are CaptureRig's

            if (doMid || doFar)
            {
                var spots = Spots(RangeStage.BeachHero, 5, 14f);   // near the dummy: the sand strip is narrow, 32 m apart sends them 100 m off
                if (spots.Count == 0) { Debug.LogError("Range: no open ground near the dummy for the battle stills"); Finish(1); yield break; }
                int found = spots.Count;
                for (int k = 0; spots.Count < 5; k++) spots.Add(found > 1 ? spots[1 + k % (found - 1)] : spots[0]);
                // Lod takes O240 = [1], O120 = [2], T1 = [3], the T1 series = [4]: T1 on the dummy's own spot (its smoke has had
                // the 10 s below and the far bands' time), the series on the next nearest; the far bands see the whole strip
                spots[3] = spots[0]; spots[4] = spots[1];
                Debug.Log("Range: beach battle stills at " + string.Join(" ", spots.ConvertAll(q => q.ToString("0", Inv))));
                if (doClose) for (int f = 0; f < 10 * RangeStage.Fps; f++) yield return null;   // let the close shot's smoke go
                yield return Lod(spots, peaks, doMid, doFar);
            }
            done(true);
        }

        void PoseBeach() { HoldPose(); SceneHooks.CloseUp = closeUp; Shader.SetGlobalFloat("_TWClose", closeUp); }

        /// <summary>Begin the close film so that its frame 0 is drawn on the next frame: the places, the shot clock (in
        /// that frame's Update) and the switch to 24 fps after frame 0. The night's star shell is held off for the film:
        /// the plate has none, and one bursting mid-shot would light the whole field.</summary>
        void StartFilm()
        {
            HoldFlare();
            frame = 0; begun = false; filming = true; pendingFps = RangeStage.Fps;
        }

        FieldInfo flareField; TW.Presentation.Terrain.NightLights flareOwner; float flareWas;

        void HoldFlare()
        {
            flareOwner = FindFirstObjectByType<TW.Presentation.Terrain.NightLights>();
            flareField = typeof(TW.Presentation.Terrain.NightLights).GetField("nextFlare", BindingFlags.Instance | BindingFlags.NonPublic);
            if (flareOwner == null || flareField == null) { Debug.LogWarning("Range: could not hold the star shell off (no NightLights.nextFlare)"); return; }
            flareWas = (float)flareField.GetValue(flareOwner);
            flareField.SetValue(flareOwner, float.MaxValue);
            Debug.Log("Range: beach: star shell held off for the film (it was due at t " + RangeStage.Inv(flareWas) + ", now " + RangeStage.Inv(Time.time) + ")");
        }

        void ReleaseFlare()
        {
            if (flareOwner != null && flareField != null) flareField.SetValue(flareOwner, Mathf.Max(flareWas, Time.time + 6f));
            flareOwner = null;
        }

        /// <summary>The dummy on the runner's spot, turned like the plate's (facing the camera, 28 degrees round so its side
        /// shows), then moved a little on the ground and scaled until its projected box is the plate's dummy box.</summary>
        void PlaceBeachDummy(MapData map)
        {
            var hp = host.Local.World.Position[hero];
            heroFoot = new Vector3(RangeStage.BeachHero.x, RenderGround.Sample(map, RangeStage.BeachHero.x, RangeStage.BeachHero.y), RangeStage.BeachHero.y);
            dummy = gameObject.AddComponent<TargetDummy>();
            dummy.Layer = RangeStage.DummyLayer;
            cam.cullingMask |= 1 << RangeStage.DummyLayer;
            HoldPose();
            float yaw = RangeStage.BeachHeading + 180f + 28f;
            if (float.TryParse(Gym.Opt(Options, "dummyyaw"), NumberStyles.Float, Inv, out float dy)) yaw = dy;
            var foot = heroFoot; float scale = 1f, width = 1f;
            dummy.Depth = float.TryParse(Gym.Opt(Options, "dummydepth"), NumberStyles.Float, Inv, out float dd) ? dd : 2f;   // the plate's board is a thick slab
            // 0.4 of the corridor's warm self-light: the board then reads the plate dummy's mean colour (65, 61, 58 RGB)
            dummy.SetEmission(new Color(0.13f, 0.09f, 0.05f) * (float.TryParse(Gym.Opt(Options, "dummyglow"), NumberStyles.Float, Inv, out float dg) ? dg : 0.4f));
            var plate = RangeStage.PlateBox;
            float wantCx = (plate.x + plate.z) * 0.5f, wantBottom = plate.w, wantH = plate.w - plate.y, wantW = plate.z - plate.x;
            var pts = new List<Vector3>();
            bool fit = Gym.Opt(Options, "dummyfit") != "0";
            for (int it = 0; it < (fit ? 40 : 1); it++)
            {
                dummy.Place(foot, yaw); dummy.Scale = scale; dummy.Width = width;
                dummy.WorldPoints(pts);
                var box = Project(pts);
                float cx = (box.x + box.z) * 0.5f, h = box.w - box.y, bw = box.z - box.x;
                if (!fit) break;
                scale *= wantH / Mathf.Max(1f, h);
                width = Mathf.Clamp(width * wantW / Mathf.Max(1f, bw * wantH / Mathf.Max(1f, h)), 0.5f, 2.5f);
                var fs = ToScreen(foot);
                var r = cam.ViewportPointToRay(Viewport(fs + new Vector2(wantCx - cx, wantBottom - box.w)));
                if (Mathf.Abs(r.direction.y) < 1e-4f) break;
                var q = r.origin + r.direction * ((foot.y - r.origin.y) / r.direction.y);
                foot = new Vector3(q.x, RenderGround.Sample(map, q.x, q.z), q.z);
            }
            dummy.Place(foot, yaw); dummy.Scale = scale; dummy.Width = width;
            ground = foot; target = dummy.Chest;
            muzzle = OnPlane(RangeStage.MuzzleScreen, foot.y + 1.3f);
            var hr = cam.ViewportPointToRay(Viewport(RangeStage.MuzzleHighScreen));
            float depth = Vector3.Dot(target - cam.transform.position, cam.transform.forward);
            muzzleHigh = hr.origin + hr.direction * (depth / Mathf.Max(0.05f, Vector3.Dot(hr.direction, cam.transform.forward)));
            Debug.Log("Range: beach: dummy foot " + RangeStage.V3(foot) + " (" + RangeStage.Inv(Vector2.Distance(new Vector2(foot.x, foot.z), RangeStage.BeachHero)) + " m from the runner's spot "
                + RangeStage.V3(heroFoot) + ", runner now " + RangeStage.Inv(hp.x) + ", " + RangeStage.Inv(hp.z) + "), scale " + RangeStage.Inv(scale) + ", width " + RangeStage.Inv(width) + ", depth " + RangeStage.Inv(dummy.Depth) + ", yaw " + RangeStage.Inv(yaw));
        }

        void ClearFoes()
        {
            host.WriteWorlds(m =>
            {
                var w = m.World;
                for (int i = 0; i < w.HighWater; i++)
                    if ((w.Flags[i] & (uint)TW.Sim.UnitFlags.Alive) != 0 && w.Team[i] == 1) w.Despawn(i);
            });
        }

        /// <summary>The footage's runner: a frog rifleman that cannot die, sent along the sand at `speed`.</summary>
        int Runner(GymDirector dir, Vector2 at, Vector2 to, float speed)
        {
            var d = to - at; float yaw = Mathf.Atan2(d.x, d.y) * Mathf.Rad2Deg;
            int sl = dir.Spawn(0, TW.Sim.InfantryArchetype.Frog, at.x, at.y, yaw);
            if (sl < 0) return -1;
            host.WriteWorlds(m =>
            {
                var w = m.World; w.Hp[sl] = 1e6f; w.MaxHp[sl] = 1e6f; w.Speed[sl] = speed;
                var c = m.Map.NavCellOf(new float3(to.x, 0f, to.y));
                w.GoalId[sl] = m.Fields.GetGoal(TW.Sim.Nav.GoalKey.Cell(m.Map.NavIndex(c.x, c.y), TW.Sim.Nav.NavMode.Infantry));
            });
            return sl;
        }

        /// <summary>The footage's prop clearing: every kit prop (wrecks, stakes, trees) out of the draw within `radius` of
        /// `focus` and within `corridor` of the line from the camera (`from`) to it.</summary>
        void ClearProps(Vector2 focus, float radius, float corridor, Vector2 from)
        {
            var bp = FindFirstObjectByType<TW.Presentation.Terrain.BattlefieldProps>();
            if (bp == null) { Debug.LogWarning("Range: no BattlefieldProps to clear"); return; }
            var fi = typeof(TW.Presentation.Terrain.BattlefieldProps).GetField("batches", BindingFlags.NonPublic | BindingFlags.Instance);
            var dict = fi != null ? fi.GetValue(bp) as System.Collections.IDictionary : null;
            if (dict == null) { Debug.LogWarning("Range: ClearProps: no batches"); return; }
            var mods = new List<TW.Presentation.Terrain.BattlefieldKit.Module>();
            foreach (var k in dict.Keys) if (k is TW.Presentation.Terrain.BattlefieldKit.Module mm) mods.Add(mm);
            var found = new List<(int page, int slot, Matrix4x4 m)>(); int hidden = 0;
            foreach (var mm in mods)
                for (int i = 0; i <= 10; i++)
                {
                    found.Clear(); bp.Within(mm, Vector2.Lerp(from, focus, i / 10f), i == 10 ? radius : corridor, found);
                    foreach (var f in found) { bp.Hide(mm, f.page, f.slot); hidden++; }
                    if (i == 10) { found.Clear(); bp.Within(mm, focus, radius, found); foreach (var f in found) { bp.Hide(mm, f.page, f.slot); hidden++; } }
                }
            Debug.Log("Range: beach: props hidden " + hidden + " (the footage: 25)");
        }

        // ----------------------------------------------------------------------------------------------- the close film
        IEnumerator Close(Vector2 spot, TW.Presentation.Terrain.NightLights night)
        {
            var map = host.Local.Map;
            var foot = new Vector3(spot.x, RenderGround.Sample(map, spot.x, spot.y), spot.y);
            dummy = gameObject.AddComponent<TargetDummy>();
            dummy.Place(foot, RangeStage.Heading + 180f + 28f);   // faces the camera turned 28 degrees, as the plate's does (its side shows)
            dummy.Layer = RangeStage.DummyLayer;
            cam.cullingMask |= 1 << RangeStage.DummyLayer;
            tc.enabled = false;
            tc.Zoom = RangeStage.CloseZoom;
            cam.fieldOfView = RangeStage.Fov; cam.nearClipPlane = 0.2f; cam.aspect = RangeStage.W / (float)RangeStage.H;
            var rot = Quaternion.Euler(RangeStage.Pitch, RangeStage.Heading, 0f);
            Solve(rot, foot);
            posePos = cam.transform.position; poseRot = rot; poseSet = true;
            float camGround = RenderGround.Sample(map, cam.transform.position.x, cam.transform.position.z);
            if (cam.transform.position.y < camGround + 0.6f) Debug.LogWarning("Range: the camera is at the ground (" + cam.transform.position + ")");
            SceneHooks.CloseUp = 1f; Shader.SetGlobalFloat("_TWClose", 1f);
            if (night != null) night.CutLamps();

            ground = foot; target = dummy.Chest;
            muzzle = OnPlane(RangeStage.MuzzleScreen, foot.y + 1.3f);
            var hr = cam.ViewportPointToRay(Viewport(RangeStage.MuzzleHighScreen));
            float depth = Vector3.Dot(target - cam.transform.position, cam.transform.forward);
            muzzleHigh = hr.origin + hr.direction * (depth / Mathf.Max(0.05f, Vector3.Dot(hr.direction, cam.transform.forward)));

            for (int f = 0; f < 24; f++) { HoldPose(); SceneHooks.CloseUp = 1f; Shader.SetGlobalFloat("_TWClose", 1f); yield return null; }
            // the dummy's box as drawn: one frame without it and one with it, differenced
            grab = true; while (grab) yield return null;
            var without = grabbed; var with = grabbedWith;
            var drawn = DrawnBox(without, with);
            Object.Destroy(without); Object.Destroy(with);
            WriteDummy(drawn);

            frame = 0; begun = false; filming = true;
            while (filming) yield return null;
            File.WriteAllText(Path.Combine(outDir, "close.json"), "{\n  \"effect\": \"" + Gym.Esc(effect) + "\", \"fps\": " + RangeStage.Fps + ", \"t0\": " + RangeStage.Inv(t0)
                + ", \"zoom\": " + RangeStage.Inv(RangeStage.CloseZoom) + ",\n  \"frames\": [\n" + perFrame + "\n  ]\n}\n");
            CelRange.Clear();
            Object.Destroy(dummy); dummy = null;
            poseSet = false;
            cam.ResetAspect();
            tc.enabled = true;
        }

        static Vector3 Viewport(Vector2 px) => new Vector3(px.x / RangeStage.W, 1f - px.y / RangeStage.H, 0f);

        Vector3 OnPlane(Vector2 px, float y)
        {
            var r = cam.ViewportPointToRay(Viewport(px));
            float t = Mathf.Abs(r.direction.y) > 1e-4f ? (y - r.origin.y) / r.direction.y : -1f;
            if (t <= 0f) t = 8f;
            return r.origin + r.direction * t;
        }

        /// <summary>Move the camera (its rotation held) until the dummy's projected box has the plate's centre and height.</summary>
        void Solve(Quaternion rot, Vector3 foot)
        {
            var pts = new List<Vector3>();
            dummy.WorldPoints(pts);
            Vector3 fwd = rot * Vector3.forward, right = rot * Vector3.right, up = rot * Vector3.up;
            Vector3 chest = foot + Vector3.up * TargetDummy.ChestHeight;
            Vector3 c = chest - fwd * 11.5f;
            float wantX = (RangeStage.PlateBox.x + RangeStage.PlateBox.z) * 0.5f, wantY = (RangeStage.PlateBox.y + RangeStage.PlateBox.w) * 0.5f, wantH = RangeStage.PlateBox.w - RangeStage.PlateBox.y;
            float tanHalf = Mathf.Tan(RangeStage.Fov * 0.5f * Mathf.Deg2Rad);
            for (int it = 0; it < 40; it++)
            {
                cam.transform.SetPositionAndRotation(c, rot);
                var box = Project(pts);
                float cx = (box.x + box.z) * 0.5f, cy = (box.y + box.w) * 0.5f, h = box.w - box.y;
                float depth = Vector3.Dot(chest - c, fwd);
                float pxPerM = RangeStage.H / (2f * tanHalf * depth);
                c += fwd * (depth - depth * h / wantH);
                c -= right * ((wantX - cx) / pxPerM);
                c += up * ((wantY - cy) / pxPerM);
            }
            cam.transform.SetPositionAndRotation(c, rot);
        }

        /// <summary>The screen box (pixels from the top left) of world points through the current camera.</summary>
        Vector4 Project(List<Vector3> pts)
        {
            float x0 = float.MaxValue, y0 = float.MaxValue, x1 = float.MinValue, y1 = float.MinValue;
            foreach (var p in pts)
            {
                var v = cam.WorldToViewportPoint(p);
                float x = v.x * RangeStage.W, y = (1f - v.y) * RangeStage.H;
                x0 = Mathf.Min(x0, x); x1 = Mathf.Max(x1, x); y0 = Mathf.Min(y0, y); y1 = Mathf.Max(y1, y);
            }
            return new Vector4(x0, y0, x1, y1);
        }

        Vector2 ToScreen(Vector3 p) { var v = cam.WorldToViewportPoint(p); return new Vector2(v.x * RangeStage.W, (1f - v.y) * RangeStage.H); }

        /// <summary>The pixels that changed between the frame without the dummy and with it: its box as drawn, outline
        /// included (a row or column counts only with at least two changed pixels, so a flicker elsewhere does not).</summary>
        static Vector4 DrawnBox(Texture2D a, Texture2D b)
        {
            var pa = a.GetPixels32(); var pb = b.GetPixels32();
            int w = a.width, h = a.height;
            var rows = new int[h]; var cols = new int[w];
            for (int y = 0; y < h; y++)
                for (int x = 0; x < w; x++)
                {
                    int i = y * w + x;
                    int d = Mathf.Abs(pa[i].r - pb[i].r) + Mathf.Abs(pa[i].g - pb[i].g) + Mathf.Abs(pa[i].b - pb[i].b);
                    if (d > 24) { rows[h - 1 - y]++; cols[x]++; }
                }
            int x0 = -1, x1 = -1, y0 = -1, y1 = -1;
            for (int x = 0; x < w; x++) if (cols[x] >= 2) { if (x0 < 0) x0 = x; x1 = x; }
            for (int y = 0; y < h; y++) if (rows[y] >= 2) { if (y0 < 0) y0 = y; y1 = y; }
            return new Vector4(x0, y0, x1 + 1, y1 + 1);
        }

        void WriteDummy(Vector4 drawn)
        {
            var pts = new List<Vector3>();
            dummy.WorldPoints(pts);
            var box = Project(pts);
            var plate = RangeStage.PlateBox;
            var sb = new StringBuilder("{\n");
            sb.Append("  \"bbox\": [").Append(RangeStage.Inv(box.x)).Append(", ").Append(RangeStage.Inv(box.y)).Append(", ").Append(RangeStage.Inv(box.z)).Append(", ").Append(RangeStage.Inv(box.w)).Append("],\n");
            sb.Append("  \"rendered_bbox\": [").Append(drawn.x).Append(", ").Append(drawn.y).Append(", ").Append(drawn.z).Append(", ").Append(drawn.w).Append("],\n");
            sb.Append("  \"plate_bbox\": [").Append(plate.x).Append(", ").Append(plate.y).Append(", ").Append(plate.z).Append(", ").Append(plate.w).Append("],\n");
            sb.Append("  \"target\": ").Append(RangeStage.V2(ToScreen(target))).Append(",\n");
            sb.Append("  \"ground\": ").Append(RangeStage.V2(ToScreen(ground))).Append(",\n");
            sb.Append("  \"muzzle\": ").Append(RangeStage.V2(ToScreen(muzzle))).Append(", \"muzzle_high\": ").Append(RangeStage.V2(ToScreen(muzzleHigh))).Append(",\n");
            sb.Append("  \"w\": ").Append(RangeStage.W).Append(", \"h\": ").Append(RangeStage.H).Append(",\n");
            sb.Append("  \"world\": {\"target\": ").Append(RangeStage.V3(target)).Append(", \"ground\": ").Append(RangeStage.V3(ground))
                .Append(", \"muzzle\": ").Append(RangeStage.V3(muzzle)).Append(", \"muzzle_high\": ").Append(RangeStage.V3(muzzleHigh)).Append("},\n");
            if (Beach)
            {
                var e = cam.transform.rotation.eulerAngles; var q = cam.transform.rotation;
                sb.Append("  \"stage\": \"beach\",\n");
                sb.Append("  \"dummy\": {\"foot\": ").Append(RangeStage.V3(dummy.Foot)).Append(", \"scale\": ").Append(RangeStage.Inv(dummy.Scale)).Append(", \"width\": ").Append(RangeStage.Inv(dummy.Width)).Append(", \"depth\": ").Append(RangeStage.Inv(dummy.Depth)).Append(", \"yaw\": ").Append(RangeStage.Inv(dummy.Yaw))
                    .Append(", \"runner_spot\": ").Append(RangeStage.V3(heroFoot)).Append(", \"from_runner_m\": ").Append(RangeStage.Inv(Vector2.Distance(new Vector2(dummy.Foot.x, dummy.Foot.z), RangeStage.BeachHero))).Append("},\n");
                sb.Append("  \"camera\": {\"pos\": ").Append(RangeStage.V3(cam.transform.position)).Append(", \"rot\": [").Append(RangeStage.Inv(q.x)).Append(", ").Append(RangeStage.Inv(q.y)).Append(", ")
                    .Append(RangeStage.Inv(q.z)).Append(", ").Append(RangeStage.Inv(q.w)).Append("], \"euler\": ").Append(RangeStage.V3(e))
                    .Append(", \"fov\": ").Append(RangeStage.Inv(RangeStage.BeachFov)).Append(", \"source\": [2560, 1440], \"near\": ").Append(RangeStage.Inv(cam.nearClipPlane))
                    .Append(", \"plate_to_source_px\": [").Append(string.Join(", ", System.Array.ConvertAll(RangeStage.PlateToGame, v => v.ToString("R", Inv)))).Append("]")
                    .Append(", \"supersample\": ").Append(superSample).Append(", \"close_up\": ").Append(RangeStage.Inv(closeUp))
                    .Append(", \"distance\": ").Append(RangeStage.Inv(Vector3.Distance(cam.transform.position, target))).Append("}\n}\n");
            }
            else
            sb.Append("  \"camera\": {\"pos\": ").Append(RangeStage.V3(cam.transform.position)).Append(", \"pitch\": ").Append(RangeStage.Inv(RangeStage.Pitch))
                .Append(", \"heading\": ").Append(RangeStage.Inv(RangeStage.Heading)).Append(", \"fov\": ").Append(RangeStage.Inv(RangeStage.Fov))
                .Append(", \"distance\": ").Append(RangeStage.Inv(Vector3.Distance(cam.transform.position, target))).Append("}\n}\n");
            File.WriteAllText(Path.Combine(outDir, "dummy.json"), sb.ToString());
        }

        Texture2D RenderTex()
        {
            var t = new Texture2D(RangeStage.W, RangeStage.H, TextureFormat.RGB24, false);
            Draw(t);
            return t;
        }

        void Render(string path)
        {
            try
            {
                if (tex == null) tex = new Texture2D(RangeStage.W, RangeStage.H, TextureFormat.RGB24, false);
                Draw(tex);
                File.WriteAllBytes(path, tex.EncodeToPNG());
            }
            catch (System.Exception ex) { Debug.LogError("Range: frame " + path + " failed: " + ex); }
        }

        Texture2D big;

        /// <summary>The camera into a 1344x768 RGB24 texture. With superSample 2 it draws at 2688x1536 and averages each
        /// 2x2 block on the stored (sRGB) values, the way the plate was halved from the edit's 2752x1536 (cv2 INTER_AREA).</summary>
        void Draw(Texture2D into)
        {
            int ss = superSample, w = RangeStage.W * ss, h = RangeStage.H * ss;
            var rt = RenderTexture.GetTemporary(w, h, 24, RenderTextureFormat.ARGB32);
            cam.targetTexture = rt; cam.Render(); cam.targetTexture = null;
            if (poseCustom) cam.projectionMatrix = poseProj;
            var was = RenderTexture.active; RenderTexture.active = rt;
            if (ss == 1)
            {
                into.ReadPixels(new Rect(0, 0, w, h), 0, 0); into.Apply(false);
                RenderTexture.active = was; RenderTexture.ReleaseTemporary(rt);
                return;
            }
            if (big == null || big.width != w || big.height != h) { if (big != null) Destroy(big); big = new Texture2D(w, h, TextureFormat.RGB24, false); }
            big.ReadPixels(new Rect(0, 0, w, h), 0, 0);
            RenderTexture.active = was; RenderTexture.ReleaseTemporary(rt);
            var src = big.GetRawTextureData<byte>();
            var dst = into.GetRawTextureData<byte>();
            int sw = w * 3, dw = RangeStage.W * 3;
            for (int y = 0; y < RangeStage.H; y++)
            {
                int r0 = 2 * y * sw, r1 = r0 + sw, o = y * dw;
                for (int x = 0; x < dw; x += 3)
                {
                    int s0 = r0 + 2 * x, s1 = r1 + 2 * x;
                    for (int c = 0; c < 3; c++)
                        dst[o + x + c] = (byte)((src[s0 + c] + src[s0 + 3 + c] + src[s1 + c] + src[s1 + 3 + c] + 2) >> 2);
                }
            }
            into.Apply(false);
        }

        // ------------------------------------------------------------------------------------------------- mid and far
        static float PitchAt(float zoom) => Mathf.Lerp(25f, 62f, Mathf.InverseLerp(60f, 240f, zoom));   // the game's camera tips over from 60 to 240

        IEnumerator Lod(List<Vector2> spots, List<float> peaks, bool mid, bool far)
        {
            string dir = Path.Combine(outDir, "lod"); Directory.CreateDirectory(dir);
            CaptureRig.Rig.YawPin = -90f;   // the pose is the same every run, whatever the battle camera drifted to
            var bands = new List<(string name, float zoom, RangeLod tier, Vector2 spot)>();
            if (far) { bands.Add(("O240", 240f, RangeLod.Far, spots[1])); bands.Add(("O120", 120f, RangeLod.Far, spots[2])); }
            if (mid) bands.Add(("T1", 30f, RangeLod.Mid, spots[3]));
            var json = new StringBuilder("{\n  \"effect\": \"" + Gym.Esc(effect) + "\",\n  \"bands\": [\n");
            for (int bi = 0; bi < bands.Count; bi++)
            {
                var (name, zoom, tier, spot) = bands[bi];
                if (bi > 0) for (int f = 0; f < 6 * RangeStage.Fps; f++) yield return null;
                float pitch = PitchAt(zoom);
                var still = new StringBuilder();
                // the frame without the effect, from the same pose: the draws the effect adds are counted against it
                string pre = Path.Combine(dir, name + "_pre.png");
                yield return Still(pre, spot, zoom, pitch);
                int baseDraws = FrameBudget.DrawCalls; long baseVerts = FrameBudget.Vertices; int baseInd = FrameBudget.IndirectDraws;
                var s = Arm(spot, tier, zoom);
                for (int k = 0; k < peaks.Count; k++)
                {
                    while (CelRange.Now < peaks[k] - 1f / RangeStage.Fps) yield return null;
                    string path = Path.Combine(dir, name + "_" + k + ".png");
                    yield return Still(path, spot, zoom, pitch);
                    if (still.Length > 0) still.Append(",\n");
                    still.Append("      {\"path\": \"").Append(Path.GetFileName(path)).Append("\", \"t\": ").Append(RangeStage.Inv(Time.time - (s != null && s.Fired ? s.FiredAt : Time.time)))
                        .Append(", \"draws\": ").Append(FrameBudget.DrawCalls).Append(", \"verts\": ").Append(FrameBudget.Vertices).Append(", \"indirect\": ").Append(FrameBudget.IndirectDraws)
                        .Append(", \"draws_added\": ").Append(FrameBudget.DrawCalls - baseDraws).Append(", \"verts_added\": ").Append(FrameBudget.Vertices - baseVerts).Append("}");
                }
                ticking = false; CelRange.Clear();
                json.Append(bi > 0 ? ",\n" : "").Append("    {\"band\": \"").Append(name).Append("\", \"zoom\": ").Append(RangeStage.Inv(zoom)).Append(", \"pitch\": ").Append(RangeStage.Inv(pitch))
                    .Append(", \"tier\": \"").Append(tier).Append("\", \"spot\": ").Append(RangeStage.V2(spot))
                    .Append(",\n     \"baseline\": {\"draws\": ").Append(baseDraws).Append(", \"verts\": ").Append(baseVerts).Append(", \"indirect\": ").Append(baseInd).Append("},\n     \"stills\": [\n")
                    .Append(still).Append("\n     ]}");
            }
            json.Append("\n  ]");
            if (mid)
            {
                // two seconds at T1 from the fire, every second frame (12 a second: each still is posed, then drawn)
                for (int f = 0; f < 6 * RangeStage.Fps; f++) yield return null;
                var spot = spots[4];
                float pitch = PitchAt(30f);
                var s = Arm(spot, RangeLod.Mid, 30f);
                yield return null;
                CaptureRig.Series(dir, "t1_series", spot.x, spot.y, 30f, 30f, pitch, 24, 0, RangeStage.W, RangeStage.H);
                string last = Path.Combine(dir, "t1_series_23.png");
                float until = Time.realtimeSinceStartup + 120f;
                int most = 0;
                while (!File.Exists(last) && Time.realtimeSinceStartup < until) { most = Mathf.Max(most, FrameBudget.DrawCalls); yield return null; }
                for (int k = 0; k < 24; k++) if (File.Exists(Path.Combine(dir, "t1_series_" + k.ToString("00") + ".png"))) lodWritten++;
                ticking = false; CelRange.Clear();
                json.Append(",\n  \"t1_series\": {\"stills\": 24, \"every_s\": ").Append(RangeStage.Inv(2f / RangeStage.Fps)).Append(", \"spot\": ").Append(RangeStage.V2(spot)).Append(", \"most_draws\": ").Append(most).Append("}");
            }
            json.Append("\n}\n");
            File.WriteAllText(Path.Combine(outDir, "lod.json"), json.ToString());
            CaptureRig.Rig.YawPin = float.NaN;
        }

        /// <summary>The effect queued at a battle spot (fires at once): the shot comes along the close shot's line.</summary>
        RangeShot Arm(Vector2 spot, RangeLod tier, float zoom)
        {
            var map = host.Local.Map;
            var g = new Vector3(spot.x, RenderGround.Sample(map, spot.x, spot.y), spot.y);
            Vector3 along = target - muzzle; along.y = 0f;
            if (along.sqrMagnitude < 1e-4f) along = Quaternion.Euler(0f, RangeStage.Heading + 90f, 0f) * Vector3.forward;   // no close shot this run: across the view, from its left
            along.Normalize();
            var t = g + Vector3.up * TargetDummy.ChestHeight;
            UnityEngine.Random.InitState(seed);
            CelRange.Begin(t - along * 35f + Vector3.up * 0.35f, t - along * 35f + Vector3.up * 3f, t, g);
            var s = CelRange.Fire(effect, 0f);
            if (s != null) { s.Lod = tier; s.Zoom = zoom; s.Seed = seed; s.Eye = cam.transform.position; }
            ticking = true;
            return s;
        }

        IEnumerator Still(string path, Vector2 spot, float zoom, float pitch)
        {
            if (File.Exists(path)) File.Delete(path);
            CaptureRig.Shot(path, spot.x, spot.y, zoom, 30f, pitch, RangeStage.W, RangeStage.H);
            float until = Time.realtimeSinceStartup + 60f;
            while (!File.Exists(path) && Time.realtimeSinceStartup < until) yield return null;
            yield return null;
            if (File.Exists(path)) lodWritten++;
            else Debug.LogError("Range: still " + path + " was never written");
        }

        // --------------------------------------------------------------------------------------------- where to stand
        /// <summary>`count` spots of open ground, 32 m apart, nearest the gym's stage first: no trench, wire, bunker or
        /// blocked cell, prop or cover near, and nobody alive near. The field is cluttered, so the first pass asks for 12 m
        /// clear and nobody within 30 m, and each later pass asks less of the spots still missing.</summary>
        List<Vector2> Spots(Vector2 near, int count, float apart = 32f)
        {
            var map = host.Local.Map;
            var w = host.Local.World;
            var size = map.SizeMeters;
            var cands = new List<Vector2>();
            for (float z = 30f; z < size.y - 30f; z += 4f)
                for (float x = 30f; x < size.x - 30f; x += 4f) cands.Add(new Vector2(x, z));
            cands.Sort((a, b) => (a - near).sqrMagnitude.CompareTo((b - near).sqrMagnitude));
            var used = new List<Vector2>();
            var back = -new Vector2(Mathf.Sin(RangeStage.Heading * Mathf.Deg2Rad), Mathf.Cos(RangeStage.Heading * Mathf.Deg2Rad));
            foreach (var (r, alone) in new[] { (12f, 30f), (9f, 25f), (7f, 20f), (5f, 15f), (4f, 10f) })
                foreach (var c in cands)
                {
                    if (used.Count >= count) return used;
                    bool clear = true;
                    foreach (var u in used) if ((u - c).sqrMagnitude < apart * apart) { clear = false; break; }
                    if (!clear || !Open(map, c.x, c.y, r)) continue;
                    // the close camera stands about 11 m back along the heading: open ground there too
                    var cb = c + back * 11f;
                    if (!Open(map, cb.x, cb.y, Mathf.Min(5f, r))) continue;
                    bool nobody = true;
                    for (int i = 0; i < w.HighWater && nobody; i++)
                        if (w.IsAlive(i) && math.distancesq(new float2(w.Position[i].x, w.Position[i].z), new float2(c.x, c.y)) < alone * alone) nobody = false;
                    if (!nobody) continue;
                    used.Add(c);
                    Debug.Log("Range: spot " + c.ToString("0", Inv) + " (clear " + r + " m, nobody within " + alone + " m)");
                }
            return used;
        }

        static bool Open(MapData map, float x, float z, float r)
        {
            var size = map.SizeMeters;
            for (float zz = z - r; zz <= z + r; zz += 2f)
                for (float xx = x - r; xx <= x + r; xx += 2f)
                {
                    if (xx < 3f || zz < 3f || xx > size.x - 3f || zz > size.y - 3f) return false;
                    var layer = map.LayerAt(new float3(xx, 0f, zz));
                    if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked | NavLayer.Wire | NavLayer.Bunker)) != 0) return false;
                }
            for (int i = 0; i < map.Props.Length; i++)
            {
                var q = map.Props[i].Pos;
                if (math.abs(q.x - x) < r && math.abs(q.z - z) < r) return false;
            }
            if (map.StaticCover.IsCreated)
                for (int i = 0; i < map.StaticCover.Length; i++)
                {
                    var cv = map.StaticCover[i];
                    if (cv.OwnerSlot >= 0) continue;
                    if (math.abs(cv.Center.x - x) < r + 6f && math.abs(cv.Center.z - z) < r + 6f) return false;
                }
            return true;
        }

        void Finish(int code)
        {
            if (finished) return;
            finished = true; filming = false; ticking = false;
            CelRange.Clear();
            Time.captureFramerate = 0;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            TW.Presentation.Terrain.Storm.Hold = false;
            CaptureRig.Rig.YawPin = float.NaN;
            clockRun = false; ReleaseFlare();
            if (poseCustom && cam != null) { cam.ResetProjectionMatrix(); poseCustom = false; }
            if (tex != null) Destroy(tex);
            if (big != null) Destroy(big);
            if (!quit) return;
            EditorApplication.ExitPlaymode();
            EditorApplication.delayCall += () => EditorApplication.Exit(code);
        }
    }
}
