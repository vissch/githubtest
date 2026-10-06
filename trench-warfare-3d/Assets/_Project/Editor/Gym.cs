// Phase: tooling (the gym, 2026-09-28) — TW > Gym, and the gym run. The owner: "Gym somewhere all the results in the merged
// build are playable and testable per animation and special ability too." The window plays one GymCatalogue entry at a
// time in Play (GreyboxCorridor) on the battle's own drawing, with a button per zoom band. Gym.Run plays the whole
// catalogue (or a filtered part) unattended: per entry it clears the stage, stages the entry (Perf/GymDirector), waits
// for it to happen, photographs it at each band through CaptureRig (so every still is posed and measured the one right
// way), tiles the bands into one JPG, and writes a JSON sidecar: what was expected, which events came, log errors, the
// capture numbers, and for a pinned man the clip he is drawn in. summary.json lists every entry and its flags; the bug
// catcher reads it (tw-bug-catcher). A flag the kept older run raised on the same entry is "recurring": summary.json lists
// it and the run appends it to the board's lessons.md (../tw3d-board beside the repo, or TW_BOARD), so a lesson is written
// without anyone choosing to. Tools/abtest.py gym diffs two runs (each band's changed pixels, every sidecar number, blind
// pairs); the run is paced in game time (CaptureFps), so the same code draws the same run.
// Runs are written outside every checkout (an untracked file changes the tree land.py checks):
// %LOCALAPPDATA%\TrenchWarfare\gym\<yyyyMMdd-HHmm>-<sha>\, or TW_GYM. A run keeps itself and the newest older run and
// deletes older gym runs (only folders holding gym-run.txt: nothing else is ever touched); it stops when it passes
// 1 GB or the disk has under 10 GB free. Raw PNGs are kept only for flagged entries.
//   Tools/tw eval 'return TW.Editor.Gym.Run("tabs=clips filter=Fire max=20");'
//   Unity.exe -batchmode -projectPath <project> -executeMethod TW.Editor.Gym.CommandLine -twgym "tabs=abilities" -logFile <log>
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using UnityEditor;
using UnityEditor.SceneManagement;
using UnityEngine;
using TW.Perf;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Editor
{
    public static class Gym
    {
        public const string EnvVar = "TW_GYM";
        const string Request = "TW.Gym.Request";
        // What Run leaves for the Play it starts: this mark, then the options. SessionState.GetString hands back "" for a
        // key nobody set, whatever default is passed, and "" is also a run of the whole catalogue: without the mark every
        // ordinary Play in the editor started one (seen 2026-10-05).
        const string Armed = "gym:";
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        static readonly CultureInfo Inv = CultureInfo.InvariantCulture;
        public static string LastRun = "";

        /// <summary>Play the catalogue unattended. Options: tabs=scenes,clips,units,abilities,deaths,events (all when absent),
        /// filter=&lt;part of a name, or parts split by |&gt;, film=&lt;seconds: a unit fights an enemy line, filmed to mp4&gt;, max=&lt;entries&gt;, bands=all|close (close: T3, T2, T1; all: the six zooms, never a strip), out=&lt;run folder&gt;,
        /// light=day (a clear noon instead of night and rain, so a figure can be judged), minutes=&lt;wall-clock limit, default 45&gt;, quit=1 (exit the editor when done; CommandLine adds it).</summary>
        public static string Run(string options = "")
        {
            if (EditorApplication.isPlaying) { StartRunner(options); return "gym running in this Play session: " + options; }
            if (EditorSceneManager.GetActiveScene().path != Scene)
            {
                if (!EditorSceneManager.SaveCurrentModifiedScenesIfUserWantsTo()) return "the open scene has unsaved changes: save or discard them first";
                EditorSceneManager.OpenScene(Scene);
            }
            SessionState.SetString(Request, Armed + (options ?? ""));
            EditorApplication.EnterPlaymode();
            return "entering Play for the gym: " + options;
        }

        /// <summary>-executeMethod entry: options from -twgym "&lt;options&gt;", and the editor exits when the run ends:
        /// 0 no flags, 2 flagged entries, 1 it could not run or a guard stopped it (1 GB, 10 GB free, the wall-clock limit).</summary>
        public static void CommandLine()
        {
            var args = System.Environment.GetCommandLineArgs();
            string opts = "";
            for (int i = 0; i < args.Length - 1; i++) if (args[i] == "-twgym") opts = args[i + 1];
            Run(opts + " quit=1");
        }

        [InitializeOnLoadMethod]
        static void Hook()
        {
            EditorApplication.playModeStateChanged -= OnPlayMode;
            EditorApplication.playModeStateChanged += OnPlayMode;
        }

        static void OnPlayMode(PlayModeStateChange change)
        {
            if (change != PlayModeStateChange.EnteredPlayMode) return;
            string req = SessionState.GetString(Request, "");
            if (!req.StartsWith(Armed, System.StringComparison.Ordinal)) return;   // a Play nobody asked the gym for
            SessionState.EraseString(Request);   // a crash must not leave the editor looping
            StartRunner(req.Substring(Armed.Length));
        }

        static void StartRunner(string options)
        {
            var host = Object.FindFirstObjectByType<SimHost>();
            if (host == null) { Debug.LogError("Gym: no SimHost in the open scene"); if (Opt(options, "quit") == "1") EditorApplication.Exit(1); return; }
            var run = host.gameObject.GetComponent<GymRun>();   // never `??` on a component: a missing one is Unity's fake null
            if (run == null) run = host.gameObject.AddComponent<GymRun>();
            run.Options = options ?? "";
        }

        /// <summary>Join a folder of f_0000.png frames (15 a second) into an mp4 with ffmpeg (TW_FFMPEG, else on PATH); the
        /// frames are deleted when it worked and kept when it did not.</summary>
        internal static string Encode(string frames, string mp4)
        {
            string exe = System.Environment.GetEnvironmentVariable("TW_FFMPEG");
            if (string.IsNullOrEmpty(exe)) exe = "ffmpeg";
            try
            {
                var psi = new System.Diagnostics.ProcessStartInfo(exe, "-y -loglevel error -framerate 15 -i f_%04d.png -c:v libx264 -pix_fmt yuv420p -crf 23 \"" + Path.GetFullPath(mp4) + "\"")
                { WorkingDirectory = frames, UseShellExecute = false, CreateNoWindow = true, RedirectStandardError = true };
                using (var p = System.Diagnostics.Process.Start(psi))
                {
                    string err = p.StandardError.ReadToEnd();
                    if (!p.WaitForExit(180000)) { p.Kill(); return "ffmpeg timed out; frames kept in " + frames; }
                    if (p.ExitCode != 0 || !File.Exists(mp4)) return "ffmpeg failed (" + p.ExitCode + ": " + err.Trim() + "); frames kept in " + frames;
                }
                Directory.Delete(frames, true);
                return "wrote " + mp4 + " (" + new FileInfo(mp4).Length / 1024 + " KB)";
            }
            catch (System.Exception ex) { return "no ffmpeg (" + ex.Message + "); frames kept in " + frames; }
        }

        internal static string Opt(string options, string key)
        {
            var m = Regex.Match(" " + (options ?? "") + " ", @"\s" + key + @"=(\S+)");
            return m.Success ? m.Groups[1].Value : null;
        }

        /// <summary>The folder gym runs go in: TW_GYM, else %LOCALAPPDATA%\TrenchWarfare\gym.</summary>
        public static string Root()
        {
            string env = System.Environment.GetEnvironmentVariable(EnvVar);
            if (!string.IsNullOrEmpty(env)) return env;
            return Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.LocalApplicationData), "TrenchWarfare", "gym");
        }

        internal static string ShortSha()
        {
            try
            {
                var p = new System.Diagnostics.Process { StartInfo = new System.Diagnostics.ProcessStartInfo("git", "rev-parse --short HEAD") { RedirectStandardOutput = true, UseShellExecute = false, CreateNoWindow = true, WorkingDirectory = Directory.GetCurrentDirectory() } };
                p.Start(); string s = p.StandardOutput.ReadToEnd().Trim(); p.WaitForExit(3000);
                return string.IsNullOrEmpty(s) ? "nosha" : s;
            }
            catch { return "nosha"; }
        }

        /// <summary>Keep `keep` and the newest other gym run; delete the other folders that hold gym-run.txt.</summary>
        internal static void Prune(string root, string keep)
        {
            if (!Directory.Exists(root)) return;
            var runs = new List<DirectoryInfo>();
            foreach (var d in new DirectoryInfo(root).GetDirectories())
                if (File.Exists(Path.Combine(d.FullName, "gym-run.txt")) && !string.Equals(d.FullName.TrimEnd('\\', '/'), keep.TrimEnd('\\', '/'), System.StringComparison.OrdinalIgnoreCase)) runs.Add(d);
            runs.Sort((a, b) => b.CreationTimeUtc.CompareTo(a.CreationTimeUtc));
            for (int i = 1; i < runs.Count; i++)
            {
                try { runs[i].Delete(true); Debug.Log("Gym: pruned old run " + runs[i].Name); }
                catch (System.Exception e) { Debug.LogWarning("Gym: could not prune " + runs[i].FullName + ": " + e.Message); }
            }
        }

        internal static long Size(string dir)
        {
            long n = 0;
            foreach (var f in new DirectoryInfo(dir).GetFiles("*", SearchOption.AllDirectories)) n += f.Length;
            return n;
        }

        /// <summary>Tile the band stills left to right into one JPG, 400 px a cell.</summary>
        internal static void Sheet(List<string> pngs, string outJpg, int cellW = 400)
        {
            var tex = new List<Texture2D>();
            foreach (var p in pngs) { if (!File.Exists(p)) continue; var t = new Texture2D(2, 2, TextureFormat.RGB24, false); if (t.LoadImage(File.ReadAllBytes(p))) tex.Add(t); else Object.DestroyImmediate(t); }
            if (tex.Count == 0) return;
            int cellH = Mathf.Max(1, Mathf.RoundToInt(cellW * tex[0].height / (float)tex[0].width));
            var sheet = new Texture2D(cellW * tex.Count, cellH, TextureFormat.RGB24, false);
            for (int i = 0; i < tex.Count; i++)
            {
                var px = new Color[cellW * cellH];
                for (int y = 0; y < cellH; y++) for (int x = 0; x < cellW; x++) px[y * cellW + x] = tex[i].GetPixelBilinear((x + .5f) / cellW, (y + .5f) / cellH);
                sheet.SetPixels(i * cellW, 0, cellW, cellH, px);
                Object.DestroyImmediate(tex[i]);
            }
            sheet.Apply();
            Directory.CreateDirectory(Path.GetDirectoryName(outJpg));
            File.WriteAllBytes(outJpg, sheet.EncodeToJPG(80));
            Object.DestroyImmediate(sheet);
        }

        internal static float JsonNumber(string json, string key)
        {
            var m = Regex.Match(json ?? "", "\"" + key + "\"\\s*:\\s*(-?[0-9.eE+-]+)");
            return m.Success && float.TryParse(m.Groups[1].Value, NumberStyles.Float, Inv, out float v) ? v : float.NaN;
        }

        internal static string Esc(string s) => (s ?? "").Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", " ").Replace("\r", " ");
    }

    /// <summary>The gym run itself: a coroutine on the SimHost's object, in Play.</summary>
    public sealed class GymRun : MonoBehaviour
    {
        public string Options = "";
        static readonly CultureInfo Inv = CultureInfo.InvariantCulture;
        const long MaxRunBytes = 1L << 30, MinFreeBytes = 10L << 30;
        /// <summary>Frames a game second while the gym runs (Time.captureFramerate): one sim tick a frame at 20 Hz.</summary>
        public const int CaptureFps = 20;
        float deadline = float.MaxValue; bool finished, quitWhenDone;
        TW.Presentation.Terrain.Storm[] storms; float[] stormFreeze;   // the storms' own freeze, put back by Finish

        /// <summary>The watchdog: whatever happens inside the run (an exception that kills the coroutine, a wait that
        /// never ends), a batch editor never outlives the wall-clock limit holding the checkout and the GPU.</summary>
        void Update()
        {
            if (!finished && Time.realtimeSinceStartup > deadline)
            {
                Debug.LogError("Gym: the run passed its wall-clock limit; stopping");
                Finish(quitWhenDone, 1);
            }
        }

        IEnumerator Start()
        {
            bool quit = quitWhenDone = Gym.Opt(Options, "quit") == "1";
            float minutes = float.TryParse(Gym.Opt(Options, "minutes"), NumberStyles.Float, Inv, out float mm) ? mm : 45f;
            deadline = Time.realtimeSinceStartup + minutes * 60f;
            // Paced in game time, not the wall clock (2026-10-01): with a real-time step the sim ran as many ticks a frame
            // as the machine allowed and every wait was wall seconds, so each entry began, and was shot, at a different
            // tick on every run (the same code twice: 26-39 % of a machine's close band repainted, a barrage's deaths 1
            // then 3). A fixed frame step makes the sim, the drawing's own clocks and every wait below the same each run;
            // the wall clock is left only to the watchdog and the capture timeouts. The presentation's own random numbers
            // are seeded too, and the terrain's millisecond budgets are off, as PerfBench's held clock has them
            // (2026-10-01): otherwise how much of a crater is painted and dug by a given frame is the machine's.
            Time.captureFramerate = CaptureFps;
            UnityEngine.Random.InitState(1917);
            TW.Presentation.Terrain.GreyboxTerrainView.Unmetered = true;
            // and lightning does not freeze the world: a strike's freeze frame stops game time (Time.timeScale 0) for a
            // second, and strikes are timed from UnityEngine.Random, which the drawing draws from in its own order, so a
            // freeze in one run and not the next moved every later entry's ticks (2026-10-01: 6-18 ticks from the 14th
            // entry on, and the units' events with them). The strikes still flash; the run puts the freeze back.
            storms = Object.FindObjectsByType<TW.Presentation.Terrain.Storm>(FindObjectsSortMode.None);
            stormFreeze = new float[storms.Length];
            for (int k = 0; k < storms.Length; k++) { stormFreeze[k] = storms[k].FreezeSeconds; storms[k].FreezeSeconds = 0f; }
            var host = GetComponent<SimHost>();
            float until = Time.realtimeSinceStartup + 30f;
            while ((host.Local == null || host.Local.World.Tick < 60) && Time.realtimeSinceStartup < until) yield return null;
            if (host.Local == null) { Debug.LogError("Gym: the match never started"); Finish(quit, 1); yield break; }
            // the night's clocks counted from its Start, in frames the scene loaded at real speed: from here on they
            // count the gym's own frames, so a star shell goes up at the same moment in every run
            var night = FindFirstObjectByType<TW.Presentation.Terrain.NightLights>();
            if (night != null) night.Rewind();

            string sha = Gym.ShortSha();
            string dir = Gym.Opt(Options, "out") ?? Path.Combine(Gym.Root(), System.DateTime.Now.ToString("yyyyMMdd-HHmm", Inv) + "-" + sha);
            Directory.CreateDirectory(dir);
            File.WriteAllText(Path.Combine(dir, "gym-run.txt"), "gym run " + sha + " " + System.DateTime.Now.ToString("s", Inv) + "\noptions: " + Options + "\n");
            Gym.Prune(Path.GetDirectoryName(dir.TrimEnd('\\', '/')), dir);
            Gym.LastRun = dir;
            string raw = Path.Combine(dir, "raw"); Directory.CreateDirectory(raw);

            var director = GymDirector.Attach(host);
            if (!director.Quiet()) Debug.LogWarning("Gym: could not stop the ambient shells (worlds unaligned)");
            // light=day: the same ground under a clear noon. The default is untouched - the night field is what the
            // game is - but a figure, a pose and a missing effect cannot be judged in rain at night, which is what
            // the first filming produced 350 sheets of (unit-look/baseline).
            if (Gym.Opt(Options, "light") == "day")
            {
                var sky0 = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
                if (sky0 == null) Debug.LogWarning("Gym: light=day, but there is no Atmosphere in the scene");
                else { sky0.Relight(TW.Presentation.Terrain.BiomeProfile.ClearDay()); sky0.Rain = 0f; sky0.Squalls = 0f; yield return null; }
            }
            // The noise floor: how much of an IDLE close frame repaints on its own (rain, flicker, a lamp). Measured
            // here, in this run's own weather, because the flag below ("nothing drawn") is worth nothing except as a
            // comparison against it - a guessed constant is how the first filming flagged 0 of 350.
            float floor = 0f;
            for (int c = 0; c < 2; c++)
            {
                if (c == 0) { Debug.Log("Gym: " + director.Bare()); yield return new WaitForSeconds(1f); }
                string ca = Path.Combine(raw, "calib" + c + "_a.png"), cb = Path.Combine(raw, "calib" + c + "_b.png");
                CaptureRig.Shot(ca, director.Stage.x, director.Stage.y, GymStrip.Zoom, 30f, 25f, 800, 450);
                float cu = Time.realtimeSinceStartup + 30f;
                while (CaptureRig.Pending() != "0" && Time.realtimeSinceStartup < cu) yield return null;
                yield return new WaitForSeconds(0.5f);
                CaptureRig.Shot(cb, director.Stage.x, director.Stage.y, GymStrip.Zoom, 30f, 25f, 800, 450);
                cu = Time.realtimeSinceStartup + 30f;
                while (CaptureRig.Pending() != "0" && Time.realtimeSinceStartup < cu) yield return null;
                yield return null;   // the rig writes the last still and its sidecar the frame after the queue empties; deleting before that left calib*_b.png in every run
                float cf = Gym.JsonNumber(CaptureRig.Diff(ca, cb), "changed_frac");
                if (!float.IsNaN(cf) && cf > floor) floor = cf;
                TryDelete(ca); TryDelete(cb); TryDelete(Path.ChangeExtension(ca, ".json")); TryDelete(Path.ChangeExtension(cb, ".json"));
            }
            float threshold = GymStrip.Threshold(floor);
            // one floor per distinct strip zoom: a 6 m frame repaints more of itself on its own than a 16 m one
            var floors = new Dictionary<float, float> { { GymStrip.Zoom, floor } };
            // A floor PER ZOOM, measured up front on the idle bare stage (look-08). The strip's zoom comes from how
            // tall the subject is, so a run uses a handful of them, and a 6 m frame repaints 0.2466 of itself on its
            // own where a 16 m one repaints a hundredth. Judging a close entry against the 16 m floor - or against
            // three times its own - is what flagged every close entry in round 3. Measured here rather than lazily
            // beside the staged subject, because a staged stage is not idle: he breathes, the machine idles.
            foreach (float z in new[] { GymStrip.ZoomFor(GymStrip.SubjectHeight(false, false), 25f),
                                        GymStrip.ZoomFor(GymStrip.SubjectHeight(true, false), 25f),
                                        GymStrip.ZoomFor(GymStrip.SubjectHeight(false, true), 25f),
                                        GymStrip.Zoom, GymStrip.FlyerZoom })
            {
                if (floors.ContainsKey(z)) continue;
                float got = float.NaN;
                yield return StartCoroutine(MeasureFloor(raw, director.Stage, z, float.NaN, v => got = v));
                floors[z] = float.IsNaN(got) ? floor : got;
                Debug.Log("Gym: noise floor at zoom " + z.ToString("0.0", Inv) + " is " + floors[z].ToString("0.0000", Inv)
                          + ", threshold " + GymStrip.Threshold(floors[z]).ToString("0.0000", Inv));
            }
            // THE CONTROLS (look-08): a strip of the idle stage, which by construction draws nothing new. Shot at the
            // closest and the widest zoom the run uses and judged exactly as an entry is, against its own zoom's
            // floor. A control that comes out UNFLAGGED says the rule can no longer fail, so the run's own
            // "nothing drawn" flags are worth nothing - it goes into summary.json either way, for FLAGS.md to read.
            var controls = new StringBuilder();
            foreach (float z in new[] { GymStrip.ZoomFor(GymStrip.SubjectHeight(false, false), 25f), GymStrip.FlyerZoom })
            {
                var ch = new List<float>();
                string c0 = Path.Combine(raw, "ctrl_" + z.ToString("0", Inv) + "_0.png");
                CaptureRig.Shot(c0, director.Stage.x, director.Stage.y, z, 30f, 25f, 800, 450);
                float cu2 = Time.realtimeSinceStartup + 30f;
                while (CaptureRig.Pending() != "0" && Time.realtimeSinceStartup < cu2) yield return null;
                for (int i = 1; i < GymStrip.Frames; i++)
                {
                    yield return new WaitForSeconds(0.5f);
                    string ci = Path.Combine(raw, "ctrl_" + z.ToString("0", Inv) + "_" + i + ".png");
                    CaptureRig.Shot(ci, director.Stage.x, director.Stage.y, z, 30f, 25f, 800, 450);
                    cu2 = Time.realtimeSinceStartup + 30f;
                    while (CaptureRig.Pending() != "0" && Time.realtimeSinceStartup < cu2) yield return null;
                    yield return null;
                    ch.Add(Gym.JsonNumber(CaptureRig.Diff(c0, ci), "changed_frac"));
                    TryDelete(ci); TryDelete(Path.ChangeExtension(ci, ".json"));
                }
                TryDelete(c0); TryDelete(Path.ChangeExtension(c0, ".json"));
                float cfl = floors.TryGetValue(z, out float cf2) ? cf2 : floor;
                string verdict = GymStrip.NothingDrawn(ch, cfl);
                float cbest = 0f; foreach (var v in ch) if (!float.IsNaN(v) && v > cbest) cbest = v;
                controls.Append(controls.Length == 0 ? "" : ", ").Append("{\"zoom\": ").Append(z.ToString("0.0", Inv))
                        .Append(", \"floor\": ").Append(cfl.ToString("0.0000", Inv))
                        .Append(", \"threshold\": ").Append(GymStrip.Threshold(cfl).ToString("0.0000", Inv))
                        .Append(", \"best\": ").Append(cbest.ToString("0.0000", Inv))
                        .Append(", \"flagged\": ").Append(verdict != null ? "true" : "false")
                        .Append(", \"flag\": ").Append(verdict == null ? "null" : "\"" + Gym.Esc(verdict) + "\"").Append("}");
                Debug.Log("Gym: control at zoom " + z.ToString("0.0", Inv) + ": " + (verdict ?? "NOT FLAGGED - the rule cannot fail here"));
            }
            // THE OCCLUSION REFERENCE (look-08, fault 2): how much of a close frame one rifleman owns when nothing
            // stands in front of him. Measured here, on the bare stage, in the same run and at the same zoom and
            // share a strip uses, so every entry's share is measured against this run's own weather and camera.
            float visRef = float.NaN;
            yield return StartCoroutine(MeasureSubjectRef(raw, host, director, v => visRef = v));
            Debug.Log("Gym: a lone rifleman owns " + (float.IsNaN(visRef) ? "(not measured)" : visRef.ToString("0.0000", Inv))
                      + " of his own close frame; a subject under " + (GymStrip.MinVisible * 100f).ToString("0", Inv) + "% of that is flagged hidden");
            File.AppendAllText(Path.Combine(dir, "gym-run.txt"), "noise floor: " + floor.ToString("0.0000", Inv) + " of a close frame; nothing-drawn threshold " + threshold.ToString("0.0000", Inv) + "\n");
            Debug.Log("Gym: noise floor " + floor.ToString("0.0000", Inv) + ", threshold " + threshold.ToString("0.0000", Inv));
            var entries = Select(GymCatalogue.All(host.Local.World));
            Debug.Log($"Gym: {entries.Count} entries -> {dir}");

            var summary = new StringBuilder();
            var summaryFlags = new HashSet<string>();
            summary.Append("{\n  \"sha\": \"").Append(sha).Append("\", \"options\": \"").Append(Gym.Esc(Options))
                   .Append("\", \"subject_visible_ref\": ").Append(float.IsNaN(visRef) ? "null" : visRef.ToString("0.0000", Inv))
                   .Append(", \"min_visible\": ").Append(GymStrip.MinVisible.ToString("0.00", Inv))
                   .Append(", \"noise_floor\": ").Append(floor.ToString("0.0000", Inv)).Append(", \"threshold\": ").Append(threshold.ToString("0.0000", Inv))
                   .Append(", \"entries\": [\n");
            int flagged = 0, done = 0; string stopped = null;
            float t0 = Time.realtimeSinceStartup;
            foreach (var e in entries)
            {
                var drive = new DriveInfo(Path.GetPathRoot(Path.GetFullPath(dir)));
                if (drive.AvailableFreeSpace < MinFreeBytes) { stopped = "under 10 GB free on " + drive.Name; break; }
                if (Gym.Size(dir) > MaxRunBytes) { stopped = "the run passed 1 GB"; break; }

                // bare=1: an EMPTY stage, and no smouldering hull the renderer kept after the sim despawned it
                // ALWAYS for a strip entry (look-08): round 3 photographed Units/Skimmer with the previous entry's
                // wreck in frame and seven unit entries with a hull or a walker leg between the camera and the man,
                // because Clear() leaves standing what the renderer still draws. bare=1 forces it for every tab.
                string bared = null;
                if (Gym.Opt(Options, "bare") == "1" || GymStrip.Expects(e)) { bared = director.Bare(); Debug.Log("Gym: " + bared); }
                else director.Clear();
                yield return new WaitForSeconds(1.5f);
                // each entry draws from its own seed, so its pictures do not hang on what the entries before it drew
                UnityEngine.Random.InitState(Seed(e.Tab + "/" + e.Name));
                director.NextStage();
                var r = director.Begin(e);
                if (bared != null) r.Log.Add(bared);
                int pinned = -1, slot = -1;
                float wait;
                var shots = new List<string>(); var jsons = new List<string>();
                string truth = null; bool machineChanged = false; float stripFloor = floor; float subjVis = float.NaN;
                // The TIME STRIP: one close band at five moments across the entry's life, instead of six zooms of one
                // moment. A death, an ability, an event or a unit's fire IS a moment in time, and the first filming
                // proved a single still cannot tell "nothing was drawn" from "the still missed it" - its three far
                // bands showed the whole small stage as a dot, and it flagged 0 of 350. Clips (a pose) and Scenes (a
                // whole stage you want the overviews of) keep their bands; bands=all forces the old path everywhere.
                bool strip = GymStrip.Expects(e) && Gym.Opt(Options, "bands") != "all";
                string stem = Safe(e.Tab + "_" + e.Name);
                // every strip frame shares ONE camera pose, worked out after the subject is placed and before anything
                // is triggered: a frame shot from somewhere else makes its diff against the 'before' frame meaningless.
                Vector2 sfocus = e.Tab == GymTab.Abilities ? director.Stage + new Vector2(0f, 30f) : director.Stage;
                float szoom = GymStrip.Zoom, saimY = float.NaN, sheight = float.NaN;
                bool previewOnly = false;
                // a still is the pose, not the weather: no camera shake (a barrage moved the camera 1-10 m off its
                // pose and voided the stills) and no lightning (a flash blew out 5 % of a trench still); both come
                // back after the capture. For a strip this has to hold across every frame, the 'before' one included.
                float shake = TW.Presentation.Tactical.CameraShake.Strength;
                var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
                bool lightning = sky != null && sky.Lightning;
                bool held = TW.Presentation.Terrain.Storm.Hold;
                // hud=1 puts the HUD, the selection marker and an ability's aiming disc back in the picture. They are
                // off by default (look-04): an aiming disc repaints a tenth of the frame, so the strip's measurement
                // could not tell the overlay from the effect it is there to isolate.
                bool hudOpt = Gym.Opt(Options, "hud") == "1";
                bool overlaysWere = TW.Presentation.Tactical.CombatFx.ShowOverlays;
                bool ringsWere = TW.Presentation.Tactical.TankRenderer.ShowRings;
                // STAGING FIRST, trigger second (look-04): the victim, the machine or the unit has to be standing in
                // the 'before' frame, or the measured change is "a man appeared" and not "an effect was drawn".
                if (e.Tab == GymTab.Scenes)
                {
                    yield return StartCoroutine(director.Scene((GymScene)e.Id));
                    wait = GymDirector.SceneSettle((GymScene)e.Id);
                }
                else
                {
                    try { wait = Place(director, e, ref pinned, ref slot, out previewOnly); }
                    catch (System.Exception ex) { Debug.LogException(ex); wait = -1f; r.Log.Add("staging threw: " + ex.Message); }
                }
                if (wait < 0f)
                {
                    SafeWrite(dir, summary, e, r, null, new List<string> { "not staged" }, ref flagged, ref done, null, null); continue;
                }
                // film=<seconds>: a unit fights an enemy rifle line 70 m off its nose (inside every machine's reach), filmed below
                float film = e.Tab == GymTab.Units && slot >= 0 && float.TryParse(Gym.Opt(Options, "film"), NumberStyles.Float, Inv, out float fs) ? fs : 0f;
                if (film > 0f) { r.Log.Add("film: " + RiderLab.Stop(slot) + ", " + RiderLab.Enemies(slot, 12, 70f)); }   // held, so the take keeps it (a Salvo drove into a trench)
                if (strip)
                {
                    yield return new WaitForSeconds(1f);   // the staging settles: he stands still, the machine is at rest
                    // THE SUBJECT: the picture is of him, so the zoom comes from how tall he is, not from a band.
                    Vector3 foot = new Vector3(sfocus.x, 0f, sfocus.y);
                    bool sVeh = false, sWalk = false;
                    if (slot >= 0 && host.Local != null && slot < host.Local.World.HighWater)
                    {
                        var w0 = host.Local.World;
                        byte ch = w0.ChassisOf(w0.Archetype[slot]);
                        sVeh = TW.Sim.ChassisKind.IsArmoured(ch); sWalk = TW.Sim.ChassisKind.IsWalker(ch);
                        if (host.Presenter != null) foot = (Vector3)host.Presenter.Drawn(slot);
                    }
                    sheight = GymStrip.SubjectHeight(sVeh, sWalk);
                    szoom = GymStrip.ZoomFor(sheight, 25f);
                    saimY = foot.y + sheight * 0.5f;
                    sfocus = new Vector2(foot.x, foot.z);
                    if (GymStrip.Flyer(e)) { szoom = GymStrip.FlyerZoom; saimY = GymStrip.FlyerAimY; sheight = 25f; }   // PlaneLow is 25 m up
                    CaptureRig.Subject = foot; CaptureRig.SubjectHeight = sheight;
                    r.Log.Add("subject: " + (sWalk ? "walker" : sVeh ? "machine" : "man") + " " + sheight.ToString("0.0", Inv) + " m at ("
                              + foot.x.ToString("0.0", Inv) + ", " + foot.y.ToString("0.0", Inv) + ", " + foot.z.ToString("0.0", Inv)
                              + "), zoom " + szoom.ToString("0.0", Inv) + (slot >= 0 ? "; " + director.Truth(slot) : ""));
                    TW.Presentation.Tactical.CameraShake.Strength = 0f; TW.Presentation.Tactical.CameraShake.Reset();
                    if (sky != null) sky.Lightning = false;
                    TW.Presentation.Terrain.Storm.Hold = true;   // the bolt itself (Atmosphere only lights it)
                    Overlays(GymStrip.ShowsOverlays(e, hudOpt), GymStrip.ShowsSelection(e, hudOpt));
                    // The noise floor at THIS zoom, measured on the staged-but-not-triggered stage: a tighter frame
                    // repaints a larger share of itself on its own, so one floor measured at 16 m is the wrong
                    // yardstick for a 6 m one. Once per distinct zoom, kept for the rest of the run.
                    float zfloor;
                    if (!floors.TryGetValue(szoom, out zfloor))
                    {
                        // a fallback only: every zoom the run can use was measured on the idle stage before the
                        // catalogue. Here the subject is standing in frame and breathing, so this floor reads high.
                        float got2 = float.NaN;
                        yield return StartCoroutine(MeasureFloor(raw, sfocus, szoom, saimY, v => got2 = v));
                        zfloor = float.IsNaN(got2) ? floor : got2;
                        floors[szoom] = zfloor;
                        Debug.Log("Gym: noise floor at zoom " + szoom.ToString("0.0", Inv) + " is " + zfloor.ToString("0.0000", Inv) + " (measured late, with the subject staged)");
                    }
                    stripFloor = zfloor;
                    // the 'before' frame: the subject standing on the stage, a tick before the trigger. Everything the
                    // strip measures is measured against it, so the measurement isolates the EFFECT.
                    string before = Path.Combine(raw, stem + "_" + GymStrip.Names[0] + ".png");
                    CaptureRig.Shot(before, sfocus.x, sfocus.y, szoom, 30f, 25f, 800, 450, saimY);
                    float bu = Time.realtimeSinceStartup + 30f;
                    while (CaptureRig.Pending() != "0" && Time.realtimeSinceStartup < bu) yield return null;
                    shots.Add(before); jsons.Add(Path.ChangeExtension(before, ".json"));
                    // HOW MUCH OF HIM THE CAMERA SEES (look-08, fault 2). The same pose, shot again with the man in
                    // this one slot not drawn: the share of the frame that changes between the two is his own pixels
                    // AS SEEN - a pixel a hull owns does not change when he stops being drawn. Taken here, before
                    // the trigger, so no effect of the entry is counted as part of him.
                    if (slot >= 0 && !sVeh && !sWalk)
                    {
                        var vatHide = Object.FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
                        if (vatHide == null) r.Log.Add("visible: not measured - no VATRenderer in the scene");
                        else
                        {
                            string hid = Path.Combine(raw, stem + "_vis.png");
                            vatHide.Hide(slot, true);
                            yield return null; yield return null;   // the mask is read in VATRenderer's own LateUpdate
                            CaptureRig.Shot(hid, sfocus.x, sfocus.y, szoom, 30f, 25f, 800, 450, saimY);
                            float hu = Time.realtimeSinceStartup + 30f;
                            while (CaptureRig.Pending() != "0" && Time.realtimeSinceStartup < hu) yield return null;
                            yield return null;
                            vatHide.Hide(slot, false);
                            float cfWith = Gym.JsonNumber(CaptureRig.Diff(before, hid), "changed_frac");
                            subjVis = GymStrip.VisibleShare(cfWith, visRef, stripFloor);
                            r.Log.Add("visible: his own pixels are " + (float.IsNaN(cfWith) ? "(unread)" : cfWith.ToString("0.0000", Inv))
                                      + " of the frame against a lone rifleman's " + (float.IsNaN(visRef) ? "(unmeasured)" : visRef.ToString("0.0000", Inv))
                                      + " over the floor " + stripFloor.ToString("0.0000", Inv) + ", so "
                                      + (float.IsNaN(subjVis) ? "no share" : (subjVis * 100f).ToString("0", Inv) + "% of him"));
                            TryDelete(hid); TryDelete(Path.ChangeExtension(hid, ".json"));
                        }
                    }
                    else if (slot >= 0)
                        r.Log.Add("visible: not measured - VATRenderer.Hide masks a MAN in one sim slot and there is no "
                                  + "per-slot equivalent for a " + (sWalk ? "walker" : "machine") + ", which TankRenderer draws from the world");
                    try { Trigger(director, e, slot, previewOnly); }
                    catch (System.Exception ex) { Debug.LogException(ex); r.Log.Add("trigger threw: " + ex.Message); }
                    var moments = GymStrip.Moments(wait);
                    float begun = Time.time;
                    for (int i = 0; i < moments.Length; i++)
                    {
                        float at2 = begun + moments[i];
                        while (Time.time < at2) yield return null;
                        Overlays(GymStrip.ShowsOverlays(e, hudOpt), GymStrip.ShowsSelection(e, hudOpt));   // re-asserted: the HUD rebuilds itself between frames
                        string png = Path.Combine(raw, stem + "_" + GymStrip.Names[i + 1] + ".png");
                        CaptureRig.Shot(png, sfocus.x, sfocus.y, szoom, 30f, 25f, 800, 450, saimY);
                        float su = Time.realtimeSinceStartup + 30f;
                        while (CaptureRig.Pending() != "0" && Time.realtimeSinceStartup < su) yield return null;
                        shots.Add(png); jsons.Add(Path.ChangeExtension(png, ".json"));
                    }
                    // views=front,side: the SAME entry photographed again from the man's front and side, close enough
                    // that he fills half the frame. Only the first view (Std) is diffed and flagged: the noise floor
                    // is measured per pose, and a frame shot from somewhere else cannot be compared with the 'before'
                    // frame. These passes are evidence for the eye - what he carries, what his silhouette is.
                    var gviews = GymStrip.ParseViews(Gym.Opt(Options, "views"));
                    for (int vi = 1; vi < gviews.Length; vi++)
                    {
                        var gv = gviews[vi];
                        float subjYaw = slot >= 0 && host.Local != null && slot < host.Local.World.HighWater
                                      ? host.Local.World.Yaw[slot] * Mathf.Rad2Deg : 0f;
                        float vyaw = GymStrip.ViewYaw(gv, subjYaw), vpitch = GymStrip.ViewPitch(gv);
                        float vzoom = GymStrip.ZoomFor(sheight, vpitch, GymStrip.ViewShare(gv), GymStrip.CloseZoomFloor);
                        // YawPin 0 makes shot.Yaw a WORLD yaw, and ZoomFloor lets the rig go inside tc.ZoomMin
                        CaptureRig.Rig.YawPin = 0f; CaptureRig.Rig.ZoomFloor = GymStrip.CloseZoomFloor;
                        r.Log.Add("view " + GymStrip.ViewSuffix(gv) + ": yaw " + vyaw.ToString("0", Inv) + " (he faces "
                                  + subjYaw.ToString("0", Inv) + "), pitch " + vpitch.ToString("0", Inv) + ", zoom " + vzoom.ToString("0.0", Inv));
                        for (int i = 0; i < GymStrip.Frames; i++)
                        {
                            string vpng = Path.Combine(raw, stem + "_" + GymStrip.ViewSuffix(gv) + "_" + GymStrip.Names[i] + ".png");
                            CaptureRig.Shot(vpng, sfocus.x, sfocus.y, vzoom, vyaw, vpitch, 800, 450, saimY);
                            float vu = Time.realtimeSinceStartup + 30f;
                            while (CaptureRig.Pending() != "0" && Time.realtimeSinceStartup < vu) yield return null;
                            shots.Add(vpng);   // the sheet, not `jsons`: these frames are not judged
                            yield return new WaitForSeconds(0.3f);
                        }
                        CaptureRig.Rig.YawPin = float.NaN; CaptureRig.Rig.ZoomFloor = float.NaN;
                    }
                    truth = slot >= 0 ? director.Truth(slot) : null;
                    machineChanged = slot >= 0 && director.MachineChanged(slot);
                    yield return null;
                    TW.Presentation.Tactical.CameraShake.Strength = shake;
                    if (sky != null) sky.Lightning = lightning;
                    TW.Presentation.Terrain.Storm.Hold = held;
                    Overlays(overlaysWere, ringsWere);
                    CaptureRig.NoSubject();
                }
                else if (e.Tab != GymTab.Scenes)
                {
                    try { Trigger(director, e, slot, previewOnly); }
                    catch (System.Exception ex) { Debug.LogException(ex); r.Log.Add("trigger threw: " + ex.Message); }
                    yield return new WaitForSeconds(wait);
                }
                else if (r.Watch.Count > 0)
                {
                    // the watched men's drawn facing, a frame at a time, while the scene settles (GymDirector.SampleFacing)
                    float settleUntil = Time.time + wait;
                    while (Time.time < settleUntil) { yield return null; director.SampleFacing(Time.deltaTime); }
                }
                else yield return new WaitForSeconds(wait);

                if (e.Tab == GymTab.Scenes && (GymScene)e.Id == GymScene.AdvanceUnderFire) director.FocusOnWatched();
                bool capture = e.Expect != GymExpect.Covered && e.Expect != GymExpect.Excluded;
                if (capture && !strip)
                {
                    int bands = e.Tab == GymTab.Clips || Gym.Opt(Options, "bands") == "close" ? 3 : GymCatalogue.Bands.Length;
                    TW.Presentation.Tactical.CameraShake.Strength = 0f; TW.Presentation.Tactical.CameraShake.Reset();
                    if (sky != null) sky.Lightning = false;
                    TW.Presentation.Terrain.Storm.Hold = true;
                    for (int b = 0; b < bands; b++)
                    {
                        var band = GymCatalogue.Bands[b];
                        string png = Path.Combine(raw, stem + "_" + band.Name + ".png");
                        CaptureRig.Shot(png, r.Focus.x, r.Focus.z, band.Zoom, 30f, 25f, 800, 450);
                        shots.Add(png); jsons.Add(Path.ChangeExtension(png, ".json"));
                    }
                    float capUntil = Time.realtimeSinceStartup + 30f;
                    while (CaptureRig.Pending() != "0" && Time.realtimeSinceStartup < capUntil) yield return null;
                    yield return null;
                    TW.Presentation.Tactical.CameraShake.Strength = shake;
                    if (sky != null) sky.Lightning = lightning;
                    TW.Presentation.Terrain.Storm.Hold = held;
                }
                if (film > 0f)
                {
                    // two takes, followed: close on the unit, then wide enough to see the line it is fighting; each joined
                    // into an mp4 (ffmpeg: TW_FFMPEG or on PATH) and its frames deleted, or the frames kept when no ffmpeg
                    // framed down the line of fire: from behind the unit towards the enemy's middle, so its fire runs up the
                    // screen; close on the unit, then wide over both (following a fixed world yaw left the enemy off-screen)
                    var cam = Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
                    TankCapture.Follow(-1, 22f, 30f);
                    foreach (var (take, share) in new[] { ("close", 0.4f), ("wide", 0.6f) })
                    {
                        string frames = Path.Combine(dir, "Film", Safe(e.Name) + "_" + take);
                        FrameFight(host, cam, slot, take == "wide");
                        yield return new WaitForSeconds(0.5f);
                        r.Log.Add("film: " + RiderLab.Film(frames, film * share, 15, 960, 540));
                        float filmUntil = Time.realtimeSinceStartup + film * share * 6f + 30f;
                        while (!RiderLab.FilmStatus().StartsWith("done") && Time.realtimeSinceStartup < filmUntil) { FrameFight(host, cam, slot, take == "wide"); yield return null; }
                        r.Log.Add("film: " + RiderLab.FilmStatus() + "; " + Gym.Encode(frames, Path.Combine(dir, "Film", Safe(e.Name) + "_" + take + ".mp4")));
                    }
                    r.Log.Add("film: " + RiderLab.ClearEnemies(slot, 150f));
                }
                director.End();
                // What the strip measures: how much of the close frame each moment differs from the 'before' frame by.
                // The raw PNGs are still on disk here; they are deleted below only when the entry came out unflagged.
                List<float> stripChanged = null;
                if (strip && shots.Count >= 2)
                {
                    stripChanged = new List<float>();
                    // the first GymStrip.Frames shots only: any further ones are the views= passes, shot from a
                    // different pose, so a diff against shots[0] would measure the camera move and not the effect
                    for (int i = 1; i < shots.Count && i < GymStrip.Frames; i++) stripChanged.Add(Gym.JsonNumber(CaptureRig.Diff(shots[0], shots[i]), "changed_frac"));
                }
                List<string> flags;
                try { flags = Judge(host, director, e, r, pinned, slot, jsons, stripChanged, stripFloor, previewOnly, machineChanged, subjVis); }
                catch (System.Exception ex) { Debug.LogException(ex); flags = new List<string> { "judging threw: " + ex.Message }; }
                string sheet = capture ? Path.Combine(dir, e.Tab.ToString(), Safe(e.Name) + ".jpg") : null;
                try { if (capture) Gym.Sheet(shots, sheet, e.Tab == GymTab.Clips ? 640 : 480); }
                catch (System.Exception ex) { Debug.LogException(ex); flags.Add("sheet failed: " + ex.Message); }
                SafeWrite(dir, summary, e, r, jsons, flags, ref flagged, ref done, sheet, pinned >= 0 && host.Animation != null ? host.Animation.State[pinned] : (AnimState?)null, stripChanged, truth, machineChanged, previewOnly, subjVis, visRef);
                foreach (var f in flags) summaryFlags.Add(e + " | " + Regex.Replace(f, @"[0-9.]+", "#"));
                if (flags.Count == 0) foreach (var f in shots) { TryDelete(f); TryDelete(Path.ChangeExtension(f, ".json")); }
            }
            director.Clear();
            director.Unquiet();
            summary.Append("  ],\n  \"done\": ").Append(done).Append(", \"flagged\": ").Append(flagged)
                   .Append(", \"seconds\": ").Append((Time.realtimeSinceStartup - t0).ToString("0", Inv))
                   .Append(", \"stopped\": ").Append(stopped == null ? "null" : "\"" + Gym.Esc(stopped) + "\"").Append("\n}\n");
            summary.Length -= 2;   // reopen the object: drop "}" and its newline
            summary.Append(",\n  \"noise_floors\": {");
            bool f0 = true;
            foreach (var kv in floors) { summary.Append(f0 ? "" : ", ").Append('"').Append(kv.Key.ToString("0.0", Inv)).Append("\": ").Append(kv.Value.ToString("0.0000", Inv)); f0 = false; }
            summary.Append("},\n  \"controls\": [").Append(controls.ToString()).Append("]\n}\n");
            var recurring = Recurring(dir, summaryFlags);
            summary.Length -= 2;   // reopen the object: drop "}\n"
            summary.Append(",\n  \"recurring\": [");
            for (int i = 0; i < recurring.Count; i++) summary.Append(i > 0 ? ", " : "").Append('"').Append(Gym.Esc(recurring[i])).Append('"');
            summary.Append("]\n}\n");
            File.WriteAllText(Path.Combine(dir, "summary.json"), summary.ToString());
            AppendLessons(sha, recurring);
            Debug.Log($"Gym: {done} entries, {flagged} flagged{(stopped != null ? ", stopped: " + stopped : "")} -> {dir}");
            Finish(quit, stopped != null ? 1 : flagged > 0 ? 2 : 0);
        }

        /// <summary>A filter is one part of a name, or several split by '|' (any of them matches).</summary>
        internal static bool Matches(string name, string filter)
        {
            foreach (var part in filter.Split('|'))
                if (part.Length > 0 && name.IndexOf(part, System.StringComparison.OrdinalIgnoreCase) >= 0) return true;
            return false;
        }

        /// <summary>The camera on a fight: behind `slot`, looking at the middle of the living enemies within 150 m; close
        /// (22 m) on the unit, or wide over both.</summary>
        static void FrameFight(SimHost host, TW.Presentation.Tactical.TacticalCamera cam, int slot, bool wide)
        {
            if (cam == null || host == null || host.Local == null || !host.Local.World.IsAlive(slot)) return;
            var w = host.Local.World; var p = w.Position[slot]; Vector2 me = new Vector2(p.x, p.z), sum = Vector2.zero; int n = 0;
            for (int i = 0; i < w.HighWater; i++)
            {
                if (i == slot || !w.IsAlive(i) || w.Team[i] == w.Team[slot]) continue;
                var q = new Vector2(w.Position[i].x, w.Position[i].z);
                if ((q - me).sqrMagnitude < 150f * 150f) { sum += q; n++; }
            }
            Vector2 foe = n > 0 ? sum / n : me + new Vector2(Mathf.Sin(w.Yaw[slot]), Mathf.Cos(w.Yaw[slot])) * 60f;
            Vector2 d = foe - me; float yaw = Mathf.Atan2(d.x, d.y) * Mathf.Rad2Deg - cam.BaseYaw - cam.AutoYaw;   // the view's heading is BaseYaw + AutoYaw + yaw
            if (wide) cam.FrameFrom(me + d * 0.45f, Mathf.Max(35f, d.magnitude * 0.9f), yaw);
            else cam.FrameFrom(me + d.normalized * 6f, 22f, yaw);
        }

        List<GymEntry> Select(List<GymEntry> all)
        {
            string tabs = Gym.Opt(Options, "tabs"), filter = Gym.Opt(Options, "filter");
            int max = int.TryParse(Gym.Opt(Options, "max"), NumberStyles.Integer, Inv, out int m) ? m : int.MaxValue;
            var list = new List<GymEntry>();
            foreach (var e in all)
            {
                if (tabs != null && tabs.ToLowerInvariant().IndexOf(e.Tab.ToString().ToLowerInvariant(), System.StringComparison.Ordinal) < 0) continue;
                if (filter != null && !Matches(e.Name, filter)) continue;
                list.Add(e);
                if (list.Count >= max) break;
            }
            return list;
        }

        /// <summary>Put the entry on the stage; seconds to wait before the photographs, or -1 when it cannot be staged.
        /// Place then Trigger in one call: the inspector's single-entry button and the band (non-strip) path.</summary>
        static float Stage(GymDirector d, GymEntry e, ref int pinned, ref int slot)
        {
            float wait = Place(d, e, ref pinned, ref slot, out bool previewOnly);
            if (wait >= 0f) Trigger(d, e, slot, previewOnly);
            return wait;
        }

        /// <summary>
        /// The first half: everything that has to be STANDING THERE before the strip's 'before' frame is taken - the
        /// victim, the machine, the unit, the rifle line - and nothing that makes the entry's event happen. Split from
        /// Trigger on 2026-10-06 (look-04): round 2's 'before' frame was of an empty field, so a strip's measured
        /// change was "a man appeared" rather than "an effect was drawn", and the subject could not be framed before
        /// it existed. Returns the seconds to wait after the trigger, or -1 when the entry cannot be staged.
        /// `previewOnly` says the trigger will only replay the event into the effects: nothing happens in the sim.
        /// </summary>
        static float Place(GymDirector d, GymEntry e, ref int pinned, ref int slot, out bool previewOnly)
        {
            previewOnly = false;
            var at = d.Stage;
            switch (e.Tab)
            {
                case GymTab.Clips:
                {
                    pinned = d.PlayClip(e.Figure, (Clip)e.Id, at);
                    if (pinned < 0) return -1f;
                    slot = pinned;
                    return Mathf.Clamp(Clips.Table[e.Id].Seconds * 0.6f, 0.4f, 4f);   // photographed mid-clip
                }
                case GymTab.Units:
                    slot = d.Spawn(0, e.Id, at.x, at.y, 30f);
                    d.Hold(slot);   // seen where it was put, not walking out of the close shot to the front trench
                    return slot >= 0 ? 4f : -1f;
                case GymTab.Abilities:
                {
                    var target = at + new Vector2(0f, 30f);
                    d.Current.Focus = new Unity.Mathematics.float3(target.x, 0f, target.y);
                    return 12f;
                }
                case GymTab.Deaths:
                {
                    var kind = (DeathKind)e.Id;
                    slot = d.DeathStage(kind, 0, at);
                    return slot >= 0 ? (kind == DeathKind.Gas ? 40f : kind == DeathKind.Crushed ? 20f : 12f) : -1f;
                }
                case GymTab.Events:
                {
                    if (e.Expect != GymExpect.Preview) return 0f;
                    var t = (SimEventType)e.Id;
                    bool vehicle = VehicleEvent(t);
                    slot = d.Spawn(0, vehicle ? VehicleArchetype.Maw : 0, at.x, at.y, 30f);
                    if (slot < 0) return -1f;
                    d.Hold(slot);
                    previewOnly = !vehicle;
                    // a machine's event is staged FOR REAL (look-04): round 2's VehicleDestroyed.jpg showed the same
                    // whole tank in all five cells because the event was only replayed to the effects. A hull set
                    // alight with almost no hit points left is burnt, knocked out, cooked off and despawned by the
                    // sim's own step, so the wreck, the cook-off and the fire are the game's, not a replay's.
                    return vehicle ? 12f : 3f;
                }
            }
            return -1f;
        }

        /// <summary>Does this event belong to a machine, so the gym can stage it by setting a hull alight?</summary>
        static bool VehicleEvent(SimEventType t) =>
            t.ToString().StartsWith("Vehicle", System.StringComparison.Ordinal) || t == SimEventType.WreckRecorded;

        /// <summary>The second half: what makes the entry's event happen, on the tick the strip measures from.</summary>
        static void Trigger(GymDirector d, GymEntry e, int slot, bool previewOnly)
        {
            var at = d.Stage;
            switch (e.Tab)
            {
                case GymTab.Abilities:
                    d.Ability((OffMapAbilityId)e.Id, at + new Vector2(0f, 30f));
                    break;
                case GymTab.Deaths:
                    d.DeathTrigger((DeathKind)e.Id, slot, at);
                    break;
                case GymTab.Events:
                {
                    if (e.Expect != GymExpect.Preview) break;
                    var t = (SimEventType)e.Id;
                    if (previewOnly) { d.Preview(t, slot, at, new Unity.Mathematics.float3(0f, 0f, 1f), 1f); break; }
                    if (slot < 0 || d.Host == null) break;
                    d.Host.WriteWorlds(m =>
                    {
                        if (m.World != null && slot < m.World.HighWater) m.World.Hp[slot] = 1f;
                        if (m.Modules != null && m.Modules.Fire.IsCreated && slot < m.Modules.Fire.Length) m.Modules.Fire[slot] = 0.9f;
                    });
                    d.Current?.Log.Add("staged for real: the hull is alight (fire 0.9) with 1 hp; the sim burns, knocks out and cooks it off");
                    break;
                }
            }
        }

        /// <summary>The entry's flags: what a person should look at. Empty means it did what the catalogue expects.</summary>
        static List<string> Judge(SimHost host, GymDirector d, GymEntry e, GymDirector.Result r, int pinned, int slot, List<string> jsons,
                                  List<float> stripChanged = null, float floor = 0f, bool previewOnly = false, bool machineChanged = false,
                                  float subjectVisible = float.NaN)
        {
            // Did the staging make the entry's event happen FOR REAL? An entry whose event was only replayed into the
            // effects, whose victim lived or whose ability the sim refused says nothing about what the game draws, so
            // it is flagged "not staged" and NOT judged on what the pictures show (look-04, 2026-10-06: round 2 called
            // VehicleDestroyed proven off five cells of the same undamaged tank).
            string unstaged = GymStrip.Expects(e)
                ? GymStrip.NotStaged(e, previewOnly, r.Victim >= 0, r.VictimDied, r.Count(SimEventType.AbilityFired) > 0, machineChanged)
                : null;
            var flags = new List<string>();
            if (r.Errors > 0) flags.Add(r.Errors + " log errors");
            if (r.Canary && r.Desync) flags.Add("desync");
            switch (e.Tab)
            {
                case GymTab.Clips:
                    if (pinned >= 0 && host.Animation != null && host.Animation.State[pinned].Clip != (Clip)e.Id) flags.Add("pinned man is drawn in " + host.Animation.State[pinned].Clip);
                    break;
                case GymTab.Abilities:
                    if (e.Expect == GymExpect.Rejected && r.Rejects == 0) flags.Add("expected the sim to refuse it; it did not");
                    if (e.Expect != GymExpect.Rejected && r.Count(SimEventType.AbilityFired) == 0) flags.Add(r.Rejects > 0 ? "refused by the sim" : "no AbilityFired");
                    break;
                case GymTab.Deaths:
                    if (!r.VictimDied) flags.Add("he did not die (only his own Death event counts)");
                    break;
                case GymTab.Units:
                    if (!d.Alive(slot)) flags.Add("not alive 4 s after spawning");
                    break;
                case GymTab.Scenes:
                    if ((GymScene)e.Id == GymScene.MachineFlattens)
                    {
                        if (r.Count(SimEventType.PropChanged) + r.Count(SimEventType.WireBreached) == 0) flags.Add("the machine flattened nothing");
                        break;
                    }
                    if ((GymScene)e.Id == GymScene.BarrageOnTrees)
                    {
                        // a standing tree takes 220 hp and a shell falls off from its centre, so twelve may break none and that is
                        // fair; none of them harmed at all is the chain from the blast to the wood broken
                        d.TreeHarm(r, out int harmed, out int broken);
                        r.Log.Add($"trees: {r.Trees.Count} in the stand, {harmed} harmed, {broken} broken");
                        if (r.Trees.Count > 0 && harmed == 0) flags.Add("the barrage did not touch the trees");
                        break;
                    }
                    if (r.Watch.Count == 0) flags.Add("no men staged");
                    else if ((GymScene)e.Id != GymScene.TrenchLine && r.WatchedHits + r.WatchedNearMisses + r.WatchedDeaths + r.WatchedSuppressed == 0)
                        flags.Add("nothing reached the men");
                    break;
            }
            foreach (var j in jsons)
            {
                if (!File.Exists(j)) { flags.Add("no capture " + Path.GetFileName(j)); continue; }
                string s = File.ReadAllText(j);
                float blown = Gym.JsonNumber(s, "blown_frac"), pose = Gym.JsonNumber(s, "pose_error_m");
                if (blown > 0.02f) flags.Add(Path.GetFileNameWithoutExtension(j) + ": blown " + blown.ToString("0.000", Inv));
                if (pose >= 0.5f) flags.Add(Path.GetFileNameWithoutExtension(j) + ": camera off pose " + pose.ToString("0.00", Inv) + " m (invalid still)");
            }
            // The measured flag the first filming could not raise: an entry that promises something on screen and whose
            // whole strip stays inside the noise an idle stage makes on its own drew nothing a player would see.
            if (unstaged != null) flags.Add(unstaged);
            string nothing = unstaged == null && GymStrip.Expects(e) ? GymStrip.NothingDrawn(stripChanged, floor) : null;
            if (nothing != null) flags.Add(nothing);
            // was the picture OF the subject? CaptureRig writes subject_in_frame and subject_height_frac when it was
            // told what the shot is of; a strip whose victim is off frame or a few pixels tall cannot be read at all.
            foreach (var j in jsons)
            {
                if (!File.Exists(j)) continue;
                string sj = File.ReadAllText(j);
                float inFrameS = Gym.JsonNumber(sj, "subject_in_frame"), frac = Gym.JsonNumber(sj, "subject_height_frac");
                if (float.IsNaN(inFrameS)) continue;
                string which = Path.GetFileNameWithoutExtension(j);
                if (inFrameS < 0.5f) { flags.Add(which + ": subject off frame"); continue; }
                if (!float.IsNaN(frac) && frac < 0.15f)
                    flags.Add(which + ": subject fills only " + (frac * 100f).ToString("0", Inv) + "% of the frame's height");
            }
            // ...and was he DRAWN, not merely in frame (look-08, fault 2): subject_in_frame is a viewport test and
            // passed seven round-3 entries whose man stood behind a hull or a walker leg.
            string hiddenFlag = GymStrip.Hidden(subjectVisible);
            if (hiddenFlag != null) flags.Add(hiddenFlag);
            if (e.Tab == GymTab.Clips && jsons.Count > 0 && File.Exists(jsons[0]) && Gym.JsonNumber(File.ReadAllText(jsons[0]), "men_in_frame") < 1f) flags.Add("no man in the T3 frame");
            return flags;
        }

        static void WriteEntry(string dir, StringBuilder summary, GymEntry e, GymDirector.Result r, List<string> jsons, List<string> flags,
                               ref int flagged, ref int done, string sheet = null, AnimState? pose = null, List<float> stripChanged = null,
                               string truth = null, bool machineChanged = false, bool previewOnly = false,
                               float subjectVisible = float.NaN, float subjectVisibleRef = float.NaN)
        {
            done++; if (flags.Count > 0) flagged++;
            var sb = new StringBuilder();
            sb.Append("{\n  \"entry\": \"").Append(Gym.Esc(e.ToString())).Append("\", \"tab\": \"").Append(e.Tab).Append("\", \"id\": ").Append(e.Id)
              .Append(", \"expect\": \"").Append(e.Expect).Append("\", \"note\": \"").Append(Gym.Esc(e.Note)).Append("\",\n");
            sb.Append("  \"trigger_tick\": ").Append(r.TriggerTick).Append(", \"errors\": ").Append(r.Errors).Append(", \"warnings\": ").Append(r.Warnings)
              .Append(", \"rejects\": ").Append(r.Rejects).Append(", \"previews\": ").Append(r.Previews)
              .Append(", \"canary\": ").Append(r.Canary ? "true" : "false").Append(", \"desync\": ").Append(r.Canary ? (r.Desync ? "true" : "false") : "null").Append(",\n");   // a desync is only seen with the canary
            sb.Append("  \"victim\": ").Append(r.Victim).Append(", \"victim_died\": ").Append(r.VictimDied ? "true" : "false").Append(",\n");
            // THE SIM'S TRUTH for this entry, beside the pictures: round 2's verdicts could not be checked because the
            // sidecar said nothing about whether the man really died or the machine was really destroyed (look-04).
            sb.Append("  \"truth\": {\"victim\": ").Append(r.Victim).Append(", \"died\": ").Append(r.VictimDied ? "true" : "false")
              .Append(", \"cause_killer_slot\": ").Append(r.VictimCause)
              .Append(", \"machine_changed\": ").Append(machineChanged ? "true" : "false")
              .Append(", \"ability_fired\": ").Append(r.Count(SimEventType.AbilityFired))
              .Append(", \"rejected\": ").Append(r.Rejects)
              .Append(", \"preview_only\": ").Append(previewOnly ? "true" : "false")
              .Append(", \"world\": ").Append(truth == null ? "null" : "\"" + Gym.Esc(truth) + "\"").Append("},\n");
            sb.Append("  \"consequence\": {\"men\": ").Append(r.Watch.Count).Append(", \"hits\": ").Append(r.WatchedHits).Append(", \"near_misses\": ").Append(r.WatchedNearMisses)
              .Append(", \"suppressed\": ").Append(r.WatchedSuppressed).Append(", \"deaths\": ").Append(r.WatchedDeaths)
              .Append(", \"turnabouts_per_man_min\": ").Append((r.WatchedSeconds > 0f ? r.Turnabouts / (r.WatchedSeconds / 60f) : 0f).ToString("0.0", Inv))
              .Append(", \"worst_man_turnabouts\": ").Append(r.WorstTurnabouts)
              .Append(", \"upright_share\": ").Append((r.WatchedSeconds > 0f ? r.UprightSeconds / r.WatchedSeconds : 0f).ToString("0.00", Inv)).Append("},\n");
            sb.Append("  \"events\": {");
            bool first = true;
            foreach (var kv in r.Events) { sb.Append(first ? "" : ", ").Append('"').Append(kv.Key).Append("\": ").Append(kv.Value); first = false; }
            sb.Append("},\n");
            if (pose.HasValue)
            {
                var p = pose.Value;
                sb.Append("  \"pose\": {\"clip\": \"").Append(p.Clip).Append("\", \"frame\": ").Append(p.Frame.ToString("0.00", Inv)).Append(", \"rung\": \"").Append(p.Rung)
                  .Append("\", \"shown_yaw\": ").Append(p.ShownYaw.ToString("0.000", Inv)).Append(", \"body_yaw\": ").Append(p.BodyYaw.ToString("0.000", Inv)).Append("},\n");
            }
            if (stripChanged != null)
            {
                sb.Append("  \"strip_changed\": [");
                for (int i = 0; i < stripChanged.Count; i++) sb.Append(i > 0 ? ", " : "").Append(float.IsNaN(stripChanged[i]) ? "-1" : stripChanged[i].ToString("0.0000", Inv));   // -1: the diff could not be read
                sb.Append("],\n");
            }
            // How much of the subject the camera really saw, and the lone rifleman this run measured him against.
            // null for a vehicle or a walker: VATRenderer.Hide is per sim slot and only men go through it (look-08).
            sb.Append("  \"subject_visible_frac\": ").Append(float.IsNaN(subjectVisible) ? "null" : subjectVisible.ToString("0.000", Inv))
              .Append(", \"subject_visible_ref\": ").Append(float.IsNaN(subjectVisibleRef) ? "null" : subjectVisibleRef.ToString("0.0000", Inv))
              .Append(", \"min_visible\": ").Append(GymStrip.MinVisible.ToString("0.00", Inv)).Append(",\n");
            sb.Append("  \"sheet\": ").Append(sheet == null ? "null" : "\"" + Gym.Esc(sheet) + "\"").Append(",\n  \"log\": [");
            for (int i = 0; i < r.Log.Count; i++) sb.Append(i > 0 ? ", " : "").Append('"').Append(Gym.Esc(r.Log[i])).Append('"');
            sb.Append("],\n  \"flags\": [");
            for (int i = 0; i < flags.Count; i++) sb.Append(i > 0 ? ", " : "").Append('"').Append(Gym.Esc(flags[i])).Append('"');
            sb.Append("]\n}\n");
            string path = Path.Combine(dir, e.Tab.ToString(), Safe(e.Name) + ".json");
            Directory.CreateDirectory(Path.GetDirectoryName(path));
            File.WriteAllText(path, sb.ToString());
            summary.Append(done > 1 ? ",\n" : "").Append("    {\"entry\": \"").Append(Gym.Esc(e.ToString())).Append("\", \"expect\": \"").Append(e.Expect).Append("\", \"flags\": [");
            for (int i = 0; i < flags.Count; i++) summary.Append(i > 0 ? ", " : "").Append('"').Append(Gym.Esc(flags[i])).Append('"');
            summary.Append("]}");
        }

        /// <summary>This run's flag signatures ("entry | flag" with numbers blanked) also raised by the newest older gym run.</summary>
        static List<string> Recurring(string dir, HashSet<string> now)
        {
            var list = new List<string>();
            try
            {
                string root = Path.GetDirectoryName(dir.TrimEnd('\\', '/'));
                DirectoryInfo prev = null;
                foreach (var d in new DirectoryInfo(root).GetDirectories())
                    if (!string.Equals(d.FullName.TrimEnd('\\', '/'), dir.TrimEnd('\\', '/'), System.StringComparison.OrdinalIgnoreCase)
                        && File.Exists(Path.Combine(d.FullName, "summary.json")) && (prev == null || d.CreationTimeUtc > prev.CreationTimeUtc)) prev = d;
                if (prev == null) return list;
                string old = File.ReadAllText(Path.Combine(prev.FullName, "summary.json"));
                foreach (Match m in Regex.Matches(old, "\"entry\": \"([^\"]*)\"[^\\]]*\"flags\": \\[([^\\]]*)\\]"))
                    foreach (Match f in Regex.Matches(m.Groups[2].Value, "\"([^\"]*)\""))
                    {
                        string sig = m.Groups[1].Value + " | " + Regex.Replace(f.Groups[1].Value, @"[0-9.]+", "#");
                        if (now.Contains(sig) && !list.Contains(sig)) list.Add(sig);
                    }
            }
            catch (System.Exception ex) { Debug.LogWarning("Gym: could not compare with the last run: " + ex.Message); }
            return list;
        }

        /// <summary>Append recurring flags to the board's lessons.md, when the board is there (the gym never commits it).</summary>
        static void AppendLessons(string sha, List<string> recurring)
        {
            if (recurring.Count == 0) return;
            try
            {
                string board = System.Environment.GetEnvironmentVariable("TW_BOARD");
                if (string.IsNullOrEmpty(board)) board = Path.GetFullPath(Path.Combine(Directory.GetCurrentDirectory(), "..", "..", "tw3d-board"));
                string file = Path.Combine(board, "lessons.md");
                if (!File.Exists(file)) return;
                var sb = new StringBuilder();
                string day = System.DateTime.Now.ToString("yyyy-MM-dd", Inv);
                foreach (var r in recurring) sb.Append("| ").Append(day).Append(" | gym | recurring flag: ").Append(r.Replace("|", "/")).Append(" | gym run ").Append(sha).Append(" |\n");
                File.AppendAllText(file, sb.ToString());
            }
            catch (System.Exception ex) { Debug.LogWarning("Gym: could not append lessons: " + ex.Message); }
        }

        /// <summary>
        /// The noise floor at one camera pose: two stills half a second apart with nothing triggered, and the share
        /// of the frame that repainted between them (rain, flicker, a lamp, a man breathing). `aimY` NaN leaves the
        /// rig's own aim. Hands the number to `got`, or NaN when the rig gave nothing.
        /// </summary>
        IEnumerator MeasureFloor(string raw, Vector2 at, float zoom, float aimY, System.Action<float> got)
        {
            string a = Path.Combine(raw, "calibz_a.png"), b = Path.Combine(raw, "calibz_b.png");
            CaptureRig.Shot(a, at.x, at.y, zoom, 30f, 25f, 800, 450, aimY);
            float u = Time.realtimeSinceStartup + 30f;
            while (CaptureRig.Pending() != "0" && Time.realtimeSinceStartup < u) yield return null;
            yield return new WaitForSeconds(0.5f);
            CaptureRig.Shot(b, at.x, at.y, zoom, 30f, 25f, 800, 450, aimY);
            u = Time.realtimeSinceStartup + 30f;
            while (CaptureRig.Pending() != "0" && Time.realtimeSinceStartup < u) yield return null;
            yield return null;   // the rig writes the last still and its sidecar the frame after the queue empties
            float f = Gym.JsonNumber(CaptureRig.Diff(a, b), "changed_frac");
            foreach (var tmp in new[] { a, b }) { TryDelete(tmp); TryDelete(Path.ChangeExtension(tmp, ".json")); }
            got(float.IsNaN(f) ? float.NaN : Mathf.Max(f, 0f));
        }

        /// <summary>
        /// cfRef for the occlusion measure (look-08): the share of a close frame ONE rifleman owns when he stands
        /// alone on a bare stage. Shot exactly as a strip's 'before' frame is - same zoom, same share, same pose,
        /// the same overlays hidden - and then shot again with him not drawn; the diff is all of him. NaN when the
        /// man could not be staged or the rig gave nothing, which leaves every share NaN and nothing flagged.
        /// </summary>
        IEnumerator MeasureSubjectRef(string raw, SimHost host, GymDirector director, System.Action<float> got)
        {
            float result = float.NaN;
            var vat = Object.FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
            if (vat == null) { Debug.LogWarning("Gym: no VATRenderer, so no occlusion reference"); got(result); yield break; }
            bool overlaysWere = TW.Presentation.Tactical.CombatFx.ShowOverlays;
            bool ringsWere = TW.Presentation.Tactical.TankRenderer.ShowRings;
            Debug.Log("Gym: " + director.Bare());
            yield return new WaitForSeconds(1.5f);
            var at = director.NextStage();
            int slot = director.Spawn(0, GymCatalogue.ArchetypeForFigure(0), at.x, at.y, 30f);   // the rifleman
            if (slot < 0) { Debug.LogWarning("Gym: could not stage the reference rifleman"); got(result); yield break; }
            yield return new WaitForSeconds(1.5f);
            float h = GymStrip.SubjectHeight(false, false), zoom = GymStrip.ZoomFor(h, 25f);
            Vector3 foot = host != null && host.Presenter != null ? (Vector3)host.Presenter.Drawn(slot) : new Vector3(at.x, 0f, at.y);
            CaptureRig.Subject = foot; CaptureRig.SubjectHeight = h;
            Overlays(false, false);
            string a = Path.Combine(raw, "visref_a.png"), b = Path.Combine(raw, "visref_b.png");
            CaptureRig.Shot(a, foot.x, foot.z, zoom, 30f, 25f, 800, 450, foot.y + h * 0.5f);
            float u = Time.realtimeSinceStartup + 30f;
            while (CaptureRig.Pending() != "0" && Time.realtimeSinceStartup < u) yield return null;
            vat.Hide(slot, true);
            yield return null; yield return null;   // the mask is read in VATRenderer\'s own LateUpdate
            CaptureRig.Shot(b, foot.x, foot.z, zoom, 30f, 25f, 800, 450, foot.y + h * 0.5f);
            u = Time.realtimeSinceStartup + 30f;
            while (CaptureRig.Pending() != "0" && Time.realtimeSinceStartup < u) yield return null;
            yield return null;
            vat.Hide(slot, false);
            float cf = Gym.JsonNumber(CaptureRig.Diff(a, b), "changed_frac");
            if (!float.IsNaN(cf) && cf > 0f) result = cf;
            foreach (var tmp in new[] { a, b }) { TryDelete(tmp); TryDelete(Path.ChangeExtension(tmp, ".json")); }
            CaptureRig.NoSubject();
            Overlays(overlaysWere, ringsWere);
            Debug.Log("Gym: " + director.Bare());
            yield return new WaitForSeconds(0.5f);
            got(result);
        }

        static void SafeWrite(string dir, StringBuilder summary, GymEntry e, GymDirector.Result r, List<string> jsons, List<string> flags,
                              ref int flagged, ref int done, string sheet, AnimState? pose, List<float> stripChanged = null,
                              string truth = null, bool machineChanged = false, bool previewOnly = false,
                              float subjectVisible = float.NaN, float subjectVisibleRef = float.NaN)
        {
            try { WriteEntry(dir, summary, e, r, jsons, flags, ref flagged, ref done, sheet, pose, stripChanged, truth, machineChanged, previewOnly, subjectVisible, subjectVisibleRef); }
            catch (System.Exception ex) { Debug.LogException(ex); }
        }

        /// <summary>
        /// Show or hide everything that is HUD: the ability aiming disc and the selection marker (CombatFx draws both
        /// in the world), the banner, and every UI Toolkit document - as Perf/PerfBench.HideHud does for an image run.
        /// A gym strip hides them (look-04): an aiming disc or a selection ring repaints a large share of a close
        /// frame, and the strip's measurement is there to isolate the EFFECT, not the instrument's own furniture.
        /// </summary>
        static void Overlays(bool show, bool rings)
        {
            TW.Presentation.Tactical.CombatFx.ShowOverlays = show;
            // the cyan ground ring under a machine is drawn in the WORLD by TankRenderer, so the UIDocument sweep
            // below never touched it: round 3 has it lying across the hull of every vehicle entry (look-08).
            TW.Presentation.Tactical.TankRenderer.ShowRings = rings;
            var display = show
                ? new UnityEngine.UIElements.StyleEnum<UnityEngine.UIElements.DisplayStyle>(UnityEngine.UIElements.StyleKeyword.Null)
                : new UnityEngine.UIElements.StyleEnum<UnityEngine.UIElements.DisplayStyle>(UnityEngine.UIElements.DisplayStyle.None);
            foreach (var doc in Object.FindObjectsByType<UnityEngine.UIElements.UIDocument>(FindObjectsSortMode.None))
                if (doc.rootVisualElement != null) doc.rootVisualElement.style.display = display;
        }

        static string Safe(string s)
        {
            var sb = new StringBuilder(s.Length);
            foreach (char c in s) sb.Append(char.IsLetterOrDigit(c) || c == '_' || c == '-' ? c : '_');
            return sb.ToString();
        }

        static void TryDelete(string f) { try { if (File.Exists(f)) File.Delete(f); } catch { } }

        /// <summary>An entry's seed for the presentation's random numbers: FNV-1a of its name, the same in every run.</summary>
        static int Seed(string name)
        {
            uint h = 2166136261u;
            foreach (char c in name) { h ^= c; h *= 16777619u; }
            return (int)h;
        }

        void Finish(bool quit, int code)
        {
            if (finished) return;
            finished = true;
            Time.captureFramerate = 0;
            TW.Presentation.Terrain.GreyboxTerrainView.Unmetered = false;
            if (storms != null) for (int k = 0; k < storms.Length; k++) if (storms[k] != null) storms[k].FreezeSeconds = stormFreeze[k];
            StopAllCoroutines();
            Destroy(this);
            if (!quit) return;
            EditorApplication.ExitPlaymode();
            EditorApplication.delayCall += () => EditorApplication.Exit(code);
        }
    }

    /// <summary>TW > Gym: one entry at a time, by hand, in Play.</summary>
    public sealed class GymWindow : EditorWindow
    {
        GymTab tab;
        string filter = "";
        Vector2 scroll;
        int follow = -1;
        string last = "";

        [MenuItem("TW/Gym")]
        static void Open() => GetWindow<GymWindow>("Gym");

        void OnGUI()
        {
            if (!Application.isPlaying)
            {
                EditorGUILayout.HelpBox("Enter Play in GreyboxCorridor to use the gym by hand, or run the whole catalogue:", MessageType.Info);
                if (GUILayout.Button("Run the gym (every tab, every band)")) Debug.Log(Gym.Run(""));
                if (!string.IsNullOrEmpty(Gym.LastRun)) EditorGUILayout.LabelField("Last run", Gym.LastRun);
                return;
            }
            var host = Object.FindFirstObjectByType<SimHost>();
            if (host == null || host.Local == null) { EditorGUILayout.HelpBox("No SimHost in this scene.", MessageType.Warning); return; }
            var d = GymDirector.Attach(host);

            EditorGUILayout.BeginHorizontal();
            if (GUILayout.Button("Quiet the battle")) d.Quiet();
            if (GUILayout.Button("Unquiet")) d.Unquiet();
            if (GUILayout.Button("Clear the stage")) d.Clear();
            if (GUILayout.Button("Run this tab")) Debug.Log(Gym.Run("tabs=" + tab.ToString().ToLowerInvariant() + (filter.Length > 0 ? " filter=" + filter : "")));
            EditorGUILayout.EndHorizontal();

            EditorGUILayout.BeginHorizontal();
            EditorGUILayout.LabelField("Look", GUILayout.Width(40));
            foreach (var b in GymCatalogue.Bands)
                if (GUILayout.Button(b.Name)) GymDirector.Look(d.Current != null ? new Vector2(d.Current.Focus.x, d.Current.Focus.z) : d.Stage, b.Zoom);
            EditorGUILayout.EndHorizontal();

            tab = (GymTab)GUILayout.Toolbar((int)tab, System.Enum.GetNames(typeof(GymTab)));
            filter = EditorGUILayout.TextField("Filter", filter);
            if (!string.IsNullOrEmpty(last)) EditorGUILayout.HelpBox(last, MessageType.None);

            List<GymEntry> list;
            switch (tab)
            {
                case GymTab.Clips: list = GymCatalogue.Clips(); break;
                case GymTab.Units: list = GymCatalogue.Units(host.Local.World); break;
                case GymTab.Abilities: list = GymCatalogue.Abilities(); break;
                case GymTab.Deaths: list = GymCatalogue.Deaths(); break;
                case GymTab.Scenes: list = GymCatalogue.Scenes(); break;
                default: list = GymCatalogue.Events(); break;
            }
            scroll = EditorGUILayout.BeginScrollView(scroll);
            foreach (var e in list)
            {
                if (filter.Length > 0 && e.Name.IndexOf(filter, System.StringComparison.OrdinalIgnoreCase) < 0) continue;
                EditorGUILayout.BeginHorizontal();
                EditorGUILayout.LabelField(new GUIContent(e.Name + "  (" + e.Expect + ")", e.Note), GUILayout.MinWidth(220));
                bool can = e.Expect != GymExpect.Covered && e.Expect != GymExpect.Excluded;
                GUI.enabled = can;
                if (GUILayout.Button("Play", GUILayout.Width(50))) Play(d, host, e);
                GUI.enabled = true;
                EditorGUILayout.EndHorizontal();
            }
            EditorGUILayout.EndScrollView();

            if (follow >= 0 && host.Animation != null)
            {
                EditorGUILayout.LabelField("Trace of slot " + follow);
                EditorGUILayout.TextArea(host.Animation.TraceText(12), GUILayout.MinHeight(120));
            }
        }

        void Play(GymDirector d, SimHost host, GymEntry e)
        {
            d.Clear();
            d.NextStage();
            d.Begin(e);
            var at = d.Stage;
            int slot = -1;
            switch (e.Tab)
            {
                case GymTab.Clips: slot = d.PlayClip(e.Figure, (Clip)e.Id, at); break;
                case GymTab.Units: slot = d.Spawn(0, e.Id, at.x, at.y, 30f); break;
                case GymTab.Abilities: { var t = at + new Vector2(0f, 30f); d.Current.Focus = new Unity.Mathematics.float3(t.x, 0f, t.y); d.Ability((OffMapAbilityId)e.Id, t); break; }
                case GymTab.Deaths: slot = d.Death((DeathKind)e.Id, 0, at); break;
                case GymTab.Events:
                {
                    var t = (SimEventType)e.Id;
                    bool vehicle = t.ToString().StartsWith("Vehicle", System.StringComparison.Ordinal) || t == SimEventType.WreckRecorded;
                    slot = d.Spawn(0, vehicle ? VehicleArchetype.Maw : 0, at.x, at.y, 30f);
                    d.Preview(t, slot, at, new Unity.Mathematics.float3(0f, 0f, 1f), 1f);
                    break;
                }
                case GymTab.Scenes: d.StartCoroutine(d.Scene((GymScene)e.Id)); break;
            }
            if (slot >= 0 && host.Animation != null) { host.Animation.Follow(slot); follow = slot; }
            GymDirector.Look(new Vector2(d.Current.Focus.x, d.Current.Focus.z), e.Tab == GymTab.Clips ? 7.5f : 30f);
            last = e + (slot >= 0 ? " on slot " + slot : "") + (string.IsNullOrEmpty(e.Note) ? "" : ": " + e.Note);
        }

        void OnInspectorUpdate() { if (Application.isPlaying) Repaint(); }
    }
}
