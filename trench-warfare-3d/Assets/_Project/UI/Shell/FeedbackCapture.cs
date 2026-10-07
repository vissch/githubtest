// Phase: tooling (2026-10-07) — the feedback capture (F10): the screen as a picture and the game as a state file.
// The owner: "inside the game i want a button i can press f10 that will pause the game, take a screenshot and save
// the variables and metadata of the game so i can give feedback. it should connect to the tasks board". The router
// (ShellRouter.Feedback) calls Gather and Write here on the frame of the press and takes the picture; the box for his
// words (FeedbackScreen) comes up a frame later, so it is never in the picture, and Save writes his words in.
// A capture is a folder: capture.json (FeedbackRecord, as JsonUtility writes it) and shot.png. It goes where the asset
// board's watcher looks: TW_FEEDBACK, else %LOCALAPPDATA%\TrenchWarfare\feedback, the way the gym's runs go to their
// folder. Tools/assetboard/feedback.py takes it in from there and the board lists it as a task. While the box is open
// the folder also holds a file named "writing": the board leaves a capture alone until that file is gone, so his words
// are in the state file before it moves.
// The format is Tools/assetboard/feedback.example.json; FeedbackCaptureTests holds this record to it key by key. The
// field names below ARE the file's keys, which is why they are not spelled as the rest of the code's are.
// Not in a capture: the match itself. It is not recorded (owner, 2026-10-07: "Picture and state only"), so a capture
// names the seed and the tick and cannot be returned to.
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using UnityEngine;
using UnityEngine.SceneManagement;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Sim;

namespace TW.UI
{
    [Serializable]
    public sealed class FeedbackRecord
    {
        public string schema = FeedbackCapture.Schema;
        public string id = "", when_local = "", when_utc = "", note = "", shot = FeedbackCapture.ShotName, scene = "";
        /// <summary>A match was running. On a menu `match` is written all the same (JsonUtility writes no null) and means nothing.</summary>
        public bool in_match;
        public bool hud_toolkit;
        public Build build = new Build();
        public Machine machine = new Machine();
        public Match match = new Match();
        public View view = new View();
        public Perf perf = new Perf();
        public GameSettings settings = GameSettings.Defaults();
        /// <summary>Knobs.ToJson(): the run-time knobs that were set and read, as its own JSON in a string.</summary>
        public string knobs = "{}";

        [Serializable]
        public sealed class Build
        {
            public string version = "", unity = "";
            public bool editor;
            /// <summary>In the editor, the project's folder: the board reads the checkout's branch and commit from it.</summary>
            public string project = "";
            /// <summary>In a build, the build-info.json beside the exe as it is (the commit it was built from), else empty.</summary>
            public string build_info = "";
        }

        [Serializable]
        public sealed class Machine
        {
            public string name = "", gpu = "", cpu = "";
            public int ram_mb, screen_w, screen_h;
            public bool fullscreen;
            public string quality = "";
        }

        [Serializable] public sealed class Units { public int team, archetype, count; }

        [Serializable]
        public sealed class Match
        {
            public uint tick;
            public float tick_seconds;
            public int winner = -1;
            public int[] silver = new int[2];
            public int alive, high_water;
            /// <summary>SimWorld.Hash() at the press, in hex.</summary>
            public string hash = "";
            /// <summary>What held the clock before F10 did: None when the match was running.</summary>
            public string holds_before = "None";
            public float speed = 1f;
            public Units[] units = new Units[0];
            public MatchLaunch.Request request = new MatchLaunch.Request();
            public MatchReport report = new MatchReport();
        }

        [Serializable]
        public sealed class View
        {
            public Vector3 position, euler;
            public float fov;
            public Vector2 focus;
            public float zoom;
        }

        [Serializable] public sealed class Perf { public float frame_ms; public int draw_calls; public long vertices; public int indirect_draws; }
    }

    public static class FeedbackCapture
    {
        public const string Schema = "tw-feedback/1";
        public const string EnvVar = "TW_FEEDBACK";
        public const string FileName = "capture.json", ShotName = "shot.png", OpenName = "writing";
        public const int MaxNote = 2000;
        static readonly CultureInfo Inv = CultureInfo.InvariantCulture;

        /// <summary>The folder captures go in: TW_FEEDBACK, else %LOCALAPPDATA%\TrenchWarfare\feedback.</summary>
        public static string Root()
        {
            string env = Environment.GetEnvironmentVariable(EnvVar);
            if (!string.IsNullOrEmpty(env)) return env;
            return Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "TrenchWarfare", "feedback");
        }

        /// <summary>The game as it is now. Reads only: nothing in the match changes. With no host (a menu) the record has
        /// the scene, the settings, the machine and the build, and says in_match false.</summary>
        public static FeedbackRecord Gather(SimHost host, MatchClock clock, MatchStats stats, Camera cam, DateTime now)
        {
            var r = new FeedbackRecord
            {
                id = now.ToString("yyyy-MM-dd-HHmmss", Inv),
                when_local = now.ToString("yyyy-MM-dd HH:mm:ss", Inv),
                when_utc = now.ToUniversalTime().ToString("yyyy-MM-dd'T'HH:mm:ss'Z'", Inv),
                scene = SceneManager.GetActiveScene().name,
                hud_toolkit = HudBridge.UseToolkitHud,
                settings = SettingsStore.Current ?? GameSettings.Defaults(),
                knobs = Knobs.ToJson(),
            };
            r.build.version = Application.version; r.build.unity = Application.unityVersion; r.build.editor = Application.isEditor;
            string above = Path.GetDirectoryName(Application.dataPath) ?? "";
            if (Application.isEditor) r.build.project = above.Replace('\\', '/');
            else r.build.build_info = ReadOrEmpty(Path.Combine(above, "build-info.json"));
            r.machine.name = SystemInfo.deviceName; r.machine.gpu = SystemInfo.graphicsDeviceName; r.machine.cpu = SystemInfo.processorType;
            r.machine.ram_mb = SystemInfo.systemMemorySize; r.machine.screen_w = Screen.width; r.machine.screen_h = Screen.height;
            r.machine.fullscreen = Screen.fullScreen;
            int q = QualitySettings.GetQualityLevel(); var names = QualitySettings.names;
            r.machine.quality = q >= 0 && q < names.Length ? names[q] : q.ToString(Inv);
            r.perf.frame_ms = Time.unscaledDeltaTime * 1000f;
            r.perf.draw_calls = FrameBudget.DrawCalls; r.perf.vertices = FrameBudget.Vertices; r.perf.indirect_draws = FrameBudget.IndirectDraws;
            if (cam != null)
            {
                r.view.position = cam.transform.position; r.view.euler = cam.transform.eulerAngles; r.view.fov = cam.fieldOfView;
                var tc = cam.GetComponent<TacticalCamera>();
                if (tc != null) { r.view.focus = tc.Focus; r.view.zoom = tc.Zoom; }
            }
            if (host == null || host.Local == null) return r;

            var w = host.Local.World;
            var m = r.match;
            r.in_match = true;
            m.tick = w.Tick; m.tick_seconds = w.Config.TickSeconds; m.winner = w.WinnerTeam;
            m.silver[0] = w.Silver[0]; m.silver[1] = w.Silver[1];
            m.alive = w.AliveCount; m.high_water = w.HighWater;
            m.hash = w.Hash().ToString("x16", Inv);
            m.holds_before = clock != null ? clock.Holds.ToString() : "None";
            m.speed = clock != null ? clock.Speed : host.TimeScale;
            m.units = Count(w);
            m.request = MatchLaunch.Running ?? MatchLaunch.Request.From(host);
            m.report = stats != null ? stats.Report(m.request) : new MatchReport();
            return r;
        }

        /// <summary>The men and machines alive, by side and kind: the sim keeps no such count, so this walks the slots.</summary>
        static FeedbackRecord.Units[] Count(SimWorld w)
        {
            var by = new SortedDictionary<int, int>();
            for (int i = 0; i < w.HighWater; i++)
            {
                if (!w.IsAlive(i)) continue;
                int key = (w.Team[i] << 8) | w.Archetype[i];
                by.TryGetValue(key, out int n); by[key] = n + 1;
            }
            var list = new List<FeedbackRecord.Units>(by.Count);
            foreach (var kv in by) list.Add(new FeedbackRecord.Units { team = kv.Key >> 8, archetype = kv.Key & 0xFF, count = kv.Value });
            return list.ToArray();
        }

        static string ReadOrEmpty(string path)
        {
            try { return File.Exists(path) ? File.ReadAllText(path) : ""; }
            catch (Exception) { return ""; }
        }

        /// <summary>Write a new capture and return its folder: root/id, or id-2, id-3 when that second already has one
        /// (the record's id follows). The folder is marked open: call Done when his words are in.</summary>
        public static string Write(FeedbackRecord r, string root = null)
        {
            root = string.IsNullOrEmpty(root) ? Root() : root;
            string stem = r.id, dir = Path.Combine(root, stem);
            for (int n = 2; Directory.Exists(dir); n++) { r.id = stem + "-" + n.ToString(Inv); dir = Path.Combine(root, r.id); }
            Directory.CreateDirectory(dir);
            File.WriteAllText(Path.Combine(dir, OpenName), "");
            Save(r, dir);
            return dir;
        }

        /// <summary>The state file, whole or not at all (written beside it and swapped in, as the settings are).</summary>
        public static void Save(FeedbackRecord r, string dir)
        {
            Directory.CreateDirectory(dir);
            string path = Path.Combine(dir, FileName), tmp = path + ".tmp";
            File.WriteAllText(tmp, JsonUtility.ToJson(r, true));
            if (File.Exists(path)) File.Replace(tmp, path, null); else File.Move(tmp, path);
        }

        public static FeedbackRecord Read(string dir) => JsonUtility.FromJson<FeedbackRecord>(File.ReadAllText(Path.Combine(dir, FileName)));

        /// <summary>His words are in (or he left none): the board may take the capture.</summary>
        public static void Done(string dir)
        {
            try { File.Delete(Path.Combine(dir, OpenName)); }
            catch (Exception e) { Debug.LogWarning($"FeedbackCapture: could not close {dir}: {e.Message}"); }
        }

        /// <summary>His words as they are kept: trimmed, and no longer than MaxNote.</summary>
        public static string Clip(string words)
        {
            words = (words ?? "").Trim();
            return words.Length <= MaxNote ? words : words.Substring(0, MaxNote);
        }
    }
}
