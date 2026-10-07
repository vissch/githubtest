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
// What tells one moment from another (2026-10-07, after the critique): the checkout's branch and commit as they were
// at the press (read from the .git files, no process started), every man's place in one packed line, what was
// selected, where the cursor stood and on what ground, the armed ability, and the last errors the console took.
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
        /// <summary>The last errors and exceptions the console took before the press, oldest first: "HH:mm:ss  first line".</summary>
        public string[] errors = new string[0];

        [Serializable]
        public sealed class Build
        {
            public string version = "", unity = "";
            public bool editor;
            /// <summary>In the editor, the project's folder: the board reads the checkout's branch and commit from it.</summary>
            public string project = "";
            /// <summary>In a build, the build-info.json beside the exe as it is (the commit it was built from), else empty.</summary>
            public string build_info = "";
            /// <summary>In the editor, the checkout's branch and commit (8 characters) at the press: the board reads them
            /// later, when the checkout may have moved on. Empty when the project is in no checkout, and in a build.</summary>
            public string branch = "", commit = "";
        }

        [Serializable]
        public sealed class Machine
        {
            public string name = "", gpu = "", cpu = "";
            public int ram_mb, screen_w, screen_h;
            public bool fullscreen;
            public string quality = "";
        }

        [Serializable] public sealed class Units { public int team, archetype, count; public string name = ""; }

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
            /// <summary>Every man and machine alive, one after another in one line: "slot team archetype x z hp;". A line
            /// and not an array, so six hundred men are one line of the file and not four thousand.</summary>
            public string positions = "";
            /// <summary>The slots he had selected, with spaces between.</summary>
            public string selected = "";
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
            /// <summary>The cursor on the screen, in pixels from the bottom left; where it pointed on the ground, when it
            /// was over the field (cursor_on_ground); the support ability that was armed, or None.</summary>
            public Vector2 cursor;
            public bool cursor_on_ground;
            public Vector3 cursor_ground;
            public string armed = "None";
        }

        /// <summary>frame_ms is the mean of the frames before the press, not the frame of the press.</summary>
        [Serializable] public sealed class Perf { public float frame_ms; public int draw_calls; public long vertices; public int indirect_draws; }
    }

    public static class FeedbackCapture
    {
        public const string Schema = "tw-feedback/1";
        public const string EnvVar = "TW_FEEDBACK";
        public const string FileName = "capture.json", ShotName = "shot.png", OpenName = "writing";
        public const int MaxNote = 2000;
        public const int MaxErrors = 8, MaxErrorLength = 240;
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
        public static FeedbackRecord Gather(SimHost host, MatchClock clock, MatchStats stats, Camera cam, DateTime now, HudController hud = null)
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
            if (Application.isEditor) { r.build.project = above.Replace('\\', '/'); Checkout(above, r.build); }
            else r.build.build_info = ReadOrEmpty(Path.Combine(above, "build-info.json"));
            r.machine.name = SystemInfo.deviceName; r.machine.gpu = SystemInfo.graphicsDeviceName; r.machine.cpu = SystemInfo.processorType;
            r.machine.ram_mb = SystemInfo.systemMemorySize; r.machine.screen_w = Screen.width; r.machine.screen_h = Screen.height;
            r.machine.fullscreen = Screen.fullScreen;
            int q = QualitySettings.GetQualityLevel(); var names = QualitySettings.names;
            r.machine.quality = q >= 0 && q < names.Length ? names[q] : q.ToString(Inv);
            r.perf.frame_ms = frameMs > 0f ? frameMs : Time.unscaledDeltaTime * 1000f;
            r.errors = errors.ToArray();
            r.perf.draw_calls = FrameBudget.DrawCalls; r.perf.vertices = FrameBudget.Vertices; r.perf.indirect_draws = FrameBudget.IndirectDraws;
            if (cam != null)
            {
                r.view.position = cam.transform.position; r.view.euler = cam.transform.eulerAngles; r.view.fov = cam.fieldOfView;
                var tc = cam.GetComponent<TacticalCamera>();
                if (tc != null) { r.view.focus = tc.Focus; r.view.zoom = tc.Zoom; }
                var panel = cam.GetComponent<TestPanel>();
                if (panel != null)
                {
                    r.view.armed = panel.Armed.ToString();
                    if (panel.TryGroundPoint(out var ground)) { r.view.cursor_on_ground = true; r.view.cursor_ground = ground; }
                }
            }
            var mouse = UnityEngine.InputSystem.Mouse.current;
            if (mouse != null) r.view.cursor = mouse.position.ReadValue();
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
            m.positions = Places(w);
            if (hud != null && hud.Selection != null) m.selected = Slots(hud.Selection.Model.Items);
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
            foreach (var kv in by) list.Add(new FeedbackRecord.Units { team = kv.Key >> 8, archetype = kv.Key & 0xFF, count = kv.Value, name = UnitLook.Name((byte)(kv.Key & 0xFF)) ?? "" });
            return list.ToArray();
        }

        /// <summary>Where everyone alive stands: "slot team archetype x z hp;" for each, metres to one decimal.</summary>
        static string Places(SimWorld w)
        {
            var sb = new System.Text.StringBuilder(w.AliveCount * 24);
            for (int i = 0; i < w.HighWater; i++)
            {
                if (!w.IsAlive(i)) continue;
                var p = w.Position[i];
                sb.Append(i.ToString(Inv)).Append(' ').Append(((int)w.Team[i]).ToString(Inv)).Append(' ').Append(((int)w.Archetype[i]).ToString(Inv)).Append(' ')
                  .Append(p.x.ToString("0.#", Inv)).Append(' ').Append(p.z.ToString("0.#", Inv)).Append(' ').Append(w.Hp[i].ToString("0", Inv)).Append(';');
            }
            return sb.ToString();
        }

        static string Slots(IReadOnlyList<UnitHandle> items)
        {
            var sb = new System.Text.StringBuilder(items.Count * 5);
            for (int i = 0; i < items.Count; i++) { if (i > 0) sb.Append(' '); sb.Append(items[i].Slot.ToString(Inv)); }
            return sb.ToString();
        }

        // ---- what the game keeps between presses, so a capture can say it -------------------------------------------
        static float frameMs;
        static readonly List<string> errors = new List<string>();
        // kept across scene loads (an error before a restart is still his to report), put back when Play ends
        static FeedbackCapture() => SceneStatics.Register(nameof(FeedbackCapture), Forget);

        /// <summary>Every frame (the router's Update): the running mean a capture reports as its frame time. The frame of
        /// the press alone is the worst witness: it is the one the press itself made longer.</summary>
        public static void Frame(float unscaledSeconds)
        {
            float ms = unscaledSeconds * 1000f;
            frameMs = frameMs <= 0f ? ms : Mathf.Lerp(frameMs, ms, 0.05f);
        }

        /// <summary>The console's log hook (the router subscribes): errors and exceptions are kept, the last MaxErrors.</summary>
        public static void Heard(string condition, string stackTrace, LogType type)
        {
            if (type != LogType.Error && type != LogType.Exception && type != LogType.Assert) return;
            string line = (condition ?? "").Split('\n')[0].Trim();
            if (line.Length > MaxErrorLength) line = line.Substring(0, MaxErrorLength);
            if (errors.Count >= MaxErrors) errors.RemoveAt(0);
            errors.Add(DateTime.Now.ToString("HH:mm:ss", Inv) + "  " + line);
        }

        /// <summary>Nothing heard and no frames counted (a test's clean start).</summary>
        public static void Forget() { errors.Clear(); frameMs = 0f; }

        /// <summary>The branch and commit of the checkout a project folder is in, from the .git files as they are: HEAD,
        /// the ref it names, packed-refs. A worktree's .git is a file that names its own folder, whose commondir holds
        /// the refs. Anything unreadable leaves both empty: the board then reads the checkout itself, later.</summary>
        public static void Checkout(string project, FeedbackRecord.Build b)
        {
            try
            {
                string at = project, dot = null;
                for (int up = 0; up < 4 && !string.IsNullOrEmpty(at); up++, at = Path.GetDirectoryName(at))
                {
                    string d = Path.Combine(at, ".git");
                    if (Directory.Exists(d)) { dot = d; break; }
                    if (File.Exists(d))
                    {
                        string named = File.ReadAllText(d).Trim();
                        if (named.StartsWith("gitdir:", StringComparison.Ordinal)) dot = Path.GetFullPath(Path.Combine(at, named.Substring(7).Trim()));
                        break;
                    }
                }
                if (dot == null) return;
                string head = File.ReadAllText(Path.Combine(dot, "HEAD")).Trim(), sha = head;
                if (head.StartsWith("ref:", StringComparison.Ordinal))
                {
                    string name = head.Substring(4).Trim(), common = dot;
                    string shared = Path.Combine(dot, "commondir");
                    if (File.Exists(shared)) common = Path.GetFullPath(Path.Combine(dot, File.ReadAllText(shared).Trim()));
                    b.branch = name.StartsWith("refs/heads/", StringComparison.Ordinal) ? name.Substring(11) : name;
                    sha = "";
                    foreach (string home in new[] { dot, common })
                    {
                        string loose = Path.Combine(home, name);
                        if (File.Exists(loose)) { sha = File.ReadAllText(loose).Trim(); break; }
                    }
                    string packed = Path.Combine(common, "packed-refs");
                    if (sha.Length == 0 && File.Exists(packed))
                        foreach (string line in File.ReadAllLines(packed))
                            if (line.EndsWith(" " + name, StringComparison.Ordinal)) { sha = line.Substring(0, line.IndexOf(' ')); break; }
                }
                else b.branch = "HEAD";
                b.commit = sha.Length >= 8 ? sha.Substring(0, 8) : "";
            }
            catch (Exception) { b.branch = ""; b.commit = ""; }
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
            string dir = Claim(r, root);
            try { Save(r, dir); }
            catch (Exception) { Withdraw(dir); throw; }
            return dir;
        }

        /// <summary>The folder a new capture goes in, made and marked open, before anything is written into it: whoever
        /// asks holds its name from here on, so a write that fails later has a folder to take back (Withdraw).</summary>
        public static string Claim(FeedbackRecord r, string root = null)
        {
            root = string.IsNullOrEmpty(root) ? Root() : root;
            string stem = r.id, dir = Path.Combine(root, stem);
            for (int n = 2; Directory.Exists(dir); n++) { r.id = stem + "-" + n.ToString(Inv); dir = Path.Combine(root, r.id); }
            Directory.CreateDirectory(dir);
            File.WriteAllText(Path.Combine(dir, OpenName), "");
            return dir;
        }

        /// <summary>A capture went wrong part way. With a state file it stands as it is and is closed; without one it is
        /// nothing the board can read, and a folder left marked open would sit there for ever: it is removed.</summary>
        public static void Withdraw(string dir)
        {
            try
            {
                if (File.Exists(Path.Combine(dir, FileName))) { Done(dir); return; }
                if (Directory.Exists(dir)) Directory.Delete(dir, true);
            }
            catch (Exception e) { Debug.LogWarning($"FeedbackCapture: could not take back {dir}: {e.Message}"); }
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
