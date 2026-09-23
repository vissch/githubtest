// Phase: tooling (perf pass, 2026-09-23) — builds the Windows x64 player that TW.Perf.PerfBench measures.
// Windows x64 is the only ship target (owner, 2026-09-20), and until now no Windows player had ever been built or
// measured: every number in docs/05 came from the editor. This builds the enabled EditorBuildSettings scenes to
// Builds/WinBench (gitignored) and writes build-info.json beside the exe (commit, dirty flag, backend, dev flag), which
// PerfBench copies into every report so a number always says what it was measured on.
//   Menu:     TW/Build/Windows Bench, TW/Build/Windows Bench (Development)
//   Live:     unity command eval "TW.Editor.BuildWindows.Queue(false); return 1;"   (returns at once, builds on the next
//             editor tick; poll Builds/WinBench/build-status.txt)
//   Batch:    Unity.exe -batchmode -quit -projectPath . -executeMethod TW.Editor.BuildWindows.CommandLine [-twdev]
// A release build has no profiler markers (they compile out); build the Development variant to attribute time.
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Text;
using UnityEditor;
using UnityEditor.Build.Reporting;
using Debug = UnityEngine.Debug;

namespace TW.Editor
{
    public static class BuildWindows
    {
        public const string ReleaseDir = "Builds/WinBench", DevelopmentDir = "Builds/WinBenchDev", Exe = "TrenchWarfare.exe";

        [MenuItem("TW/Build/Windows Bench")] static void Menu() => Debug.Log(Build(false));
        [MenuItem("TW/Build/Windows Bench (Development)")] static void MenuDev() => Debug.Log(Build(true));

        /// <summary>Schedules a build for the next editor tick and returns at once, so a `unity command eval` does not
        /// hold the command server for the minutes a first build (every shader variant) takes.</summary>
        public static string Queue(bool development)
        {
            string dir = development ? DevelopmentDir : ReleaseDir;
            Directory.CreateDirectory(dir);
            File.WriteAllText(Path.Combine(dir, "build-status.txt"), "queued " + DateTime.Now.ToString("s", CultureInfo.InvariantCulture));
            EditorApplication.delayCall += () => Build(development);
            return "queued " + (development ? "development" : "release") + " build into " + dir;
        }

        public static string Build(bool development)
        {
            string dir = development ? DevelopmentDir : ReleaseDir;
            Directory.CreateDirectory(dir);
            string status = Path.Combine(dir, "build-status.txt");
            File.WriteAllText(status, "building " + DateTime.Now.ToString("s", CultureInfo.InvariantCulture));
            if (!PlayerSettings.enableFrameTimingStats)
                Debug.LogWarning("BuildWindows: Frame Timing Stats is off, so the player cannot report GPU time; set PlayerSettings.enableFrameTimingStats");
            var scenes = new List<string>();
            foreach (var s in EditorBuildSettings.scenes) if (s.enabled) scenes.Add(s.path);
            var opts = new BuildPlayerOptions
            {
                scenes = scenes.ToArray(),
                locationPathName = Path.Combine(dir, Exe),
                target = BuildTarget.StandaloneWindows64,
                targetGroup = BuildTargetGroup.Standalone,
                options = development ? BuildOptions.Development : BuildOptions.None,
            };
            var started = DateTime.UtcNow;
            BuildReport report;
            try { report = BuildPipeline.BuildPlayer(opts); }
            catch (Exception e) { File.WriteAllText(status, "failed: " + e.Message); throw; }
            var sum = report.summary;
            double seconds = (DateTime.UtcNow - started).TotalSeconds;
            string line = $"{sum.result}: {opts.locationPathName} ({sum.totalSize / 1048576} MB, {sum.totalErrors} errors, {sum.totalWarnings} warnings, {seconds:0} s)";
            WriteInfo(dir, development, sum, seconds, scenes);
            File.WriteAllText(status, line);
            Debug.Log("BuildWindows: " + line);
            return line;
        }

        /// <summary>-executeMethod entry for a closed editor. -twdev builds the Development variant.</summary>
        public static void CommandLine()
        {
            bool dev = Array.IndexOf(Environment.GetCommandLineArgs(), "-twdev") >= 0;
            string line = Build(dev);
            EditorApplication.Exit(line.StartsWith("Succeeded", StringComparison.Ordinal) ? 0 : 1);
        }

        static void WriteInfo(string dir, bool development, BuildSummary sum, double seconds, List<string> scenes)
        {
            string sha = Git("rev-parse HEAD").Trim();
            string porcelain = Git("status --porcelain");
            int dirty = 0; foreach (var l in porcelain.Split('\n')) if (l.Trim().Length > 0) dirty++;
            var backend = PlayerSettings.GetScriptingBackend(UnityEditor.Build.NamedBuildTarget.Standalone);
            var sb = new StringBuilder();
            sb.Append("{ \"git_sha\": \"").Append(sha).Append("\", \"dirty_files\": ").Append(dirty)
              .Append(", \"unity\": \"").Append(UnityEngine.Application.unityVersion).Append("\"")
              .Append(", \"backend\": \"").Append(backend).Append("\"")
              .Append(", \"development\": ").Append(development ? "true" : "false")
              .Append(", \"frame_timing_stats\": ").Append(PlayerSettings.enableFrameTimingStats ? "true" : "false")
              .Append(", \"result\": \"").Append(sum.result).Append("\"")
              .Append(", \"size_mb\": ").Append((sum.totalSize / 1048576.0).ToString("0.0", CultureInfo.InvariantCulture))
              .Append(", \"build_seconds\": ").Append(seconds.ToString("0", CultureInfo.InvariantCulture))
              .Append(", \"built_utc\": \"").Append(DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ssZ", CultureInfo.InvariantCulture)).Append("\"")
              .Append(", \"scenes\": [");
            for (int i = 0; i < scenes.Count; i++) sb.Append(i > 0 ? ", " : "").Append('"').Append(scenes[i]).Append('"');
            sb.Append("] }");
            File.WriteAllText(Path.Combine(dir, "build-info.json"), sb.ToString());
        }

        static string Git(string args)
        {
            try
            {
                var p = new ProcessStartInfo("git", args) { RedirectStandardOutput = true, UseShellExecute = false, CreateNoWindow = true };
                using var proc = Process.Start(p);
                string o = proc.StandardOutput.ReadToEnd();
                proc.WaitForExit(10000);
                return o;
            }
            catch (Exception) { return ""; }
        }
    }
}
