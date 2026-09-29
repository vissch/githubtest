// Phase: tooling (deaths, 2026-09-29) — one TW.Perf.PerfBench run from batch mode with nobody driving, for an A/B of a
// knob (the absurd deaths: fx.deathAbsurd 0 against 1 on the barrage), one Unity launch per side. CaptureRig.Bench does
// the same from a running editor: the bench string goes in the TW_BENCH environment variable (it crosses the domain
// reload on entering Play), the bench runs the match, writes its report and leaves Play. Compare two reports with
// Tools/aosa/cmp.py.
//
// Run it by name WITH a graphics device (not -nographics): the draw counts and the render thread are what it measures.
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter BenchRuns \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_BENCH_RUN, the bench string, e.g. "scenario=barrage knobs=fx.deathAbsurd=1 label=a1 out=C:/t/a1.json"
// (its out= outside the checkout, whose untracked files land.py would see).
// It asserts only that the report was written: it is an instrument.
using System.Collections;
using System.IO;
using NUnit.Framework;
using UnityEditor.SceneManagement;
using UnityEngine;
using UnityEngine.TestTools;

namespace TW.Tests
{
    public class BenchRuns
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        const string RunVar = "TW_BENCH_RUN";
        /// <summary>TW.Perf.PerfBench.EnvVar, by name: this assembly does not reference TW.Perf (an asmdef edge is a seam).</summary>
        const string BenchVar = "TW_BENCH";

        static string Args()
        {
            string args = System.Environment.GetEnvironmentVariable(RunVar);
            Assert.That(args, Is.Not.Null.And.Not.Empty, RunVar + " names no bench");
            return args;
        }

        /// <summary>The report's path: the bench string's out= (BenchOptions splits on space, comma and semicolon).</summary>
        static string Out(string args)
        {
            foreach (var token in args.Split(new[] { ' ', ',', ';' }, System.StringSplitOptions.RemoveEmptyEntries))
                if (token.StartsWith("out=")) return token.Substring(4);
            return "perf.json";
        }

        [UnityTest, Explicit("A bench run from batch mode; run by name, with a graphics device.")]
        public IEnumerator TheBenchRuns()
        {
            string before = Args();
            if (File.Exists(Out(before))) File.Delete(Out(before));
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            System.Environment.SetEnvironmentVariable(BenchVar, before);
            yield return new EnterPlayMode();
            // read AFTER entering Play: the domain reloads there, and locals set before it come back as defaults
            string args = Args(), report = Out(args);
            // the bench leaves Play itself when its report is written (quit=1, the default)
            for (int f = 0; f < 60 * 60 * 10 && Application.isPlaying; f++) yield return null;
            if (Application.isPlaying) yield return new ExitPlayMode();
            TestContext.Out.WriteLine($"bench: {(File.Exists(report) ? "wrote " + report : "NO REPORT")} ({args})");
            Assert.IsTrue(File.Exists(report), "the bench wrote no report");
        }
    }
}
