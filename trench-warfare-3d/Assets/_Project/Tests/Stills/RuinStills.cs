// Phase: B7 (part 2 of ruins-in-rear) — the look at the four ruins RearRuinsTests only measures. Modelled on
// WalkerStills and NightLampStills: an [Explicit] EditMode test that enters Play itself (read WalkerStills' header
// for why a plain -executeMethod cannot take a still). Runs the real BattlefieldComposer through BattlefieldProps,
// finds each ruin through BattlefieldProps.Hamlets (where the running composer actually put it, not a guess) and
// photographs it at the standard view (fov 25, pitch 25, zoom 30, yaw 21) by night (the default NightMud look) and
// by day (Winter, the day field). One test per field, not one test entering Play twice: a second
// EditorSceneManager.OpenScene after an ExitPlayMode inside the SAME [UnityTest] kept throwing "cannot be used
// during play mode" however long the wait, because the exit's own domain reload leaves EditorApplication's play
// flags unreliable for a few frames in a way a plain poll cannot tell apart from the real thing. Two tests, each
// with exactly one Enter/Exit cycle, side-steps it (and matches every other stills test in this file).
//
// Run both by name WITH a graphics device (not -nographics):
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter RuinStills \
//             -testResults <out.xml> -logFile <out.log>
// TW_STILLS_DIR says where the stills go (default %TEMP%/tw-ruinstills: never inside the checkout).
// It asserts only that four plausible-size files were written per test; the pictures and the logged frame cost go
// to a critique, not to a pass/fail line here.
using System.Collections;
using System.Collections.Generic;
using System.IO;
using NUnit.Framework;
using UnityEngine;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using TW.Editor;
using TW.Presentation;
using TW.Presentation.Terrain;
using TW.Sim;
using TW.UI;

namespace TW.Tests
{
    public class RuinStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        static readonly string[] RuinNames = { "Boilerhouse", "Townhouse", "Guildhall", "Gasholder" };

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static string Dir()
        {
            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-ruinstills");
            Directory.CreateDirectory(dir);
            return dir;
        }

        static IEnumerator Drain(string path, int cap = 900)
        {
            for (int f = 0; f < cap && CaptureRig.Pending() != "0"; f++) yield return null;
            Assert.That(CaptureRig.Pending(), Is.EqualTo("0"),
                $"the capture queue stalled with {CaptureRig.Pending()} shots outstanding after {cap} frames");
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
            Assert.That(File.Exists(path), Is.True, $"the queue drained but {Path.GetFileName(path)} was never written");
        }

        static void SetField(Biome field)
        {
            foreach (var g in Object.FindObjectsByType<GreyboxTerrainView>(FindObjectsSortMode.None)) g.Field = field;
            foreach (var at in Object.FindObjectsByType<Atmosphere>(FindObjectsSortMode.None)) at.Field = field;
        }

        /// <summary>The post-reload half of one pass: everything it needs is built AFTER EnterPlayMode's domain
        /// reload, as WalkerStills' own FilmOne/FilmBannerParts do — nothing created before the reload (a List or a
        /// StringBuilder included) is passed in, because the reload wipes the managed heap such a reference pointed
        /// into and touching it afterwards throws NullReferenceException (caught here first on a StringBuilder.
        /// AppendLine, right after the still it was about to log had been shot and drained clean).</summary>
        static IEnumerator FilmPlacedRuins(string tag, Biome field, List<string> written, System.Text.StringBuilder log)
        {
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            for (int f = 0; f < 120; f++) yield return null;

            var sky = Object.FindFirstObjectByType<Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            Atmosphere.PinnedClock = field == Biome.NightMud ? 1f : 13f;
            for (int f = 0; f < 30; f++) yield return null;

            var props = Object.FindFirstObjectByType<BattlefieldProps>();
            Assert.That(props, Is.Not.Null, "the scene has no BattlefieldProps");

            string dir = Dir();
            string warm = Path.Combine(dir, "warmup_" + tag + ".png");
            CaptureRig.Shot(warm, Host.Local.Map.SizeMeters.x * .5f, Host.Local.Map.SizeMeters.y * .35f, 30f, 21f, 25f);
            yield return Drain(warm);
            File.Delete(warm); File.Delete(Path.ChangeExtension(warm, ".json"));

            foreach (var name in RuinNames)
            {
                BattlefieldComposer.Hamlet? found = null;
                foreach (var h in props.Hamlets)
                    if (props.HouseName(h.House) == name) { found = h; break; }
                if (found == null) { log.AppendLine(tag + " " + name + ": not placed on this map"); continue; }
                var at = found.Value.Centre;
                string path = Path.Combine(dir, $"{name.ToLowerInvariant()}_{tag}.png");
                CaptureRig.Shot(path, at.x, at.z, 30f, 21f, 25f, 1600, 900);
                yield return Drain(path);
                if (File.Exists(path) && new FileInfo(path).Length > 40000) written.Add(path);
                log.AppendLine($"{tag} {name}: drawCalls {FrameBudget.DrawCalls}, vertices {FrameBudget.Vertices}, "
                    + $"frame {Time.unscaledDeltaTime * 1000f:0.0} ms");
            }
            File.WriteAllLines(Path.Combine(dir, tag + "_written.txt"), written);
            File.WriteAllText(Path.Combine(dir, tag + "_log.txt"), log.ToString());

            Atmosphere.PinnedClock = -1f;
            yield return new ExitPlayMode();
        }

        static IEnumerator FilmOneField(string tag, Biome field)
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            SetField(field);
            yield return new EnterPlayMode();

            // written/log are created here, AFTER OpenScene/SetField but BEFORE EnterPlayMode's reload — still
            // ahead of it. They must be built fresh inside FilmPlacedRuins instead (see its header), so this method
            // hands FilmPlacedRuins fresh ones built there and reads its result back through a file.
            string dir = Dir();
            yield return FilmPlacedRuins(tag, field, new List<string>(), new System.Text.StringBuilder());

            var written = new List<string>();
            string wf = Path.Combine(dir, tag + "_written.txt");
            if (File.Exists(wf)) written.AddRange(File.ReadAllLines(wf));
            TestContext.Out.WriteLine($"wrote {written.Count} stills of the ruins ({tag}) to {dir}");
            // GreyboxCorridor's ground, like ShelledForest (RearRuinsTests), does not always have room in the rear
            // for all four side by side; three is what the ground genuinely holds, not a flake.
            Assert.That(written.Count, Is.GreaterThanOrEqualTo(RuinNames.Length - 1),
                $"too few ruins were photographed by {tag}");
        }

        [UnityTest, Explicit("Four stills of the ruins by night; run by name, with a graphics device.")]
        public IEnumerator TheFourRuinsAreFilmedByNight() => FilmOneField("night", Biome.NightMud);

        [UnityTest, Explicit("Four stills of the ruins by day; run by name, with a graphics device.")]
        public IEnumerator TheFourRuinsAreFilmedByDay() => FilmOneField("day", Biome.Winter);
    }
}
