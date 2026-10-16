// Phase: readability (2026-10-16, tooling) — the owner's close-range reshoot, as a test. He looked at all 38 units at
// the game's own close zoom by hand and found 14 of 76 stills where "men barely separate from the ground": dark olive
// lumps in a near-black trench while the wet mud beside them carries bright specular sheets. That measurement could
// not be rerun, so this test IS the proof: it photographs every unit of ProvingGround.Catalogue() at zoom 9, pitch 25,
// yaws 21 and 201, and fails naming every still where a man is in frame and contrast_median is under 0.25 or the
// sidecar carries a warning. At the commit that added it, it is red on about 14 stills — that is the baseline.
//
// Run it by name WITH a graphics device (not -nographics), the way WreckStills is run:
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter UnitCloseStills \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_STILLS_DIR where the stills go (default %TEMP%/tw-unitclose: never inside the checkout).
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using NUnit.Framework;
using UnityEngine;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using TW.Editor;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;
using TW.UI;

namespace TW.Tests
{
    public class UnitCloseStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        const float Bar = 0.25f;          // the owner's bar on a still with a man in it
        const float Zoom = 9f, Pitch = 25f;
        static readonly float[] Yaws = { 21f, 201f };
        static readonly string[] YawTags = { "a", "b" };
        const int MenPerUnit = 6;

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        /// <summary>Lets the sim run for a few real frames and then pins the clock and the weather again: Hold stops
        /// time dead, and a man spawned under a stopped clock is still drawn blending in from where he was.</summary>
        static IEnumerator Settle(int frames)
        {
            Time.timeScale = 1f;
            for (int f = 0; f < frames; f++) yield return null;
            CaptureRig.Hold(120f);
        }

        static string Safe(string name)
        {
            var sb = new System.Text.StringBuilder();
            foreach (char c in name ?? "") sb.Append(char.IsLetterOrDigit(c) ? c : '-');
            return sb.ToString().Trim('-');
        }

        static float Num(string json, string key)
        {
            int i = json.IndexOf("\"" + key + "\"");
            if (i < 0) return float.NaN;
            i = json.IndexOf(':', i) + 1;
            int end = i;
            // no char literal for the closing brace here: the brace-balance check counts braces outside strings
            while (end < json.Length && json[end] != ',' && json[end] != '\n') end++;
            return float.TryParse(json.Substring(i, end - i).Trim(), NumberStyles.Float, CultureInfo.InvariantCulture, out float v) ? v : float.NaN;
        }

        /// <summary>The warnings list of a sidecar, as written text ("" when it is empty).</summary>
        static string Warnings(string json)
        {
            int i = json.IndexOf("\"warnings\"");
            if (i < 0) return "";
            int a = json.IndexOf('[', i), b = json.IndexOf(']', a);
            return a < 0 || b < 0 ? "" : json.Substring(a + 1, b - a - 1).Trim();
        }

        [UnityTest, Explicit("Every unit photographed at close range and judged on figure/ground contrast; run by name, with a graphics device.")]
        public IEnumerator EveryUnitReadsAtCloseRange()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            yield return new EnterPlayMode();
            // everything after the reload is a fresh enumerator: locals captured before the domain reload come back null
            yield return InPlay();
        }

        static IEnumerator InPlay()
        {
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-unitclose");
            Directory.CreateDirectory(dir);

            var host = Host;
            host.ScriptedPeer = false; host.PeerAttacks = false;
            host.WriteWorlds(m => { var b = m.World.GetSystem<AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            // the clock and the weather pinned first, so two runs of this test differ by the change alone
            TestContext.Out.WriteLine(CaptureRig.Hold(120f));
            yield return Settle(120);

            var units = ProvingGround.Catalogue(host.Local.World);
            TestContext.Out.WriteLine(units.Count + " units in the catalogue");
            var lines = new List<string>();
            var offenders = new List<string>();

            for (int u = 0; u < units.Count; u++)
            {
                var unit = units[u];
                // clear the field first: 38 units left standing pile into one knot, ShotCrowd aims at the wrong square
                // and every later still frames the wrong army
                host.WriteWorlds(m => { var w = m.World; for (int s = 0; s < w.HighWater; s++) if (w.IsAlive(s)) w.Despawn(s); });
                yield return Settle(20);
                int placed = 0;
                host.WriteWorlds(m => placed = ProvingGround.Place(m, 0, unit.Archetype, MenPerUnit));
                yield return Settle(60);
                int alive = 0;
                { var w = host.Local.World; for (int s = 0; s < w.HighWater; s++) if (w.IsAlive(s)) alive++; }
                TestContext.Out.WriteLine($"{u:00} {unit.Name}: placed {placed}, alive on the field {alive}");

                for (int y = 0; y < Yaws.Length; y++)
                {
                    string stem = $"{u:00}-{Safe(unit.Name)}-{YawTags[y]}";
                    string path = Path.Combine(dir, stem + ".png");
                    TestContext.Out.WriteLine(CaptureRig.ShotCrowd(path, Zoom, Yaws[y], Pitch));
                    yield return Drain(path);
                    string side = Path.ChangeExtension(path, ".json");
                    if (!File.Exists(side)) { offenders.Add(stem + ": no sidecar written"); continue; }
                    string json = File.ReadAllText(side);
                    float men = Num(json, "men_in_frame"), med = Num(json, "contrast_median"), p10 = Num(json, "contrast_p10");
                    string warn = Warnings(json);
                    lines.Add($"{stem}\tmen_in_frame {men:0}\tcontrast_median {med:0.0000}\tcontrast_p10 {p10:0.0000}\twarnings {(warn.Length > 0 ? warn : "-")}");
                    if (men >= 1f && (med < Bar || warn.Length > 0))
                        offenders.Add($"{stem}: men {men:0}, contrast_median {med:0.0000}, warnings {(warn.Length > 0 ? warn : "-")}");
                }
            }

            File.WriteAllText(Path.Combine(dir, "info.txt"), string.Join("\n", lines) + "\n");
            foreach (var l in lines) TestContext.Out.WriteLine(l);
            CaptureRig.Release();
            TestContext.Out.WriteLine(lines.Count + " stills in " + dir + ", " + offenders.Count + " below the bar");
            Assert.That(offenders, Is.Empty, "stills where a man is in frame and he does not read against the ground (bar: contrast_median >= "
                        + Bar.ToString("0.00", CultureInfo.InvariantCulture) + ", no warning):\n" + string.Join("\n", offenders));
            yield return new ExitPlayMode();
        }
    }
}
