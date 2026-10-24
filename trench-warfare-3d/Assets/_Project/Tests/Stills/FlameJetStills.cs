// Phase: VFX pass (owner, 2026-10-19) - the capture harness for the flamethrower jet's near third.
//
// Why this file exists. The first try of flame-jet-opaque-near could not be judged: three captures of ONE build
// measured near_edge_corr 0.157, 0.170 and 0.211 and core_over_tip_luma 1.448, 1.609 and 1.366, a spread wider than
// the distance left to either bar. The cause was the clock. Every valve ramp, Perlin and Mathf.Repeat phase in the
// stream was sampled at absolute Time.time, which a session cannot rewind: a fresh play session already reads some
// 80 s before a burst can be fired, and a paused editor's stepped frames carry a large, variable deltaTime, so no
// two captures ever looked at the same moment of the same stream.
//
// Flamethrower.PinnedNow fixes that. With it set, the burst's birth and its age are the numbers written here and
// nothing else: born at 100 s and sampled at 101.3, 101.5 or 101.7 s, so the age is a number written here, the valve
// is exactly Clamp01(age/0.10) * Clamp01(age/0.14) = 1, and every phase in the drawing is a function of that sample
// time alone.
// The world is held (CaptureRig.Hold: clock stopped, weather pinned at 120 s) and the dice are reseeded, so the
// sky, the ground and the jet's own Seed repeat too.
//
// One still per session, chosen by TW_FLAME_CAP, so three captures measure repeatability ACROSS sessions rather
// than one image copied three times, and so no card from an earlier burst can be left on screen.
//
// The three stills are three AGES of that pinned clock (1.3, 1.5 and 1.7 s), not one age shot three times. With a
// single const age the pinned clock made all three sessions render byte-identical pngs, and the spread they were
// meant to measure was 0 by construction (the owner, 2026-10-24: "edge_spread 0 is that one frame counted three
// times"). The burst now lasts a fixed 3 s, so every age is wide of both valve ramps and of the play-out threshold.
//
// Run it by name WITH a graphics device (not -nographics), once per still:
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter FlameJetStills \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_FLAME_CAP  which still (1, 2, 3 or plate; plate fires no burst) - default 1,
//              TW_STILLS_DIR where the stills go (default %TEMP%/tw-flamejetstills: never inside the checkout),
//              TW_FLAME_SETTLE ticks to settle before the burst (300).
using System.Collections;
using System.Globalization;
using System.IO;
using NUnit.Framework;
using UnityEngine;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using TW.Editor;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Presentation.Terrain;
using TW.Sim;
using TW.UI;

namespace TW.Tests
{
    public class FlameJetStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        static readonly CultureInfo Inv = CultureInfo.InvariantCulture;

        // critic r1's own pose: GreyboxCorridor at night, T2, focus on the nozzle, the stream across the frame
        const float FocusX = 100f, FocusZ = 104f, Zoom = 16f, Yaw = 0f, Pitch = 25f;
        const int W = 1600, H = 900;

        // the pinned clock: born at Birth, photographed Age seconds later, the burst lasting Seconds. Three ages,
        // one per capture (TW_FLAME_CAP 1, 2, 3; the plate keeps the middle one), so three sessions photograph three
        // different moments of the SAME pinned stream instead of one frame counted three times. Every age clears
        // both valve ramps (0.10 s and 0.14 s) and every `left` (1.7 / 1.5 / 1.3 s) clears the play-out threshold
        // (left < (Last - Hold) / 12f, 0.833 s here), so the valve is exactly 1 in all three and only the Perlin and
        // Mathf.Repeat phases - functions of PinnedNow alone - differ.
        const float Birth = 100f, Seconds = 3f;
        static readonly float[] Ages = { 1.3f, 1.5f, 1.7f };
        static float AgeFor(string which) => which == "2" ? Ages[1] : which == "3" ? Ages[2] : which == "1" ? Ages[0] : Ages[1];
        const int Seed = 20250925;

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static string Env(string k, string d) { string v = System.Environment.GetEnvironmentVariable(k); return string.IsNullOrEmpty(v) ? d : v; }

        static string Dir()
        {
            string dir = Env("TW_STILLS_DIR", Path.Combine(Path.GetTempPath(), "tw-flamejetstills"));
            Directory.CreateDirectory(dir);
            return dir;
        }

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        // EnterPlayMode is yielded straight from the [UnityTest] method: a level of enumerator nesting round it
        // breaks Unity's play-mode-state detection (TracerStills carries the same note and the scar to go with it).
        [UnityTest, Explicit("One flame-jet still on a pinned clock; run by name, with a graphics device. TW_FLAME_CAP picks which.")]
        public IEnumerator FlameJet_IsCaptured()
        {
            Setup();
            yield return new EnterPlayMode();
            yield return InPlay();
        }

        static void Setup()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            foreach (var g in Object.FindObjectsByType<GreyboxTerrainView>(FindObjectsSortMode.None)) g.Field = Biome.NightMud;
            foreach (var at in Object.FindObjectsByType<Atmosphere>(FindObjectsSortMode.None)) at.Field = Biome.NightMud;
            Assert.That(Host, Is.Not.Null, "no SimHost in the scene");
        }

        static IEnumerator InPlay()
        {
            string which = Env("TW_FLAME_CAP", "1");
            bool plate = which == "plate";
            float age = AgeFor(which);
            string dir = Dir();
            string name = plate ? "bg-t2.png" : "jet-t2-" + which + ".png";
            string path = Path.Combine(dir, name);
            var info = new System.Text.StringBuilder();
            void Say(string s) { info.AppendLine(s); File.WriteAllText(Path.Combine(dir, "info." + which + ".txt"), info.ToString()); Debug.Log("[flamejetstills] " + s); }

            for (int f = 0; f < 2000 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            CombatFx.ShowOverlays = false;

            var sky = Object.FindFirstObjectByType<Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            Time.captureDeltaTime = 1f / 30f;

            try
            {
                int settle = int.Parse(Env("TW_FLAME_SETTLE", "300"), Inv);
                Host.TimeScale = 8f;
                while (Host.Local.World.Tick < (uint)settle) yield return null;
                Host.TimeScale = 0f;
                Say($"capture {which} (age {age:0.##} s): settled at tick {Host.Local.World.Tick}, seed {Host.Seed}, battlefield seed {Host.BattlefieldSeed}");

                Say(CaptureRig.Hold(120f));            // clock stopped, weather pinned: one sky for all four stills
                for (int f = 0; f < 4; f++) yield return null;

                Random.InitState(Seed);
                var flames = Flamethrower.Active;
                Assert.That(flames, Is.Not.Null, "no CombatFx in the scene, so no Flamethrower to fire");
                flames.Clear();

                if (!plate)
                {
                    // the nozzle: a man's weapon height over the drawn ground at the focus point
                    float ground = RenderGround.Sample(Host.Local.Map, FocusX, FocusZ);
                    var nozzle = new Vector3(FocusX, ground + 1.1f, FocusZ);
                    // along screen-right, flattened: the probe reads the nozzle as the fire mask's left edge and the
                    // tip as its right, so the stream has to run across the frame and not into it
                    var cam = Camera.main;
                    Vector3 aim = cam != null
                        ? Vector3.ProjectOnPlane(cam.transform.right, Vector3.up).normalized
                        : Vector3.right;
                    Flamethrower.PinnedNow = Birth;
                    flames.Burst(-1, nozzle, aim, Vector3.zero, Seconds);   // slot -1: a fixed nozzle, riding nobody
                    Say($"burst born at pinned {Birth:0.##} s from {nozzle.x:0.00},{nozzle.y:0.00},{nozzle.z:0.00} " +
                        $"aim {aim.x:0.000},{aim.y:0.000},{aim.z:0.000} for {Seconds:0.##} s, jets {flames.Jets}");
                    Flamethrower.PinnedNow = Birth + age;
                    for (int f = 0; f < 2; f++) yield return null;           // one frame to lay the chain, one to draw it
                    Say($"sampled at pinned {Birth + age:0.##} s: age {age:0.##} s, left {Seconds - age:0.##} s, valve 1, jets {flames.Jets}");
                }
                else
                {
                    Flamethrower.PinnedNow = Birth + age;                   // the same pinned clock, no fire
                    for (int f = 0; f < 2; f++) yield return null;
                    Say($"the plate: the same pose and the same pinned clock at age {age:0.##} s, no burst");
                }

                if (File.Exists(path)) File.Delete(path);
                Say(CaptureRig.Shot(path, FocusX, FocusZ, Zoom, Yaw, Pitch, W, H));
                yield return Drain(path);
                Assert.That(File.Exists(path), Is.True, "the still was not written: " + path);
                Say($"wrote {name}");
            }
            finally
            {
                Flamethrower.PinnedNow = -1f;
                Time.captureDeltaTime = 0f;
                CaptureRig.Release();
            }
            yield return new ExitPlayMode();
        }
    }
}
