// Phase: deaths (2026-10-19, evidence) — the frog's death flop photographed ALONE and findable, after critic r1 threw
// out frog-death-flop-r2 (12/100: "no frog can be found at the frame centre ... the stills are not frog alone ...
// the 275 px leg motion has no noise floor"). Read DeathStills.cs's header for why this is an [Explicit] EditMode
// test that enters Play itself.
//
// Run it by name WITH a graphics device (not -nographics):
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter FrogFlopStills \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_STILLS_DIR where the stills go (default %TEMP%/tw-frogflop), TW_FROG_BASE=1 films the r2 framing
// (zoom 30, pitch 25) as the probe's noise floor instead of the new take.
//
// What one take shoots, all on one frog staged by DeathLab.FrogAlone on empty ground far from the battle:
//   <t>_p05/p10/p15.png   death +0.5 / +1.0 / +1.5 s, aimed at the body, pitch 60 (a belly-up body foreshortens
//                         away at pitch 25), marked with CaptureRig.MarkFallen so every sidecar says where he is;
//   <t>_frz_b/frz_c.png   the SAME held instant as p15 (CaptureRig.Hold, never released in between): the probe's
//                         noise floor for "the legs moved", which r1 said was missing;
//   <t>_plate.png         that same instant with the body taken away (VATRenderer.ClearFallen): p15 minus the plate
//                         IS the frog's pixels, so the probe measures his area instead of guessing a box.
// The zoom is not typed: take 0 shoots a trial still and reads mark_px_per_m out of its own sidecar, and the zoom is
// scaled from that until a metre is at least 150 px (the 5%-of-an-800x600-crop bar).
// It asserts the take is honest (men_in_frame 0, fallen_in_frame 0, mark_in_frame 1, rain 0, pose_error_m < 0.5) and
// fails the take rather than letting a lying set be committed; judging the pictures is the probe's and the critic's job.
using System.Collections;
using System.Globalization;
using System.IO;
using NUnit.Framework;
using UnityEngine;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using TW.Editor;
using TW.Presentation;
using TW.Presentation.Units;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;
using Unity.Mathematics;
using TW.UI;

namespace TW.Tests
{
    public class FrogFlopStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        const int W = 1920, H = 1080;
        /// <summary>A metre must be at least this many pixels for the frog to fill 5% of an 800x600 crop.</summary>
        const float WantPerM = 150f;

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();
        static VATRenderer Vat => Object.FindFirstObjectByType<VATRenderer>();

        /// <summary>The still and its sidecar from an earlier run taken away before the shot is queued: Drain returns
        /// the moment the file exists, so a stale pair was read as this run's and a take passed on last run's numbers.</summary>
        static void Fresh(string path)
        {
            if (File.Exists(path)) File.Delete(path);
            string json = Path.ChangeExtension(path, ".json");
            if (File.Exists(json)) File.Delete(json);
        }

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        /// <summary>One number out of a sidecar, or NaN when it is not there (no JSON parser in the test assembly).</summary>
        static float Side(string png, string key)
        {
            string json = Path.ChangeExtension(png, ".json");
            if (!File.Exists(json)) return float.NaN;
            string text = File.ReadAllText(json);
            int at = text.IndexOf("\"" + key + "\"");
            if (at < 0) return float.NaN;
            int colon = text.IndexOf(':', at);
            if (colon < 0) return float.NaN;
            int end = colon + 1;
            // the terminators as a string, not as char literals: a closing brace in a char literal unbalances
            // the braces for Tools/checks/brace_balance.py, which strips strings and comments but not chars
            while (end < text.Length && ",\n}".IndexOf(text[end]) < 0) end++;
            return float.TryParse(text.Substring(colon + 1, end - colon - 1).Trim(), NumberStyles.Float, CultureInfo.InvariantCulture, out float v) ? v : float.NaN;
        }

        static void Honest(string png, bool marked)
        {
            Assert.That(File.Exists(png), "no still at " + png);
            Assert.That(Side(png, "men_in_frame"), Is.EqualTo(0f), png + ": a living man shares the frame");
            Assert.That(Side(png, "fallen_in_frame"), Is.EqualTo(0f), png + ": another body shares the frame");
            Assert.That(Side(png, "rain"), Is.LessThan(0.001f), png + ": rain is not pinned off");
            Assert.That(Side(png, "pose_error_m"), Is.LessThan(0.5f), png + ": the camera is not where the shot asked");
            if (marked) Assert.That(Side(png, "mark_in_frame"), Is.EqualTo(1f), png + ": the marked frog is not in the frame");
        }

        /// <summary>Which fallen body the current take staged (VATRenderer's index).</summary>
        int Marked;

        /// <summary>Open ground for a staging: no trench, ladder, wire, bunker or blocked cell (deep water is blocked,
        /// and the first take of this set staged a frog in the sea and photographed flat blue) and no prop within
        /// `pad` metres of (x, z). DeathStills.Open, cut down to one body instead of a row.</summary>
        static bool Open(MapData map, float x, float z, float pad)
        {
            var size = map.SizeMeters;
            for (float zz = z - pad; zz <= z + pad; zz += 2f)
                for (float xx = x - pad; xx <= x + pad; xx += 2f)
                {
                    if (xx < 3f || zz < 3f || xx > size.x - 3f || zz > size.y - 3f) return false;
                    var layer = map.LayerAt(new float3(xx, 0f, zz));
                    if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked | NavLayer.Wire | NavLayer.Bunker)) != 0) return false;
                }
            for (int i = 0; i < map.Props.Length; i++)
            {
                var q = map.Props[i].Pos;
                if (Mathf.Abs(q.x - x) < pad && Mathf.Abs(q.z - z) < pad) return false;
            }
            return true;
        }

        /// <summary>Empty ground, a frog, his death: he is marked once he is a body, and no earlier body is left.</summary>
        IEnumerator Stage(float x, float z)
        {
            Vat.ClearFallen();                       // no body from an earlier take can be in this one's frame
            DeathGags.Pin(1f);
            TestContext.Out.WriteLine($"frog at {x}, {z}: " + DeathLab.FrogAlone(x, z));
            // the body that landed on this patch, whichever index he took: a death elsewhere in the battle can beat
            // him into the list, and marking index 0 blindly would mark a stranger
            for (int f = 0; f < 900 && Vat.FallenCount < 1; f++) yield return null;
            Assert.That(Vat.FallenCount, Is.GreaterThanOrEqualTo(1), $"nothing fell on the patch at {x}, {z}");
            // the body nearest the patch: the list is empty when the take begins, so he is ours, but a spawn is
            // snapped to the nav grid and he does not lie exactly where he was asked for
            int mine = 0; float best = float.MaxValue;
            for (int i = 0; i < Vat.FallenCount; i++)
            {
                var at = Vat.FallenPos(i);
                float d = Vector2.Distance(new Vector2(at.x, at.z), new Vector2(x, z));
                if (d < best) { best = d; mine = i; }
            }
            TestContext.Out.WriteLine($"body {mine} of {Vat.FallenCount}, {best:F1} m from the patch");
            Marked = mine;
            CaptureRig.MarkFallen(mine);
            yield return null;
        }

        [UnityTest, Explicit("The frog's death flop photographed alone; run by name, with a graphics device.")]
        public IEnumerator TheFrogsDeathFlopIsPhotographedAlone()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            Assert.That(Vat, Is.Not.Null, "no VATRenderer");

            // read AFTER entering Play: the domain reloads there and a local set before it comes back as a default
            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-frogflop");
            Directory.CreateDirectory(dir);
            bool baseMode = System.Environment.GetEnvironmentVariable("TW_FROG_BASE") == "1";

            // quiet: nobody deploys, attacks or drops a shell of their own, so the only death is the one we stage
            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            for (int f = 0; f < 120; f++) yield return null;
            // the weather AFTER the scene has settled: a biome profile rewrites Atmosphere.Rain when it is applied
            VfxLab.Rain(0f);
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 120f;
            Time.captureDeltaTime = 1f / 30f;        // the game clock steps a fixed 1/30 s a frame while filming
            var tc = Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
            Assert.That(tc, Is.Not.Null, "no TacticalCamera");
            tc.BaseYaw = -90f;
            if (!baseMode)
            {
                // without this, Pose clamps every zoom up to NearZoom (~15) and the frog can never fill the crop
                tc.ZoomMin = 1.5f; tc.CloseFitHeight = 1.2f;
            }
            float pitch = baseMode ? 25f : 60f;
            // The camera looks 1.5 m above the body, not at it: at pitch 60 and ~190 px/m a body aimed at exactly
            // sits high in the frame (the first set cut him off at the top edge), because what is drawn stands above
            // the point the mark projects. Base mode keeps the tactical camera's own aim, as r2 had it.
            const float AimAbove = 1.5f;

            // open ground far from the battle, a take's own patch each, found in the map rather than typed: the first
            // run of this take staged its frog off the playable ground and photographed flat blue water
            var map = Host.Local.Map;
            var crowd = CaptureRig.Crowd();
            var spots = new System.Collections.Generic.List<Vector2>();
            for (float zz = 20f; zz < map.SizeMeters.y - 20f && spots.Count < 4; zz += 8f)
                for (float xx = 20f; xx < map.SizeMeters.x - 20f && spots.Count < 4; xx += 8f)
                {
                    // far from the fight: at 60 m the wide base framing still caught two other corpses that had
                    // fallen on the patch while the frog was being staged
                    if (Vector2.Distance(new Vector2(xx, zz), crowd) < 120f) continue;
                    bool clear = true;
                    foreach (var u in spots) if (Vector2.Distance(u, new Vector2(xx, zz)) < 16f) clear = false;
                    if (clear && Open(map, xx, zz, 8f)) spots.Add(new Vector2(xx, zz));
                }
            Assert.That(spots.Count, Is.EqualTo(4), "no four patches of open ground away from the battle");
            TestContext.Out.WriteLine("patches: " + string.Join(", ", spots));
            int written = 0;
            float zoom = baseMode ? 30f : 6f;

            if (!baseMode)
            {
                // take 0: one trial still, and the zoom read off its own sidecar instead of typed
                yield return Stage(spots[0].x, spots[0].y);
                for (int attempt = 0; attempt < 5; attempt++)
                {
                    var at = Vat.FallenPos(Marked);
                    string trial = Path.Combine(dir, $"trial_{attempt}.png");
                    Fresh(trial);
                    CaptureRig.Shot(trial, at.x, at.z, zoom, 40f, pitch, W, H, at.y + AimAbove);
                    yield return Drain(trial);
                    float perM = Side(trial, "mark_px_per_m");
                    TestContext.Out.WriteLine($"trial {attempt}: zoom {zoom:F2} -> {perM:F1} px/m");
                    if (!(perM > 1f)) break;
                    if (perM >= WantPerM) break;
                    zoom = Mathf.Max(tc.ZoomMin, zoom * perM / (WantPerM * 1.2f));   // 20% of margin over the bar
                }
            }

            for (int take = 0; take < 3; take++)
            {
                string stem = baseMode ? $"base{take}" : $"take{take}";
                var spot = spots[take + (baseMode ? 0 : 1)];
                yield return Stage(spot.x, spot.y);
                float death = Time.time;
                var body = Vat.FallenPos(Marked);
                float aimY = baseMode ? float.NaN : body.y + AimAbove;

                if (!baseMode)
                {
                    foreach (var shot in new[] { (dt: 0.5f, name: "p05"), (dt: 1.0f, name: "p10") })
                    {
                        while (Time.time < death + shot.dt) yield return null;
                        string path = Path.Combine(dir, $"{stem}_{shot.name}.png");
                        tc.BaseYaw = -90f;
                        Fresh(path);
                        CaptureRig.Shot(path, body.x, body.z, zoom, 40f, pitch, W, H, aimY);
                        yield return Drain(path);
                        Honest(path, true); written++;
                    }
                }
                while (Time.time < death + 1.5f) yield return null;

                // one held instant: p15, two more of the very same pose (the noise floor), then the body taken away
                // (the plate). Never released in between, or the plate would be a different world.
                CaptureRig.Hold(120f);
                string[] frozen = baseMode ? new[] { "still" } : new[] { "p15", "frz_b", "frz_c" };
                foreach (string name in frozen)
                {
                    string path = Path.Combine(dir, $"{stem}_{name}.png");
                    tc.BaseYaw = -90f;
                    Fresh(path);
                    CaptureRig.Shot(path, body.x, body.z, zoom, 40f, pitch, W, H, aimY);
                    yield return Drain(path);
                    Honest(path, true); written++;
                }
                Vat.ClearFallen();
                yield return null;
                string plate = Path.Combine(dir, $"{stem}_plate.png");
                tc.BaseYaw = -90f;
                Fresh(plate);
                CaptureRig.Shot(plate, body.x, body.z, zoom, 40f, pitch, W, H, aimY);
                yield return Drain(plate);
                Honest(plate, false); written++;
                CaptureRig.Release();
                for (int f = 0; f < 30; f++) yield return null;
            }

            CaptureRig.MarkOff();
            DeathLab.Absurd(-1f);
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            Time.captureDeltaTime = 0f;
            yield return new ExitPlayMode();
            TestContext.Out.WriteLine($"wrote {written} stills to {dir} at zoom {zoom.ToString("0.00", CultureInfo.InvariantCulture)}");
            Assert.That(written, Is.GreaterThanOrEqualTo(baseMode ? 6 : 18), "the set is short: a take failed to land");
        }
    }
}
