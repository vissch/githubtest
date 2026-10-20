// Phase: deaths (2026-10-20, evidence) — the frog's death flop photographed ALONE, at the framing a player actually
// sees, every shot taken with the clock stopped. Critic r1 gave frog-death-flop-r3 24/100: "at the player framing
// (zoom 30, pitch 25) nothing reads as a frog at its mark; the area floor is taken at zoom 30 against takes at 4.66;
// motion counts weather and exposure change (wind 8.3 vs 3.0)". Read DeathStills.cs's header for why this is an
// [Explicit] EditMode test that enters Play itself.
//
// Run it by name WITH a graphics device (not -nographics):
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter FrogFlopStills \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_STILLS_DIR where the stills go (default %TEMP%/tw-frogflop).
//
// ONE framing for everything, and it is the player's: zoom 30, pitch 25, 1920x1080, the tactical camera's own aim,
// TacticalCamera.ZoomMin and CloseFitHeight untouched. r3's zoom trial drove the takes to 4.66 and then compared
// their area against a floor shot at 30 — a comparison of two different pictures. There is no trial here and no
// second framing: every still of every take, frog and plate alike, comes out of the same camera, so the probe can
// subtract one from another and what is left is the change and nothing else.
//
// What one take shoots, all on one frog staged by DeathLab.FrogAlone on empty ground far from the battle, each still
// inside its OWN CaptureRig.Hold(120f) / Release():
//   <t>_m0.png / <t>_m1.png     death +1.0 s and +2.0 s — a second apart, both inside the 2.6 s twitch whose
//                               one-second read FrogDeathTests.TheTwitchReadsASecondApart guards. The motion pair.
//   <t>_i0.png / <t>_i1.png     death +4.0 s and +5.0 s — the twitch is over: the same camera, the same second's
//                               gap, nothing moving. The idle pair IS the noise floor for the motion pair.
//   <t>_plate.png               still inside t_i1's hold, the body taken away (VATRenderer.ClearFallen): i1 minus
//   <t>_plate2.png              the plate IS the frog's pixels. A second plate in the same hold is the floor for
//                               that subtraction: frog-free ground against frog-free ground, same instant.
// Why Hold for all of them: r3's live shots read wind 7.4-8.3 and its held ones 2.98 (take1_p05.json vs
// take1_p15.json), so the blast gust rode in the live frames only and every live-versus-held difference counted the
// weather. CaptureRig.Hold stops the clock and pins the weather clock, and r3's three held shots of one pose
// differed by exactly 0 px — that is the instrument the critic asked for.
//
// It asserts every take is honest (men_in_frame 0, fallen_in_frame 0, mark_in_frame 1, rain 0, pose_error_m < 0.5)
// and fails the take rather than letting a lying set be committed; judging the pictures is probe.py's and the
// critic's job.
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
        /// <summary>The player's framing, not a framing chosen to flatter the frog.</summary>
        const float Zoom = 30f, Pitch = 25f, Yaw = 40f;

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
            Assert.That(Side(png, "zoom"), Is.EqualTo(Zoom), png + ": not the player's zoom");
            Assert.That(Side(png, "pitch"), Is.EqualTo(Pitch), png + ": not the player's pitch");
            if (marked) Assert.That(Side(png, "mark_in_frame"), Is.EqualTo(1f), png + ": the marked frog is not in the frame");
        }

        /// <summary>Which fallen body the current take staged (VATRenderer's index).</summary>
        int Marked;

        /// <summary>
        /// Where the battle is, taken from the map and not from the sim: the middle of the corridor. Two different
        /// reads of the living men threw a NullReferenceException here on four runs of this batch — CaptureRig.Crowd
        /// (through SimHost.Presenter) and a direct read of the sim world's Position array alike, and a try/catch
        /// around the direct read did not catch it, so whatever is null is not reachable from this test's code at
        /// this moment in a batch-mode Play. It is not worth a blocked leg: this number only chooses WHERE to stage a
        /// frog, GreyboxCorridor puts its two lines either side of the middle of its 90 x 276 m corridor by
        /// construction, and Honest() is what makes a take honest — it fails any still whose sidecar counts a living
        /// man in the frame, however the patch was picked.
        /// </summary>
        static Vector2 Battle(MapData map) => new Vector2(map.SizeMeters.x * 0.5f, map.SizeMeters.y * 0.5f);

        /// <summary>A line straight to disk. TestContext.Out is buffered and everything written after the last
        /// flush is lost when the coroutine throws, so four runs of this capture reported an exception with no trace
        /// of how far the take had got. A file append is on disk before the next statement runs.</summary>
        static string TraceDir;
        static void Mark(string text)
        {
            if (!string.IsNullOrEmpty(TraceDir)) File.AppendAllText(Path.Combine(TraceDir, "trace.txt"), text + System.Environment.NewLine);
            TestContext.Out.WriteLine(text);
        }

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
            Mark($"frog at {x}, {z}: " + DeathLab.FrogAlone(x, z));
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
            Mark($"body {mine} of {Vat.FallenCount}, {best:F1} m from the patch");
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
            TraceDir = dir;
            Mark("--- run begins");

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
            // ZoomMin and CloseFitHeight are deliberately NOT touched, and aimY stays NaN (the tactical camera's own
            // aim): a still a player could not get is no evidence about what a player sees.

            // open ground far from the battle, a take's own patch each, found in the map rather than typed: the first
            // run of this take staged its frog off the playable ground and photographed flat blue water
            var map = Host.Local.Map;
            Mark($"map {map.SizeMeters.x:F0} x {map.SizeMeters.y:F0} m, {map.Props.Length} props");
            var crowd = Battle(map);
            Mark($"battle taken as the corridor centre {crowd.x:F0}, {crowd.y:F0}");
            // The FARTHEST open patches, not the first ones found. At zoom 30 the frame is 1920 px / 31.36 px per m =
            // 61 m across, so the first run of this framing caught a living man walking into take2's frame from a
            // battle 120 m away: the wide player framing sees eight times the ground r3's zoom 4.66 did. GreyboxCorridor
            // is 90 x 276 m, so there is no corner of it a fixed distance bar can reach — a hard 120 m floor on a 90 m
            // wide map is a bar the map cannot clear. Instead EVERY open candidate is collected, sorted by how far it
            // lies from the thickest knot of men, and the three used are simply the most remote the map has, kept 40 m
            // apart from each other (Stage clears the fallen list, so no neighbouring take's body can share a frame;
            // the living are what distance has to buy). Honest() is still the guard: a man in the frame fails the take.
            var candidates = new System.Collections.Generic.List<Vector2>();
            try
            {
                for (float zz = 20f; zz < map.SizeMeters.y - 20f; zz += 4f)
                    for (float xx = 20f; xx < map.SizeMeters.x - 20f; xx += 4f)
                        if (Open(map, xx, zz, 8f)) candidates.Add(new Vector2(xx, zz));
            }
            catch (System.Exception e) { TestContext.Out.WriteLine("SCAN THREW: " + e); throw; }
            Mark($"{candidates.Count} open candidates");
            candidates.Sort((a, b) => Vector2.Distance(b, crowd).CompareTo(Vector2.Distance(a, crowd)));
            var spots = new System.Collections.Generic.List<Vector2>();
            foreach (var c in candidates)
            {
                if (spots.Count >= 3) break;
                bool clear = true;
                foreach (var u in spots) if (Vector2.Distance(u, c) < 40f) clear = false;
                if (clear) spots.Add(c);
            }
            Assert.That(spots.Count, Is.EqualTo(3), "no three remote patches of open ground on this map");
            foreach (var s in spots) Mark($"patch {s.x:F0}, {s.y:F0} is {Vector2.Distance(s, crowd):F0} m from the battle");
            TestContext.Out.WriteLine("patches: " + string.Join(", ", spots));
            int written = 0;

            for (int take = 0; take < 3; take++)
            {
                string stem = $"take{take}";
                var spot = spots[take];
                Mark($"take {take} staging at {spot.x:F0}, {spot.y:F0}");
                yield return Stage(spot.x, spot.y);
                float death = Time.time;
                Mark($"take {take} staged, Vat is {(Vat == null ? "NULL" : "there")}, Marked {Marked}");
                var body = Vat.FallenPos(Marked);
                Mark($"take {take} body at {body.x:F1}, {body.y:F1}, {body.z:F1}");

                // the four poses, each one photographed with the clock stopped and let go again: m0/m1 a second apart
                // inside the twitch, i0/i1 a second apart after it. Same camera, same weather clock, four times.
                foreach (var shot in new[] { (dt: 1.0f, name: "m0"), (dt: 2.0f, name: "m1"), (dt: 4.0f, name: "i0") })
                {
                    while (Time.time < death + shot.dt) yield return null;
                    CaptureRig.Hold(120f);
                    string path = Path.Combine(dir, $"{stem}_{shot.name}.png");
                    tc.BaseYaw = -90f;
                    Fresh(path);
                    Mark($"shooting {shot.name}");
                    CaptureRig.Shot(path, body.x, body.z, Zoom, Yaw, Pitch, W, H);
                    yield return Drain(path);
                    Honest(path, true); written++;
                    CaptureRig.Release();
                    for (int f = 0; f < 5; f++) yield return null;
                }

                // i1 and, without ever releasing, the two plates: the body is taken away inside the same held instant,
                // so i1 minus plate is the frog and plate minus plate2 is what that subtraction costs on bare ground.
                while (Time.time < death + 5.0f) yield return null;
                CaptureRig.Hold(120f);
                string i1 = Path.Combine(dir, $"{stem}_i1.png");
                tc.BaseYaw = -90f;
                Fresh(i1);
                CaptureRig.Shot(i1, body.x, body.z, Zoom, Yaw, Pitch, W, H);
                yield return Drain(i1);
                Honest(i1, true); written++;

                Vat.ClearFallen();
                yield return null;
                foreach (string name in new[] { "plate", "plate2" })
                {
                    string path = Path.Combine(dir, $"{stem}_{name}.png");
                    tc.BaseYaw = -90f;
                    Fresh(path);
                    CaptureRig.Shot(path, body.x, body.z, Zoom, Yaw, Pitch, W, H);
                    yield return Drain(path);
                    Honest(path, false); written++;
                }
                CaptureRig.Release();
                for (int f = 0; f < 30; f++) yield return null;
            }

            CaptureRig.MarkOff();
            DeathLab.Absurd(-1f);
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            Time.captureDeltaTime = 0f;
            yield return new ExitPlayMode();
            TestContext.Out.WriteLine($"wrote {written} stills to {dir} at zoom {Zoom.ToString("0.00", CultureInfo.InvariantCulture)}");
            Assert.That(written, Is.GreaterThanOrEqualTo(15), "the set is short: a take failed to land");
        }
    }
}
