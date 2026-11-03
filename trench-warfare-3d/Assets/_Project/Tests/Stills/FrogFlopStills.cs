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
// Every still is checked honest (men_in_frame 0, fallen_in_frame 0, mark_in_frame 1, rain 0, pose_error_m < 0.5)
// and a take with one dishonest still is shot again on the next remote patch rather than kept; judging the pictures
// is probe.py's and the critic's job.
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

        /// <summary>The same honesty test as before, as a verdict instead of an assertion: a take spoiled by a living
        /// man who walked into the frame is re-shot on the next patch (the player framing is 61 m wide, so no patch of
        /// a 90 x 276 m map is safe from a man who keeps walking), while a dishonest still is still never kept. The
        /// reason goes to the trace.</summary>
        static bool Clean(string png, bool marked)
        {
            string why = null;
            if (!File.Exists(png)) why = "no still";
            else if (Side(png, "men_in_frame") != 0f) why = "a living man shares the frame";
            else if (Side(png, "fallen_in_frame") != 0f) why = "another body shares the frame";
            else if (!(Side(png, "rain") < 0.001f)) why = "rain is not pinned off";
            else if (!(Side(png, "pose_error_m") < 0.5f)) why = "the camera is not where the shot asked";
            else if (Side(png, "zoom") != Zoom) why = "not the player's zoom";
            else if (Side(png, "pitch") != Pitch) why = "not the player's pitch";
            else if (marked && Side(png, "mark_in_frame") != 1f) why = "the marked frog is not in the frame";
            if (why != null) Mark(Path.GetFileName(png) + " is not honest: " + why);
            return why == null;
        }

        /// <summary>Where every living man is standing right now, read the way CaptureRig.Crowd reads them (the live
        /// flag and the drawn position). A static method on purpose: see the note on the Sort below.</summary>
        static System.Collections.Generic.List<Vector2> Living()
        {
            var list = new System.Collections.Generic.List<Vector2>();
            var host = Object.FindFirstObjectByType<SimHost>();
            if (host == null || host.Local == null) return list;
            var w = host.Local.World;
            for (int i = 0; i < w.HighWater; i++)
            {
                if ((w.Flags[i] & 1u) == 0) continue;
                var q = (Vector3)host.Presenter.Drawn(i);
                list.Add(new Vector2(q.x, q.z));
            }
            return list;
        }

        /// <summary>The open patch with the most empty ground around it: of every candidate at least 40 m from a patch
        /// already used or already spoiled, the one whose NEAREST living man is farthest away. Chosen fresh for each
        /// take, because the men walk. The run of 2026-11-03 chose by distance from the corridor's centre instead and
        /// lost five takes to men in the frame: on GreyboxCorridor the two sides spawn at the ENDS and march inward,
        /// so "far from the middle" is exactly where the living are.</summary>
        static Vector2 BestPatch(System.Collections.Generic.List<Vector2> open, System.Collections.Generic.List<Vector2> avoid,
                                 System.Collections.Generic.List<Vector2> men, out float clearance)
        {
            var best = Vector2.zero; float bestClear = -1f;
            for (int i = 0; i < open.Count; i++)
            {
                var c = open[i];
                bool blocked = false;
                for (int j = 0; j < avoid.Count; j++) if (Vector2.Distance(avoid[j], c) < 40f) blocked = true;
                if (blocked) continue;
                float near = float.MaxValue;
                for (int j = 0; j < men.Count; j++) near = Mathf.Min(near, Vector2.Distance(men[j], c));
                if (near > bestClear) { bestClear = near; best = c; }
            }
            clearance = bestClear;
            return best;
        }

        /// <summary>How far (x, z) is from the segment body -> camera, in metres on the ground plane. Step 5's
        /// instrument: take2 of the previous run measured 0 body pixels because a vehicle hull stood on this line,
        /// and a take whose line of sight is blocked is cheaper to drop before its other five stills than after.</summary>
        static float ToSegment(Vector2 a, Vector2 b, Vector2 q)
        {
            var ab = b - a;
            float len2 = ab.sqrMagnitude;
            float t = len2 < 1e-4f ? 0f : Mathf.Clamp01(Vector2.Dot(q - a, ab) / len2);
            return Vector2.Distance(a + ab * t, q);
        }

        /// <summary>Nothing between the body and the camera: no living man (Living() includes the men in vehicle
        /// slots, which is what put a hull in front of take2) and no prop within `pad` metres of the sight line. The
        /// camera is read out of the take's own first sidecar, so it is where the shot really ended up, not where it
        /// was asked for.</summary>
        static bool Sighted(MapData map, Vector2 bodyXZ, string firstPng, float pad)
        {
            float cx = Side(firstPng, "cam_x"), cz = Side(firstPng, "cam_z");
            if (float.IsNaN(cx) || float.IsNaN(cz)) { Mark("no cam_x/cam_z in " + Path.GetFileName(firstPng) + ": the sight line cannot be checked"); return true; }
            // only the NEAR stretch of the sight line, the first NearM metres out from the body: that is where a
            // hull or a man can hide a body 1.2 m long (what cost the previous run take2). The whole 60 m line to a
            // zoom-30 camera flags men who are nowhere near the body, and the honest witness for those is the render
            // itself (men_in_frame in Clean), not this arithmetic: the first run of this guard spoiled six takes on it.
            const float NearM = 15f;
            var away = new Vector2(cx, cz) - bodyXZ;
            var cam = bodyXZ + (away.magnitude > NearM ? away.normalized * NearM : away);
            var men = Living();
            for (int i = 0; i < men.Count; i++)
                if (ToSegment(bodyXZ, cam, men[i]) < pad)
                { Mark($"sight line blocked: a living man at {men[i].x:F0}, {men[i].y:F0}"); return false; }
            for (int i = 0; i < map.Props.Length; i++)
            {
                var q = new Vector2(map.Props[i].Pos.x, map.Props[i].Pos.z);
                if (ToSegment(bodyXZ, cam, q) < pad)
                { Mark($"sight line blocked: a prop at {q.x:F0}, {q.y:F0}"); return false; }
            }
            return true;
        }

        /// <summary>The visibility guard (step 4): the frog's own pixels, counted in C# out of the two stills of one
        /// held instant. A pixel counts when its three channels differ from the plate by more than 30 in total,
        /// whichever way the light went (a fallen casts no shadow — VATRenderer.Fallen.cs:292 — so the darker pixels
        /// are the frog against the lit night mud). The window is 6 m of ground around the mark, read from the
        /// sidecar. GetPixels32 rows run bottom-up and mark_py counts down from the top, hence H - 1 - mark_py.
        /// This is only "can he be seen at all": probe.py still judges the picture.</summary>
        static int BodyPixels(string livePng, string platePng)
        {
            float perM = Side(livePng, "mark_px_per_m"), mx = Side(livePng, "mark_px"), my = Side(livePng, "mark_py");
            if (float.IsNaN(perM) || float.IsNaN(mx) || float.IsNaN(my)) { Mark("no mark in " + Path.GetFileName(livePng) + ": the body cannot be counted"); return -1; }
            var live = new Texture2D(2, 2);
            var plate = new Texture2D(2, 2);
            try
            {
                if (!live.LoadImage(File.ReadAllBytes(livePng)) || !plate.LoadImage(File.ReadAllBytes(platePng))) { Mark("a still would not load"); return -1; }
                if (live.width != plate.width || live.height != plate.height) { Mark("the still and its plate differ in size"); return -1; }
                var a = live.GetPixels32();
                var b = plate.GetPixels32();
                int w = live.width, h = live.height;
                int side = Mathf.Min(Mathf.RoundToInt(6f * perM), Mathf.Min(w, h));
                int x0 = Mathf.Clamp(Mathf.RoundToInt(mx) - side / 2, 0, w - side);
                int yTop = Mathf.Clamp(Mathf.RoundToInt(my) - side / 2, 0, h - side);
                int changed = 0;
                for (int row = yTop; row < yTop + side; row++)
                {
                    int y = h - 1 - row;                 // GetPixels32 is bottom-up, the sidecar's y top-down
                    int at = y * w;
                    for (int x = x0; x < x0 + side; x++)
                    {
                        var p = a[at + x];
                        var q = b[at + x];
                        int d = Mathf.Abs(p.r - q.r) + Mathf.Abs(p.g - q.g) + Mathf.Abs(p.b - q.b);
                        if (d > 30) changed++;
                    }
                }
                return changed;
            }
            finally { Object.DestroyImmediate(live); Object.DestroyImmediate(plate); }
        }

        /// <summary>Which fallen body the current take staged (VATRenderer's index).</summary>
        int Marked;

        /// <summary>Reading the living men threw a NullReferenceException on four runs of this batch, and the earlier
        /// takes worked around it by guessing the battle's place from the map. The cause is now known and fixed (a
        /// local captured by a lambda inside this coroutine; see the note at the patch choice), so Living() reads them
        /// straight and BestPatch stages each frog by where the men actually are.</summary>

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
            // Every patch of open ground on the map, collected once: which of them a take uses is decided per take,
            // by where the living are then (BestPatch). At zoom 30 the frame is 1920 px / 31.36 px per m = 61 m across
            // — eight times the ground r3's zoom 4.66 saw — and GreyboxCorridor is only 90 x 276 m, so no fixed
            // distance bar can buy an empty frame on it. Stage clears the fallen list, so no other take's body can
            // share a frame; Clean() is the guard for the living, and a man in the frame re-shoots the take.
            var candidates = new System.Collections.Generic.List<Vector2>();
            try
            {
                for (float zz = 20f; zz < map.SizeMeters.y - 20f; zz += 4f)
                    for (float xx = 20f; xx < map.SizeMeters.x - 20f; xx += 4f)
                        if (Open(map, xx, zz, 8f)) candidates.Add(new Vector2(xx, zz));
            }
            // to DISK, not to TestContext.Out: the run of 2026-11-03 died with a NullReferenceException and the
            // only trace of it was a line number inside this method, because the buffered output of a coroutine
            // that throws is lost (see Mark). Marked here, the exception and its real line survive the throw.
            catch (System.Exception e) { Mark("SCAN THREW: " + e); throw; }
            Mark($"{candidates.Count} open candidates");
            // No ranking by distance from the corridor's centre any more, and no lambda reading a local of this
            // coroutine: a local a lambda captures lives in a display-class object that the compiler never allocates
            // once the state machine has crossed a `yield return`, so writing it throws a NullReferenceException —
            // the exception that killed five runs on 2026-11-03 at `var crowd = ...`, captured by the old Sort.
            var used = new System.Collections.Generic.List<Vector2>();
            int written = 0, spoiled = 0;

            // A take is kept only if every one of its six stills is honest, and its patch is chosen FRESH, from where
            // the living men are standing at that moment (BestPatch): the player framing is 61 m wide, the two sides
            // spawn at the corridor's ends and march inward, and a patch that was empty a minute ago is not empty now.
            // A spoiled take is shot again on another patch rather than kept with a man in the frame.
            for (int take = 0; take < 3; )
            {
                Assert.That(spoiled, Is.LessThan(12), "twelve takes spoiled: no patch of this map stays clear of the living (the allowance was 6 until this leg, and the two new guards — a blocked sight line and a body under 50 px — spend takes the old run kept)");
                var men = Living();
                float clearance;
                var spot = BestPatch(candidates, used, men, out clearance);
                // Every candidate is within 40 m of a patch already used or spoiled: BestPatch has nothing left and
                // returns (0, 0) with clearance -1 — the map's corner, where the run of this leg staged two takes and
                // read both their legs as one blob. The used list is forgotten instead, so a patch may be offered a
                // second time, which is honest (the men have moved) where a frog at 0, 0 is not.
                if (clearance < 0f)
                {
                    Mark("no patch is 40 m from one already used: the used list is cleared and the map offered again");
                    used.Clear();
                    spot = BestPatch(candidates, used, men, out clearance);
                }
                Assert.That(clearance, Is.GreaterThan(0f), "no open patch on this map at all");
                Mark($"take {take}: {men.Count} living men, patch {spot.x:F0}, {spot.y:F0} with {clearance:F0} m of clear ground");
                used.Add(spot);                       // used or spoiled, this patch is not offered again
                string stem = $"take{take}";
                bool dirty = false;
                int got = 0;
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
                    if (Clean(path, true)) got++; else dirty = true;
                    CaptureRig.Release();
                    for (int f = 0; f < 5; f++) yield return null;
                }

                // Step 5, as soon as the first still has told us where the camera really stood: a take with
                // something on the sight line is dropped now rather than after its other five stills. A hull in
                // front of the body is exactly what cost the previous run take2 (0 body pixels measured).
                if (!dirty && !Sighted(map, new Vector2(body.x, body.z), Path.Combine(dir, $"{stem}_m0.png"), 6f))
                {
                    dirty = true;
                    Mark($"take {take}: the sight line is blocked, this patch is dropped");
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
                if (Clean(i1, true)) got++; else dirty = true;

                Vat.ClearFallen();
                yield return null;
                foreach (string name in new[] { "plate", "plate2" })
                {
                    string path = Path.Combine(dir, $"{stem}_{name}.png");
                    tc.BaseYaw = -90f;
                    Fresh(path);
                    CaptureRig.Shot(path, body.x, body.z, Zoom, Yaw, Pitch, W, H);
                    yield return Drain(path);
                    if (Clean(path, false)) got++; else dirty = true;
                }
                CaptureRig.Release();
                for (int f = 0; f < 30; f++) yield return null;

                // Step 4: a take whose body cannot be SEEN is never kept. i1 against its plate, both out of the one
                // held instant just released, counted in the 6 m window around the mark. At 31.4 px/m a body is
                // hundreds of pixels, so under 50 means he is hidden behind something and the take is re-shot.
                if (!dirty)
                {
                    int seen = BodyPixels(Path.Combine(dir, $"{stem}_i1.png"), Path.Combine(dir, $"{stem}_plate.png"));
                    Mark($"take {take}: {seen} body pixels in the window");
                    if (seen >= 0 && seen < 50) { dirty = true; Mark($"take {take}: the body is hidden ({seen} px), shooting it again"); }
                }

                if (dirty || got < 6)
                {
                    spoiled++;
                    Mark($"take {take} spoiled ({got} of 6 honest): shooting it again on the next patch");
                    continue;
                }
                written += got;
                take++;
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
