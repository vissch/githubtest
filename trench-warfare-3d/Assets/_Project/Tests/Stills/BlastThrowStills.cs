// Phase: blast knockback death, round 2 (2026-10-20, tooling) — the measurement behind harsh critic r1 (12/100) on
// blast-knockback-death: "after.png shows the killed men still in a heap where they stood", the calibration flagged
// unreadable, and before/after 2984 frames apart instead of 3 s. This harness is the whole fix for those capture
// findings, and it is copied in shape from DeathStills (read its header for why it is an [Explicit] EditMode test that
// enters Play itself).
//
// Six runs on open ground, each a row of six men 2.5 m apart standing ACROSS the throw (along z, DeathLab.Row's
// alongZ), each photographed straight down (pitch 89, zoom 20) at -1 s, +0.5 s and +3 s around the moment a shell
// bursts: blast1..3 get the shell, calm1..3 get nothing and so give the probe its noise floor. The row stands across
// the throw (not along it, as r2-g1 first shot it) so every standing man shares one x and the throw is all in x:
// |dx| from the standing column no longer depends on which man a matcher pairs it with, which a row thrown along its
// own axis cannot give (docs/evidence/blast-knockback-death-r2-g1/probe.py). Straight down means world x and z map
// onto screen pixels at one scale with no foreshortening to correct.
//
// The moments are measured in Time.time, which is what the shots are taken on, so the seconds are right.
// Time.captureDeltaTime = 1/30 is asked for as well, but it does not hold in batch mode: the measured `frame` gaps
// are 17 and 94 where 30 a second would give 45 and 120 (docs/evidence/blast-knockback-death-r2-g1/frames.txt), so
// the sidecars' frame counter is a diagnostic, never the clock. Why it is lit and dry: the clock is
// pinned at 30 (midday) with rain and squalls off — r1's clock 120 squall is why contrast_median read 0.111 against
// the 0.12 bar (CaptureRig.cs:327).
//
// Run it by name WITH a graphics device (not -nographics):
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter BlastThrowStills \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_STILLS_DIR where the stills go (default %TEMP%/tw-blastthrow: never inside the checkout, whose
// untracked files land.py would see), TW_BLAST_TAG the stem's prefix (`pre` for the build as it stands, `post` after
// the change), TW_STILLS_FIELD the look (Winter, the day field, by default: the men must read from above).
// It asserts only that the stills were written and that the +3 s blast sidecars carry fallen >= 4: it is an
// instrument. The proof is the pixels, never the log it prints.
using System.Collections;
using System.Collections.Generic;
using System.IO;
using NUnit.Framework;
using UnityEngine;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using TW.Editor;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;
using Unity.Mathematics;
using TW.UI;

namespace TW.Tests
{
    public class BlastThrowStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        /// <summary>The runs, in order: three with a shell, three without.</summary>
        static readonly string[] Runs = { "blast1", "blast2", "blast3", "calm1", "calm2", "calm3" };
        /// <summary>Men in a row, and the metres between them (the probe reads px_per_m off this spacing).</summary>
        const int Men = 6;
        const float Spacing = 2.5f;
        /// <summary>The shell: radius 10 puts every man of the row inside 0.8 * radius, the throw gate in
        /// AnimationController.Death.cs:209, and 600 damage kills men on 100 hp outright.</summary>
        const float Radius = 10f, Damage = 600f;
        const float ShellOffsetX = 5f;   // the shell sits this far to -x of the row line, so the throw runs across the row
        /// <summary>The three moments, in seconds from the burst, and the names they take in a stem.</summary>
        static readonly float[] Moments = { -1f, 0.5f, 3f };
        static readonly string[] MomentNames = { "m1_0", "p0_5", "p3_0" };

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static IEnumerator Drain(string path)
        {
            string side = Path.ChangeExtension(path, ".json");
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !(File.Exists(path) && File.Exists(side)); f++) yield return null;
        }

        /// <summary>Clear a shot's own files before it is taken. A retake writes over a png that is already there, so
        /// Drain's File.Exists arm passed immediately and Side() read the take before it: blast2's -1 s take was
        /// accepted on a drawn_infantry its own sidecar says was 0 (leg g1-02's first run).</summary>
        static void Forget(string path)
        {
            string side = Path.ChangeExtension(path, ".json");
            if (File.Exists(path)) File.Delete(path);
            if (File.Exists(side)) File.Delete(side);
        }

        /// <summary>DeathStills.Open: a patch of open ground, no trench, ladder, wire, bunker or blocked cell from 4 m
        /// before (x, z - back) to 4 m past (x + w, z + ahead), and no prop on the row's own strip.</summary>
        static bool Open(MapData map, float x, float z, float w, float back, float ahead)
        {
            var size = map.SizeMeters;
            for (float zz = z - back - 4f; zz <= z + ahead + 4f; zz += 2f)
                for (float xx = x - 4f; xx <= x + w + 4f; xx += 2f)
                {
                    if (xx < 3f || zz < 3f || xx > size.x - 3f || zz > size.y - 3f) return false;
                    var layer = map.LayerAt(new float3(xx, 0f, zz));
                    if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked | NavLayer.Wire | NavLayer.Bunker)) != 0) return false;
                }
            for (int i = 0; i < map.Props.Length; i++)
            {
                var q = map.Props[i].Pos;
                if (q.x > x - 3f && q.x < x + w + 3f && q.z > z - 4f && q.z < z + 4f) return false;
            }
            return true;
        }

        /// <summary>One number out of a sidecar, NaN if it is not there (CaptureRig writes our own json).</summary>
        static float Side(string json, string key)
        {
            if (!File.Exists(json)) return float.NaN;
            string text = File.ReadAllText(json);
            int at = text.IndexOf("\"" + key + "\"");
            if (at < 0) return float.NaN;
            string rest = text.Substring(at + key.Length + 2).TrimStart(' ', ':');
            // the closing brace is spelled (char)125, not '}': validate.py counts braces in the file and one
            // inside a char literal reads to it as a brace the file never closes
            int end = rest.IndexOfAny(new[] { ',', '\n', '\r', (char)125 });
            if (end < 0) return float.NaN;
            return float.TryParse(rest.Substring(0, end), System.Globalization.NumberStyles.Float,
                                  System.Globalization.CultureInfo.InvariantCulture, out float v) ? v : float.NaN;
        }

        [UnityTest, Explicit("Blast throws photographed straight down for probe.py; run by name, with a graphics device.")]
        public IEnumerator BlastThrowsArePhotographedFromAbove()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            // the capture rig is shared with whatever ran in this editor before: a gym run's leftover shots were still
            // queued, their output folder had since been pruned, and the Debug.LogError each one raised failed this test
            // on a log message that has nothing to do with the stills. The pixels and the files are the verdict here.
            LogAssert.ignoreFailingMessages = true;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            // lit: the default NightMud field is Dark, and the first set came back a blue-black top-down frame the critic
            // would call unreadable again. Winter is the day field (DeathStills' TW_STILLS_FIELD), set on the scene BEFORE
            // Play — a serialized field survives the domain reload, and the scene is not saved.
            string fieldName = System.Environment.GetEnvironmentVariable("TW_STILLS_FIELD");
            if (!System.Enum.TryParse(fieldName, true, out TW.Presentation.Terrain.Biome field)) field = TW.Presentation.Terrain.Biome.Winter;
            foreach (var g in Object.FindObjectsByType<TW.Presentation.Terrain.GreyboxTerrainView>(FindObjectsSortMode.None)) g.Field = field;
            foreach (var at in Object.FindObjectsByType<TW.Presentation.Terrain.Atmosphere>(FindObjectsSortMode.None)) at.Field = field;
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            // read AFTER entering Play: the domain reloads there, and locals set before it come back as defaults
            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-blastthrow");
            Directory.CreateDirectory(dir);
            string tag = System.Environment.GetEnvironmentVariable("TW_BLAST_TAG");
            if (string.IsNullOrEmpty(tag)) tag = "pre";

            // quiet: no enemy deploys or attacks, no stray shells (each run makes its own deaths, or none)
            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            for (int f = 0; f < 120; f++) yield return null;
            // dry and lit, and the base yaw pinned so a shot's yaw cannot drift with the camera's own
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;
            CaptureRig.Rig.YawPin = 0f;
            Time.captureDeltaTime = 1f / 30f;
            // and anything already in that queue is drained before the first run, so no other set's shot poses the camera
            // in the middle of one of ours
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            TestContext.Out.WriteLine(DeathLab.Absurd(0f));   // plain throws, no gag: the throw itself is what is measured

            // a throwaway shot before the real loop: the very first CaptureRig.Shot of a run came back with
            // drawn_infantry (and men_in_frame) 0 even over a fresh, correctly-positioned, full-hp row -- the VAT
            // culling pipeline (VATRenderer.LateUpdate, which reads ViewEye.Main's frustum) was not yet warmed up
            // this early in Play. One disposable shot here, discarded, pays that cost before any run is judged.
            {
                string warm = Path.Combine(Path.GetTempPath(), "tw-blastthrow-warmup.png");
                CaptureRig.Hold(30f);
                CaptureRig.Shot(warm, Host.Local.Map.SizeMeters.x * 0.5f, Host.Local.Map.SizeMeters.y * 0.5f, 20f, 0f, 89f, 1280, 720);
                yield return Drain(warm);
                CaptureRig.Release();
                TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;
                if (File.Exists(warm)) File.Delete(warm);
                string warmJson = Path.ChangeExtension(warm, ".json");
                if (File.Exists(warmJson)) File.Delete(warmJson);
            }

            var map = Host.Local.Map;
            var size = map.SizeMeters;
            float rowLength = (Men - 1) * Spacing;
            int written = 0;
            // the patches, in the order the runs take them. Separated 22 m along x and 26 m along z from each other:
            // a shot sees 41 m x 23 m of ground (zoom 20, straight down). The shell is 5 m to -x of the row and the
            // throw now runs across the row, along +x, out to about 9 m, so the ground a run needs spans x - 9 to
            // x + 13; 22 m along x still keeps a neighbour's column out of the frame, and 26 m along z covers the
            // frame's own 11.5 m with room to spare.
            var patches = new List<Vector2>();
            for (float zz = 30f; zz < size.y - 30f && patches.Count < 12; zz += 4f)
                for (float xx = 8f; xx < size.x - 20f && patches.Count < 12; xx += 4f)
                {
                    bool clear = true;
                    foreach (var c in patches) if (Mathf.Abs(c.x - xx) < 22f && Mathf.Abs(c.y - zz) < 26f) clear = false;
                    if (clear && Open(map, xx - 9f, zz - 4f, 22f, 0f, rowLength + 8f)) patches.Add(new Vector2(xx, zz));
                }
            TestContext.Out.WriteLine($"{patches.Count} open patches: {string.Join(" ", patches)}");
            Assert.That(patches.Count, Is.GreaterThanOrEqualTo(2), "fewer than two open patches on this map");
            var offRow = new List<string>();   // every man the shell did not throw across the row
            int turn = 0;
            foreach (string run in Runs)
            {
                bool blast = run.StartsWith("blast");
                // this run's own patch first, then every other one in turn if it will not give a clean frame
                bool shot = false;
                for (int attempt = 0; attempt < patches.Count && !shot; attempt++)
                {
                    var patch = patches[(turn + attempt) % patches.Count];
                    float x = patch.x, z = patch.y;
                    // clean ground first: from straight above an old corpse reads as a standing man and never moves,
                    // so one left lying in the frame is a body the probe measures as thrown nowhere (leg g1).
                    var vat = Object.FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
                    if (vat != null) vat.ClearFallen();
                    // and no other man anywhere on the field: drawn_infantry counts every foot figure the game drew,
                    // so the match's own squads standing near a patch read as men of the row (11 for a row of six on
                    // the first patch tried), and the "exactly Men" rule then burned patch after patch until the map
                    // ran out of open ground. Moved off-map, not despawned, for the reason given below.
                    int swept = 0;
                    Host.WriteWorlds(m =>
                    {
                        var w = m.World;
                        for (int sl = 0; sl < w.HighWater; sl++)
                        {
                            if ((w.Flags[sl] & (uint)UnitFlags.Alive) == 0) continue;
                            if ((w.Flags[sl] & (uint)UnitFlags.Vehicle) != 0) continue;
                            if (w.Position[sl].x < -400f) continue;
                            w.Position[sl] = new float3(-500f, 0f, -500f);
                            swept++;
                        }
                    });
                    for (int f = 0; f < 30; f++) yield return null;

                    string slots = DeathLab.Row(Men, x, z, 0, Spacing, 100f, -1f, alongZ: true);
                    TestContext.Out.WriteLine($"{run} attempt {attempt + 1} at {x}, {z}: {slots} (swept {swept} men off the field)");
                    Assert.That(slots, Does.StartWith("slots "), run + ": the row failed: " + slots);
                    var mine = new List<int>();
                    foreach (string part in slots.Substring(6).Split(' ')) if (int.TryParse(part, out int slot)) mine.Add(slot);

                    // the presenter must have captured the fresh row before anything is photographed.
                    // SimPresenter.Capture sets count = HighWater and runs only after a stepped tick; until it has,
                    // Drawn(slot) is float3.zero for the row's slots and PoseCount is 0, so the VAT fill job
                    // (VATRenderer.cs:262, over PoseCount/PoseSlot) and CaptureRig.Measure (CaptureRig.cs:270, over
                    // Drawn) both see a row standing at the origin: drawn_infantry 0 and men_in_frame 0 over a row
                    // whose world.Position is perfectly right, which is the 0-read that burned seven runs in a row
                    // (leg g1's note). No CaptureRig.Hold before this wait: Hold pins Time.timeScale to 0
                    // (CaptureRig.cs:507), no tick steps, and the presenter could then never catch up.
                    int caught = -1;
                    for (int f = 0; f < 300 && caught < 0; f++)
                    {
                        bool ready = Host.Presenter.PoseCount > 0;
                        foreach (int slot in mine)
                            if (math.distance(Host.Presenter.Drawn(slot), Host.Local.World.Position[slot]) >= 0.5f) ready = false;
                        if (ready) caught = f; else yield return null;
                    }
                    TestContext.Out.WriteLine($"  {run} attempt {attempt + 1}: the presenter caught the row after " +
                                              (caught < 0 ? "300 frames it never did" : caught + " frames"));

                    // two seconds to stand and settle before the row is first photographed
                    float t0 = Time.time + 2f;
                    while (Time.time < t0) yield return null;
                    float cx = x, cz = z + rowLength * 0.5f;   // the camera's focus: the row's own middle, so every body stays inside the frame
                    // the shell to the -x side of the row, never on it. On the row line Explosion.Dir is zero and
                    // AnimationController.Death throws each man straight away from the burst, which is along the row
                    // itself; and the crater then lands on exactly the ground the measurement needs. Offset 5 m: the
                    // furthest man is sqrt(5^2 + 6.25^2) = 8.0 m out, still inside the throw gate
                    // (closeness = 1.15 - dist/(0.8*Radius) = 0.15 > 0 at Radius 10), and his throw carries an x
                    // fraction of 5/8; the middle men's is 1.0.
                    float sx = x - ShellOffsetX;

                    // one posed shot, frozen for the shot only (GoreZoomStills' trick): the tactical camera eases
                    // towards a pose and the rig puts it back when the queue drains, so a shot taken while it is still
                    // moving comes out from the wrong place. Queueing the same shot three times did not cure it -
                    // blast3's +3 s still landed at pitch 85.1, yaw 327 and 1.37 m off, which tilts every body in the
                    // frame at once. Hold stops the clock dead, so the camera cannot move between pose and render.
                    IEnumerator Take(string file)
                    {
                        Forget(file);
                        CaptureRig.Hold(30f);
                        CaptureRig.Shot(file, cx, cz, 20f, 0f, 89f, 1280, 720);
                        yield return Drain(file);
                        CaptureRig.Release();
                        TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;   // Release gives the sky its own clock back
                    }

                    bool dirty = false;
                    float burstAt = 0f;
                    for (int k = 0; k < Moments.Length && !dirty; k++)
                    {
                        // the burst is at t0 + 1 s, and -1 s is the standing row. From +0.5 s on the moment is
                        // measured from when the shell was actually queued, because the -1 s shot may have taken
                        // retakes below and a fixed t0 + 1 + Moments[k] would then photograph the wrong instant.
                        float due = k == 0 ? t0 + 1f + Moments[k] : burstAt + Moments[k];
                        while (Time.time < due) yield return null;
                        string path = Path.Combine(dir, $"{tag}_{run}_{MomentNames[k]}.png");
                        if (k != 0) { yield return Take(path); continue; }

                        // exactly the row and nothing else in the frame, read off the still itself: four living men
                        // walked into calm1's frame once (drawn_infantry 10 for a row of six), and a wanderer is a
                        // blob the probe's matcher can pair with a row man.
                        // Up to three takes at the SAME patch before the patch is abandoned: the 0-read is
                        // intermittent at one spot, and one bad frame used to burn a candidate patch, so a run ran
                        // out of open ground. The "exactly Men men" rule itself is unchanged - it is the instrument's
                        // only guard against a wanderer.
                        bool clean = false;
                        for (int take = 0; take < 3 && !clean; take++)
                        {
                            if (take > 0) for (int f = 0; f < 30; f++) yield return null;
                            yield return Take(path);
                            string side = Path.ChangeExtension(path, ".json");
                            float drawn = Side(side, "drawn_infantry");
                            if (Mathf.Abs(drawn - Men) < 0.5f) { clean = true; break; }
                            // which of the three causes this is, said in this same run, so no second Unity run is
                            // needed to tell them apart: a stale presenter (drawn far from position), a camera
                            // mismatch (ViewEye.Main is not the camera the shot posed), or a bad pose (pose_error_m
                            // large, cam_x/cam_z away from the cx, cz asked for).
                            TestContext.Out.WriteLine($"  {run} attempt {attempt + 1} take {take + 1}: drawn_infantry {drawn}, not {Men}");
                            TestContext.Out.WriteLine($"    PoseCount {Host.Presenter.PoseCount}" +
                                                      $" viewEyeIsMain {ReferenceEquals(ViewEye.Main, Camera.main)}" +
                                                      $" timeScale {Time.timeScale}" +
                                                      $" men_in_frame {Side(side, "men_in_frame")}" +
                                                      $" cam {Side(side, "cam_x")}, {Side(side, "cam_z")}" +
                                                      $" pitch {Side(side, "cam_pitch")}" +
                                                      $" pose_error_m {Side(side, "pose_error_m")}" +
                                                      $" (asked for {cx}, {cz})");
                            foreach (int slot in mine)
                                TestContext.Out.WriteLine($"    slot {slot} position {Host.Local.World.Position[slot]}" +
                                                          $" drawn {Host.Presenter.Drawn(slot)}" +
                                                          $" alive {(Host.Local.World.Flags[slot] & (uint)UnitFlags.Alive) != 0}");
                        }
                        if (!clean)
                        {
                            TestContext.Out.WriteLine($"  {run} attempt {attempt + 1}: three takes all unclean: on to the next patch");
                            dirty = true;
                            break;
                        }
                        // where each man stood, and whether the controller is the one posing him, logged for the next
                        // leg to read if the numbers come out low (never as the proof: the proof is the pixels)
                        foreach (int slot in mine)
                            TestContext.Out.WriteLine($"  {run} slot {slot} stood at {Host.Local.World.Position[slot]} " +
                                                      $"(UseAnimationController {Host.UseAnimationController})");
                        // the -1 s still is on disk before the shell is queued, so it is a standing row for certain
                        if (blast) TestContext.Out.WriteLine($"  {run}: {DeathLab.Shell(sx, cz, Radius, Damage)}");
                        burstAt = Time.time;
                    }
                    // a rejected row needs no clearing up here: the next attempt's own sweep moves every living
                    // man off the field and clears every fallen body before it spawns its row, which is what used to
                    // be done by hand for the rejected row alone (leg g1's blast3/calm1 both failed at a spot an
                    // earlier failed attempt had quietly left six live men in).
                    if (dirty) continue;
                    shot = true;
                    turn++;
                    // counted here, not per shot: a retaken run overwrites its own stills, and each must count once
                    foreach (string moment in MomentNames) if (File.Exists(Path.Combine(dir, $"{tag}_{run}_{moment}.png"))) written++;

                    // the throws, as the controller recorded them: scanned back over the ticks the burst could have fallen on
                    if (blast && Host.Animation != null)
                    {
                        uint now = Host.Local.World.Tick;
                        uint oldest = now > 600u ? now - 600u : 0u;
                        foreach (int slot in mine)
                        {
                            bool found = false;
                            for (uint t = now; t > oldest && !found; t--)
                                if (Host.Animation.TryDeath(slot, t, out var rec))
                                {
                                    TestContext.Out.WriteLine($"  {run} slot {slot} death tick {t}: throw {rec.ThrowX:0.00}, {rec.ThrowZ:0.00}, up {rec.ThrowUp:0.00}");
                                    // the geometry this harness exists for: with the shell 5 m to -x of the row
                                    // line every man is thrown across the row, out to +x, never along it. Not
                                    // |x| > |z|: the end men sit 6.25 m up the row from the shell's own z, so
                                    // their direction is 5 : 6.25 and the z part is the larger one. Collected,
                                    // not asserted here, so a bad geometry still leaves all 18 stills on disk.
                                    if (!(rec.ThrowX > 0f))
                                        offRow.Add($"{run} slot {slot}: throw {rec.ThrowX:0.00}, {rec.ThrowZ:0.00} is not across the row (+x)");
                                    found = true;
                                }
                            if (!found) TestContext.Out.WriteLine($"  {run} slot {slot}: no death record in the ring");
                        }
                    }
                    for (int f = 0; f < 30; f++) yield return null;
                }
                Assert.That(shot, Is.True, run + ": no patch gave a clean frame of exactly " + Men + " men");
            }

            CaptureRig.Release();
            DeathLab.Absurd(-1f);
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            CaptureRig.Rig.YawPin = float.NaN;
            Time.captureDeltaTime = 0f;
            yield return new ExitPlayMode();
            TestContext.Out.WriteLine($"wrote {written} stills to {dir}");
            Assert.That(written, Is.EqualTo(Runs.Length * Moments.Length), "not every moment of every run was photographed");
            Assert.That(offRow, Is.Empty, "the shell threw men along the row instead of across it: " + string.Join("; ", offRow));

            // the one number read back out of the stills here: the bodies the +3 s blast frames drew
            foreach (string run in Runs)
            {
                if (!run.StartsWith("blast")) continue;
                string side = Path.Combine(dir, $"{tag}_{run}_p3_0.json");
                float fallen = Side(side, "fallen");
                TestContext.Out.WriteLine($"{run} +3 s: fallen {fallen}");
                Assert.That(fallen, Is.GreaterThanOrEqualTo(4f), run + ": fewer than four bodies lie at +3 s (" + side + ")");
            }
        }
    }
}
