// Phase: deaths (2026-10-24, evidence) — ONE flop of ONE frog, in daylight, photographed with his own standing
// picture and with a control death beside it. The owner threw out frog-death-flop-r3: "the body does not read as a
// frog on its back, and the probe does not look. take0_p05.png is face-down, take1_p15.png is a different tangle with
// a rifle, and the red star stays in take1_plate.png and take2_plate.png after the body is removed."
// What that reading changed here:
//   - the field is put on the scene BEFORE Play (Biome.Winter, as DeathStills.cs does it): r3 was shot at night on
//     NightMud and take1_p15 is near black, so nothing in it could read as anything;
//   - DebrisRenderer.Gore = 0 for the take: the "red star" is CombatFx.HitBlood's ground splat, which ClearFallen
//     leaves behind, so r3's plate was not the clean ground its mask claimed;
//   - one flop, one patch, one camera fixed on the frog's standing spot (the flop's fly is 0.35 m straight up, so he
//     rests where he stood). r3's three takes were three different deaths on three patches: three tangles, not three
//     captures of one flop;
//   - flop_alive: the same frog standing on his feet, same light, same zoom — the only honest pixel reference for his
//     back, so "belly up" can be read off the pixels instead of out of the plan's own sidecar;
//   - a control death on a second patch with DeathGags.Pin(0f): today's death, no rest roll, no twitch, so every bar
//     has an absence measured from this same build;
//   - mark_figure in every sidecar: nothing in r3 proved the frog bake (figure 2) was drawn rather than the Soldier,
//     and the owner read a rifle in one still.
//
// Run it by name WITH a graphics device (not -nographics):
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter FrogFlopStills \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_STILLS_DIR where the stills go (default %TEMP%/tw-frogflop), TW_STILLS_FIELD the biome to light the
// field with (default Winter).
//
// The fourteen stills, all at one camera per take, aimed at the patch he stands on:
//   flop_ground.png                the empty patch, before a frog is on it: the plate for his standing picture
//   flop_alive.png                 the frog standing on it, before the shell
//   flop_p05/p10/p15.png           death +0.5 / +1.0 / +1.5 s of that one flop
//   flop_frz_b/frz_c.png           the SAME held instant as flop_p15 (CaptureRig.Hold, never released in between):
//                                  three captures of one flop, which is the probe's noise floor
//   flop_plate.png                 that instant with the body taken away (VATRenderer.ClearFallen): p15 minus the
//                                  plate IS the frog's pixels, so no box is guessed
//   ctrl_ground/alive/p05/p10/p15/plate   the same, on the control death (DeathGags.Pin(0f))
// The zoom is not typed: a trial patch is staged, a trial still shot, and mark_px_per_m read out of its own sidecar
// until a metre is at least 150 px (the 5%-of-an-800x600-crop bar).
// It asserts each still is honest (the death shots: men_in_frame 0, fallen_in_frame 0, mark_in_frame 1,
// mark_figure 2, rain 0, pose_error_m < 0.5; the alive shots: a man IS in frame and no body is) and fails the take
// rather than letting a lying set be committed; judging the pictures is the probe's and the critic's job.
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
        /// <summary>The scale the take is shot at, in pixels per metre: the frame is then about 21 m wide, so the
        /// body stays in it even when the control death throws it 5 m off the patch, and the frog is still about a
        /// hundred pixels long.</summary>
        const float WantPerM = 90f;
        /// <summary>The camera aims at the ground he stands on, nothing above it: aimed 1.5 m higher (as r3 had it)
        /// the body came out against the top edge of the frame, cut off, at this zoom.</summary>
        const float AimAbove = 0f;
        const float Pitch = 60f;

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

        /// <summary>What every still must be true of: dry, from the pose that was asked for, and no second body.</summary>
        static void Ground(string png)
        {
            Assert.That(File.Exists(png), "no still at " + png);
            Assert.That(Side(png, "fallen_in_frame"), Is.EqualTo(0f), png + ": another body shares the frame");
            Assert.That(Side(png, "rain"), Is.LessThan(0.001f), png + ": rain is not pinned off");
            Assert.That(Side(png, "pose_error_m"), Is.LessThan(0.5f), png + ": the camera is not where the shot asked");
        }

        /// <summary>A still of the dead frog: he is alone, he is marked, and the bake that was drawn is the frog's.</summary>
        static void Dead(string png, bool marked)
        {
            Ground(png);
            Assert.That(Side(png, "men_in_frame"), Is.EqualTo(0f), png + ": a living man shares the frame");
            if (marked)
            {
                Assert.That(Side(png, "mark_in_frame"), Is.EqualTo(1f), png + ": the marked frog is not in the frame");
                Assert.That(Side(png, "mark_figure"), Is.EqualTo(2f), png + ": the bake drawn is not the frog's (figure 2)");
            }
        }

        /// <summary>A still of the frog still standing: a man IS in frame (he is the reference), and there is no mark
        /// to ask about, because the mark names a fallen body and nobody has fallen yet.</summary>
        static void Alive(string png)
        {
            Ground(png);
            Assert.That(Side(png, "men_in_frame"), Is.GreaterThanOrEqualTo(1f), png + ": no living frog in the frame");
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

        static int Slot(string spawned)
        {
            Assert.That(spawned, Does.StartWith("slot "), "the frog was not spawned: " + spawned);
            int end = spawned.IndexOf(';');
            string s = end < 0 ? spawned : spawned.Substring(0, end);
            return int.Parse(s.Substring(5).Trim());
        }

        /// <summary>The shell fired, the body waited for, and the body nearest the patch marked: a death elsewhere in
        /// the battle can beat him into the list, and marking index 0 blindly would mark a stranger.</summary>
        IEnumerator Kill(float x, float z)
        {
            TestContext.Out.WriteLine($"shell for the frog at {x}, {z}: " + DeathLab.FrogKill(x, z));
            for (int f = 0; f < 900 && Vat.FallenCount < 1; f++) yield return null;
            Assert.That(Vat.FallenCount, Is.GreaterThanOrEqualTo(1), $"nothing fell on the patch at {x}, {z}");
            int mine = 0; float best = float.MaxValue;
            for (int i = 0; i < Vat.FallenCount; i++)
            {
                var at = Vat.FallenPos(i);
                float d = Vector2.Distance(new Vector2(at.x, at.z), new Vector2(x, z));
                if (d < best) { best = d; mine = i; }
            }
            TestContext.Out.WriteLine($"body {mine} of {Vat.FallenCount}, {best:F1} m from the patch, figure {Vat.FallenFigure(mine)}");
            // 8 m, not the flop's 0: today's death (the control) throws the body, and the only other bodies in the
            // world are 100 m away in the battle
            Assert.That(best, Is.LessThan(8f), "the nearest body is not the frog who stood on this patch");
            Marked = mine;
            CaptureRig.MarkFallen(mine);
            yield return null;
        }

        [UnityTest, Explicit("One flop of the frog, in daylight, with his standing picture and a control; run by name, with a graphics device.")]
        public IEnumerator TheFrogsDeathFlopIsPhotographedAlone()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            // the light: set on the scene BEFORE Play (a serialized field survives the domain reload; the scene is not
            // saved). r3 was shot on the scene's own night field and came back unreadably dark.
            if (!System.Enum.TryParse(System.Environment.GetEnvironmentVariable("TW_STILLS_FIELD"), true, out TW.Presentation.Terrain.Biome field))
                field = TW.Presentation.Terrain.Biome.Winter;
            foreach (var g in Object.FindObjectsByType<TW.Presentation.Terrain.GreyboxTerrainView>(FindObjectsSortMode.None)) g.Field = field;
            foreach (var at in Object.FindObjectsByType<TW.Presentation.Terrain.Atmosphere>(FindObjectsSortMode.None)) at.Field = field;
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            Assert.That(Vat, Is.Not.Null, "no VATRenderer");

            // read AFTER entering Play: the domain reloads there and a local set before it comes back as a default
            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-frogflop");
            Directory.CreateDirectory(dir);

            // no gore for this set: the blood splat CombatFx.HitBlood paints on the ground stays after ClearFallen, so
            // with it on the plate is not the ground behind him and the mask is not the frog
            float gore = TW.Presentation.Tactical.DebrisRenderer.Gore;
            TW.Presentation.Tactical.DebrisRenderer.Gore = 0f;

            // quiet: nobody deploys, attacks or drops a shell of their own, so the only deaths are the two we stage
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
            // without this, Pose clamps every zoom up to NearZoom (~15) and the frog can never fill the crop
            tc.ZoomMin = 1.5f; tc.CloseFitHeight = 1.2f;

            // open ground far from the battle, a patch each for the trial, the flop and the control, found in the map
            // rather than typed: the first run of this take staged its frog off the playable ground and photographed
            // flat blue water
            var map = Host.Local.Map;
            var crowd = CaptureRig.Crowd();
            var spots = new System.Collections.Generic.List<Vector2>();
            for (float zz = 20f; zz < map.SizeMeters.y - 20f && spots.Count < 3; zz += 8f)
                for (float xx = 20f; xx < map.SizeMeters.x - 20f && spots.Count < 3; xx += 8f)
                {
                    // far from the fight: at 60 m the wide framing still caught other corpses that had fallen on the
                    // patch while the frog was being staged
                    if (Vector2.Distance(new Vector2(xx, zz), crowd) < 120f) continue;
                    bool clear = true;
                    foreach (var u in spots) if (Vector2.Distance(u, new Vector2(xx, zz)) < 16f) clear = false;
                    if (clear && Open(map, xx, zz, 8f)) spots.Add(new Vector2(xx, zz));
                }
            Assert.That(spots.Count, Is.EqualTo(3), "no three patches of open ground away from the battle");
            TestContext.Out.WriteLine("patches: " + string.Join(", ", spots));
            int written = 0;
            float zoom = 6f;

            // the trial patch: one frog killed and photographed only to read mark_px_per_m out of his own sidecar, so
            // the zoom of the real take is measured and not typed. His body is cleared before the take begins.
            {
                Vat.ClearFallen();
                DeathGags.Pin(1f);
                TestContext.Out.WriteLine("trial frog: " + DeathLab.FrogStand(spots[0].x, spots[0].y));
                yield return Kill(spots[0].x, spots[0].y);
                for (int attempt = 0; attempt < 5; attempt++)
                {
                    var at = Vat.FallenPos(Marked);
                    string trial = Path.Combine(dir, $"trial_{attempt}.png");
                    Fresh(trial);
                    CaptureRig.Shot(trial, at.x, at.z, zoom, 40f, Pitch, W, H, at.y + AimAbove);
                    yield return Drain(trial);
                    float perM = Side(trial, "mark_px_per_m");
                    TestContext.Out.WriteLine($"trial {attempt}: zoom {zoom:F2} -> {perM:F1} px/m");
                    if (!(perM > 1f)) break;
                    if (Mathf.Abs(perM - WantPerM) < WantPerM * 0.1f) break;
                    // the zoom IS the camera's distance, so px/m scales with 1/zoom: this lands on the wanted scale
                    // instead of merely clearing a bar, which left the first set so close that the control's body,
                    // thrown 5 m, was outside the frame
                    zoom = Mathf.Max(tc.ZoomMin, zoom * perM / WantPerM);
                }
            }

            // two takes of one death each: the flop (the gag, as the game plays it) and the control (DeathGags.Pin(0),
            // today's death: no rest roll, no twitch). Same patch size, same light, same zoom, one camera each.
            foreach (var take in new[] { (stem: "flop", absurd: 1f, spot: spots[1]), (stem: "ctrl", absurd: 0f, spot: spots[2]) })
            {
                Vat.ClearFallen();                       // no body from the trial or the other take can be in frame
                CaptureRig.MarkOff();
                TestContext.Out.WriteLine(DeathLab.Absurd(take.absurd));

                // ONE camera for the whole take, on the patch itself and never moved again: the ground he will stand
                // on, read out of the map, so the picture of the empty ground, the picture of him standing on it and
                // every picture of his body are the same frame of the same place.
                float aimY = RenderGround.Sample(map, take.spot.x, take.spot.y) + AimAbove;

                // the empty ground FIRST, before a frog exists: the plate for his standing picture. The take's other
                // plate is shot after the shell, so it holds a crater and cannot say what he covers while alive.
                string ground = Path.Combine(dir, $"{take.stem}_ground.png");
                tc.BaseYaw = -90f;
                Fresh(ground);
                CaptureRig.Shot(ground, take.spot.x, take.spot.y, zoom, 40f, Pitch, W, H, aimY);
                yield return Drain(ground);
                Dead(ground, false); written++;
                // A spawn written straight into the world (TankCapture.Spawn) does not always survive the next second
                // of play: the first run of this take found the frog gone (flags 0, hp 0) a frame after he was asked
                // for, with a body in the frame nobody staged (the sim re-simulates from its own snapshot and the
                // frog was never in it). It sticks about one time in three, so he is asked for again until the world
                // itself says the slot holds a live frog on this patch, and only then is his picture taken.
                int slot = -1;
                for (int tries = 0; tries < 25 && slot < 0; tries++)
                {
                    int ask = Slot(DeathLab.FrogStand(take.spot.x, take.spot.y));
                    for (int f = 0; f < 60; f++) yield return null;   // let the picture draw him where he was spawned
                    var w = Host.Local.World;
                    float off = Vector2.Distance(new Vector2(w.Position[ask].x, w.Position[ask].z), take.spot);
                    TestContext.Out.WriteLine($"{take.stem}: try {tries} slot {ask} flags {w.Flags[ask]} hp {w.Hp[ask]:F1} archetype {w.Archetype[ask]} {off:F1} m off the patch, fallen {Vat.FallenCount} drawn {Vat.DrawnInfantry}");
                    // alive is not enough: a rolled-back spawn leaves the slot to whoever had it, and the second take
                    // checked a stranger with 100 hp 170 m away and photographed the ground
                    if ((w.Flags[ask] & (uint)UnitFlags.Alive) != 0 && w.Hp[ask] > 0f
                        && w.Archetype[ask] == InfantryArchetype.Frog && off < 3f) slot = ask;
                }
                Assert.That(slot, Is.GreaterThanOrEqualTo(0), "the frog would not stay alive on his patch");
                Vat.ClearFallen();                       // a body the sim replayed while he was being staged
                yield return null;
                TestContext.Out.WriteLine($"{take.stem}: frog slot {slot} standing at {(Vector3)Host.Presenter.Drawn(slot)}");

                string alive = Path.Combine(dir, $"{take.stem}_alive.png");
                tc.BaseYaw = -90f;
                Fresh(alive);
                CaptureRig.Shot(alive, take.spot.x, take.spot.y, zoom, 40f, Pitch, W, H, aimY);
                yield return Drain(alive);
                Alive(alive); written++;

                yield return Kill(take.spot.x, take.spot.y);
                float death = Time.time;

                foreach (var shot in new[] { (dt: 0.5f, name: "p05"), (dt: 1.0f, name: "p10") })
                {
                    while (Time.time < death + shot.dt) yield return null;
                    string path = Path.Combine(dir, $"{take.stem}_{shot.name}.png");
                    tc.BaseYaw = -90f;
                    Fresh(path);
                    CaptureRig.Shot(path, take.spot.x, take.spot.y, zoom, 40f, Pitch, W, H, aimY);
                    yield return Drain(path);
                    Dead(path, true); written++;
                }
                while (Time.time < death + 1.5f) yield return null;

                // one held instant: p15, and for the flop two more of the very same pose (three captures of one flop,
                // the probe's noise floor), then the body taken away (the plate). Never released in between, or the
                // plate would be a different world.
                CaptureRig.Hold(120f);
                string[] frozen = take.stem == "flop" ? new[] { "p15", "frz_b", "frz_c" } : new[] { "p15" };
                foreach (string name in frozen)
                {
                    string path = Path.Combine(dir, $"{take.stem}_{name}.png");
                    tc.BaseYaw = -90f;
                    Fresh(path);
                    CaptureRig.Shot(path, take.spot.x, take.spot.y, zoom, 40f, Pitch, W, H, aimY);
                    yield return Drain(path);
                    Dead(path, true); written++;
                }
                Vat.ClearFallen();
                yield return null;
                string plate = Path.Combine(dir, $"{take.stem}_plate.png");
                tc.BaseYaw = -90f;
                Fresh(plate);
                CaptureRig.Shot(plate, take.spot.x, take.spot.y, zoom, 40f, Pitch, W, H, aimY);
                yield return Drain(plate);
                Dead(plate, false); written++;
                CaptureRig.Release();
                for (int f = 0; f < 30; f++) yield return null;
            }

            CaptureRig.MarkOff();
            DeathLab.Absurd(-1f);
            TW.Presentation.Tactical.DebrisRenderer.Gore = gore;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            Time.captureDeltaTime = 0f;
            yield return new ExitPlayMode();
            TestContext.Out.WriteLine($"wrote {written} stills to {dir} at zoom {zoom.ToString("0.00", CultureInfo.InvariantCulture)}");
            Assert.That(written, Is.EqualTo(14), "the set is short: a take failed to land");
        }
    }
}
