// Phase: ability-fx-captures-r2 (2026-10-19) — the proof that the chlorine bank LIES ON THE GROUND. The harsh critic
// threw r1's two gas stills out: both were taken from the wrong pose (pose_error_m 0.94 and 15.39) and the one number
// that mattered, the share of gas pixels low down, was 0.003. This harness reshoots gas in the real night corridor at
// the play zoom (T1, 30) and at the overview (O240), each with a NO-GAS PLATE from the same pose at the same frame, so
// a pixel probe can tell gas from field by subtraction. docs/evidence/ability-fx-captures-r2/probe.py measures them.
//  - every wait is a FRAME COUNT, never Time.time, so the plate pass hits the same frames and so the same sim ticks;
//  - CombatFx.ShowOverlays off hides the yellow ability-area disc, which is gas-coloured and would be counted as gas;
//  - CameraShake off is the pose_error fix: the rig poses the camera one frame and photographs it the next, and shake
//    moved it in between;
//  - SessionState "tw.gasbank.plate", or TW_GAS_PLATE=1, runs the identical script with the gas calls skipped, so
//    only the gas cards differ; SessionState "tw.gasbank.dir" or TW_STILLS_DIR says where the stills go
//    (default %TEMP%/tw-gasbank: never inside the checkout).
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Text;
using NUnit.Framework;
using UnityEngine;
using UnityEditor;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using Unity.Mathematics;
using TW.Editor;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;
using TW.UI;

namespace TW.Tests
{
    public class GasBankStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        /// <summary>The cloud warms up for 60 ticks and its source then feeds the field for 240 more, so it is alive
        /// from tick 60 to tick 300 after the call. The sim runs 20 ticks a second against 30 captured frames, so a
        /// tick is 1.5 frames: 200 frames are 133 ticks, which is 73 ticks into the bank's life - spread out of its
        /// first puff and still thick. (470 frames, the first try, was 313 ticks: the bank had gone.)</summary>
        const int WaitFrames = 200;
        /// <summary>Every block is padded to this many frames, so both passes stay in step frame for frame.</summary>
        const int BlockFrames = 520;
        /// <summary>A still from the wrong pose is worthless (the critic's verdict on r1).</summary>
        const float PoseBar = 0.5f;

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        /// <summary>The character a json number ends on, held as a code because validate.py counts a file's braces.</summary>
        const char CloseBrace = (char)125;

        static readonly List<string> rows = new List<string>();
        static float worstPose;

        [UnityTest, Explicit("Two gas stills and their plates in the night corridor; run by name, with a graphics device.")]
        public IEnumerator GasBankPoolsOnTheGroundAtBothBands()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            yield return new EnterPlayMode();
            yield return Shoot();     // a fresh enumerator after the reload (WalkerStills' lesson)
        }

        static IEnumerator Shoot()
        {
            rows.Clear(); worstPose = 0f;
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");

            // SessionState in an editor driven by eval; the env vars for a -runTests launch of its own, which is how
            // every stills harness here is actually run (the pipeline's run_tests selects no [Explicit] test at all)
            bool plate = SessionState.GetBool("tw.gasbank.plate", false)
                         || System.Environment.GetEnvironmentVariable("TW_GAS_PLATE") == "1";
            string dir = SessionState.GetString("tw.gasbank.dir", "");
            if (string.IsNullOrEmpty(dir)) dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-gasbank");
            Directory.CreateDirectory(dir);
            TestContext.Out.WriteLine((plate ? "PLATE pass" : "GAS pass") + " into " + dir);

            CombatFx.ShowOverlays = false;            // the yellow ability-area disc is gas-coloured: it would be counted as gas
            CameraShake.Strength = 0f; CameraShake.Reset();
            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            for (int f = 0; f < 60; f++) yield return null;
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;   // the owner's night look
            Time.captureDeltaTime = 1f / 30f;

            var tc = Object.FindFirstObjectByType<TacticalCamera>();
            Assert.That(tc, Is.Not.Null, "no TacticalCamera in the scene");
            var map = Host.Local.Map;
            var size = map.SizeMeters;

            // one patch of open ground in the MIDDLE of the field, used by both bands: the overview frames the whole
            // corridor round it anyway. Not near the edge - the first run searched from x = 20 and gassed x = 26,
            // where every call was taken and no cloud ever appeared in the field (nothing is drawn out there).
            float x = -1f, z = -1f, nearest = float.MaxValue;
            for (float zz = 30f; zz < size.y - 30f; zz += 4f)
                for (float xx = 12f; xx < size.x - 12f; xx += 4f)
                {
                    if (!Open(map, xx, zz, 10f)) continue;
                    float d = (xx - size.x * 0.5f) * (xx - size.x * 0.5f) + (zz - size.y * 0.5f) * (zz - size.y * 0.5f);
                    if (d < nearest) { nearest = d; x = xx; z = zz; }
                }
            Assert.Greater(x, 0f, "no open ground for a gas bank on this map");
            TestContext.Out.WriteLine("gas centre " + x + ", " + z);

            // O240 FIRST: its frame holds the whole map, so it must not hold the T1 block's leavings
            var far = new List<Vector2>();
            for (int k = -2; k <= 2; k++) far.Add(new Vector2(x, Mathf.Clamp(z + k * 40f, 20f, size.y - 20f)));
            yield return Block(dir, "gas-o240" + (plate ? "-plate" : ""), "o240", tc, x, z, 240f, 25f, far, plate, map, cloud: false);

            // chlorine-bank-visual-comps: the T1 band shoots the cloud stand-in too, so the owner sees both options
            // ("flat patches, or a rolling cloud") off the same tick, the same pose, the same night
            yield return Block(dir, "gas-t1" + (plate ? "-plate" : ""), "t1", tc, x, z, 30f, 25f,
                               new List<Vector2> { new Vector2(x, z) }, plate, map, cloud: !plate);

            Time.captureDeltaTime = 0f;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            CameraShake.Strength = 1f;                // a player setting: put it back
            CombatFx.ShowOverlays = true;
            CombatFx.GasBankCloud = false;             // capture only: never leave it on
            File.WriteAllText(Path.Combine(dir, plate ? "manifest-plate.json" : "manifest.json"),
                              "{\n  \"stills\": [\n" + string.Join(",\n", rows.ToArray()) + "\n  ]\n}\n");
            yield return new ExitPlayMode();

            Assert.Less(worstPose, PoseBar, "the worst pose error was " + worstPose.ToString("0.000") + " m: a still from the wrong pose is worthless");
        }

        /// <summary>One band: frame it, gas it (unless this is the plate pass), wait out the cloud's life in FRAMES,
        /// photograph it, then pad to a fixed block so the two passes stay in step.</summary>
        static IEnumerator Block(string dir, string stem, string band, TacticalCamera tc, float x, float z,
                                 float zoom, float pitch, List<Vector2> points, bool plate, MapData map, bool cloud)
        {
            int spent = 0;
            tc.BaseYaw = -90f; tc.Zoom = zoom; tc.Focus = new Vector2(x, z);
            for (int f = 0; f < 10; f++) { yield return null; spent++; }

            var answers = new List<string>();
            int taken = 0;
            foreach (var p in points)
            {
                if (!plate)
                {
                    // ChlorineGas has a 1800-tick cooldown, so a second call is refused; VfxLab.Call tops the credit up
                    Host.WriteWorlds(m =>
                    {
                        var a = m.World.GetSystem<OffMapAbilitySystem>();
                        if (a != null && a.Cooldown.IsCreated)
                            a.Cooldown[0 * OffMapAbilitySystem.AbilitySlots + (int)OffMapAbilityId.ChlorineGas] = 0;
                    });
                    string said = VfxLab.Call((int)OffMapAbilityId.ChlorineGas, p.x, p.y);
                    for (int f = 0; f < 8; f++) { yield return null; spent++; }
                    // the sim sets the cooldown only on a call it ACCEPTED: a refusal (the match already won, no
                    // credit, the cooldown) is silent otherwise, and r2's first run shot four stills with no gas in
                    // the world at all
                    int cool = Cooldown();
                    if (cool > 0) taken++;
                    said += cool > 0 ? " (taken)" : " (REFUSED: the sim set no cooldown)";
                    answers.Add(said);
                    TestContext.Out.WriteLine(band + " call at " + p.x + ", " + p.y + ": " + said);
                }
                for (int f = 0; f < 4; f++) { yield return null; spent++; }
            }

            for (int f = 0; f < WaitFrames; f++) { yield return null; spent++; }

            string path = Path.Combine(dir, stem + ".png");
            float best = float.MaxValue;
            for (int tryNo = 0; tryNo < 3; tryNo++)
            {
                CaptureRig.Shot(path, x, z, zoom, 40f, pitch, 1280, 720);
                yield return Drain(path);
                spent += 12;
                float err = Sidecar(path, "pose_error_m");
                if (err < best) best = err;
                TestContext.Out.WriteLine(stem + " try " + tryNo + ": pose_error_m " + err.ToString("0.000"));
                if (err < PoseBar) break;
            }
            if (best > worstPose) worstPose = best;

            uint tick = Host.Local.World.Tick;

            // r2-2: what the FRAME that was photographed actually held. The 133-card reading of r2's README came from
            // an ordinary play frame, not from a capture frame, and the rig turns the TacticalCamera off for the whole
            // set - so whether the cards exist in the captured frame was still unknown, and it is the first fork.
            // the field is read through reflection: GasSmokeSystem lives in TW.Sim.Combat, which this test assembly
            // does not reference, and an asmdef reference list is a seam (CLAUDE.md) - not worth a seam commit for
            // one diagnostic. Its Gas array is an ordinary NativeArray<float>, so only the system's own type is hidden.
            int cells = 0; float fieldMax = 0f;
            object local = Host.Local;
            object gasField = local.GetType().GetProperty("Gas")?.GetValue(local)
                           ?? local.GetType().GetField("Gas")?.GetValue(local);
            if (gasField != null
                && gasField.GetType().GetField("Gas")?.GetValue(gasField) is Unity.Collections.NativeArray<float> field
                && field.IsCreated)
                for (int i = 0; i < field.Length; i++)
                {
                    float c = field[i];
                    if (c >= 0.8f) cells++;                 // the floor DrawGasBank's Refresh uses
                    if (c > fieldMax) fieldMax = c;
                }
            yield return null;                               // one frame on, so gasCards holds the list of a drawn frame
            int cards = Cards(); float recipes = Recipes(); bool ready = Books();

            float ground = RenderGround.Sample(map, x, z);
            var sb = new StringBuilder();
            sb.Append("    { \"still\": \"").Append(stem).Append(".png\", \"subject\": \"chlorine gas bank\", \"band\": \"")
              .Append(band).Append("\", \"plate\": ").Append(plate ? "true" : "false")
              .Append(", \"x\": ").Append(x.ToString("0.###")).Append(", \"z\": ").Append(z.ToString("0.###"))
              .Append(", \"ground_y\": ").Append(ground.ToString("0.####"))
              .Append(", \"sim_tick\": ").Append(tick).Append(", \"frame\": ").Append(Time.frameCount)
              .Append(", \"pose_error_m\": ").Append(best.ToString("0.####"))
              .Append(", \"calls_taken\": ").Append(taken)
              .Append(", \"gas_cells\": ").Append(cells).Append(", \"gas_field_max\": ").Append(fieldMax.ToString("0.##"))
              .Append(", \"gas_cards\": ").Append(cards)
              .Append(", \"recipes\": ").Append(recipes.ToString("0.##"))
              .Append(", \"books_ready\": ").Append(ready ? "true" : "false")
              .Append(", \"draw_calls\": ").Append(FrameBudget.DrawCalls)
              .Append(", \"calls\": [");
            for (int i = 0; i < answers.Count; i++) sb.Append(i > 0 ? ", " : "").Append('"').Append(answers[i]).Append('"');
            sb.Append("] }");
            rows.Add(sb.ToString());

            if (cloud)
            {
                CombatFx.GasBankCloud = true;
                yield return null; spent++;
                yield return null; spent++;

                string cloudStem = stem + "-cloud";
                string cloudPath = Path.Combine(dir, cloudStem + ".png");
                CaptureRig.Shot(cloudPath, x, z, zoom, 40f, pitch, 1280, 720);
                yield return Drain(cloudPath);
                spent += 12;

                var cloudSb = new StringBuilder();
                cloudSb.Append("    { \"still\": \"").Append(cloudStem).Append(".png\", \"subject\": \"chlorine gas bank (cloud stand-in)\", \"band\": \"")
                       .Append(band).Append("\", \"plate\": false")
                       .Append(", \"x\": ").Append(x.ToString("0.###")).Append(", \"z\": ").Append(z.ToString("0.###"))
                       .Append(", \"ground_y\": ").Append(ground.ToString("0.####"))
                       .Append(", \"sim_tick\": ").Append(tick).Append(", \"frame\": ").Append(Time.frameCount)
                       .Append(", \"pose_error_m\": ").Append(Sidecar(cloudPath, "pose_error_m").ToString("0.####"))
                       .Append(", \"calls_taken\": ").Append(taken)
                       .Append(", \"draw_calls\": ").Append(FrameBudget.DrawCalls)
                       .Append(", \"calls\": [] }");
                rows.Add(cloudSb.ToString());

                CombatFx.GasBankCloud = false;
            }

            for (int f = spent; f < BlockFrames; f++) yield return null;
        }

        /// <summary>The scene's CombatFx, or null: every reading below is read off it by reflection, because the
        /// bank's card list, its recipe switch and its books are all private and none of them is worth a public
        /// surface for one diagnostic.</summary>
        static CombatFx Fx => Object.FindFirstObjectByType<CombatFx>();

        const BindingFlags Hidden = BindingFlags.Instance | BindingFlags.NonPublic;

        /// <summary>How many gas cards CombatFx packed in the last frame it drew (-1: no CombatFx or no such field).</summary>
        static int Cards()
        {
            var fx = Fx; if (fx == null) return -1;
            var f = typeof(CombatFx).GetField("gasCards", Hidden);
            return f?.GetValue(fx) is System.Collections.ICollection list ? list.Count : -1;
        }

        /// <summary>CombatFx.recipes: under 0.5 the drawn banks are off and the old translucent blocks stand in.</summary>
        static float Recipes()
        {
            var fx = Fx; if (fx == null) return -1f;
            var f = typeof(CombatFx).GetField("recipes", Hidden);
            return f?.GetValue(fx) is float v ? v : -1f;
        }

        /// <summary>CombatFx.books.Ready: one missing PNG disables every flipbook.</summary>
        static bool Books()
        {
            var fx = Fx; if (fx == null) return false;
            var f = typeof(CombatFx).GetField("books", Hidden);
            return f?.GetValue(fx) is FlipbookFx books && books.Ready;
        }

        /// <summary>The chlorine cooldown of player 0, in ticks: 1800 right after a call the sim took, 0 after one it refused.</summary>
        static int Cooldown()
        {
            var a = Host.Local.World.GetSystem<OffMapAbilitySystem>();
            return a != null && a.Cooldown.IsCreated
                 ? a.Cooldown[0 * OffMapAbilitySystem.AbilitySlots + (int)OffMapAbilityId.ChlorineGas] : -1;
        }

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            Assert.That(CaptureRig.Pending(), Is.EqualTo("0"), "the capture queue stalled");
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
            Assert.That(File.Exists(path), Is.True, Path.GetFileName(path) + " was never written");
        }

        /// <summary>One number out of the still's .json sidecar, by key.</summary>
        static float Sidecar(string png, string key)
        {
            string side = Path.ChangeExtension(png, ".json");
            if (!File.Exists(side)) return float.MaxValue;
            string text = File.ReadAllText(side);
            int at = text.IndexOf("\"" + key + "\"");
            if (at < 0) return float.MaxValue;
            int colon = text.IndexOf(':', at);
            int end = text.IndexOfAny(new[] { ',', '\n', CloseBrace }, colon + 1);
            return float.TryParse(text.Substring(colon + 1, end - colon - 1).Trim(),
                                  System.Globalization.NumberStyles.Float,
                                  System.Globalization.CultureInfo.InvariantCulture, out float v) ? v : float.MaxValue;
        }

        /// <summary>Open ground for a bank centred on (x, z) (VfxStills.Open: no trench, wire, bunker, blocked cell,
        /// prop or held cover within r, and none of the sim's static cover within r + 6).</summary>
        static bool Open(MapData map, float x, float z, float r)
        {
            var size = map.SizeMeters;
            for (float zz = z - r; zz <= z + r; zz += 2f)
                for (float xx = x - r; xx <= x + r; xx += 2f)
                {
                    if (xx < 3f || zz < 3f || xx > size.x - 3f || zz > size.y - 3f) return false;
                    var layer = map.LayerAt(new float3(xx, 0f, zz));
                    if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked | NavLayer.Wire | NavLayer.Bunker)) != 0) return false;
                }
            for (int i = 0; i < map.Props.Length; i++)
            {
                var q = map.Props[i].Pos;
                if (math.abs(q.x - x) < r && math.abs(q.z - z) < r) return false;
            }
            if (map.StaticCover.IsCreated)
                for (int i = 0; i < map.StaticCover.Length; i++)
                {
                    var cv = map.StaticCover[i];
                    if (cv.OwnerSlot >= 0) continue;
                    if (math.abs(cv.Center.x - x) < r + 6f && math.abs(cv.Center.z - z) < r + 6f) return false;
                }
            return true;
        }
    }
}
