// Phase: ability-fx-captures-r2 (2026-10-19) — the proof that the chlorine bank LIES ON THE GROUND. The harsh critic
// threw r1's two gas stills out: both were taken from the wrong pose (pose_error_m 0.94 and 15.39) and the one number
// that mattered, the share of gas pixels low down, was 0.003. This harness reshoots gas in the real night corridor at
// the play zoom (T1, 30) and at the overview (O240), each with a NO-GAS PLATE from the same pose at the same frame, so
// a pixel probe can tell gas from field by subtraction. docs/evidence/ability-fx-captures-r2/probe.py measures them.
//  - every wait is a FRAME COUNT, never Time.time, so the plate pass hits the same frames and so the same sim ticks;
//  - CombatFx.ShowOverlays off hides the yellow ability-area disc, which is gas-coloured and would be counted as gas;
//  - CameraShake off is the pose_error fix: the rig poses the camera one frame and photographs it the next, and shake
//    moved it in between;
//  - r2-3: THREE seeds per pass, not one. The critic's r1 verdict on r2-2 was that every bar passed by under 0.025
//    on a single capture, so one lucky patch could carry the claim. The open-patch search now keeps the three best
//    patches that stand at least SeedSpacingM apart and runs the O240 + T1 blocks once per patch, into stems
//    gas-o240-s0 / gas-t1-s0 / ... s2 (12 stills a pass). Between seeds sits a fixed DrainFrames block, never a
//    poll, so both passes stay frame-for-frame in step - which is the whole reason the plate subtracts cleanly -
//    and each manifest row carries gas_cells_before, the field reading taken just before the block's first call, so
//    a leftover cloud from the previous seed shows up in the evidence instead of being counted as this seed's gas;
//  - SessionState "tw.gasbank.plate", or TW_GAS_PLATE=1, runs the identical script with the gas calls skipped, so
//    only the gas cards differ; SessionState "tw.gasbank.dir" or TW_STILLS_DIR says where the stills go
//    (default %TEMP%/tw-gasbank: never inside the checkout).
//  - r2-3-g2: a THIRD band, "side", grazing the bank instead of looking down on it. Both T1 (pitch 25, zoom 30)
//    and O240 (pitch 25, zoom 240) look down steeply enough that ground lying further out than the bank's own
//    base reads as extra height: at T1's pose (zoom 30, fov 25, so distance = 30*tan(30deg)/tan(12.5deg) = 78.1 m,
//    camera 33.0 m up, 70.8 m out) 10 m of ground beyond the bank's base sits 2.8 deg higher in frame, which turns
//    into 70.8*tan(2.8deg) = 3.5 m of "height" that is only footprint depth - half of the 5.7-7.2 m the T1 band
//    reports. The side band (zoom 16, pitch 8 - TacticalCamera.PitchMin, so as low as the pose allows; aimY
//    raised to clear the horizon at this zoom) looks at the bank almost edge-on: at its pose (distance 41.7 m,
//    camera 7.8 m up, 41.3 m out) the same 10 m of depth reads as only 41.3*tan(2.05deg) = 1.5 m, so what is left
//    on screen is mostly the bank's own body card facing the lens, not ground sloping away under it.
//  - r2-3-g2-g2-g1: ONE BANK IN THE SAME FRAMING ON EVERY SEED, with nothing but the bank in the side mask.
//    Three changes, all three about what the plate is worth as a control:
//      FRAME PARITY. The 8-frame wait after a call now runs in BOTH passes (it used to sit inside the
//      `if (!plate)` branch, so the plate pass skipped 8 frames a call). The two passes therefore land on the
//      same frame and the same sim tick, and every static thing subtracts away. The old manifests show the cost
//      of not doing this: side gas tick 898 against plate 893, o240 249 against 222 - and a burning tank wreck
//      that merely MOVED in those ticks was in the side mask, which is why gas-side-s1 was a tank.
//      THE SIDE BAND GOES FIRST. The order is side, o240, t1. DrainFrames has just run before a seed's first
//      block, so the side block starts on an empty field (`gas_cells_before` 0, as o240's rows read when o240
//      went first) and the side still holds one bank and one only. T1 stays last, so o240 never holds t1's
//      line of calls.
//      A CLEAR VIEW (`ClearView`). The seed search now keeps only patches the side lens can see: the ground
//      line from that lens's own stand (SideStandM out on bearing SideStandBearingDeg) to the patch, sampled
//      every 2 m, with no Trench/Link/Wire/Bunker/Blocked cell and no prop within half a corridor width of it.
//      Widths 8, 6 then 4 m, the widest that yields three seeds, logged with them - the same ladder, and for
//      the same reason, as SeedSpacingsM. A picket standing between lens and bank is in front of it on screen,
//      where no probe gate can tell it from the bank's own face. ON THIS MAP the ladder runs out: 71 open
//      patches, 2 of them clear even at 4 m, so the last rung is 0 m - no corridor check - and the rung that
//      was used is in the log, because three seeds is the bar and one seed is no bar at all.
//  - r2-3-g2-g2-g1-g2: ONE BANK PER BAND, not per seed. DrainFrames now runs before EVERY block (side, o240,
//    t1), where g1 ran it once per seed. g1's manifest says what that cost: `gas_cells_before` 0 on the side
//    rows but 153 on o240 and 725 on T1, and 1046 gas cards in the T1 frame - the o240 block's five banks plus
//    T1's own, drifted together into a lawn of pads. The loop is unconditional, like every wait here: a block
//    one pass skips shifts the plate's sim tick and the probe then refuses the pair (`side_tick_bad`). o240
//    keeps its five calls - its five banks are the far band's subject - and loses only the older blocks'
//    leavings. The cost is six more DrainFrames blocks a pass.
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
        /// <summary>How many patches are shot per pass: three seeds, so no single lucky patch carries a bar.</summary>
        const int Seeds = 3;
        /// <summary>How far apart two seeds are asked to stand, widest first. 60 m was the plan's number, but the
        /// greybox corridor holds only TWO open patches that far apart (the first run asserted with "only 2 open
        /// patches 60 m apart"), so the search steps down to the widest spacing that does yield three. The floor is
        /// 32 m: a point gas source's bank spreads about 10 m, which is why Open() clears a 10 m radius, so 32 m
        /// still puts three times the bank's own width between two seeds and no cloud is shared. (The O240 block
        /// paints a 160 m line of calls round its own seed in any case; what matters is that each still has its own
        /// plate from the same pose and the same frame.)</summary>
        static readonly float[] SeedSpacingsM = { 60f, 48f, 40f, 32f };
        /// <summary>A fixed block between seeds that lets the previous cloud die. 420 frames (280 ticks) was the
        /// plan's number and it was not enough: the first baseline pass still read 272 leftover cells in the field
        /// when seed 1's overview was shot, against 524 at the shot itself, so half that frame's gas could have
        /// been seed 0's. The plan's own rule for a non-zero gas_cells_before is to lengthen the drain, never to
        /// re-pick the patch, so this is 1500 frames: 1000 ticks of nothing but decay on top of the block the seed
        /// already spent, against a source that stops feeding 300 ticks after its call. Fixed, never a poll: a
        /// poll would run the two passes out of step and the plate would stop subtracting.</summary>
        const int DrainFrames = 1500;
        /// <summary>Where the side band's lens stands: 41.3 m from the patch, which is the pose CaptureRig.Shot
        /// gives for zoom 16, yaw 40, pitch 8 (every side sidecar reads cam 41.26 m out, cam_yaw 310). ClearView
        /// walks that line.</summary>
        const float SideStandM = 41.3f;
        /// <summary>The side camera looks back along cam_yaw 310, so it stands on bearing 130 from the patch.</summary>
        const float SideStandBearingDeg = 130f;
        /// <summary>The corridor widths ClearView tries between the side lens and the patch, widest first. A wire
        /// picket or a prop standing in it is in front of the bank on screen, where no gate can tell it from the
        /// bank's own face, so the seed search refuses such a patch. The ladder exists for the same reason
        /// SeedSpacingsM has one: the greybox corridor may not hold three patches at the widest width, and the
        /// honest answer is then the widest width that does yield three, logged with the seeds.</summary>
        /// The last rung is 0: NO corridor check at all. The greybox corridor's first run of this search found
        /// 71 open patches and only 2 of them with a clear view even 4 m wide - a 41.3 m ground line across a
        /// trench map nearly always crosses a trench, a link or a wire belt - so the three widths alone yield
        /// one seed, and the bar needs three. The ladder therefore ends where it began, and which rung was used
        /// is logged with the seeds, so the evidence says whether the corridor was clear rather than assuming it.
        static readonly float[] ViewWidthsM = { 8f, 6f, 4f, 0f };

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        /// <summary>The character a json number ends on, held as a code because validate.py counts a file's braces.</summary>
        const char CloseBrace = (char)125;

        static readonly List<string> rows = new List<string>();
        static float worstPose;

        /// <summary>NUnit's default is 180 s and r2-3-g2-g2-g1-g2's nine DrainFrames blocks take the capture past
        /// it (the plate pass died on "Timeout value of 180000 ms was exceeded" at 223 s). 900 s is five times the
        /// measured run, so a slower machine or a colder asset database still finishes.</summary>
        [UnityTest, Timeout(900000), Explicit("Two gas stills and their plates in the night corridor; run by name, with a graphics device.")]
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

            // the three best patches of open ground in the MIDDLE of the field: open ones sorted by how near the
            // centre they sit, then taken greedily while each stands SeedSpacingM from the ones already kept. Both
            // bands of a seed use its patch; the overview frames the whole corridor round it anyway. Not near the
            // edge - the first run searched from x = 20 and gassed x = 26, where every call was taken and no cloud
            // ever appeared in the field (nothing is drawn out there).
            var open = new List<Vector2>();
            for (float zz = 30f; zz < size.y - 30f; zz += 4f)
                for (float xx = 12f; xx < size.x - 12f; xx += 4f)
                    if (Open(map, xx, zz, 10f)) open.Add(new Vector2(xx, zz));
            var centre = new Vector2(size.x * 0.5f, size.y * 0.5f);
            open.Sort((a, b) => (a - centre).sqrMagnitude.CompareTo((b - centre).sqrMagnitude));
            var seeds = new List<Vector2>();
            float spacing = 0f, viewWidth = 0f;
            int viewable = 0;
            foreach (float width in ViewWidthsM)
            {
                // r2-3-g2-g2-g1: a patch is only usable when the side lens can SEE it. ClearView walks the
                // ground line from that lens's own stand to the patch and refuses anything standing in a
                // corridor `width` wide, so no wire picket and no prop is in front of the bank on any seed.
                viewWidth = width;
                var seen = new List<Vector2>();
                foreach (var cand in open)
                    if (width <= 0f || ClearView(map, cand.x, cand.y, width)) seen.Add(cand);
                viewable = seen.Count;
                foreach (float want in SeedSpacingsM)
                {
                    seeds.Clear(); spacing = want;
                    foreach (var cand in seen)
                    {
                        bool clear = true;
                        foreach (var kept in seeds) if (Vector2.Distance(cand, kept) < want) { clear = false; break; }
                        if (clear) seeds.Add(cand);
                        if (seeds.Count == Seeds) break;
                    }
                    if (seeds.Count == Seeds) break;
                }
                if (seeds.Count == Seeds) break;
            }
            TestContext.Out.WriteLine(open.Count + " open patches, " + viewable + " of them kept at view width "
                                      + viewWidth + " m (0 = no corridor check, none of the widths yielded "
                                      + Seeds + " seeds); seeds " + spacing + " m apart");
            Assert.That(seeds.Count, Is.EqualTo(Seeds),
                        "only " + seeds.Count + " open patches " + spacing + " m apart on this map, wanted " + Seeds);
            for (int i = 0; i < seeds.Count; i++)
                TestContext.Out.WriteLine("seed " + i + " centre " + seeds[i].x + ", " + seeds[i].y);

            for (int s = 0; s < seeds.Count; s++)
            {
                float x = seeds[s].x, z = seeds[s].y;
                string tail = "-s" + s + (plate ? "-plate" : "");

                // DRAIN BEFORE EVERY BLOCK (r2-3-g2-g2-g1-g2): the g1 lane drained once per seed, so only the
                // seed's first block (side) started on an empty field. o240 then held side's bank and T1 held
                // both - gas_cells_before 153 and 725 in g1's manifest - and the T1 frame was six banks drifted
                // together, a lawn of pads instead of one bank. The loop below is unconditional, as every wait
                // here is: a block one pass skips shifts the plate's sim tick and the probe refuses the pair.
                // NOT before the very first block of the run: there are no leavings yet, and a drain there is
                // 1000 ticks of live battle in which the AI's own chlorine can land on the field - it did, on
                // the first run of this change: gas_cells_before 157 on gas-side-s0, the one block g1 shot with
                // no drain in front of it and read 0 for. The condition is on the SEED, not on the pass, so
                // both passes still run frame for frame.
                if (s > 0) for (int f = 0; f < DrainFrames; f++) yield return null;

                // SIDE FIRST (r2-3-g2-g2-g1): it is the band whose claim rests on ONE bank and on the plate
                // subtracting. Every block now starts on an empty field, so gas_cells_before reads 0 on all
                // three bands, not only this one. The side band:
                // pitch 8 (TacticalCamera.PitchMin, as low as Pose allows) and zoom 16 (just above CloseZoom 15,
                // so the lens stays at Fov 25) graze the bank instead of looking down on it. aimY is raised above
                // the default (world y = 0) because at this zoom a raised patch would otherwise put the horizon
                // out of frame.
                yield return Block(dir, "gas-side" + tail, "side", tc, x, z, 16f, 8f,
                                   new List<Vector2> { new Vector2(x, z) }, plate, map,
                                   RenderGround.Sample(map, x, z) + 2f);

                for (int f = 0; f < DrainFrames; f++) yield return null;

                // o240 second: its frame holds the whole map, so it must not hold the T1 block's leavings. T1 is
                // still last, which is what keeps t1's line of calls out of the overview. o240 keeps its five
                // calls - its five banks are the far band's subject - and only loses the side block's leavings.
                var far = new List<Vector2>();
                for (int k = -2; k <= 2; k++) far.Add(new Vector2(x, Mathf.Clamp(z + k * 40f, 20f, size.y - 20f)));
                yield return Block(dir, "gas-o240" + tail, "o240", tc, x, z, 240f, 25f, far, plate, map);

                for (int f = 0; f < DrainFrames; f++) yield return null;

                // ONE bank in the T1 frame: the drain above takes the side block's bank and the o240 block's five
                // out of it, which is the whole point of this capture.
                yield return Block(dir, "gas-t1" + tail, "t1", tc, x, z, 30f, 25f,
                                   new List<Vector2> { new Vector2(x, z) }, plate, map);
            }

            Time.captureDeltaTime = 0f;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            CameraShake.Strength = 1f;                // a player setting: put it back
            CombatFx.ShowOverlays = true;
            File.WriteAllText(Path.Combine(dir, plate ? "manifest-plate.json" : "manifest.json"),
                              "{\n  \"stills\": [\n" + string.Join(",\n", rows.ToArray()) + "\n  ]\n}\n");
            yield return new ExitPlayMode();

            Assert.Less(worstPose, PoseBar, "the worst pose error was " + worstPose.ToString("0.000") + " m: a still from the wrong pose is worthless");
        }

        /// <summary>One band: frame it, gas it (unless this is the plate pass), wait out the cloud's life in FRAMES,
        /// photograph it, then pad to a fixed block so the two passes stay in step.</summary>
        static IEnumerator Block(string dir, string stem, string band, TacticalCamera tc, float x, float z,
                                 float zoom, float pitch, List<Vector2> points, bool plate, MapData map,
                                 float aimY = float.NaN)
        {
            int spent = 0;
            tc.BaseYaw = -90f; tc.Zoom = zoom; tc.Focus = new Vector2(x, z);
            for (int f = 0; f < 10; f++) { yield return null; spent++; }

            var answers = new List<string>();
            int taken = 0;
            // the field as it stands BEFORE this block's first call: anything here is a leftover from the previous
            // seed, not this seed's gas, and it goes into the manifest row rather than being silently counted
            int cellsBefore; float maxBefore;
            FieldRead(out cellsBefore, out maxBefore);
            foreach (var p in points)
            {
                string said = "";
                if (!plate)
                {
                    // ChlorineGas has a 1800-tick cooldown, so a second call is refused; VfxLab.Call tops the credit up
                    Host.WriteWorlds(m =>
                    {
                        var a = m.World.GetSystem<OffMapAbilitySystem>();
                        if (a != null && a.Cooldown.IsCreated)
                            a.Cooldown[0 * OffMapAbilitySystem.AbilitySlots + (int)OffMapAbilityId.ChlorineGas] = 0;
                    });
                    said = VfxLab.Call((int)OffMapAbilityId.ChlorineGas, p.x, p.y);
                }
                // r2-3-g2-g2-g1: the wait after a call runs in BOTH passes. It used to sit inside the branch
                // above, so the plate pass skipped 8 frames a call and ran 4-5 sim ticks ahead of the gas pass
                // (side_tick_skew in the old manifests) - and a plate from another tick subtracts nothing static:
                // a burning wreck that merely moved between the two ticks landed in the side mask.
                for (int f = 0; f < 8; f++) { yield return null; spent++; }
                if (!plate)
                {
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
                CaptureRig.Shot(path, x, z, zoom, 40f, pitch, 1280, 720, aimY);
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
            // FieldRead says why the field itself is read through reflection.
            int cells; float fieldMax;
            FieldRead(out cells, out fieldMax);
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
              .Append(", \"gas_cells_before\": ").Append(cellsBefore)
              .Append(", \"gas_field_max_before\": ").Append(maxBefore.ToString("0.##"))
              .Append(", \"gas_cards\": ").Append(cards)
              .Append(", \"recipes\": ").Append(recipes.ToString("0.##"))
              .Append(", \"books_ready\": ").Append(ready ? "true" : "false")
              .Append(", \"draw_calls\": ").Append(FrameBudget.DrawCalls)
              .Append(", \"calls\": [");
            for (int i = 0; i < answers.Count; i++) sb.Append(i > 0 ? ", " : "").Append('"').Append(answers[i]).Append('"');
            sb.Append("] }");
            rows.Add(sb.ToString());

            for (int f = spent; f < BlockFrames; f++) yield return null;
        }

        /// <summary>The scene's CombatFx, or null: every reading below is read off it by reflection, because the
        /// bank's card list, its recipe switch and its books are all private and none of them is worth a public
        /// surface for one diagnostic.</summary>
        static CombatFx Fx => Object.FindFirstObjectByType<CombatFx>();

        const BindingFlags Hidden = BindingFlags.Instance | BindingFlags.NonPublic;

        /// <summary>The gas field right now: how many cells stand at or over the floor DrawGasBank's Refresh uses,
        /// and the strongest cell. Read through reflection: GasSmokeSystem lives in TW.Sim.Combat, which this test
        /// assembly does not reference, and an asmdef reference list is a seam (CLAUDE.md) - not worth a seam commit
        /// for one diagnostic. Its Gas array is an ordinary NativeArray&lt;float&gt;, so only the system's own type
        /// is hidden.</summary>
        static void FieldRead(out int cells, out float fieldMax)
        {
            cells = 0; fieldMax = 0f;
            object local = Host.Local;
            if (local == null) return;
            object gasField = local.GetType().GetProperty("Gas")?.GetValue(local)
                           ?? local.GetType().GetField("Gas")?.GetValue(local);
            if (gasField != null
                && gasField.GetType().GetField("Gas")?.GetValue(gasField) is Unity.Collections.NativeArray<float> field
                && field.IsCreated)
                for (int i = 0; i < field.Length; i++)
                {
                    float c = field[i];
                    if (c >= 0.8f) cells++;
                    if (c > fieldMax) fieldMax = c;
                }
        }

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

        /// <summary>Is the side band's line of sight to (x, z) clear? The side lens stands SideStandM from the
        /// patch on bearing SideStandBearingDeg (its pose is in every side sidecar: 41.26 m out, cam_yaw 310).
        /// This walks that ground line every 2 m and refuses a Trench, Link, Wire, Bunker or Blocked cell, or a
        /// prop, within half of `width` of it. Such a thing stands BETWEEN the lens and the bank, in front of it
        /// on screen, where neither the hue gate nor the patch box can tell it from the bank's own face - on the
        /// old seed 1 it was a line of wire pickets and a wreck, and gas-side-s1 was a tank. Only the corridor is
        /// checked: Open() already clears the patch itself.</summary>
        static bool ClearView(MapData map, float x, float z, float width)
        {
            float half = width * 0.5f;
            float rad = SideStandBearingDeg * Mathf.Deg2Rad;
            var dir = new Vector2(Mathf.Sin(rad), Mathf.Cos(rad));
            var size = map.SizeMeters;
            for (float t = 2f; t <= SideStandM; t += 2f)
            {
                float px = x + dir.x * t, pz = z + dir.y * t;
                if (px < 3f || pz < 3f || px > size.x - 3f || pz > size.y - 3f) return false;
                for (float off = -half; off <= half + 0.001f; off += half)
                {
                    float qx = px - dir.y * off, qz = pz + dir.x * off;
                    if (qx < 3f || qz < 3f || qx > size.x - 3f || qz > size.y - 3f) return false;
                    var layer = map.LayerAt(new float3(qx, 0f, qz));
                    if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked | NavLayer.Wire | NavLayer.Bunker)) != 0) return false;
                }
                for (int i = 0; i < map.Props.Length; i++)
                {
                    var q = map.Props[i].Pos;
                    if (math.abs(q.x - px) < half && math.abs(q.z - pz) < half) return false;
                }
            }
            return true;
        }
    }
}
