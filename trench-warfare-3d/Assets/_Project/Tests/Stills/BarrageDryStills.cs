// Phase: VFX pass (2026-10-19, ability-fx-captures-r4) - the HE barrage photographed on DRY ground, after the harsh
// critic's fix 2: the barrage drew the water ability's blue plume, because CombatFx read SceneHooks.IsWater, which
// calls a painted damp patch and every dressed shell hole on the flooded night field "water". The fix reads
// SceneHooks.WaterDepth instead (metres), so this test's job is the A/B that proves it from pixels.
// It shoots four stills at one pose: a no-effect plate at T1 (zoom 30) and O240 BEFORE the ability is called, so no
// area marker is in frame, then the barrage at the same two bands, 0.4 s after the sim's first Explosion near the
// point. probe.py diffs each shot against its own plate, so the mask is the effect and nothing else.
// Env: TW_BARRAGEDRY_DIR the out folder (default <project>/../Captures/barragedry); TW_BARRAGEDRY_BASELINE=1 nulls
// SceneHooks.WaterDepth after the point is chosen, which drops CombatFx back on the old IsWater test - that run is
// the "before" picture, taken by the same recipe at the same pose.
// moments.txt records the point, how many probed cells IsWater called wet and the deepest water found: that line is
// the diagnosis in writing.
using System.Collections;
using System.IO;
using NUnit.Framework;
using UnityEngine;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using TW.Editor;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Sim;
using TW.Sim.Match;
using TW.UI;

namespace TW.Tests
{
    public class BarrageDryStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        /// <summary>The moment the barrage's first shell goes off near the point. A named class, not a local function:
        /// EventPump names a subscriber from its declaring type.</summary>
        sealed class Latch
        {
            public SimEventType Want; public Vector2 Near; public float At = -1f;
            public void Hear(SimEvent e)
            {
                if (e.Type != Want || At > 0f) return;
                if (Mathf.Abs(e.Pos.x - Near.x) > 25f || Mathf.Abs(e.Pos.z - Near.y) > 25f) return;
                At = Time.time;
            }
        }

        /// <summary>The deepest standing water on a 2 m grid within `r` of (x, z), and how many of those cells
        /// IsWater calls wet. The two disagree on damp ground: that disagreement is the bug being fixed.</summary>
        static (float deepest, int wet, int cells) Water(float x, float z, float r)
        {
            float deepest = 0f; int wet = 0, cells = 0;
            for (float zz = z - r; zz <= z + r; zz += 2f)
                for (float xx = x - r; xx <= x + r; xx += 2f)
                {
                    cells++;
                    if (SceneHooks.WaterDepth != null) deepest = Mathf.Max(deepest, SceneHooks.WaterDepth(xx, zz));
                    if (SceneHooks.IsWater != null && SceneHooks.IsWater(xx, zz)) wet++;
                }
            return (deepest, wet, cells);
        }

        [UnityTest, Explicit("The HE barrage on dry ground, with a no-effect plate at each band; run by name, with a graphics device.")]
        public IEnumerator BarrageOnDryGroundAtTwoBands()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            // critic r1, fix 1: r4 shot barrage-t1 with the yellow aim disc on, so effect_area measured the marker.
            // Overlays off and shake 0 keeps the plate and the shot on one camera: the mask is the burst, nothing else.
            TestContext.Out.WriteLine("quiet frame: " + CaptureRig.QuietFrame());

            string dir = System.Environment.GetEnvironmentVariable("TW_BARRAGEDRY_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetDirectoryName(Application.dataPath), "Captures", "barragedry");
            Directory.CreateDirectory(dir);
            bool baseline = System.Environment.GetEnvironmentVariable("TW_BARRAGEDRY_BASELINE") == "1";

            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            for (int f = 0; f < 120; f++) yield return null;
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;   // the weather pinned: the plate and the shot are the same evening
            Time.captureDeltaTime = 1f / 30f;                      // the clock runs while filming (CaptureRig.Hold would stop it)
            var tc = Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();

            var host = Host;
            var map = host.Local.Map;
            var size = map.SizeMeters;
            // 8 m, not VfxStills.Room("barrage") 12 m: the damp paint this test needs sits along the trenches, and the
            // 12 m rule (plus 18 m clear of static cover) rejects every damp patch on the field - the wettest point it
            // allows is 0.059 of cells wet against 0.198 in the open, and the baseline then shows no plume to speak of.
            // The plate is shot at the same pose, so a prop or a trench lip in frame subtracts out of the mask.
            const float room = 8f;

            // critic r1, fix 2: dry ground is not enough. r4's point was merely dry, so the OLD code drew earth there
            // too and the A/B was 0.36% of pixels apart. What shows the bug is ground with no standing water over
            // SplashDepth but a high share of cells the old IsWater test calls wet (a painted damp patch, dressed shell
            // holes): there the old code splashes blue and the new code throws earth. Pick the wettest such point.
            float x = -1f, z = -1f, deepest = 0f; int wet = 0, cells = 0; float share = -1f;
            float bestX = -1f, bestZ = -1f, bestDeep = 0f; int bestWet = 0, bestCells = 0; float bestShare = -1f;
            // the whole field, less the room the camera and the scatter need, at 2 m: on GreyboxCorridor the damp
            // paint sits where r4's narrower window (x 20, z 40, 6 m) never looked, and the wettest shallow ground
            // there reaches 0.225 of cells within 12 m against 0.071 inside it (measured 2026-10-19).
            for (float zz = 14f; zz < size.y - 14f; zz += 2f)
                for (float xx = 14f; xx < size.x - 14f; xx += 2f)
                {
                    if (!VfxStills.Open(map, xx, zz, room)) continue;
                    var w = Water(xx, zz, 12f);   // 12 m: the shells' scatter, and the frame the plume is drawn in.
                    // A 6 m core was tried on 2026-10-19 and read WORSE in the baseline (blue 0.0097 against 0.0665):
                    // the impacts that matter are spread over the frame, not piled on the aim point.
                    float sh = w.cells > 0 ? w.wet / (float)w.cells : 0f;
                    if (sh > bestShare) { bestShare = sh; bestX = xx; bestZ = zz; bestDeep = w.deepest; bestWet = w.wet; bestCells = w.cells; }
                    // dry over 25 m, not 12: the shells scatter wider than the frame, and one of them landing in the
                    // river makes BOTH codes splash - which is how the first r5 take came back with the before and
                    // after shot pixel for pixel the same (2026-10-19).
                    if (Water(xx, zz, 25f).deepest > CombatFx.SplashDepth) continue;
                    if (sh <= share) continue;
                    share = sh; x = xx; z = zz; deepest = w.deepest; wet = w.wet; cells = w.cells;
                }
            bool fallback = x < 0f || share < 0.5f;
            if (x < 0f) { x = bestX; z = bestZ; deepest = bestDeep; wet = bestWet; cells = bestCells; share = bestShare; }
            Assert.Greater(x, 0f, "no open ground for a barrage at all");
            string note = fallback
                ? " (FALLBACK: the wettest dry point reaches only " + share.ToString("0.###") + " of cells IsWater-wet, under 0.5)"
                : "";
            TestContext.Out.WriteLine("barrage at " + x + ", " + z + ": deepest water " + deepest.ToString("0.###")
                + " m, IsWater called " + wet + " of " + cells + " probed cells wet (share " + share.ToString("0.###") + ")" + note);

            var log = new System.Text.StringBuilder();
            log.Append("barrage at ").Append(x).Append(", ").Append(z)
               .Append(": deepest standing water ").Append(deepest.ToString("0.###")).Append(" m, SplashDepth ")
               .Append(CombatFx.SplashDepth).Append(" m; IsWater called ").Append(wet).Append(" of ").Append(cells)
               .Append(" probed cells wet (share ").Append(share.ToString("0.###")).Append(')').Append(note)
               .Append(baseline ? "; BASELINE run: SceneHooks.WaterDepth nulled, CombatFx on the old IsWater test" : "")
               .Append('\n');

            // the plates first: the ability is not called yet, so no area marker exists to draw
            int written = 0;
            foreach (var band in new[] { ("t1", 30f), ("o240", 240f) })
            {
                string path = Path.Combine(dir, "plate-" + band.Item1 + ".png");
                if (tc != null) tc.BaseYaw = -90f;
                CaptureRig.Shot(path, x, z, band.Item2, 21f, 25f, 1600, 900);
                yield return Drain(path);
                if (File.Exists(path)) written++;
            }
            log.Append("plate-t1 and plate-o240: the same pose, before the call, no area marker\n");

            if (baseline) SceneHooks.WaterDepth = null;   // the "before": CombatFx falls back on the old IsWater test

            var latch = new Latch { Want = SimEventType.Explosion, Near = new Vector2(x, z) };
            host.Events.OnEvent += latch.Hear;
            TestContext.Out.WriteLine("barrage staged: " + VfxLab.Scene("barrage", x, z));
            float start = Time.time;
            while (latch.At < 0f && Time.time < start + 20f) yield return null;   // the barrage warms up ~4 s before its first shell
            string moment;
            if (latch.At < 0f) moment = "Explosion never fired in 20 s: shot anyway";
            // 2 s, not r4's 0.4 s: at 0.4 s one or two shells have landed, and with 13% of the ground damp the old
            // code's blue plume was a coin flip - the before and after shot came back byte-identical (2026-10-19).
            // By 2 s a dozen shells have landed and their plumes overlap, and the T1 effect area is still 0.11.
            else { float due = latch.At + 2f; while (Time.time < due) yield return null; moment = "2 s after the first Explosion"; }
            log.Append("barrage-t1 and barrage-o240: ").Append(moment).Append('\n');
            TestContext.Out.WriteLine("barrage: " + moment);

            foreach (var band in new[] { ("t1", 30f), ("o240", 240f) })
            {
                string path = Path.Combine(dir, "barrage-" + band.Item1 + ".png");
                if (tc != null) tc.BaseYaw = -90f;
                CaptureRig.Shot(path, x, z, band.Item2, 21f, 25f, 1600, 900);
                yield return Drain(path);
                if (File.Exists(path)) written++;
            }

            host.Events.OnEvent -= latch.Hear;
            Time.captureDeltaTime = 0f;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            File.WriteAllText(Path.Combine(dir, "moments.txt"), log.ToString());
            TestContext.Out.WriteLine("wrote " + written + " stills to " + dir);
            yield return new ExitPlayMode();
            Assert.AreEqual(4, written, "four stills: a plate and a shot at each band");
        }
    }
}
