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
// R5-2 (2026-10-20, plan step 3): the point is chosen by the DISAGREEMENT share - cells IsWater calls wet while
// WaterDepth is under SplashDepth - and when the field's best is under 0.5 the test MAKES that ground: up to three
// warm-up barrages at the point, 6 s apart, re-probing between them, because dressed shell holes are what turns
// IsWater true without filling to SplashDepth. The 25 m dryness is re-checked after each round; a point whose
// craters now hold real water is dropped for the next best. Every round's share goes into moments.txt. The baseline
// toggle nulls WaterDepth only after the warm-up and the plates, so both codes stand on the same dressed ground.
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

        /// <summary>The deepest standing water on a 2 m grid within `r` of (x, z), how many of those cells IsWater
        /// calls wet, and how many of them DISAGREE: IsWater true while WaterDepth is under SplashDepth. The
        /// disagreeing cells are exactly the ground where the old code splashes blue and the new code throws earth,
        /// so their share - not the wet share - is what stages this A/B (plan step 3, 2026-10-20).</summary>
        static (float deepest, int wet, int split, int cells) Water(float x, float z, float r)
        {
            float deepest = 0f; int wet = 0, split = 0, cells = 0;
            for (float zz = z - r; zz <= z + r; zz += 2f)
                for (float xx = x - r; xx <= x + r; xx += 2f)
                {
                    cells++;
                    float d = SceneHooks.WaterDepth != null ? SceneHooks.WaterDepth(xx, zz) : 0f;
                    deepest = Mathf.Max(deepest, d);
                    bool iw = SceneHooks.IsWater != null && SceneHooks.IsWater(xx, zz);
                    if (iw) wet++;
                    if (iw && d < CombatFx.SplashDepth) split++;
                }
            return (deepest, wet, split, cells);
        }

        /// <summary>The disagreement share within 12 m of (x, z): the frame the plume is drawn in.</summary>
        static float SplitShare(float x, float z)
        {
            var w = Water(x, z, 12f);
            return w.cells > 0 ? w.split / (float)w.cells : 0f;
        }

        /// <summary>No standing water over SplashDepth within 25 m - the shells scatter wider than the frame, and one
        /// landing in real water makes BOTH codes splash, which killed r5's first take.</summary>
        static bool DryWide(float x, float z) => Water(x, z, 25f).deepest <= CombatFx.SplashDepth;

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

            // critic r1, fix 2 and plan step 3 (2026-10-20): dry ground is not enough, and neither is damp ground.
            // What shows the bug is the DISAGREEMENT: cells IsWater calls wet while WaterDepth is under SplashDepth.
            // There the old code splashes blue and the new code throws earth; everywhere else both codes agree and the
            // A/B comes back near pixel-identical (r5: 0.0183 against 0.0172). Score the share of such cells within
            // 12 m, keep the 25 m dryness rule, and pick the highest.
            float x = -1f, z = -1f, deepest = 0f; int wet = 0, split = 0, cells = 0; float share = -1f;
            float bestX = -1f, bestZ = -1f, bestDeep = 0f; int bestWet = 0, bestSplit = 0, bestCells = 0; float bestShare = -1f;
            // the whole field, less the room the camera and the scatter need, at 2 m.
            var ranked = new System.Collections.Generic.List<(float sh, float xx, float zz)>();
            for (float zz = 14f; zz < size.y - 14f; zz += 2f)
                for (float xx = 14f; xx < size.x - 14f; xx += 2f)
                {
                    if (!VfxStills.Open(map, xx, zz, room)) continue;
                    var w = Water(xx, zz, 12f);   // 12 m: the shells' scatter, and the frame the plume is drawn in.
                    float sh = w.cells > 0 ? w.split / (float)w.cells : 0f;
                    if (sh > bestShare) { bestShare = sh; bestX = xx; bestZ = zz; bestDeep = w.deepest; bestWet = w.wet; bestSplit = w.split; bestCells = w.cells; }
                    // dry over 25 m, not 12: the shells scatter wider than the frame, and one of them landing in the
                    // river makes BOTH codes splash - which is how the first r5 take came back with the before and
                    // after shot pixel for pixel the same (2026-10-19).
                    if (!DryWide(xx, zz)) continue;
                    ranked.Add((sh, xx, zz));
                    if (sh <= share) continue;
                    share = sh; x = xx; z = zz; deepest = w.deepest; wet = w.wet; split = w.split; cells = w.cells;
                }
            if (x < 0f) { x = bestX; z = bestZ; deepest = bestDeep; wet = bestWet; split = bestSplit; cells = bestCells; share = bestShare; }
            Assert.Greater(x, 0f, "no open ground for a barrage at all");
            ranked.Sort((p, q) => q.sh.CompareTo(p.sh));   // best first: the fallbacks when a warm-up floods a point
            TestContext.Out.WriteLine("barrage at " + x + ", " + z + ": deepest water " + deepest.ToString("0.###")
                + " m, IsWater wet " + wet + " of " + cells + ", disagreeing " + split
                + " (share " + share.ToString("0.###") + ")");

            var log = new System.Text.StringBuilder();
            log.Append("search: the disagreement share (IsWater wet AND WaterDepth < SplashDepth ")
               .Append(CombatFx.SplashDepth).Append(" m) of cells on a 2 m grid within 12 m, no water over SplashDepth within 25 m\n");
            log.Append("round 0: ").Append(x).Append(", ").Append(z).Append(" share ").Append(share.ToString("0.####"))
               .Append(" (").Append(split).Append(" of ").Append(cells).Append(" cells; IsWater wet ").Append(wet)
               .Append("; deepest ").Append(deepest.ToString("0.###")).Append(" m)\n");

            // plan step 3: when the field's best point is under 0.5, MAKE the ground. Dressed shell holes are what
            // turns IsWater true without filling to SplashDepth - the bug's real habitat - so fire a warm-up barrage,
            // let it land and settle, and re-probe. Up to 3 rounds. A crater that now holds water deeper than
            // SplashDepth would make both codes splash, so re-check the 25 m dryness and drop the point if it does.
            int nextRank = 0;
            for (int round = 1; round <= 3 && share < 0.5f; round++)
            {
                TestContext.Out.WriteLine("warm-up round " + round + ": barrage at " + x + ", " + z
                    + " to dress the ground (share " + share.ToString("0.###") + " is under 0.5)");
                VfxLab.Scene("barrage", x, z);
                float until = Time.time + 6f;                       // the shells land and the holes settle
                while (Time.time < until) yield return null;
                bool dry = DryWide(x, z);
                float sh2 = SplitShare(x, z);
                var w2 = Water(x, z, 12f);
                log.Append("round ").Append(round).Append(": warm-up barrage at ").Append(x).Append(", ").Append(z)
                   .Append(" -> share ").Append(sh2.ToString("0.####")).Append(" (").Append(w2.split).Append(" of ")
                   .Append(w2.cells).Append(" cells; deepest ").Append(w2.deepest.ToString("0.###")).Append(" m)")
                   .Append(dry ? "" : "; FLOODED over 25 m, point dropped").Append('\n');
                if (!dry)
                {
                    // move to the next best point and start its rounds there
                    while (nextRank < ranked.Count && (Mathf.Approximately(ranked[nextRank].xx, x) && Mathf.Approximately(ranked[nextRank].zz, z)))
                        nextRank++;
                    if (nextRank >= ranked.Count) { log.Append("no point left to move to\n"); break; }
                    x = ranked[nextRank].xx; z = ranked[nextRank].zz; nextRank++;
                    share = SplitShare(x, z);
                    log.Append("moved to ").Append(x).Append(", ").Append(z).Append(" share ").Append(share.ToString("0.####")).Append('\n');
                    continue;
                }
                share = sh2; deepest = w2.deepest; wet = w2.wet; split = w2.split; cells = w2.cells;
            }
            bool fallback = share < 0.5f;
            string note = fallback
                ? " (FALLBACK: the best disagreement share after the warm-up rounds is only " + share.ToString("0.###") + ", under 0.5)"
                : "";
            log.Append("final: barrage at ").Append(x).Append(", ").Append(z)
               .Append("; disagreement share ").Append(share.ToString("0.####")).Append(" (").Append(split)
               .Append(" of ").Append(cells).Append(" cells, IsWater wet ").Append(wet)
               .Append("); deepest standing water ").Append(deepest.ToString("0.###")).Append(" m, SplashDepth ")
               .Append(CombatFx.SplashDepth).Append(" m").Append(note)
               .Append(baseline ? "; BASELINE run: SceneHooks.WaterDepth nulled after the plates, CombatFx on the old IsWater test" : "")
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
