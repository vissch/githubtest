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
            float room = VfxStills.Room("barrage");

            // dry ground: open enough for a barrage, and no standing water over SplashDepth within 14 m. The driest
            // point found is kept as a fallback, so a flooded field is reported rather than shot as if it were dry.
            float x = -1f, z = -1f, deepest = 0f; int wet = 0, cells = 0;
            float bestX = -1f, bestZ = -1f, bestDeep = float.MaxValue; int bestWet = 0, bestCells = 0;
            for (float zz = 40f; zz < size.y - 40f && x < 0f; zz += 6f)
                for (float xx = 20f; xx < size.x - 20f && x < 0f; xx += 6f)
                {
                    if (!VfxStills.Open(map, xx, zz, room)) continue;
                    var w = Water(xx, zz, 14f);
                    if (w.deepest < bestDeep) { bestDeep = w.deepest; bestX = xx; bestZ = zz; bestWet = w.wet; bestCells = w.cells; }
                    if (w.deepest > CombatFx.SplashDepth) continue;
                    x = xx; z = zz; deepest = w.deepest; wet = w.wet; cells = w.cells;
                }
            bool fallback = x < 0f;
            if (fallback) { x = bestX; z = bestZ; deepest = bestDeep; wet = bestWet; cells = bestCells; }
            Assert.Greater(x, 0f, "no open ground for a barrage at all");
            TestContext.Out.WriteLine("barrage at " + x + ", " + z + ": deepest water " + deepest.ToString("0.###")
                + " m, IsWater called " + wet + " of " + cells + " probed cells wet"
                + (fallback ? " (FALLBACK: nothing drier than SplashDepth " + CombatFx.SplashDepth + " m)" : ""));

            var log = new System.Text.StringBuilder();
            log.Append("barrage at ").Append(x).Append(", ").Append(z)
               .Append(": deepest standing water ").Append(deepest.ToString("0.###")).Append(" m, SplashDepth ")
               .Append(CombatFx.SplashDepth).Append(" m; IsWater called ").Append(wet).Append(" of ").Append(cells)
               .Append(" probed cells wet").Append(fallback ? " (FALLBACK: no point drier than SplashDepth)" : "")
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
            else { float due = latch.At + 0.4f; while (Time.time < due) yield return null; moment = "0.4 s after the first Explosion"; }
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
