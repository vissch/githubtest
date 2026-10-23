// Phase: VFX pass (2026-10-19, tooling) — the ability effects photographed against a no-effect plate of the same pose
// and the same sim tick, so a probe can measure how much of the frame the effect actually covers (harsh critic r1
// fix 3: "the probe never finds the effect in the frame; the yellow ability-area disc floods the T1 stills").
// Four subjects (smoke, HE barrage, a water splash, ParaDrop) at two bands (T1 zoom 30, O240 zoom 240).
// Env: TW_STILLS_DIR the folder (default %TEMP%/tw-abilityfx), TW_ABILITYFX_PLATE=1 runs the identical script with the
// staging call skipped — the plate pass. TW_ABILITYFX_REPS=N (default 1) shoots every subject and band N times over,
// naming the files subject_band_rN.png, so a bar can be set outside the spread of one build's own captures.
// TW_ABILITYFX_REP=N shoots only rep N and keeps the _rN name, so a rep can be a fresh editor run of its own.
// The drop's three reps are shot at THREE shot frames (DropPeaks 210/220/230), so they stand on three sim ticks and
// show three moments of the drop's life; TW_ABILITYFX_DROP_PEAK=a[,b,c] overrides them (rep N takes the Nth value).
// The drop is the one subject whose plate DOES stage: its 13 paratroopers, their fallen and their fire would otherwise
// be the whole diff, so the plate drops the same men on the same ticks with CombatFx.DropFx off (the g2 measurement,
// the owner 2026-10-21: "hold the plate's men, turn the beams off, so the mask is the drop and not the soldiers"). The ability-area disc is off (CombatFx.ShowOverlays), the clock is pinned and
// nothing else moves, so what differs between a still and its plate is the effect and nothing else.
// It asserts only that the stills were written and that the drop really came: the numbers belong to the probe
// (docs/evidence/ability-fx-captures-r3/probe.py).
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
    public class AbilityFxStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static bool PlatePass => System.Environment.GetEnvironmentVariable("TW_ABILITYFX_PLATE") == "1";

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        /// <summary>Open ground for an effect centred on (x, z): no trench, ladder, wire, bunker or blocked cell within
        /// `r` metres, no prop within r, and no ruin or wall the sim holds as cover within r + 6.
        /// Copied verbatim from VfxStills so both harnesses pick the same points in the same order.</summary>
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

        /// <summary>How much room each subject needs, how high the camera aims, how many frames after the staging its
        /// effect is at its fullest, and how long the whole block lasts in frames. Every wait is a frame count, never
        /// Time.time, so the plate pass shoots the same frames — and so the same sim ticks — as the still pass.</summary>
        static (float room, float aim, int peak, int block) Plan(string subject)
        {
            switch (subject)
            {
                // The sim runs at 20 ticks a second against 30 captured frames, so a frame is two thirds of a tick, and
                // a peak has to clear the ability's WarmupTicks (OffMapAbilities.cs) or the still is taken before the
                // payload arrives. Baseline r1 shot the barrage 11 ticks before its first shell and the drop 11 ticks
                // before its men were down - which is why both read 0.0000 against their plates.
                case "smoke": return (12f, 1f, 150, 260);         // warmup 40 ticks (60 frames)
                case "barrage": return (12f, 2f, 200, 320);       // warmup 80 ticks, 12 shells over 120 more: mid-barrage
                case "splash": return (7f, 3f, 20, 120);          // one shell, bursting the next tick
                // the drop's block runs 360 frames so the last rep's cards (longest life 5 s = 150 frames) are gone
                // before the next rep stages on the same point
                // 220 frames is the landing dust at its fullest: a sweep of 155 / 190 / 220 / 250 at zoom 30 showed a
                // faint haze at 155, a thin cloud at 190, a cloud over the whole landing at 220, and nothing left at 250.
                default: return (12f, 12f, 220, 360);             // drop: warmup 100 ticks (150 frames), the landing dust at its fullest
            }
        }

        /// <summary>The drop's shot frame, overridden per rep by TW_ABILITYFX_DROP_PEAK (one number, or a comma list
        /// where rep N takes the Nth value: that is a sweep of shot frames in one launch). -1 when unset.</summary>
        /// <summary>The drop's shot frame per rep: three moments of one drop, all inside the plateau a recorded sweep
        /// found (155 a faint haze, 190 a thin cloud, 220 a cloud over the whole landing, 250 nothing left). Three
        /// different frames put the three reps on three sim ticks by construction, so their spread is the drop's own
        /// variation and not render noise.</summary>
        static readonly int[] DropPeaks = { 210, 220, 230 };

        static int DropPeakOverride(int rep)
        {
            var raw = System.Environment.GetEnvironmentVariable("TW_ABILITYFX_DROP_PEAK");
            if (string.IsNullOrEmpty(raw)) return -1;
            var parts = raw.Split(',');
            var pick = parts[Mathf.Clamp(rep - 1, 0, parts.Length - 1)].Trim();
            return int.TryParse(pick, out int n) && n > 0 ? n : -1;
        }

        /// <summary>Paratroopers alive in the local world: how a drop is proved to have come down, without reading
        /// the event stream (the events are gone by the frame after the tick that raised them).</summary>
        static int Paras()
        {
            var h = Host;
            if (h == null || h.Local == null) return -1;
            var w = h.Local.World;
            int n = 0;
            for (int s = 0; s < w.HighWater; s++) if (w.IsAlive(s) && w.Archetype[s] == InfantryArchetype.Para) n++;
            return n;
        }

        [UnityTest, Explicit("The ability effects photographed against a plate; run by name, with a graphics device.")]
        public IEnumerator EveryAbilityEffectReadsAgainstItsPlate()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            // ParaDrop is a Brass ability (FactionRoster.MayCall) and player 0 is FactionA, so the player the calls are
            // made for has to be Brass or every drop is refused (OffMapAbilities.cs:202).
            foreach (var h in Object.FindObjectsByType<SimHost>(FindObjectsSortMode.None)) h.FactionA = (byte)FactionId.Brass;
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");

            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-abilityfx");
            Directory.CreateDirectory(dir);
            bool plate = PlatePass;

            TW.Presentation.Tactical.CombatFx.ShowOverlays = false;   // the yellow ability-area disc is drawn here (DrawAim)
            // the green and red night halo of the men's own fire (tracerNightA/B) washed over the drop's T1 frame; off in
            // both passes, so none of it reaches the mask
            TW.Presentation.Tactical.CombatFx.ShowTracers = false;
            // A burst shakes the camera, and the rig poses the camera one frame and photographs it the next, so the
            // shake pushed it off the pose it was given: baseline r1's splash came back with pose_error_m 4.4 at O240.
            TW.Presentation.Tactical.CameraShake.Strength = 0f;
            TW.Presentation.Tactical.CameraShake.Reset();
            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;
            Time.captureDeltaTime = 1f / 30f;
            var tc = Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();

            int written = 0;
            var map = Host.Local.Map;
            var size = map.SizeMeters;
            var abilities = Host.Local.World.GetSystem<OffMapAbilitySystem>();
            int dropped = 0;
            var used = new List<Vector2>();
            // One subject an editor run (TW_ABILITYFX_SUBJECT). A run that staged all four put the smoke of the first
            // block and the paratroopers of the last into the others' O240 frames, where a zoom of 240 sees the whole
            // 90 x 276 m map: the probe would have counted another subject's effect as this one's.
            string one = System.Environment.GetEnvironmentVariable("TW_ABILITYFX_SUBJECT");
            string[] subjects = string.IsNullOrEmpty(one) ? new[] { "smoke", "barrage", "splash", "drop" } : one.Split(',');
            // How many times each subject and band is shot. More than one is how the spread of a single build is
            // measured, so a bar can sit outside it.
            int reps = 1;
            int.TryParse(System.Environment.GetEnvironmentVariable("TW_ABILITYFX_REPS"), out reps);
            if (reps < 1) reps = 1;
            // TW_ABILITYFX_REP=N shoots exactly rep N, in its own editor run, and still writes the _rN suffix.
            int only = 0;
            int.TryParse(System.Environment.GetEnvironmentVariable("TW_ABILITYFX_REP"), out only);
            if (only < 1) only = 0;
            int firstRep = only > 0 ? only : 1, lastRep = only > 0 ? only : reps;
            bool suffix = only > 0 || reps > 1;
            // The point a subject and band were shot on, so every rep stands on the same ground. Open ground a drop may
            // land on is scarce, and because both passes now stage every rep, an earlier rep's men and craters sit in the
            // still AND in the plate and cancel.
            var points = new Dictionary<string, Vector2>();

            for (int f = 0; f < 120; f++) yield return null;   // let the field settle before the first block

            // One block per subject AND per band, on its own point, with the camera already at the band's zoom when
            // the effect is staged. Two things need that: the cards that grow with the zoom (FlipbookFx.FarGrow) read
            // CurrentZoom as they are born, not as the shot is posed, and the handlers that cull past a zoom see the
            // band they will be photographed at. The overview band goes FIRST so its frame, which holds the whole map,
            // cannot contain an earlier block's smoke, craters or paratroopers.
            for (int rep = firstRep; rep <= lastRep; rep++)
            foreach (string subject in subjects)
            foreach (float zoom in new[] { 240f, 30f })
            {
                string band = zoom < 100f ? "T1" : "O240";
                var pl = Plan(subject);
                if (subject == "drop") { int over = DropPeakOverride(rep); pl.peak = over > 0 ? over : DropPeaks[Mathf.Clamp(rep - 1, 0, DropPeaks.Length - 1)]; }
                // How far this band's point must stand from the one the other band used. The overview band runs first,
                // so this only keeps the standard view's frame clear of what the overview block left behind: water is
                // scarce and a splash is gone in 1.5 s, and open ground a drop can land on is scarcer still (7 points
                // on this map at room 12), so each gets what its own leavings need and no more.
                float apart = subject == "splash" ? 20f : subject == "drop" ? 34f : 60f;
                if (tc != null) { tc.BaseYaw = -90f; tc.Zoom = zoom; }
                float x = -1f, z = -1f;
                string key = subject + "_" + band;
                if (points.TryGetValue(key, out var had)) { x = had.x; z = had.y; }
                for (float zz = 40f; zz < size.y - 40f && x < 0f; zz += 6f)
                    for (float xx = 20f; xx < size.x - 20f && x < 0f; xx += 6f)
                    {
                        bool clear = true;
                        foreach (var u in used) if (Mathf.Abs(u.x - xx) < apart && Mathf.Abs(u.y - zz) < apart) clear = false;
                        if (!clear) continue;
                        if (subject == "splash")
                        {
                            // a wet point: the splash column only reads over water (CombatFx reads SceneHooks.IsWater)
                            if (SceneHooks.IsWater == null || !SceneHooks.IsWater(xx, zz)) continue;
                            x = xx; z = zz;
                            continue;
                        }
                        if (!Open(map, xx, zz, pl.room)) continue;
                        if (subject == "drop" && abilities != null && !abilities.CanLand(0, new float3(xx, 0f, zz))) continue;
                        x = xx; z = zz;
                    }
                if (x < 0f)
                {
                    int nOpen = 0, nLand = 0, nClear = 0;
                    for (float zz = 40f; zz < size.y - 40f; zz += 6f)
                        for (float xx = 20f; xx < size.x - 20f; xx += 6f)
                        {
                            bool clear = true;
                            foreach (var u in used) if (Mathf.Abs(u.x - xx) < apart && Mathf.Abs(u.y - zz) < apart) clear = false;
                            if (clear) nClear++;
                            if (Open(map, xx, zz, pl.room)) nOpen++;
                            if (abilities != null && abilities.CanLand(0, new float3(xx, 0f, zz))) nLand++;
                        }
                    TestContext.Out.WriteLine(subject + " " + band + ": no point found; map " + size.x + "x" + size.y
                        + ", cells clear " + nClear + ", open " + nOpen + ", CanLand " + nLand
                        + ", abilities " + (abilities == null ? "null" : "found"));
                    continue;
                }
                if (!points.ContainsKey(key)) { points[key] = new Vector2(x, z); used.Add(new Vector2(x, z)); }
                if (tc != null) tc.Focus = new Vector2(x, z);
                for (int f = 0; f < 10; f++) yield return null;   // the camera takes the new zoom and focus

                int before = Paras();
                // the drop's own drawing is the only thing the plate pass leaves out of a drop block
                TW.Presentation.Tactical.CombatFx.DropFx = !plate;
                if (!plate || subject == "drop")
                {
                    // the first band's call put the ability on its cooldown (HeBarrage 1200 ticks, ParaDrop 1500), which
                    // would refuse the second band's; cleared here, not in the sim
                    Host.WriteWorlds(m =>
                    {
                        var ab = m.World.GetSystem<OffMapAbilitySystem>();
                        if (ab != null && ab.Cooldown.IsCreated) for (int i = 0; i < ab.Cooldown.Length; i++) ab.Cooldown[i] = 0;
                    });
                    string said = subject == "smoke" ? VfxLab.Call((int)OffMapAbilityId.SmokeScreen, x, z)
                                : subject == "barrage" ? VfxLab.Call((int)OffMapAbilityId.HeBarrage, x, z)
                                : subject == "splash" ? VfxLab.Shell(x, z)
                                : VfxLab.Call((int)OffMapAbilityId.ParaDrop, x, z);
                    TestContext.Out.WriteLine(subject + " " + band + " at " + x + ", " + z + ": " + said);
                }
                else TestContext.Out.WriteLine(subject + " " + band + " at " + x + ", " + z + ": plate, nothing staged");
                if (plate && subject == "drop") TestContext.Out.WriteLine("drop " + band + ": plate staged the same drop with DropFx off");

                for (int f = 0; f < pl.peak; f++) yield return null;

                string path = Path.Combine(dir, subject + "_" + band + (suffix ? "_r" + rep : "") + (plate ? "_plate" : "") + ".png");
                CaptureRig.Shot(path, x, z, zoom, 40f, 25f, 1280, 720, pl.aim);
                yield return Drain(path);
                if (File.Exists(path)) written++;
                for (int f = 0; f < Mathf.Max(0, pl.block - pl.peak); f++) yield return null;   // the block is a fixed length either way

                if (subject == "drop")
                {
                    dropped += Paras() - before;
                    TestContext.Out.WriteLine("drop " + band + " r" + rep + ": paratroopers in the world " + before + " -> " + Paras());
                }
            }

            Time.captureDeltaTime = 0f;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            TW.Presentation.Tactical.CameraShake.Strength = 1f;
            TW.Presentation.Tactical.CombatFx.DropFx = true;
            TW.Presentation.Tactical.CombatFx.ShowTracers = true;
            TestContext.Out.WriteLine("wrote " + written + (plate ? " plates to " : " stills to ") + dir);
            yield return new ExitPlayMode();
            Assert.AreEqual(2 * subjects.Length * (lastRep - firstRep + 1), written, "a still is missing");
            if (System.Array.IndexOf(subjects, "drop") >= 0) Assert.Greater(dropped, 0, "no paratrooper came down: the drop was refused, so the paradrop still photographs nothing");
        }
    }
}
