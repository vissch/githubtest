// Phase: deaths (2026-09-28, tooling) — the look at the absurd deaths (DeathGags), from batch mode with nobody driving,
// the way WalkerStills photographs the walkers (read its header for why it is an [Explicit] EditMode test that enters
// Play itself). Each scene is staged by Editor/DeathLab through the sim's own systems (a shell, a machine gun, a tank
// driven over a row, a gas call, fire, the beam) and filmed as a strip of stills from the side, then tiled into a sheet.
//
// Run it by name WITH a graphics device (not -nographics):
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter DeathStills \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_STILLS_DIR where the stills go (default %TEMP%/tw-deathstills: never inside the checkout, whose
// untracked files land.py would see), TW_DEATH_ABSURD the intensity (default 1; 0 films today's deaths for a before),
// TW_DEATH_SCENES a comma list (default every scene DeathLab knows), TW_STILLS_FIELD the look (NightMud, the scene's own,
// by default; Winter is the day field, overcast and snowed on).
// It asserts only that stills were written: it is an instrument; the pictures go to a critique.
using System.Collections;
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
    public class DeathStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        static readonly string[] AllScenes = { "shell", "heap", "shot", "mg", "wounds", "fire", "gas", "beam", "crush" };

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        /// <summary>How each scene is framed: zoom, yaw off the pinned base yaw, pitch, and the height aimed at. Close and
        /// three-quarters for a man shot (seen in the first sets: a punt reads at zoom 13), wide and low for a shell's arcs.</summary>
        static (float zoom, float yaw, float pitch, float aim) Frame(string scene)
        {
            switch (scene)
            {
                case "shell": case "heap": return (26f, 40f, 14f, 4f);
                case "frogheap": case "frogshell": return (22f, 220f, 32f, 3f);   // from the other side: from 40 deg a house near the camera hid the row (rounds 5-10)   // steeper: low at 18 deg, a house near the camera hid the row (rounds 5-8)   // closer: at 26 a frog was a speck (round 3)
                case "frogs": return (28f, 40f, 24f, 4f);   // the Hopper 9 m up in the frame too (round 5: off the top)
                case "frogshot": case "frogmg": return (22f, 40f, 32f, 1.2f);   // the row and its shooters 18 m off
                case "crush": return (20f, 40f, 18f, 1.5f);
                case "beam": return (10f, 40f, 32f, 0.4f);
                case "parts": return (9f, 40f, 38f, 0.1f);   // a row of parts on the ground, close
                case "machine": case "maw": case "salvo": return (30f, 40f, 22f, 6f);  // a turret's leap: wide and low, aimed up so its 14 m peak stays in (critic round 16 found it cut off)
                case "skimmer": return (40f, 40f, 26f, 2f);   // its fan glides 18-27 m astern
                case "walker": return (18f, 40f, 20f, 1.5f);  // a belly-flop: close and low
                case "croaker": return (24f, 220f, 30f, 3f);
                case "hopper": return (36f, 40f, 30f, 4f);   // it flies 9 m up and falls: all of it in (round 1: off the top)
                default: return (13f, 40f, 20f, 1.2f);
            }
        }

        /// <summary>The wounds scene turns about the row, a still each way in turn: close from the three-quarter front and
        /// from behind, high and steep, and the zoom the game is played at (hit blood must read at all four).</summary>
        static (float zoom, float yaw, float pitch, float aim) WoundView(int k)
        {
            switch (k % 4)
            {
                case 0: return (9f, 40f, 28f, 1.0f);
                case 1: return (9f, 220f, 28f, 1.0f);
                case 2: return (14f, 130f, 55f, 0.8f);
                default: return (30f, 40f, 25f, 0.5f);
            }
        }

        /// <summary>How long each scene takes to play out, and how many stills it gets across that time.</summary>
        static (float seconds, int frames) Timing(string scene)
        {
            switch (scene)
            {
                case "shell": case "heap": return (4f, 12);
                case "frogheap": case "frogshell": return (5f, 12);   // shelled 0.8 s in: the row seen standing first
                case "frogs": return (3f, 4);
                case "shot": case "mg": case "wounds": case "frogshot": case "frogmg": return (14f, 16);
                case "crush": return (14f, 16);
                case "fire": return (12f, 12);
                case "gas": return (22f, 12);
                case "beam": return (16f, 16);
                case "parts": return (4f, 4);
                case "machine": case "maw": case "salvo": case "skimmer": case "walker": case "croaker": case "hopper": return (8f, 16);
                default: return (8f, 12);
            }
        }

        /// <summary>When still k of a scene is taken, in seconds from its staging. A machine is shelled 1.5 s in
        /// (DeathLab.Machine), and its hop, a walker's pop and a turret's leap are over within a second: evenly 0.53 s apart
        /// they fell between stills (critic round 7), so a machine's first twelve stills are 0.16 s apart from 1.4 s and
        /// the last four are spread to the end.</summary>
        static float Due(string scene, int k, float seconds, int frames)
        {
            bool machine = scene == "machine" || scene == "maw" || scene == "salvo" || scene == "skimmer" || scene == "walker" || scene == "croaker" || scene == "hopper";
            if (!machine || frames < 16) return seconds * k / Mathf.Max(1, frames - 1);
            if (k < 12) return 1.4f + 0.16f * k;
            return 3.2f + (seconds - 3.2f) * (k - 11) / 4f;
        }

        /// <summary>A patch of open ground for a scene: no trench, ladder, wire, bunker or blocked cell (deep water is blocked)
        /// from 4 m before (x, z - back) to 4 m past (x + w, z + ahead), and no prop on the row's own strip. The first
        /// staging stood a row on the bridge.</summary>
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
                if (q.x > x - 6f && q.x < x + w + 6f && q.z > z - back - 6f && q.z < z + 4f) return false;   // the row's strip and the ground between it and the camera (frog round 4: a wall hid half a row)
            }
            return true;
        }

        /// <summary>The crush machine's road: nothing it cannot drive through (a blocked cell, a bunker) within 4 m of x
        /// from z0 to z1. A trench it bridges and wire it flattens (the first sets asked for open ground and found none).</summary>
        /// <summary>No building (SceneHooks.StandingTall: a house, a ruin) within 3 m of the box, sampled every 3 m: the
        /// row, the ground between it and the camera, and the line to its shooters (frog rounds 5-7: a black house hid
        /// the shell scenes, a ruin took the gunner's fire).</summary>
        static bool Tall(MapData map, float x0, float x1, float z0, float z1)
        {
            // the ruins and walls the sim holds (what men take cover behind): a house the prop layer does not know (r8 test)
            if (map.StaticCover.IsCreated)
                for (int i = 0; i < map.StaticCover.Length; i++)
                {
                    var cv = map.StaticCover[i];
                    if (cv.OwnerSlot >= 0) continue;
                    float r = cv.Radius * 0.5f + 2f;
                    if (cv.Center.x > x0 - r && cv.Center.x < x1 + r && cv.Center.z > z0 - r && cv.Center.z < z1 + r) return false;
                }
            var hook = TW.Presentation.SceneHooks.StandingTall;
            if (hook == null) return true;
            for (float zz = z0; zz <= z1; zz += 3f)
                for (float xx = x0; xx <= x1; xx += 3f)
                    if (hook(xx, zz, 8f) < 3f) return false;
            return true;
        }

        /// <summary>A scene whose men are shot by others standing 18-30 m off.</summary>
        static bool Shooters(string scene) => scene == "frogshot" || scene == "frogmg";

        static bool Road(MapData map, float x, float z0, float z1)
        {
            for (float zz = z0; zz <= z1; zz += 2f)
                for (float xx = x - 4f; xx <= x + 4f; xx += 2f)
                    if ((map.LayerAt(new float3(xx, 0f, zz)) & (NavLayer.Blocked | NavLayer.Bunker)) != 0) return false;
            return true;
        }

        [UnityTest, Explicit("The absurd deaths photographed; run by name, with a graphics device.")]
        public IEnumerator EveryDeathGagIsPhotographed()
        {

            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            // the look: set on the scene before Play (a serialized field survives the domain reload; the scene is not saved)
            if (System.Enum.TryParse(System.Environment.GetEnvironmentVariable("TW_STILLS_FIELD"), true, out TW.Presentation.Terrain.Biome field))
            {
                foreach (var g in Object.FindObjectsByType<TW.Presentation.Terrain.GreyboxTerrainView>(FindObjectsSortMode.None)) g.Field = field;
                foreach (var at in Object.FindObjectsByType<TW.Presentation.Terrain.Atmosphere>(FindObjectsSortMode.None)) at.Field = field;
            }
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            // read AFTER entering Play: the domain reloads there, and locals set before it come back as defaults
            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-deathstills");
            Directory.CreateDirectory(dir);
            float absurd = float.TryParse(System.Environment.GetEnvironmentVariable("TW_DEATH_ABSURD"), System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture, out float a) ? a : 1f;
            string wanted = System.Environment.GetEnvironmentVariable("TW_DEATH_SCENES");
            string[] scenes = string.IsNullOrEmpty(wanted) ? AllScenes : wanted.Split(',');
            // the crush machine drives on 15 m past its row: filmed last, it cannot park in another scene's shot
            System.Array.Sort(scenes, (p, q) => (p == "crush" ? 1 : 0).CompareTo(q == "crush" ? 1 : 0));
            // quiet: no enemy deploys or attacks, no stray shells (the scenes make their own deaths)
            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            for (int f = 0; f < 120; f++) yield return null;
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;
            // a still costs about 0.3 s of game time to capture (the clock runs on through the stall, measured round 16),
            // so stills asked 0.16 s apart landed 0.33 s apart: the game clock steps 1/30 s a frame while filming
            Time.captureDeltaTime = 1f / 30f;
            var tc = Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
            TestContext.Out.WriteLine(DeathLab.Absurd(absurd));

            var map = Host.Local.Map;
            var size = map.SizeMeters;
            int written = 0;
            var used = new System.Collections.Generic.List<Vector2>();
            string tag = absurd.ToString("0.#", System.Globalization.CultureInfo.InvariantCulture).Replace('.', '_');
            foreach (string scene in scenes)
            {
                // open ground a row of men wide
                float x = -1f, z = -1f;
                for (float zz = 40f; zz < size.y - 50f && x < 0f; zz += 6f)
                    for (float xx = 8f; xx < size.x - 24f && x < 0f; xx += 6f)
                    {
                        bool clear = true;
                        foreach (var u in used) if (Mathf.Abs(u.x - xx) < 28f && Mathf.Abs(u.y - zz) < 34f) clear = false;   // frog round 2: a scene's survivors stood in the next one's frame
                        // the row's own ground (shooters may stand across a trench); the crush machine drives from 15 m short
                        // of the row, at its middle, so its road must hold nothing it cannot drive through
                        bool road = scene != "crush" || Road(map, xx + 3f, zz - 18f, zz + 6f);
                        if (Shooters(scene) && !Open(map, xx + 2f, zz + 12f, 4f, 2f, 2f)) road = false;
                        if (scene.StartsWith("frog") && !Tall(map, xx - 16f, xx + 20f, zz - 18f, Shooters(scene) ? zz + 12f : zz + 6f)) road = false;   // the camera looks in on a diagonal from well back: all round
                        if (scene == "beam" && xx < 14f) road = false;   // the beam is called 10 m short of the row: on the map
                        if (clear && road && Open(map, xx, zz, 14f, scene.StartsWith("frog") ? 10f : 5f, 5f)) { x = xx; z = zz; }   // a frog scene keeps 10 m clear toward the camera (round 5: a ruin in front of the MG row)
                    }
                if (x < 0f) { TestContext.Out.WriteLine(scene + ": no open ground left"); continue; }
                used.Add(new Vector2(x, z));
                TestContext.Out.WriteLine(scene + " at " + x + ", " + z + ": " + DeathLab.Scene(scene, x, z));
                var (seconds, frames) = Timing(scene);
                float start = Time.time;
                string stem = $"a{tag}_{scene}";
                for (int k = 0; k < frames; k++)
                {
                    float due = start + Due(scene, k, seconds, frames) + (scene.StartsWith("frog") && k == 0 ? 0.15f : 0f);   // the first still once the row is drawn (round 6: an empty field)
                    while (Time.time < due) yield return null;
                    string path = Path.Combine(dir, $"{stem}_{k:00}.png");
                    // the base yaw pinned before every shot, as WalkerStills does, or the shot's yaw drifts with the camera's own
                    var fr = scene == "wounds" ? WoundView(k) : Frame(scene);
                    if (tc != null) tc.BaseYaw = -90f;
                    CaptureRig.Shot(path, x + 4f, z - 2f + (Shooters(scene) && scene != "wounds" ? 5f : 0f), fr.zoom, fr.yaw, fr.pitch, 1280, 720, fr.aim);   // the shooters in the frame too
                    yield return Drain(path);
                    if (File.Exists(path)) written++;
                }
                CaptureRig.Sheet(dir, stem, Path.Combine(dir, stem + "_sheet.png"), 4, 480);
                // the aftermath at the standard view, the zoom the game is played at
                string wide = Path.Combine(dir, $"{stem}_standard.png");
                if (tc != null) tc.BaseYaw = -90f;
                CaptureRig.Shot(wide, x + 4f, z, 30f, 40f, 25f, 1280, 720);
                yield return Drain(wide);
                for (int f = 0; f < 30; f++) yield return null;
            }
            DeathLab.Absurd(-1f);
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            Time.captureDeltaTime = 0f;
            yield return new ExitPlayMode();
            TestContext.Out.WriteLine($"wrote {written} stills to {dir}");
            Assert.That(written, Is.GreaterThan(scenes.Length * 4), "too few stills landed to judge the deaths");
        }
    }
}
