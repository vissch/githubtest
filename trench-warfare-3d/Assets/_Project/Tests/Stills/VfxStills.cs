// Phase: VFX pass (2026-10-01, tooling) — the game's effects photographed one at a time for a blind critique (the owner,
// 2026-10-01: "go through all the vfx see how they can be better"). Each effect is staged by VfxLab.Scene on open ground,
// filmed as a contact sheet over its life, then shot once more at the zoom the game is played at.
// Env: TW_STILLS_DIR the folder (default %TEMP%/tw-vfxstills), TW_VFX_SCENES a comma list (default VfxLab.Scenes),
// TW_STILLS_FIELD the look (NightMud, the scene's own, by default; Winter is the day field, overcast and snowed on).
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
    public class VfxStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        /// <summary>How each effect is framed: zoom, yaw off the pinned base yaw, pitch, and the height aimed at.</summary>
        static (float zoom, float yaw, float pitch, float aim) Frame(string scene)
        {
            switch (scene)
            {
                case "shell": return (22f, 40f, 28f, 3f);
                case "barrage": return (45f, 40f, 32f, 2f);
                case "cookoff": return (26f, 40f, 28f, 4f);
                case "burning": return (22f, 40f, 28f, 3f);
                case "gas": case "smoke": return (40f, 40f, 34f, 1f);
                case "beam": return (40f, 40f, 30f, 4f);
                case "strafe": return (45f, 40f, 30f, 3f);
                case "fire": return (18f, 40f, 30f, 1f);
                case "flare": return (60f, 40f, 30f, 3f);
                case "rain": return (20f, 40f, 25f, 2f);
                default: return (24f, 40f, 28f, 2f);
            }
        }

        /// <summary>How long each effect takes to play out, and how many stills it gets across that time.</summary>
        static (float seconds, int frames) Timing(string scene)
        {
            switch (scene)
            {
                case "shell": return (5f, 12);
                case "barrage": return (14f, 16);
                case "cookoff": return (40f, 16);
                case "burning": return (16f, 12);
                case "gas": return (24f, 12);
                case "smoke": return (20f, 12);
                case "beam": return (12f, 16);
                case "strafe": return (10f, 16);
                case "fire": return (12f, 12);
                case "flare": return (16f, 8);
                case "rain": return (4f, 4);
                default: return (8f, 12);
            }
        }

        /// <summary>When still k is taken, in seconds from the staging. A cook-off is shelled 1.5 s in and over within
        /// seconds, then its wreck burns for minutes: ten stills 0.25 s apart from 1.4 s, the last six spread to the end.</summary>
        static float Due(string scene, int k, float seconds, int frames)
        {
            if (scene == "cookoff")
            {
                if (k < 10) return 1.4f + 0.25f * k;
                return 4f + (seconds - 4f) * (k - 9) / 6f;
            }
            return seconds * k / Mathf.Max(1, frames - 1);
        }

        /// <summary>Open ground for an effect centred on (x, z): no trench, ladder, wire, bunker or blocked cell within
        /// `r` metres, no prop within r, and no ruin or wall the sim holds as cover within r + 6.</summary>
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

        /// <summary>How much open ground an effect needs round its centre.</summary>
        static float Room(string scene) => scene == "barrage" || scene == "gas" || scene == "smoke" || scene == "strafe" || scene == "beam" ? 12f : 7f;

        [UnityTest, Explicit("The game's effects photographed; run by name, with a graphics device.")]
        public IEnumerator EveryEffectIsPhotographed()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            if (System.Enum.TryParse(System.Environment.GetEnvironmentVariable("TW_STILLS_FIELD"), true, out TW.Presentation.Terrain.Biome field))
            {
                foreach (var g in Object.FindObjectsByType<TW.Presentation.Terrain.GreyboxTerrainView>(FindObjectsSortMode.None)) g.Field = field;
                foreach (var at in Object.FindObjectsByType<TW.Presentation.Terrain.Atmosphere>(FindObjectsSortMode.None)) at.Field = field;
            }
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-vfxstills");
            Directory.CreateDirectory(dir);
            string wanted = System.Environment.GetEnvironmentVariable("TW_VFX_SCENES");
            string[] scenes = string.IsNullOrEmpty(wanted) ? VfxLab.Scenes : wanted.Split(',');
            // what lights or wets the whole field is filmed last, so it cannot spill into another effect's stills
            System.Array.Sort(scenes, (p, q) => Late(p).CompareTo(Late(q)));
            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            for (int f = 0; f < 120; f++) yield return null;
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;
            Time.captureDeltaTime = 1f / 30f;   // the game clock steps 1/30 s a frame while filming (a still stalls ~0.3 s)
            var tc = Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();

            var map = Host.Local.Map;
            var size = map.SizeMeters;
            int written = 0;
            var used = new System.Collections.Generic.List<Vector2>();
            foreach (string scene in scenes)
            {
                float room = Room(scene);
                float x = -1f, z = -1f;
                for (float zz = 40f; zz < size.y - 40f && x < 0f; zz += 6f)
                    for (float xx = 20f; xx < size.x - 20f && x < 0f; xx += 6f)
                    {
                        bool clear = true;
                        foreach (var u in used) if (Mathf.Abs(u.x - xx) < 34f && Mathf.Abs(u.y - zz) < 34f) clear = false;
                        if (clear && Open(map, xx, zz, room)) { x = xx; z = zz; }
                    }
                if (x < 0f) { TestContext.Out.WriteLine(scene + ": no open ground left"); continue; }
                used.Add(new Vector2(x, z));
                TestContext.Out.WriteLine(scene + " at " + x + ", " + z + ": " + VfxLab.Scene(scene, x, z));
                var (seconds, frames) = Timing(scene);
                float start = Time.time;
                string stem = $"v_{scene}";
                var fr = Frame(scene);
                for (int k = 0; k < frames; k++)
                {
                    float due = start + Due(scene, k, seconds, frames);
                    while (Time.time < due) yield return null;
                    string path = Path.Combine(dir, $"{stem}_{k:00}.png");
                    if (tc != null) tc.BaseYaw = -90f;
                    CaptureRig.Shot(path, x, z, fr.zoom, fr.yaw, fr.pitch, 1280, 720, fr.aim);
                    yield return Drain(path);
                    if (File.Exists(path)) written++;
                }
                CaptureRig.Sheet(dir, stem, Path.Combine(dir, stem + "_sheet.png"), 4, 480);
                string wide = Path.Combine(dir, $"{stem}_standard.png");
                if (tc != null) tc.BaseYaw = -90f;
                CaptureRig.Shot(wide, x, z, 30f, 40f, 25f, 1280, 720);
                yield return Drain(wide);
                if (scene == "rain") VfxLab.Rain(0f);
            }
            Time.captureDeltaTime = 0f;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            TestContext.Out.WriteLine("wrote " + written + " stills to " + dir);
            yield return new ExitPlayMode();
            Assert.Greater(written, 0, "no stills were written");
        }

        static int Late(string scene) => scene == "rain" ? 2 : scene == "flare" ? 1 : 0;
    }
}
