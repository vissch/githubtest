// Phase: gas-smoke-night (the owner, 2026-10-19: the ability clouds were tuned by day; at night black or grey smoke
// vanishes). The instrument for that one question: the gas bank and the smoke bank, each on its own open ground, each
// filmed once at the zoom the game is played at (30) after ten seconds of captured time, so the cloud is at full spread.
// Env: TW_STILLS_DIR the folder (default %TEMP%/tw-gasnight), TW_STILLS_FIELD the look (NightMud is the night, Winter
// the day field) — both VfxStills' own, so no new flag. It asserts only that the stills were written: the numbers come
// from docs/evidence/gas-smoke-night/probe.py, measured off the pixels.
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
    public class GasNightStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        /// <summary>Open ground for a cloud centred on (x, z): VfxStills' own test, so the two instruments stage alike.</summary>
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

        const float Room = 12f;     // a bank spreads: VfxStills' room for "gas" and "smoke"

        [UnityTest, Explicit("The gas and smoke banks photographed at zoom 30; run by name, with a graphics device.")]
        public IEnumerator GasAndSmokeBanksArePhotographed()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            string fieldName = System.Environment.GetEnvironmentVariable("TW_STILLS_FIELD");
            if (System.Enum.TryParse(fieldName, true, out TW.Presentation.Terrain.Biome field))
            {
                foreach (var g in Object.FindObjectsByType<TW.Presentation.Terrain.GreyboxTerrainView>(FindObjectsSortMode.None)) g.Field = field;
                foreach (var at in Object.FindObjectsByType<TW.Presentation.Terrain.Atmosphere>(FindObjectsSortMode.None)) at.Field = field;
            }
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-gasnight");
            Directory.CreateDirectory(dir);
            // named after the field actually filmed, read back off the sky, not off the variable asked for
            var skyNow = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            string stem = skyNow != null ? skyNow.Field.ToString().ToLowerInvariant() : "scene";
            TestContext.Out.WriteLine("field asked " + (fieldName ?? "(none)") + ", filmed " + stem);

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
            foreach (string scene in new[] { "gas", "smoke" })
            {
                float x = -1f, z = -1f;
                for (float zz = 40f; zz < size.y - 40f && x < 0f; zz += 6f)
                    for (float xx = 20f; xx < size.x - 20f && x < 0f; xx += 6f)
                    {
                        bool clear = true;
                        foreach (var u in used) if (Mathf.Abs(u.x - xx) < 34f && Mathf.Abs(u.y - zz) < 34f) clear = false;
                        if (clear && Open(map, xx, zz, Room)) { x = xx; z = zz; }
                    }
                Assert.Greater(x, 0f, scene + ": no open ground left");
                used.Add(new Vector2(x, z));
                TestContext.Out.WriteLine(scene + " at " + x + ", " + z + ": " + VfxLab.Scene(scene, x, z));
                // ten seconds of captured time: the gas warms up over 60 ticks and the field decays with a ~9 s
                // half-life, so this is the bank at its fullest spread
                float due = Time.time + 10f;
                while (Time.time < due) yield return null;
                string path = Path.Combine(dir, $"{scene}_{stem}.png");
                if (tc != null) tc.BaseYaw = -90f;
                CaptureRig.Shot(path, x, z, 30f, 40f, 25f, 1280, 720, 1f);
                yield return Drain(path);
                if (File.Exists(path)) written++;
            }
            Time.captureDeltaTime = 0f;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            TestContext.Out.WriteLine("wrote " + written + " stills to " + dir);
            yield return new ExitPlayMode();
            Assert.AreEqual(2, written, "both stills must be written to " + dir);
        }
    }
}
