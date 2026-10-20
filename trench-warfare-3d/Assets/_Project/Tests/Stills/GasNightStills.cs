// Phase: gas-smoke-night-g1 (the owner, 2026-10-20: the night contrast must sit outside the spread of three day
// captures, from one locked camera). The instrument: the gas bank and the smoke bank, each on its own open ground,
// each filmed three times at the zoom the game is played at (30), 0.5 s apart starting at ten seconds of captured
// time, so the three catch the flipbook moving across the bank's full spread. The camera is locked across the whole
// run: CameraShake off (CameraShake.Strength = 0, Reset()) and CaptureRig.Rig.YawPin pinned, instead of writing
// TacticalCamera.BaseYaw (which the rig eases back toward its own target the next frame).
// Env: TW_STILLS_DIR the folder (default %TEMP%/tw-gasnight), TW_STILLS_FIELD the look (NightMud is the night, Winter
// the day field) — both VfxStills' own, so no new flag. It asserts only that the stills were written: the numbers come
// from docs/evidence/gas-smoke-night-g1/probe.py, measured off the pixels.
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
            // the camera held dead still for the whole run: a shell burst near a man wrote CameraShake's 1-10 m
            // push into the pose a frame after CaptureRig posed it (pose_error_m 1.51 on the smoke site), and
            // writing BaseYaw lost its value to the rig easing yaw back toward its own target a frame later
            CameraShake.Strength = 0f; CameraShake.Reset();
            CaptureRig.Rig.YawPin = -90f;

            var map = Host.Local.Map;
            var size = map.SizeMeters;
            int written = 0;
            const int Repeats = 3;   // hard-coded: measures the bank's own spread, not a tunable
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
                // half-life, so this is the bank at its fullest spread. Three shots, 0.5 s apart, stay inside that
                // decay (~4 % over the three) while the flipbook moves between them: that wobble is the spread.
                float start = Time.time + 10f;
                for (int k = 0; k < Repeats; k++)
                {
                    float due = start + 0.5f * k;
                    while (Time.time < due) yield return null;
                    string path = Path.Combine(dir, $"{scene}_{stem}_{k}.png");
                    CaptureRig.Shot(path, x, z, 30f, 40f, 25f, 1280, 720, 1f);
                    yield return Drain(path);
                    if (File.Exists(path)) written++;
                }
            }
            Time.captureDeltaTime = 0f;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            CameraShake.Strength = 1f;
            CaptureRig.Rig.YawPin = float.NaN;
            TestContext.Out.WriteLine("wrote " + written + " stills to " + dir);
            yield return new ExitPlayMode();
            Assert.AreEqual(6, written, "all six stills must be written to " + dir);
        }
    }
}
