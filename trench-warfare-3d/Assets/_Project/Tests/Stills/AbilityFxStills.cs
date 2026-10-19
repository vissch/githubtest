// Phase: VFX pass (2026-10-19, tooling) — the ability effects photographed against a no-effect plate of the same pose
// and the same sim tick, so a probe can measure how much of the frame the effect actually covers (harsh critic r1
// fix 3: "the probe never finds the effect in the frame; the yellow ability-area disc floods the T1 stills").
// Four subjects (smoke, HE barrage, a water splash, ParaDrop) at two bands (T1 zoom 30, O240 zoom 240).
// Env: TW_STILLS_DIR the folder (default %TEMP%/tw-abilityfx), TW_ABILITYFX_PLATE=1 runs the identical script with the
// staging call skipped — the plate pass. The ability-area disc is off (CombatFx.ShowOverlays), the clock is pinned and
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
                case "smoke": return (12f, 1f, 150, 260);
                case "barrage": return (12f, 2f, 90, 200);
                case "splash": return (7f, 3f, 20, 120);
                default: return (12f, 12f, 120, 240);   // drop: WarmupTicks 100 + 20 frames
            }
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

            for (int f = 0; f < 120; f++) yield return null;   // let the field settle before the first block

            foreach (string subject in subjects)
            {
                var pl = Plan(subject);
                float x = -1f, z = -1f;
                for (float zz = 40f; zz < size.y - 40f && x < 0f; zz += 6f)
                    for (float xx = 20f; xx < size.x - 20f && x < 0f; xx += 6f)
                    {
                        bool clear = true;
                        foreach (var u in used) if (Mathf.Abs(u.x - xx) < 34f && Mathf.Abs(u.y - zz) < 34f) clear = false;
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
                            foreach (var u in used) if (Mathf.Abs(u.x - xx) < 34f && Mathf.Abs(u.y - zz) < 34f) clear = false;
                            if (clear) nClear++;
                            if (Open(map, xx, zz, pl.room)) nOpen++;
                            if (abilities != null && abilities.CanLand(0, new float3(xx, 0f, zz))) nLand++;
                        }
                    TestContext.Out.WriteLine(subject + ": no point found; map " + size.x + "x" + size.y
                        + ", cells clear " + nClear + ", open " + nOpen + ", CanLand " + nLand
                        + ", abilities " + (abilities == null ? "null" : "found"));
                    continue;
                }
                used.Add(new Vector2(x, z));

                int before = Paras();
                if (!plate)
                {
                    string said = subject == "smoke" ? VfxLab.Call((int)OffMapAbilityId.SmokeScreen, x, z)
                                : subject == "barrage" ? VfxLab.Call((int)OffMapAbilityId.HeBarrage, x, z)
                                : subject == "splash" ? VfxLab.Shell(x, z)
                                : VfxLab.Call((int)OffMapAbilityId.ParaDrop, x, z);
                    TestContext.Out.WriteLine(subject + " at " + x + ", " + z + ": " + said);
                }
                else TestContext.Out.WriteLine(subject + " at " + x + ", " + z + ": plate, nothing staged");

                for (int f = 0; f < pl.peak; f++) yield return null;

                foreach (float zoom in new[] { 30f, 240f })
                {
                    string band = zoom < 100f ? "T1" : "O240";
                    string path = Path.Combine(dir, subject + "_" + band + (plate ? "_plate" : "") + ".png");
                    if (tc != null) tc.BaseYaw = -90f;
                    CaptureRig.Shot(path, x, z, zoom, 40f, 25f, 1280, 720, pl.aim);
                    yield return Drain(path);
                    if (File.Exists(path)) written++;
                }
                for (int f = 0; f < Mathf.Max(0, pl.block - pl.peak); f++) yield return null;   // the block is a fixed length either way

                if (subject == "drop")
                {
                    dropped = Paras() - before;
                    TestContext.Out.WriteLine("drop: paratroopers in the world " + before + " -> " + Paras());
                }
            }

            Time.captureDeltaTime = 0f;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            TestContext.Out.WriteLine("wrote " + written + (plate ? " plates to " : " stills to ") + dir);
            yield return new ExitPlayMode();
            Assert.AreEqual(2 * subjects.Length, written, "a still is missing");
            if (!plate && System.Array.IndexOf(subjects, "drop") >= 0) Assert.Greater(dropped, 0, "no paratrooper came down: the drop was refused, so the paradrop still photographs nothing");
        }
    }
}
