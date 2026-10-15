// Phase: infantry-firing (2026-10-15) — the four infantry types firing, photographed the way DeathStills photographs the
// deaths (read its header for why it is an [Explicit] EditMode test that enters Play itself): six men of one squad abreast,
// an enemy group ahead of them, the squad walked forward by hand (SimHost.WriteWorlds) while the sim's own direct fire
// shoots for them. It is the before/after film of three repairs: a phase offset per man so a squad does not fire in
// lockstep, the tracer cut to the gun, and a pose for the shot fired while advancing.
//
// Run it by name WITH a graphics device (not -nographics), or `leg play InfantryFiringStills`:
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter InfantryFiringStills \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_STILLS_DIR where the stills go (default %TEMP%/tw-infantryfiring: never inside the checkout, whose
// untracked files land.py would see), TW_STILLS_FIELD the look (Winter by default: the day field, overcast and snowed on,
// where a tracer's start and a man's pose read clearly), TW_FIRING_UNITS a comma list of archetype ids (default 0,1,2,3:
// Rifle, Assault, Machinegunner, Sniper).
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
    public class InfantryFiringStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        const int Men = 6;              // one squad abreast
        const float Spacing = 1.6f;     // m between them, as DeathLab.Row stands a row
        const float Pace = 1.4f;        // m/s: the rifleman's own walk (CombatTables), the advancing shot's pace
        const float Seconds = 8f;       // of film an archetype
        const int Frames = 14;

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        /// <summary>A patch of open ground for a squad: no trench, ladder, wire, bunker or blocked cell over the strip the
        /// squad walks and the ground the enemy group stands on, and no prop on it (DeathStills.Open, the same test).</summary>
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
                if (q.x > x - 3f && q.x < x + w + 3f && q.z > z - 4f && q.z < z + 6f) return false;   // only the squad's own strip
            }
            return true;
        }

        [UnityTest, Explicit("The infantry's firing photographed; run by name, with a graphics device.")]
        public IEnumerator EveryInfantryTypeFiringIsPhotographed()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            // the look: set on the scene before Play (a serialized field survives the domain reload; the scene is not saved)
            string fieldName = System.Environment.GetEnvironmentVariable("TW_STILLS_FIELD");
            if (string.IsNullOrEmpty(fieldName)) fieldName = "Winter";
            if (System.Enum.TryParse(fieldName, true, out TW.Presentation.Terrain.Biome field))
            {
                foreach (var g in Object.FindObjectsByType<TW.Presentation.Terrain.GreyboxTerrainView>(FindObjectsSortMode.None)) g.Field = field;
                foreach (var at in Object.FindObjectsByType<TW.Presentation.Terrain.Atmosphere>(FindObjectsSortMode.None)) at.Field = field;
            }
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            // read AFTER entering Play: the domain reloads there, and locals set before it come back as defaults
            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-infantryfiring");
            Directory.CreateDirectory(dir);
            string wanted = System.Environment.GetEnvironmentVariable("TW_FIRING_UNITS");
            string[] units = string.IsNullOrEmpty(wanted) ? new[] { "0", "1", "2", "3" } : wanted.Split(',');

            // quiet: no enemy deploys or attacks, no stray shells (the squads make their own fight)
            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            for (int f = 0; f < 120; f++) yield return null;
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;
            // a still costs about 0.3 s of game time to capture (DeathStills, round 16): the clock steps 1/30 s a frame
            Time.captureDeltaTime = 1f / 30f;
            var tc = Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();

            var map = Host.Local.Map;
            var size = map.SizeMeters;
            int written = 0;
            var used = new System.Collections.Generic.List<Vector2>();
            foreach (string unit in units)
            {
                if (!int.TryParse(unit.Trim(), out int arche)) continue;
                // ground the squad can walk over: as far ahead as the film carries them if the corridor allows it, else
                // less (the first run asked for 30 m of open ground ahead and GreyboxCorridor has none)
                float x = -1f, z = -1f;
                foreach (float ahead in new[] { 12f, 8f, 4f })
                {
                    for (float zz = 40f; zz < size.y - 40f && x < 0f; zz += 6f)
                        for (float xx = 8f; xx < size.x - 24f && x < 0f; xx += 6f)
                        {
                            bool clear = true;
                            foreach (var u in used) if (Mathf.Abs(u.x - xx) < 24f && Mathf.Abs(u.y - zz) < 40f) clear = false;
                            if (clear && Open(map, xx, zz, Men * Spacing, 5f, ahead)) { x = xx; z = zz; }
                        }
                    if (x >= 0f) break;
                }
                if (x < 0f) { TestContext.Out.WriteLine(arche + ": no open ground left"); continue; }
                used.Add(new Vector2(x, z));

                // the squad abreast, and an enemy group 22 m ahead for them to shoot at
                var squad = new System.Collections.Generic.List<int>();
                for (int k = 0; k < Men; k++)
                {
                    string s = TankCapture.Spawn(0, arche, x + k * Spacing, z, 0f);
                    if (s.StartsWith("slot") && int.TryParse(s.Substring(5), out int slot)) squad.Add(slot);
                    else TestContext.Out.WriteLine(arche + ": " + s);
                }
                for (int k = 0; k < 3; k++) TankCapture.Spawn(1, (int)InfantryArchetype.Rifle, x + 1f + k * 2.5f, z + 22f, 180f);
                // nobody falls inside the film: the shot is what is being looked at, not the dying
                Host.WriteWorlds(m =>
                {
                    for (int i = 0; i < m.World.HighWater; i++)
                        if ((m.World.Flags[i] & (uint)UnitFlags.Alive) != 0) { m.World.MaxHp[i] = 9000f; m.World.Hp[i] = 9000f; }
                });
                TestContext.Out.WriteLine($"archetype {arche}: {squad.Count} men at {x:0.0}, {z:0.0}, the enemy at z {z + 22f:0.0}");

                float start = Time.time;
                int next = 0;
                while (next < Frames)
                {
                    // the squad walks into its own fire: the sim has no move order for a man, so the show walks them
                    // (SimHost.WriteWorlds, the only way SHOW may write the world) at the rifleman's own pace
                    float dt = Time.deltaTime;
                    Host.WriteWorlds(m =>
                    {
                        foreach (int slot in squad)
                            if ((m.World.Flags[slot] & (uint)UnitFlags.Alive) != 0)
                            {
                                var p = m.World.Position[slot]; p.z += Pace * dt; m.World.Position[slot] = p;
                            }
                    });
                    float due = start + Seconds * next / Mathf.Max(1, Frames - 1);
                    if (Time.time < due) { yield return null; continue; }
                    float walked = Pace * (Time.time - start);
                    string path = Path.Combine(dir, $"a{arche}_{next:00}.png");
                    if (tc != null) tc.BaseYaw = -90f;
                    CaptureRig.Shot(path, x + Men * Spacing * 0.5f, z + walked + 4f, 12f, 40f, 16f, 1280, 720, 1.2f);
                    yield return Drain(path);
                    if (File.Exists(path)) written++;
                    next++;
                }
                CaptureRig.Sheet(dir, $"a{arche}_", Path.Combine(dir, $"a{arche}_sheet.png"), 4, 480);
                // the same squad from in front, where a tracer leaving the gun and a man's pose read together
                string face = Path.Combine(dir, $"a{arche}_front.png");
                if (tc != null) tc.BaseYaw = -90f;
                CaptureRig.Shot(face, x + Men * Spacing * 0.5f, z + Pace * (Time.time - start) + 10f, 14f, 200f, 12f, 1280, 720, 1.2f);
                yield return Drain(face);
                if (File.Exists(face)) written++;
                for (int f = 0; f < 30; f++) yield return null;
            }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            Time.captureDeltaTime = 0f;
            yield return new ExitPlayMode();
            TestContext.Out.WriteLine($"wrote {written} stills to {dir}");
            Assert.That(written, Is.GreaterThan(units.Length * 4), "too few stills landed to judge the firing");
        }
    }
}
