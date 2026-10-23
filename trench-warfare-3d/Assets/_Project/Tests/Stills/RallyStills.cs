// Phase: SHOW (act-officer-rally, 2026-10-23) — the officer's rally photographed, the way InfantryFiringStills
// photographs a squad firing (read its header for why this is an [Explicit] EditMode test that enters Play itself):
// six riflemen on open ground are pinned down (SimHost.WriteWorlds forces their suppression, the only way SHOW may
// write the world) until they cower, then an officer is held beside them. TW_RALLY_DRAW=0 sets
// AnimationController.DrawRally false for the "before" pass (the reader exists but nothing draws it); the default
// is the "after" pass. Prints `rallies=N gestures=M` from the real AnimationController's RallyWatch — the probe
// log the act-officer-rally done-when compares against the gesture captured on screen.
//
// Run it by name WITH a graphics device (not -nographics), or `leg play RallyStills`:
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter RallyStills \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_STILLS_DIR where the stills go (default %TEMP%/tw-rally: never inside the checkout, whose
// untracked files land.py would see), TW_RALLY_NIGHT=1 for the night field (NightMud; day is Winter),
// TW_RALLY_DRAW=0 for the before pass (DrawRally false).
// It asserts the stills were written and that the officer did rally the squad: it is an instrument for the
// before/after capture, not a judge of how the gesture looks.
using System.Collections;
using System.IO;
using NUnit.Framework;
using UnityEngine;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using TW.Editor;
using TW.Presentation;
using TW.Presentation.Terrain;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;
using Unity.Mathematics;
using TW.UI;

namespace TW.Tests
{
    public class RallyStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        const int Men = 6;
        const float Spacing = 1.6f;
        const float OfficerStandoff = 20f;  // m behind the squad, outside the 15 m aura radius, for the "before" shot
        const int PinTicks = 180;           // ticks of forced suppression before the officer is let in
        const int Frames = 10;
        const float Seconds = 8f;           // of film the officer's rally

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static string Env(string k, string d) { string v = System.Environment.GetEnvironmentVariable(k); return string.IsNullOrEmpty(v) ? d : v; }

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        /// <summary>A patch of open ground for the squad and the officer's standoff: no trench, ladder, wire, bunker
        /// or blocked cell, and no prop on it (DeathStills.Open / InfantryFiringStills.Open, the same test).</summary>
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
                if (q.x > x - 3f && q.x < x + w + 3f && q.z > z - back - 4f && q.z < z + ahead + 4f) return false;
            }
            return true;
        }

        static bool Cowering(Clip c) => c == Clip.ProneIdle || c == Clip.Shield || c == Clip.HitProne || c == Clip.ProneFlinch;

        // The coroutine body is a separate IEnumerator, driven by hand below: a plain try/catch around a long
        // [UnityTest] coroutine (one big state machine with several locals live across many yields and a nested
        // local function) lost its continuation under /optimize+ and threw a bare NullReferenceException with no
        // useful site; splitting the body out and pumping it through MoveNext from a small try/catch fixed it and
        // lets a failure still close Play cleanly instead of leaving the editor stuck in Play.
        [UnityTest, Explicit("The officer's rally photographed; run by name, with a graphics device.")]
        public IEnumerator TheOfficerRallyIsPhotographed()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            bool night = Env("TW_RALLY_NIGHT", "0") == "1";
            var field = night ? Biome.NightMud : Biome.Winter;
            foreach (var g in Object.FindObjectsByType<GreyboxTerrainView>(FindObjectsSortMode.None)) g.Field = field;
            foreach (var at in Object.FindObjectsByType<Atmosphere>(FindObjectsSortMode.None)) at.Field = field;
            yield return new EnterPlayMode();

            System.Exception caught = null;
            var body = Body();
            while (true)
            {
                bool moved;
                try { moved = body.MoveNext(); }
                catch (System.Exception e) { caught = e; break; }
                if (!moved) break;
                yield return body.Current;
            }
            if (caught != null)
            {
                yield return new ExitPlayMode();
                throw caught;
            }
        }

        IEnumerator Body()
        {
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");

            // read AFTER entering Play: the domain reloads there, and locals set before it come back as defaults
            bool draw = Env("TW_RALLY_DRAW", "1") != "0";
            AnimationController.DrawRally = draw;
            string dir = Env("TW_STILLS_DIR", Path.Combine(Path.GetTempPath(), "tw-rally"));
            Directory.CreateDirectory(dir);

            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            for (int f = 0; f < 120; f++) yield return null;
            var sky = Object.FindFirstObjectByType<Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            Atmosphere.PinnedClock = 30f;
            Time.captureDeltaTime = 1f / 30f;
            var tc = Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();

            var map = Host.Local.Map;
            var size = map.SizeMeters;
            float x = -1f, z = -1f;
            for (float zz = 40f; zz < size.y - 40f && x < 0f; zz += 6f)
                for (float xx = 8f; xx < size.x - 24f && x < 0f; xx += 6f)
                    if (Open(map, xx, zz, Men * Spacing, 8f, 4f)) { x = xx; z = zz; }
            Assert.That(x, Is.GreaterThanOrEqualTo(0f), "no open ground for the squad and the officer's standoff");

            var men = new int[Men];
            for (int k = 0; k < Men; k++)
            {
                string s = TankCapture.Spawn(0, InfantryArchetype.Rifle, x + k * Spacing, z, 180f);
                if (s.StartsWith("slot") && int.TryParse(s.Substring(5), out int slot)) men[k] = slot;
                else { TestContext.Out.WriteLine("rifleman: " + s); men[k] = -1; }
            }
            string officerSpawn = TankCapture.Spawn(0, InfantryArchetype.Officer, x + Men * Spacing * 0.5f, z - OfficerStandoff, 0f);
            int officer = -1;
            if (officerSpawn.StartsWith("slot") && int.TryParse(officerSpawn.Substring(5), out int oslot)) officer = oslot;
            Assert.That(officer, Is.GreaterThanOrEqualTo(0), "officer: " + officerSpawn);
            // nobody falls inside the film: the rally is what is being looked at, not the dying
            Host.WriteWorlds(m =>
            {
                for (int i = 0; i < m.World.HighWater; i++)
                    if ((m.World.Flags[i] & (uint)UnitFlags.Alive) != 0) { m.World.MaxHp[i] = 9000f; m.World.Hp[i] = 9000f; }
            });

            // pin the squad: force their suppression above the threshold until the ladder draws them flat, the
            // officer held back outside his aura radius so the cap is the squad's own suppression, not his
            bool AllFlat()
            {
                var a = Host.Animation; if (a == null) return false;
                foreach (var m in men) if (m >= 0 && !Cowering(a.State[m].Clip) && a.State[m].Stance != (byte)Stance.Prone) return false;
                return true;
            }
            for (int t = 0; t < PinTicks && !AllFlat(); t++)
            {
                Host.WriteWorlds(w2 => { foreach (var m in men) if (m >= 0) w2.World.Suppression[m] = 100f; });
                yield return null;
            }
            Assert.IsTrue(AllFlat(), "setup: the squad must be cowering before the officer arrives");
            string flatShot = Path.Combine(dir, "rally_00_flat.png");
            if (tc != null) tc.BaseYaw = -90f;
            CaptureRig.Shot(flatShot, x + Men * Spacing * 0.5f, z, 10f, 40f, 20f, 1600, 900);
            yield return Drain(flatShot);

            // the officer's rally: held at a fixed spot beside the squad every frame (SimHost.WriteWorlds), to
            // beat whatever order or formation logic would otherwise walk him off; nobody but the officer caps
            // the squad's suppression from here, so a rally proves the aura, not our own forcing
            var besideSquad = new float3(x + Men * Spacing * 0.5f, 0f, z + 2f);
            float start = Time.time;
            int next = 0;
            while (next < Frames)
            {
                Host.WriteWorlds(w2 =>
                {
                    if (officer >= 0 && (w2.World.Flags[officer] & (uint)UnitFlags.Alive) != 0) w2.World.Position[officer] = besideSquad;
                });
                float due = start + Seconds * next / Mathf.Max(1, Frames - 1);
                if (Time.time < due) { yield return null; continue; }
                string path = Path.Combine(dir, $"rally_{next + 1:00}.png");
                if (tc != null) tc.BaseYaw = -90f;
                CaptureRig.Shot(path, x + Men * Spacing * 0.5f, z + 1f, 10f, 40f, 20f, 1600, 900);
                yield return Drain(path);
                next++;
            }
            CaptureRig.Sheet(dir, "rally_", Path.Combine(dir, "rally_sheet.png"), 4, 480);

            int rallies = Host.Animation != null ? Host.Animation.RallyWatch.Rallies : 0;
            int gestures = Host.Animation != null ? Host.Animation.RallyWatch.Gestures : 0;
            TestContext.Out.WriteLine($"rallies={rallies} gestures={gestures}");

            Atmosphere.PinnedClock = -1f;
            Time.captureDeltaTime = 0f;
            yield return new ExitPlayMode();

            int written = 0;
            foreach (var f in Directory.GetFiles(dir, "rally_*.png")) written++;
            Assert.That(written, Is.GreaterThan(Frames), "too few stills landed to judge the rally");
            Assert.That(rallies, Is.GreaterThan(0), "the aura never capped anyone for the officer to rally");
        }
    }
}
