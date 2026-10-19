// Phase: VFX pass (2026-10-19, ability-fx-captures) — the five ability/environment looks the critic found no capture for
// at all (gas, the smoke screen, the HE barrage, the water splash and the ParaDrop), each fired in GreyboxCorridor by the
// game's own systems and shot at two bands: T1 (zoom 30, the band the game is played at) and O240 (the overview, where a
// bank is only a blanket). The clock runs (Time.captureDeltaTime) and the weather is pinned, so two subjects' stills are
// comparable. It asserts only that stills were written: the pictures and their sidecars go to a critique
// (docs/evidence/ability-fx-captures/, measured by probe.py).
// Moments: water, the drop and the barrage wait on the sim's own event (Explosion / DropLanded) and shoot 0.4 s after it
// — the barrage warms up 4 s before its first shell, so a fixed delay photographs empty ground; gas and smoke shoot at a
// fixed spread time, when the bank is grown (GasBankFirst..Last, SmokeBankFirst..Last).
// The ParaDrop is Brass's alone (FactionRoster.MayCall) and GreyboxCorridor seats Iron at A, which refuses the call
// outright (measured 2026-10-19: DropLanded never fired), so seat A is set to Brass in the open scene before Play and
// the scene is never saved. If the drop still does not land the still is taken anyway and moments.txt says so.
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
using TW.UI;

namespace TW.Tests
{
    public class AbilityFxStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        /// <summary>The subjects, in the order they are fired. The drop goes first: it is refused within
        /// ParaDropKeepOut of the enemy's rear trench, and the open-ground search walks up z from the near side.</summary>
        static readonly string[] Subjects = { "paradrop", "barrage", "water", "gas", "smoke" };

        /// <summary>How long after the staging a subject is shot when no sim event marks its moment.</summary>
        static float Spread(string subject) => subject == "gas" ? 8f : 6f;

        /// <summary>Which sim event marks a subject's moment, or None for a fixed time.</summary>
        static SimEventType Moment(string subject)
            => subject == "water" || subject == "barrage" ? SimEventType.Explosion
             : subject == "paradrop" ? SimEventType.DropLanded
             : SimEventType.None;

        /// <summary>The moment a subject's own sim event first fires near its point. A named class, not a local function:
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

        [UnityTest, Explicit("Five ability and environment looks photographed at two bands; run by name, with a graphics device.")]
        public IEnumerator FiveAbilitiesAtTwoBands()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            // ParaDrop is Brass's alone (FactionRoster.MayCall) and GreyboxCorridor seats Iron at A, which refuses the
            // call: seat A is set to Brass in the open scene, before Play, and the scene is never saved.
            foreach (var h in Object.FindObjectsByType<SimHost>(FindObjectsSortMode.None)) h.FactionA = (byte)FactionId.Brass;
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            Debug.Log("[abilityfx] in play, the match is up");

            string dir = System.Environment.GetEnvironmentVariable("TW_ABILITYFX_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetDirectoryName(Application.dataPath), "Captures", "abilityfx");
            Directory.CreateDirectory(dir);

            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            for (int f = 0; f < 120; f++) yield return null;
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;   // the weather pinned: every still is the same evening
            Time.captureDeltaTime = 1f / 30f;                      // the clock runs while filming (CaptureRig.Hold would stop it)
            var tc = Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
            Debug.Log("[abilityfx] settled, the clock runs and the weather is pinned; camera " + (tc == null ? "missing" : "found"));

            var host = Host;
            Debug.Log("[abilityfx] host " + (host == null ? "null" : "ok") + ", pump " + (host == null || host.Events == null ? "null" : "ok"));
            var latch = new Latch();
            host.Events.OnEvent += latch.Hear;
            Debug.Log("[abilityfx] subscribed");

            var map = host.Local.Map;
            var size = map.SizeMeters;
            Debug.Log("[abilityfx] map " + size.x + " x " + size.y + ", props " + map.Props.Length);
            int written = 0;
            var used = new System.Collections.Generic.List<Vector2>();
            var log = new System.Text.StringBuilder();
            foreach (string subject in Subjects)
            {
                float room = VfxStills.Room(subject);
                float x = -1f, z = -1f;
                if (subject == "water")
                {
                    // the splash comes from a burst landing on a wet cell (CombatFx.cs:821 reads SceneHooks.IsWater)
                    int wet = 0;
                    for (float zz = 8f; zz < size.y - 8f && x < 0f; zz += 4f)
                        for (float xx = 8f; xx < size.x - 8f && x < 0f; xx += 4f)
                        {
                            if (SceneHooks.IsWater == null || !SceneHooks.IsWater(xx, zz)) continue;
                            wet++;
                            if (VfxStills.Open(map, xx, zz, 7f)) { x = xx; z = zz; }
                        }
                    TestContext.Out.WriteLine("water: " + wet + " wet cells found");
                }
                else
                {
                    for (float zz = 40f; zz < size.y - 40f && x < 0f; zz += 6f)
                        for (float xx = 20f; xx < size.x - 20f && x < 0f; xx += 6f)
                        {
                            bool clear = true;
                            foreach (var u in used) if (Mathf.Abs(u.x - xx) < 24f && Mathf.Abs(u.y - zz) < 24f) clear = false;   // the field is 90 m wide: 34 m apart leaves the last subject no ground
                            if (clear && VfxStills.Open(map, xx, zz, room)) { x = xx; z = zz; }
                        }
                }
                if (x < 0f) { TestContext.Out.WriteLine(subject + ": no ground for it"); log.Append(subject).Append(": NO GROUND, no stills\n"); continue; }
                used.Add(new Vector2(x, z));
                latch.At = -1f; latch.Want = Moment(subject); latch.Near = new Vector2(x, z);
                Debug.Log("[abilityfx] " + subject + " at " + x + ", " + z + ": " + VfxLab.Scene(subject, x, z));
                TestContext.Out.WriteLine(subject + " at " + x + ", " + z);
                float start = Time.time;
                string moment;
                if (latch.Want == SimEventType.None)
                {
                    while (Time.time < start + Spread(subject)) yield return null;
                    moment = Spread(subject).ToString("0.#") + " s after the call, the bank grown";
                }
                else
                {
                    while (latch.At < 0f && Time.time < start + 14f) yield return null;
                    if (latch.At < 0f) moment = latch.Want + " never fired in 14 s: shot anyway";
                    else { float due = latch.At + 0.4f; while (Time.time < due) yield return null; moment = "0.4 s after " + latch.Want; }
                }
                TestContext.Out.WriteLine(subject + ": " + moment);
                log.Append(subject).Append(" at ").Append(x).Append(", ").Append(z).Append(": ").Append(moment).Append('\n');
                foreach (var band in new[] { ("t1", 30f), ("o240", 240f) })
                {
                    string path = Path.Combine(dir, subject + "-" + band.Item1 + ".png");
                    if (tc != null) tc.BaseYaw = -90f;
                    CaptureRig.Shot(path, x, z, band.Item2, 21f, 25f, 1600, 900);
                    yield return Drain(path);
                    if (File.Exists(path)) written++;
                }
            }
            host.Events.OnEvent -= latch.Hear;
            Time.captureDeltaTime = 0f;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            File.WriteAllText(Path.Combine(dir, "moments.txt"), log.ToString());
            TestContext.Out.WriteLine("wrote " + written + " stills to " + dir);
            yield return new ExitPlayMode();
            Assert.Greater(written, 0, "no stills were written");
        }
    }
}
