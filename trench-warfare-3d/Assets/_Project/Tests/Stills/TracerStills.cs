// Phase: tracers-side-by-side (2026-10-15) - one night moment of a battle with smoke and machine-gun fire, filmed
// twice from the same camera on the same seed and step: once with today's beams (fx.tracerInSmoke off, the default)
// and once with the tracers drawn inside the smoke (fx.tracerInSmoke on). Two stills, two short films and a sheet
// with both side by side, for a brief to the owner (decisions.md, 2026-10-08: he takes the default, keep today's
// beams, until he has seen both). No game code is changed by this test.
//
// Run it by name WITH a graphics device (not -nographics):
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter TracerStills \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_STILLS_DIR where the stills go (default %TEMP%/tw-tracerstills: never inside the checkout),
// TW_TRACER_STRESS riflemen a side (300), TW_TRACER_SETTLE ticks before the smoke is called (1200).
using System.Collections;
using System.Globalization;
using System.IO;
using NUnit.Framework;
using UnityEngine;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using TW.Editor;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Presentation.Terrain;
using TW.Sim;
using TW.Sim.Match;
using TW.UI;

namespace TW.Tests
{
    public class TracerStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        static readonly CultureInfo Inv = CultureInfo.InvariantCulture;
        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        static string Env(string k, string d) { string v = System.Environment.GetEnvironmentVariable(k); return string.IsNullOrEmpty(v) ? d : v; }

        static string Dir()
        {
            string dir = Env("TW_STILLS_DIR", Path.Combine(Path.GetTempPath(), "tw-tracerstills"));
            Directory.CreateDirectory(dir);
            return dir;
        }

        // EnterPlayMode is yielded directly from the [UnityTest] method, not from a called helper: a level of
        // enumerator nesting round it breaks Unity's play-mode-state detection (isPlaying stays false forever,
        // the symptom that cost time here) even though no exception is thrown. TW_KNOBS is a process-wide env var,
        // so nothing needs restoring: the next variant (or run) sets its own value.
        [UnityTest, Order(1), Explicit("The side-by-side tracer moment, today's beams; run by name, with a graphics device.")]
        public IEnumerator TodayBeams_AreFilmed()
        {
            Setup("today");
            yield return new EnterPlayMode();
            yield return InPlay("today");
        }

        [UnityTest, Order(2), Explicit("The side-by-side tracer moment, tracers inside the smoke; run by name, with a graphics device.")]
        public IEnumerator TracersInSmoke_AreFilmed()
        {
            Setup("insmoke");
            yield return new EnterPlayMode();
            yield return InPlay("insmoke");
        }

        static void Setup(string variant)
        {
            System.Environment.SetEnvironmentVariable("TW_KNOBS", variant == "insmoke" ? "stress.mix=1,fx.tracerInSmoke=1" : "stress.mix=1");
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            foreach (var g in Object.FindObjectsByType<GreyboxTerrainView>(FindObjectsSortMode.None)) g.Field = Biome.NightMud;
            foreach (var at in Object.FindObjectsByType<Atmosphere>(FindObjectsSortMode.None)) at.Field = Biome.NightMud;
            var host = Host;
            Assert.That(host, Is.Not.Null, "no SimHost in the scene");
            host.StressUnits = int.Parse(Env("TW_TRACER_STRESS", "300"), Inv);
        }

        static IEnumerator InPlay(string variant)
        {
            for (int f = 0; f < 2000 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            string dir = Dir();
            var info = new System.Text.StringBuilder();
            void Say(string s) { info.AppendLine(s); File.WriteAllText(Path.Combine(dir, "info." + variant + ".txt"), info.ToString()); Debug.Log("[tracerstills] " + s); }
            CombatFx.ShowOverlays = false;
            Say($"variant {variant}: knobs {Knobs.ToJson()}, seed {Host.Seed}, battlefield seed {Host.BattlefieldSeed}");

            var sky = Object.FindFirstObjectByType<Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            Atmosphere.PinnedClock = 30f;
            Time.captureDeltaTime = 1f / 30f;

            int settle = int.Parse(Env("TW_TRACER_SETTLE", "1200"), Inv);
            Host.TimeScale = 8f;
            while (Host.Local.World.Tick < (uint)settle) yield return null;
            Host.TimeScale = 1f;
            Say($"settled at tick {Host.Local.World.Tick}, alive {Host.Local.World.AliveCount}");

            Vector2 at = CaptureRig.Crowd();
            Say($"crowd at {at.x:0.0}, {at.y:0.0}");
            Say(VfxLab.Call((int)OffMapAbilityId.SmokeScreen, at.x, at.y));

            // the stress battle's own riflemen rarely trade fire in the crowd's own 20 m cell by the time the cloud
            // has built (runs here: zero shots logged from the battle alone); two facing lines of machine-gunners,
            // stationary and close enough to see each other, guarantee the "machine-gun fire" the goal asks for
            const int Gunners = 5;
            for (int i = 0; i < Gunners; i++)
            {
                float z = at.y + (i - (Gunners - 1) * 0.5f) * 2.5f;
                string sA = TankCapture.Spawn(0, InfantryArchetype.Machinegunner, at.x - 9f, z, 90f);
                string sB = TankCapture.Spawn(1, InfantryArchetype.Machinegunner, at.x + 9f, z, -90f);
                if (sA.StartsWith("slot ")) { int slot = int.Parse(sA.Substring(5)); Host.WriteWorlds(m => m.World.Speed[slot] = 0f); }
                if (sB.StartsWith("slot ")) { int slot = int.Parse(sB.Substring(5)); Host.WriteWorlds(m => m.World.Speed[slot] = 0f); }
            }
            Say($"placed {Gunners} machine-gunners a side, facing each other 18 m apart astride {at.x:0.0}, {at.y:0.0} (the stress battle alone logged no shot there)");

            uint untilTick = Host.Local.World.Tick + 240u;
            while (Host.Local.World.Tick < untilTick) yield return null;
            Say($"cloud built over the fight at tick {Host.Local.World.Tick}");

            Say(CaptureRig.Hold(120f));
            for (int f = 0; f < 8; f++) yield return null;

            string stillPath = Path.Combine(dir, "moment." + variant + ".png");
            if (File.Exists(stillPath)) File.Delete(stillPath);
            CaptureRig.Shot(stillPath, at.x, at.y, 30f, 21f, 25f, 1600, 900);
            yield return Drain(stillPath);
            Assert.That(File.Exists(stillPath), Is.True, "the still was not written");
            uint stillTick = Host.Local.World.Tick;
            Say(CaptureRig.Release());   // the world was held for the still; the film needs it running so the men keep firing

            ShotLog.Begin(4000, Host.Local.World.Tick);
            const int Frames = 36;
            for (int k = 0; k < Frames; k++)
            {
                string path = Path.Combine(dir, $"film.{variant}_{k:00}.png");
                if (File.Exists(path)) File.Delete(path);
                CaptureRig.Shot(path, at.x, at.y, 30f, 21f, 25f, 1600, 900);
                yield return Drain(path);
                for (int w = 0; w < 4; w++) yield return null;   // 1/12 s of game time apart at the 1/30 s step above
            }
            CaptureRig.Sheet(dir, "film." + variant, Path.Combine(dir, "film." + variant + "_sheet.png"), 6, 320);

            Say($"still tick {stillTick}, crowd {at.x:0.0},{at.y:0.0}, alive {Host.Local.World.AliveCount}, shots logged {ShotLog.Count}");
            Assert.Greater(ShotLog.Count, 0, "no tracer shots were logged: no machine-gun fire in the moment");
            ShotLog.Stop();

            Time.captureDeltaTime = 0f;
            Atmosphere.PinnedClock = -1f;
            yield return new ExitPlayMode();
        }

        [Test, Order(3), Explicit("The side-by-side sheet from the two filmed moments; run after both Film tests.")]
        public void PairSheet_IsBuiltFromBothMoments()
        {
            string dir = Dir();
            string today = Path.Combine(dir, "moment.today.png"), insmoke = Path.Combine(dir, "moment.insmoke.png");
            Assert.That(File.Exists(today), Is.True, "run TodayBeams_AreFilmed first");
            Assert.That(File.Exists(insmoke), Is.True, "run TracersInSmoke_AreFilmed first");
            string pair0 = Path.Combine(dir, "pair_00.png"), pair1 = Path.Combine(dir, "pair_01.png");
            File.Copy(today, pair0, true);
            File.Copy(insmoke, pair1, true);
            string outPath = Path.Combine(dir, "tracers_sheet.png");
            string result = CaptureRig.Sheet(dir, "pair", outPath, 2, 800);
            Assert.That(File.Exists(pair0), Is.True);
            Assert.That(File.Exists(pair1), Is.True);
            Assert.That(File.Exists(outPath), Is.True, result);
        }
    }
}
