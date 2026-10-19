// Phase: VFX harsh-critic fix (2026-10-19, paradrop-inbound-marker-r6), on top of r5's three pairs (zoom 240, lamps
// and rain pinned, centred on CaptureRig's own screen projection of the landing point, Rig.Mark). r5's critique
// (22/100): its probe compared a 0-255 channel spread against 0.06, so any 1-level drop counted and the puff was
// never actually shown; and one shot at landTick + 1 cannot tell a puff that holds from a single frame. This version
// shoots at landTick + 3 and landTick + 6, each with a twin that has the puff's book hidden
// (CombatFx.HideBookForTests), plus a re-shot of the +3 pose in the same frozen state for the noise floor, so
// docs/evidence/paradrop-inbound-marker-r6/probe.py can take the minimum of the two ticks' puff-minus-nopuff counts.
// The support disc stays masked (CombatFx.ShowOverlays = false, r5's fix 2: it is HUD drawn in the world and expires
// on real time, which moved it 173 px between r2's shots).
// Pinning: Knobs lights.realLamps 43 (every lamp keeps its real light: nothing rotates out) and lights.waterLamps 0
// (NightLights.RealLamps.cs); Atmosphere.PinnedClock set once and never released, so every shot falls in the same
// squall (Atmosphere.cs: RainNow is a function of the pinned clock alone). The landing tick is read from the sim's
// own DropInbound event (its Tick is the command's real processing tick, InputDelayTicks later than the tick it was
// issued on), not guessed from Time.time. All eight DropLanded events fire on one tick (OffMapAbilities.cs ~409),
// so both frames hold the same stick.
// Run it by name WITH a graphics device (not -nographics):
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter DropLandingDustStills \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_DROPDUST_DIR where the stills go (default docs/evidence/paradrop-inbound-marker-r6 at the repo root).
using System.Collections;
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
    public class DropLandingDustStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        const uint WarmupTicks = 100;   // OffMapAbilities.cs: AbilityStats for ParaDrop

        /// <summary>Open ground player 0 may actually drop on (CanLand: not water, not a trench, short of the enemy's
        /// rear line by ParaDropKeepOut), found by scanning rather than guessed at from a map this test does not own.</summary>
        static Vector2 FindDropGround(SimHost host)
        {
            var abilities = host.Local.World.GetSystem<OffMapAbilitySystem>();
            var size = host.Local.Map.SizeMeters;
            for (float zz = 24f; zz < size.y - 24f; zz += 6f)
                for (float xx = size.x * 0.5f - 60f; xx <= size.x * 0.5f + 60f; xx += 6f)
                {
                    if (xx < 10f || xx > size.x - 10f) continue;
                    if (abilities.CanLand(0, new Unity.Mathematics.float3(xx, 0f, zz))) return new Vector2(xx, zz);
                }
            return new Vector2(size.x * 0.5f, size.y * 0.3f);
        }

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        /// <summary>The processing tick of the one DropInbound event near the drop point, or 0 while still waiting.</summary>
        sealed class Latch
        {
            public Vector2 Near;
            public uint Tick;
            public void Hear(SimEvent e)
            {
                if (e.Type != SimEventType.DropInbound || Tick != 0) return;
                if (Mathf.Abs(e.Pos.x - Near.x) > 25f || Mathf.Abs(e.Pos.z - Near.y) > 25f) return;
                Tick = e.Tick == 0 ? uint.MaxValue : e.Tick;   // tick 0 cannot happen this late in a match; guards the sentinel
            }
        }

        [UnityTest, Explicit("Zoom-240 stills at landTick -2, +3 and +6 with puff-hidden twins, lamps and rain pinned; run by name, with a graphics device.")]
        public IEnumerator DropLandingShotBeforeAndAfter()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            // ParaDrop is Brass's alone (FactionRoster.MayCall); GreyboxCorridor may seat another faction at A.
            foreach (var h in Object.FindObjectsByType<SimHost>(FindObjectsSortMode.None)) h.FactionA = (byte)FactionId.Brass;
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            var host = Host;

            string dir = System.Environment.GetEnvironmentVariable("TW_DROPDUST_DIR");
            if (string.IsNullOrEmpty(dir))
                dir = Path.GetFullPath(Path.Combine(Application.dataPath, "..", "..", "docs", "evidence", "paradrop-inbound-marker-r6"));
            Directory.CreateDirectory(dir);

            // pin the lamps (every lamp keeps a real light: none rotates out between the two shots) and the weather
            // clock (pins RainNow too, since it is computed from the clock alone); never Released, so every shot falls
            // in the same squall. Mask the support disc too (fix 2): it is HUD drawn in the world (CombatFx.cs
            // ~1185) and expires on real time (Until = Time.time + 10f), so it is gone from a later shot while
            // present in an earlier one — the 173 px the harsh critic found. Masked, it is absent from every shot.
            Knobs.Set("lights.realLamps", "43");
            Knobs.Set("lights.waterLamps", "0");
            Atmosphere.PinnedClock = 120f;
            CombatFx.ShowOverlays = false;
            for (int f = 0; f < 10; f++) yield return null;

            Vector2 drop = FindDropGround(host);
            float dropX = drop.x, dropZ = drop.y;
            Vector3 landing = new Vector3(dropX, RenderGround.Sample(host.Local.Map, dropX, dropZ), dropZ);
            CaptureRig.Rig.Mark = landing;

            var latch = new Latch { Near = drop };
            host.Events.OnEvent += latch.Hear;
            TankCapture.Silver();
            host.Issue(new SimCommand { Tick = host.Local.World.Tick, Type = CommandType.SupportFire, A = (int)OffMapAbilityId.ParaDrop, Pos = new Unity.Mathematics.float3(dropX, 0f, dropZ) });

            for (int f = 0; f < 300 && latch.Tick == 0; f++) yield return null;
            host.Events.OnEvent -= latch.Hear;
            Assert.Greater(latch.Tick, 0u, "DropInbound never fired: the call was refused");
            uint landTick = latch.Tick + WarmupTicks;

            string before = Path.Combine(dir, "z240-before.png");
            string p3 = Path.Combine(dir, "z240-p3.png");
            string p3b = Path.Combine(dir, "z240-p3b.png");
            string nopuff3 = Path.Combine(dir, "z240-nopuff-p3.png");
            string p6 = Path.Combine(dir, "z240-p6.png");
            string nopuff6 = Path.Combine(dir, "z240-nopuff-p6.png");
            var shots = new[] { before, p3, p3b, nopuff3, p6, nopuff6 };
            foreach (var p in shots) if (File.Exists(p)) File.Delete(p);

            var fx = Object.FindFirstObjectByType<CombatFx>();
            Assert.That(fx, Is.Not.Null, "no CombatFx");

            Time.timeScale = 1f;
            for (int f = 0; f < 1200 && host.Local.World.Tick < landTick - 2; f++) yield return null;
            Time.timeScale = 0f;
            CaptureRig.Shot(before, dropX, dropZ, 240f, 21f, 25f);
            yield return Drain(before);

            // +3: the puff is up. Shot frozen, then re-shot in the same frozen state for the noise floor, then once
            // more with the puff's book hidden — the men and the ground are identical in all three, so the only
            // difference the probe can see is the puff.
            Time.timeScale = 1f;
            for (int f = 0; f < 1200 && host.Local.World.Tick < landTick + 3; f++) yield return null;
            Time.timeScale = 0f;
            CaptureRig.Shot(p3, dropX, dropZ, 240f, 21f, 25f);
            yield return Drain(p3);
            for (int f = 0; f < 10; f++) yield return null;
            CaptureRig.Shot(p3b, dropX, dropZ, 240f, 21f, 25f);
            yield return Drain(p3b);
            fx.HideBookForTests(FlipbookFx.Book.DustPuff, true);
            CaptureRig.Shot(nopuff3, dropX, dropZ, 240f, 21f, 25f);
            yield return Drain(nopuff3);
            fx.HideBookForTests(FlipbookFx.Book.DustPuff, false);

            // +6: the same pair again, three ticks later, so one number proves the puff holds and is not one frame
            Time.timeScale = 1f;
            for (int f = 0; f < 1200 && host.Local.World.Tick < landTick + 6; f++) yield return null;
            Time.timeScale = 0f;
            CaptureRig.Shot(p6, dropX, dropZ, 240f, 21f, 25f);
            yield return Drain(p6);
            fx.HideBookForTests(FlipbookFx.Book.DustPuff, true);
            CaptureRig.Shot(nopuff6, dropX, dropZ, 240f, 21f, 25f);
            yield return Drain(nopuff6);
            fx.HideBookForTests(FlipbookFx.Book.DustPuff, false);

            Time.timeScale = 1f;
            Atmosphere.PinnedClock = -1f;
            CombatFx.ShowOverlays = true;
            Knobs.Clear();
            foreach (var p in shots) Assert.That(File.Exists(p), Is.True, Path.GetFileName(p) + " was not written");
            yield return new ExitPlayMode();
        }
    }
}
