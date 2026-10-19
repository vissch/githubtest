// Phase: VFX harsh-critic fix (2026-10-19, paradrop-inbound-marker-r5), on top of r2's honest pair (zoom 240, lamps
// and rain pinned, either side of the landing tick, centred on CaptureRig's own screen projection of the landing
// point, Rig.Mark). r2's critique (24/100): the support disc's HUD marker (Until = Time.time + 10f, real time) had
// expired between the two shots minutes of frames apart and moved 173 px; the "after" shot alone cannot tell the
// puff from render noise (no noise floor was measured); and the 8 landed men in the window are not split from the
// puff. This version masks the disc (CombatFx.ShowOverlays = false, as NightLampStills/TracerStills already do for
// their own HUD draws), shoots the "after" pose twice in the same frozen state (timeScale 0, Time.time does not
// advance, so this is a pure render-noise pair, not a second landing) and shoots a third still with the dust puff's
// book hidden (CombatFx.HideBookForTests) so dustprobe3.py can subtract the men from the puff.
// Pinning: Knobs lights.realLamps 43 (every lamp keeps its real light: nothing rotates out) and lights.waterLamps 0
// (NightLights.RealLamps.cs); Atmosphere.PinnedClock set once and never released, so every shot falls in the same
// squall (Atmosphere.cs: RainNow is a function of the pinned clock alone). The landing tick is read from the sim's
// own DropInbound event (its Tick is the command's real processing tick, InputDelayTicks later than the tick it was
// issued on), not guessed from Time.time.
// Run it by name WITH a graphics device (not -nographics):
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter DropLandingDustStills \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_DROPDUST_DIR where the stills go (default docs/evidence/paradrop-inbound-marker-r5 at the repo root).
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

        [UnityTest, Explicit("Zoom-240 before/after the paradrop landing tick, lamps and rain pinned; run by name, with a graphics device.")]
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
                dir = Path.GetFullPath(Path.Combine(Application.dataPath, "..", "..", "docs", "evidence", "paradrop-inbound-marker-r5"));
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
            string after = Path.Combine(dir, "z240-after.png");
            string after2 = Path.Combine(dir, "z240-after2.png");
            string nopuff = Path.Combine(dir, "z240-nopuff.png");
            foreach (var p in new[] { before, after, after2, nopuff }) if (File.Exists(p)) File.Delete(p);

            Time.timeScale = 1f;
            for (int f = 0; f < 1200 && host.Local.World.Tick < landTick - 2; f++) yield return null;
            Time.timeScale = 0f;
            CaptureRig.Shot(before, dropX, dropZ, 240f, 21f, 25f);
            yield return Drain(before);

            Time.timeScale = 1f;
            for (int f = 0; f < 1200 && host.Local.World.Tick < landTick + 1; f++) yield return null;
            Time.timeScale = 0f;
            CaptureRig.Shot(after, dropX, dropZ, 240f, 21f, 25f);
            yield return Drain(after);

            // the noise pair: same frozen state, one build, no landing in between (timeScale 0, so Time.time does
            // not move) — whatever differs here is render noise, not the puff
            for (int f = 0; f < 10; f++) yield return null;
            CaptureRig.Shot(after2, dropX, dropZ, 240f, 21f, 25f);
            yield return Drain(after2);

            var fx = Object.FindFirstObjectByType<CombatFx>();
            fx.HideBookForTests(FlipbookFx.Book.DustPuff, true);
            CaptureRig.Shot(nopuff, dropX, dropZ, 240f, 21f, 25f);
            yield return Drain(nopuff);
            fx.HideBookForTests(FlipbookFx.Book.DustPuff, false);

            Time.timeScale = 1f;
            Atmosphere.PinnedClock = -1f;
            CombatFx.ShowOverlays = true;
            Knobs.Clear();
            Assert.That(File.Exists(before) && File.Exists(after) && File.Exists(after2) && File.Exists(nopuff), "a still was not written");
            yield return new ExitPlayMode();
        }
    }
}
