// Phase: VFX critique fix (2026-10-19, paradrop-inbound-marker-r2) — r1's dust probe measured a camp lamp going dark
// (151,87,33 -> 34,34,42) ~570 px from the drop, under a hand-found window: a lamp rotating out of
// NightLights.RealLamps' budget between two "identical" renders, not the puff. This shoots the honest pair instead:
// zoom 240, lamps and rain pinned, either side of the landing tick, centred on CaptureRig's own screen projection of
// the landing point (Rig.Mark), the way AbilityFxStills (docs/evidence/ability-fx-captures) already photographs
// ParaDrop. Pinning: Knobs lights.realLamps 43 (every lamp keeps its real light: nothing rotates out) and
// lights.waterLamps 0 (NightLights.RealLamps.cs); Atmosphere.PinnedClock set once and never released, so the two
// shots fall in the same squall (Atmosphere.cs: RainNow is a function of the pinned clock alone). The landing tick
// is read from the sim's own DropInbound event (its Tick is the command's real processing tick, InputDelayTicks
// later than the tick it was issued on), not guessed from Time.time.
// Run it by name WITH a graphics device (not -nographics):
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter DropLandingDustStills \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_DROPDUST_DIR where the stills go (default docs/evidence/paradrop-inbound-marker-r2 at the repo root).
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
                dir = Path.GetFullPath(Path.Combine(Application.dataPath, "..", "..", "docs", "evidence", "paradrop-inbound-marker-r2"));
            Directory.CreateDirectory(dir);

            // pin the lamps (every lamp keeps a real light: none rotates out between the two shots) and the weather
            // clock (pins RainNow too, since it is computed from the clock alone); never Released, so both shots fall
            // in the same squall
            Knobs.Set("lights.realLamps", "43");
            Knobs.Set("lights.waterLamps", "0");
            Atmosphere.PinnedClock = 120f;
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
            if (File.Exists(before)) File.Delete(before);
            if (File.Exists(after)) File.Delete(after);

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

            Time.timeScale = 1f;
            Atmosphere.PinnedClock = -1f;
            Knobs.Clear();
            Assert.That(File.Exists(before) && File.Exists(after), "a still was not written");
            yield return new ExitPlayMode();
        }
    }
}
