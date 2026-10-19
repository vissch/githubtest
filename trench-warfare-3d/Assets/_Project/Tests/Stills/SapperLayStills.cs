// Phase: sapper-lay-mine-anim (2026-10-19) — the sapper's kneel-and-place (SapperLayMineTests) photographed idle vs
// mid-lay, built line-for-line on InfantryFiringStills (read its header for why this is an [Explicit] EditMode test
// that enters Play itself). One sapper on open ground, ordered to lay a mine on his own cell (the same UnitAbility
// command ProvingGround.OrderSappers issues) so he kneels in place instead of walking: the two stills then differ
// only by his pose, which is what docs/evidence/sapper-lay-mine-anim/probe.py measures.
//
// Run it by name WITH a graphics device (not -nographics), or `leg play SapperLayStills`:
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter SapperLayStills \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_STILLS_DIR where the stills go (default %TEMP%/tw-sapperlay: never inside the checkout, whose
// untracked files land.py would see).
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
using TW.Sim.Units;
using Unity.Mathematics;
using TW.UI;

namespace TW.Tests
{
    public class SapperLayStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        static bool Open(MapData map, float x, float z)
        {
            var size = map.SizeMeters;
            for (float zz = z - 4f; zz <= z + 4f; zz += 2f)
                for (float xx = x - 4f; xx <= x + 4f; xx += 2f)
                {
                    if (xx < 3f || zz < 3f || xx > size.x - 3f || zz > size.y - 3f) return false;
                    var layer = map.LayerAt(new float3(xx, 0f, zz));
                    if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked | NavLayer.Wire | NavLayer.Bunker)) != 0) return false;
                }
            for (int i = 0; i < map.Props.Length; i++)
            {
                var q = map.Props[i].Pos;
                if (q.x > x - 4f && q.x < x + 4f && q.z > z - 4f && q.z < z + 4f) return false;
            }
            return true;
        }

        [UnityTest, Explicit("The sapper's kneel-and-place photographed; run by name, with a graphics device.")]
        public IEnumerator SapperLayingIsPhotographed()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");

            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-sapperlay");
            Directory.CreateDirectory(dir);

            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            for (int f = 0; f < 120; f++) yield return null;
            // stilled weather: rain and squalls move every frame, which would swamp the pose shift the probe looks for
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;
            Time.captureDeltaTime = 1f / 30f;

            var map = Host.Local.Map;
            var size = map.SizeMeters;
            float x = -1f, z = -1f;
            for (float zz = 40f; zz < size.y - 40f && x < 0f; zz += 6f)
                for (float xx = 8f; xx < size.x - 8f && x < 0f; xx += 6f)
                    if (Open(map, xx, zz)) { x = xx; z = zz; }
            Assert.That(x, Is.GreaterThanOrEqualTo(0f), "no open ground for the sapper");

            string s = TankCapture.Spawn(0, (int)InfantryArchetype.Sapper, x, z, 0f);
            Assert.That(s, Does.StartWith("slot"), s);
            int slot = int.Parse(s.Substring(5));
            for (int f = 0; f < 30; f++) yield return null;

            string idle = Path.Combine(dir, "idle.png");
            CaptureRig.Shot(idle, x, z, 6f, 40f, 10f, 1280, 720, 1.2f);
            yield return Drain(idle);

            // the same command ProvingGround.OrderSappers issues, on his own cell: he lays where he stands, not after walking
            var w = Host.Local.World;
            Host.LocalDriver.Issue(new SimCommand
            {
                Tick = w.Tick, Player = 0, Type = CommandType.UnitAbility,
                A = slot, B = (int)UnitAbilityId.LayMine, Pos = w.Position[slot]
            });

            float start = Time.time;
            while (Time.time - start < 1.5f) yield return null;
            string laying = Path.Combine(dir, "laying.png");
            CaptureRig.Shot(laying, x, z, 6f, 40f, 10f, 1280, 720, 1.2f);
            yield return Drain(laying);

            TestContext.Out.WriteLine("clip: " + Clip.KneelInspect + " / " + Clip.StandToKneel);
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            Time.captureDeltaTime = 0f;
            yield return new ExitPlayMode();

            Assert.That(File.Exists(idle), "idle still missing");
            Assert.That(File.Exists(laying), "laying still missing");
        }
    }
}
