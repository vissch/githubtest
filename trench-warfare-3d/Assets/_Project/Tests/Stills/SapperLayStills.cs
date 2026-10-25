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
// untracked files land.py would see); TW_STILLS_FRAME_OFFSET (int frames, default 0) shifts both shots later by that
// many frames, so three runs at 0 / 9 / 18 are three real poses of the same kneel, not three copies of one frame.
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

        /// <summary>
        /// A cell he can be photographed on: open ground all round, nobody else alive within 25 m, and — so the
        /// night lamp lights his front instead of burning a white disc through his head — exactly one lamp 3-7 m
        /// away, standing nearer the camera than he is (the camera is pinned to yaw 0 and so stands on -Z) and well
        /// off the view axis.
        /// </summary>
        static bool Open(MapData map, SimWorld w, IReadOnlyList<Vector3> lamps, bool dark, float x, float z, out Vector3 lamp)
        {
            lamp = default;
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
            for (int i = 0; i < w.HighWater; i++)
            {
                if (!w.IsAlive(i)) continue;
                var q = w.Position[i];
                if ((q.x - x) * (q.x - x) + (q.z - z) * (q.z - z) < 25f * 25f) return false;
            }
            if (!dark) return true;                                             // no lamp is lit: nothing to keep out of his silhouette
            int near = 0;
            for (int i = 0; i < lamps.Count; i++)
            {
                var q = lamps[i];
                float d = Mathf.Sqrt((q.x - x) * (q.x - x) + (q.z - z) * (q.z - z));
                if (d < LampNear || d > LampFar) continue;
                near++;
                lamp = q;
            }
            if (near != 1) return false;
            if (lamp.z > z - 1f) return false;                                  // behind him from the camera: its glow would sit on his head
            float off = Mathf.Abs(lamp.x - x);
            return off >= LampOffMin && off <= LampOffMax;
        }

        const float LampNear = 3f, LampFar = 7f, LampOffMin = 2.5f, LampOffMax = 7f;

        [UnityTest, Explicit("The sapper's kneel-and-place photographed; run by name, with a graphics device.")]
        public IEnumerator SapperLayingIsPhotographed()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            // TW_STILLS_FIELD picks the look; unset, the scene keeps its own NightMud. Winter (the day field,
            // BiomeProfile Dark = false) lights his pose evenly and lights no lamp — but CaptureRig.Measure's
            // warnings are tuned for a night scene, so a day set comes back warning "brighter than a night scene
            // should be" and, on snow, with his tunic barely separate from the ground. Night with the lamp rule
            // below is the better picture of the two; see docs/evidence/sapper-lay-mine-anim-g2/note.
            var field = TW.Presentation.Terrain.Biome.NightMud; bool pickedField = false;
            if (System.Enum.TryParse(System.Environment.GetEnvironmentVariable("TW_STILLS_FIELD"), true, out field)) pickedField = true;
            if (pickedField)
            {
                foreach (var g in Object.FindObjectsByType<TW.Presentation.Terrain.GreyboxTerrainView>(FindObjectsSortMode.None)) g.Field = field;
                foreach (var at in Object.FindObjectsByType<TW.Presentation.Terrain.Atmosphere>(FindObjectsSortMode.None)) at.Field = field;
            }
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
            // On a dark field a lamp has to stand in front of him and off the view axis; on a day field none is lit,
            // so the rule has nothing to choose and the cell is picked on open ground alone.
            bool dark = TW.Presentation.SceneMood.Night;
            IReadOnlyList<Vector3> lamps = new List<Vector3>();
            if (dark)
            {
                var night = Object.FindFirstObjectByType<TW.Presentation.Terrain.NightLights>();
                for (int f = 0; f < 600 && (night == null || !night.Built); f++) { yield return null; night = night ?? Object.FindFirstObjectByType<TW.Presentation.Terrain.NightLights>(); }
                Assert.That(night, Is.Not.Null, "no NightLights: no lamp to keep out of his silhouette");
                Assert.That(night.Built, "NightLights never finished building its lamps");
                lamps = night.LampPoints;
            }
            float x = -1f, z = -1f; Vector3 lamp = default;
            for (float zz = 40f; zz < size.y - 40f && x < 0f; zz += 6f)
                for (float xx = 8f; xx < size.x - 8f && x < 0f; xx += 6f)
                    if (Open(map, Host.Local.World, lamps, dark, xx, zz, out var l)) { x = xx; z = zz; lamp = l; }
            Assert.That(x, Is.GreaterThanOrEqualTo(0f), "no cell with open ground and one lamp in front of him, off axis");
            if (!dark) TestContext.Out.WriteLine($"day field {field}: no lamp lit, cell ({x}, {z})");
            float lampDist = dark ? Mathf.Sqrt((lamp.x - x) * (lamp.x - x) + (lamp.z - z) * (lamp.z - z)) : LampNear;
            if (dark) TestContext.Out.WriteLine($"cell ({x}, {z}) lamp {lamp} offset ({lamp.x - x:0.##}, {lamp.z - z:0.##}) dist {lampDist:0.##}");
            if (dark)
            {
                Assert.That(lampDist, Is.InRange(LampNear, LampFar), "the chosen lamp is not 3-7 m away");
                Assert.That(lamp.z, Is.LessThanOrEqualTo(z - 1f), "the lamp stands behind him: its glow would blow out his head");
                Assert.That(Mathf.Abs(lamp.x - x), Is.InRange(LampOffMin, LampOffMax), "the lamp sits on the view axis");
            }

            string s = TankCapture.Spawn(0, (int)InfantryArchetype.Sapper, x, z, 180f);
            Assert.That(s, Does.StartWith("slot"), s);
            int slot = int.Parse(s.Substring(5));
            for (int f = 0; f < 30; f++) yield return null;

            int offset = 0;
            int.TryParse(System.Environment.GetEnvironmentVariable("TW_STILLS_FRAME_OFFSET"), out offset);
            if (offset < 0) offset = 0;
            TestContext.Out.WriteLine("frame offset: " + offset);

            // The pair used to be shot at zoom 6 and come back 119x138 px, with only 37 columns carrying his pose: a
            // shot's zoom is clamped to tc.NearZoom (CaptureRig.Pose), and with the stock camera NearZoom works out
            // near 15, so the camera really stood ~22 m off. Lowering ZoomMin and CloseFitHeight lets the shot's own
            // zoom through; they are fields on live objects, so the finally puts them back.
            var tc = Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
            Assert.That(tc, Is.Not.Null, "no TacticalCamera to frame him with");
            float keepZoomMin = tc.ZoomMin, keepFit = tc.CloseFitHeight, keepYawPin = CaptureRig.Rig.YawPin;
            tc.ZoomMin = 2f; tc.CloseFitHeight = 3f; CaptureRig.Rig.YawPin = 0f;
            // AimY is a WORLD height: at 0.9 the first set looked a metre and a half under his feet, because this
            // ground stands that high, and he came back cut off at the top of the frame. Aim at his chest.
            float aimY = map.Height.Sample(x, z) + 0.9f;

            string idle = Path.Combine(dir, "idle.png");
            string laying = Path.Combine(dir, "laying.png");
            try
            {
                for (int f = 0; f < offset; f++) yield return null;
                CaptureRig.Shot(idle, x, z, 3.4f, 0f, 13f, 1280, 720, aimY);
                yield return Drain(idle);

                // the same command ProvingGround.OrderSappers issues, on his own cell: he lays where he stands, not after walking
                var w = Host.Local.World;
                Host.LocalDriver.Issue(new SimCommand
                {
                    Tick = w.Tick, Player = 0, Type = CommandType.UnitAbility,
                    A = slot, B = (int)UnitAbilityId.LayMine, Pos = w.Position[slot]
                });

                // SapperSystem.LaySeconds is 3 s, so 1.2 s + offset/30 lands mid-lay for offsets 0 / 9 / 18
                float start = Time.time, wait = 1.2f + offset / 30f;
                while (Time.time - start < wait) yield return null;
                CaptureRig.Shot(laying, x, z, 3.4f, 0f, 13f, 1280, 720, aimY);
                yield return Drain(laying);

                TestContext.Out.WriteLine("clip: " + Clip.KneelInspect + " / " + Clip.StandToKneel);
            }
            finally
            {
                tc.ZoomMin = keepZoomMin; tc.CloseFitHeight = keepFit; CaptureRig.Rig.YawPin = keepYawPin;
            }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            Time.captureDeltaTime = 0f;
            yield return new ExitPlayMode();

            Assert.That(File.Exists(idle), "idle still missing");
            Assert.That(File.Exists(laying), "laying still missing");
        }
    }
}
