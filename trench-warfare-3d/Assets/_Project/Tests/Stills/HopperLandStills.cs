// Phase: the battle (2026-10-27) — the picture of the Hopper's LAND frame (TankRenderer.HopArc.cs).
//
// Shaped on HopperHopStills.cs; read its header for why a still has to be an EditMode test that enters Play itself
// (CaptureRig poses the camera on one frame and photographs on the NEXT, so there is no -executeMethod shape for it,
// and CaptureRig/TankCapture/RiderLab live in the Editor-only TW.Editor).
//
// What would go wrong silently: the hop arc settles the Hopper back over the trench lip and the ground under it stays
// clean, so the hop reads as a float, not as weight coming down. The owner saw exactly that in apex.png. Nothing
// outside the renderer could see the frame the arc wraps on (`landed`), so no still could be taken OF that frame:
// TankRenderer.HopArcLandingsOf(slot) exists for this test to wait on.
//
// Three landings are photographed, each the same way: wait for the count to rise, let exactly 3 pinned frames pass
// (the dust card is then 0.1 s old and has grown a little; deterministic because Time.captureDeltaTime is pinned),
// then freeze the clock and shoot the still AND its own plate — the same instant with TankRenderer switched off.
// TankRenderer draws its own flipbooks, so the plate drops the hull and the dust together and what differs is the
// Hopper plus what it put on the ground. Three landings give the spread of one build, which is the probe's bar.
//
// It is [Explicit] so the ordinary gate skips it: a normal EditMode run must not enter Play. Run it by name, WITH a
// graphics device (-batchmode, NOT -nographics). Stills go to Captures/hopperland, never into the checkout tree;
// they are copied by hand into docs/evidence/hopper-hop-arc-g2/ with their .json sidecars.
using System.Collections;
using System.Collections.Generic;
using System.IO;
using NUnit.Framework;
using UnityEngine;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using Unity.Mathematics;
using TW.Editor;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Sim;
using TW.Sim.Terrain;
using TW.UI;

namespace TW.Tests
{
    public class HopperLandStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        // The same locked-off camera as HopperHopStills: AimY sits between the cruise and the arc's top, and at zoom
        // 30 (the budget view the owner asked for) the ground under the hull is in shot, which is where the dust is.
        const float Zoom = 30f, Yaw = 0f, Pitch = 25f, AimY = 10.5f;
        const int Landings = 3, FramesAfterLanding = 3;

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();
        static TankRenderer Tanks => Object.FindFirstObjectByType<TankRenderer>();

        static string Dir()
        {
            string root = Directory.GetParent(Application.dataPath).FullName;
            string d = Path.Combine(root, "Captures", "hopperland");
            Directory.CreateDirectory(d);
            return d;
        }

        /// <summary>Wait for a shot to be on disk (WalkerStills.Drain: the queue reads "0" one render before the PNG
        /// exists, so waiting on the queue alone reads the previous file).</summary>
        static IEnumerator Drain(string path, int cap = 900)
        {
            for (int f = 0; f < cap && CaptureRig.Pending() != "0"; f++) yield return null;
            Assert.That(CaptureRig.Pending(), Is.EqualTo("0"), "the capture queue stalled");
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
            Assert.That(File.Exists(path), Is.True, $"the queue drained but {Path.GetFileName(path)} was never written");
        }

        /// <summary>A point on a trench or wire belt, found in the map instead of typed in: the hop only starts where
        /// the ground is worth clearing (TankRenderer.HopArcCrossing), so the still has to stand on one.</summary>
        static bool FindCrossing(MapData map, out float cx, out float cz)
        {
            cx = cz = 0f;
            var size = map.SizeMeters;
            float mid = size.x * 0.5f;
            for (float z = 30f; z < size.y - 30f; z += 2f)
                for (float d = 0f; d < size.x * 0.35f; d += 2f)
                    for (int s = -1; s <= 1; s += 2)
                    {
                        float x = mid + s * d;
                        if (TankRenderer.HopArcCrossing((byte)map.LayerAt(new float3(x, 0f, z)))) { cx = x; cz = z; return true; }
                    }
            return false;
        }

        /// <summary>One frozen instant photographed with the hull and then WITHOUT it (HopperHopStills.Pair): the
        /// clock is held, so nothing in the world changes between the two frames except whether TankRenderer is
        /// allowed to submit, and what differs IS the Hopper and the dust it put on the ground.</summary>
        static IEnumerator Pair(string with, string without, TankRenderer tanks, float x, float z)
        {
            CaptureRig.Hold(30f);                      // Time.timeScale = 0; frames and LateUpdates still run
            for (int f = 0; f < 5; f++) yield return null;
            CaptureRig.Shot(with, x, z, Zoom, Yaw, Pitch, 1600, 900, AimY);
            yield return Drain(with);
            tanks.enabled = false;
            for (int f = 0; f < 5; f++) yield return null;
            CaptureRig.Shot(without, x, z, Zoom, Yaw, Pitch, 1600, 900, AimY);
            yield return Drain(without);
            tanks.enabled = true;
            CaptureRig.Release();
            for (int f = 0; f < 5; f++) yield return null;
        }

        [UnityTest, Explicit("Stills: run by name, with a graphics device.")]
        public IEnumerator TheHoppersLandingIsPhotographedThreeTimesAtTheTrenchLip()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            for (int f = 0; f < 120; f++) yield return null;

            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;
            Time.captureDeltaTime = 1f / 30f;          // one pinned step a frame, so two runs sample the arc alike
            Assert.That(SceneMood.Night, Is.True, "the goal asks for night, and this scene's biome is not dark");

            Assert.That(FindCrossing(Host.Local.Map, out float x, out float z), Is.True, "no trench or wire in the map");
            TestContext.Out.WriteLine($"crossing at ({x:0.0}, {z:0.0}), layer {Host.Local.Map.LayerAt(new float3(x, 0f, z))}");

            string dir = Dir();
            string spawned = TankCapture.Spawn(0, VehicleArchetype.Hopper, x, z, 0f);
            Assert.That(spawned, Does.StartWith("slot "), "the spawn itself failed: " + spawned);
            int slot = int.Parse(spawned.Substring(5));
            RiderLab.Stop(slot);                       // held over the one crossing, so the arc cycles in place
            for (int f = 0; f < 60; f++) yield return null;
            var tanks = Tanks;
            Assert.That(tanks, Is.Not.Null, "no TankRenderer");
            Assert.That(tanks.Ready, Is.True, "TankRenderer is not drawing: every hull would be missing");
            Assert.That(Host.Local.World.IsAlive(slot), Is.True, "the Hopper died before it was photographed");

            var shots = new List<string>();
            for (int n = 1; n <= Landings; n++)
            {
                int was = tanks.HopArcLandingsOf(slot);
                for (int f = 0; f < 300 && tanks.HopArcLandingsOf(slot) == was; f++) yield return null;
                Assert.That(tanks.HopArcLandingsOf(slot), Is.GreaterThan(was),
                            $"the arc never wrapped a {n}th time: the Hopper is not hopping over this ground");
                for (int f = 0; f < FramesAfterLanding; f++) yield return null;
                TestContext.Out.WriteLine($"landing {n}: count {tanks.HopArcLandingsOf(slot)}, phase {tanks.HopArcPhaseOf(slot):0.000}");
                string land = Path.Combine(dir, $"land_{n}.png"), plate = Path.Combine(dir, $"land_{n}_empty.png");
                yield return Pair(land, plate, tanks, x, z);
                shots.Add(land); shots.Add(plate);
            }

            foreach (string p in shots)
            {
                Assert.That(File.Exists(p), Is.True, p + " was never written");
                Assert.That(new FileInfo(p).Length, Is.GreaterThan(1000), p + " is empty");
                TestContext.Out.WriteLine($"{Path.GetFileName(p)} {new FileInfo(p).Length} bytes");
            }
            Assert.That(Host.Local.World.IsAlive(slot), Is.True, "the Hopper was dead by the last still");

            Time.captureDeltaTime = 0f;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            yield return new ExitPlayMode();
        }
    }
}
