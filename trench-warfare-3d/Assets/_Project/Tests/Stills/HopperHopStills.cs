// Phase: the battle (2026-10-19) — the picture of the Hopper's hop arc (TankRenderer.HopArc.cs).
//
// Shaped on WalkerStills.cs; read its header for why a still has to be an EditMode test that enters Play itself
// (CaptureRig poses the camera on one frame and photographs on the NEXT, so there is no -executeMethod shape for it,
// and CaptureRig/TankCapture/RiderLab live in the Editor-only TW.Editor).
//
// What would go wrong silently: the arc is in the renderer and green in EditMode arithmetic, and the Hopper still
// reads as a dead-level cruise on screen — too small to see, or hidden behind the hull's own 9 m lift. So this
// photographs the same locked-off camera at the hull's cruise (phase 0), at the top of one arc (phase 0.5), and
// that identical held instant a second time, which is the noise floor the probe divides by. Each of those two
// instants is also shot with the renderer switched off (empty.png, empty_apex.png): one frozen instant with and
// without the hull, so what differs IS the Hopper and no box is guessed.
//
// It is [Explicit] so the ordinary gate skips it: a normal EditMode run must not enter Play. Run it by name, WITH a
// graphics device (-batchmode, NOT -nographics). Stills go to Captures/hopperhop, never into the checkout tree;
// they are copied by hand into docs/evidence/hopper-hop-arc/ with their .json sidecars.
using System.Collections;
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
    public class HopperHopStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        // The rig aims at world y = 0 by default, which at zoom 30 put the hull — 9 m up before the arc even starts —
        // half out of the top of the frame. AimY sits between the cruise and the arc's top, so both are in shot.
        const float Zoom = 30f, Yaw = 0f, Pitch = 25f, AimY = 10.5f;

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();
        static TankRenderer Tanks => Object.FindFirstObjectByType<TankRenderer>();

        static string Dir()
        {
            string root = Directory.GetParent(Application.dataPath).FullName;
            string d = Path.Combine(root, "Captures", "hopperhop");
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

        /// <summary>One frozen instant photographed with the hull and then WITHOUT it.
        ///
        /// The first run shot a single plate before the Hopper was spawned and compared every still with it. That
        /// confounds the measurement: the lamps, the fog and the men move between two instants, so 7% of the frame
        /// differed and the "topmost row of the hull" came out as row 0, the sky. Freeze the clock instead and
        /// toggle the RENDERER (WalkerStills does the same): nothing in the world changes between the two frames
        /// except whether TankRenderer is allowed to submit, so what differs IS the Hopper.</summary>
        static IEnumerator Pair(string with, string without, TankRenderer tanks, float x, float z, string again = null)
        {
            CaptureRig.Hold(30f);                      // Time.timeScale = 0; frames and LateUpdates still run
            for (int f = 0; f < 5; f++) yield return null;
            CaptureRig.Shot(with, x, z, Zoom, Yaw, Pitch, 1600, 900, AimY);
            yield return Drain(with);
            if (again != null)
            {
                CaptureRig.Shot(again, x, z, Zoom, Yaw, Pitch, 1600, 900, AimY);   // the same held instant twice
                yield return Drain(again);
            }
            tanks.enabled = false;
            for (int f = 0; f < 5; f++) yield return null;
            CaptureRig.Shot(without, x, z, Zoom, Yaw, Pitch, 1600, 900, AimY);
            yield return Drain(without);
            tanks.enabled = true;
            CaptureRig.Release();
            for (int f = 0; f < 5; f++) yield return null;
        }

        [UnityTest, Explicit("Stills: run by name, with a graphics device.")]
        public IEnumerator TheHoppersHopIsPhotographedOverATrench()
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

            Assert.That(FindCrossing(Host.Local.Map, out float x, out float z), Is.True, "no trench or wire in the map");
            TestContext.Out.WriteLine($"crossing at ({x:0.0}, {z:0.0}), layer {Host.Local.Map.LayerAt(new float3(x, 0f, z))}");

            string dir = Dir();
            string empty = Path.Combine(dir, "empty.png"), cruise = Path.Combine(dir, "cruise.png");
            string apex = Path.Combine(dir, "apex.png"), apexB = Path.Combine(dir, "apex_b.png");
            string emptyApex = Path.Combine(dir, "empty_apex.png");

            string spawned = TankCapture.Spawn(0, VehicleArchetype.Hopper, x, z, 0f);
            Assert.That(spawned, Does.StartWith("slot "), "the spawn itself failed: " + spawned);
            int slot = int.Parse(spawned.Substring(5));
            RiderLab.Stop(slot);                       // held over the one crossing, so the arc cycles in place
            for (int f = 0; f < 60; f++) yield return null;
            var tanks = Tanks;
            Assert.That(tanks, Is.Not.Null, "no TankRenderer");
            Assert.That(tanks.Ready, Is.True, "TankRenderer is not drawing: every hull would be missing");
            Assert.That(Host.Local.World.IsAlive(slot), Is.True, "the Hopper died before it was photographed");

            // Phase 0 is the cruise. Wait for the arc to wrap, then photograph: posed this frame, shot the next, so
            // the hull is one pinned step (1/48 of an arc, 7 cm) off its cruise. That is the "level" picture.
            for (int f = 0; f < 300 && tanks.HopArcPhaseOf(slot) < 0.6f; f++) yield return null;
            for (int f = 0; f < 300 && tanks.HopArcPhaseOf(slot) > 0.02f; f++) yield return null;
            TestContext.Out.WriteLine($"cruise at phase {tanks.HopArcPhaseOf(slot):0.000}");
            yield return Pair(cruise, empty, tanks, x, z);

            // Up to the top of the next arc, then FREEZE and shoot the same held instant twice. Frozen, the two
            // apex stills differ only by what the rig itself cannot repeat: that difference is the noise floor.
            for (int f = 0; f < 300 && tanks.HopArcPhaseOf(slot) < 0.46f; f++) yield return null;
            TestContext.Out.WriteLine($"apex at phase {tanks.HopArcPhaseOf(slot):0.000}");
            yield return Pair(apex, emptyApex, tanks, x, z, apexB);

            foreach (string p in new[] { empty, cruise, apex, apexB, emptyApex })
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
