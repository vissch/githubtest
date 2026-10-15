// Phase: units-one-size-pictures — the before and after of the owner's "units... should allways be one size"
// (eba897db, docs/reference/decisions.md "d-one-size"). One battle moment — infantry, a rider, a tank and a walker
// in view — photographed at three zooms so the grow that used to happen (and now does not) can be seen and measured.
// TW_STILLS_DIR says where the stills go (default %TEMP%/tw-zoomscale: never inside the checkout).
using System.Collections;
using System.IO;
using NUnit.Framework;
using UnityEngine;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using TW.Editor;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Presentation.Units;
using TW.Sim;
using TW.Sim.Nav;
using TW.UI;

namespace TW.Tests
{
    public class ZoomScaleStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        /// <summary>Wait for a shot to be ON DISK, and FAIL if it never arrives (WalkerStills' Drain).</summary>
        static IEnumerator Drain(string path = null, int cap = 900)
        {
            for (int f = 0; f < cap && CaptureRig.Pending() != "0"; f++) yield return null;
            Assert.That(CaptureRig.Pending(), Is.EqualTo("0"),
                $"the capture queue stalled with {CaptureRig.Pending()} shots outstanding after {cap} frames");
            if (path == null) yield break;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
            Assert.That(File.Exists(path), Is.True, $"the queue drained but {Path.GetFileName(path)} was never written");
        }

        [UnityTest, Explicit("Infantry, a rider, a tank and a walker at three zooms; run by name, with a graphics device.")]
        public IEnumerator UnitsArePhotographedAtThreeZooms()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            yield return new EnterPlayMode();
            yield return Shoot();     // a fresh enumerator after the reload (WalkerStills' lesson)
        }

        static IEnumerator Shoot()
        {
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            for (int f = 0; f < 120; f++) yield return null;

            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-zoomscale");
            Directory.CreateDirectory(dir);

            // quiet, and a still sky: nobody deploys, nobody shells the lane
            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<TW.Sim.Match.AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            for (int f = 0; f < 120; f++) yield return null;
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;

            // a Pincer walker with eight riflemen climbing aboard: infantry AND a rider in one call
            string setup = RiderLab.Setup(6, 8);
            TestContext.Out.WriteLine("RiderLab.Setup: " + setup);
            Assert.That(setup, Does.StartWith("slot "), "RiderLab.Setup failed: " + setup);
            int walkerSlot = int.Parse(setup.Substring(5, setup.IndexOf(' ', 5) - 5));
            RiderLab.Panel(false);

            var wp = Host.Local.World.Position[walkerSlot];
            string spawnedTank = TankCapture.Spawn(0, VehicleArchetype.Maw, wp.x + 18f, wp.z - 6f, 0f);
            TestContext.Out.WriteLine("tank spawn: " + spawnedTank);
            Assert.That(spawnedTank, Does.StartWith("slot "), "tank spawn failed: " + spawnedTank);
            int tankSlot = int.Parse(spawnedTank.Substring(5));

            for (int f = 0; f < 90; f++) yield return null;   // the men run in and climb aboard

            RiderLab.Stop(walkerSlot);
            RiderLab.Stop(tankSlot);
            for (int f = 0; f < 10; f++) yield return null;
            CaptureRig.Hold(30f);                             // nothing moves between shots
            for (int f = 0; f < 5; f++) yield return null;

            var vat = Object.FindFirstObjectByType<VATRenderer>();
            Assert.That(vat, Is.Not.Null, "no VATRenderer in the scene");
            var tanks = Object.FindFirstObjectByType<TankRenderer>();
            Assert.That(tanks, Is.Not.Null, "no TankRenderer in the scene");
            var tankModel = TankModel.Load("Maw", VehicleArchetype.Maw, "Hull", VehicleSize.Tank);
            var walkerModel = TankModel.Load("Pincer", VehicleArchetype.Pincer, "Body", VehicleSize.Walker);
            Assert.That(tankModel, Is.Not.Null, "Maw did not load from Resources/Vehicles/Maw");
            Assert.That(walkerModel, Is.Not.Null, "Pincer did not load from Resources/Vehicles/Pincer");

            var focus = Host.Local.World.Position[walkerSlot];
            var zooms = new (string tag, float zoom)[] { ("close", 10f), ("tactical", 30f), ("far", 120f) };
            var log = new System.Text.StringBuilder();
            int written = 0;
            foreach (var (tag, zoom) in zooms)
            {
                string path = Path.Combine(dir, $"z{zoom:000}.png");
                CaptureRig.Shot(path, focus.x, focus.z, zoom, -45f, 25f, 1280, 720);
                yield return Drain(path);
                if (File.Exists(path) && new FileInfo(path).Length > 20000) written++;

                float manH = vat.ScaleAt(zoom) * FigureMetrics.BakeHeightM;
                float riderH = tanks.RiderScaleAt(vat, zoom) * FigureMetrics.BakeHeightM;
                log.AppendLine($"zoom {zoom:0} ({tag}): man {manH:0.00} m, rider {riderH:0.00} m, tank {MachineScale.DrawnHeightM(tankModel, zoom):0.00} m, walker {MachineScale.DrawnHeightM(walkerModel, zoom):0.00} m");
            }
            File.WriteAllText(Path.Combine(dir, "heights.txt"), log.ToString());

            CaptureRig.Release();
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            yield return new ExitPlayMode();

            TestContext.Out.WriteLine(log.ToString());
            TestContext.Out.WriteLine($"wrote {written} stills to {dir}");
            Assert.That(written, Is.EqualTo(3), "not every zoom was photographed");
        }
    }
}
