// Phase: gore-by-zoom — the measurement behind the owner's "the red gore mesh coming out of the shot units is
// sometimes a bit big, we want this to scale based off the zoom" (2026-10-13). Part 1 ran it for the before, part 2
// for the after, same scene and same staging, so the two sets of numbers and pictures stand side by side. Men who can take many hits are shot by several weapons
// (a machine gun, rifles, a sniper and an SMG) in the real battle scene, at the closest zoom, the play zoom and the
// farthest zoom that still shows blood. DebrisRenderer.Watch records every red piece thrown with the size it was
// thrown at; the numbers go to gore.txt as a share of the drawn man, and a PNG is shot at each zoom.
// TW_STILLS_DIR says where they go (default %TEMP%/tw-gorezoom: never inside the checkout).
using System.Collections;
using System.Collections.Generic;
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
using TW.UI;

namespace TW.Tests
{
    public class GoreZoomStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        /// <summary>The closest the camera goes (TacticalCamera.ZoomMin), the zoom the game is played at, and the
        /// farthest zoom blood is drawn at all (CombatFx.BloodFarZoom; the camera itself reaches 600).</summary>
        static readonly float[] Zooms = { 6f, 30f, 120f };
        /// <summary>Red pieces wanted at each zoom before the shot (the owner's "over at least twenty hits").</summary>
        const int WantSamples = 20;
        /// <summary>Seconds of fighting waited out at each zoom for those samples.</summary>
        const float WaitSeconds = 90f;

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        /// <summary>Wait for a shot to be ON DISK, and FAIL if it never arrives (ZoomScaleStills' Drain).</summary>
        static IEnumerator Drain(string path = null, int cap = 900)
        {
            for (int f = 0; f < cap && CaptureRig.Pending() != "0"; f++) yield return null;
            Assert.That(CaptureRig.Pending(), Is.EqualTo("0"),
                $"the capture queue stalled with {CaptureRig.Pending()} shots outstanding after {cap} frames");
            if (path == null) yield break;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
            Assert.That(File.Exists(path), Is.True, $"the queue drained but {Path.GetFileName(path)} was never written");
        }

        /// <summary>Red: the hit blood's HitRed (0.85, 0.03, 0.03) and a gore lump's GoreRed (0.55, 0.04, 0.04), and
        /// nothing else thrown on a battlefield (earth, wood and stone all carry g and b well up).</summary>
        static bool IsRed(Color c) => c.r > 0.4f && c.g < 0.15f && c.b < 0.15f;

        [UnityTest, Explicit("Several weapons on men who bleed, at three zooms; run by name, with a graphics device.")]
        public IEnumerator GoreIsMeasuredAndPhotographedAtThreeZooms()
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
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-gorezoom");
            Directory.CreateDirectory(dir);

            // a still sky and no shelling, but the peer's own men are left as they are: switching the scripted peer off
            // took the shooters' orders away with it and nothing fired at all (first run: 0 red pieces in 1800 frames)
            Host.WriteWorlds(m => { var b = m.World.GetSystem<TW.Sim.Match.AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            for (int f = 0; f < 60; f++) yield return null;
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;

            // the wounds staging: six men tough enough to be hit many times, a machine gunner and two riflemen on them
            float x = 100f, z = 120f;
            string staged = DeathLab.Scene("wounds", x, z);
            TestContext.Out.WriteLine("DeathLab wounds: " + staged);
            Assert.That(staged, Does.StartWith("slots "), "the wounds staging failed: " + staged);
            // and two more weapons, so the measurement covers a light hit and a heavy one
            string sniper = TankCapture.Spawn(1, InfantryArchetype.Sniper, x + 8f, z + 30f, 180f);
            string smg = TankCapture.Spawn(1, InfantryArchetype.Assault, x, z + 28f, 180f);
            TestContext.Out.WriteLine("sniper: " + sniper + "; smg: " + smg);
            Assert.That(sniper, Does.StartWith("slot "), "the sniper did not spawn: " + sniper);
            Assert.That(smg, Does.StartWith("slot "), "the SMG man did not spawn: " + smg);

            var vat = Object.FindFirstObjectByType<VATRenderer>();
            Assert.That(vat, Is.Not.Null, "no VATRenderer in the scene");
            var cam = Camera.main;
            Assert.That(cam, Is.Not.Null, "no main camera");
            var tactical = cam.GetComponent<TacticalCamera>();
            Assert.That(tactical, Is.Not.Null, "the main camera is not the TacticalCamera, so the zoom cannot be set");

            var log = new System.Text.StringBuilder();
            int written = 0;
            foreach (float zoom in Zooms)
            {
                // the zoom the EFFECTS see (CombatFx.ViewNow reads the live camera), not only the shot's
                tactical.Frame(new Vector2(x + 3f, z + 2f), zoom);
                for (int f = 0; f < 30; f++) yield return null;

                var sizes = new List<float>();
                int thrown = 0;
                DebrisRenderer.Watch = (piece, scale, tint) => { thrown++; if (IsRed(tint)) sizes.Add(scale); };
                // waited out in SECONDS, not frames: in batch mode a frame is a millisecond, so 1800 of them were three
                // seconds of fighting and nothing had been shot yet (second run: 0 pieces of any kind thrown)
                float t0 = Time.realtimeSinceStartup;
                while (sizes.Count < WantSamples && Time.realtimeSinceStartup - t0 < WaitSeconds) yield return null;
                DebrisRenderer.Watch = null;

                // frozen only for the shot: Hold stops the clock dead (timeScale 0), and held while measuring it
                // stopped the fight too — 90 s of a still world threw not one piece (third run)
                string path = Path.Combine(dir, $"gore-z{zoom:000}.png");
                CaptureRig.Hold(30f);
                CaptureRig.Shot(path, x + 3f, z + 2f, zoom, -45f, 25f, 1280, 720);
                yield return Drain(path);
                CaptureRig.Release();
                TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;   // Release gives the sky its own clock back
                if (File.Exists(path) && new FileInfo(path).Length > 20000) written++;

                float manH = vat.ScaleAt(zoom) * FigureMetrics.BakeHeightM;   // the drawn man, in metres
                float boost = CombatFx.HitZoomBoost(zoom);
                float mean = 0f, most = 0f;
                foreach (float s in sizes) { mean += s; if (s > most) most = s; }
                if (sizes.Count > 0) mean /= sizes.Count;
                // the card's size from the one place it now lives (GoreScale, gore-by-zoom part 2); before part 2 this
                // read 0.7f * figure * boost and (0.7f + 0.5f) * figure * boost, the numbers that were in CombatFx
                float cardLight = GoreScale.Sized(GoreScale.CardShare, zoom);
                float cardHeavy = GoreScale.Sized(GoreScale.CardShare + GoreScale.CardHeavyShare, zoom);
                log.AppendLine($"zoom {zoom:0}: man {manH:0.00} m; {sizes.Count} red pieces, mean {mean:0.000} m " +
                               $"({mean / manH:0.0%} of the man), largest {most:0.000} m ({most / manH:0.0%}); " +
                               $"zoom boost x{boost:0.00}; blood card {cardLight:0.00}-{cardHeavy:0.00} m " +
                               $"({cardLight / manH:0.0%}-{cardHeavy / manH:0.0%} of the man)");
                TestContext.Out.WriteLine($"zoom {zoom:0}: {thrown} pieces thrown in all, {sizes.Count} of them red");
                Assert.That(sizes.Count, Is.GreaterThanOrEqualTo(WantSamples),
                    $"only {sizes.Count} red pieces of {thrown} thrown at zoom {zoom:0} in {WaitSeconds:0} s of fighting at that zoom: nobody is bleeding");
            }
            File.WriteAllText(Path.Combine(dir, "gore.txt"), log.ToString());

            CaptureRig.Release();
            DebrisRenderer.Watch = null;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            yield return new ExitPlayMode();

            TestContext.Out.WriteLine(log.ToString());
            TestContext.Out.WriteLine($"wrote {written} stills to {dir}");
            Assert.That(written, Is.EqualTo(Zooms.Length), "not every zoom was photographed");
        }
    }
}
