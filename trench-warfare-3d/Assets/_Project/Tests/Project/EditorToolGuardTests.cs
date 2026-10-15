// Phase: tooling (code review 2026-10-04, E3 and E4). Two faults of the editor labs that an agent could not see from
// what they answered: TankCapture.Get() looked its DontSave Shooter up with FindFirstObjectByType, which never returns
// one, so every Shot/Follow made another Shooter and a Follow could not be stopped; and out of Play every lab entry
// still answered "queued ..." / "filming ..." while nothing could ever happen.
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Editor;

namespace TW.Tests
{
    public class EditorToolGuardTests
    {
        const string OutOfPlay = "not in Play";

        [Test]
        public void TwoCallsShareOneShooter()   // [E3]
        {
            var get = typeof(TankCapture).GetMethod("Get",
                BindingFlags.Static | BindingFlags.NonPublic | BindingFlags.Public);
            Assert.IsNotNull(get, "[E3] TankCapture.Get is gone");
            var a = get.Invoke(null, null);
            var b = get.Invoke(null, null);
            Assert.AreSame(a, b, "[E3] a second call made a second Shooter");
            // Resources.FindObjectsOfTypeAll does find DontSave objects; FindFirstObjectByType is the one that does not.
            Assert.AreEqual(1, Resources.FindObjectsOfTypeAll<TankCapture.Shooter>().Length,
                "[E3] more than one TankCapture Shooter is alive");
        }

        [Test]
        public void OutOfPlayEveryToolSaysSo()   // [E4]
        {
            var said = new (string what, string answer)[]
            {
                ("CaptureRig.Shot", CaptureRig.Shot("e4.png", 0f, 0f, 20f, 0f)),
                ("CaptureRig.ShotCrowd", CaptureRig.ShotCrowd("e4.png", 20f, 0f)),
                ("CaptureRig.Series", CaptureRig.Series("e4", "e4", 0f, 0f, 20f, 0f, 25f, 2)),
                ("CaptureRig.Profile", CaptureRig.Profile("e4.json", 2)),
                ("TankCapture.Shot", TankCapture.Shot("e4.png")),
                ("TankCapture.Follow", TankCapture.Follow(1)),
                ("RiderLab.Film", RiderLab.Film("e4-film")),
                ("RiderLab.VolleyShot", RiderLab.VolleyShot("e4.png")),
                ("WeightLab.Knob", WeightLab.Knob("tank.weight", "1")),
                ("WeightLab.Trace", WeightLab.Trace(1, "e4.csv", 1f)),
                ("LookLab.Knob", LookLab.Knob("look.lift", "1")),
                ("LookLab.Clear", LookLab.Clear()),
            };
            foreach (var s in said)
                Assert.That(s.answer, Does.Contain(OutOfPlay),
                    "[E4] " + s.what + " answered \"" + s.answer + "\" in edit mode, where nothing can happen");
            Assert.AreEqual("0", CaptureRig.Pending(), "[E4] an edit-mode call queued a shot nobody will ever take");
        }

        [Test]
        public void TheAssetScaleAuditRefusesToRunInPlay()   // [E8]
        {
            // in Play it would build and dispose a second BattlefieldKit, and the dispose takes the live kit's global
            // _PigmentSheet down with it. It has to refuse before the kit is built, so this costs nothing.
            string file = System.IO.Path.Combine(System.IO.Path.GetTempPath(), "e8-audit.md");
            if (System.IO.File.Exists(file)) System.IO.File.Delete(file);
            UnityEngine.TestTools.LogAssert.Expect(LogType.Warning, new System.Text.RegularExpressions.Regex("Asset scale audit: " + OutOfPlay + ".*"));
            Assert.IsNull(AssetScaleAudit.Write(file, playing: true), "[E8] the audit ran while the game was in Play");
            Assert.IsFalse(System.IO.File.Exists(file), "[E8] the audit wrote its report while the game was in Play");
        }

        [Test]
        public void TheLabPutsTheCameraBackAsItFoundIt()   // [E12]
        {
            var go = new GameObject("e12-tc");
            try
            {
                var c = go.AddComponent<TW.Presentation.Tactical.TacticalCamera>();
                c.ZoomMin = 9f; c.Pitch = 50f; c.ClosePitch = 55f;
                RiderLab.Camera(0, 20f, 0f, 30f);   // answers E4's "not in Play"; the camera writes happen first
                Assert.AreEqual(4f, c.ZoomMin, "[E12] the lab did not lower ZoomMin, so there is nothing to restore");
                Assert.AreEqual(30f, c.Pitch, "[E12] the lab did not write Pitch");
                RiderLab.CameraRestore();
                Assert.AreEqual(9f, c.ZoomMin, "[E12] ZoomMin was never put back");
                Assert.AreEqual(50f, c.Pitch, "[E12] Pitch was never put back");
                Assert.AreEqual(55f, c.ClosePitch, "[E12] ClosePitch was never put back");
            }
            finally { RiderLab.CameraRestore(); Object.DestroyImmediate(go); }
        }

        [Test]
        public void UnfreezingGivesBackTheScaleThatWasThere()   // [E12]
        {
            var go = new GameObject("e12-host");
            try
            {
                var h = go.AddComponent<TW.Presentation.SimHost>();
                h.TimeScale = 0.5f;
                RiderLab.Freeze(true);
                Assert.AreEqual(0f, h.TimeScale, "[E12] Freeze(true) did not stop the sim clock");
                RiderLab.Freeze(false);
                Assert.AreEqual(0.5f, h.TimeScale,
                    "[E12] Freeze(false) assumed a scale of 1: a half-speed world came back at " + h.TimeScale);
            }
            finally { Object.DestroyImmediate(go); }
        }

        [Test]
        public void AMeasuredRunDrawsNoPropsForTheSceneView()   // [E10]
        {
            var camGo = new GameObject("e10-cam");
            var profGo = new GameObject("e10-prof");
            try
            {
                TW.Presentation.Terrain.BattlefieldProps.EditorCamera = camGo.AddComponent<Camera>();
                var prof = profGo.AddComponent<CaptureRig.Profiler>();
                prof.Begin(System.IO.Path.Combine(System.IO.Path.GetTempPath(), "e10-profile.json"), 2);
                var late = typeof(CaptureRig.Profiler).GetMethod("LateUpdate",
                    BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public);
                Assert.IsNotNull(late, "[E10] CaptureRig.Profiler.LateUpdate is gone");
                late.Invoke(prof, null);
                Assert.IsNull(TW.Presentation.Terrain.BattlefieldProps.EditorCamera,
                    "[E10] the profiler left the Scene view's camera in place: the props are culled against two frusta");
            }
            finally
            {
                Object.DestroyImmediate(profGo); Object.DestroyImmediate(camGo);
                TW.Presentation.Terrain.BattlefieldProps.EditorCamera = null;
            }
        }

        [TearDown]
        public void Clean()
        {
            Drop<CaptureRig.Rig>(typeof(CaptureRig), "rig", null);
            Drop<TankCapture.Shooter>(typeof(TankCapture), "shooter", null);
            Drop<RiderLab.Bench>(null, null, typeof(RiderLab.Bench));
            Drop<WeightLab.Bench>(null, null, typeof(WeightLab.Bench));
            Drop<LookLab.Lab>(null, null, typeof(LookLab.Lab));
            if (System.IO.Directory.Exists("e4-film")) System.IO.Directory.Delete("e4-film", true);
        }

        /// <summary>Destroys every object of this kind (DontSave ones too) and nulls the static that holds it.</summary>
        static void Drop<T>(System.Type owner, string field, System.Type instanceHolder) where T : Component
        {
            foreach (var c in Resources.FindObjectsOfTypeAll<T>())
                if (c != null) Object.DestroyImmediate(c.gameObject);
            if (owner != null)
                owner.GetField(field, BindingFlags.Static | BindingFlags.NonPublic | BindingFlags.Public)
                     ?.SetValue(null, null);
            if (instanceHolder != null)
                instanceHolder.GetField("Instance", BindingFlags.Static | BindingFlags.Public)
                              ?.SetValue(null, null);
        }
        [Test]   // [M6] TankCapture.Spawn took any team and any archetype and clamped them into the roster
        public void ASpawnOffTheMatchTableIsRefused()
        {
            Assert.IsNotNull(TankCapture.SpawnError(2, 0), "[M6] team 2 is not a team, so a spawn must be refused");
            Assert.IsNotNull(TankCapture.SpawnError(0, -1), "[M6] archetype -1 is not in the match table");
            Assert.IsNull(TankCapture.SpawnError(0, 0), "[M6] team 0, archetype 0 is a rifleman and must be allowed");
        }

        [Test]   // [M6] RiderLab parsed every spawn answer, "slot -1" included, and then indexed the world with it
        public void OnlyARealSlotIsReadOutOfASpawnAnswer()
        {
            Assert.IsFalse(RiderLab.Slot("slot -1", out _), "[M6] slot -1 is not a slot");
            Assert.IsFalse(RiderLab.Slot("no SimHost", out _), "[M6] an error message holds no slot");
            Assert.IsTrue(RiderLab.Slot("slot 7", out int s), "[M6] slot 7 is a slot");
            Assert.AreEqual(7, s, "[M6] the slot read back must be the one the spawn said");
        }
    }
}
