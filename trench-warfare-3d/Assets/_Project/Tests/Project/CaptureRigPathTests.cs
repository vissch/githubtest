// Phase: tooling (code review 2026-10-04, E1 and E2). Two faults of the capture rig that only ever showed up as a
// capture that never finished: a bare file name threw out of Directory.CreateDirectory("") so nothing was written,
// and a throw inside a set left `running` true and TacticalCamera disabled, so every later Shot answered "queued"
// forever and the camera never came back.
using System.Collections;
using System.IO;
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Editor;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class CaptureRigPathTests
    {
        const string BareName = "e1-bare-name.json";
        GameObject tcGo, camGo, rigGo;

        [Test]
        public void ABareFileNameStillGetsItsDirectory()   // [E1]
        {
            Assert.DoesNotThrow(() => CaptureRig.Textures(BareName),
                "[E1] a bare file name threw instead of being written beside the project");
        }

        [Test]
        public void StressDoesNotDefaultToABareFileName()   // [E1]
        {
            var p = typeof(CaptureRig).GetMethod("Stress").GetParameters()[1];
            Assert.AreEqual("path", p.Name, "[E1] Stress's second parameter is no longer the path");
            var d = p.DefaultValue as string;
            Assert.IsTrue(d == null || Path.IsPathRooted(d),
                "[E1] Stress still defaults to a relative path: \"" + d + "\"");
        }

        [Test]
        public void AnAbandonedSetPutsTheCameraBack()   // [E2]
        {
            tcGo = new GameObject("e2-tc");
            var tc = tcGo.AddComponent<TacticalCamera>();
            camGo = new GameObject("e2-cam") { tag = "MainCamera" };
            camGo.AddComponent<Camera>();
            rigGo = new GameObject("e2-rig");
            var rig = rigGo.AddComponent<CaptureRig.Rig>();
            rig.Queue.Enqueue(new CaptureRig.Rig.Shot { Path = "e2-never-written.png", Zoom = 20f });

            var run = typeof(CaptureRig.Rig).GetMethod("Run",
                BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public);
            Assert.IsNotNull(run, "[E2] CaptureRig.Rig.Run is gone");
            var e = (IEnumerator)run.Invoke(rig, null);
            Assert.IsTrue(e.MoveNext(), "[E2] the set ended before its first pose frame");
            Assert.IsFalse(tc.enabled, "[E2] the set never switched the tactical camera off, so it posed nothing");

            ((System.IDisposable)e).Dispose();   // what an abandoned coroutine does: the finally must run

            Assert.IsTrue(tc.enabled, "[E2] an abandoned set left TacticalCamera disabled: the camera never comes back");
            var running = typeof(CaptureRig.Rig).GetField("running",
                BindingFlags.Instance | BindingFlags.NonPublic);
            Assert.IsNotNull(running, "[E2] CaptureRig.Rig.running is gone");
            Assert.IsFalse((bool)running.GetValue(rig),
                "[E2] an abandoned set left running true: every later Shot answers \"queued\" forever");
        }

        [Test]
        public void AHeldPausedWorldComesBackPaused()   // [E12]
        {
            Time.timeScale = 0f;
            CaptureRig.Hold();
            CaptureRig.Release();
            Assert.AreEqual(0f, Time.timeScale,
                "[E12] Release assumed the clock had been running: a world held while paused came back at " + Time.timeScale);
        }

        [TearDown]
        public void Clean()
        {
            Time.timeScale = 1f;
            if (File.Exists(BareName)) File.Delete(BareName);
            foreach (var go in new[] { tcGo, camGo, rigGo }) if (go != null) Object.DestroyImmediate(go);
            tcGo = camGo = rigGo = null;
        }
    }
}
