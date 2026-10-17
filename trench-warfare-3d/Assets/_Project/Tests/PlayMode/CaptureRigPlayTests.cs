// Phase: review fix (2026-10-17) — a shot whose directory CaptureRig.Rig.Run cannot write used to kill the
// coroutine mid-queue, leaving the TacticalCamera disabled and `running` stuck true for the rest of the Play
// session. This proves the queue drains and the camera is restored even when one shot fails.
#if UNITY_EDITOR
using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class CaptureRigPlayTests
    {
        GameObject camGo, tcGo;
        string tempFile, tempDir;
        List<string> logs;
        Application.LogCallback handler;

        [UnitySetUp]
        public IEnumerator SetUp()
        {
            camGo = new GameObject("cam-test");
            var cam = camGo.AddComponent<Camera>();
            camGo.tag = "MainCamera";
            tcGo = new GameObject("tc-test");
            tcGo.AddComponent<TacticalCamera>();

            logs = new List<string>();
            handler = (msg, st, type) => logs.Add(msg);
            Application.logMessageReceived += handler;

            tempFile = Path.GetTempFileName();
            tempDir = Path.Combine(Path.GetTempPath(), "captureRigTest_" + Guid.NewGuid().ToString("N"));
            yield return null;
        }

        [UnityTearDown]
        public IEnumerator TearDown()
        {
            LogAssert.ignoreFailingMessages = false;
            Application.logMessageReceived -= handler;
            if (camGo != null) UnityEngine.Object.Destroy(camGo);
            if (tcGo != null) UnityEngine.Object.Destroy(tcGo);
            try { if (File.Exists(tempFile)) File.Delete(tempFile); } catch { }
            try { if (Directory.Exists(tempDir)) Directory.Delete(tempDir, true); } catch { }

            var rigType = Type.GetType("TW.Editor.CaptureRig, TW.Editor");
            if (rigType != null)
            {
                var field = rigType.GetField("rig", BindingFlags.Static | BindingFlags.NonPublic);
                if (field != null) field.SetValue(null, null);
            }
            yield return null;
        }

        [UnityTest]
        public IEnumerator ABadShotDoesNotKillTheSet()
        {
            LogAssert.ignoreFailingMessages = true;
            var rigType = Type.GetType("TW.Editor.CaptureRig, TW.Editor");
            if (rigType == null) { Assert.Fail("could not find TW.Editor.CaptureRig by reflection"); yield break; }

            var shotMethod = rigType.GetMethod("Shot", BindingFlags.Static | BindingFlags.Public);
            var pendingMethod = rigType.GetMethod("Pending", BindingFlags.Static | BindingFlags.Public);
            if (shotMethod == null || pendingMethod == null)
            {
                Assert.Fail("CaptureRig.Shot or CaptureRig.Pending not found by reflection");
                yield break;
            }

            string badPath = Path.Combine(tempFile, "shot.png");
            string goodPath = Path.Combine(tempDir, "shot.png");

            shotMethod.Invoke(null, new object[] { badPath, 0f, 0f, 20f, 0f, 25f, 64, 64, float.NaN });
            shotMethod.Invoke(null, new object[] { goodPath, 0f, 0f, 20f, 0f, 25f, 64, 64, float.NaN });

            var tc = tcGo.GetComponent<TacticalCamera>();

            float until = Time.realtimeSinceStartup + 20f;
            while ((string)pendingMethod.Invoke(null, null) != "0" && Time.realtimeSinceStartup < until)
                yield return null;
            // Pending() reports the queue, which empties a frame or two before the coroutine's own finally block
            // restores the camera: give that finally a moment to run before checking tc.enabled below.
            until = Time.realtimeSinceStartup + 5f;
            while (!tcGo.GetComponent<TacticalCamera>().enabled && Time.realtimeSinceStartup < until)
                yield return null;

            // [found-restore-camera-and-running-flag-in-captureri-20261017-1]
            bool loggedBadShot = false;
            foreach (var line in logs)
                if (line.Contains("[rig]") && line.Contains("shot.png")) { loggedBadShot = true; break; }
            Assert.IsTrue(loggedBadShot, "[found-restore-camera-and-running-flag-in-captureri-20261017-1] the bad shot's failure was not logged by name");

            Assert.AreEqual("0", (string)pendingMethod.Invoke(null, null),
                "[found-restore-camera-and-running-flag-in-captureri-20261017-1] the queue never drained after the bad shot");

            Assert.IsTrue(tc.enabled,
                "[found-restore-camera-and-running-flag-in-captureri-20261017-1] the TacticalCamera was not re-enabled after the queue drained");

            if (SystemInfo.graphicsDeviceType != UnityEngine.Rendering.GraphicsDeviceType.Null)
            {
                Assert.IsTrue(File.Exists(goodPath),
                    "[found-restore-camera-and-running-flag-in-captureri-20261017-1] the second, valid shot's PNG does not exist");
            }
        }
    }
}
#endif
