// Phase: review-fix (found-restore-the-hud-panel-target-in-a-finally-no-20261015-0, 2026-10-15)
// [found-restore-the-hud-panel-target-in-a-finally-no-20261015-0] HudCapture.Runner.Run saved panel.targetTexture,
// clearColor and colorClearValue and restored them mid-coroutine, with no try/finally between. Stopping the
// coroutine early (what Play exit or a destroyed runner does) skips the restore and the RenderTexture release,
// leaving the shared PanelSettings pointed at a dead texture for the rest of the editor session. These tests drive
// the iterator by hand and Dispose it where a stopped coroutine would be disposed, with no Play and no frames.
using System;
using System.Collections;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Editor;

namespace TW.Tests
{
    public class HudCaptureTests
    {
        PanelSettings panel;
        RenderTexture oldTexture;
        Camera cam;
        GameObject camGo, runnerGo;
        HudCapture.Runner runner;

        [SetUp]
        public void SetUp()
        {
            panel = ScriptableObject.CreateInstance<PanelSettings>();
            oldTexture = new RenderTexture(4, 4, 0) { name = "HudCaptureTests old" };
            panel.targetTexture = oldTexture;
            panel.clearColor = false;
            panel.colorClearValue = Color.red;

            camGo = new GameObject("HudCaptureTests cam");
            cam = camGo.AddComponent<Camera>();

            runnerGo = new GameObject("HudCaptureTests runner");
            runner = runnerGo.AddComponent<HudCapture.Runner>();
        }

        [TearDown]
        public void TearDown()
        {
            if (runnerGo != null) UnityEngine.Object.DestroyImmediate(runnerGo);
            if (camGo != null) UnityEngine.Object.DestroyImmediate(camGo);
            if (panel != null) UnityEngine.Object.DestroyImmediate(panel);
            if (oldTexture != null) UnityEngine.Object.DestroyImmediate(oldTexture);
        }

        [Test]
        public void Dispose_RestoresThePanel()
        {
            var it = runner.Run(panel, cam, System.IO.Path.Combine(System.IO.Path.GetTempPath(), "tw-hudcapture-test.png"), 64, 64);
            Assert.That(it.MoveNext(), Is.True, "[found-restore-the-hud-panel-target-in-a-finally-no-20261015-0] the coroutine did not reach its first yield");
            Assert.That(panel.targetTexture, Is.Not.SameAs(oldTexture),
                "[found-restore-the-hud-panel-target-in-a-finally-no-20261015-0] the panel was not pointed at a capture target");

            ((IDisposable)it).Dispose();

            Assert.That(panel.targetTexture, Is.SameAs(oldTexture),
                "[found-restore-the-hud-panel-target-in-a-finally-no-20261015-0] leaves panel.targetTexture at its old value was violated: targetTexture was not restored");
            Assert.That(panel.clearColor, Is.False,
                "[found-restore-the-hud-panel-target-in-a-finally-no-20261015-0] clearColor was not restored");
            Assert.That(panel.colorClearValue, Is.EqualTo(Color.red),
                "[found-restore-the-hud-panel-target-in-a-finally-no-20261015-0] colorClearValue was not restored");
        }

        [Test]
        public void Dispose_ReleasesTheCaptureTextures()
        {
            var it = runner.Run(panel, cam, System.IO.Path.Combine(System.IO.Path.GetTempPath(), "tw-hudcapture-test.png"), 64, 64);
            it.MoveNext();

            ((IDisposable)it).Dispose();

            foreach (var rt in Resources.FindObjectsOfTypeAll<RenderTexture>())
            {
                Assert.That(rt.name, Does.Not.StartWith("HudCapture "),
                    $"[found-restore-the-hud-panel-target-in-a-finally-no-20261015-0] releases both RenderTextures was violated: '{rt.name}' is still alive");
            }
        }
    }
}
