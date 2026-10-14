// Phase: tooling (code review 2026-10-04, F9). HudCapture returned the PNG path before it wrote anything (an old
// file at that path read as the new capture) and put the shared HUD PanelSettings back only on the straight path, so
// Play ending or a second Shoot left the panel pointing at a destroyed RenderTexture and the HUD drew nowhere.
using System.IO;
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Editor;

namespace TW.Tests
{
    public class HudCaptureGuardTests
    {
        [Test]
        public void AnArmedCaptureClearsTheOldPng()   // [F9]
        {
            var arm = typeof(HudCapture).GetMethod("Arm", BindingFlags.Static | BindingFlags.NonPublic | BindingFlags.Public);
            Assert.IsNotNull(arm, "[F9] HudCapture.Arm is missing: an old PNG still reads as the new capture");
            string png = Path.Combine(Path.GetTempPath(), "f9-hud.png");
            File.WriteAllBytes(png, new byte[] { 1, 2, 3 });
            File.WriteAllText(png + ".json", "{}");

            string full = (string)arm.Invoke(null, new object[] { png });

            Assert.IsTrue(Path.IsPathRooted(full), "[F9] Arm answered a path that is not rooted: " + full);
            Assert.IsFalse(File.Exists(full), "[F9] the old PNG is still there, so the path Shoot answers reads as this capture");
            Assert.IsFalse(File.Exists(full + ".json"), "[F9] the old sidecar is still there");
        }

        [Test]
        public void ThePanelGoesBackWhenTheRunnerDies()   // [F9]
        {
            var runnerType = typeof(HudCapture).GetNestedType("Runner", BindingFlags.NonPublic | BindingFlags.Public);
            Assert.IsNotNull(runnerType, "[F9] HudCapture.Runner is not reachable");
            var begin = runnerType.GetMethod("Begin", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
            Assert.IsNotNull(begin, "[F9] HudCapture.Runner.Begin is missing: the panel is still taken over inside the coroutine");

            var panel = ScriptableObject.CreateInstance<PanelSettings>();
            var mine = new RenderTexture(8, 8, 0) { name = "F9 panel target" };
            var rtUi = new RenderTexture(8, 8, 24);
            var rtScene = new RenderTexture(8, 8, 24);
            panel.targetTexture = mine;
            var go = new GameObject("F9 runner");
            try
            {
                var runner = go.AddComponent(runnerType);
                begin.Invoke(runner, new object[] { panel, rtUi, rtScene });
                Assert.AreSame(rtUi, panel.targetTexture, "[F9] Begin did not point the panel at the capture target");
                Assert.IsTrue(HudCapture.Busy, "[F9] a running capture does not say it is busy, so a second Shoot would steal the panel");

                Object.DestroyImmediate(go);

                Assert.AreSame(mine, panel.targetTexture, "[F9] the HUD panel was left on the capture's destroyed target");
                Assert.IsFalse(HudCapture.Busy, "[F9] the capture that died still counts as running");
            }
            finally
            {
                if (go != null) Object.DestroyImmediate(go);
                Object.DestroyImmediate(mine);
                if (rtUi != null) Object.DestroyImmediate(rtUi);
                if (rtScene != null) Object.DestroyImmediate(rtScene);
                Object.DestroyImmediate(panel);
            }
        }
    }
}
