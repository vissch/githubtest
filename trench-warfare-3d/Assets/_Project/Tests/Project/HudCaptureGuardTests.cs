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

        [Test]
        public void ATranslucentHudPixelKeepsItsBrightness()   // [F10]
        {
            var over = typeof(HudCapture).GetMethod("Over", BindingFlags.Static | BindingFlags.NonPublic | BindingFlags.Public);
            Assert.IsNotNull(over, "[F10] HudCapture.Over is missing: translucent HUD pixels are still darkened on capture");

            // a 50% white plate over black must read half white, not half of half (the old double-multiply)
            var half = (Color32)over.Invoke(null, new object[] { new Color32(128, 128, 128, 128), new Color32(0, 0, 0, 255) });
            Assert.AreEqual(128, half.r, 1, "[F10] a translucent HUD pixel came out darker than it is: premultiplied alpha was multiplied twice");
            Assert.AreEqual(128, half.g, 1, "[F10] a translucent HUD pixel came out darker than it is: premultiplied alpha was multiplied twice");
            Assert.AreEqual(128, half.b, 1, "[F10] a translucent HUD pixel came out darker than it is: premultiplied alpha was multiplied twice");

            var opaque = (Color32)over.Invoke(null, new object[] { new Color32(200, 10, 10, 255), new Color32(255, 255, 255, 255) });
            Assert.AreEqual(200, opaque.r, 1, "[F10] an opaque HUD pixel should not blend with the scene behind it");
            Assert.AreEqual(10, opaque.g, 1, "[F10] an opaque HUD pixel should not blend with the scene behind it");
            Assert.AreEqual(10, opaque.b, 1, "[F10] an opaque HUD pixel should not blend with the scene behind it");

            var clamped = (Color32)over.Invoke(null, new object[] { new Color32(255, 255, 255, 128), new Color32(255, 255, 255, 255) });
            Assert.AreEqual(255, clamped.r, 0, "[F10] Over must clamp instead of overflowing a byte");
            Assert.AreEqual(255, clamped.g, 0, "[F10] Over must clamp instead of overflowing a byte");
            Assert.AreEqual(255, clamped.b, 0, "[F10] Over must clamp instead of overflowing a byte");
        }
    }
}
