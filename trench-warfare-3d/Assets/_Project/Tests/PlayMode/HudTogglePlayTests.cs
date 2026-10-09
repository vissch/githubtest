// Phase: B6 (implemented) — [I1] F9 through the real hotkey path (HudHotkeys -> HudController.ApplyFlag) must
// always leave exactly one HUD shown: not the IMGUI BattleHud and the Toolkit HUD together, and never neither.
using System.Collections;
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.InputSystem;
using UnityEngine.InputSystem.LowLevel;
using UnityEngine.SceneManagement;
using UnityEngine.TestTools;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Sim;
using TW.UI;

namespace TW.Tests
{
    public class HudTogglePlayTests
    {
        GameObject camGo, hostGo;
        BattleHud legacy;
        HudController hud;
        Keyboard kb;
        bool savedToolkitHud;
        InputSettings.BackgroundBehavior savedBackground;
        InputSettings.EditorInputBehaviorInPlayMode savedEditorInput;
        Scene ownScene;
        Scene savedActive;
        readonly List<Camera> untagged = new List<Camera>();

        [UnitySetUp]
        public IEnumerator SetUp()
        {
            HudBootstrap.Disabled = true; ShellBoot.Disabled = true;
            savedToolkitHud = HudBridge.UseToolkitHud;
            HudBridge.UseToolkitHud = true;
            // [I1b] in batch mode there is no focused Game view, so the defaults keep queued key events out of the
            // player's keyboard state and KeyMap.DownRaw never sees F9. Both settings are restored in TearDown.
            savedBackground = InputSystem.settings.backgroundBehavior;
            savedEditorInput = InputSystem.settings.editorInputBehaviorInPlayMode;
            InputSystem.settings.backgroundBehavior = InputSettings.BackgroundBehavior.IgnoreFocus;
            InputSystem.settings.editorInputBehaviorInPlayMode = InputSettings.EditorInputBehaviorInPlayMode.AllDeviceInputAlwaysGoesToGameView;
            kb = InputSystem.AddDevice<Keyboard>();

            // [I1c] a scene of our own, so every GameObject below lands in it and no other class's scene holds them;
            // and no camera but ours stays tagged MainCamera, which is the only camera HudController looks for.
            savedActive = SceneManager.GetActiveScene();
            ownScene = SceneManager.CreateScene("hud-toggle-" + Time.frameCount);
            SceneManager.SetActiveScene(ownScene);
            foreach (var c in Camera.allCameras)
                if (c.CompareTag("MainCamera")) { c.tag = "Untagged"; untagged.Add(c); }

            camGo = new GameObject("main-camera"); camGo.tag = "MainCamera";
            var cam = camGo.AddComponent<Camera>();
            legacy = camGo.AddComponent<BattleHud>();

            hostGo = new GameObject("sim-host");
            var host = hostGo.AddComponent<SimHost>();
            legacy.Host = host;

            hud = HudBootstrap.Create(host);
            // [I1c] a stray MainCamera must fail here, not in the body: HudController finds `legacy` only through
            // Camera.main, so another scene's camera leaves this test's BattleHud on next to the Toolkit HUD.
            Assert.That(Camera.main, Is.EqualTo(cam), "[I1c] Camera.main is not this test's own camera" + Cameras());
            yield return null;
        }

        [UnityTearDown]
        public IEnumerator TearDown()
        {
            if (kb != null) InputSystem.RemoveDevice(kb);
            InputSystem.settings.backgroundBehavior = savedBackground;
            InputSystem.settings.editorInputBehaviorInPlayMode = savedEditorInput;
            if (hud != null) Object.Destroy(hud.gameObject);
            if (hostGo != null) Object.Destroy(hostGo);
            if (camGo != null) Object.Destroy(camGo);
            HudBridge.UseToolkitHud = savedToolkitHud;
            HudBootstrap.Disabled = false; ShellBoot.Disabled = false;
            // [I1c] give the other scenes their tags back, then drop ours. Ours was created, never loaded Single, so
            // it is never the last loaded scene and the unload cannot fail.
            foreach (var c in untagged) if (c != null) c.tag = "MainCamera";
            untagged.Clear();
            if (savedActive.IsValid() && savedActive.isLoaded) SceneManager.SetActiveScene(savedActive);
            if (ownScene.IsValid() && ownScene.isLoaded) yield return SceneManager.UnloadSceneAsync(ownScene);
            yield return null;
        }

        // [I1c] proof and guard in one: when another class leaves a scene loaded whose camera is tagged
        // MainCamera, Camera.main is that camera, HudController never finds this test's BattleHud and never switches
        // it off, so two HUDs are shown before any F9 press. The message names every tagged camera and its scene.
        static string Cameras()
        {
            var sb = new System.Text.StringBuilder(" | MainCamera-tagged cameras: ");
            bool any = false;
            foreach (var c in Camera.allCameras)
                if (c.CompareTag("MainCamera"))
                { sb.Append(any ? ", " : "").Append(c.name).Append(" (").Append(c.gameObject.scene.name).Append(")"); any = true; }
            if (!any) sb.Append("none");
            return sb.Append(" | Camera.main: ").Append(Camera.main == null ? "null"
                : Camera.main.name + " (" + Camera.main.gameObject.scene.name + ")").ToString();
        }

        static int ShownCount(UIDocument doc, BattleHud legacy)
        {
            int toolkit = doc.rootVisualElement.style.display.value == DisplayStyle.Flex ? 1 : 0;
            int imgui = legacy.enabled ? 1 : 0;
            return toolkit + imgui;
        }

        IEnumerator PressF9()
        {
            InputSystem.QueueStateEvent(kb, new KeyboardState(Key.F9));
            yield return null;
            InputSystem.QueueStateEvent(kb, new KeyboardState());
            yield return null;
        }

        [UnityTest]
        public IEnumerator F9ThroughTheHotkeyPathAlwaysLeavesExactlyOneHud()
        {
            var doc = hud.GetComponent<UIDocument>();
            yield return null;
            yield return null;   // the first Refresh caches `legacy` and switches it off (HudController.LegacyInterim)

            Assert.That(ShownCount(doc, legacy), Is.EqualTo(1), "[I1] before any F9 press there must already be exactly one HUD" + Cameras());

            yield return PressF9();
            Assert.That(ShownCount(doc, legacy), Is.EqualTo(1), "[I1] F9 left no HUD at all (or two) after the first press");
            Assert.That(legacy.enabled, Is.True, "[I1] F9 through the hotkey path must have switched back to the IMGUI HUD");

            yield return PressF9();
            Assert.That(ShownCount(doc, legacy), Is.EqualTo(1), "[I1] F9 left no HUD at all (or two) after the second press");
            Assert.That(doc.rootVisualElement.style.display.value, Is.EqualTo(DisplayStyle.Flex), "[I1] the second F9 must bring the Toolkit HUD back");
        }
    }
}
