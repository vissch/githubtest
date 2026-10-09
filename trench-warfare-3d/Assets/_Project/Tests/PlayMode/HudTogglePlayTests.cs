// Phase: B6 (implemented) — [I1] F9 through the real hotkey path (HudHotkeys -> HudController.ApplyFlag) must
// always leave exactly one HUD shown: not the IMGUI BattleHud and the Toolkit HUD together, and never neither.
using System.Collections;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.InputSystem;
using UnityEngine.InputSystem.LowLevel;
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

            camGo = new GameObject("main-camera"); camGo.tag = "MainCamera";
            camGo.AddComponent<Camera>();
            legacy = camGo.AddComponent<BattleHud>();

            hostGo = new GameObject("sim-host");
            var host = hostGo.AddComponent<SimHost>();
            legacy.Host = host;

            hud = HudBootstrap.Create(host);
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
            yield return null;
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

            Assert.That(ShownCount(doc, legacy), Is.EqualTo(1), "[I1] before any F9 press there must already be exactly one HUD");

            yield return PressF9();
            Assert.That(ShownCount(doc, legacy), Is.EqualTo(1), "[I1] F9 left no HUD at all (or two) after the first press");
            Assert.That(legacy.enabled, Is.True, "[I1] F9 through the hotkey path must have switched back to the IMGUI HUD");

            yield return PressF9();
            Assert.That(ShownCount(doc, legacy), Is.EqualTo(1), "[I1] F9 left no HUD at all (or two) after the second press");
            Assert.That(doc.rootVisualElement.style.display.value, Is.EqualTo(DisplayStyle.Flex), "[I1] the second F9 must bring the Toolkit HUD back");
        }
    }
}
