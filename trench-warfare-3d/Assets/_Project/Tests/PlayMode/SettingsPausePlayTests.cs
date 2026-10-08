// Phase: B6 (tests added 2026-10-08 for code review findings L7, L2, L4) — the settings screen walked through a real
// panel, because a Button only answers a click when it is on one. L2's walk starts at Esc, and a batch PlayMode run
// processes no key event a MonoBehaviour.Update can see, so this test pushes the pause menu the way the Esc branch
// itself does (ShellRouter.Update -> Push(new PauseMenuScreen())) and walks the rest with real clicks.
using System.Collections;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.UI;

namespace TW.Tests
{
    public class SettingsPausePlayTests
    {
        bool savedBoot;
        GameObject host;
        PanelSettings savedPanel;
        Vector2Int savedReference;   // what the panel asset held before this test, put back in TearDown
        Vector2Int liveReference;    // what the player's own UI scale asks for: what BACK must leave behind

        [UnitySetUp]
        public IEnumerator SetUp()
        {
            savedBoot = ShellBoot.Disabled; ShellBoot.Disabled = true;
            HudBootstrap.Disabled = true;
            // a router left by an earlier test keeps this one from binding (Instance wins and destroys the new one);
            // Destroy is deferred to the end of the frame, so take the frame
            if (ShellRouter.Instance != null) { Object.Destroy(ShellRouter.Instance.gameObject); yield return null; }
            yield return null;
        }

        [UnityTearDown]
        public IEnumerator TearDown()
        {
            if (ShellRouter.Instance != null) Object.Destroy(ShellRouter.Instance.gameObject);
            if (savedPanel != null) { savedPanel.referenceResolution = savedReference; savedPanel = null; }
            if (host != null) { Object.Destroy(host); host = null; }
            MatchLaunch.Current = null;
            InputFocus.Reset();
            HudBootstrap.Disabled = false;
            ShellBoot.Disabled = savedBoot;
            yield return null;
        }

        static void Click(VisualElement root, string name)
        {
            var b = root.Q<Button>(name);
            Assert.That(b, Is.Not.Null, "no button " + name);
            Assert.That(b.enabledInHierarchy, Is.True, name + " is disabled");
            using (var e = NavigationSubmitEvent.GetPooled()) { e.target = b; b.SendEvent(e); }
        }

        static void Click(Button b)
        {
            using (var e = NavigationSubmitEvent.GetPooled()) { e.target = b; b.SendEvent(e); }
        }

        static System.Collections.Generic.List<Button> Caps(ShellScreen screen)
        {
            var caps = new System.Collections.Generic.List<Button>();
            screen.Root.Q("controls-list").Query<Button>().ForEach(b => { if (b.ClassListContains("tw-keycap")) caps.Add(b); });
            return caps;
        }

        // [L7] Listen() refreshed the cap it was about to take over (RefreshCap with the NEW cap's action) instead of
        // the one that was listening, so the first cap stayed on "PRESS A KEY" for good: two caps both looked armed
        // and the player could not tell which key the next press would take. In Play, because a button only answers a
        // click through a real panel.
        [UnityTest]
        public IEnumerator ASecondKeyCapRefreshesTheOneThatWasListening()
        {
            var router = ShellBoot.EnsureRoot();
            Assert.That(router, Is.Not.Null, "ShellAssets missing: run TW/UI/Build Shell Assets");
            yield return null;
            yield return null;
            Assert.That(router.Top, Is.InstanceOf<MainMenuScreen>(), "with no match the router shows the main menu");

            Click(router.Top.Root, "btn-settings");
            yield return null;
            Assert.That(router.Top, Is.InstanceOf<SettingsScreen>(), "SETTINGS opens the settings screen");
            var caps = Caps(router.Top);
            Assert.That(caps.Count, Is.GreaterThan(1), "the controls page needs two key-caps for this test");
            var first = caps[0]; var second = caps[1];
            string wasFirst = first.text;

            Click(first);
            yield return null;
            Assert.That(first.text, Is.EqualTo("PRESS A KEY"), "clicking the first key-cap did not start a capture");
            Click(second);
            yield return null;

            Assert.That(second.text, Is.EqualTo("PRESS A KEY"), "[L7] the second key-cap did not take the capture");
            Assert.That(first.text, Is.EqualTo(wasFirst), "[L7] the cap that was listening still reads PRESS A KEY: the second click refreshed the wrong cap");
            Assert.That(first.ClassListContains("tw-keycap--listening"), Is.False, "[L7] the cap that was listening is still shown as armed");

            Click(router.Top.Root, "btn-back");
            yield return null;
        }

        // [L2] A key capture takes MatchClock.Hold.Modal; before the fix only the key and cancel callbacks released it,
        // so BACK with a cap still saying PRESS A KEY closed the screen with the hold on. RESUME drops only Hold.Menu,
        // so the match stayed frozen for the rest of the game. The walk the finding gives, minus the Esc key press
        // (batch delivers none): the pause menu is pushed exactly as the router's Esc branch pushes it.
        [UnityTest]
        public IEnumerator AKeyCaptureLeftRunningMustNotFreezeTheMatch()
        {
            MatchLaunch.Current = new MatchLaunch.Request { GeneratedBattlefield = false, PlaytestMap = false };
            host = new GameObject("l2-host"); host.SetActive(false);
            var sim = host.AddComponent<SimHost>();
            host.SetActive(true);
            yield return null;

            var router = ShellBoot.EnsureRoot();
            Assert.That(router, Is.Not.Null, "ShellAssets missing: run TW/UI/Build Shell Assets");
            yield return null;
            Assert.That(router.Clock, Is.Not.Null, "[L2] the router found no match clock, so this test proves nothing");

            router.Push(new PauseMenuScreen());
            yield return null;
            Click(router.Top.Root, "btn-settings");
            yield return null;
            Assert.That(router.Top, Is.InstanceOf<SettingsScreen>(), "SETTINGS opens the settings screen");

            var caps = Caps(router.Top);
            Assert.That(caps.Count, Is.GreaterThan(0), "the controls page needs a key-cap for this test");
            Click(caps[0]);
            yield return null;
            Assert.That(caps[0].text, Is.EqualTo("PRESS A KEY"), "clicking the key-cap did not start a capture");

            Click(router.Top.Root, "btn-back");
            yield return null;
            Click(router.Top.Root, "btn-resume");
            yield return null;

            Assert.That(router.Clock.Has(MatchClock.Hold.Modal), Is.False,
                "[L2] the match is still held by the key capture after BACK and RESUME");
            uint before = sim.Local.World.Tick;
            for (int i = 0; i < 30; i++) yield return null;
            Assert.That(router.Clock.Holds, Is.EqualTo(MatchClock.Hold.None),
                "[L2] the match is still held after RESUME: " + router.Clock.Holds);
            Assert.That(sim.Local.World.Tick, Is.GreaterThan(before), "[L2] the sim stopped counting ticks after the key capture");
        }

        // [L4] Refill() (DEFAULTS) rebuilt the interface page from the draft but never put the draft's UI scale into
        // the engine: SetValueWithoutNotify raises no callback. So DEFAULTS left the whole UI at the dragged scale
        // while the page read 1.0, and OnUnbind's revert compared the draft with itself and skipped.
        [UnityTest]
        public IEnumerator DefaultsThenBackMustPutTheUiScaleBack()
        {
            var panel = Resources.Load<PanelSettings>(HudBootstrap.PanelResource);
            Assert.That(panel, Is.Not.Null, "missing " + HudBootstrap.PanelResource + ": run TW/UI/Build All UI Assets");
            savedReference = panel.referenceResolution; savedPanel = panel;
            // the baseline is the live settings' own scale, recorded from the engine — not a formula and not a
            // number read back from the screen under test
            SettingsApplier.ApplyInterface(SettingsStore.Current);
            liveReference = panel.referenceResolution;

            var router = ShellBoot.EnsureRoot();
            Assert.That(router, Is.Not.Null, "ShellAssets missing: run TW/UI/Build Shell Assets");
            yield return null;
            Click(router.Top.Root, "btn-settings");
            yield return null;
            Assert.That(router.Top, Is.InstanceOf<SettingsScreen>(), "SETTINGS opens the settings screen");

            var slider = router.Top.Root.Q<Slider>("slider-ui-scale");
            Assert.That(slider, Is.Not.Null, "no slider-ui-scale");
            float drag = Mathf.Approximately(slider.value, slider.highValue) ? slider.lowValue : slider.highValue;
            slider.value = drag;
            yield return null;
            Assert.That(panel.referenceResolution, Is.Not.EqualTo(liveReference),
                "dragging the UI scale slider changed nothing, so this test would pass on a broken DEFAULTS");

            Click(router.Top.Root, "btn-defaults");
            yield return null;
            Click(router.Top.Root, "btn-back");
            yield return null;

            Assert.That(panel.referenceResolution, Is.EqualTo(liveReference),
                "[L4] the UI stayed at the previewed scale after DEFAULTS and BACK");
        }
    }
}
