// Phase: B6 (test added 2026-10-08 for code review finding L7) — the settings screen's key capture, walked through a
// real panel, because a Button only answers a click when it is on one. L2 (a capture left running by BACK keeps
// MatchClock.Hold.Modal and freezes the match) is fixed in SettingsScreen.OnUnbind but has no test here: its walk
// starts with Esc, and a batch PlayMode run processes no input event any MonoBehaviour.Update can see (proven: a probe
// component at the router's own execution order, with the input system pumped by hand, counted zero presses). That
// test needs an editor with a window; it is written and waiting in the relay leg folder.
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
    }
}
