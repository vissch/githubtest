// Phase: B6 (implemented; test added by review 3, 2026-10-12) — VIEW THE FIELD on the debrief really shows the match.
// [U080.1] Review 3, P1 — "Opaque screen fill prevents the debrief View Field action".
// The tw-screen / tw-dim classes sit on the named child debrief-screen, while a screen's Root is the TemplateContainer
// around it, so DebriefScreen's toggle on Root reached nothing: the opaque #0E0F10 fill stayed over the match and
// VIEW THE FIELD showed a black screen. Needs a real panel, so PlayMode (the rig of ShellRouterPlayTests).
using System.Collections;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.UI;

namespace TW.Tests
{
    public class DebriefViewFieldPlayTests
    {
        [UnityTest]
        public IEnumerator ViewFieldShowsTheField()
        {
            bool boot = ShellBoot.Disabled; ShellBoot.Disabled = true;
            var assets = ShellAssets.Load();
            Assert.That(assets != null && assets.Panel != null && assets.Debrief != null, "ShellAssets missing: run TW/UI/Build Shell Assets");
            var go = new GameObject("debrief view field test");
            try
            {
                var doc = go.AddComponent<UIDocument>();
                doc.panelSettings = assets.Panel;
                var router = go.AddComponent<ShellRouter>();
                router.Assets = assets;
                yield return null;
                yield return null;
                router.ClearStack();
                var screen = new DebriefScreen(new MatchReport { Winner = 0 });
                router.Push(screen);
                yield return null;

                var fill = screen.Root.Q("debrief-screen");
                Assert.That(fill, Is.Not.Null, "the test's rig: Debrief.uxml has no debrief-screen");
                Assert.That(fill.resolvedStyle.backgroundColor.a, Is.GreaterThan(0.9f),
                    "the test's rig: the screen's styles did not resolve");

                var btn = screen.Root.Q<Button>("btn-view-field");
                Assert.That(btn, Is.Not.Null, "the test's rig: Debrief.uxml has no btn-view-field");
                // A NavigationSubmitEvent through the panel does not reach the handler here (ShellScreen.Bind clears the
                // buttons' focus and the router's container ignores picking), so call the button's own entry point.
                // What catches the bug is the paint below, not the click's route.
                screen.ViewField();
                yield return null;

                Assert.That(fill.resolvedStyle.backgroundColor.a, Is.EqualTo(0f),
                    "[U080.1] the debrief still paints an opaque fill over the match while viewing the field");
                var bg = fill.resolvedStyle.backgroundImage;
                Assert.That(bg.texture == null && bg.sprite == null && bg.renderTexture == null && bg.vectorImage == null,
                    "[U080.1] the dim vignette still paints over the match while viewing the field");
                Assert.That(screen.Root.Q("debrief-plate").resolvedStyle.display, Is.EqualTo(DisplayStyle.None),
                    "[U080.1] the debrief plate is still shown while viewing the field");
                Assert.That(screen.Root.Q("debrief-logo").resolvedStyle.display, Is.EqualTo(DisplayStyle.None),
                    "[U080.1] the debrief logo is still shown while viewing the field");
                Assert.That(screen.Root.Q("view-hint").resolvedStyle.display, Is.EqualTo(DisplayStyle.Flex),
                    "[U080.1] no hint says how to get back to the report");

                screen.OnEscape();
                yield return null;
                Assert.That(screen.Root.Q("debrief-plate").resolvedStyle.display, Is.EqualTo(DisplayStyle.Flex),
                    "[U080.1] Esc did not bring the report back");
                Assert.That(screen.Root.ClassListContains("tw-dim"), Is.False,
                    "[U080.1] the container gained tw-dim: the toggle is on the wrong element");
            }
            finally
            {
                Object.Destroy(go);
                ShellBoot.Disabled = boot;
            }
            yield return null;
        }
    }
}
