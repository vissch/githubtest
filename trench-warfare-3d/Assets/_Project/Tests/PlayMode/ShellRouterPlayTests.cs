// Phase: B6 (implemented; test added in the perf pass, 2026-09-23) — a screen the shell pops leaves the panel.
// ShellRouter.Pop called Unbind (which clears the screen's Root) before Root?.RemoveFromHierarchy(), so no popped screen
// was ever taken off the panel. Pressing Play in the battle scene never pushes the main menu, so the editor never
// showed it; a player build boots to the menu, and the menu stayed drawn over the whole match after Skirmish (found by
// the first player screenshot of the performance benchmark). The same fault left a resumed pause menu on screen.
using System.Collections;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using UnityEngine.UIElements;
using TW.UI;
using TW.Presentation;

namespace TW.Tests
{
    public class ShellRouterPlayTests
    {
        [UnityTest]
        public IEnumerator APoppedScreenLeavesThePanel()
        {
            bool boot = ShellBoot.Disabled; ShellBoot.Disabled = true;
            var assets = ShellAssets.Load();
            Assert.That(assets != null && assets.Panel != null && assets.MainMenu != null, "ShellAssets missing: run TW/UI/Build Shell Assets");
            var go = new GameObject("shell router test");
            try
            {
                var doc = go.AddComponent<UIDocument>();
                doc.panelSettings = assets.Panel;
                var router = go.AddComponent<ShellRouter>();
                router.Assets = assets;
                yield return null;   // Start binds the test scene: no SimHost in it, so the router pushes the main menu
                yield return null;
                Assert.That(router.Top, Is.Not.Null, "with no match the router shows the main menu");
                int drawn = doc.rootVisualElement.childCount;
                router.ClearStack();
                Assert.That(router.Top, Is.Null);
                Assert.That(doc.rootVisualElement.childCount, Is.EqualTo(drawn - 1),
                    "the popped main menu is still on the panel: it would stay drawn over the match");
            }
            finally
            {
                Object.Destroy(go);
                ShellBoot.Disabled = boot;
            }
            yield return null;
        }

        /// <summary>A screen that counts what the router does to it.</summary>
        sealed class CountingScreen : ShellScreen
        {
            readonly VisualTreeAsset tree;
            public int Uncovered, Unbound;
            public CountingScreen(VisualTreeAsset tree) { this.tree = tree; }
            public override VisualTreeAsset Tree(ShellAssets a) => tree;
            protected override void OnBind() { }
            protected override void OnUnbind() { Unbound++; }
            public override void OnUncovered() { Uncovered++; }
        }

        // [L1] ClearStack popped one by one, so each Pop uncovered the screen below — in the NEW scene, since
        // OnSceneLoaded clears the stack: the map screen rebuilt its 3D view inside the battle scene and reclaimed
        // HudBridge.PointerOverUi, which the next screen's OnUnbind then nulled (the HUD's click mask lost).
        [UnityTest]
        public IEnumerator ClearingTheStackUnbindsWithoutUncovering()
        {
            bool boot = ShellBoot.Disabled; ShellBoot.Disabled = true;
            var assets = ShellAssets.Load();
            Assert.That(assets != null && assets.Panel != null && assets.MainMenu != null, "ShellAssets missing: run TW/UI/Build Shell Assets");
            // a router left by an earlier test keeps this one's Awake from binding (Instance wins and destroys it):
            // Destroy is deferred to the end of the frame, so take the frame
            if (ShellRouter.Instance != null) { Object.Destroy(ShellRouter.Instance.gameObject); yield return null; }
            var go = new GameObject("shell router cleardown test");
            var mask = HudBridge.PointerOverUi;
            try
            {
                var doc = go.AddComponent<UIDocument>();
                doc.panelSettings = assets.Panel;
                var router = go.AddComponent<ShellRouter>();
                router.Assets = assets;
                yield return null;   // Start binds the test scene
                router.ClearStack();  // drop whatever the bind pushed: this test owns the stack

                var lower = new CountingScreen(assets.MainMenu);
                var upper = new CountingScreen(assets.MainMenu);
                router.Push(lower);
                router.Push(upper);
                Assert.That(router.Depth, Is.EqualTo(2));

                System.Func<Vector2, bool> hudMask = _ => true;   // the HUD's click mask, as HudBootstrap leaves it
                HudBridge.PointerOverUi = hudMask;
                lower.Uncovered = 0;

                router.ClearStack();

                Assert.That(router.Depth, Is.EqualTo(0));
                Assert.That(upper.Unbound, Is.EqualTo(1), "the top screen is unbound");
                Assert.That(lower.Unbound, Is.EqualTo(1), "and so is the one below it");
                Assert.That(lower.Uncovered, Is.EqualTo(0),
                    "ClearStack uncovered the screen below: in a scene load it would rebuild that screen's 3D view in the new scene");
                Assert.That(HudBridge.PointerOverUi, Is.SameAs(hudMask),
                    "the HUD's click mask did not survive the teardown");
                Assert.That(doc.rootVisualElement.childCount, Is.EqualTo(0), "nothing is left on the panel");
            }
            finally
            {
                Object.Destroy(go);
                HudBridge.PointerOverUi = mask;
                ShellBoot.Disabled = boot;
            }
            yield return null;
        }
    }
}
