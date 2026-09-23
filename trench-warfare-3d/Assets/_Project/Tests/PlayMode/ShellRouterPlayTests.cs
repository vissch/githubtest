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
    }
}
