// Phase: B6 (test added 2026-10-08 for code review finding L3) — the mission select's map picture must be the card's
// own ground. In Play, because a list row is a Button and a Button only answers a click on a real panel.
using System.Collections;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.UI;

namespace TW.Tests
{
    public class MissionSelectPlayTests
    {
        bool savedBoot;

        [UnitySetUp]
        public IEnumerator SetUp()
        {
            savedBoot = ShellBoot.Disabled; ShellBoot.Disabled = true;
            HudBootstrap.Disabled = true;
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

        // [L3] RefreshThumb rendered the thumbnail with Render(seed), which defaults to ShelledForest: the WINTER LINE
        // card showed the shelled wood, not the frozen line the card loads (its Ground is WinterLine).
        [UnityTest]
        [Category("Long")]
        public IEnumerator TheMissionThumbnailShowsTheCardsOwnGround()
        {
            var router = ShellBoot.EnsureRoot();
            Assert.That(router, Is.Not.Null, "ShellAssets missing: run TW/UI/Build Shell Assets");
            yield return null;
            yield return null;
            Assert.That(router.Top, Is.InstanceOf<MainMenuScreen>(), "with no match the router shows the main menu");

            Click(router.Top.Root, "btn-skirmish");
            yield return null;
            Assert.That(router.Top, Is.InstanceOf<MissionSelectScreen>(), "SKIRMISH opens the mission select");

            Click(router.Top.Root, "row-winter-line");
            yield return null;

            var img = router.Top.Root.Q("info-image");
            Assert.That(img, Is.Not.Null, "no info-image");
            var shown = img.style.backgroundImage.value.texture;
            Assert.That(shown, Is.Not.Null, "[L3] the WINTER LINE card drew no thumbnail at all");

            var want = MapThumbnail.Render(1917u, Ground.WinterLine);
            Assert.That(want, Is.Not.Null, "the winter line's own thumbnail could not be generated");
            try
            {
                Assert.That(new Vector2Int(shown.width, shown.height), Is.EqualTo(new Vector2Int(want.width, want.height)),
                    "[L3] the WINTER LINE card shows a picture of another ground: " + shown.width + "x" + shown.height + " against " + want.width + "x" + want.height);
                var a = shown.GetPixels32(); var b = want.GetPixels32();
                int differ = 0;
                for (int i = 0; i < a.Length && i < b.Length; i++) if (!a[i].Equals(b[i])) differ++;
                Assert.That(differ, Is.EqualTo(0), "[L3] the WINTER LINE card shows the shelled wood, not the frozen line it loads: " + differ + " of " + a.Length + " pixels differ");
            }
            finally { Object.DestroyImmediate(want); }

            Click(router.Top.Root, "btn-back");
            yield return null;
        }
    }
}
