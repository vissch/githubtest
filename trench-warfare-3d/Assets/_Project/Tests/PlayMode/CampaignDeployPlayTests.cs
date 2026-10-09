// Phase: B6 (test added 2026-10-07 for code review finding P1a) — the one path no test crossed: a scene load with the
// campaign screens up. MetaServices caches the strategic map view (a plain GameObject) behind IStrategicMapView, so a
// view the scene load destroyed still reads as "not null"; nothing forgot it per scene. Walks the shell's own router
// by clicking its real buttons, so a dead click fails loudly and no view is built by hand.
using System.Collections;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.SceneManagement;
using UnityEngine.TestTools;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.UI;

namespace TW.Tests
{
    public class CampaignDeployPlayTests
    {
        static void Click(VisualElement root, string name)
        {
            var b = root.Q<Button>(name);
            Assert.That(b, Is.Not.Null, "no button " + name + " on " + root.name);
            Assert.That(b.enabledInHierarchy, Is.True, name + " is disabled");
            using (var e = NavigationSubmitEvent.GetPooled()) { e.target = b; b.SendEvent(e); }
        }

        // [P1a] After DEPLOY the scene load destroys the strategic map's GameObject; MetaServices handed it out again
        // (the interface reference is not null) and StrategicMapScreen.OnUncovered -> Show threw
        // MissingReferenceException inside ShellRouter.OnSceneLoaded, before Bind(s): no Host, no pause menu, no
        // debrief, and the menu screens left drawn over the battle.
        [UnityTest]
        public IEnumerator TheFirstCampaignDeployBindsTheBattleAndLeavesNoMenuOverIt()
        {
            bool? persist = ProfileStore.PersistOverride;
            var profile = ProfileStore.Current;   // put the real one back: this test's fake outlived it, with saving on again
            ProfileStore.PersistOverride = false;
            ProfileStore.Use(new CampaignProfile { Faction = 0, Gold = 500 });
            CampaignSession.Clear();
            MatchLaunch.Current = null;
            try
            {
                yield return SceneManager.LoadSceneAsync(MatchLaunch.MenuScene, LoadSceneMode.Single);
            // [I1c] the menu load above tears the battle scene down, but left loaded its camera is tagged MainCamera
            // and wins Camera.main for whatever class runs next (HudTogglePlayTests saw two HUDs). Leave an empty
            // scene with no camera behind instead.
            var after = SceneManager.CreateScene("after-campaign-deploy");
            SceneManager.SetActiveScene(after);
            yield return SceneManager.UnloadSceneAsync(MatchLaunch.MenuScene);
                var router = ShellBoot.EnsureRoot();
                Assert.That(router, Is.Not.Null, "ShellAssets missing: run TW/UI/Build Shell Assets");
                yield return null;
                yield return null;
                Assert.That(router.Top, Is.InstanceOf<MainMenuScreen>(), "the menu scene opens on the main menu");

                Click(router.Top.Root, "btn-campaign");
                yield return null;
                Assert.That(router.Top, Is.InstanceOf<StrategicMapScreen>(), "CAMPAIGN opens the strategic map");
                var map = (StrategicMapScreen)router.Top;
                Assert.That(MetaServices.Map, Is.Not.Null, "the map screen made its 3D view");

                var list = map.Root.Q("node-list");
                Assert.That(list.childCount, Is.GreaterThan(0), "the front line has no nodes");
                var row = (Button)list[0];
                using (var e = NavigationSubmitEvent.GetPooled()) { e.target = row; row.SendEvent(e); }
                yield return null;
                Assert.That(map.Selected, Is.Not.Null, "the first row selected no node");

                Click(map.Root, "btn-select");
                yield return null;
                Assert.That(router.Top, Is.InstanceOf<StagingScreen>(), "SELECT MISSION opens staging");

                Click(router.Top.Root, "btn-deploy");
                for (int i = 0; i < 240 && SceneManager.GetActiveScene().name != MatchLaunch.BattleScene; i++) yield return null;
                Assert.That(SceneManager.GetActiveScene().name, Is.EqualTo(MatchLaunch.BattleScene), "DEPLOY did not load the battle scene");
                yield return null;
                yield return null;

                Assert.That(router.Host, Is.Not.Null, "the router did not bind the battle's SimHost: no pause menu, no debrief");
                Assert.That(router.Clock, Is.Not.Null, "the pause menu has no match clock to hold");
                Assert.That(router.Depth, Is.EqualTo(0), "a menu screen is still on the stack over the battle");
                var doc = router.GetComponent<UIDocument>();
                Assert.That(doc.rootVisualElement.childCount, Is.EqualTo(0), "a menu plate is still drawn over the battle");
                Assert.That(MetaServices.Map == null || !(MetaServices.Map is Object o && o == null), Is.True,
                    "MetaServices still hands out the map view the scene load destroyed");
                LogAssert.NoUnexpectedReceived();
            }
            finally
            {
                if (ShellRouter.Instance != null) Object.Destroy(ShellRouter.Instance.gameObject);
                MatchLaunch.Current = null;
                CampaignSession.Clear();
                SceneStatics.ResetSession();
                ProfileStore.Use(profile);
                ProfileStore.PersistOverride = persist;
            }
            yield return SceneManager.LoadSceneAsync(MatchLaunch.MenuScene, LoadSceneMode.Single);
        }
    }
}
