// Phase: B6 (implemented) — every shell screen's UXML has the parts its controller queries, and binds with no panel.
// Also the mission catalog: at least one card, the first card is today's map (seed 1917, today's enemy knobs), and
// every difficulty has a deploy interval the sim can divide by.
using NUnit.Framework;
using UnityEditor;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.UI;

namespace TW.Tests
{
    public class ShellUxmlTests
    {
        const string Folder = "Assets/_Project/UI/Shell/";

        static VisualElement Instantiate(string name, string folder = Folder)
        {
            var tree = AssetDatabase.LoadAssetAtPath<VisualTreeAsset>(folder + name + ".uxml");
            Assert.That(tree, Is.Not.Null, $"{folder}{name}.uxml did not load");
            return tree.Instantiate();
        }

        static void Check(string uxml, string[] names, ShellScreen screen, string folder = Folder)
        {
            var root = Instantiate(uxml, folder);
            foreach (var n in names) Assert.That(root.Q(n), Is.Not.Null, $"{uxml}.uxml has no element named '{n}'");
            Assert.DoesNotThrow(() => { screen.Bind(root, null); screen.Unbind(); }, $"{screen.GetType().Name} must bind without a router or a panel");
            root.Query<Button>().ForEach(b => Assert.That(b.ClassListContains("tw-btn") || b.ClassListContains("tw-tab") || b.ClassListContains("tw-keycap") || b.ClassListContains("tw-list-row"), $"{uxml}: button '{b.name}' has no skin class"));
        }

        [Test] public void PauseMenu() => Check("PauseMenu", PauseMenuScreen.RequiredNames, new PauseMenuScreen());
        [Test] public void Settings() => Check("Settings", SettingsScreen.RequiredNames, new SettingsScreen());
        [Test] public void Debrief() => Check("Debrief", DebriefScreen.RequiredNames, new DebriefScreen(new MatchReport { Winner = 0 }));
        [Test] public void MainMenu() => Check("MainMenu", MainMenuScreen.RequiredNames, new MainMenuScreen());
        [Test] public void MissionSelect() => Check("MissionSelect", MissionSelectScreen.RequiredNames, new MissionSelectScreen());
        [Test] public void Armoury() => Check("Armoury", ArmouryScreen.RequiredNames, new ArmouryScreen(), "Assets/_Project/UI/Resources/Shell/");

        // the campaign screens (docs/21 phase 6) take a profile of their own so nothing reaches the player's profile.json
        [Test] public void HomeFront() => Check("HomeFront", HomeFrontScreen.RequiredNames, new HomeFrontScreen(new CampaignProfile()), "Assets/_Project/UI/Resources/Shell/");
        [Test] public void StrategicMap() => Check("StrategicMap", StrategicMapScreen.RequiredNames, new StrategicMapScreen(new CampaignProfile()), "Assets/_Project/UI/Resources/Shell/");
        [Test] public void Staging() => Check("Staging", StagingScreen.RequiredNames, new StagingScreen(new CampaignProfile(), "lowlands", 0), "Assets/_Project/UI/Resources/Shell/");

        [Test]
        public void TheHomeFrontSellsWhatTheTableSays()
        {
            var profile = new CampaignProfile { Gold = 100 };
            CampaignSession.Clear();   // no building left selected by an earlier test
            var root = Instantiate("HomeFront", "Assets/_Project/UI/Resources/Shell/");
            var screen = new HomeFrontScreen(profile);
            screen.Bind(root, null);
            try
            {
                Assert.That(screen.Selected, Is.EqualTo(FactionBuildings.IronBuildings[0].Id));
                Assert.That(root.Q<Label>("gold-value").text, Is.EqualTo("100"));
                Assert.That(root.Q<Button>("btn-expand").enabledSelf, Is.True);
                Assert.That(root.Q<Button>("btn-buy-0").enabledSelf, Is.False, "nothing sells at the ground floor");
                screen.Expand();
                Assert.That(profile.StageOf(0, screen.Selected), Is.EqualTo(1));
                Assert.That(root.Q<Label>("gold-value").text, Is.EqualTo("60"));
                Assert.That(root.Q<Button>("btn-buy-0").enabledSelf, Is.True);
                screen.SetFaction(1);
                Assert.That(screen.Selected, Is.EqualTo(FactionBuildings.BrassBuildings[0].Id));
                Assert.That(profile.Faction, Is.EqualTo(1));
            }
            finally { screen.Unbind(); }
        }

        [Test]
        public void TheMapOpensOnTheLowlandsAndStagingBuildsTheRequest()
        {
            var profile = new CampaignProfile();
            CampaignSession.Clear();   // no node left selected by an earlier test
            var root = Instantiate("StrategicMap", "Assets/_Project/UI/Resources/Shell/");
            var map = new StrategicMapScreen(profile);
            map.Bind(root, null);
            try
            {
                Assert.That(map.Selected, Is.EqualTo("lowlands"));
                Assert.That(map.Mission, Is.Zero);
                Assert.That(root.Q<Button>("btn-select").enabledSelf, Is.True);
                map.Select("the-citadel");
                Assert.That(root.Q<Button>("btn-select").enabledSelf, Is.False, "a locked country cannot be fought");
                Assert.That(root.Q<Label>("info-progress").text, Does.Contain("NEEDS"));
            }
            finally { map.Unbind(); }

            profile.SetStage(0, "supply-depot", 1);
            var depot = FactionBuildings.Find(0, "supply-depot");
            profile.SetTier(0, depot.Id, 0, 1);   // WAR CHEST tier 1
            // an unlock beyond the base two, so 'no picks' can be told from 'every unlock' (critic r5: it fielded every unlock)
            var signals = FactionBuildings.Find(0, "signals-station");
            int unlockLine = System.Array.FindIndex(signals.Lines, l => l.Kind == LineKind.AbilityUnlock);
            profile.SetStage(0, "signals-station", 1);
            profile.SetTier(0, signals.Id, unlockLine, 1);
            Assert.That(FactionBuildings.AbilityMask(profile, 0), Is.Not.EqualTo(FactionBuildings.BaseAbilityMask), "the profile has an unlock past the base two");
            var staging = new StagingScreen(profile, "lowlands", 0);
            staging.Bind(Instantiate("Staging", "Assets/_Project/UI/Resources/Shell/"), null);
            try
            {
                Assert.That(staging.BuildRequest().AbilityMaskA, Is.EqualTo(FactionBuildings.BaseAbilityMask), "no picks deploys the base two, not every unlock");
                staging.SetDifficulty(2);
                staging.Toggle(TW.Sim.Match.OffMapAbilityId.HeBarrage);
                var r = staging.BuildRequest();
                Assert.That(r.MissionId, Is.EqualTo("lowlands/0"));
                Assert.That(r.Difficulty, Is.EqualTo("HARD"));
                Assert.That(r.StartingSilver, Is.EqualTo(300 + FactionBuildings.SilverPerTier));
                Assert.That(r.FactionA, Is.Zero); Assert.That(r.FactionB, Is.EqualTo(1));
                Assert.That(r.AbilityMaskA, Is.EqualTo(1u << (int)TW.Sim.Match.OffMapAbilityId.HeBarrage));
                Assert.That(staging.Picks, Is.EqualTo(r.AbilityMaskA));
            }
            finally { staging.Unbind(); }
        }

        [Test]
        public void A_Swap_Between_The_Map_And_The_Home_Front_Keeps_What_Was_Selected()
        {
            // Main -> Home Front -> map -> (pop) Home Front -> TO THE FRONT pushes a fresh map: it opens on the node the
            // player left, and a fresh Home Front on the building (critique round 2, left open until round 6)
            const string Shell = "Assets/_Project/UI/Resources/Shell/";
            var profile = new CampaignProfile();
            CampaignSession.Clear();
            try
            {
                var map = new StrategicMapScreen(profile); map.Bind(Instantiate("StrategicMap", Shell), null);
                map.Select("river-line"); map.Unbind();
                var again = new StrategicMapScreen(profile); again.Bind(Instantiate("StrategicMap", Shell), null);
                Assert.That(again.Selected, Is.EqualTo("river-line"), "a fresh map opens where the last one was left");
                again.Unbind();

                string third = FactionBuildings.IronBuildings[2].Id;
                var home = new HomeFrontScreen(profile); home.Bind(Instantiate("HomeFront", Shell), null);
                home.Select(third); home.Unbind();
                var home2 = new HomeFrontScreen(profile); home2.Bind(Instantiate("HomeFront", Shell), null);
                Assert.That(home2.Selected, Is.EqualTo(third), "a fresh Home Front opens on the building left");
                home2.Unbind();

                CampaignSession.Clear();   // leaving a match: the map opens on the campaign's opening again
                var fresh = new StrategicMapScreen(profile); fresh.Bind(Instantiate("StrategicMap", Shell), null);
                Assert.That(fresh.Selected, Is.EqualTo("lowlands"));
                fresh.Unbind();
            }
            finally { CampaignSession.Clear(); }
        }

        /// <summary>The map keeps the mission the player chose when its node is selected again (back from staging, a rebuild
        /// of the list): it reset to the node's next mission every time (critic r5).</summary>
        /// <summary>The enemy is the other side, whichever the player took (every node named Brass: Brass fought Brass).</summary>
        [Test]
        public void PlayingBrassFightsIron()
        {
            var profile = new CampaignProfile { Faction = 1 };
            var staging = new StagingScreen(profile, "lowlands", 0);
            staging.Bind(Instantiate("Staging", "Assets/_Project/UI/Resources/Shell/"), null);
            try
            {
                var r = staging.BuildRequest();
                Assert.That(r.FactionA, Is.EqualTo(1)); Assert.That(r.FactionB, Is.EqualTo(0), "Brass fights Iron");
            }
            finally { staging.Unbind(); }
        }

        [Test]
        public void TheMapKeepsTheChosenMissionWhenItsNodeIsSelectedAgain()
        {
            var profile = new CampaignProfile();
            profile.Complete("lowlands", 0); profile.Complete("lowlands", 1);
            var map = new StrategicMapScreen(profile);
            map.Bind(Instantiate("StrategicMap", "Assets/_Project/UI/Resources/Shell/"), null);
            try
            {
                map.Select("lowlands");
                Assert.That(map.Mission, Is.EqualTo(2), "a node opens on its next mission");
                map.SelectMission(0);   // a replay of the first
                map.Select("lowlands");
                Assert.That(map.Mission, Is.EqualTo(0), "the choice survives the node being selected again");
                map.OnUncovered();
                Assert.That(map.Mission, Is.EqualTo(0), "and coming back to the map");
            }
            finally { map.Unbind(); }
        }

        [Test]
        public void ADebriefPaysCampaignGoldOnce()
        {
            var profile = new CampaignProfile { Gold = 10 };
            bool? persist = ProfileStore.PersistOverride;
            ProfileStore.PersistOverride = false; ProfileStore.Use(profile);
            try
            {
                CampaignSession.Begin("lowlands", 0, 0);
                var root = Instantiate("Debrief");
                var screen = new DebriefScreen(new MatchReport { Winner = 0 });
                screen.Bind(root, null);
                Assert.That(profile.Gold, Is.EqualTo(10 + CampaignGraph.RewardFirst));
                Assert.That(profile.IsComplete("lowlands", 0), Is.True);
                Assert.That(profile.LastNode, Is.EqualTo("lowlands"));
                Assert.That(root.Q<Label>("gold-earned").text, Does.Contain("+" + CampaignGraph.RewardFirst));
                Assert.That(root.Q<Button>("btn-continue").text, Is.EqualTo(DebriefScreen.ToTheMap));
                Assert.That(root.Q("gold-row").ClassListContains("tw-hidden"), Is.False);
                screen.Unbind();

                var rebound = Instantiate("Debrief"); screen.Bind(rebound, null);   // the same debrief bound again (VIEW FIELD and back)
                Assert.That(rebound.Q<Label>("gold-earned").text, Does.Contain("+" + CampaignGraph.RewardFirst), "a re-bind shows what that win paid");
                screen.Unbind();
                var againRoot = Instantiate("Debrief");
                var again = new DebriefScreen(new MatchReport { Winner = 0 }); again.Bind(againRoot, null);   // RESTART, won again
                Assert.That(profile.Gold, Is.EqualTo(10 + CampaignGraph.RewardFirst), "a re-bind (RESTART, VIEW FIELD) pays nothing more");
                Assert.That(againRoot.Q<Label>("gold-earned").text, Is.EqualTo(DebriefScreen.NoGoldAgain), "the restarted match's win says so (it showed GOLD +25, paying nothing)");
                again.Unbind();

                CampaignSession.Begin("lowlands", 0, 0);
                var replay = new DebriefScreen(new MatchReport { Winner = 0 }); var replayRoot = Instantiate("Debrief"); replay.Bind(replayRoot, null);
                Assert.That(profile.Gold, Is.EqualTo(10 + CampaignGraph.RewardFirst), "a mission already won pays nothing");
                Assert.That(replayRoot.Q<Label>("gold-earned").text, Is.EqualTo(DebriefScreen.NoGoldAgain));
                replay.Unbind();

                CampaignSession.Begin("lowlands", 1, 0);
                var lost = new DebriefScreen(new MatchReport { Winner = 1 }); var lostRoot = Instantiate("Debrief"); lost.Bind(lostRoot, null);
                Assert.That(profile.IsComplete("lowlands", 1), Is.False, "a defeat records nothing");
                Assert.That(lostRoot.Q<Label>("gold-earned").text, Is.EqualTo(DebriefScreen.NoGoldForADefeat));
                lost.Unbind();
                var drawn = new DebriefScreen(new MatchReport { Winner = -1 }); var drawnRoot = Instantiate("Debrief"); drawn.Bind(drawnRoot, null);
                Assert.That(drawnRoot.Q<Label>("gold-earned").text, Is.EqualTo(DebriefScreen.NoGoldForADraw), "a draw is not called a defeat");
                drawn.Unbind();

                CampaignSession.Clear();
                var skirmish = new DebriefScreen(new MatchReport { Winner = 0 }); var skRoot = Instantiate("Debrief"); skirmish.Bind(skRoot, null);
                Assert.That(skRoot.Q("gold-row").ClassListContains("tw-hidden"), Is.True, "a skirmish shows no gold row");
                Assert.That(skRoot.Q<Button>("btn-continue").text, Is.EqualTo("CONTINUE"));
                skirmish.Unbind();
            }
            finally { CampaignSession.Clear(); CampaignSession.ResumeMap = false; ProfileStore.Use(null); ProfileStore.PersistOverride = persist; }
        }

        [Test]
        public void DebriefRowsMatchTheReport()
        {
            var root = Instantiate("Debrief");
            foreach (var row in DebriefScreen.Rows)
            {
                Assert.That(root.Q("row-" + row), Is.Not.Null, $"Debrief.uxml has no row '{row}'");
                Assert.That(root.Q<Label>(row + "-you"), Is.Not.Null, $"Debrief.uxml has no '{row}-you' cell");
                Assert.That(root.Q<Label>(row + "-enemy"), Is.Not.Null, $"Debrief.uxml has no '{row}-enemy' cell");
            }
            var report = new MatchReport { Winner = 0, DurationSeconds = 754f };
            report.MenLost[0] = 12; report.MenLost[1] = 40; report.Kills[0] = 30; report.Shots[0] = 300;
            var screen = new DebriefScreen(report);
            screen.Bind(root, null);
            Assert.That(root.Q<Label>("result").text, Is.EqualTo("VICTORY"));
            Assert.That(root.Q<Label>("men-lost-you").text, Is.EqualTo("12"));
            Assert.That(root.Q<Label>("men-lost-enemy").text, Is.EqualTo("40"));
            Assert.That(root.Q<Label>("accuracy-you").text, Is.EqualTo("10%"));
            Assert.That(root.Q<Label>("duration").text, Does.Contain("12:34"));
            screen.Unbind();
        }

        [Test]
        public void ShellAssetsAndCatalogExist()
        {
            var assets = AssetDatabase.LoadAssetAtPath<ShellAssets>("Assets/_Project/UI/Resources/ShellAssets.asset");
            Assert.That(assets, Is.Not.Null, "ShellAssets.asset missing (TW/UI/Build Shell Assets)");
            Assert.That(assets.Panel, Is.Not.Null); Assert.That(assets.MainMenu, Is.Not.Null); Assert.That(assets.MissionSelect, Is.Not.Null);
            Assert.That(assets.PauseMenu, Is.Not.Null); Assert.That(assets.Settings, Is.Not.Null); Assert.That(assets.Debrief, Is.Not.Null);
            Assert.That(assets.Catalog, Is.Not.Null); Assert.That(assets.Catalog.Cards.Length, Is.GreaterThanOrEqualTo(1));
            var first = assets.Catalog.Cards[0];
            Assert.That(first.BattlefieldSeed, Is.EqualTo(1917u), "the first card is today's map");
            var r = first.ToRequest(first.DefaultDifficulty);
            Assert.That(r.PeerDeployEveryTicks, Is.EqualTo(40)); Assert.That(r.PeerAttackGarrison, Is.EqualTo(8)); Assert.That(r.StartingSilver, Is.EqualTo(300));
            Assert.That(r.Bombardment, Is.EqualTo(8f)); Assert.That(r.GeneratedBattlefield, Is.True);
            foreach (var c in assets.Catalog.Cards) foreach (var d in c.Difficulties)
                Assert.That(d.PeerDeployEveryTicks, Is.GreaterThan(0), $"{c.Title} / {d.Name}: SimHost divides by PeerDeployEveryTicks");
        }

        [Test]
        public void ARequestAppliesToAHostsFields()
        {
            var go = new UnityEngine.GameObject("host-apply-test"); go.SetActive(false);
            try
            {
                var host = go.AddComponent<SimHost>();
                MatchLaunch.Current = new MatchLaunch.Request { StartingSilver = 999, PeerDeployEveryTicks = 77, BattlefieldSeed = 42, PeerDeploysTanks = true, Difficulty = "HARD" };
                MatchLaunch.Apply(host);
                Assert.That(host.StartingSilver, Is.EqualTo(999)); Assert.That(host.PeerDeployEveryTicks, Is.EqualTo(77));
                Assert.That(host.BattlefieldSeed, Is.EqualTo(42u)); Assert.That(host.PeerDeploysTanks, Is.True);
                Assert.That(MatchLaunch.Running.Difficulty, Is.EqualTo("HARD"));
                MatchLaunch.Current = new MatchLaunch.Request { PeerDeployEveryTicks = 0 };
                MatchLaunch.Apply(host);
                Assert.That(host.PeerDeployEveryTicks, Is.EqualTo(1), "a zero interval would divide by zero in SimHost");
            }
            finally { MatchLaunch.Current = null; UnityEngine.Object.DestroyImmediate(go); }
        }

        [Test]
        public void NoMissionShellsTheFieldAllMatchLong()
        {
            // the owner, 2026-09-28: no constant bombardment. A host starts quiet, a mission's rate is carried and not
            // applied, and the battle scene does not set one of its own
            var go = new UnityEngine.GameObject("host-quiet-test"); go.SetActive(false);
            try
            {
                var host = go.AddComponent<SimHost>();
                Assert.That(host.BombardmentPerMinute, Is.EqualTo(0f), "a new host is quiet");
                host.BombardmentPerMinute = 8f;   // what the scene held before
                MatchLaunch.Current = new MatchLaunch.Request { Bombardment = 8f };
                MatchLaunch.Apply(host);
                Assert.That(host.BombardmentPerMinute, Is.EqualTo(0f), "a mission's rate must not reach the host");
                var scene = System.IO.File.ReadAllText($"Assets/_Project/Scenes/{MatchLaunch.BattleScene}.unity");
                Assert.That(scene, Does.Contain("BombardmentPerMinute: 0\n").Or.Contain("BombardmentPerMinute: 0\r"), "the battle scene's own rate");
            }
            finally { MatchLaunch.Current = null; UnityEngine.Object.DestroyImmediate(go); }
        }
    }
}
