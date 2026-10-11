// Phase: B6 (implemented) — every shell screen's UXML has the parts its controller queries, and binds with no panel.
// Also the mission catalog: at least one card, the first card is today's map (seed 1917, today's enemy knobs), and
// every difficulty has a deploy interval the sim can divide by.
using NUnit.Framework;
using UnityEditor;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;
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

        // [L5] every node carries EnemyFaction = 1, but the battle gives the enemy the other side of the player:
        // a Brass player (Faction 1) fights Iron, so the map must say IRON.
        [Test]
        public void TheMapNamesTheSideTheBattleActuallyGivesTheEnemy()
        {
            var profile = new CampaignProfile { Faction = 1 };
            var map = new StrategicMapScreen(profile);
            map.Bind(Instantiate("StrategicMap", "Assets/_Project/UI/Resources/Shell/"), null);
            try
            {
                map.Select("lowlands");
                var text = map.Root.Q<Label>("info-enemy")?.text ?? "";
                Assert.That(text, Does.Contain("IRON"), "[L5] a Brass player fights Iron, but the map names the enemy " + text);
                Assert.That(text, Does.Not.Contain("BRASS"), "[L5] the map names the player's own faction as the enemy: " + text);
            }
            finally { map.Unbind(); }
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
            SceneStatics.Reset();   // ProvingGroundTests runs in the same domain and its writes now mark the match as tooled
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

        // [N8.2] F8's test panel stays in every match, so a match it wrote into must pay no gold (owner, 2026-10-07).
        // Uses no member this unit adds, so it compiles on the old tree too: the literal "NO GOLD" and SceneStatics.Reset().
        [Test]
        public void ACampaignWinAfterTheProvingGroundPanelGaveSilverPaysNoGold()
        {
            SceneStatics.Reset();
            var profile = new CampaignProfile { Gold = 10 };
            bool? persist = ProfileStore.PersistOverride;
            ProfileStore.PersistOverride = false; ProfileStore.Use(profile);
            var cfg = SimConfig.Default; cfg.Endless = true;
            var m = MatchSim.CreatePlaytest(cfg);
            try
            {
                var ground = new ProvingGround(a => { a(m); return true; }, () => m, () => null);
                ground.GiveSilver(0, 1000);

                CampaignSession.Begin("lowlands", 0, 0);
                var root = Instantiate("Debrief");
                var screen = new DebriefScreen(new MatchReport { Winner = 0 });
                screen.Bind(root, null);
                Assert.That(profile.Gold, Is.EqualTo(10), "[N8.2] a win of a match the test panel wrote into paid gold");
                Assert.That(profile.IsComplete("lowlands", 0), Is.False, "[N8.2] a win of a match the test panel wrote into marked the mission complete");
                Assert.That(root.Q<Label>("gold-earned").text, Does.Contain("NO GOLD"), "[N8.2] the gold line does not say why a tooled win pays nothing");
                Assert.That(root.Q("gold-row").ClassListContains("tw-hidden"), Is.False, "[N8.2] the gold row is hidden instead of saying why");
                screen.Unbind();
            }
            finally { m.Dispose(); CampaignSession.Clear(); ProfileStore.Use(null); ProfileStore.PersistOverride = persist; SceneStatics.Reset(); }
        }

        // [N8.2a][N8.2e] The panel's own AI tabs write into the running match (ProvingGround.SetAi -> the host's Peer knobs),
        // so a win after one must pay no gold and record nothing (the second reader of N8.2, 2026-10-11).
        [Test]
        public void ACampaignWinAfterTheProvingGroundPanelTurnedTheEnemyOffPaysNoGold()
        {
            SceneStatics.Reset();
            var profile = new CampaignProfile { Gold = 10 };
            bool? persist = ProfileStore.PersistOverride;
            ProfileStore.PersistOverride = false; ProfileStore.Use(profile);
            var go = new UnityEngine.GameObject("pg-ai-tab-test"); go.SetActive(false);   // SimHost.Awake must not run
            try
            {
                var host = go.AddComponent<SimHost>();
                var panel = new ProvingGroundPanel(ProvingGround.For(host));
                var panelRoot = Instantiate("ProvingGroundPanel", "Assets/_Project/UI/Resources/Shell/");
                panel.Bind(panelRoot, null);                       // no router: only the director the panel was given
                Assert.That(panelRoot.Q<Button>("tab-ai-0"), Is.Not.Null, "[N8.2a] the panel has no AI OFF tab");
                panel.SetAi(0);                                    // what that tab is wired to
                panel.Unbind();

                CampaignSession.Begin("lowlands", 0, 0);
                var root = Instantiate("Debrief");
                var screen = new DebriefScreen(new MatchReport { Winner = 0 });
                screen.Bind(root, null);
                Assert.That(profile.Gold, Is.EqualTo(10), "[N8.2a] a win against an enemy the panel turned off paid gold");
                Assert.That(profile.IsComplete("lowlands", 0), Is.False, "[N8.2a] a win against an enemy the panel turned off recorded the mission");
                Assert.That(root.Q<Label>("gold-earned").text, Does.Contain("NO GOLD"), "[N8.2a] the gold line does not say why a tooled win pays nothing");
                Assert.That(host.ScriptedPeer, Is.False, "[N8.2e] the OFF tab did not turn the enemy off");
                screen.Unbind();
            }
            finally { UnityEngine.Object.DestroyImmediate(go); CampaignSession.Clear(); ProfileStore.Use(null); ProfileStore.PersistOverride = persist; SceneStatics.Reset(); }
        }

        // [N8.2e] The panel's AI tabs end in the static ApplyAi(ai, host): the knobs of the *running* match
        // (ProvingGround.cs:166 old / :172 new). A win after one pays nothing and records nothing (the second
        // reader, 2026-10-11).
        [Test]
        public void ApplyingAnAiPresetOnARunningMatchPaysNoGoldOnAWin()
        {
            SceneStatics.Reset();
            var profile = new CampaignProfile { Gold = 10 };
            bool? persist = ProfileStore.PersistOverride;
            ProfileStore.PersistOverride = false; ProfileStore.Use(profile);
            var go = new UnityEngine.GameObject("pg-applyai-test"); go.SetActive(false);   // SimHost.Awake must not run
            try
            {
                var host = go.AddComponent<SimHost>();
                ProvingGround.ApplyAi(0, host);                     // the OFF preset, the way the panel's tab applies it

                CampaignSession.Begin("lowlands", 0, 0);
                var root = Instantiate("Debrief");
                var screen = new DebriefScreen(new MatchReport { Winner = 0 });
                screen.Bind(root, null);
                Assert.That(profile.Gold, Is.EqualTo(10), "[N8.2e] a win of a match whose AI knobs the panel changed paid gold");
                Assert.That(profile.IsComplete("lowlands", 0), Is.False, "[N8.2e] a win of a match whose AI knobs the panel changed recorded the mission");
                Assert.That(root.Q<Label>("gold-earned").text, Does.Contain("NO GOLD"), "[N8.2e] the gold line does not say why a tooled win pays nothing");
                Assert.That(host.ScriptedPeer, Is.False, "[N8.2e] the OFF preset did not turn the enemy off");
                screen.Unbind();
            }
            finally { UnityEngine.Object.DestroyImmediate(go); CampaignSession.Clear(); ProfileStore.Use(null); ProfileStore.PersistOverride = persist; SceneStatics.Reset(); }
        }

        // [N8.2d] The owner, 2026-10-11: a win in a tooled match is "Not recorded: no progress and no gold; a clean
        // win later earns both". The line must say both, not only the gold.
        [Test]
        public void ATooledCampaignWinSaysTheMissionIsNotRecorded()
        {
            SceneStatics.Reset();
            var profile = new CampaignProfile { Gold = 10 };
            bool? persist = ProfileStore.PersistOverride;
            ProfileStore.PersistOverride = false; ProfileStore.Use(profile);
            var cfg = SimConfig.Default; cfg.Endless = true;
            var m = MatchSim.CreatePlaytest(cfg);
            try
            {
                var ground = new ProvingGround(a => { a(m); return true; }, () => m, () => null);
                ground.GiveSilver(0, 1000);

                CampaignSession.Begin("lowlands", 0, 0);
                var root = Instantiate("Debrief");
                var screen = new DebriefScreen(new MatchReport { Winner = 0 });
                screen.Bind(root, null);
                var text = root.Q<Label>("gold-earned").text;
                Assert.That(text, Does.Contain("NOT RECORDED"), "[N8.2d] the line does not say the mission is not recorded");
                Assert.That(text, Does.Contain("NO GOLD"), "[N8.2d] the line no longer says there is no gold");
                screen.Unbind();
            }
            finally { m.Dispose(); CampaignSession.Clear(); ProfileStore.Use(null); ProfileStore.PersistOverride = persist; SceneStatics.Reset(); }
        }

        // The control for the test above: a panel only opened and closed writes nothing, so the win pays as before.
        // Deliberately NOT tagged [N8.2]: it is green on the old code by design.
        [Test]
        public void ACampaignWinWithTheProvingGroundPanelOpenedAndUnusedPaysAsBefore()
        {
            SceneStatics.Reset();
            var profile = new CampaignProfile { Gold = 10 };
            bool? persist = ProfileStore.PersistOverride;
            ProfileStore.PersistOverride = false; ProfileStore.Use(profile);
            try
            {
                var panel = new ProvingGroundPanel();
                var panelRoot = Instantiate("ProvingGroundPanel", "Assets/_Project/UI/Resources/Shell/");
                panel.Bind(panelRoot, null); panel.Unbind();   // no router: its director stays null and nothing is written

                CampaignSession.Begin("lowlands", 0, 0);
                var root = Instantiate("Debrief");
                var screen = new DebriefScreen(new MatchReport { Winner = 0 });
                screen.Bind(root, null);
                Assert.That(profile.Gold, Is.EqualTo(10 + CampaignGraph.RewardFirst), "an unused panel must not cost the win its gold");
                Assert.That(profile.IsComplete("lowlands", 0), Is.True, "an unused panel must not cost the win its mission");
                Assert.That(root.Q<Label>("gold-earned").text, Does.Contain("+" + CampaignGraph.RewardFirst));
                screen.Unbind();
            }
            finally { CampaignSession.Clear(); ProfileStore.Use(null); ProfileStore.PersistOverride = persist; SceneStatics.Reset(); }
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

        const string NormalBlurb = "Today's opponent: deploys every two seconds, attacks at eight men, shells you when it can and when you attack.";
        const string HardBlurb = "Fast deploys, early attacks, tanks whenever it can afford them, and a barrage on any attack you make.";

        // [U095.1] the blurbs on screen were saved before the barrage answered your attack
        [Test]
        public void MissionCardBlurbsSayTheBarrageAnswersYourAttack()
        {
            var assets = AssetDatabase.LoadAssetAtPath<ShellAssets>("Assets/_Project/UI/Resources/ShellAssets.asset");
            Assert.That(assets, Is.Not.Null, "[U095.1] ShellAssets.asset missing (TW/UI/Build Shell Assets)");
            foreach (var c in assets.Catalog.Cards)
            {
                Assert.That(c.Difficulties[1].Blurb, Is.EqualTo(NormalBlurb), "[U095.1] " + c.Title + " NORMAL: the blurb is the pre-barrage one");
                Assert.That(c.Difficulties[2].Blurb, Is.EqualTo(HardBlurb), "[U095.1] " + c.Title + " HARD: the blurb is the pre-barrage one");
            }
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
