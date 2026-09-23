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
    }
}
