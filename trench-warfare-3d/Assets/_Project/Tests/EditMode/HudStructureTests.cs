// Phase: B6 (implemented) — the battle HUD's tree is what the controller expects, before any panel exists.
// The IMGUI HUD could be wrong in any way at all and pass a green gate, because no test executed OnGUI. The Toolkit
// HUD is a tree: this instantiates BattleHud.uxml, checks every element the controller queries by name, checks that
// every layout layer ignores picking (that IS the click mask: panel.Pick answers only for chrome), builds the cards
// for the default roster and checks their count, hotkeys and that no button can take keyboard focus (Space must stay
// the tactical pause). No SimHost, no scene, no frame.
using NUnit.Framework;
using Unity.Collections;
using UnityEditor;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Sim;
using TW.UI;

namespace TW.Tests
{
    public class HudStructureTests
    {
        const string HudPath = "Assets/_Project/UI/Resources/Hud/BattleHud.uxml";
        const string CardPath = "Assets/_Project/UI/Resources/Hud/UnitCard.uxml";
        const string OrdersPath = "Assets/_Project/UI/Resources/Hud/TrenchOrders.uxml";
        const string RowPath = "Assets/_Project/UI/Resources/Hud/ObjectiveRow.uxml";

        static VisualElement Instantiate(string path)
        {
            var tree = AssetDatabase.LoadAssetAtPath<VisualTreeAsset>(path);
            Assert.That(tree, Is.Not.Null, $"{path} did not load as a VisualTreeAsset");
            return tree.Instantiate();
        }

        static RosterEntry[] DefaultRoster()
        {
            var na = new NativeArray<RosterEntry>(RosterEntry.SlotCount, Allocator.Temp);
            RosterEntry.FillDefault(na, 0);
            var r = na.ToArray(); na.Dispose(); return r;
        }

        [Test]
        public void EveryElementTheControllerQueriesExists()
        {
            var root = Instantiate(HudPath);
            foreach (var name in HudView.RequiredNames)
                Assert.That(root.Q(name), Is.Not.Null, $"BattleHud.uxml has no element named '{name}'");
        }

        [Test]
        public void LayoutLayersIgnorePicking()
        {
            var root = Instantiate(HudPath);
            foreach (var name in HudView.IgnorePickingNames)
                Assert.That(root.Q(name).pickingMode, Is.EqualTo(PickingMode.Ignore),
                    $"'{name}' must ignore picking, or a click on empty screen counts as a click on the HUD and the barrage never fires");
        }

        [Test]
        public void ChromePicks()
        {
            var root = Instantiate(HudPath);
            foreach (var name in new[] { "gauges", "bar", "minimap", "speed-bar", "objectives" })
                Assert.That(root.Q(name).pickingMode, Is.EqualTo(PickingMode.Position), $"'{name}' is chrome and must pick, or clicks fall through to the field");
        }

        [Test]
        public void BuildMakesOneCardPerSlotPlusTwoSupportAndNoFocusableButton()
        {
            var root = Instantiate(HudPath);
            var card = AssetDatabase.LoadAssetAtPath<VisualTreeAsset>(CardPath);
            Assert.That(card, Is.Not.Null);
            var refs = HudView.Build(root, card, DefaultRoster(), new[] { 150, 120 });
            Assert.That(refs.Cards.Count, Is.EqualTo(RosterEntry.SlotCount + HudText.SupportCards));
            Assert.That(refs.SupportCards.Count, Is.EqualTo(HudText.SupportCards), "barrage, gas, paratroopers");
            int infantry = refs.RosterInfantry.childCount, armour = refs.RosterArmour.childCount;
            Assert.That(infantry + armour, Is.EqualTo(RosterEntry.SlotCount), "every roster slot is in one of the two groups");
            Assert.That(armour, Is.EqualTo(4), "the default roster fields four machines");
            for (int s = 0; s < RosterEntry.SlotCount; s++)
                Assert.That(refs.Cards[s].Hotkey.text, Is.EqualTo(((s + 1) % 10).ToString()), $"slot {s} hotkey badge");
            // the digit row is ten deploy slots now, so the support cards are on the free block of the function row
            for (int i = 0; i < HudText.SupportCards; i++)
                Assert.That(refs.SupportCards[i].Hotkey.text, Is.EqualTo("F" + (5 + i)), $"support card {i}");
            root.Query<Button>().ForEach(b => Assert.That(b.focusable, Is.False, $"button '{b.name}' is focusable: Space would re-press it instead of pausing"));
        }

        [Test]
        public void CardsCarryTheirPortraitClassAndName()
        {
            var root = Instantiate(HudPath);
            var refs = HudView.Build(root, AssetDatabase.LoadAssetAtPath<VisualTreeAsset>(CardPath), DefaultRoster(), new[] { 150, 120 });
            foreach (var c in refs.Cards)
            {
                Assert.That(c.PortraitClass, Does.StartWith("tw-portrait-"), c.Title);
                Assert.That(c.Portrait.ClassListContains(c.PortraitClass), c.Title);
                Assert.That(c.Name.text, Is.Not.Empty, "every card is named");
                Assert.That(c.Name.text, Is.EqualTo(c.Name.text.ToUpperInvariant()), "names are uppercase: USS has no text-transform");
                Assert.That(c.Tip.Length, Is.LessThanOrEqualTo(100), $"{c.Title}: tooltip is {c.Tip.Length} characters");
            }
        }

        [Test]
        public void TheOrderClusterAndObjectiveRowTemplatesHaveTheirParts()
        {
            var orders = Instantiate(OrdersPath);
            foreach (var n in new[] { "orders", "fallback", "lock", "holdfire", "advance", "gap", "garrison", "lock-icon", "holdfire-icon" })
                Assert.That(orders.Q(n), Is.Not.Null, $"TrenchOrders.uxml has no '{n}'");
            foreach (var n in new[] { "fallback", "lock", "holdfire", "advance" })
                Assert.That(orders.Q<Button>(n), Is.Not.Null, $"'{n}' must be a Button");
            var row = Instantiate(RowPath);
            foreach (var n in new[] { "objective", "bullet", "objective-name", "objective-progress" })
                Assert.That(row.Q(n), Is.Not.Null, $"ObjectiveRow.uxml has no '{n}'");
        }
    }
}
