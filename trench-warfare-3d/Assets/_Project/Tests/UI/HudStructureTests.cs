// Phase: B6 (implemented) — the battle HUD's tree is what the controller expects, before any panel exists.
// The IMGUI HUD could be wrong in any way at all and pass a green gate, because no test executed OnGUI. The Toolkit
// HUD is a tree: this instantiates BattleHud.uxml, checks every element the controller queries by name, checks that
// every layout layer ignores picking (that IS the click mask: panel.Pick answers only for chrome), builds the cards
// for the default roster and checks their count, hotkeys and that no button can take keyboard focus (Space must stay
// the tactical pause). No SimHost, no scene, no frame.
using System.IO;
using System.Text.RegularExpressions;
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
        public void EveryCardCarriesItsFilmWindowAboveThePortraitAndBelowTheRim()
        {
            // The tiny film draws over the portrait it replaces, and under everything that explains the card: the
            // rim, the lock, the nameplate, the cost, the hotkey and the cooldown shade (CardFilm).
            foreach (var (what, holder) in new[] { ("UnitCard.uxml", Instantiate(CardPath)), ("the code fallback", FallbackCard()) })
            {
                var card = holder.Q<Button>("card");
                Assert.That(card, Is.Not.Null, what);
                var film = card.Q("film"); var line = card.Q("filmline");
                Assert.That(film, Is.Not.Null, $"{what} has no element named 'film'");
                Assert.That(line, Is.Not.Null, $"{what} has no element named 'filmline'");
                Assert.That(film.pickingMode, Is.EqualTo(PickingMode.Ignore), $"{what}: the film must not eat the card's click");
                Assert.That(line.pickingMode, Is.EqualTo(PickingMode.Ignore), $"{what}: nor the line");
                int Idx(string n) => card.IndexOf(card.Q(n));
                Assert.That(Idx("film"), Is.GreaterThan(Idx("portrait")), $"{what}: the film draws over the portrait");
                Assert.That(Idx("filmline"), Is.GreaterThan(Idx("film")), $"{what}: the line draws over the film");
                foreach (var over in new[] { "rim", "lock", "name", "cost", "hotkey", "cooldown" })
                    Assert.That(Idx(over), Is.GreaterThan(Idx("filmline")), $"{what}: '{over}' must draw over the film");
            }
        }

        /// <summary>The card tree HudView builds in code (no template asset): one card, built the same way.</summary>
        static VisualElement FallbackCard()
        {
            var root = Instantiate(HudPath);
            var refs = HudView.Build(root, null, DefaultRoster(), new[] { 150, 120 });
            return refs.Cards[0].Root.parent;
        }

        [Test]
        public void BuildMakesOneCardPerSlotAndPerSupportAbilityAndNoFocusableButton()
        {
            var root = Instantiate(HudPath);
            var card = AssetDatabase.LoadAssetAtPath<VisualTreeAsset>(CardPath);
            Assert.That(card, Is.Not.Null);
            var refs = HudView.Build(root, card, DefaultRoster(), new[] { 150, 120 });
            Assert.That(refs.Cards.Count, Is.EqualTo(RosterEntry.SlotCount + HudView.SupportAbilities.Length), "one card a slot and one a support ability (six since docs/21 phase 5)");
            Assert.That(refs.SupportCards.Count, Is.EqualTo(HudView.SupportAbilities.Length));
            int infantry = refs.RosterInfantry.childCount, armour = refs.RosterArmour.childCount;
            Assert.That(infantry + armour, Is.EqualTo(RosterEntry.SlotCount), "every roster slot is in one of the two groups");
            Assert.That(armour, Is.EqualTo(4), "the default roster fields four machines");
            for (int s = 0; s < RosterEntry.SlotCount; s++)
                Assert.That(refs.Cards[s].Hotkey.text, Is.EqualTo(((s + 1) % 10).ToString()), $"slot {s} hotkey badge");
            for (int i = 0; i < refs.SupportCards.Count; i++)
                Assert.That(refs.SupportCards[i].Hotkey.text, Is.EqualTo(HudText.SupportHotkey(i)), $"support card {i} carries its bound key");
            // the digit row is ten deploy slots, so barrage, gas and paratroopers sit on the free block of the function row
            for (int i = 0; i < HudText.SupportCards; i++)
                Assert.That(refs.SupportCards[i].Hotkey.text, Is.EqualTo("F" + (5 + i)), $"support card {i} is on F{5 + i} by default");
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
            // round 16 (owner): the hold-fire button gave its place to the trench's troop-category chips; F holds fire
            foreach (var n in new[] { "orders", "fallback", "lock", "categories", "advance", "gap", "garrison", "lock-icon", "holdfire-tag" })
                Assert.That(orders.Q(n), Is.Not.Null, $"TrenchOrders.uxml has no '{n}'");
            foreach (var n in new[] { "fallback", "lock", "advance" })
                Assert.That(orders.Q<Button>(n), Is.Not.Null, $"'{n}' must be a Button");
            var row = Instantiate(RowPath);
            foreach (var n in new[] { "objective", "bullet", "objective-name", "objective-progress" })
                Assert.That(row.Q(n), Is.Not.Null, $"ObjectiveRow.uxml has no '{n}'");
        }

        const string HudSheetPath = "Assets/_Project/UI/Resources/Hud/BattleHud.uss";
        const string TokensPath = "Assets/_Project/UI/Skin/dustfront.tokens.uss";

        [Test]
        public void TheAimHintSitsOnAPlateThatReadsOverBrightGround()
        {
            string sheet = File.ReadAllText(Path.GetFullPath(HudSheetPath));
            var block = Regex.Match(sheet, @"\.hud-hint\s*\{([^}]*)\}");
            Assert.That(block.Success, Is.True, "BattleHud.uss has no '.hud-hint { ... }' rule");
            string body = block.Groups[1].Value;

            var bg = Regex.Match(body, @"background-color:\s*rgba\(\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*\)");
            Assert.That(bg.Success, Is.True, "'.hud-hint' must declare a background-color plate, or the text has nothing behind it");
            float alpha = float.Parse(bg.Groups[4].Value, System.Globalization.CultureInfo.InvariantCulture);
            Assert.That(alpha, Is.GreaterThanOrEqualTo(0.85f), "the hint plate must be near-opaque, or bright ground shows through it");

            Assert.That(Regex.IsMatch(body, @"\bright\s*:"), Is.False, "'.hud-hint' must not span the screen edge-to-edge: a screen-wide plate letterboxes the HUD instead of backing the words");

            string tokens = File.ReadAllText(Path.GetFullPath(TokensPath));
            var textBright = Regex.Match(tokens, @"--tw-text-bright:\s*#([0-9A-Fa-f]{6})");
            Assert.That(textBright.Success, Is.True, "dustfront.tokens.uss has no --tw-text-bright");
            Color text = HexToColor(textBright.Groups[1].Value);

            float plateR = float.Parse(bg.Groups[1].Value, System.Globalization.CultureInfo.InvariantCulture);
            float plateG = float.Parse(bg.Groups[2].Value, System.Globalization.CultureInfo.InvariantCulture);
            float plateB = float.Parse(bg.Groups[3].Value, System.Globalization.CultureInfo.InvariantCulture);
            // composite the plate over pure white: the worst a sun-lit, blown-out crop can give behind it
            Color plateOverWhite = new Color(
                Mathf.Lerp(255f, plateR, alpha) / 255f,
                Mathf.Lerp(255f, plateG, alpha) / 255f,
                Mathf.Lerp(255f, plateB, alpha) / 255f);

            float ratio = ContrastRatio(text, plateOverWhite);
            Assert.That(ratio, Is.GreaterThanOrEqualTo(4.5), $"WCAG 2.1 AA for normal text is 4.5:1; '.hud-hint' over a blown-out crop is only {ratio:F2}:1");
        }

        static Color HexToColor(string hex)
        {
            byte r = byte.Parse(hex.Substring(0, 2), System.Globalization.NumberStyles.HexNumber);
            byte g = byte.Parse(hex.Substring(2, 2), System.Globalization.NumberStyles.HexNumber);
            byte b = byte.Parse(hex.Substring(4, 2), System.Globalization.NumberStyles.HexNumber);
            return new Color32(r, g, b, 255);
        }

        static float RelativeLuminance(Color c)
        {
            float Lin(float v) => v <= 0.03928f ? v / 12.92f : Mathf.Pow((v + 0.055f) / 1.055f, 2.4f);
            return 0.2126f * Lin(c.r) + 0.7152f * Lin(c.g) + 0.0722f * Lin(c.b);
        }

        static float ContrastRatio(Color a, Color b)
        {
            float la = RelativeLuminance(a), lb = RelativeLuminance(b);
            float lighter = Mathf.Max(la, lb), darker = Mathf.Min(la, lb);
            return (lighter + 0.05f) / (darker + 0.05f);
        }

        /// <summary>[I12] F9 is read with KeyMap.Down, which answers only while the field owns the input. With DownRaw
        /// it toggled the HUD from a menu and while the settings screen was capturing a key. Update() cannot run in
        /// EditMode (it needs a Keyboard and a HudController), so this is a source ratchet on the call itself.</summary>
        [Test]
        public void TheHudToggleGoesThroughTheFocusCheck()
        {
            var src = System.IO.File.ReadAllText("Assets/_Project/UI/HudHotkeys.cs");
            Assert.That(src, Does.Not.Contain("DownRaw("), "[I12] HudHotkeys reads a key with DownRaw, skipping InputFocus");
            Assert.That(src, Does.Contain("KeyMap.Down(GameAction.HudToggle)"), "[I12] the HUD toggle must be read with KeyMap.Down");
        }

        /// <summary>[I13] The interim hooks the Toolkit HUD needed while the IMGUI HUD was still the HUD are gone:
        /// HudHotkeys.LegacyOverlayActive (always false by the time Update() can run, so both its branches were dead),
        /// the BattleHud.MinimapRect sync (TestPanel asks HudBridge while the Toolkit HUD is on), TrenchOrderCluster
        /// .Interactive (no writer anywhere), HoverCard.key (assigned, never read) and BindCard's inTransit parameter
        /// with the card badge it fed (the one caller always passed 0).</summary>
        [Test]
        public void TheInterimHudHooksAreGone()
        {
            const System.Reflection.BindingFlags Any = System.Reflection.BindingFlags.Public
                | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance
                | System.Reflection.BindingFlags.Static;
            Assert.That(typeof(HudHotkeys).GetField("LegacyOverlayActive", Any), Is.Null,
                "[I13] HudHotkeys.LegacyOverlayActive is dead: hotkeys only run while the Toolkit HUD is the HUD");
            Assert.That(typeof(TrenchOrderCluster).GetField("Interactive", Any), Is.Null,
                "[I13] TrenchOrderCluster.Interactive has no writer: it is always true");
            Assert.That(typeof(HoverCard).GetField("key", Any), Is.Null,
                "[I13] HoverCard.key is assigned and never read; keyCount is the one that counts");
            var bind = typeof(HudView).GetMethod("BindCard");
            Assert.That(bind, Is.Not.Null);
            foreach (var p in bind.GetParameters())
                Assert.That(p.Name, Is.Not.EqualTo("inTransit"), "[I13] BindCard's inTransit is always 0 at its one caller");
            Assert.That(typeof(CardRefs).GetField("Badge", Any), Is.Null,
                "[I13] the card badge was only ever written from inTransit, so it went with it");
            var ctrl = System.IO.File.ReadAllText("Assets/_Project/UI/HudController.cs");
            Assert.That(ctrl, Does.Not.Contain("BattleHud.MinimapRect ="),
                "[I13] the MinimapRect sync is dead: TestPanel reads HudBridge while the Toolkit HUD is on");
        }
    }
}
