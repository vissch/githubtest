// Phase: B6 (implemented) — builds the battle HUD's element tree and binds sim values into it, with no SimHost in sight.
// Everything here is pure over a VisualElement root, a roster and plain numbers, so an EditMode test can instantiate
// BattleHud.uxml, call Build and Bind*, and assert every label against the sim constant it quotes (HudBindTests).
// Binding is by comparison: a Label's text is set only when the cached value changed, a class only toggled when the
// state changed, so a steady frame touches nothing (the IMGUI HUD allocated 13 KB a frame rebuilding strings).
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Sim;
using TW.Sim.Match;

namespace TW.UI
{
    /// <summary>One card on the bar and the last values written into it.</summary>
    public sealed class CardRefs
    {
        public Button Root;
        public VisualElement Portrait, Cooldown, Rim, Lock, Weapon;
        public Label Name, Cost, Hotkey, Badge;
        public int Slot = -1;                      // roster slot, or -1 for a support card
        public byte Archetype;
        public OffMapAbilityId Ability = OffMapAbilityId.None;
        public string Title, Tip;                  // tooltip text, built once
        public string Label;                       // the nameplate (shorter than Title on the support cards)
        public string PortraitClass;
        public int BaseCost;
        // cached state
        public int LastCostText = int.MinValue, LastCooldownPct = -1, LastBadge = -1;
        public bool LastLocked, LastPoor, LastArmed, LastOver, LastCooling, LastEnabled = true;
        public bool IsSupport => Slot < 0;
    }

    public sealed class HudRefs
    {
        public VisualElement Root;
        public Label SilverValue, IncomeValue, MenValue, EnemyValue, TimeValue, SpeedValue;
        public VisualElement Objectives, ObjectivesList;
        public VisualElement MinimapBezel, Minimap, MinimapGround, MinimapDots, MinimapView;
        public VisualElement SpeedBar; public readonly Button[] SpeedButtons = new Button[5];
        public VisualElement Bar, RosterInfantry, RosterArmour, Support;
        public VisualElement Tooltip; public Label TooltipTitle, TooltipBody, TooltipCost, Hint;
        public VisualElement OrdersLayer, Banner, PausePlate; public Label BannerText;
        public readonly List<CardRefs> Cards = new List<CardRefs>(RosterEntry.SlotCount + 2);
        public readonly List<CardRefs> SupportCards = new List<CardRefs>(2);
        // cached values
        public int LastSilver = int.MinValue, LastMen = -1, LastEnemy = -1, LastSeconds = -1, LastSpeedIdx = -1;
        public float LastIncome = float.NaN;
        public bool LastPaused;
    }

    public static class HudView
    {
        public const string RootName = "hud-root";
        public static readonly string[] RequiredNames =
        {
            "hud-root", "region-topleft", "region-topright", "region-bottom", "gauges", "silver-value", "income-value", "men-value",
            "enemy-value", "time-value", "speed-value", "objectives", "objectives-list", "minimap-bezel", "minimap", "minimap-ground",
            "minimap-dots", "minimap-view", "speed-bar", "speed-pause", "speed-1", "speed-2", "speed-4", "speed-8", "bar",
            "roster-infantry", "roster-armour", "support", "tooltip", "tooltip-title", "tooltip-body", "tooltip-cost", "hint",
            "orders-layer", "banner", "banner-text", "pause-plate",
        };
        public static readonly string[] IgnorePickingNames = { "hud-root", "region-topleft", "region-topright", "region-bottom", "orders-layer", "banner", "hint", "tooltip", "pause-plate" };

        /// <summary>The support abilities the bar offers, in card order; the hotkeys are "9" and "0".</summary>
        public static readonly OffMapAbilityId[] SupportAbilities =
        {
            OffMapAbilityId.HeBarrage, OffMapAbilityId.ChlorineGas, OffMapAbilityId.CreepingBarrage, OffMapAbilityId.SmokeScreen, OffMapAbilityId.StrafeRun, OffMapAbilityId.Beam,
        };

        /// <summary>
        /// Query the fixed elements, instantiate one card per roster slot (infantry left, machines right) and one per
        /// support ability, and turn keyboard focus off on every button so Space stays the tactical pause.
        /// </summary>
        public static HudRefs Build(VisualElement root, VisualTreeAsset cardTemplate, RosterEntry[] roster, int[] supportCosts)
        {
            var r = new HudRefs { Root = root.name == RootName ? root : root.Q(RootName) ?? root };
            r.SilverValue = root.Q<Label>("silver-value"); r.IncomeValue = root.Q<Label>("income-value");
            r.MenValue = root.Q<Label>("men-value"); r.EnemyValue = root.Q<Label>("enemy-value");
            r.TimeValue = root.Q<Label>("time-value"); r.SpeedValue = root.Q<Label>("speed-value");
            r.Objectives = root.Q("objectives"); r.ObjectivesList = root.Q("objectives-list");
            r.MinimapBezel = root.Q("minimap-bezel"); r.Minimap = root.Q("minimap"); r.MinimapGround = root.Q("minimap-ground");
            r.MinimapDots = root.Q("minimap-dots"); r.MinimapView = root.Q("minimap-view");
            r.SpeedBar = root.Q("speed-bar");
            r.SpeedButtons[0] = root.Q<Button>("speed-pause"); r.SpeedButtons[1] = root.Q<Button>("speed-1");
            r.SpeedButtons[2] = root.Q<Button>("speed-2"); r.SpeedButtons[3] = root.Q<Button>("speed-4"); r.SpeedButtons[4] = root.Q<Button>("speed-8");
            r.Bar = root.Q("bar"); r.RosterInfantry = root.Q("roster-infantry"); r.RosterArmour = root.Q("roster-armour"); r.Support = root.Q("support");
            r.Tooltip = root.Q("tooltip"); r.TooltipTitle = root.Q<Label>("tooltip-title"); r.TooltipBody = root.Q<Label>("tooltip-body");
            r.TooltipCost = root.Q<Label>("tooltip-cost"); r.Hint = root.Q<Label>("hint");
            r.OrdersLayer = root.Q("orders-layer"); r.Banner = root.Q("banner"); r.BannerText = root.Q<Label>("banner-text"); r.PausePlate = root.Q("pause-plate");

            for (int s = 0; s < roster.Length; s++)
            {
                var e = roster[s];
                var card = MakeCard(cardTemplate, e.IsVehicle ? r.RosterArmour : r.RosterInfantry);
                card.Slot = s; card.Archetype = e.Archetype; card.BaseCost = e.Cost;
                card.Title = HudText.Name(e.Archetype).ToUpperInvariant();
                card.Tip = HudText.Tip(e.Archetype);
                card.Hotkey.text = HudText.Hotkey(s);
                card.Label = HudText.Name(e.Archetype).ToUpperInvariant();
                card.Name.text = card.Label;
                SetPortrait(card, HudText.PortraitName(e.Archetype));
                SetWeaponBadge(card, e.Archetype);
                r.Cards.Add(card);
            }
            for (int i = 0; i < SupportAbilities.Length; i++)
            {
                var card = MakeCard(cardTemplate, r.Support);
                card.Ability = SupportAbilities[i];
                card.BaseCost = supportCosts != null && i < supportCosts.Length ? supportCosts[i] : 0;
                var text = HudText.Support(card.Ability);
                card.Title = text.Name.ToUpperInvariant();
                card.Tip = text.Tip;
                card.Hotkey.text = HudText.SupportHotkey(i);
                card.Label = text.Card;
                card.Name.text = card.Label;
                SetPortrait(card, text.Portrait);
                r.Cards.Add(card); r.SupportCards.Add(card);
            }
            root.Query<Button>().ForEach(b => b.focusable = false);
            ApplyScale(r);
            return r;
        }

        /// <summary>
        /// Lay the HUD out in a space HudLayout.HudScale times smaller than the panel and scale it back up, so the whole
        /// battle HUD draws 1.5x while the menus sharing the panel do not. Picking, worldBound and ScreenToPanel all see
        /// the scaled result; only code that places elements from panel coordinates divides by HudScale. Where the
        /// screen is too narrow for the bar (4:3 and narrower), the bar alone shrinks to fit, anchored bottom-centre.
        /// </summary>
        public static void ApplyScale(HudRefs r)
        {
            var hud = r.Root;
            if (hud == null || hud.name != RootName) return;
            float k = HudLayout.HudScale, pct = 100f / k;
            hud.style.right = StyleKeyword.Auto; hud.style.bottom = StyleKeyword.Auto;
            hud.style.width = Length.Percent(pct); hud.style.height = Length.Percent(pct);
            hud.style.transformOrigin = new TransformOrigin(0, 0, 0);
            hud.style.scale = new Scale(new Vector3(k, k, 1f));
            if (r.Bar != null)
            {
                r.Bar.style.transformOrigin = new TransformOrigin(Length.Percent(50f), Length.Percent(100f), 0f);
                hud.RegisterCallback<GeometryChangedEvent>(_ => FitBar(r));
            }
        }

        /// <summary>The bar's shrink factor for a HUD this wide (HUD space): 1 when it fits with a margin either side.</summary>
        public static float BarFit(float hudWidth) => Mathf.Clamp01((hudWidth - 2f * HudLayout.InsetPx) / HudLayout.BarWidth());

        static void FitBar(HudRefs r)
        {
            float w = r.Root.resolvedStyle.width;
            if (float.IsNaN(w) || w <= 0f) return;
            float f = BarFit(w);
            if (Mathf.Abs(r.Bar.resolvedStyle.scale.value.x - f) > 0.001f) r.Bar.style.scale = new Scale(new Vector3(f, f, 1f));
        }

        public const string SlotClass = "hud-card-slot", FirstSlotClass = "hud-card-slot--first";

        static CardRefs MakeCard(VisualTreeAsset template, VisualElement parent)
        {
            var c = new CardRefs();
            VisualElement holder;
            if (template != null) { holder = template.Instantiate(); parent.Add(holder); }
            else { holder = BuildCardFallback(); parent.Add(holder); }
            holder.AddToClassList(SlotClass);   // the gap rule's hook: a runtime TemplateContainer carries no unity-template-container class
            if (parent.childCount == 1) holder.AddToClassList(FirstSlotClass);   // USS has no :first-child, so the group's first card is marked here
            c.Root = holder.Q<Button>("card");
            c.Portrait = holder.Q("portrait"); c.Cooldown = holder.Q("cooldown"); c.Rim = holder.Q("rim"); c.Lock = holder.Q("lock"); c.Weapon = holder.Q("weapon");
            c.Name = holder.Q<Label>("name"); c.Cost = holder.Q<Label>("cost"); c.Hotkey = holder.Q<Label>("hotkey"); c.Badge = holder.Q<Label>("badge");
            return c;
        }

        /// <summary>The card tree in code, for tests that run without the template asset.</summary>
        static VisualElement BuildCardFallback()
        {
            var holder = new VisualElement();
            var b = new Button { name = "card" }; b.AddToClassList("tw-card"); b.AddToClassList("hud-card"); holder.Add(b);
            void Ve(string n, string cls) { var v = new VisualElement { name = n, pickingMode = PickingMode.Ignore }; v.AddToClassList(cls); b.Add(v); }
            void Lb(string n, string cls) { var l = new Label { name = n, pickingMode = PickingMode.Ignore }; l.AddToClassList(cls); b.Add(l); }
            Ve("portrait", "tw-card__portrait"); Ve("cooldown", "tw-card__cooldown"); Ve("rim", "tw-card__rim"); Ve("lock", "tw-card__lock"); Ve("weapon", "tw-card__weapon");
            Lb("name", "tw-card__nameplate"); Lb("cost", "tw-card__cost"); Lb("hotkey", "tw-card__hotkey"); Lb("badge", "tw-card__badge");
            return holder;
        }

        /// <summary>The portrait is a USS class per unit name (dustfront.components.uss), so the skin folder stays the one
        /// place art lives and a missing file degrades to the plate colour rather than a load failure.</summary>
        public static void SetPortrait(CardRefs c, string portraitName)
        {
            c.PortraitClass = "tw-portrait-" + portraitName;
            c.Portrait.AddToClassList(c.PortraitClass);
        }

        static void SetWeaponBadge(CardRefs c, byte archetype)
        {
            string cls = archetype == 0 ? "tw-ico-rifle" : archetype == 1 ? "tw-ico-smg" : archetype == 2 ? "tw-ico-mg" : null;
            if (cls == null) return;
            c.Weapon.AddToClassList(cls);
            c.Weapon.style.display = DisplayStyle.Flex;
        }

        // ---- binding ------------------------------------------------------------------------------------------------

        /// <summary>A roster card from the sim's numbers. Returns true when anything on it changed.</summary>
        public static bool BindCard(CardRefs c, int silver, int cooldownTicks, int cooldownTotalTicks, bool unlocked, bool over, float tickSeconds, int inTransit)
        {
            bool changed = false;
            bool cooling = cooldownTicks > 0;
            bool poor = unlocked && !cooling && silver < c.BaseCost;
            bool enabled = !over && unlocked && !cooling && !poor;
            changed |= Toggle(c.Root, "is-locked", !unlocked, ref c.LastLocked);
            changed |= Toggle(c.Root, "is-cooling", cooling, ref c.LastCooling);
            changed |= Toggle(c.Root, "is-poor", poor, ref c.LastPoor);
            changed |= Toggle(c.Root, "is-over", over, ref c.LastOver);
            if (enabled != c.LastEnabled) { c.Root.SetEnabled(enabled); c.LastEnabled = enabled; changed = true; }
            int costText = cooling ? -Mathf.Max(1, Mathf.CeilToInt(cooldownTicks * tickSeconds)) : c.BaseCost;   // negative = seconds
            if (costText != c.LastCostText)
            {
                c.Cost.text = costText < 0 ? IntText.Seconds(-costText) : IntText.Get(costText);
                c.LastCostText = costText; changed = true;
            }
            int pct = cooling && cooldownTotalTicks > 0 ? Mathf.Clamp(Mathf.RoundToInt(100f * cooldownTicks / cooldownTotalTicks), 1, 100) : 0;
            if (pct != c.LastCooldownPct) { c.Cooldown.style.height = Length.Percent(pct * 0.86f); c.LastCooldownPct = pct; changed = true; }
            if (inTransit != c.LastBadge)
            {
                c.Badge.text = inTransit > 0 ? IntText.Get(inTransit) : "";
                c.Badge.style.display = inTransit > 0 ? DisplayStyle.Flex : DisplayStyle.None;
                c.LastBadge = inTransit; changed = true;
            }
            return changed;
        }

        /// <summary>A support card: armed shows the red rim and AIM on the nameplate.</summary>
        public static bool BindSupportCard(CardRefs c, int silver, int cooldownTicks, int cooldownTotalTicks, bool armed, bool over, float tickSeconds)
        {
            bool changed = false;
            bool cooling = cooldownTicks > 0;
            bool poor = !cooling && silver < c.BaseCost;
            bool enabled = armed || (!over && !cooling && !poor);
            changed |= Toggle(c.Root, "is-armed", armed, ref c.LastArmed);
            changed |= Toggle(c.Root, "is-cooling", cooling, ref c.LastCooling);
            changed |= Toggle(c.Root, "is-poor", poor && !armed, ref c.LastPoor);
            changed |= Toggle(c.Root, "is-over", over, ref c.LastOver);
            if (enabled != c.LastEnabled) { c.Root.SetEnabled(enabled); c.LastEnabled = enabled; changed = true; }
            if (armed != (c.Name.text == HudText.Aim)) { c.Name.text = armed ? HudText.Aim : c.Label; changed = true; }
            int costText = cooling ? -Mathf.Max(1, Mathf.CeilToInt(cooldownTicks * tickSeconds)) : c.BaseCost;
            if (costText != c.LastCostText) { c.Cost.text = costText < 0 ? IntText.Seconds(-costText) : IntText.Get(costText); c.LastCostText = costText; changed = true; }
            int pct = cooling && cooldownTotalTicks > 0 ? Mathf.Clamp(Mathf.RoundToInt(100f * cooldownTicks / cooldownTotalTicks), 1, 100) : 0;
            if (pct != c.LastCooldownPct) { c.Cooldown.style.height = Length.Percent(pct * 0.86f); c.LastCooldownPct = pct; changed = true; }
            return changed;
        }

        public static void BindGauges(HudRefs r, int silver, float income, int men, int enemy, int seconds, float speed, bool paused)
        {
            if (silver != r.LastSilver) { r.SilverValue.text = IntText.Get(silver); r.LastSilver = silver; }
            if (!Mathf.Approximately(income, r.LastIncome)) { r.IncomeValue.text = income >= 10f ? $"+{income:0}/s" : $"+{income:0.#}/s"; r.LastIncome = income; }
            if (men != r.LastMen) { r.MenValue.text = IntText.Get(men); r.LastMen = men; }
            if (enemy != r.LastEnemy) { r.EnemyValue.text = IntText.Get(enemy); r.LastEnemy = enemy; }
            if (seconds != r.LastSeconds) { r.TimeValue.text = IntText.Clock(seconds); r.LastSeconds = seconds; }
            int speedIdx = paused ? 0 : SpeedIndex(speed);
            if (speedIdx != r.LastSpeedIdx || paused != r.LastPaused)
            {
                r.SpeedValue.text = paused ? HudText.Paused : SpeedText(speedIdx);
                r.SpeedValue.style.visibility = paused ? Visibility.Visible : Visibility.Hidden;   // the speed bar already shows 1x..8x
                r.SpeedValue.EnableInClassList("tw-alarm", paused);
                for (int k = 0; k < r.SpeedButtons.Length; k++) r.SpeedButtons[k]?.EnableInClassList("tw-btn--on", k == speedIdx);
                r.PausePlate?.EnableInClassList("is-visible", paused);
                r.LastSpeedIdx = speedIdx; r.LastPaused = paused;
            }
        }

        public static int SpeedIndex(float speed) => speed <= 0f ? 0 : speed < 1.5f ? 1 : speed < 3f ? 2 : speed < 6f ? 3 : 4;
        public static string SpeedText(int idx) => idx switch { 0 => HudText.Paused, 1 => "1x", 2 => "2x", 3 => "4x", _ => "8x" };
        public static readonly float[] SpeedOfIndex = { 0f, 1f, 2f, 4f, 8f };

        public static bool Toggle(VisualElement e, string cls, bool on, ref bool last, bool force = false)
        {
            if (!force && on == last) return false;
            e.EnableInClassList(cls, on);
            last = on;
            return true;
        }

        public static void ShowHint(HudRefs r, string text)
        {
            if (r.Hint == null) return;
            bool show = !string.IsNullOrEmpty(text);
            if (show && r.Hint.text != text) r.Hint.text = text;
            r.Hint.EnableInClassList("is-visible", show);
        }
    }

    /// <summary>Integer-to-text without a fresh string per frame: the strings for small values are made once.</summary>
    public static class IntText
    {
        const int Cache = 4096;
        static readonly string[] ints = new string[Cache];
        static readonly string[] secs = new string[256];
        static readonly string[] clocks = new string[3600 * 2];

        public static string Get(int v)
        {
            if (v < 0 || v >= Cache) return v.ToString();
            return ints[v] ??= v.ToString();
        }
        public static string Seconds(int s)
        {
            if (s < 0 || s >= secs.Length) return s + "s";
            return secs[s] ??= s + "s";
        }
        public static string Clock(int seconds)
        {
            if (seconds < 0) seconds = 0;
            if (seconds >= clocks.Length) return $"{seconds / 60}:{seconds % 60:00}";
            return clocks[seconds] ??= $"{seconds / 60:00}:{seconds % 60:00}";
        }
    }
}
