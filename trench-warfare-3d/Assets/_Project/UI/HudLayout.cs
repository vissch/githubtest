// Phase: B6 (implemented) — the battle HUD's spacing, in reference pixels, in one place.
// The panel scales with screen size against a 1920x1080 reference (PanelSettings), so every number here is a 1080p
// pixel and the same number appears once more as a --tw-* token in dustfront.tokens.uss; SkinAssetTests hold the two
// equal. Successor of BattleHud.BarMetrics/BarConsumed: the bar's width is the sum of what the cards consume, never
// an independent guess at it, and HudLayoutTests hold that sum inside the reference width at every supported aspect.
using TW.Sim;

namespace TW.UI
{
    public static class HudLayout
    {
        // The panel's reference resolution. 1600x900 rather than 1920x1080 draws the whole interface 1.2x larger on
        // every screen (a 72 px card is 86 px tall at 1080p, Dust Front's card size); every px below is in this space.
        public const float ReferenceHeight = 900f, ReferenceWidthPx = 1600f;

        // the bottom bar
        public const float BarHeightPx = 100f;   // BattleHud.BarHeight
        public const float CardPx = 72f;         // >= 70 so the unit name still fits (HudLayoutTests)
        public const float GapPx = 8f;           // BattleHud.Gap
        public const float InsetPx = 12f;        // BattleHud.Inset
        public const float DividerPx = 20f;      // BattleHud.Divider
        public const float CaptionPx = 14f;      // the INFANTRY / ARMOUR / SUPPORT caption above each group
        public const int SupportSlots = 2;
        public const int Dividers = 2;           // infantry | armour | support

        // the order cluster anchored on a trench
        public const float OrderBtnPx = 62f;     // BattleHud b
        public const float OrderGapPx = 6f;      // BattleHud gap
        public const float OrderOffPx = 40f;     // BattleHud off: half the hole the trench shows through
        public const float ClusterWidthPx = 4f * OrderBtnPx + 2f * OrderGapPx + 2f * OrderOffPx;   // 340
        public const float ClusterTopMinPx = 70f;
        public const float ClusterBottomMarginPx = 26f;

        // the rest
        public const float MinimapScale = 3.2f;  // minimap pixels per nav cell (BattleHud.MapScale)
        public const float MinimapBezelPx = 24f;
        public const float TooltipDelaySeconds = 0.35f;
        public const float BannerSeconds = 3f;

        /// <summary>The reference-space width the panel gives a screen of this aspect (height is always ReferenceHeight).</summary>
        public static float ReferenceWidth(float screenW, float screenH) => ReferenceHeight * screenW / screenH;

        /// <summary>
        /// The bar's width for a number of cards and dividers: the left inset, every card at a (card + gap) pitch
        /// less the trailing gap, a divider per group break, and the matching right inset. What the layout consumes.
        /// </summary>
        public static float BarWidth(int cards, int dividers) =>
            InsetPx * 2f + cards * CardPx + (cards - 1) * GapPx + dividers * DividerPx;

        /// <summary>The default bar: every roster slot plus the support pair, in three groups.</summary>
        public static float BarWidth() => BarWidth(RosterEntry.SlotCount + SupportSlots, Dividers);

        /// <summary>The de-overlap threshold for neighbouring trench clusters, as BattleHud.cs:430 had it.</summary>
        public const float ClusterOverlapPx = 2f * (OrderOffPx + 2f * OrderBtnPx + OrderGapPx);
    }
}
