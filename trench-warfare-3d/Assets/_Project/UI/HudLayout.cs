// Phase: B6 (implemented) — the battle HUD's spacing, in reference pixels, in one place.
// The panel scales with screen size against a 1920x1080 reference (PanelSettings), so every number here is a 1080p
// pixel and the same number appears once more as a --tw-* token in dustfront.tokens.uss; SkinAssetTests hold the two
// equal. Successor of BattleHud.BarMetrics/BarConsumed: the bar's width is the sum of what the cards consume, never
// an independent guess at it, and HudLayoutTests hold that sum inside the reference width at every supported aspect.
using Unity.Collections;
using TW.Sim;

namespace TW.UI
{
    public static class HudLayout
    {
        // The panel's reference resolution. 1600x900 rather than 1920x1080 draws the whole interface 1.2x larger on
        // every screen (a 72 px card is 86 px tall at 1080p, Dust Front's card size); every px below is in this space.
        public const float ReferenceHeight = 900f, ReferenceWidthPx = 1600f;

        // The battle HUD draws another 1.5x on top of that (owner, 2026-09-23: "the rest of the ui 1.5x"), and the
        // infantry cards twice the other cards so they come out 3x their earlier size ("especially the icons of the
        // soldier 3x"). The HUD root is laid out in a space HudScale times smaller than the panel and scaled back up
        // (HudView.ApplyScale), so every HUD number below is in THAT space: 1067 x 600 at 16:9. The shell's menus share
        // the panel and are not scaled. Three times the soldier and one and a half the rest do not fit one row at 16:9,
        // so ARMOUR and SUPPORT stack in two rows beside the infantry, which stand as tall as both.
        public const float HudScale = 1.5f;
        public static float HudHeight => ReferenceHeight / HudScale;
        // the bottom bar
        public const float CardPx = 72f;         // >= 70 so the unit name still fits (HudLayoutTests)
        public const float InfantryCardPx = 2f * CardPx + StackGapPx;   // as tall as ARMOUR over SUPPORT: ~3x the pre-HudScale card
        public const float StackGapPx = 6f;      // between the ARMOUR row and the SUPPORT row
        public const float GapPx = 8f;           // BattleHud.Gap
        public const float InsetPx = 12f;        // BattleHud.Inset
        public const float DividerPx = 20f;      // BattleHud.Divider
        public const float CaptionPx = 14f;      // the INFANTRY / ARMOUR / SUPPORT caption above each group
        public const float CaptionGapPx = 2f;    // under each caption
        public const float BarPadTopPx = 6f, BarPadBottomPx = 8f;
        public const int SupportSlots = 2;
        public const int Dividers = 1;           // infantry | (armour over support)
        /// <summary>One caption line (INFANTRY, ARMOUR), then the cards: infantry beside ARMOUR over SUPPORT, whose
        /// caption sits beside its two cards instead of taking a line of its own (round 7: one rhythm, tops and bottoms
        /// aligned).</summary>
        public const float StackHeightPx = CaptionPx + CaptionGapPx + InfantryCardPx;   // 166
        public const float BarHeightPx = BarPadTopPx + StackHeightPx + BarPadBottomPx;  // 180

        // the order cluster anchored on a trench
        public const float OrderBtnPx = 62f;     // BattleHud b
        public const float OrderGapPx = 6f;      // BattleHud gap
        public const float OrderOffPx = 40f;     // BattleHud off: half the hole the trench shows through
        public const float ClusterWidthPx = 4f * OrderBtnPx + 2f * OrderGapPx + 2f * OrderOffPx;   // 340
        public const float ClusterTopMinPx = 70f;
        public const float ClusterBottomMarginPx = 26f;

        // the rest
        public const float MinimapScale = 3.2f;  // minimap pixels per nav cell (BattleHud.MapScale)
        public const float MinimapBezelPx = 8f;   // round 7: the 24 px frame spent 5% of the screen on metal
        public const float TooltipDelaySeconds = 0.35f;
        public const float BannerSeconds = 3f;

        /// <summary>The reference-space width the panel gives a screen of this aspect (height is always ReferenceHeight).</summary>
        public static float ReferenceWidth(float screenW, float screenH) => ReferenceHeight * screenW / screenH;

        /// <summary>A row of cards at a (card + gap) pitch less the trailing gap.</summary>
        public static float Row(int cards, float cardPx) => cards <= 0 ? 0f : cards * cardPx + (cards - 1) * GapPx;

        /// <summary>
        /// The bar's width: the left inset, the infantry row, a card gap, the divider and a gap, the wider of the ARMOUR
        /// and SUPPORT rows (stacked), and the matching right inset. What the layout consumes.
        /// </summary>
        public static float BarWidth(int infantry, int armour, int support) =>
            InsetPx * 2f + Row(infantry, InfantryCardPx) + GapPx + DividerPx + GapPx + UnityEngine.Mathf.Max(Row(armour, CardPx), Row(support, CardPx));

        /// <summary>The default bar: our side's default roster split into infantry and armour, plus the support pair.</summary>
        public static float BarWidth()
        {
            var roster = new NativeArray<RosterEntry>(RosterEntry.SlotCount, Allocator.Temp);
            RosterEntry.FillDefault(roster, 0);
            int armour = 0;
            for (int s = 0; s < roster.Length; s++) if (roster[s].IsVehicle) armour++;
            int infantry = roster.Length - armour;
            roster.Dispose();
            return BarWidth(infantry, armour, SupportSlots);
        }

        /// <summary>The de-overlap threshold for neighbouring trench clusters, as BattleHud.cs:430 had it.</summary>
        public const float ClusterOverlapPx = 2f * (OrderOffPx + 2f * OrderBtnPx + OrderGapPx);
    }
}
