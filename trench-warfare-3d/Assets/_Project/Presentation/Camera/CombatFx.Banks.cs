// Phase: VFX pass (owner, 2026-09-28; tw3d-board catalogue IN-8, look specs L07 and L08) — part of CombatFx. Behind
// fx.recipes, the gas and smoke fields as the drawn banks: chlorine as GasBank, a low rolling bank standing ON the ground
// (its drawing fills the bottom third of the card, so a man's legs are in it and his head is not), the smoke screen as
// SmokeBank mounds. Per 4 m field cell as before, placed and boiled by a hash of the cell (no random draws); from the
// overview (BankMergeZoom) one card a 2 x 2 block, twice the size, so the field is a blanket with an edge and not
// four times the cards. The field only changes per sim tick, so which cells stand and where their ground is are found
// once a tick (and again when the zoom crosses BankMergeZoom); a frame only boils them.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Terrain;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        public const float BankMergeZoom = 120f;
        public const float GasBankFirst = 17f, GasBankLast = 28f;       // the bank at its full spread (its early frames are a puff)
        // How clearly the bank reads (film 2026-10-07: at 0.3-0.8 it was fainter than the old wisps at night and gone on snow):
        // a thin cell's card and a full one's, and its colour by day, deeper than the sheet's so it is green ON snow
        public const float GasBankThin = 0.6f, GasBankFull = 1f;
        /// <summary>How flat a gas card is drawn: the sheet fills the bottom third, so a 7 m card squashed to 0.55 tops out at 1.28 m - gas pools, it does not stand over a man.</summary>
        public const float GasBankSquash = 0.55f;
        /// <summary>Cells one close card stands for (the owner, 2026-10-21: "774 wide, low cards 4 m apart wash into
        /// a pale sheet" - -g1's own sidecars read gas_cards 774 at the close camera). A puff per block of cells,
        /// so gaps between them are what makes a puff read as a puff, not a sheet.
        /// Only the close drawing (step == 1) uses this; the merged-out overview (step 2, GasBankSquash) is
        /// untouched, so O240 does not move.
        /// 2026-10-22, the owner: "one bank is one cloud" - the close bank still read 38, 41 and 38 blobs, and about
        /// 22 of them are man-sized masses (docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2-g1), so the
        /// build has to merge them. The knob is DENSITY, not width: every GasPuffWide over 2.0 broke the owner's
        /// green_low_share >= 0.30 floor, because a wider card stands taller on screen and the share under a man's
        /// head falls. The block keeps every card's size (width is (5 + 2*Thick) * (0.9 + 0.2*h2) * GasPuffWide)
        /// and only shortens the lane BETWEEN cards - 3 x 4 = 12 m, 2 x 4 = 8 m, 1 x 4 = 4 m - so the hope was that
        /// the lanes that split the mask fill in while the silhouette's top does not rise. It does rise: the lane
        /// fill sits high on screen, so the share under a man's head falls with the block. Swept by capture, one
        /// GasBankStills gas + plate pass
        /// each at TW_GAS_WAIT_FRAMES 200 (the carried bars in brackets: low_share >= 0.30, height_frac 0.08-0.15,
        /// rise_m >= 2.4, top_share >= 0.28, soft_share >= 0.14, area_frac >= 0.05):
        ///   3 (KEPT) -> t1_visible_clouds 24/24/23, t1_cloud_count 38/41/38, top_share 0.2908, low_share 0.3045,
        ///         height_frac 0.0836, rise_m 2.653, soft_share 0.1448, area_frac 0.0532, green_px ~240k: the only
        ///         row of the sweep that holds every carried bar - and it is a bank of ~22 man-sized masses
        ///    2 -> visible 10, count 18, top 0.8408, low 0.1112, height 0.3100, rise 7.559, soft 0.0954,
        ///         area 0.4512, green_px 494571: it merges, but 2.25x the cards at the same card size doubles the
        ///         coverage and the mass reaches high up the frame, so the low_share floor, the height ceiling and
        ///         the soft edge all break at once
        ///    1 -> visible 1, count 5, top 0.9991, low 0.0685, height 0.5470, rise 10.289, soft 0.0411,
        ///         area 0.8218, green_px 758028: one cloud, and it is a green wall over four fifths of the frame
        /// So DENSITY alone cannot buy the merge either: the lanes only fill in once the gas also stands over a
        /// man's head, which is exactly the owner's green_low_share floor (the open row in
        /// docs/reference/decisions.md). 3 is kept, so the close bank stays ~23 visible clouds and not one. The
        /// untried row is block 2 with the width/stand pair re-fitted DOWN to the shorter 8 m lane (GasPuffWide
        /// 1.2 / GasPuffStand 0.83): no capture has measured it.
        /// (connected-component counts do not interpolate: re-measure after any change here.)</summary>
        public const int GasPuffBlock = 3;
        /// <summary>Cells one OVERVIEW card stands for (the owner, 2026-10-22: the overview is "still the old squash
        /// blanket"). The merged-out draw used the zoom step itself (2), so cards stood 8 m apart while each is
        /// 11-15 m wide: they overlapped into one blanket whose thin fringe cells each left a 50-150 px speck
        /// (o240_blob_top in -g1-g1/probe.json: one 4275 px blob, then a tail of specks). The card's own width
        /// still multiplies by the local step, so a bigger block widens only the GAP. Swept by capture, one
        /// GasBankStills gas + plate pass each at TW_GAS_WAIT_FRAMES 200:
        ///   2 (the old blanket) -> o240_cloud_count 21/15/20, cloud_share 0.87, green_area_frac 0.0094:
        ///         one 4275 px blob plus a tail of 50-150 px fringe specks - the owner's complaint
        ///    3 -> 34 clouds, share 0.7515, top blob 1173 px: cards still touch, so it is a smaller blanket
        ///    4 -> 29 clouds, share 0.6231, area 0.0028: the blanket is gone, every card stands alone
        ///    6 -> 19 clouds, share 0.5933, area 0.0021
        ///    8 -> 17 clouds, share 0.6568, area 0.0020
        ///   16 -> 13 clouds, share 0.5888, o240_green_px 2055 (count does NOT fall with the block past 12)
        ///   12 -> 11 clouds, share 0.4777, area 0.0010: KEPT, the fewest clouds of the sweep, so a ceiling
        ///         of 15 sits outside the old 15-21 spread with room, and still well over a 3-cloud floor.
        ///         Coverage pays for it (0.0094 -> 0.0010) and the o240 area/share bars are re-set: the owner
        ///         asked for clouds with gaps, not for coverage.
        /// (connected-component counts do not interpolate: re-measure after any change here.)</summary>
        public const int GasCloudBlock = 12;
        /// <summary>How much wider than a cell-block's own 5-7 m an OVERVIEW card is drawn (the owner, 2026-10-22:
        /// "At the overview the chlorine bank is invisible" - `o240_visible_clouds` read 0, 0 and 0 on three
        /// captures of the merged-out build, docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2-g1-g2's
        /// probe.json, because the biggest overview blob was 119 px against a tank-square of ~323 px. Before this,
        /// the overview width multiplied by the local step (2) itself, so GasCloudBlock could only widen the GAP,
        /// never the card. Cards stand GasCloudBlock 12 x FieldCellSize 4 = 48 m apart, so there is room to grow a
        /// card to ~35 m before they touch again. Swept by capture, one GasBankStills gas + plate pass each at
        /// TW_GAS_WAIT_FRAMES 200, measured against `o240_visible_clouds` (need 4 or more) and `o240_cloud_count`
        /// (stay under 15):
        ///   3.0 -> visible_clouds 0, cloud_count 16: too small to clear the tank-square, and already over the
        ///         count ceiling
        ///   4.0 -> visible_clouds 5, cloud_count 16: clears the visible floor but still over the count ceiling
        ///   5.0 -> KEPT: visible_clouds 10, cloud_count 13 - clears both, with room (the owner's "cloud a
        ///         player can see" in every frame)
        /// (connected-component counts do not interpolate: re-measure after any change here.)</summary>
        public const float GasCloudWide = 5.0f;
        /// <summary>How tall a close gas card is drawn against its width (the owner, 2026-10-21: a puff with a rim and a
        /// height, like cards-smoke.jpg). -g1's 0.32 was fitted to 774 cards 4 m apart (GasBankCards' close sheet)
        /// and does not carry over to GasPuffBlock's ~86 cards 12 m apart: measured here, not guessed
        /// (docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g1/README.md), at the block's own frame window
        /// (GasPuffFirst..GasPuffLast below):
        ///   1.0 -> green_height_frac 0.0786, green_low_share 0.3299: clears both bars with almost no room (the
        ///         old sheet build's own 0.0768 ceiling-adjacent reading)
        ///   1.5 -> 0.2063 / 0.1437: over the 0.15 ceiling
        ///   2.0 -> 0.1730 / 0.2171: still over the ceiling, under the 0.30 low floor
        ///   1.2, full SmokeBank window -> 0.1057 / 0.1941: height is in range but low_share breaks the floor
        ///   1.2, GasPuffFirst..GasPuffLast -> 0.095 / 0.346: kept, room at both ends of every bar
        /// (height_frac does not move monotonically with stand: largest_blob picks one connected puff out of a
        /// sparse, gapped field, so which puff it keeps - and that puff's own screen-space height - can jump
        /// between nearby stands; re-measure after any further change here, do not interpolate).
        /// 2026-10-22: re-fitted from 1.2 to 0.50 in the same row as GasPuffWide 2.0, so the drawn height holds
        /// (width 2.0x, stand 1.2 -> 0.50: a 7 m block draws 14 m wide and 7 m tall, as before).</summary>
        public const float GasPuffStand = 0.50f;
        /// <summary>How much wider than a cell-block's own 5-7 m a close card is drawn (the owner, 2026-10-22:
        /// "separate flat green tree-cards"). Close cards stand GasPuffBlock 3 x FieldCellSize 4 = 12 m apart, so at
        /// 1.0 every card is a lone tree; past ~1.8 they overlap into one mass. Height stays width * GasPuffStand,
        /// so the stand comes down as this goes up and the drawn height holds at today's ~7 m. Measured, not
        /// guessed, at wait 200 with GasPuffStand re-fitted in the same row (the goal's own bars in brackets:
        /// green_height_frac 0.08-0.15, green_low_share >= 0.30, green_rise_m >= 2.4):
        ///   1.0 / 1.2  -> height 0.0950, low 0.3462, rise 2.575, t1_top_share 0.0803: the old build, 39 lone cards
        ///   1.8 / 0.67 -> 0.1281 / 0.1937 / 3.964, top 0.3739: merged, but low_share breaks the 0.30 floor
        ///   1.9 / 0.55 -> 0.0872 / 0.2954 / 2.765, top 0.2879: a hair under the floor
        ///   2.0 / 0.50 -> 0.0836 / 0.3045 / 2.653, top 0.2908: KEPT - the widest merge the low_share floor allows
        ///   2.1 / 0.47 -> 0.0851 / 0.3018 / 2.697, top 0.2904: the same look, less room at the floor
        ///   2.2 / 0.50 -> 0.1300 / 0.1961 / 3.686, top 0.4306: one mass, floor broken
        ///   2.4 / 0.50 -> 0.1320 / 0.1869 / 3.746, top 0.4244   2.4 / 0.42 -> 0.1148 / 0.2114 / 3.254
        ///   2.4 / 0.35 -> 0.0666 / 0.3413 / 2.151: under the 0.08 height floor and the 2.4 m rise
        ///   2.8 / 0.38 -> 0.1434 / 0.1784 / 4.056   3.0 / 0.40 -> 0.1776 / 0.1470 / 5.149: over the 0.15 ceiling
        /// The merge and the low_share floor pull against each other: once the cards merge into one mass it reaches
        /// back into the field, and the far cards' pixels sit high on screen, so the share under a man's head
        /// falls. 2.0 is where both hold. t1_cloud_count barely moves at any width (35-48 against the old 39-44),
        /// so it is a diagnostic and not a bar (component counts do not interpolate: re-measure after any change
        /// here, never interpolate).</summary>
        public const float GasPuffWide = 2.0f;
        public static readonly Color GasBankNight = new Color(0.74f, 0.80f, 0.34f), GasBankDay = new Color(0.50f, 0.64f, 0.12f);

        /// <summary>The opacity of a gas cell's card at a thickness (0 to 1).</summary>
        public static float GasBankAlpha(float thick) => Mathf.Lerp(GasBankThin, GasBankFull, Mathf.Clamp01(thick));

        public const float SmokeBankFirst = 12f, SmokeBankLast = 20.5f; // the mound grown (the firebooks cut stops at 21)
        /// <summary>The close gas puff's own frame window, inside SmokeBankFirst..SmokeBankLast: the mound's early,
        /// wider frames, not its tall late ones. Measured (this unit): the full SmokeBank window at GasPuffStand
        /// 1.2 read green_low_share 0.19 against the owner's 0.30 floor, so the puff's cross-section at the frames
        /// it boils between is squatter here, not just shorter.</summary>
        const float GasPuffFirst = 12f, GasPuffLast = 16f;

        /// <summary>A card's worth of field: its hash, how thick, and where it stands.</summary>
        struct BankCell { public uint Hash; public float Thick, X, Z, Ground; }

        /// <summary>One field's standing cells, found once a sim tick at one merge step.</summary>
        sealed class BankCache { public readonly List<BankCell> Cells = new List<BankCell>(1024); public uint Tick = uint.MaxValue; public int Step; }
        readonly BankCache gasBank = new BankCache(), smokeBank = new BankCache();

        /// <summary>Cells a card stands for at this zoom: 1, or 2 (a 2 x 2 block) from the overview.</summary>
        public static int BankStep(float zoom) => zoom >= BankMergeZoom ? 2 : 1;

        /// <summary>The frame a cell's bank shows: boiled slowly back and forth between two frames of the book.</summary>
        public static float BankFrame(float first, float last, float slow) => first + (last - first) * (0.5f + 0.5f * Mathf.Sin(slow));

        int CurrentBankStep()
        {
            return BankStep(ViewEye.Zoom);
        }

        /// <summary>Refill a field's cells if the tick or the step has moved. floor: the least a cell must hold to stand;
        /// full: what counts as thick; jitter: how far off its centre a card may stand; salt: the cell hash's primes.</summary>
        void Refresh(BankCache cache, Unity.Collections.NativeArray<float> field, int width, int length, int step, float floor, float full, float jitter, int p1, int p2)
        {
            uint tick = Host.Local.World.Tick;
            if (cache.Tick == tick && cache.Step == step) return;
            cache.Tick = tick; cache.Step = step; cache.Cells.Clear();
            float cs = MapData.FieldCellSize;
            for (int z = 0; z < length; z += step)
            for (int x = 0; x < width; x += step)
            {
                float c = 0f;
                for (int dz = 0; dz < step && z + dz < length; dz++)
                for (int dx = 0; dx < step && x + dx < width; dx++)
                    c = Mathf.Max(c, field[(z + dz) * width + x + dx]);
                if (c < floor) continue;
                uint h = (uint)(x * p1 ^ z * p2);
                float h1 = (h & 1023) / 1023f, h2 = ((h >> 10) & 1023) / 1023f;
                float wx = (x + 0.5f * step) * cs + (h1 - 0.5f) * jitter, wz = (z + 0.5f * step) * cs + (h2 - 0.5f) * jitter;
                cache.Cells.Add(new BankCell { Hash = h, Thick = Mathf.Clamp01(c / full), X = wx, Z = wz, Ground = RenderGround.Sample(Host.Local.Map, wx, wz) });
            }
        }

        /// <summary>L07/L08 release: at each source of a gas cloud a GasVent (a low jet spreading) for its 32 frames; at each
        /// source of a smoke screen the SmokeBank's own early frames (the mound growing out of the canister). Close only:
        /// from the overview the bank itself is the read.</summary>
        void OnBankSpawned(SimEvent e)
        {
            if (recipes < 0.5f || books == null || !books.Ready) return;
            if (CurrentBankStep() > 1) return;
            Vector3 at = (Vector3)e.Pos; at.y = RenderGround.Sample(Host.Local.Map, at.x, at.z) - 0.1f;
            var kind = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored;
            if (e.Type == SimEventType.GasCloudSpawned) books.Add(FlipbookFx.Book.GasVent, at, 4f, 32f / 12f, kind, alpha: 0.8f);
            else books.Add(FlipbookFx.Book.SmokeBank, at, 5f, 21f / 12f, kind, alpha: 0.7f);
        }

        void DrawGasBank(GasSmokeSystem gas, float now, Bounds bounds)
        {
            int step = CurrentBankStep();
            Refresh(gasBank, gas.Gas, gas.Width, gas.Length, step == 1 ? GasPuffBlock : GasCloudBlock, 0.8f, 14f, 2.4f, 73856093, 19349663);
            gasCards.Clear();
            foreach (var b in gasBank.Cells)
            {
                float h2 = ((b.Hash >> 10) & 1023) / 1023f, h3 = ((b.Hash >> 20) & 1023) / 1023f;
                float slow = now * 0.22f + h3 * 6.2832f;
                // the block widens the GAP between cards (GasPuffBlock below); GasPuffWide widens the close card
                // itself over that gap, so the cards overlap into one cloud instead of standing as separate trees.
                float width = (5f + 2f * b.Thick) * (0.9f + 0.2f * h2) * (step == 1 ? GasPuffWide : GasCloudWide);
                var kind = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (((b.Hash >> 5) & 1) == 0 ? FlipbookFx.Kind.Mirror : 0);
                // close (step 1): the standing puff, its drawing reaching the cell's floor, so the card sits at the
                // smoke bank's anchor and not GasBankSquash's sunk one. From the overview out: today's squashed blanket.
                if (step == 1) gasCards.Add(FlipbookFx.Pack(new Vector3(b.X, b.Ground - 0.2f, b.Z), width, width * GasPuffStand, BankFrame(GasPuffFirst, GasPuffLast, slow), 1f, 1f, 0f, kind, GasBankAlpha(b.Thick)));
                else gasCards.Add(FlipbookFx.Pack(new Vector3(b.X, b.Ground - 0.35f, b.Z), width, width * GasBankSquash, BankFrame(GasBankFirst, GasBankLast, slow), 1f, 1f, 0f, kind, GasBankAlpha(b.Thick)));
            }
            books.DrawPacked(step == 1 ? FlipbookFx.Book.GasPuff : FlipbookFx.Book.GasBank, gasCards, bounds);
        }

        void DrawSmokeBank(GasSmokeSystem gas, float now, Bounds bounds)
        {
            int step = CurrentBankStep();
            Refresh(smokeBank, gas.Smoke, gas.Width, gas.Length, step, 1.5f, 20f, 2.2f, 83492791, 29349663);
            smokeCards.Clear();
            foreach (var b in smokeBank.Cells)
            {
                float h2 = ((b.Hash >> 10) & 1023) / 1023f, h3 = ((b.Hash >> 20) & 1023) / 1023f;
                float slow = now * 0.18f + h3 * 6.2832f;
                float width = (6f + 2f * b.Thick) * (0.9f + 0.2f * h2) * step;
                var kind = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (((b.Hash >> 5) & 1) == 0 ? FlipbookFx.Kind.Mirror : 0);
                // a white band is the most readable thing on a night field: at most 0.7, so the HUD rings show through it
                smokeCards.Add(FlipbookFx.Pack(new Vector3(b.X, b.Ground - 0.2f, b.Z), width, width, BankFrame(SmokeBankFirst, SmokeBankLast, slow), 1f, 1f, 0f, kind, Mathf.Min(0.7f, 0.3f + 0.45f * b.Thick)));
            }
            if (smokeCards.Count > 0) books.DrawPacked(FlipbookFx.Book.SmokeBank, smokeCards, bounds);
        }
    }
}
