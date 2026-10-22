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
        const float GasBankFirst = 17f, GasBankLast = 28f;       // the bank at its full spread (its early frames are a puff)
        // How clearly the bank reads (film 2026-10-07: at 0.3-0.8 it was fainter than the old wisps at night and gone on snow):
        // a thin cell's card and a full one's, and its colour by day, deeper than the sheet's so it is green ON snow
        public const float GasBankThin = 0.6f, GasBankFull = 1f;
        /// <summary>How flat a gas card is drawn: the sheet fills the bottom third, so a 7 m card squashed to 0.55 tops out at 1.28 m - gas pools, it does not stand over a man.</summary>
        public const float GasBankSquash = 0.55f;
        /// <summary>Cells one close card stands for (the owner, 2026-10-21: "774 wide, low cards 4 m apart wash into
        /// a pale sheet" - -g1's own sidecars read gas_cards 774 at the close camera). One puff per 3 x 3 block of
        /// cells is ~86 cards 12 m apart: gaps between them are what makes a puff read as a puff, not a sheet.
        /// Only the close drawing (step == 1) uses this; the merged-out overview (step 2, GasBankSquash) is
        /// untouched, so O240 does not move.</summary>
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
        /// between nearby stands; re-measure after any further change here, do not interpolate).</summary>
        public const float GasPuffStand = 1.2f;
        public static readonly Color GasBankNight = new Color(0.74f, 0.80f, 0.34f), GasBankDay = new Color(0.50f, 0.64f, 0.12f);

        /// <summary>The opacity of a gas cell's card at a thickness (0 to 1).</summary>
        public static float GasBankAlpha(float thick) => Mathf.Lerp(GasBankThin, GasBankFull, Mathf.Clamp01(thick));

        const float SmokeBankFirst = 12f, SmokeBankLast = 20.5f; // the mound grown (the firebooks cut stops at 21)
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
            var cam = Camera.main;
            return BankStep(cam != null && cam.TryGetComponent<IZoomSource>(out var zs) ? zs.CurrentZoom : 0f);
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
                // the block widens the GAP between cards (GasPuffBlock below), not the card itself: a close puff
                // stays 5-7 m wide at any block size, so gaps between puffs are what makes a puff read.
                float width = (5f + 2f * b.Thick) * (0.9f + 0.2f * h2) * (step == 1 ? 1f : step);
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
