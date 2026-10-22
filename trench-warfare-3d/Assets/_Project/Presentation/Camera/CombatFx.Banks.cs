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
        // GasBankThin raised r2-3: with Erode the shader cuts the drawing at threshold = (1 - cardAlpha) * (0.45 + 0.55 * edge);
        // the sheet carries mean ink 0.167-0.183 in the frames the bank draws, so at 0.6 the threshold (0.18-0.40) cut the
        // whole drawing away and the bank rendered as flat olive smears, not a cloud (harsh critic r1, 22/100).
        public const float GasBankThin = 0.95f, GasBankFull = 1f;
        /// <summary>How flat a gas card is drawn: the sheet fills the bottom third, so a 7 m card squashed to 0.75 tops out at 1.75 m - gas pools, it does not stand over a man.</summary>
        public const float GasBankSquash = 0.75f;
        /// <summary>The bank as a volume (r2-3 g1-g1-g1), three card shapes over the same card count: the bed stretches
        /// from GasBankSquash at a thin cell to GasBankCore at the thick heart; a rim cell draws a narrower, broken lip;
        /// the per-block crown stands over the bed so the bank has a height and a rim, not one flat quilt.</summary>
        public const float GasBankCore = 1.15f, GasBankRim = 0.95f, GasBankCrown = 0.90f;
        /// <summary>The nominal card a cell draws, before its hash widens it: the crown's lift is measured off this.</summary>
        public const float GasBankCardWidth = 7f;
        /// <summary>How tall a bed card is at a thickness (0 to 1).</summary>
        public static float GasBedHeight(float width, float thick) => width * Mathf.Lerp(GasBankSquash, GasBankCore, Mathf.Clamp01(thick));
        /// <summary>How high the sheet's drawing reaches on a card of this height: it fills the bottom third.</summary>
        public static float GasDrawnTop(float height) => height / 3f;
        /// <summary>How far a block's crown card is lifted off the ground: half the bed's drawn top there.</summary>
        public static float GasCrownLift(float thick, int step) => 0.5f * GasDrawnTop(GasBedHeight(GasBankCardWidth * step, thick));
        /// <summary>How high the crown's drawing reaches over the ground at a block thickness.</summary>
        public static float GasCrownTop(float thick, int step) => GasCrownLift(thick, step) + GasDrawnTop(8f * step * GasBankCrown);
        public static readonly Color GasBankNight = new Color(0.78f, 0.86f, 0.22f), GasBankDay = new Color(0.50f, 0.64f, 0.12f);

        /// <summary>The opacity of a gas cell's card at a thickness (0 to 1).</summary>
        public static float GasBankAlpha(float thick) => Mathf.Lerp(GasBankThin, GasBankFull, Mathf.Clamp01(thick));

        const float SmokeBankFirst = 12f, SmokeBankLast = 20.5f; // the mound grown (the firebooks cut stops at 21)

        /// <summary>A card's worth of field: its hash, how thick, and where it stands.</summary>
        struct BankCell { public uint Hash; public float Thick, X, Z, Ground; public bool Edge; }

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

        /// <summary>The most a step x step block of the field holds; 0 when the block is off the field.</summary>
        static float BlockMax(Unity.Collections.NativeArray<float> field, int width, int length, int x, int z, int step)
        {
            if (x < 0 || z < 0 || x >= width || z >= length) return 0f;
            float c = 0f;
            for (int dz = 0; dz < step && z + dz < length; dz++)
            for (int dx = 0; dx < step && x + dx < width; dx++)
                c = Mathf.Max(c, field[(z + dz) * width + x + dx]);
            return c;
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
                float c = BlockMax(field, width, length, x, z, step);
                if (c < floor) continue;
                // where the bank ends: a neighbour block under the floor (or off the field) makes this cell a rim cell,
                // so DrawGasBank can draw a broken lip there instead of one more flat pad
                bool edge = BlockMax(field, width, length, x - step, z, step) < floor
                         || BlockMax(field, width, length, x + step, z, step) < floor
                         || BlockMax(field, width, length, x, z - step, step) < floor
                         || BlockMax(field, width, length, x, z + step, step) < floor;
                uint h = (uint)(x * p1 ^ z * p2);
                float h1 = (h & 1023) / 1023f, h2 = ((h >> 10) & 1023) / 1023f;
                float wx = (x + 0.5f * step) * cs + (h1 - 0.5f) * jitter, wz = (z + 0.5f * step) * cs + (h2 - 0.5f) * jitter;
                cache.Cells.Add(new BankCell { Hash = h, Thick = Mathf.Clamp01(c / full), X = wx, Z = wz, Ground = RenderGround.Sample(Host.Local.Map, wx, wz), Edge = edge });
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

        /// <summary>A 2 x 2 block of standing gas cells, folded into one wider body card (r2-3 step 7: the per-cell
        /// cards alone left gaps the probe's texture_var_drop read as the bare ground showing through).</summary>
        struct GasBlock { public int Count; public float X, Z, Ground, Thick; public uint Hash; }
        readonly Dictionary<long, GasBlock> gasBlocks = new Dictionary<long, GasBlock>(256);

        void DrawGasBank(GasSmokeSystem gas, float now, Bounds bounds)
        {
            int step = CurrentBankStep();
            Refresh(gasBank, gas.Gas, gas.Width, gas.Length, step, 0.8f, 14f, 2.4f, 73856093, 19349663);
            gasCards.Clear();
            float blockSize = 2f * MapData.FieldCellSize * step;
            gasBlocks.Clear();
            foreach (var b in gasBank.Cells)
            {
                float h2 = ((b.Hash >> 10) & 1023) / 1023f, h3 = ((b.Hash >> 20) & 1023) / 1023f;
                float slow = now * 0.22f + h3 * 6.2832f;
                float width = (6f + 3f * b.Thick) * (0.9f + 0.2f * h2) * step;
                var kind = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (((b.Hash >> 5) & 1) == 0 ? FlipbookFx.Kind.Mirror : 0);
                // bed inside the bank, lip at its edge: the rim card is narrower, a touch taller and rolled off true by
                // its own hash, so the bank ends in a broken lip and not on a tile line
                float h4 = ((b.Hash >> 15) & 1023) / 1023f;
                float cw = b.Edge ? width * 0.85f : width;
                float ch = b.Edge ? cw * GasBankRim : GasBedHeight(cw, b.Thick);
                float roll = b.Edge ? (h4 - 0.5f) * 2f * 8f * Mathf.Deg2Rad : 0f;
                gasCards.Add(FlipbookFx.Pack(new Vector3(b.X, b.Ground - 0.10f, b.Z), cw, ch, BankFrame(GasBankFirst, GasBankLast, slow), 1f, 1f, roll, kind, GasBankAlpha(b.Thick)));

                long key = ((long)Mathf.FloorToInt(b.X / blockSize) << 32) ^ (uint)Mathf.FloorToInt(b.Z / blockSize);
                gasBlocks.TryGetValue(key, out var blk);
                blk.Count++; blk.X += b.X; blk.Z += b.Z; blk.Ground += b.Ground; blk.Thick = Mathf.Max(blk.Thick, b.Thick); blk.Hash ^= b.Hash;
                gasBlocks[key] = blk;
            }
            foreach (var kv in gasBlocks)
            {
                var blk = kv.Value;
                // the crown is the volume: only a full block (three cells of four, half thick) gets one, and it stands
                // lifted over the bed instead of lying in it as step 7's flat 11 m pad did
                if (blk.Count < 3 || blk.Thick < 0.5f) continue;
                float cx = blk.X / blk.Count, cz = blk.Z / blk.Count, ground = blk.Ground / blk.Count;
                float h3 = ((blk.Hash >> 20) & 1023) / 1023f;
                float slow = now * 0.22f + h3 * 6.2832f;
                float width = 8f * step;
                var kind = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (((blk.Hash >> 5) & 1) == 0 ? FlipbookFx.Kind.Mirror : 0);
                float y = ground - 0.10f + GasCrownLift(blk.Thick, step);
                gasCards.Add(FlipbookFx.Pack(new Vector3(cx, y, cz), width, width * GasBankCrown, BankFrame(GasBankFirst, GasBankLast, slow), 1f, 1f, 0f, kind, GasBankAlpha(blk.Thick)));
            }
            books.DrawPacked(FlipbookFx.Book.GasBank, gasCards, bounds);
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
