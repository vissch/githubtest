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
        // How clearly the bank reads: a thin cell's card and a full one's, and its colour by day, deeper than the
        // sheet's so it is green ON snow. These two are a plain TRANSLUCENCY knob, nothing else. Read the shader:
        // Flipbook_URP.shader:123-126 cuts the drawing at threshold = gone * (0.45 + 0.55 * edge) with
        // gone = 1 - tone.x, and tone.x is the card's FADE (FlipbookFx.Pack puts fade in m01, opacity in m21).
        // DrawGasBank packs fade = 1f, so gone = 0, the threshold is 0 and nothing is eroded away at any opacity;
        // the opacity only multiplies alpha. r2-3 held these at 0.95-1.0 to "clear the erode threshold" - that was
        // the wrong knob, and the result was a blanket of opaque, equal pads (the owner: "a carpet of flat green
        // pads"). Lowered r2-3-g1: the fringe lets mud, wire and men through, the core still covers.
        // Steeper, not stronger (r2-3-g1 step 8): the per-cell card is now only a RIM wisp outside the mass blocks,
        // and it is what paved the frame, so the thin end drops 0.30 -> 0.12 and all but vanishes; the mass card
        // carries the read, so the full end rises 0.80 -> 0.85. 0.85 still clears
        // BurstRecipeTests.TheGasBankIsTranslucent_NotAnOpaquePad's "even the core is not opaque" (<= 0.85).
        public const float GasBankThin = 0.12f, GasBankFull = 0.85f;
        /// <summary>How flat a gas card is drawn: the sheet fills the bottom third, so a 7 m card squashed to 0.75 tops out at 1.75 m - gas pools, it does not stand over a man.</summary>
        public const float GasBankSquash = 0.75f;
        public static readonly Color GasBankNight = new Color(0.78f, 0.86f, 0.22f), GasBankDay = new Color(0.50f, 0.64f, 0.12f);

        /// <summary>The opacity of a gas cell's card at a thickness (0 to 1).</summary>
        public static float GasBankAlpha(float thick) => Mathf.Lerp(GasBankThin, GasBankFull, Mathf.Clamp01(thick));

        /// <summary>Does a 4 x 4 cell block of standing gas earn ONE mass card? A crowd that is also thick does; a
        /// lucky thick cell on its own, or a wide thin haze, is the cloud's feathered rim and gets no mass.</summary>
        public static bool GasMassBlock(int count, float thick) => count >= 4 && thick >= 0.35f;

        /// <summary>How wide that one mass card is drawn: the more cells crowd the block the broader the mass, up to
        /// a 28 m cap so one block never covers the frame. 11 m / 28 m, not the plan's 9 m / 22 m: the first
        /// reshoot read t1_changed_frac 0.053 against the owner's 0.08, which is the plan's own named risk (one
        /// mass covers less frame than a carpet did) and its own remedy - the bank is meant to be big, just not
        /// tiled.</summary>
        public static float GasMassWidth(int count, int step) => Mathf.Min(28f, 11f + 1.2f * count) * step;

        const float SmokeBankFirst = 12f, SmokeBankLast = 20.5f; // the mound grown (the firebooks cut stops at 21)

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

        /// <summary>A 4 x 4 block of standing gas cells, folded into ONE mass card (r2-3 step 7: the per-cell cards
        /// alone left gaps the probe's texture_var_drop read as the bare ground showing through). Only a block that
        /// is both crowded and thick (GasMassBlock) gets one; elsewhere the cells draw a rim wisp each, so the cloud
        /// is one mass with a feathered edge and not a blanket.</summary>
        struct GasBlock { public int Count; public float X, Z, Ground, Thick; public uint Hash; }
        readonly Dictionary<long, GasBlock> gasBlocks = new Dictionary<long, GasBlock>(256);

        void DrawGasBank(GasSmokeSystem gas, float now, Bounds bounds)
        {
            int step = CurrentBankStep();
            Refresh(gasBank, gas.Gas, gas.Width, gas.Length, step, 0.8f, 14f, 2.4f, 73856093, 19349663);
            gasCards.Clear();
            // 16 m blocks, not 8 m: one mass card stands for a 4 x 4 cell crowd, so the cloud's interior is ONE
            // mass and not a card per cell (the owner: "a carpet of flat green pads").
            float blockSize = 4f * MapData.FieldCellSize * step;
            gasBlocks.Clear();
            // pass 1: the blocks only. No cards here - which cells get one depends on their block's verdict.
            foreach (var b in gasBank.Cells)
            {
                long key = BlockKey(b.X, b.Z, blockSize);
                gasBlocks.TryGetValue(key, out var blk);
                blk.Count++; blk.X += b.X; blk.Z += b.Z; blk.Ground += b.Ground; blk.Thick = Mathf.Max(blk.Thick, b.Thick); blk.Hash ^= b.Hash;
                gasBlocks[key] = blk;
            }
            // pass 2: a rim wisp per cell, and ONLY where the cell's block earned no mass card - inside the mass the
            // per-cell cards were pure paving. Narrow and faint: a feathered edge, not a pad.
            foreach (var b in gasBank.Cells)
            {
                if (gasBlocks.TryGetValue(BlockKey(b.X, b.Z, blockSize), out var own) && GasMassBlock(own.Count, own.Thick)) continue;
                float h1 = (b.Hash & 1023) / 1023f, h2 = ((b.Hash >> 10) & 1023) / 1023f, h3 = ((b.Hash >> 20) & 1023) / 1023f;
                float slow = now * 0.22f + h3 * 6.2832f;
                float width = (4f + 2f * b.Thick) * (0.7f + 0.6f * h2) * step;
                var kind = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (((b.Hash >> 5) & 1) == 0 ? FlipbookFx.Kind.Mirror : 0);
                gasCards.Add(FlipbookFx.Pack(new Vector3(b.X, b.Ground - 0.10f + 0.25f * h1, b.Z), width, width * GasBankSquash * (0.6f + 0.4f * b.Thick), BankFrame(GasBankFirst, GasBankLast, slow), 1f, 1f, 0f, kind, GasBankAlpha(b.Thick) * 0.7f));
            }
            // pass 3: one card for each mass block, at the mean of its cells, as wide as the crowd makes it.
            foreach (var kv in gasBlocks)
            {
                var blk = kv.Value;
                if (!GasMassBlock(blk.Count, blk.Thick)) continue;
                float cx = blk.X / blk.Count, cz = blk.Z / blk.Count, ground = blk.Ground / blk.Count;
                float h3 = ((blk.Hash >> 20) & 1023) / 1023f;
                float slow = now * 0.22f + h3 * 6.2832f;
                float width = GasMassWidth(blk.Count, step);
                var kind = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (((blk.Hash >> 5) & 1) == 0 ? FlipbookFx.Kind.Mirror : 0);
                gasCards.Add(FlipbookFx.Pack(new Vector3(cx, ground - 0.10f, cz), width, width * GasBankSquash, BankFrame(GasBankFirst, GasBankLast, slow), 1f, 1f, 0f, kind, GasBankAlpha(blk.Thick)));
            }
            books.DrawPacked(FlipbookFx.Book.GasBank, gasCards, bounds);
        }

        /// <summary>Which block a world x/z falls in, as one key.</summary>
        static long BlockKey(float x, float z, float blockSize)
            => ((long)Mathf.FloorToInt(x / blockSize) << 32) ^ (uint)Mathf.FloorToInt(z / blockSize);

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
