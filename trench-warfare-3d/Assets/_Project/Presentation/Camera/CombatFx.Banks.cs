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
        // How clearly the bank reads: a thin cell's card and a full one's. One card is a haze now, not a tile - three
        // stacked cards reach 1-(1-0.62)^3 = 0.945, so the bank is volume and not stripes. Less pale than the sheet was
        // (0.14/0.5), because "pale green stripes" is the alpha as much as the overlap.
        // Its colour by day is deeper than the sheet's so it is green ON snow
        public const float GasBankThin = 0.19f, GasBankFull = 0.62f;
        /// <summary>How tall a gas card's painted band is (m): the sheet fills the bottom third of the card, so every card,
        /// thin or wide, paints the same 1.72 m of gas - under a 1.8 m man's head, and tall enough that at the T1 pose one
        /// row of cards reaches the next row's and no bare ground shows between them (a share of the width left the thin
        /// cards at 1.09 m, two fifths of the 4 m row pitch bare).</summary>
        public const float GasBankBandM = 1.78f;
        /// <summary>How flat a card of this width is drawn, so its bottom third is <see cref="GasBankBandM"/> tall.</summary>
        public static float GasBankSquashFor(float width) => 3f * GasBankBandM / width;
        /// <summary>The full DEPTH span a gas card's centre may fill (m): one full field cell (<see cref="MapData.FieldCellSize"/>),
        /// filled in two strata by <see cref="GasBankZOffset"/> rather than at random - at the old 1.2 m jitter every card
        /// sat within 0.6 m of its row line, so the 4 m row lattice still read as bands, and a random full-cell jitter
        /// covered the depth only on average, so the worst screen row still read a quarter under the bank's median.</summary>
        public const float GasBankJitterM = 4f;
        /// <summary>How far off its cell's centre a gas card may stand SIDEWAYS (m). Card widths, roll and mirror already
        /// break the X lattice, so a small amount is enough; a full-cell jitter in X pulled card centres apart and split
        /// the painted bank into separate patches.</summary>
        public const float GasBankJitterXM = 1.2f;

        /// <summary>Where in its cell a card stands in depth (m off the cell centre): even card columns in the near half,
        /// odd columns in the far half, each filling its own 2 m stratum. The union still fills the whole 4 m cell
        /// uniformly, but never by chance: a neighbouring column always has a card core over the other's soft band edge.
        /// A card is four to five columns wide, so every screen x sees both parities.</summary>
        public static float GasBankZOffset(int x, int step, float h2)
            => ((((x / step) & 1) == 0) ? -GasBankJitterM / 4f : GasBankJitterM / 4f) + (h2 - 0.5f) * GasBankJitterM / 2f;

        /// <summary>How many field blocks a gas card's thickness is read from: its own and the eight around it.</summary>
        public const int GasBankNeighbourhood = 9;
        /// <summary>A gas card's load: the mean of the nine block maxima around it (its own plus <paramref name="ringSum"/>,
        /// the eight neighbours; off the map counts as 0). A card used to read its own 4 m cell alone, so at the cloud's
        /// edge, where the field decays cell by cell, the outermost ring of cards drew right at the probe's visibility
        /// bar: that marginal paint broke into blotches and each blotch counted as its own patch (28 / 24 / 18 for one
        /// bank), and the count swung between captures because the fringe was re-diced every frame. With the mean of
        /// nine the body keeps its thickness (nine times the same value is that value), the old fringe ring falls under
        /// the stand floor and goes, and the new outermost ring stands where the gas is about twice the floor - well
        /// above the bar, so it reads as one plate. It also pulls the bank's width back by about one 4 m ring.</summary>
        public static float GasBankLoad(float self, float ringSum) => (self + ringSum) / GasBankNeighbourhood;

        public static readonly Color GasBankNight = new Color(0.74f, 0.80f, 0.34f), GasBankDay = new Color(0.50f, 0.64f, 0.12f);

        /// <summary>The opacity of a gas cell's card at a thickness (0 to 1).</summary>
        public static float GasBankAlpha(float thick) => Mathf.Lerp(GasBankThin, GasBankFull, Mathf.Clamp01(thick));

        /// <summary>How wide a gas cell's card is drawn (m): 16.2-24.2 m on the 4 m field grid, so a card spans four to
        /// five cells, neighbours overlap four to five deep, and no single card's silhouette is the shape you see. More
        /// cards reach every screen row, so the worst row sits less far under the median and the sub-threshold holes
        /// between card skirts (which read as separate patches) close. Same card count.</summary>
        public static float GasBankWidth(float thick, float hash01, int step) => (18f + 4f * thick) * (0.9f + 0.2f * hash01) * step;

        public const float SmokeBankFirst = 12f, SmokeBankLast = 20.5f; // the mound grown (the firebooks cut stops at 21)

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
        /// full: what counts as thick; jitterX / jitterZ: how far off its centre a card may stand sideways and in depth;
        /// stratifyZ: place the depth offset in two interleaved strata (<see cref="GasBankZOffset"/>) instead of at
        /// random; smooth: read a cell's load from its neighbourhood (<see cref="GasBankLoad"/>) instead of its own
        /// block; salt: the cell hash's primes.</summary>
        void Refresh(BankCache cache, Unity.Collections.NativeArray<float> field, int width, int length, int step, float floor, float full, float jitterX, float jitterZ, bool stratifyZ, bool smooth, int p1, int p2)
        {
            uint tick = Host.Local.World.Tick;
            if (cache.Tick == tick && cache.Step == step) return;
            cache.Tick = tick; cache.Step = step; cache.Cells.Clear();
            float cs = MapData.FieldCellSize;
            // one block's worth of field at (bx, bz): the max over the step x step cells it merges, 0 off the map
            float Block(int bx, int bz)
            {
                if (bx < 0 || bz < 0 || bx >= width || bz >= length) return 0f;
                float m = 0f;
                for (int dz = 0; dz < step && bz + dz < length; dz++)
                for (int dx = 0; dx < step && bx + dx < width; dx++)
                    m = Mathf.Max(m, field[(bz + dz) * width + bx + dx]);
                return m;
            }
            for (int z = 0; z < length; z += step)
            for (int x = 0; x < width; x += step)
            {
                float c = Block(x, z);
                if (smooth)
                {
                    float ring = 0f;
                    for (int dz = -1; dz <= 1; dz++)
                    for (int dx = -1; dx <= 1; dx++)
                        if (dx != 0 || dz != 0) ring += Block(x + dx * step, z + dz * step);
                    c = GasBankLoad(c, ring);
                }
                if (c < floor) continue;
                uint h = (uint)(x * p1 ^ z * p2);
                float h1 = (h & 1023) / 1023f, h2 = ((h >> 10) & 1023) / 1023f;
                float wx = (x + 0.5f * step) * cs + (h1 - 0.5f) * jitterX;
                float wz = (z + 0.5f * step) * cs + (stratifyZ ? GasBankZOffset(x, step, h2) : (h2 - 0.5f) * jitterZ);
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
            Refresh(gasBank, gas.Gas, gas.Width, gas.Length, step, 0.8f, 8f, GasBankJitterXM, GasBankJitterM, true, true, 73856093, 19349663);
            gasCards.Clear();
            foreach (var b in gasBank.Cells)
            {
                float h2 = ((b.Hash >> 10) & 1023) / 1023f, h3 = ((b.Hash >> 20) & 1023) / 1023f;
                float slow = now * 0.22f + h3 * 6.2832f;
                float width = GasBankWidth(b.Thick, h2, step);
                var kind = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (((b.Hash >> 5) & 1) == 0 ? FlipbookFx.Kind.Mirror : 0);
                // a zebra of cards is what one hard silhouette per cell looked like: break the top line, the brightness
                // (lit core, dull skirt) and the roll (+-12 degrees) by the cell's hash so no two card edges line up
                // every card sits on the ground at the same height: the old sink and +-0.25 m jitter threw away 0.6 m of
                // the band and pulled the rows apart; the roll and the frame boil already break the top line
                gasCards.Add(FlipbookFx.Pack(new Vector3(b.X, b.Ground - 0.08f, b.Z), width, width * GasBankSquashFor(width), BankFrame(GasBankFirst, GasBankLast, slow), 1f, 0.75f + 0.45f * b.Thick, (h2 - 0.5f) * 0.42f, kind, GasBankAlpha(b.Thick)));
            }
            books.DrawPacked(FlipbookFx.Book.GasBank, gasCards, bounds);
        }

        void DrawSmokeBank(GasSmokeSystem gas, float now, Bounds bounds)
        {
            int step = CurrentBankStep();
            Refresh(smokeBank, gas.Smoke, gas.Width, gas.Length, step, 1.5f, 20f, 2.2f, 2.2f, false, false, 83492791, 29349663);
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
