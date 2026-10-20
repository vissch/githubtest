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
        // How clearly the bank reads: a thin cell's card and a full one's. One card is a haze now, not a tile - three
        // stacked cards reach 1-(1-0.62)^3 = 0.945, so the bank is volume and not stripes. Less pale than the sheet was
        // (0.14/0.5), because "pale green stripes" is the alpha as much as the overlap.
        // Its colour by day is deeper than the sheet's so it is green ON snow
        public const float GasBankThin = 0.19f, GasBankFull = 0.62f;
        /// <summary>How much gas a 4 m cell must hold (after <see cref="GasBankLoad"/>) before it stands a card at all,
        /// and the load that counts as a full card. Two rounds proved the islands are not about WHICH ring is outermost:
        /// trimming the fringe to its neighbourhood's mean moved the blotches one ring in (28 / 24 / 18 -> 29 / 18 / 21
        /// patches) and lifting it with max(own, mean) made them worse again (35 / 30 / 24), because either way some ring
        /// still draws right at the paint threshold and breaks into separate patches. The floor removes that ring instead
        /// of re-weighting it: at the old 0.8 the thinnest standing card drew 0.19 + 0.43 * 0.1 = 0.233, a hair over
        /// <see cref="GasBankThin"/>; at 2.4 it draws 0.19 + 0.43 * 0.3 = 0.319, past half the bank's own core opacity
        /// (0.62 / 2 = 0.31), the same haze/paint line <c>TheGasBankEdgeIsLiftedIntoTheMass</c> uses. So every card that
        /// stands is paint, and a cell that would only haze does not stand.</summary>
        public const float GasBankStandFloor = 2.4f, GasBankFullM = 8f;
        /// <summary>How tall a gas card's painted band is (m) PER MERGED BLOCK: the sheet fills the bottom third of the
        /// card, so every card, thin or wide, paints the same 1.72 m of gas per cell it stands for - under a 1.8 m man's
        /// head at the close poses (step 1), and tall enough that at the T1 pose one row of cards reaches the next row's
        /// and no bare ground shows between them (a share of the width left the thin cards at 1.09 m, two fifths of the
        /// 4 m row pitch bare).</summary>
        public const float GasBankBandM = 1.78f;
        /// <summary>How flat a card of this width is drawn, so its bottom third is <see cref="GasBankBandM"/> tall per
        /// merged block (<paramref name="step"/>). At the overview a card's WIDTH doubles with the step but its painted
        /// depth did not: the band stayed 1.78 m against an 8 m row pitch, so half of every row was bare ground and the
        /// O240 still read as a striped rectangle. The band scales with the block the card stands for instead: 1.78 m at
        /// step 1, 3.56 m at step 2.</summary>
        public static float GasBankSquashFor(float width, int step) => 3f * GasBankBandM * step / width;
        /// <summary>The full DEPTH span a gas card's centre may fill (m) PER MERGED BLOCK: one full field cell
        /// (<see cref="MapData.FieldCellSize"/>) at step 1, the whole 8 m block at the overview's step 2,
        /// filled in two strata by <see cref="GasBankZOffset"/> rather than at random - at the old 1.2 m jitter every card
        /// sat within 0.6 m of its row line, so the 4 m row lattice still read as bands, and a random full-cell jitter
        /// covered the depth only on average, so the worst screen row still read a quarter under the bank's median.</summary>
        public const float GasBankJitterM = 4f;
        /// <summary>How far off its cell's centre a gas card may stand SIDEWAYS (m). Card widths, roll and mirror already
        /// break the X lattice, so a small amount is enough; a full-cell jitter in X pulled card centres apart and split
        /// the painted bank into separate patches.</summary>
        public const float GasBankJitterXM = 1.2f;

        /// <summary>Where in the block it stands for a card sits in depth (m off the block centre): even card columns in
        /// the near half, odd columns in the far half, each filling its own stratum. The union fills the whole block
        /// uniformly, but never by chance: a neighbouring column always has a card core over the other's soft band edge.
        /// A card is four to five columns wide, so every screen x sees both parities. Both terms scale with
        /// <paramref name="step"/>, so the spread is +-2 m of a 4 m cell at step 1 and +-4 m of the overview's 8 m block -
        /// the fixed 4 m spread left the far half of every merged row empty.</summary>
        public static float GasBankZOffset(int x, int step, float h2)
            => ((((x / step) & 1) == 0) ? -GasBankJitterM * step / 4f : GasBankJitterM * step / 4f) + (h2 - 0.5f) * GasBankJitterM * step / 2f;

        /// <summary>How many field blocks a gas card's thickness is read from: its own and the eight around it.</summary>
        public const int GasBankNeighbourhood = 9;
        /// <summary>A gas card's load: the GREATER of its own thickness and the mean of the nine block maxima around
        /// it (its own plus <paramref name="ringSum"/>, the eight neighbours; off the map counts as 0). A card used
        /// to read its own 4 m cell alone, so at the cloud's edge, where the field decays cell by cell, the outermost
        /// ring of cards drew right at the probe's visibility bar: that marginal paint broke into blotches and each
        /// blotch counted as its own patch (28 / 24 / 18 for one bank). Reading the mean instead TRIMMED that ring -
        /// it fell under the stand floor and went - but only moved the problem outward: the ring one further in is
        /// itself a ragged line of near-threshold cards, so the probe still counted 29 / 18 / 21 patches, a looser
        /// ceiling than before. The islands are not a property of which ring is outermost; they are a property of
        /// ANY outermost ring drawn at low opacity, and trimming a ring only makes a new one.
        /// `max` does not trim, it LIFTS: a fringe cell is raised to its neighbourhood's load rather than cut down to
        /// it, so the outermost ring draws thick enough to join the body it sits against instead of fading out below
        /// the paint threshold between its own neighbours. A cell inside a solid body is unchanged (max of equals,
        /// since nine times the same value over nine is that value), so the core reads exactly as it did under the
        /// mean. The bank comes out about one ring wider than the mean build - back to roughly its width before the
        /// neighbourhood read was added at all.
        /// That lift did NOT join the sheet either (35 / 30 / 24 patches, worse than both earlier builds): a ring lifted
        /// to just over the paint threshold is still a ragged line of marginal cards. The formula is kept as it is and
        /// <see cref="GasBankStandFloor"/> does the work instead - it cuts the lifted load, so no card stands near the
        /// threshold at all.</summary>
        public static float GasBankLoad(float self, float ringSum) => Mathf.Max(self, (self + ringSum) / GasBankNeighbourhood);

        public static readonly Color GasBankNight = new Color(0.74f, 0.80f, 0.34f), GasBankDay = new Color(0.50f, 0.64f, 0.12f);

        // Phase: gas-smoke-night (the owner, 2026-10-19: the ability clouds were tuned by day; at night black or grey
        // smoke vanishes). Two knobs, read once in Awake, a moonlit field only (CombatFx.ApplyTints), both at their old
        // value drawing today's image bit for bit:
        //   fx.smokeScreenNight  the luma the smoke screen's bank is painted at, at the drawing's middle ink
        //                        (FlipbookFx.NightBanks -> PaintNight), so the moon can neither dim it nor tint it blue.
        //                        0 = the old look (the biome's dust tint, lit by the moon).
        //   fx.gasNight          a lift on GasBankNight on a moonlit field. 1 = the old look. The gas sheet's own Lit
        //                        (0.75) and High (0.60) are left alone: they put its middle ink in the light band on
        //                        purpose, which is what makes it read green and not grey, so this only brightens it.
        // Tuned on docs/evidence/gas-smoke-night/smoke_night.png and gas_night.png (zoom 30, NightMud), measured by
        // probe.py beside them. SmokeBankNight is the light warm grey the screen lands on at the default.
        public const string SmokeScreenNightKnob = "fx.smokeScreenNight", GasNightKnob = "fx.gasNight";
        public static readonly Color SmokeBankNight = new Color(0.80f, 0.78f, 0.74f);
        public const float OldSmokeScreenNight = 0f, OldGasNight = 1f;
        public static readonly float DefaultSmokeScreenNight = BankLuma(SmokeBankNight);
        public const float DefaultGasNight = 1.08f;

        /// <summary>A colour's luma on probe.py's scale (Rec.709, 0 to 1).</summary>
        public static float BankLuma(Color c) => 0.2126f * c.r + 0.7152f * c.g + 0.0722f * c.b;

        /// <summary>How clearly a bank at this luma reads against ground at that luma: Michelson contrast, the one
        /// definition docs/evidence/gas-smoke-night/probe.py measures off the pixels and the test quotes.</summary>
        public static float BankContrast(float cloud, float ground)
            => cloud + ground > 0f ? Mathf.Abs(cloud - ground) / (cloud + ground) : 0f;

        /// <summary>fx.smokeScreenNight, in [0, 1] (0 = the old look).</summary>
        public static float ReadSmokeScreenNight() => Mathf.Clamp01(Knobs.Get(SmokeScreenNightKnob, DefaultSmokeScreenNight));

        /// <summary>fx.gasNight, in [1, 2] (1 = the old look).</summary>
        public static float ReadGasNight() => Mathf.Clamp(Knobs.Get(GasNightKnob, DefaultGasNight), 1f, 2f);

        /// <summary>The opacity of a gas cell's card at a thickness (0 to 1).</summary>
        public static float GasBankAlpha(float thick) => Mathf.Lerp(GasBankThin, GasBankFull, Mathf.Clamp01(thick));

        /// <summary>How wide a gas cell's card is drawn (m): 23.4-33.0 m on the 4 m field grid, so a card spans six to
        /// eight cells, neighbours overlap six to eight deep, and no single card's silhouette is the shape you see. More
        /// cards reach every screen row, so the worst row sits less far under the median and the sub-threshold holes
        /// between card skirts (which read as separate patches) close. Same card count. It was 16.2-24.2 m until the
        /// 2.4 m <see cref="GasBankStandFloor"/> dropped the bank's outer skirt and took 5-9 m off its width: wider
        /// cards put that width back and thicken the paint by overlap, without touching the floor, the alpha or the hue.
        /// A card's HEIGHT does not move with this (<c>width * GasBankSquashFor(width, step)</c> cancels the width).</summary>
        public static float GasBankWidth(float thick, float hash01, int step) => (26f + 4f * thick) * (0.9f + 0.2f * hash01) * step;

        // Phase: gas-smoke-night-g2 (the owner, 2026-10-20: "give the gas a rim and some height a player can see at
        // zoom 30"). A flat stain and a cloud have the same contrast, so the look is carried by two numbers the probe
        // beside docs/evidence/gas-smoke-night-g2 measures off the pixels: the lip on top of the bank (rim) and what
        // stands above the cloud's own far ground edge (height_m).
        //   the lip: the cells on the field's edge (a neighbour below the floor, or off the grid) are drawn brighter,
        //            taller and denser, so the blanket ends in an edge instead of fading into the grass.
        //   the height: a full cell's card is GasBankTall as tall as it is wide, its foot kept on the ground.
        // Width is untouched: the footprint does not grow and no card is added, only reshaped (the barrage frame is
        // already at 374 draws against 300).
        public const float GasBankTall = 1.35f;
        public const float GasEdgeBright = 1.25f, GasEdgeTall = 1.25f, GasEdgeAlpha = 0.15f;

        /// <summary>How a gas cell's card is lifted out of the blanket: the edge of the field gets the lip, the inside
        /// is drawn as before. Pure, so GasSmokeNightTests can hold the numbers. thick is the cell's thickness; the lip
        /// does not ease in with it, because it is the thin outer cells that need it most.</summary>
        public static void BankRim(bool edge, float thick, out float bright, out float tall, out float alphaLift)
        {
            bright = edge ? GasEdgeBright : 1f;
            tall = edge ? GasEdgeTall : 1f;
            alphaLift = edge ? GasEdgeAlpha : 0f;
        }

        public const float SmokeBankFirst = 12f, SmokeBankLast = 20.5f; // the mound grown (the firebooks cut stops at 21)
        // How thick a cell must be to stand a card, and how firm the card is drawn: at the standard view, and from the
        // overview (FlipbookFx.FarGrowMax), where the screen has to read as one mass and not as a few thick cells.
        public const float SmokeFloor = 1.5f, SmokeFarFloor = 0.15f, SmokeFarAlpha = 0.85f;
        // and wider again than FarGrow alone: a screen the men hide behind has to read as a screen from the overview,
        // not as a smudge (harsh critic r1 fix 3; measured against its own no-effect plate,
        // docs/evidence/ability-fx-captures-r3-2)
        public const float SmokeFarWide = 1.35f;

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
            return BankStep(ViewEye.Zoom);
        }

        /// <summary>Is the block of cells at (x, z) below the floor, or off the grid? Off the grid counts as below: the
        /// field stops there, so the card at its last cell is on the edge.</summary>
        static bool BlockBelow(Unity.Collections.NativeArray<float> field, int width, int length, int x, int z, int step, float floor)
        {
            if (x < 0 || z < 0 || x >= width || z >= length) return true;
            float c = 0f;
            for (int dz = 0; dz < step && z + dz < length; dz++)
            for (int dx = 0; dx < step && x + dx < width; dx++)
                c = Mathf.Max(c, field[(z + dz) * width + x + dx]);
            return c < floor;
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
                // on the field's edge when any of the four neighbouring blocks is below the floor or off the grid
                bool edge = BlockBelow(field, width, length, x - step, z, step, floor)
                         || BlockBelow(field, width, length, x + step, z, step, floor)
                         || BlockBelow(field, width, length, x, z - step, step, floor)
                         || BlockBelow(field, width, length, x, z + step, step, floor);
                uint h = (uint)(x * p1 ^ z * p2);
                float h1 = (h & 1023) / 1023f, h2 = ((h >> 10) & 1023) / 1023f;
                float wx = (x + 0.5f * step) * cs + (h1 - 0.5f) * jitterX;
                float wz = (z + 0.5f * step) * cs + (stratifyZ ? GasBankZOffset(x, step, h2) : (h2 - 0.5f) * jitterZ);
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

        void DrawGasBank(GasSmokeSystem gas, float now, Bounds bounds)
        {
            int step = CurrentBankStep();
            Refresh(gasBank, gas.Gas, gas.Width, gas.Length, step, GasBankStandFloor, GasBankFullM, GasBankJitterXM, GasBankJitterM, true, true, 73856093, 19349663);
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
                // gas-smoke-night-g2 (the owner, 2026-10-20): the edge cells get a lip and a full cell stands taller; the
                // card grows up from its foot, so the centre lifts by half of what it grew and the bank stays on the ground
                BankRim(b.Edge, b.Thick, out float bright, out float rimTall, out float alphaLift);
                float flat = width * GasBankSquashFor(width, step);
                float height = flat * Mathf.Lerp(1f, GasBankTall, b.Thick) * rimTall;
                float y = b.Ground - 0.08f + 0.5f * (height - flat);
                gasCards.Add(FlipbookFx.Pack(new Vector3(b.X, y, b.Z), width, height, BankFrame(GasBankFirst, GasBankLast, slow), 1f, (0.75f + 0.45f * b.Thick) * bright, (h2 - 0.5f) * 0.42f, kind, Mathf.Min(1f, GasBankAlpha(b.Thick) + alphaLift)));
            }
            books.DrawPacked(FlipbookFx.Book.GasBank, gasCards, bounds);
        }

        void DrawSmokeBank(GasSmokeSystem gas, float now, Bounds bounds)
        {
            int step = CurrentBankStep();
            // IN-5: the bank grows with the zoom past 80 the way a burst's plume and ring do (FlipbookFx.FarGrow), and
            // from the overview the thin edge of the screen stands as well (SmokeFarFloor) at a firmer alpha. Without
            // any of that a 40 m screen was 70 m of a 1280 px overview frame - under 1 % of it, which is why harsh
            // critic r1 fix 3 could not find the smoke at O240. The cards are repacked every frame, so the zoom is
            // read as the frame is drawn, not as the smoke was called in.
            float far = FlipbookFx.FarGrow(ViewEye.Zoom);
            float t = (far - 1f) / (FlipbookFx.FarGrowMax - 1f);
            Refresh(smokeBank, gas.Smoke, gas.Width, gas.Length, step, Mathf.Lerp(SmokeFloor, SmokeFarFloor, t), 20f, 2.2f, 2.2f, false, false, 83492791, 29349663);
            smokeCards.Clear();
            foreach (var b in smokeBank.Cells)
            {
                float h2 = ((b.Hash >> 10) & 1023) / 1023f, h3 = ((b.Hash >> 20) & 1023) / 1023f;
                float slow = now * 0.18f + h3 * 6.2832f;
                float width = (6f + 2f * b.Thick) * (0.9f + 0.2f * h2) * step * far * Mathf.Lerp(1f, SmokeFarWide, t);
                var kind = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (((b.Hash >> 5) & 1) == 0 ? FlipbookFx.Kind.Mirror : 0);
                // a white band is the most readable thing on a night field: at most 0.7 at the standard view, so the HUD
                // rings show through it; from the overview, where the rings are a few pixels anyway, up to SmokeFarAlpha
                smokeCards.Add(FlipbookFx.Pack(new Vector3(b.X, b.Ground - 0.2f, b.Z), width, width, BankFrame(SmokeBankFirst, SmokeBankLast, slow), 1f, 1f, 0f, kind,
                    Mathf.Min(Mathf.Lerp(0.7f, SmokeFarAlpha, t), Mathf.Lerp(0.3f, 0.5f, t) + 0.45f * b.Thick)));
            }
            if (smokeCards.Count > 0) books.DrawPacked(FlipbookFx.Book.SmokeBank, smokeCards, bounds);
        }
    }
}
