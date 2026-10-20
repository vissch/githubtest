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
                // on the field's edge when any of the four neighbouring blocks is below the floor or off the grid
                bool edge = BlockBelow(field, width, length, x - step, z, step, floor)
                         || BlockBelow(field, width, length, x + step, z, step, floor)
                         || BlockBelow(field, width, length, x, z - step, step, floor)
                         || BlockBelow(field, width, length, x, z + step, step, floor);
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

        void DrawGasBank(GasSmokeSystem gas, float now, Bounds bounds)
        {
            int step = CurrentBankStep();
            Refresh(gasBank, gas.Gas, gas.Width, gas.Length, step, 0.8f, 14f, 2.4f, 73856093, 19349663);
            gasCards.Clear();
            foreach (var b in gasBank.Cells)
            {
                float h2 = ((b.Hash >> 10) & 1023) / 1023f, h3 = ((b.Hash >> 20) & 1023) / 1023f;
                float slow = now * 0.22f + h3 * 6.2832f;
                float width = (5f + 2f * b.Thick) * (0.9f + 0.2f * h2) * step;
                var kind = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (((b.Hash >> 5) & 1) == 0 ? FlipbookFx.Kind.Mirror : 0);
                BankRim(b.Edge, b.Thick, out float bright, out float rimTall, out float alphaLift);
                float tall = Mathf.Lerp(1f, GasBankTall, b.Thick) * rimTall;
                float height = width * tall;
                // the drawing's bank sits in the bottom third of the card (the sheet's Ink), so the card's centre is
                // lifted by half of what it grew: the bank's foot stays on the ground it lies on
                float y = b.Ground - 0.2f + 0.5f * (height - width);
                gasCards.Add(FlipbookFx.Pack(new Vector3(b.X, y, b.Z), width, height, BankFrame(GasBankFirst, GasBankLast, slow), 1f, bright, 0f, kind, Mathf.Min(1f, GasBankAlpha(b.Thick) + alphaLift)));
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
