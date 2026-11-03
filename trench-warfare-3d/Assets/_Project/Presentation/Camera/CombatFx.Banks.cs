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

        /// <summary>Which of the three readable densities a thickness falls in: 0 skirt, 1 body, 2 core.</summary>
        public static int GasBankLevel(float thick) => thick < 0.34f ? 0 : thick < 0.67f ? 1 : 2;

        /// <summary>The opacity of a gas cell's card at a thickness (0 to 1): a step by level, not a fade, so the
        /// cloud reads as levels; an edge cell is always opaque so the shape has a silhouette.</summary>
        public static float GasBankAlpha(float thick, bool edge = false)
        {
            if (edge) return 1f;
            switch (GasBankLevel(thick))
            {
                case 0: return 0.70f;
                case 1: return 0.85f;
                default: return 1.00f;
            }
        }

        /// <summary>A card's height as a factor of its width: higher by level, so the core stands taller than the
        /// skirt; an edge cell never lies lower than the body so the rim stands up against the sky.</summary>
        public static float GasBankLift(float thick, bool edge)
        {
            float lift;
            switch (GasBankLevel(thick))
            {
                case 0: lift = 0.70f; break;
                case 1: lift = 1.00f; break;
                default: lift = 1.35f; break;
            }
            return edge ? Mathf.Max(lift, 1.00f) : lift;
        }

        /// <summary>A card's brightness: an edge cell is a darker rim outlining the shape, else brighter by level.</summary>
        public static float GasBankBright(float thick, bool edge)
        {
            if (edge) return 0.85f;
            switch (GasBankLevel(thick))
            {
                case 0: return 0.90f;
                case 1: return 1.00f;
                default: return 1.15f;
            }
        }

        /// <summary>True when any of a cell's four neighbours holds less than the field's floor, so the cell sits
        /// on the cloud's boundary and should draw as its rim.</summary>
        public static bool GasBankEdge(float up, float down, float left, float right, float floor) =>
            up < floor || down < floor || left < floor || right < floor;

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

        /// <summary>The same max-over-a-step-block read a cell's own density uses, for one neighbour block;
        /// 0 when the block falls off the field, so an edge beside the field's border reads as an edge too.</summary>
        static float BankBlockMax(Unity.Collections.NativeArray<float> field, int width, int length, int x, int z, int step)
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
                float c = 0f;
                for (int dz = 0; dz < step && z + dz < length; dz++)
                for (int dx = 0; dx < step && x + dx < width; dx++)
                    c = Mathf.Max(c, field[(z + dz) * width + x + dx]);
                if (c < floor) continue;
                uint h = (uint)(x * p1 ^ z * p2);
                float h1 = (h & 1023) / 1023f, h2 = ((h >> 10) & 1023) / 1023f;
                float wx = (x + 0.5f * step) * cs + (h1 - 0.5f) * jitter, wz = (z + 0.5f * step) * cs + (h2 - 0.5f) * jitter;
                bool edge = step == 1 && GasBankEdge(
                    BankBlockMax(field, width, length, x, z - step, step),
                    BankBlockMax(field, width, length, x, z + step, step),
                    BankBlockMax(field, width, length, x - step, z, step),
                    BankBlockMax(field, width, length, x + step, z, step),
                    floor);
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
                float lift = width * GasBankLift(b.Thick, b.Edge);
                gasCards.Add(FlipbookFx.Pack(new Vector3(b.X, b.Ground - 0.2f, b.Z), width, lift, BankFrame(GasBankFirst, GasBankLast, slow), 1f, GasBankBright(b.Thick, b.Edge), 0f, kind, GasBankAlpha(b.Thick, b.Edge)));
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
