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
        /// <summary>Where the GasBank sheet's ink tops out in its card: the Sheets row draws it in the bottom third.</summary>
        public const float GasInkTop = 1f / 3f;
        /// <summary>Where the GasCloud sheet's ink tops out in its card: the smoke bank's drawing fills its cell (its Sheets row's Ink.y).</summary>
        public const float GasCloudInkTop = 0.87f;
        /// <summary>How flat a close gas card is drawn with the cloud drawing: it fills its cell, so a 7 m card is squashed far
        /// harder than GasBankSquash to leave a standing cloud under a man's head and not a wall.</summary>
        public const float GasCloudSquash = 0.25f;
        /// <summary>How far under the ground a gas card is anchored, so the ink's foot is not a hard line on the grass.</summary>
        public const float GasBankAnchorDrop = 0.35f;
        /// <summary>The highest the drawn gas may reach: under a 1.8 m man's head with 0.1 m to spare.</summary>
        public const float GasHeadRoom = 1.7f;
        public static readonly Color GasBankNight = new Color(0.74f, 0.80f, 0.34f), GasBankDay = new Color(0.50f, 0.64f, 0.12f);

        /// <summary>The opacity of a gas cell's card at a thickness (0 to 1).</summary>
        public static float GasBankAlpha(float thick) => Mathf.Lerp(GasBankThin, GasBankFull, Mathf.Clamp01(thick));

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

        void DrawGasBank(GasSmokeSystem gas, float now, Bounds bounds)
        {
            int step = CurrentBankStep();
            Refresh(gasBank, gas.Gas, gas.Width, gas.Length, step, 0.8f, 14f, 2.4f, 73856093, 19349663);
            gasCards.Clear();
            foreach (var b in gasBank.Cells)
                PackGasCell(gasCards, b.Hash, b.Thick, b.X, b.Z, b.Ground, now, step);
            // close up the cloud drawing (Book.GasCloud) reads as a standing bank; from the overview the flat GasBank card is the blanket the O240 bars were set on
            books.DrawPacked(step > 1 ? FlipbookFx.Book.GasBank : FlipbookFx.Book.GasCloud, gasCards, bounds);
        }

        /// <summary>One gas cell's cards, and how many were added. Close up (step 1) the cell is drawn with the cloud drawing
        /// (Book.GasCloud, which fills its cell), the cell's own hash jitters how flat it is and three cells in eight carry a
        /// second, wider and brighter card: a rim around the body's silhouette, so the bank is a lumpy standing cloud and not
        /// one straight line of pale stripes; the drawn ink never tops GasHeadRoom. From the overview (step 2) it is the one
        /// flat GasBank card it always was.</summary>
        public static int PackGasCell(List<Matrix4x4> into, uint hash, float thick, float x, float z, float ground, float now, int step)
        {
            float h2 = ((hash >> 10) & 1023) / 1023f, h3 = ((hash >> 20) & 1023) / 1023f, h4 = ((hash >> 15) & 1023) / 1023f;
            float slow = now * 0.22f + h3 * 6.2832f;
            float width = (5f + 2f * thick) * (0.9f + 0.2f * h2) * step;
            var kind = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (((hash >> 5) & 1) == 0 ? FlipbookFx.Kind.Mirror : 0);
            float alpha = GasBankAlpha(thick);
            // the cloud drawing is the smoke bank's 22 frames, so the close card boils in its range: GasBankLast 28 would
            // be clamped to the sheet's last frame by the shader and the boil would stand still
            float frame = step > 1 ? BankFrame(GasBankFirst, GasBankLast, slow) : BankFrame(SmokeBankFirst, SmokeBankLast, slow);
            float height = step > 1
                ? width * GasBankSquash
                : Mathf.Min(width * GasCloudSquash * (0.8f + 0.4f * h4), (GasHeadRoom + GasBankAnchorDrop) / GasCloudInkTop);
            into.Add(FlipbookFx.Pack(new Vector3(x, ground - GasBankAnchorDrop, z), width, height, frame, 1f, 1f, 0f, kind, alpha));
            if (step > 1 || ((hash >> 7) & 7) >= 3) return 1;
            // the rim: the same cloud, a touch wider and brighter and nearly see-through, so a green edge stands out
            // around the body's silhouette (the crown card faked the volume the old drawing lacked; this drawing has it)
            float rw = width * 1.12f;
            float rh = Mathf.Min(height * 1.12f, (GasHeadRoom + GasBankAnchorDrop) / GasCloudInkTop);
            into.Add(FlipbookFx.Pack(new Vector3(x, ground - GasBankAnchorDrop, z), rw, rh, frame, 1f, 1.5f, 0f, kind, 0.30f * alpha));
            return 2;
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
