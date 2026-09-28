// Phase: VFX pass (owner, 2026-09-28; tw3d-board catalogue IN-8, look specs L07 and L08) — part of CombatFx. Behind
// fx.recipes, the gas and smoke fields as the drawn banks: chlorine as GasBank, a low rolling bank standing ON the ground
// (its drawing fills the bottom third of the card, so a man's legs are in it and his head is not), the smoke screen as
// SmokeBank mounds. Per 4 m field cell as before, placed and boiled by a hash of the cell (no random draws); from the
// overview (BankMergeZoom) one card a 2 x 2 block, twice the size, so the field is a blanket with an edge and not
// four times the cards.
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
        const float SmokeBankFirst = 12f, SmokeBankLast = 20.5f; // the mound grown (the firebooks cut stops at 21)

        /// <summary>Cells a card stands for at this zoom: 1, or 2 (a 2 x 2 block) from the overview.</summary>
        public static int BankStep(float zoom) => zoom >= BankMergeZoom ? 2 : 1;

        /// <summary>The frame a cell's bank shows: boiled slowly back and forth between two frames of the book.</summary>
        public static float BankFrame(float first, float last, float slow) => first + (last - first) * (0.5f + 0.5f * Mathf.Sin(slow));

        /// <summary>L07/L08 release: at each source of a gas cloud a GasVent (a low jet spreading) for its 32 frames; at each
        /// source of a smoke screen the SmokeBank's own early frames (the mound growing out of the canister). Close only:
        /// from the overview the bank itself is the read.</summary>
        void OnBankSpawned(SimEvent e)
        {
            if (recipes < 0.5f || books == null || !books.Ready) return;
            var cam = Camera.main;
            if (BankStep(cam != null && cam.TryGetComponent<IZoomSource>(out var zs) ? zs.CurrentZoom : 0f) > 1) return;
            Vector3 at = (Vector3)e.Pos; at.y = RenderGround.Sample(Host.Local.Map, at.x, at.z) - 0.1f;
            var kind = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored;
            if (e.Type == SimEventType.GasCloudSpawned) books.Add(FlipbookFx.Book.GasVent, at, 4f, 32f / 12f, kind, alpha: 0.8f);
            else books.Add(FlipbookFx.Book.SmokeBank, at, 5f, 21f / 12f, kind, alpha: 0.7f);
        }

        void DrawGasBank(GasSmokeSystem gas, float now, Bounds bounds)
        {
            var cam = Camera.main;
            int step = BankStep(cam != null && cam.TryGetComponent<IZoomSource>(out var zs) ? zs.CurrentZoom : 0f);
            float cs = MapData.FieldCellSize;
            gasCards.Clear();
            for (int z = 0; z < gas.Length; z += step)
            for (int x = 0; x < gas.Width; x += step)
            {
                float c = 0f;
                for (int dz = 0; dz < step && z + dz < gas.Length; dz++)
                for (int dx = 0; dx < step && x + dx < gas.Width; dx++)
                    c = Mathf.Max(c, gas.Gas[(z + dz) * gas.Width + x + dx]);
                if (c < 0.8f) continue;
                uint h = (uint)(x * 73856093 ^ z * 19349663);
                float h1 = (h & 1023) / 1023f, h2 = ((h >> 10) & 1023) / 1023f, h3 = ((h >> 20) & 1023) / 1023f;
                float thick = Mathf.Clamp01(c / 14f);
                float wx = (x + 0.5f * step) * cs + (h1 - 0.5f) * 2.4f, wz = (z + 0.5f * step) * cs + (h2 - 0.5f) * 2.4f;
                float ground = RenderGround.Sample(Host.Local.Map, wx, wz);
                float slow = now * 0.22f + h3 * 6.2832f;
                float width = (5f + 2f * thick) * (0.9f + 0.2f * h2) * step;
                var kind = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (((h >> 5) & 1) == 0 ? FlipbookFx.Kind.Mirror : 0);
                gasCards.Add(FlipbookFx.Pack(new Vector3(wx, ground - 0.2f, wz), width, width, BankFrame(GasBankFirst, GasBankLast, slow), 1f, 1f, 0f, kind, 0.3f + 0.5f * thick));
            }
            books.DrawPacked(FlipbookFx.Book.GasBank, gasCards, bounds);
        }

        void DrawSmokeBank(GasSmokeSystem gas, float now, Bounds bounds)
        {
            var cam = Camera.main;
            int step = BankStep(cam != null && cam.TryGetComponent<IZoomSource>(out var zs) ? zs.CurrentZoom : 0f);
            float cs = MapData.FieldCellSize;
            smokeCards.Clear();
            for (int z = 0; z < gas.Length; z += step)
            for (int x = 0; x < gas.Width; x += step)
            {
                float c = 0f;
                for (int dz = 0; dz < step && z + dz < gas.Length; dz++)
                for (int dx = 0; dx < step && x + dx < gas.Width; dx++)
                    c = Mathf.Max(c, gas.Smoke[(z + dz) * gas.Width + x + dx]);
                if (c < 1.5f) continue;
                uint h = (uint)(x * 83492791 ^ z * 29349663);
                float h1 = (h & 1023) / 1023f, h2 = ((h >> 10) & 1023) / 1023f, h3 = ((h >> 20) & 1023) / 1023f;
                float thick = Mathf.Clamp01(c / 20f);
                float wx = (x + 0.5f * step) * cs + (h1 - 0.5f) * 2.2f, wz = (z + 0.5f * step) * cs + (h2 - 0.5f) * 2.2f;
                float ground = RenderGround.Sample(Host.Local.Map, wx, wz);
                float slow = now * 0.18f + h3 * 6.2832f;
                float width = (6f + 2f * thick) * (0.9f + 0.2f * h2) * step;
                var kind = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (((h >> 5) & 1) == 0 ? FlipbookFx.Kind.Mirror : 0);
                // a white band is the most readable thing on a night field: at most 0.7, so the HUD rings show through it
                smokeCards.Add(FlipbookFx.Pack(new Vector3(wx, ground - 0.2f, wz), width, width, BankFrame(SmokeBankFirst, SmokeBankLast, slow), 1f, 1f, 0f, kind, Mathf.Min(0.7f, 0.3f + 0.45f * thick)));
            }
            if (smokeCards.Count > 0) books.DrawPacked(FlipbookFx.Book.SmokeBank, smokeCards, bounds);
        }
    }
}
