// Phase: VFX pass (owner, 2026-09-28; tw3d-board catalogue IN-8, look specs L07 and L08) — part of CombatFx. Behind
// fx.recipes, the gas and smoke fields as the drawn banks: chlorine as GasBank, a low rolling bank standing ON the ground
// (its drawing fills the bottom third of the card, so a man's legs are in it and his head is not), the smoke screen as
// SmokeBank mounds. Per 4 m field cell as before, placed and boiled by a hash of the cell (no random draws); from the
// overview (BankMergeZoom) the smoke keeps one card a 2 x 2 block, twice the size, but the gas is drawn as ONE standing
// card per connected cloud (FindGasClouds): a dozen squashed block cards read as a cluster of pale stripes at 240 m, one
// padded, barely squashed card per cloud reads as a cloud. The field only changes per sim tick, so which cells stand,
// where their ground is and which cloud they belong to are found once a tick (and again when the zoom crosses
// BankMergeZoom); a frame only boils them.
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
        /// <summary>How flat a close gas card is drawn (step 1, T1 and in): the sheet fills the bottom third, so a 7 m card
        /// squashed to 0.55 tops out at 1.28 m - gas pools, it does not stand over a man.</summary>
        public const float GasBankSquash = 0.55f;
        /// <summary>How flat an overview cloud card is drawn (step > 1): at 240 m a 0.55 pancake reads as a stripe, 0.8 stands.</summary>
        public const float GasCloudSquash = 0.8f;
        /// <summary>How much wider than its cells' bounding box a cloud's card is drawn, so it covers the edge cells' own spread.</summary>
        public const float GasCloudPad = 4f;
        public static readonly Color GasBankNight = new Color(0.74f, 0.80f, 0.34f), GasBankDay = new Color(0.50f, 0.64f, 0.12f);

        /// <summary>The opacity of a gas cell's card at a thickness (0 to 1).</summary>
        public static float GasBankAlpha(float thick) => Mathf.Lerp(GasBankThin, GasBankFull, Mathf.Clamp01(thick));

        const float SmokeBankFirst = 12f, SmokeBankLast = 20.5f; // the mound grown (the firebooks cut stops at 21)

        /// <summary>A card's worth of field: its hash, how thick, and where it stands.</summary>
        struct BankCell { public uint Hash; public float Thick, X, Z, Ground; }

        /// <summary>One field's standing cells, found once a sim tick at one merge step.</summary>
        sealed class BankCache { public readonly List<BankCell> Cells = new List<BankCell>(1024); public uint Tick = uint.MaxValue; public int Step; }
        readonly BankCache gasBank = new BankCache(), smokeBank = new BankCache();

        /// <summary>One connected gas cloud: where it stands, how wide its cells reach, and how thick its thickest cell is.</summary>
        public struct GasCloud { public float X, Z, Span, Thick; }

        // the overview's clouds, found once a sim tick; the buffers grow with the field and are never reallocated per frame
        readonly List<GasCloud> gasClouds = new List<GasCloud>(64);
        float[] gasFieldCopy; int[] gasLabels, gasStack;
        uint gasCloudTick = uint.MaxValue; int gasCloudStep;

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

        /// <summary>The connected clouds of a gas field: 4-connected cells at or above floor, one cloud a component.
        /// Pure (a float[] field, no scene, no NativeArray) so a test can call it. Span is the longer side of the
        /// component's bounding box in metres, X and Z its centre, Thick its thickest cell against full.</summary>
        public static void FindGasClouds(float[] field, int width, int length, float floor, float full, float cellSize, List<GasCloud> into)
        {
            int[] labels = null, stack = null;
            FindGasClouds(field, width, length, floor, full, cellSize, into, ref labels, ref stack);
        }

        /// <summary>FindGasClouds with the caller's buffers, grown only when the field grows: no allocation per frame.</summary>
        static void FindGasClouds(float[] field, int width, int length, float floor, float full, float cellSize, List<GasCloud> into, ref int[] labels, ref int[] stack)
        {
            into.Clear();
            int n = width * length;
            if (field == null || n <= 0 || field.Length < n) return;
            if (labels == null || labels.Length < n) labels = new int[n];
            if (stack == null || stack.Length < n) stack = new int[n];
            for (int i = 0; i < n; i++) labels[i] = 0;
            for (int seed = 0; seed < n; seed++)
            {
                if (labels[seed] != 0 || field[seed] < floor) continue;
                int minX = width, maxX = -1, minZ = length, maxZ = -1, top = 0;
                float peak = 0f;
                labels[seed] = 1; stack[top++] = seed;
                while (top > 0)
                {
                    int c = stack[--top], cx = c % width, cz = c / width;
                    if (cx < minX) minX = cx;
                    if (cx > maxX) maxX = cx;
                    if (cz < minZ) minZ = cz;
                    if (cz > maxZ) maxZ = cz;
                    if (field[c] > peak) peak = field[c];
                    if (cx > 0 && labels[c - 1] == 0 && field[c - 1] >= floor) { labels[c - 1] = 1; stack[top++] = c - 1; }
                    if (cx + 1 < width && labels[c + 1] == 0 && field[c + 1] >= floor) { labels[c + 1] = 1; stack[top++] = c + 1; }
                    if (cz > 0 && labels[c - width] == 0 && field[c - width] >= floor) { labels[c - width] = 1; stack[top++] = c - width; }
                    if (cz + 1 < length && labels[c + width] == 0 && field[c + width] >= floor) { labels[c + width] = 1; stack[top++] = c + width; }
                }
                into.Add(new GasCloud
                {
                    X = (minX + maxX + 1) * 0.5f * cellSize,
                    Z = (minZ + maxZ + 1) * 0.5f * cellSize,
                    Span = Mathf.Max(maxX - minX + 1, maxZ - minZ + 1) * cellSize,
                    Thick = full > 0f ? Mathf.Clamp01(peak / full) : 1f,
                });
            }
        }

        /// <summary>Refill the overview's clouds if the tick or the step has moved: the field is copied out of the sim once
        /// a tick, then flood-filled.</summary>
        void RefreshGasClouds(GasSmokeSystem gas, int step, float floor, float full)
        {
            uint tick = Host.Local.World.Tick;
            if (gasCloudTick == tick && gasCloudStep == step) return;
            gasCloudTick = tick; gasCloudStep = step;
            int n = gas.Width * gas.Length;
            if (gasFieldCopy == null || gasFieldCopy.Length < n) gasFieldCopy = new float[n];
            Unity.Collections.NativeArray<float>.Copy(gas.Gas, gasFieldCopy, n);
            FindGasClouds(gasFieldCopy, gas.Width, gas.Length, floor, full, MapData.FieldCellSize, gasClouds, ref gasLabels, ref gasStack);
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
            gasCards.Clear();
            if (step > 1)
            {
                // the overview: one standing card a cloud, so the field reads as a cloud and not as a cluster of stripes
                RefreshGasClouds(gas, step, 0.8f, 14f);
                float cs = MapData.FieldCellSize;
                foreach (var c in gasClouds)
                {
                    uint h = (uint)(Mathf.FloorToInt(c.X / cs) * 73856093 ^ Mathf.FloorToInt(c.Z / cs) * 19349663);
                    float slow = now * 0.22f + ((h >> 20) & 1023) / 1023f * 6.2832f;
                    float width = c.Span + GasCloudPad;
                    var ck = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored;
                    float ground = RenderGround.Sample(Host.Local.Map, c.X, c.Z);
                    gasCards.Add(FlipbookFx.Pack(new Vector3(c.X, ground - 0.35f, c.Z), width, width * GasCloudSquash, BankFrame(GasBankFirst, GasBankLast, slow), 1f, 1f, 0f, ck, GasBankAlpha(c.Thick)));
                }
                books.DrawPacked(FlipbookFx.Book.GasBank, gasCards, bounds);
                return;
            }
            Refresh(gasBank, gas.Gas, gas.Width, gas.Length, step, 0.8f, 14f, 2.4f, 73856093, 19349663);
            foreach (var b in gasBank.Cells)
            {
                float h2 = ((b.Hash >> 10) & 1023) / 1023f, h3 = ((b.Hash >> 20) & 1023) / 1023f;
                float slow = now * 0.22f + h3 * 6.2832f;
                float width = (5f + 2f * b.Thick) * (0.9f + 0.2f * h2) * step;
                var kind = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (((b.Hash >> 5) & 1) == 0 ? FlipbookFx.Kind.Mirror : 0);
                gasCards.Add(FlipbookFx.Pack(new Vector3(b.X, b.Ground - 0.35f, b.Z), width, width * GasBankSquash, BankFrame(GasBankFirst, GasBankLast, slow), 1f, 1f, 0f, kind, GasBankAlpha(b.Thick)));
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
