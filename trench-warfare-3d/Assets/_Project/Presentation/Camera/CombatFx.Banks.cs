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
        // How clearly the bank reads (r2-2-g1 2026-10-20: at 0.6-1.0 the bank was 0.13% of the zoom-30 night frame, invisible):
        // a thin cell's card and a full one's, and its colour by day, deeper than the sheet's so it is green ON snow
        // (r2-2-g1-g1 2026-10-20: at 0.85-1.0 every card was a flat lozenge with a hard edge - a zebra, not a bank)
        public const float GasBankThin = 0.50f, GasBankFull = 1f;
        /// <summary>Of the field's own peak, the least a gas cell must hold to be drawn: the core is the bank, the
        /// diffused tail (4%/tick) is not. Never under the absolute floor, so what kills is always at least outlined.</summary>
        public const float GasBankPeakFrac = 0.15f, GasBankFloor = 0.8f;
        /// <summary>Cards drawn per standing cell: the sheet's puff is smaller than its card, so one card a cell is a
        /// field of separate lozenges. Layered, offset and rolled, they break the body card's edge (r2-2-g1-g1).
        /// Cut to 2 to pay for the body card and measured: the frame lost a third of its bank area, so the third
        /// layer is back and the body card is paid for out of the narrower cards the head rule forces.</summary>
        public const int GasBankPuffs = 3;
        /// <summary>How flat a gas card is drawn: the sheet fills the bottom third, so the widest card (24.75 m) squashed to 0.20 tops out at 1.65 m - gas pools, it does not stand over a man.</summary>
        public const float GasBankSquash = 0.20f;
        /// <summary>The body of the bank (r2-2-g1-g1 part 3): two big cards on the core's own centre, sized from the
        /// core's own extent and capped here, so the body can never be wider than the gas that kills. One card of the
        /// sheet is one eroded puff, so a 34 m card is one 34 m soft puff: it lifts every interior pixel in one card,
        /// which is what closes the holes the per-cell puffs left between them.</summary>
        public const float GasBankBodyMax = 34f;
        /// <summary>The body card is flatter than a per-cell puff, so more of the bank lies under a man's head:
        /// 34 m at 0.15, bottom third, is 1.70 m.</summary>
        public const float GasBankBodySquash = 0.15f;
        /// <summary>How solidly the body reads. Under the puffs' core alpha: the puffs still break its edge.</summary>
        public const float GasBankBodyAlpha = 0.75f;
        public static readonly Color GasBankNight = new Color(0.86f, 0.92f, 0.52f), GasBankDay = new Color(0.50f, 0.64f, 0.12f);

        /// <summary>The opacity of a gas cell's card at a thickness (0 to 1): a smoothstep, so a rim cell fades out
        /// instead of ending on a card edge.</summary>
        public static float GasBankAlpha(float thick) => Mathf.SmoothStep(GasBankThin, GasBankFull, Mathf.Clamp01(thick));

        const float SmokeBankFirst = 12f, SmokeBankLast = 20.5f; // the mound grown (the firebooks cut stops at 21)

        /// <summary>A card's worth of field: its hash, how thick, and where it stands.</summary>
        struct BankCell { public uint Hash; public float Thick, X, Z, Ground; }

        /// <summary>One field's standing cells, found once a sim tick at one merge step.</summary>
        sealed class BankCache
        {
            public readonly List<BankCell> Cells = new List<BankCell>(1024);
            public uint Tick = uint.MaxValue; public int Step;
            /// <summary>The one-place pass's scratch, grown once and then reused: no allocation a frame.</summary>
            public int[] Mark = System.Array.Empty<int>(), Queue = System.Array.Empty<int>();
            public void Keep(int n) { if (Mark.Length < n) { Mark = new int[n]; Queue = new int[n]; } }
            /// <summary>The footprint of the one-place pass's core, summed while it is walked: how many cells it
            /// holds, its centre, its box, its thickest cell, the ground under its centre, and a hash of it. The
            /// body card is drawn from these, so it is always the shape of the gas that kills.</summary>
            public int CoreCount; public float CoreX, CoreZ, MinX, MaxX, MinZ, MaxZ, CoreThick, CoreGround; public uint CoreHash;
            public void NoCore() { CoreCount = 0; CoreX = CoreZ = CoreThick = CoreGround = 0f; CoreHash = 0u;
                                   MinX = MinZ = float.MaxValue; MaxX = MaxZ = float.MinValue; }
        }
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
        /// full: what counts as thick; peakFrac: that share of the field's own peak as a second, relative floor, and with
        /// it the one-place rule - only the largest connected run of standing cells is drawn, so the core is the bank and
        /// the diffused tail is not (0 for neither); jitter: how far off its centre a card may stand; salt: the hash's primes.</summary>
        void Refresh(BankCache cache, Unity.Collections.NativeArray<float> field, int width, int length, int step, float floor, float full, float jitter, int p1, int p2, float peakFrac = 0f)
        {
            uint tick = Host.Local.World.Tick;
            if (cache.Tick == tick && cache.Step == step) return;
            cache.Tick = tick; cache.Step = step; cache.Cells.Clear(); cache.NoCore();
            float cs = MapData.FieldCellSize;
            float thickFrom = 0f;   // a cell's thickness runs from the floor it had to clear, not from nothing
            int gw = (width + step - 1) / step, gl = (length + step - 1) / step;
            if (peakFrac > 0f)
            {
                float peak = 0f;
                for (int i = 0, n = width * length; i < n; i++) peak = Mathf.Max(peak, field[i]);
                floor = Mathf.Max(floor, peakFrac * peak);
                // and read a cell's thickness against that peak, not against an absolute `full` the field never
                // reaches: a core cell is then drawn as a core cell, not at the thin end of the alpha
                if (peak > floor) { full = peak; thickFrom = floor; }
                cache.Keep(gw * gl);
            }
            for (int gz = 0; gz < gl; gz++)
            for (int gx = 0; gx < gw; gx++)
            {
                int z = gz * step, x = gx * step;
                float c = 0f;
                for (int dz = 0; dz < step && z + dz < length; dz++)
                for (int dx = 0; dx < step && x + dx < width; dx++)
                    c = Mathf.Max(c, field[(z + dz) * width + x + dx]);
                if (peakFrac > 0f) { cache.Mark[gz * gw + gx] = c >= floor ? 0 : -1; continue; }
                if (c < floor) continue;
                Add(cache, c, x, z, step, cs, thickFrom, full, jitter, p1, p2);
            }
            if (peakFrac <= 0f) return;
            // ONE PLACE: the largest connected group of standing cells, nothing else. A player points at a bank;
            // he cannot point at the field's whole spread.
            int best = -1, bestCount = 0, group = 0;
            for (int i = 0; i < gw * gl; i++)
            {
                if (cache.Mark[i] != 0) continue;
                group++;
                int head = 0, tail = 0, count = 0;
                cache.Queue[tail++] = i; cache.Mark[i] = group;
                while (head < tail)
                {
                    int q = cache.Queue[head++]; count++;
                    int qx = q % gw, qz = q / gw;
                    for (int dz = -1; dz <= 1; dz++)
                    for (int dx = -1; dx <= 1; dx++)
                    {
                        int nx = qx + dx, nz = qz + dz;
                        if (nx < 0 || nz < 0 || nx >= gw || nz >= gl) continue;
                        int n = nz * gw + nx;
                        if (cache.Mark[n] != 0) continue;
                        cache.Mark[n] = group; cache.Queue[tail++] = n;
                    }
                }
                if (count > bestCount) { bestCount = count; best = group; }
            }
            if (best < 0) return;
            for (int gz = 0; gz < gl; gz++)
            for (int gx = 0; gx < gw; gx++)
            {
                if (cache.Mark[gz * gw + gx] != best) continue;
                int z = gz * step, x = gx * step;
                float c = 0f;
                for (int dz = 0; dz < step && z + dz < length; dz++)
                for (int dx = 0; dx < step && x + dx < width; dx++)
                    c = Mathf.Max(c, field[(z + dz) * width + x + dx]);
                Add(cache, c, x, z, step, cs, thickFrom, full, jitter, p1, p2);
                // and the core's own footprint, out of the cell just added: no second pass and no allocation
                var cell = cache.Cells[cache.Cells.Count - 1];
                cache.CoreCount++;
                cache.CoreX += cell.X; cache.CoreZ += cell.Z;
                cache.MinX = Mathf.Min(cache.MinX, cell.X); cache.MaxX = Mathf.Max(cache.MaxX, cell.X);
                cache.MinZ = Mathf.Min(cache.MinZ, cell.Z); cache.MaxZ = Mathf.Max(cache.MaxZ, cell.Z);
                cache.CoreThick = Mathf.Max(cache.CoreThick, cell.Thick);
                cache.CoreHash ^= cell.Hash;
            }
            if (cache.CoreCount > 0)
            {
                cache.CoreX /= cache.CoreCount; cache.CoreZ /= cache.CoreCount;
                cache.CoreGround = RenderGround.Sample(Host.Local.Map, cache.CoreX, cache.CoreZ);
            }
        }

        /// <summary>One standing cell as a card's worth of field: where it stands (jittered off its centre by its own
        /// hash) and how thick it reads between the floor it cleared and the field's full.</summary>
        void Add(BankCache cache, float c, int x, int z, int step, float cs, float thickFrom, float full, float jitter, int p1, int p2)
        {
            uint h = (uint)(x * p1 ^ z * p2);
            float h1 = (h & 1023) / 1023f, h2 = ((h >> 10) & 1023) / 1023f;
            float wx = (x + 0.5f * step) * cs + (h1 - 0.5f) * jitter, wz = (z + 0.5f * step) * cs + (h2 - 0.5f) * jitter;
            float thick = full > thickFrom ? (c - thickFrom) / (full - thickFrom) : c / full;
            cache.Cells.Add(new BankCell { Hash = h, Thick = Mathf.Clamp01(thick), X = wx, Z = wz, Ground = RenderGround.Sample(Host.Local.Map, wx, wz) });
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
            Refresh(gasBank, gas.Gas, gas.Width, gas.Length, step, GasBankFloor, 10f, 2.4f, 73856093, 19349663, GasBankPeakFrac);
            gasCards.Clear();
            DrawGasBankBody(now);
            foreach (var b in gasBank.Cells)
            {
                float h2 = ((b.Hash >> 10) & 1023) / 1023f, h3 = ((b.Hash >> 20) & 1023) / 1023f;
                // GasBankPuffs cards a cell, not one: the sheet draws a small puff in the middle of its card, so one
                // card a cell leaves holes between the puffs. Layered, offset and rolled, they build one body.
                for (int i = 0; i < GasBankPuffs; i++)
                {
                    uint hp = (b.Hash + (uint)i * 0x9E3779B9u) * 2654435761u;
                    float p1 = ((hp >> 6) & 1023) / 1023f, p2 = ((hp >> 16) & 1023) / 1023f, p3 = ((hp >> 26) & 63) / 63f;
                    float slow = now * 0.22f + (h3 + p3) * 6.2832f;
                    // a roll per card over the full turn, so the repeated lozenge stops lining up into stripes
                    float roll = p2 * 6.2832f;
                    // r2-2-g1-g1: wide enough that the core's cards overlap into one body. 6-9 m cards on a 4 m grid
                    // read as separate lozenges - the owner's zebra.
                    float width = (13f + 5f * b.Thick) * (0.75f + 0.5f * p1) * (0.9f + 0.2f * h2) * step;
                    float off = MapData.FieldCellSize * step * 0.5f;   // a tight layer: the bank stays one patch, not a smear
                    var kind = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (((hp >> 5) & 1) == 0 ? FlipbookFx.Kind.Mirror : 0);
                    // just above the mud, not 0.35 m under it: the sheet's ground soften (GasBank's Ground 0.55) needs
                    // the card's foot to be in frame to fade it into the ground
                    var at = new Vector3(b.X + (p1 - 0.5f) * off, b.Ground + 0.1f, b.Z + (p2 - 0.5f) * off);
                    gasCards.Add(FlipbookFx.Pack(at, width, width * GasBankSquash, BankFrame(GasBankFirst, GasBankLast, slow), 1f, 1f, roll, kind, GasBankAlpha(b.Thick)));
                }
            }
            books.DrawPacked(FlipbookFx.Book.GasBank, gasCards, bounds);
        }

        /// <summary>The bank's body: two big soft cards on the core's centre, before the per-cell puffs. The puffs
        /// alone were the body, and small puffs on a 4 m grid leave holes between them whatever their width (part 2
        /// walked floor, alpha, width, squash and layer count and the frame stayed 0.285 full). One card that is the
        /// whole footprint has no holes, so the puffs can go back to being an eroded edge.</summary>
        void DrawGasBankBody(float now)
        {
            var c = gasBank;
            if (c.CoreCount <= 0) return;
            float extent = Mathf.Max(c.MaxX - c.MinX, c.MaxZ - c.MinZ);
            float w = Mathf.Min(0.9f * extent, GasBankBodyMax);
            if (w < 1f) return;
            float off = MapData.FieldCellSize * 0.5f;
            for (int i = 0; i < 2; i++)
            {
                uint hb = (c.CoreHash + (uint)i * 0x9E3779B9u) * 2654435761u;
                float b1 = ((hb >> 6) & 1023) / 1023f, b2 = ((hb >> 16) & 1023) / 1023f, b3 = ((hb >> 26) & 63) / 63f;
                float bw = i == 0 ? w : w * 0.85f;
                var at = new Vector3(c.CoreX + (i == 0 ? 0f : (b1 - 0.5f) * off), c.CoreGround + 0.1f,
                                     c.CoreZ + (i == 0 ? 0f : (b2 - 0.5f) * off));
                var kind = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (((hb >> 5) & 1) == 0 ? FlipbookFx.Kind.Mirror : 0);
                gasCards.Add(FlipbookFx.Pack(at, bw, bw * GasBankBodySquash,
                                             BankFrame(GasBankFirst, GasBankLast, now * 0.22f + b3 * 6.2832f),
                                             1f, 1f, b2 * 6.2832f, kind, GasBankBodyAlpha));
            }
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
