// Phase: wrecks (2026-09-28, implemented) — which of a carcass's chunks are gone at each stage a wreck breaks through,
// pure, so WreckStageRulesTests hold it without a scene. The sim says the stage (PropKind) and how much of it is left
// (the prop's hit points against PropRules.StartHp); this says what is drawn:
//   Wreck:       whole, and it sheds up to half of its top band as it wears;
//   BrokenWreck: the top band is gone, and the bands between it and the keel go as it wears (a two-band hull keeps its keel);
//   Scrap:       the keel alone, flattened into a heap (TankRenderer squashes it);
//   Cleared:     nothing.
// Chunks only ever go: a later stage, or less left of the same stage, never brings one back.
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public static class WreckStageRules
    {
        public const int Whole = 0, Broken = 1, Scrap = 2, Cleared = 3;
        /// <summary>At most this share of the top band comes away while the wreck is still whole.</summary>
        public const float WholeShed = 0.5f;
        /// <summary>A scrap heap is the keel at this height (and this much wider), and it sinks this far to go.</summary>
        public const float ScrapHeight = 0.45f, ScrapWiden = 1.08f;

        /// <summary>The stage a prop kind is (-1: not wreckage).</summary>
        public static int StageOf(TW.Sim.Terrain.PropKind kind)
        {
            switch (kind)
            {
                case TW.Sim.Terrain.PropKind.Wreck: return Whole;
                case TW.Sim.Terrain.PropKind.BrokenWreck: return Broken;
                case TW.Sim.Terrain.PropKind.Scrap: return Scrap;
                case TW.Sim.Terrain.PropKind.Cleared: return Cleared;
                default: return -1;
            }
        }

        /// <summary>The first n chunks of a band's order, as bits.</summary>
        static uint Take(int[] order, int n)
        {
            uint bits = 0u;
            for (int i = 0; i < order.Length && i < n; i++) bits |= 1u << (order[i] - 1);
            return bits;
        }

        static uint BandBits(Carcass c, int band) => Take(c.ByBand[band], c.ByBand[band].Length);

        /// <summary>The chunks gone (bit k - 1 for chunk k) at this stage with this share (0..1) of its hit points left.</summary>
        public static uint Hidden(Carcass c, int stage, float share)
        {
            if (c == null) return 0u;
            float lost = 1f - Mathf.Clamp01(share);
            int top = c.Bands - 1;
            switch (stage)
            {
                case Whole:
                    return Take(c.ByBand[top], Mathf.FloorToInt(lost * WholeShed * c.ByBand[top].Length));
                case Broken:
                {
                    uint gone = BandBits(c, top);
                    // the bands between the crown and the keel, from the top down, as it wears
                    int between = 0; for (int band = 1; band < top; band++) between += c.ByBand[band].Length;
                    int n = Mathf.FloorToInt(lost * between);
                    for (int band = top - 1; band >= 1 && n > 0; band--)
                    {
                        int here = Mathf.Min(n, c.ByBand[band].Length);
                        gone |= Take(c.ByBand[band], here);
                        n -= here;
                    }
                    return gone;   // the whole top band: a superset of anything the whole stage shed
                }
                case Scrap: return c.All & ~c.Keel;
                case Cleared: return c.All;
                default: return 0u;
            }
        }

        /// <summary>The chunks that went between two masks (to throw them): in b, not in a.</summary>
        public static uint Newly(uint a, uint b) => b & ~a;
    }
}
