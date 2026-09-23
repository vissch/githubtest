// Phase: A3 (implemented) — where in his post cell a man garrisoning a trench actually stands.
// TrenchGarrisonSystem (TW.Sim.Units) hands out posts and MovementSystem (TW.Sim.Nav) walks men to them, and
// TW.Sim.Units already references TW.Sim.Nav, so this cannot live beside either of them without closing a cycle.
// It lives in Core, which both see, next to the rest of the sim's shared arithmetic.
using Unity.Mathematics;

namespace TW.Sim
{
    /// <summary>
    /// Posts are nav cells and a nav cell is 2 m, so standing every man at his cell's centre puts a garrison on a
    /// 2 m lattice: correctly spaced, and visibly ruled (owner, 2026-09-23: "we want them to spread out in the
    /// trench more"). This offsets him inside his own cell, derived from the cell alone, so it is the same on every
    /// machine, never changes while he holds the post, and costs nothing to store.
    /// </summary>
    public static class TrenchPost
    {
        /// <summary>Metres, each way. Under half a cell, so a man never drifts into the neighbouring post's.</summary>
        public const float Jitter = 0.55f;

        /// <summary>The post layout belongs to the map, not to the match, so it is drawn from a fixed stream.</summary>
        const uint LayoutSeed = 0x5EA71Fu, StreamId = 11u;

        public static float3 Offset(int cell)
        {
            if (cell < 0) return float3.zero;
            uint h = SimRandom.Mix(LayoutSeed, 2u, StreamId, (uint)cell);
            float ox = ((h & 0xFFFFu) / 65535f - 0.5f) * 2f * Jitter;
            float oz = (((h >> 16) & 0xFFFFu) / 65535f - 0.5f) * 2f * Jitter;
            return new float3(ox, 0f, oz);
        }
    }
}
