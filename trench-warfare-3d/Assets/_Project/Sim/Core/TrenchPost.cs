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
    /// trench more").
    ///
    /// The displacement is a SMOOTH field, not per-cell noise, and that distinction is the whole design.
    /// SeparationJob.GarrisonSpacing is 2 m and a nav cell is 2 m, so the lattice was exactly tuned to the
    /// separation radius: a posted garrison sat at rest with nothing pushing it. Independent per-cell offsets of
    /// ±0.55 m destroyed that — the worst adjacent pair of post points came out 0.933 m apart, well inside the
    /// radius, so those men pushed each other while their posts pulled them back and never settled. Sampling a
    /// low-frequency field instead means neighbouring posts share most of their displacement: the LINE wanders
    /// off true, which is what breaks the ruled look, while the distance BETWEEN neighbours is nearly preserved.
    /// Same amplitude, worst adjacent separation 0.933 m → 1.809 m (measured over the real post set, and held by
    /// TrenchSpreadTests.NoTwoPostPointsStandInsideEachOther).
    ///
    /// 1.809 m is still inside the 2 m radius, so the very tightest pairs in a FULLY PACKED trench keep a small
    /// standing push. That only arises through the room = 0 fallback, which only runs when the trench is full —
    /// and men shoulder to shoulder in a full trench is correct rather than a defect.
    ///
    /// Derived from the cell alone, so it is identical on every machine and never changes while he holds the post.
    /// </summary>
    public static class TrenchPost
    {
        /// <summary>Metres, each way, at the field's peak. Under half a cell, so a man stays on his own ground.</summary>
        public const float Jitter = 0.55f;

        /// <summary>Cells per wave of the field. Long, so that neighbours move together rather than apart.</summary>
        public const float Wavelength = 8f;

        /// <summary>The post layout belongs to the map, not to the match, so it is drawn from a fixed stream.</summary>
        const uint LayoutSeed = 0x5EA71Fu;

        /// <summary>Its own stream id, distinct from TrenchGarrisonSystem's 11 (see SimRandom.SystemId).</summary>
        const uint StreamId = 12u;

        public static float3 Offset(int cell, int navWidth)
        {
            if (cell < 0 || navWidth <= 0) return float3.zero;
            float cx = cell % navWidth, cz = cell / navWidth;
            return new float3(Jitter * Field(cx, cz, 2u), 0f, Jitter * Field(cx, cz, 3u));
        }

        /// <summary>A corner value of the field, in -1..1.</summary>
        static float Corner(int ix, int iz, uint salt)
        {
            uint h = SimRandom.Mix(LayoutSeed, salt, StreamId, (uint)(((iz & 0xFFFF) << 16) | (ix & 0xFFFF)));
            return (h & 0xFFFFu) / 65535f * 2f - 1f;
        }

        /// <summary>Smoothstep-interpolated value noise: continuous, so adjacent cells sample almost the same value.</summary>
        static float Field(float cx, float cz, uint salt)
        {
            float fx = cx / Wavelength, fz = cz / Wavelength;
            int ix = (int)math.floor(fx), iz = (int)math.floor(fz);
            float tx = fx - ix, tz = fz - iz;
            float sx = tx * tx * (3f - 2f * tx), sz = tz * tz * (3f - 2f * tz);
            float a = Corner(ix, iz, salt), b = Corner(ix + 1, iz, salt);
            float c = Corner(ix, iz + 1, salt), d = Corner(ix + 1, iz + 1, salt);
            float top = a + (b - a) * sx, bottom = c + (d - c) * sx;
            return top + (bottom - top) * sz;
        }
    }
}
