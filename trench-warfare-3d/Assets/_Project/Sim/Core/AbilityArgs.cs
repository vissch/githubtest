// Phase: A5 (implemented; docs/21 phase 5) — what a line ability's command carries in SimCommand.B.
// A SupportFire (and, once the sapper lands, a UnitAbility) says where a line starts in pos; its heading, the
// pattern the player picked and the length he dragged go in B as one int, so the command layout and the replay
// format do not change: heading in degrees (9 bits, 0 = up the field towards +Z, 90 = +X, clockwise from above),
// pattern (4 bits, 0 = the ability's plain form), length in metres (8 bits, 0 = the ability's own length).
// A B of 0, which is what every command before this packing carried, means "the plain ability, its own length".
using Unity.Mathematics;

namespace TW.Sim
{
    public static class AbilityArgs
    {
        public const int HeadingBits = 9, PatternBits = 4, LengthBits = 8;
        public const int HeadingMask = (1 << HeadingBits) - 1, PatternMask = (1 << PatternBits) - 1, LengthMask = (1 << LengthBits) - 1;
        public const int PatternShift = HeadingBits, LengthShift = HeadingBits + PatternBits;

        /// <summary>Pack a heading (any integer number of degrees; it is brought into 0..359), a pattern (0..15) and a
        /// length in metres (0..255, more is clamped) into one int.</summary>
        public static int Pack(int headingDeg, int pattern, int lengthM)
        {
            int heading = ((headingDeg % 360) + 360) % 360;
            int length = math.clamp(lengthM, 0, LengthMask);
            return (heading & HeadingMask) | ((pattern & PatternMask) << PatternShift) | (length << LengthShift);
        }

        public static void Unpack(int packed, out int headingDeg, out int pattern, out int lengthM)
        {
            headingDeg = (packed & HeadingMask) % 360;
            pattern = (packed >> PatternShift) & PatternMask;
            lengthM = (packed >> LengthShift) & LengthMask;
        }

        public static int PatternOf(int packed) => (packed >> PatternShift) & PatternMask;

        /// <summary>The unit XZ direction of a heading: 0 is +Z (up the field), 90 is +X. Deterministic (SimMath).</summary>
        public static float3 Heading(int headingDeg) => SimMath.DirFromYaw(((headingDeg % 360) + 360) % 360 * (SimMath.Pi / 180f));
    }
}
