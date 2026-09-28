// Phase: deaths (2026-09-28, implemented) — what a VatInstance's Tint carries. VatPad's 24 bits are full, so a fallen
// man's death gag (DeathGags) rides in Tint above the team and the tumble pitch that were there before:
//   bit 0      the team (0 / 1)
//   bits 1-5   the pitch step, end over end, 32 a turn (VATRenderer.PitchSteps)
//   bits 6-10  the roll step, side over side (a cartwheel), 32 a turn
//   bits 11-16 the squash q, 6 bits two's complement (-32..31): drawn height x (1 + q x SquashStep), width kept by volume
//   bit 17     turn and squash about his feet (a man lying down, a plank toppling) instead of his middle (0.9 m)
// Every new field is 0 on a living man, so his Tint is his team exactly as before, and a fallen man with no gag is
// team + 2 x pitch step as before. The largest value is 262,143, well inside the 2^24 a float holds exactly.
// VAT_URP.shader decodes the same arithmetic; VatTintTests holds the two together.
namespace TW.Presentation.Units
{
    public static class VatTint
    {
        public const int PitchShift = 2, RollShift = 64, SquashShift = 2048, FeetShift = 131072;
        /// <summary>The drawn height is 1 + q x this: q = -32 is 0.12 (a man under a track), q = +13 is 1.36.</summary>
        public const float SquashStep = 0.0275f;
        public const int SquashMin = -32, SquashMax = 31;

        public static float Pack(int team, int pitch, int roll = 0, int squash = 0, bool feet = false)
        {
            int q = squash < SquashMin ? SquashMin : squash > SquashMax ? SquashMax : squash;
            return (team & 1) + PitchShift * (pitch & 31) + RollShift * (roll & 31) + SquashShift * (q & 63) + (feet ? FeetShift : 0);
        }

        /// <summary>The same float arithmetic as VAT_URP.shader, so a test can check what the shader will read.</summary>
        public static void Unpack(float tint, out int team, out int pitch, out int roll, out int squash, out bool feet)
        {
            float gag = (float)System.Math.Floor(tint / 64f);
            float low = tint - gag * 64f;
            team = (int)(low % 2f);
            pitch = (int)System.Math.Floor(low * 0.5f);
            roll = (int)(gag % 32f);
            int q = (int)(System.Math.Floor(gag / 32f) % 64f);
            squash = q > 31 ? q - 64 : q;
            feet = System.Math.Floor(gag / 2048f) > 0.5f;
        }

        /// <summary>The drawn height factor a squash code stands for.</summary>
        public static float Height(int squash) => 1f + squash * SquashStep;

        /// <summary>The nearest squash code to a height factor (1 = none), clamped to what fits.</summary>
        public static int SquashOf(float height)
        {
            int q = (int)System.Math.Round((height - 1f) / SquashStep);
            return q < SquashMin ? SquashMin : q > SquashMax ? SquashMax : q;
        }
    }
}
