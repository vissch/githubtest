// Phase: deaths (2026-09-28, implemented) — what a shell takes off a man when fx.deathAbsurd is above 0, decided before
// anything is thrown (CombatFx.Gibs throws it): the limbs his corpse loses first, then exactly those parts fly, so what
// is in the air is what the body on the ground is missing. In a heap of four or more he may be blown apart (every limb
// off, the corpse a trunk) or torn in two (no corpse: both halves fly, the helmet on the upper one) — together at most
// half the time, and far less alone. His helmet, rifle and pack are kit: they fly at GORE 0, when nothing of him does.
// At intensity 0 nothing here decides: CombatFx.Gibs throws today's dice as it always has (Legacy).
// Pure and seeded (the death's seed; decisions.md 2026-09-26: presentation only, so replays agree), tested by GibPlanTests.
using System.Collections.Generic;
using Piece = TW.Presentation.Tactical.DebrisRenderer.Piece;

namespace TW.Presentation.Tactical
{
    public struct GibPlan
    {
        /// <summary>The limbs his corpse loses, as the VAT shader reads them: bit 1 the head, 2 and 3 the arms, 4 and 5
        /// the legs.</summary>
        public int Mask;
        /// <summary>Torn in two: no corpse is laid; the upper and the lower half fly. Blown apart: every limb is off.</summary>
        public bool Torn, Apart;
        public bool Helmet, Rifle, Pack;
        /// <summary>Lumps of gore thrown with him.</summary>
        public int Lumps;
        /// <summary>Intensity 0: nothing was decided here; today's dice are thrown (CombatFx.Gibs).</summary>
        public bool Legacy;
        /// <summary>He comes down whole: nothing of him or his kit flies.</summary>
        public bool Whole => !Legacy && Mask == 0 && !Torn && !Helmet && !Rifle && !Pack;

        public const int Head = 1 << 1, LeftArm = 1 << 2, RightArm = 1 << 3, LeftLeg = 1 << 4, RightLeg = 1 << 5;
        public const int AllLimbs = Head | LeftArm | RightArm | LeftLeg | RightLeg;
        /// <summary>A bit above the VAT mask, set in what CombatFx.Gibs returns when he was torn in two: the death lays no
        /// corpse (stripped before the mask goes to the renderer).</summary>
        public const int TornBit = 1 << 7;
        /// <summary>How many died beside him for a heap of four or more; the most of heaps and of one man alone that end
        /// torn in two or blown apart.</summary>
        public const int HeapOfFour = 3;
        public const float HeapBurstCap = 0.5f, AloneBurstCap = 0.15f;
        /// <summary>How much larger than life his parts are drawn, at an intensity (critic round 1: at life size an arm was
        /// a few pixels at the play zoom and read as mud; round 4: at 1.3 still none could be pointed at): 1.6 at 1, 2.2 at 2.</summary>
        public static float PartScale(float intensity) => 1f + 0.6f * System.Math.Min(2f, System.Math.Max(0f, intensity));
        /// <summary>GORE below this: one limb at most, no head, never torn or blown apart.</summary>
        public const float FullGore = 0.5f;

        static float Dice(uint seed, uint salt)
        {
            uint h = seed ^ (salt * 2246822519u); h ^= h >> 15; h *= 2654435761u; h ^= h >> 13; h *= 3266489917u; h ^= h >> 16;
            return (h & 0xFFFFFF) / 16777216f;
        }

        /// <summary>The share of deaths torn in two or blown apart at an intensity, beside `density` others.</summary>
        public static float BurstShare(float intensity, int density)
            => density >= HeapOfFour ? System.Math.Min(HeapBurstCap, 0.5f * intensity) : System.Math.Min(AloneBurstCap, 0.08f * intensity);

        /// <summary>What a shell takes off him, at intensity `a` and GORE `gore`, with `density` dead beside him.</summary>
        public static GibPlan Decide(uint seed, float a, float gore, int density)
        {
            if (a <= 0f) return new GibPlan { Legacy = true };
            var plan = new GibPlan();
            density = System.Math.Max(0, density);
            // most men a shell throws alone come down whole (as today), fewer at the new look, few in a heap
            if (Dice(seed, 1) < 0.3f / (1f + density) * (1f - 0.5f * System.Math.Min(1f, a))) return plan;
            plan.Rifle = Dice(seed, 2) < 0.6f;
            plan.Pack = Dice(seed, 3) < 0.4f;
            if (gore <= 0f) { plan.Helmet = true; return plan; }   // kit only: nothing of him
            bool full = gore >= FullGore;
            if (full && Dice(seed, 4) < BurstShare(a, density))
            {
                if (Dice(seed, 5) < 0.5f) { plan.Torn = true; plan.Helmet = false; }   // the helmet stays on the upper half
                else { plan.Apart = true; plan.Mask = AllLimbs; plan.Helmet = true; }
                plan.Lumps = Round(10f * gore * (1f + 0.5f * density));
                return plan;
            }
            plan.Helmet = true;
            int limbs = (Dice(seed, 6) < 0.35f ? 2 : 1) + System.Math.Min(density, 2) + (a > 1f && Dice(seed, 7) < a - 1f ? 1 : 0);
            if (!full) limbs = 1;
            for (int k = 0; k < limbs; k++)
            {
                int limb = 2 + (int)(Dice(seed, 10u + (uint)k) * 3.999f);   // an arm or a leg
                plan.Mask |= 1 << limb;
            }
            if (full && Dice(seed, 8) < 0.22f + 0.12f * density + 0.1f * System.Math.Min(a, 2f)) plan.Mask |= Head;
            plan.Lumps = Round(5f * gore * (1f + 0.5f * density));
            return plan;
        }

        static int Round(float x) => (int)System.Math.Round(x);

        /// <summary>The parts of him and his kit the plan throws, in the order CombatFx.Gibs throws them: a part for each
        /// limb lost, the halves if torn, then the helmet, the rifle, the pack. The gore lumps are not listed.</summary>
        public static void Pieces(in GibPlan plan, List<Piece> into)
        {
            into.Clear();
            if (plan.Legacy) return;
            if ((plan.Mask & LeftArm) != 0) into.Add(Piece.Arm);
            if ((plan.Mask & RightArm) != 0) into.Add(Piece.Arm);
            if ((plan.Mask & LeftLeg) != 0) into.Add(Piece.Leg);
            if ((plan.Mask & RightLeg) != 0) into.Add(Piece.Leg);
            if ((plan.Mask & Head) != 0) into.Add(Piece.Head);
            if (plan.Torn) { into.Add(Piece.UpperHalf); into.Add(Piece.LowerHalf); }
            if (plan.Helmet) into.Add(Piece.Helm);
            if (plan.Rifle) into.Add(Piece.Rifle);
            if (plan.Pack) into.Add(Piece.Pack);
        }
    }
}
