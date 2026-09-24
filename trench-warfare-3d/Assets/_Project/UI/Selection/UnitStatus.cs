// Phase: B6 (implemented) — what a unit is doing, in one word for the hover card and the selection panel, read from
// the sim's own state (flags, stance, suppression, target, velocity); the worst thing first, so a burning man in a
// trench reads BURNING, not IN TRENCH. For a clump, the two commonest states with their counts ("5 IN TRENCH · 2 PINNED").
// Pure functions over plain values, so the rules are tested without a world.
using System.Collections.Generic;
using TW.Sim;

namespace TW.UI
{
    public enum UnitState : byte
    {
        KnockedOut, Burning, Pinned, Immobilised, Bogged, Stalled, Suppressed,
        Vaulting, Advancing, Engaging, HoldingFire, InTrench, Running, Moving, Waiting,
    }

    public enum StateTone : byte { Normal, Warn, Alarm }

    public static class UnitStatus
    {
        public const float SuppressedAt = 50f, MovingSpeedSq = 0.3f * 0.3f;

        static readonly string[] Words =
        {
            "KNOCKED OUT", "BURNING", "PINNED", "IMMOBILISED", "BOGGED", "STALLED", "SUPPRESSED",
            "VAULTING", "ADVANCING", "ENGAGING", "HOLDING FIRE", "IN TRENCH", "RUNNING", "MOVING", "WAITING",
        };

        public static string Word(UnitState s) => Words[(int)s];

        public static StateTone Tone(UnitState s) =>
            s <= UnitState.Pinned ? StateTone.Alarm : s <= UnitState.Suppressed ? StateTone.Warn : StateTone.Normal;

        /// <summary>One unit's state, worst first. speedSq is the horizontal velocity squared (m²/s²).</summary>
        public static UnitState Of(uint flags, Stance stance, float suppression, bool hasTarget, float speedSq)
        {
            var f = (UnitFlags)flags;
            bool vehicle = (f & UnitFlags.Vehicle) != 0;
            if ((f & UnitFlags.KnockedOut) != 0) return UnitState.KnockedOut;
            if ((f & UnitFlags.Burning) != 0) return UnitState.Burning;
            if (!vehicle && (stance == Stance.Pinned || suppression >= StanceRules.PinnedSuppression)) return UnitState.Pinned;
            if ((f & UnitFlags.Immobilised) != 0) return UnitState.Immobilised;
            if ((f & UnitFlags.Bogged) != 0) return UnitState.Bogged;
            if ((f & UnitFlags.Stalled) != 0) return UnitState.Stalled;
            if (!vehicle && suppression >= SuppressedAt) return UnitState.Suppressed;
            if (stance == Stance.Vault) return UnitState.Vaulting;
            bool moving = speedSq > MovingSpeedSq;
            if ((f & UnitFlags.Exposed) != 0 && moving) return UnitState.Advancing;
            if (hasTarget && (f & UnitFlags.HoldFire) == 0) return UnitState.Engaging;
            if ((f & UnitFlags.HoldFire) != 0) return UnitState.HoldingFire;
            if ((f & UnitFlags.InTrench) != 0) return UnitState.InTrench;
            if (stance == Stance.Sprint) return UnitState.Running;
            return moving ? UnitState.Moving : UnitState.Waiting;
        }

        /// <summary>The state of the unit in this slot now.</summary>
        public static UnitState Of(SimWorld w, int slot)
        {
            var v = w.Velocity[slot];
            return Of(w.Flags[slot], (Stance)w.StanceOf[slot], w.Suppression[slot], w.TargetSlot[slot] >= 0, v.x * v.x + v.z * v.z);
        }

        /// <summary>
        /// A clump's states: the commonest first, the worse on a tie, at most two ("5 IN TRENCH · 2 PINNED"); a single
        /// state for everyone reads as the plain word. counts is indexed by UnitState.
        /// </summary>
        public static string Summary(int[] counts, out StateTone worst)
        {
            worst = StateTone.Normal;
            int total = 0, first = -1, second = -1;
            for (int s = 0; s < counts.Length; s++)
            {
                if (counts[s] == 0) continue;
                total += counts[s];
                var t = Tone((UnitState)s); if (t > worst) worst = t;
                if (first < 0 || counts[s] > counts[first]) { second = first; first = s; }
                else if (second < 0 || counts[s] > counts[second]) second = s;
            }
            if (first < 0) return "";
            if (counts[first] == total) return Word((UnitState)first);
            string text = counts[first] + " " + Word((UnitState)first);
            if (second >= 0) text += "  ·  " + counts[second] + " " + Word((UnitState)second);
            return text;
        }

        public static readonly int StateCount = Words.Length;
    }
}
