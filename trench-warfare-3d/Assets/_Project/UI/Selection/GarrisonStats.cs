// Phase: B6 (implemented) — who is in each of our trenches and in what state, for the trench's order buttons: the
// garrison badge turns amber when a third of the men are shaken or pinned and red when half are pinned (pinned men
// refuse over the top, StanceRules.PinnedSuppression), and hovering the badge shows the garrison's card (HoverCard).
// One pass over the units four times a second; flat arrays, nothing allocated after the first pass.
using UnityEngine;
using TW.Sim;

namespace TW.UI
{
    public sealed class GarrisonStats
    {
        public const float RefreshSeconds = 0.25f;
        const int Archetypes = 256;

        public enum Morale : byte { Steady, Shaken, Pinned }

        int trenches = -1;
        int[] men, states, typeCounts, common, types;
        float[] hpSum;
        float next;

        public int Trenches => trenches;
        public int Men(int t) => men[t];
        public int CountOf(int t, UnitState s) => states[t * UnitStatus.StateCount + (int)s];
        public float MeanHp(int t) => men[t] > 0 ? hpSum[t] / men[t] : 0f;
        public byte Commonest(int t) => (byte)common[t];
        public int Types(int t) => types[t];

        /// <summary>The badge's colour: pinned when half are pinned, shaken when a third are pinned or suppressed.</summary>
        public Morale MoraleOf(int t) => Grade(men[t], CountOf(t, UnitState.Pinned), CountOf(t, UnitState.Suppressed));

        public static Morale Grade(int men, int pinned, int suppressed)
        {
            if (men <= 0) return Morale.Steady;
            if (pinned * 2 >= men) return Morale.Pinned;
            if ((pinned + suppressed) * 3 >= men) return Morale.Shaken;
            return Morale.Steady;
        }

        /// <summary>This trench's state counts into a UnitStatus-sized array (for UnitStatus.Summary).</summary>
        public void CopyStates(int t, int[] into) => System.Array.Copy(states, t * UnitStatus.StateCount, into, 0, UnitStatus.StateCount);

        public void Refresh(SimWorld w, int trenchCount, bool force = false)
        {
            if (w == null) return;
            if (!force && trenchCount == trenches && Time.unscaledTime < next) return;
            next = Time.unscaledTime + RefreshSeconds;
            if (trenchCount != trenches)
            {
                trenches = trenchCount;
                men = new int[trenches]; hpSum = new float[trenches]; common = new int[trenches]; types = new int[trenches];
                states = new int[trenches * UnitStatus.StateCount]; typeCounts = new int[trenches * Archetypes];
            }
            System.Array.Clear(men, 0, men.Length); System.Array.Clear(hpSum, 0, hpSum.Length);
            System.Array.Clear(states, 0, states.Length); System.Array.Clear(typeCounts, 0, typeCounts.Length);
            for (int i = 0; i < w.HighWater; i++)
            {
                if ((w.Flags[i] & (uint)UnitFlags.Alive) == 0 || (w.Team[i] & 1) != 0) continue;
                int t = w.TrenchId[i];
                if (t < 0 || t >= trenches) continue;
                men[t]++;
                hpSum[t] += Mathf.Clamp01(w.Hp[i] / Mathf.Max(1f, w.MaxHp[i]));
                states[t * UnitStatus.StateCount + (int)UnitStatus.Of(w, i)]++;
                typeCounts[t * Archetypes + w.Archetype[i]]++;
            }
            for (int t = 0; t < trenches; t++)
            {
                int best = 0, n = 0;
                for (int a = 0; a < Archetypes; a++)
                {
                    int c = typeCounts[t * Archetypes + a];
                    if (c == 0) continue;
                    n++;
                    if (c > typeCounts[t * Archetypes + best]) best = a;
                }
                common[t] = best; types[t] = n;
            }
        }
    }
}
