// Phase: B6 (implemented) — the control groups tell you when they are in trouble (owner, 2026-09-24): a group that
// loses a quarter of its men (at least two) inside a few seconds is HIT and its chip flashes red; one with half its men
// pinned is PINNED and its chip holds amber; one with nobody left is LOST and its chip greys out for a while before it
// goes. A click on an alerting chip takes you there. Watching starts again whenever the group is reassigned.
// Pure bookkeeping over (strength, pinned, time): no Unity objects, so the rules are tested without a scene.
namespace TW.UI
{
    public sealed class GroupAlerts
    {
        public enum Alert : byte { None, Pinned, Hit, Lost }

        public const float WindowSeconds = 8f, HitShowSeconds = 5f, LostShowSeconds = 8f;
        public const int MinHitLosses = 2, MaxLossesKept = 64;

        sealed class Watch
        {
            public int Stamp = -1, Strength, Last, Pinned, Head, Count;
            public readonly float[] LossAt = new float[MaxLossesKept];
            public float HitUntil = -1f, LostUntil = -1f;
        }

        readonly Watch[] watches = new Watch[SelectionModel.GroupCount];

        public GroupAlerts() { for (int g = 0; g < watches.Length; g++) watches[g] = new Watch(); }

        /// <summary>
        /// This group's state now. stamp is SelectionModel.GroupStamp (a new stamp means new men: start again), alive how
        /// many of its men live, pinned how many of those are pinned, now the unscaled time in seconds.
        /// </summary>
        public void Observe(int g, int stamp, int alive, int pinned, float now)
        {
            var w = watches[g];
            if (stamp != w.Stamp)
            {
                w.Stamp = stamp; w.Strength = w.Last = alive; w.Pinned = pinned;
                w.Head = w.Count = 0; w.HitUntil = w.LostUntil = -1f;
                return;
            }
            for (int k = alive; k < w.Last; k++)   // each man lost since the last look
            {
                w.LossAt[w.Head] = now; w.Head = (w.Head + 1) % MaxLossesKept; if (w.Count < MaxLossesKept) w.Count++;
            }
            if (alive < w.Last)
            {
                int threshold = System.Math.Max(MinHitLosses, (w.Strength + 3) / 4);
                if (LossesWithin(w, now) >= threshold) w.HitUntil = now + HitShowSeconds;
                if (alive == 0 && w.Strength > 0) w.LostUntil = now + LostShowSeconds;
            }
            w.Last = alive; w.Pinned = pinned;
        }

        static int LossesWithin(Watch w, float now)
        {
            int n = 0;
            for (int i = 0; i < w.Count; i++) if (now - w.LossAt[i] <= WindowSeconds) n++;
            return n;
        }

        /// <summary>The worst thing true of the group now: lost, then hit, then pinned.</summary>
        public Alert Of(int g, float now)
        {
            var w = watches[g];
            if (w.Stamp < 0) return Alert.None;
            if (w.Last == 0) return w.Strength > 0 && now < w.LostUntil ? Alert.Lost : Alert.None;
            if (now < w.HitUntil) return Alert.Hit;
            if (w.Pinned * 2 >= w.Last) return Alert.Pinned;
            return Alert.None;
        }

        /// <summary>Men the group has lost within the window (for the chip's tooltip-free readout).</summary>
        public int RecentLosses(int g, float now) => LossesWithin(watches[g], now);

        /// <summary>The group's strength when it was last assigned.</summary>
        public int Strength(int g) => watches[g].Strength;
    }
}
