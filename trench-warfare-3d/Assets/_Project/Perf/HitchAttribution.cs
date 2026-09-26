// Phase: tooling (AOSA C64, 2026-09-26) - which TW marker carried a hitch. PerfBench kept only the frame, tick and ms of
// each frame over 33 ms, so runs/8 ta0-1's 19 hitches had no carrier. On a hitch frame PerfBench hands this the frame's
// marker values (the recorders' LastValue, the same frame unscaledDeltaTime measured) and keeps the largest; the report
// then says, per marker, in how many hitches it was the largest and what median share of the hitch's ms it held.
// TW markers nest (TW.Host.Update holds TW.Sim.Step, TW.Hud.Late holds TW.Hud.Refresh), so the largest is always an
// outermost one: this names the top-level carrier, not the line of code inside it.
// Pure logic, no Unity calls: HitchAttributionTests pins it.
using System;
using System.Collections.Generic;

namespace TW.Perf
{
    public static class HitchAttribution
    {
        /// <summary>The index of the largest of the first n values whose eligible flag is set, or -1 when none is above 0
        /// (a release player, whose markers compile out, reads 0 everywhere). NaN and infinities never win; a tie goes to
        /// the lower index, so the pick does not depend on anything but the values. Allocation-free: it runs in the
        /// window, on hitch frames.</summary>
        public static int Largest(double[] values, bool[] eligible, int n)
        {
            if (values == null || eligible == null) return -1;
            n = Math.Min(n, Math.Min(values.Length, eligible.Length));
            int best = -1;
            double bestV = 0.0;
            for (int i = 0; i < n; i++)
            {
                if (!eligible[i]) continue;
                double v = values[i];
                if (double.IsNaN(v) || double.IsInfinity(v) || v <= bestV) continue;
                best = i; bestV = v;
            }
            return best;
        }

        public readonly struct Carrier
        {
            public readonly string Marker;
            /// <summary>Hitches in which this marker was the largest.</summary>
            public readonly int Hitches;
            /// <summary>The median, over those hitches, of the marker's ms over the hitch's ms (0..1, or above 1 if a
            /// marker's frame and the hitch's frame were measured a little differently).</summary>
            public readonly double MedianShare;
            public Carrier(string marker, int hitches, double medianShare) { Marker = marker; Hitches = hitches; MedianShare = medianShare; }
        }

        /// <summary>Per carrier: how many of the first n hitches it was the largest marker in, and its median share of
        /// their ms. Hitches with no marker (null or empty name) or no positive hitch ms are left out. Ordered by hitches
        /// (most first), then median share (largest first), then name (ordinal), so two runs list them the same way.</summary>
        public static List<Carrier> Summarise(string[] marker, double[] markerMs, double[] hitchMs, int n)
        {
            var shares = new Dictionary<string, List<double>>(StringComparer.Ordinal);
            if (marker != null && markerMs != null && hitchMs != null)
            {
                n = Math.Min(n, Math.Min(marker.Length, Math.Min(markerMs.Length, hitchMs.Length)));
                for (int i = 0; i < n; i++)
                {
                    if (string.IsNullOrEmpty(marker[i]) || !(hitchMs[i] > 0.0) || double.IsNaN(markerMs[i])) continue;
                    if (!shares.TryGetValue(marker[i], out var list)) shares[marker[i]] = list = new List<double>();
                    list.Add(markerMs[i] / hitchMs[i]);
                }
            }
            var result = new List<Carrier>(shares.Count);
            foreach (var kv in shares) result.Add(new Carrier(kv.Key, kv.Value.Count, Median(kv.Value)));
            result.Sort((a, b) =>
            {
                int c = b.Hitches.CompareTo(a.Hitches);
                if (c != 0) return c;
                c = b.MedianShare.CompareTo(a.MedianShare);
                return c != 0 ? c : string.CompareOrdinal(a.Marker, b.Marker);
            });
            return result;
        }

        /// <summary>The middle value, or the mean of the two middle values; NaN for none.</summary>
        public static double Median(List<double> v)
        {
            if (v == null || v.Count == 0) return double.NaN;
            var s = new List<double>(v);
            s.Sort();
            int m = s.Count / 2;
            return s.Count % 2 == 1 ? s[m] : (s[m - 1] + s[m]) * 0.5;
        }
    }
}
