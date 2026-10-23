// Phase: ability-fx-captures-r3-2-2-g2-g1-g2 - how big an ability's effect must read at each zoom band.
// ability-fx-captures-r3-2-2: how big an ability's effect must read. A fixed share of the frame is not a fair test of
// a point effect: 3% of an O240 frame is a feature about 85 m across, which no shell burst honestly is. So the floor
// is a ground footprint, capped at the old share of the frame (R0 C1, a measuring choice taken by the coordinator
// 2026-10-19, not the owner; the row in docs/reference/decisions.md).
// g2: the drop is six rows, three captures a band, and is NOT measured against the footprint. Its plate now holds
// the same men with the drop's own look and the beams off, so the diff is the biplane's wake, the canopies and the
// landing dust - a drawing a ground footprint asks the wrong question of. Its bar is a fixed number a band, measured
// from the three captures of THIS LANE'S TREE, not a sum done the way the probe does it.
// g2-g1: the three captures are now three editor runs on ONE sim tick, shot 220 frames after the call - the landing
// dust at its fullest, a cloud a player can see - instead of three blocks of one run at three ticks, of which the
// first was a faint haze. The spread is now 0.03%, so the bar is 0.98 x the smallest, not 0.8 x:
// T1 0.136472 / 0.136510 / 0.136480 -> 0.1337, O240 0.013163 / 0.013165 / 0.013173 -> 0.0129.
// g2-g1-g2: smoke, barrage and splash are now shot three times a band too, each rep its own editor run on its own
// sim tick, so the same-tick check that guarded only the drop now guards all four subjects. Each of the three is
// also held against its own spread bar (0.98 x the smallest of its three captures of this lane's tree, same
// arithmetic as the drop's), alongside the unchanged ground footprint floor - whichever of the two is stricter
// wins; on this lane's captures the spread bar wins every time. splash_T1 and splash_O240 must also read as a blue
// column: the mean blue shift of the moved pixels must be more than twice the mean red shift (the owner's words;
// probe.py's docstring has the measured gap to the barrage's earth-brown and the smoke's grey). 24 rows: 3 subjects
// x 3 reps x 2 bands, plus the drop's existing 6.
using System.Globalization;
using System.IO;
using NUnit.Framework;
using UnityEngine;

namespace TW.Tests
{
    public class AbilityFxReadTests
    {
        /// <summary>The floor from the goal's rule: at least 400 m^2 of ground (about 20 x 20 m), never stricter than
        /// 0.10 of the frame. docs/evidence/ability-fx-captures-r3-2-2-g2-g1-g2/probe.py is the authority - it measures
        /// the pixels and writes probe.json; this is the same arithmetic, held against hand sums below.</summary>
        static float Floor(float frameAreaM2) => Mathf.Min(0.10f, 400f / frameAreaM2);

        const float Tol = 1e-5f;

        /// <summary>The drop's bar a band: 0.98 x the smallest of three captures of this lane's tree, rounded down to
        /// four decimals, so the bar sits just under the whole spread (which is 0.03% wide). Measured, not derived -
        /// the probe's docstring lists the six captures.</summary>
        const float DropBarT1 = 0.1337f;
        const float DropBarO240 = 0.0129f;

        /// <summary>Smoke, barrage and splash each get the same kind of bar as the drop, 0.98 x the smallest of their
        /// own three captures a band, rounded down to four decimals. Measured, not derived - the probe's docstring
        /// has all nine captures. Held alongside the unchanged ground footprint floor; whichever is stricter wins.</summary>
        static float SpreadBar(string subject, string band)
        {
            switch (subject)
            {
                case "smoke": return band == "T1" ? 0.1621f : 0.0318f;
                case "barrage": return band == "T1" ? 0.2126f : 0.0156f;
                case "splash": return band == "T1" ? 0.0883f : 0.0030f;
                default: throw new System.ArgumentException(subject);
            }
        }

        /// <summary>A splash row's mean blue shift of its moved pixels must be more than this many times its mean red
        /// shift to read as the owner's "one blue column" (probe.py's docstring has the measured gap to the barrage's
        /// earth-brown and the smoke's grey).</summary>
        const float SplashBlueFactor = 2.0f;

        // [ability-fx-captures-r3-2-2]
        [Test]
        public void AbilityFxReadAtEveryBand()
        {
            // the cap and the rule meet at 4000 m^2: 400 / 4000 is exactly 0.10
            Assert.That(Floor(4000f), Is.EqualTo(0.10f).Within(Tol), "4000 m^2 framed: the cap and the footprint meet");
            // tighter than 4000 and the cap holds, so a close band is never asked for more than it was before
            Assert.That(Floor(1600f), Is.EqualTo(0.10f).Within(Tol), "1600 m^2 framed: the cap holds");
            // wider than 4000 and the footprint rules: 400 / 8000, 400 / 40000
            Assert.That(Floor(8000f), Is.EqualTo(0.05f).Within(Tol), "8000 m^2 framed: 400 m^2 is a twentieth of it");
            Assert.That(Floor(40000f), Is.EqualTo(0.01f).Within(Tol), "40000 m^2 framed: 400 m^2 is a hundredth of it");

            string dir = Path.GetFullPath(Application.dataPath + "/../../docs/evidence/ability-fx-captures-r3-2-2-g2-g1-g2");
            string json = Path.Combine(dir, "probe.json");
            Assert.That(File.Exists(json), "probe.json is missing: run probe.py over " + dir);

            var doc = JsonUtility.FromJson<Doc>(File.ReadAllText(json));
            Assert.That(doc, Is.Not.Null, "probe.json did not parse");
            Assert.That(doc.rows, Is.Not.Null.And.Length.EqualTo(24),
                "24 rows were asked for: smoke, barrage, splash and the drop, each three reps at T1 and O240");

            var subjectCounts = new System.Collections.Generic.Dictionary<string, int>();
            foreach (var r in doc.rows)
            {
                string who = r.subject + " " + r.band;
                Assert.That(r.frame_area_m2, Is.GreaterThan(0f), who + ": the framed ground was not measured");
                string baseSubject = r.subject != null && r.subject.Contains("_") ? r.subject.Substring(0, r.subject.LastIndexOf('_')) : r.subject;
                bool isDrop = baseSubject == "drop";
                float floor;
                if (isDrop) floor = r.band == "T1" ? DropBarT1 : DropBarO240;
                else floor = Mathf.Max(SpreadBar(baseSubject, r.band), Floor(r.frame_area_m2));
                Assert.That(r.effect_area_frac, Is.GreaterThanOrEqualTo(floor - Tol),
                    string.Format(CultureInfo.InvariantCulture,
                        "{0}: effect_area_frac {1:0.000000} is under the bar {2:0.000000} ({3:0} m^2 of ground framed)",
                        who, r.effect_area_frac, floor, r.frame_area_m2));
                subjectCounts.TryGetValue(baseSubject, out int count);
                subjectCounts[baseSubject] = count + 1;
                Assert.That(r.effect_px, Is.GreaterThanOrEqualTo(150), who + ": too few changed pixels to see anything");
                Assert.That(r.pose_error_m, Is.LessThan(0.5f), who + ": the camera was not where it was asked to be");

                if (baseSubject == "splash")
                {
                    Assert.That(r.shift_rgb, Is.Not.Null.And.Length.EqualTo(3), who + ": no shift_rgb for the splash");
                    Assert.That(r.shift_rgb[2], Is.GreaterThan(SplashBlueFactor * r.shift_rgb[0]),
                        string.Format(CultureInfo.InvariantCulture,
                            "{0}: mean blue shift {1:0.0} is not more than {2:0}x the mean red shift {3:0.0} (not a blue column)",
                            who, r.shift_rgb[2], SplashBlueFactor, r.shift_rgb[0]));
                }
            }

            foreach (var subject in new[] { "smoke", "barrage", "splash", "drop" })
            {
                subjectCounts.TryGetValue(subject, out int count);
                Assert.That(count, Is.EqualTo(6), subject + ": three reps at T1 and three at O240");
            }
        }

        [System.Serializable]
        class Row
        {
            public string subject, band;
            public float effect_area_frac, frame_area_m2, pose_error_m;
            public int effect_px;
            public float[] shift_rgb;
        }

        [System.Serializable]
        class Doc { public Row[] rows; }
    }
}
