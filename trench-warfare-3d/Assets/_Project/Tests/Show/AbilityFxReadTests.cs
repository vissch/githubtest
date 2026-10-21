// Phase: ability-fx-captures-r3-2-2-g2-g2-g2 - how big an ability's effect must read at each zoom band.
// ability-fx-captures-r3-2-2: how big an ability's effect must read. A fixed share of the frame is not a fair test of
// a point effect: 3% of an O240 frame is a feature about 85 m across, which no shell burst honestly is. So the floor
// is a ground footprint, capped at the old share of the frame (R0 C1, a measuring choice taken by the coordinator
// 2026-10-19, not the owner; the row in docs/reference/decisions.md).
// g2: the drop is six rows, three captures a band, and is NOT measured against the footprint. Its plate now holds
// the same men with the drop's own look and the beams off, so the diff is the biplane's wake, the canopies and the
// landing dust - a drawing a ground footprint asks the wrong question of. Its bar is a fixed number a band, 0.8 x
// the smallest of the three captures of THIS LANE'S TREE (parent 849d908a2b), not a sum done the way the probe does
// it: T1 0.096479 / 0.099553 / 0.099528 -> 0.0771.
// g2-g2: the three O240 drop captures are three separate editor runs of the SAME sim tick 199 (the earlier three sat
// at ticks 199, 695 and 1191 of one match, so their plumes piled up in a frame that holds the whole map). All three
// read 0.020909, so the spread is 0, and the O240 bar is the smallest less twice the spread, never less than 0.0002
// under it, rounded down: floor_4(0.020909 - 0.0002) = 0.0207. T1 is untouched.
// g2-g2-g2: the close splash is a crown of water now (the owner, in the goal), so its look changed and had to be
// measured again: it is six rows too, three captures a band of this build, and it answers to BOTH its footprint floor
// and a bar outside the spread of those three - max(footprint, splash bar). The splash bar is the drop's T1 rule,
// 0.8 x the smallest of the three, because the three reps are of one match at different ticks: T1 0.098052 /
// 0.103355 / 0.106249 -> 0.0784, O240 0.010805 / 0.011056 / 0.011756 -> 0.0086. Smoke, the barrage and the drop keep
// their captures and their rules unchanged.
using System.Globalization;
using System.IO;
using NUnit.Framework;
using UnityEngine;

namespace TW.Tests
{
    public class AbilityFxReadTests
    {
        /// <summary>The floor from the goal's rule: at least 400 m^2 of ground (about 20 x 20 m), never stricter than
        /// 0.10 of the frame. docs/evidence/ability-fx-captures-r3-2-2-g2-g2/probe.py is the authority - it measures the
        /// pixels and writes probe.json; this is the same arithmetic, held against hand sums below.</summary>
        static float Floor(float frameAreaM2) => Mathf.Min(0.10f, 400f / frameAreaM2);

        const float Tol = 1e-5f;

        /// <summary>The drop's bar a band. T1: 0.8 x the smallest of three captures of one match (parent
        /// 849d908a2b). O240: three captures of one sim tick (199), each its own editor run on this lane's tree,
        /// all reading 0.020909, so the bar is the smallest less twice the spread (0) but never less than 0.0002
        /// under it. Measured, not derived - the probe's docstring lists every capture.</summary>
        const float DropBarT1 = 0.0771f;
        const float DropBarO240 = 0.0207f;

        /// <summary>The splash's bar a band: 0.8 x the smallest of this build's three captures of that band, rounded
        /// down to four decimals (the drop's T1 rule - the three reps are of one match at different ticks, not of one
        /// tick). The splash answers to its ground footprint AS WELL, so the probe holds it against
        /// max(footprint, bar); at T1 the footprint (about 0.0636) is the stricter of the two, at O240 this bar is.</summary>
        const float SplashBarT1 = 0.0784f;
        const float SplashBarO240 = 0.0086f;

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

            string dir = Path.GetFullPath(Application.dataPath + "/../../docs/evidence/ability-fx-captures-r3-2-2-g2-g2-g2");
            string json = Path.Combine(dir, "probe.json");
            Assert.That(File.Exists(json), "probe.json is missing: run probe.py over " + dir);

            var doc = JsonUtility.FromJson<Doc>(File.ReadAllText(json));
            Assert.That(doc, Is.Not.Null, "probe.json did not parse");
            Assert.That(doc.rows, Is.Not.Null.And.Length.EqualTo(16),
                "16 rows were asked for: smoke and barrage at T1 and O240, and the drop and the splash three times a band");

            int drops = 0, splashes = 0;
            foreach (var r in doc.rows)
            {
                string who = r.subject + " " + r.band;
                Assert.That(r.frame_area_m2, Is.GreaterThan(0f), who + ": the framed ground was not measured");
                bool isDrop = r.subject != null && r.subject.StartsWith("drop");
                bool isSplash = r.subject != null && r.subject.StartsWith("splash");
                // the splash answers to both: the ground it churns and a bar outside the spread of this build's three
                float floor = isDrop ? (r.band == "T1" ? DropBarT1 : DropBarO240)
                            : isSplash ? Mathf.Max(Floor(r.frame_area_m2), r.band == "T1" ? SplashBarT1 : SplashBarO240)
                            : Floor(r.frame_area_m2);
                Assert.That(r.effect_area_frac, Is.GreaterThanOrEqualTo(floor - Tol),
                    string.Format(CultureInfo.InvariantCulture,
                        "{0}: effect_area_frac {1:0.000000} is under the {2} {3:0.000000} ({4:0} m^2 of ground framed)",
                        who, r.effect_area_frac,
                        isDrop ? "fixed bar" : isSplash ? "footprint floor and fixed bar" : "footprint floor",
                        floor, r.frame_area_m2));
                if (isDrop) drops++;
                if (isSplash) splashes++;
                Assert.That(r.effect_px, Is.GreaterThanOrEqualTo(150), who + ": too few changed pixels to see anything");
                Assert.That(r.pose_error_m, Is.LessThan(0.5f), who + ": the camera was not where it was asked to be");
            }

            Assert.That(drops, Is.EqualTo(6), "the drop is three captures at T1 and three at O240");
            Assert.That(splashes, Is.EqualTo(6), "the splash is three captures at T1 and three at O240");

            // the O240 bar sits just under one number, so the three captures it was read off must be of one moment
            int tick = -1;
            foreach (var r in doc.rows)
            {
                if (r.subject == null || !r.subject.StartsWith("drop") || r.band != "O240") continue;
                if (tick < 0) tick = r.sim_tick;
                Assert.That(r.sim_tick, Is.EqualTo(tick),
                    "the three O240 drop captures must be of one sim tick: " + r.subject + " is on " + r.sim_tick
                    + ", the first on " + tick);
            }
            Assert.That(tick, Is.GreaterThan(0), "no O240 drop row carried a sim_tick");
        }

        [System.Serializable]
        class Row
        {
            public string subject, band;
            public float effect_area_frac, frame_area_m2, pose_error_m;
            public int effect_px, sim_tick;
        }

        [System.Serializable]
        class Doc { public Row[] rows; }
    }
}
