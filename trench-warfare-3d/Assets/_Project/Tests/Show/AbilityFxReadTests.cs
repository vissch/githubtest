// Phase: ability-fx-captures-r3-2-2-g2-g3 - how big an ability's effect must read at each zoom band.
// ability-fx-captures-r3-2-2: how big an ability's effect must read. A fixed share of the frame is not a fair test of
// a point effect: 3% of an O240 frame is a feature about 85 m across, which no shell burst honestly is. So the floor
// is a ground footprint, capped at the old share of the frame (R0 C1, a measuring choice taken by the coordinator
// 2026-10-19, not the owner; the row in docs/reference/decisions.md).
// g2-g3: the drop is six rows and is NOT measured against the footprint. It is shot three times more with
// CombatFx.DropCanopies off (_nochute), so the probe can measure the parachute canopies ALONE: everything else -
// the biplane's wake, the landing dust - is in both frames and cancels.
// g1: the three captures of a band are THREE DIFFERENT MOMENTS of the drop, not three shots of one frozen tick
// (the owner, 2026-10-23). A rep's still, plate and nochute still share one tick; the three reps of a band must
// carry three distinct ticks, which this test checks. Each drop row must clear two fixed bars, each 0.8 x the
// smallest of the three captures of THIS TREE (parent 1ccd1aa0c8):
//   canopy (still vs nochute)  T1 0.069965 / 0.113245 / 0.079932 -> 0.0559, O240 0.006394 / 0.008280 / 0.005307 -> 0.0042
//   drop   (still vs plate)    T1 0.072413 / 0.098643 / 0.161612 -> 0.0579, O240 0.011187 / 0.020755 / 0.016157 -> 0.0089
using System.Globalization;
using System.IO;
using NUnit.Framework;
using UnityEngine;

namespace TW.Tests
{
    public class AbilityFxReadTests
    {
        /// <summary>The floor from the goal's rule: at least 400 m^2 of ground (about 20 x 20 m), never stricter than
        /// 0.10 of the frame. docs/evidence/ability-fx-captures-r3-2-2-g2-g3-g1/probe.py is the authority - it measures the
        /// pixels and writes probe.json; this is the same arithmetic, held against hand sums below.</summary>
        static float Floor(float frameAreaM2) => Mathf.Min(0.10f, 400f / frameAreaM2);

        const float Tol = 1e-5f;

        /// <summary>The drop's bars a band: 0.8 x the smallest of three captures of this lane's tree (parent
        /// 1ccd1aa0c8), rounded down to four decimals, so each bar sits under the whole spread. Measured, not
        /// derived - the probe's docstring lists the twelve captures. The canopy bar is the owner's test: it is read
        /// off the still against its nochute, so it goes to zero when the canopies are gone however much wake and
        /// dust remain.</summary>
        const float DropBarT1 = 0.0579f;
        const float DropBarO240 = 0.0089f;
        const float CanopyBarT1 = 0.0559f;
        const float CanopyBarO240 = 0.0042f;

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

            string dir = Path.GetFullPath(Application.dataPath + "/../../docs/evidence/ability-fx-captures-r3-2-2-g2-g3-g1");
            string json = Path.Combine(dir, "probe.json");
            Assert.That(File.Exists(json), "probe.json is missing: run probe.py over " + dir);

            var doc = JsonUtility.FromJson<Doc>(File.ReadAllText(json));
            Assert.That(doc, Is.Not.Null, "probe.json did not parse");
            Assert.That(doc.rows, Is.Not.Null.And.Length.EqualTo(12),
                "12 rows were asked for: smoke, barrage and splash at T1 and O240, and the drop three times a band");

            int drops = 0;
            var dropTicks = new System.Collections.Generic.Dictionary<string, System.Collections.Generic.List<int>>();
            foreach (var r in doc.rows)
            {
                string who = r.subject + " " + r.band;
                Assert.That(r.frame_area_m2, Is.GreaterThan(0f), who + ": the framed ground was not measured");
                bool isDrop = r.subject != null && r.subject.StartsWith("drop");
                float floor = isDrop ? (r.band == "T1" ? DropBarT1 : DropBarO240) : Floor(r.frame_area_m2);
                Assert.That(r.effect_area_frac, Is.GreaterThanOrEqualTo(floor - Tol),
                    string.Format(CultureInfo.InvariantCulture,
                        "{0}: effect_area_frac {1:0.000000} is under the {2} {3:0.000000} ({4:0} m^2 of ground framed)",
                        who, r.effect_area_frac, isDrop ? "fixed bar" : "footprint floor", floor, r.frame_area_m2));
                if (isDrop)
                {
                    drops++;
                    float canopyBar = r.band == "T1" ? CanopyBarT1 : CanopyBarO240;
                    Assert.That(r.canopy_area_frac, Is.GreaterThanOrEqualTo(canopyBar - Tol),
                        string.Format(CultureInfo.InvariantCulture,
                            "{0}: canopy_area_frac {1:0.000000} is under the canopy bar {2:0.000000}: the parachute "
                            + "canopies are gone, whatever else the frame still holds", who, r.canopy_area_frac, canopyBar));
                    Assert.That(r.canopy_px, Is.GreaterThanOrEqualTo(150), who + ": too few canopy pixels to see a canopy");
                    if (!dropTicks.ContainsKey(r.band)) dropTicks[r.band] = new System.Collections.Generic.List<int>();
                    dropTicks[r.band].Add(r.sim_tick);
                }
                Assert.That(r.effect_px, Is.GreaterThanOrEqualTo(150), who + ": too few changed pixels to see anything");
                Assert.That(r.pose_error_m, Is.LessThan(0.5f), who + ": the camera was not where it was asked to be");
            }

            Assert.That(drops, Is.EqualTo(6), "the drop is three captures at T1 and three at O240");

            // three different moments, not three shots of one frozen tick (the owner, 2026-10-23)
            foreach (var band in dropTicks)
            {
                var seen = new System.Collections.Generic.HashSet<int>(band.Value);
                Assert.That(seen.Count, Is.EqualTo(3), "drop " + band.Key + ": the three captures are "
                    + seen.Count + " moment(s), not three different ones (ticks " + string.Join(", ", band.Value) + ")");
                Assert.That(seen.Contains(-1), Is.False, "drop " + band.Key + ": a capture has no sim_tick");
            }
        }

        [System.Serializable]
        class Row
        {
            public string subject, band;
            public float effect_area_frac, canopy_area_frac, frame_area_m2, pose_error_m;
            public int effect_px, canopy_px, sim_tick;
        }

        [System.Serializable]
        class Doc { public Row[] rows; }
    }
}
