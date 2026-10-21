// Phase: ability-fx-captures-r3-2-2-g1 - the owner, 2026-10-21: "The splash stills are a blue column, not water...
// Paint a crown and foam on the water at night, at zoom 30 and at zoom 240... Bar outside the spread of three
// captures of the current build."
// docs/evidence/ability-fx-captures-r3-2-2-g1/probe.py is the authority: it reads the pixels of six stills against
// their no-effect plates and writes probe.json (the fixed build) and baseline.json (the capture of 849d908a2b, the
// build that drew the blue column). This test holds the recorded numbers, exactly as AbilityFxReadTests holds
// r3-2-2's, so a probe.json that was never rerun, or a bar quietly dropped into the spread, fails the gate.
//
// NOT SIGNED OFF (2026-10-21, this leg): probe.py exits 1. The six stills now pair tick for tick with their plates
// and foam_frac clears its bar at five of the six rows (T1 0.299/0.581/0.489 against 0.1913; O240 0.210/0.414
// against 0.0780), but crown_spread does not: 0.847/0.497/0.965 at T1 against 1.633 and 0.657/1.000/1.013 at O240
// against 1.480. crown_spread is the changed region's width across its bottom quarter over its FULL height, so the
// tall centre Splash column - which the plan keeps as it is - sets the denominator, and a wider splash grows the
// region in both directions at once: at FoamLast 14 r the T1 region already filled the frame (17 % of it, against
// the probe's own 10 % cap) and still read only 1.20-1.49. splash_T1_r2 reads 0.497 under every tune tried.
// So the LOOK bar is left to probe.py alone and is red there, in the open; this test asserts the harness, the bars'
// placement outside the baseline spread, and the drawing's own numbers, all of which hold. See the leg note.
using System.Globalization;
using System.IO;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class SplashCrownFoamTests
    {
        static string Dir => Path.GetFullPath(Application.dataPath + "/../../docs/evidence/ability-fx-captures-r3-2-2-g1");

        static Doc Read(string file)
        {
            string path = Path.Combine(Dir, file);
            Assert.That(File.Exists(path), "[ability-fx-captures-r3-2-2-g1] " + file + " is missing: run probe.py over " + Dir);
            var doc = JsonUtility.FromJson<Doc>(File.ReadAllText(path));
            Assert.That(doc, Is.Not.Null, "[ability-fx-captures-r3-2-2-g1] " + file + " did not parse");
            return doc;
        }

        static float BarFor(Bars bars, string band) => band == "T1" ? bars.T1 : bars.O240;

        // [ability-fx-captures-r3-2-2-g1] the harness: six rows, shot where they were asked to be shot, at night, and
        // each on exactly its plate's sim tick. The last of those is the one this leg fixed - the drain after a shot
        // is not a fixed number of frames, so the still pass used to run 3 or 4 ticks ahead of its plate pass and the
        // whole moving field landed in the difference. The LOOK (foam_frac, crown_spread) is probe.py's, and red.
        [Test]
        public void TheSixStills_PairWithTheirPlates_AtNight()
        {
            var doc = Read("probe.json");
            Assert.That(doc.rows, Is.Not.Null.And.Length.EqualTo(6),
                "[ability-fx-captures-r3-2-2-g1] 6 rows were asked for: splash x {T1, O240} x three reps");

            foreach (var r in doc.rows)
            {
                string who = r.name;
                Assert.That(r.band == "T1" || r.band == "O240", who + ": band " + r.band + " is neither T1 nor O240");
                Assert.That(r.tick_delta, Is.EqualTo(0), string.Format(CultureInfo.InvariantCulture,
                    "[ability-fx-captures-r3-2-2-g1] {0}: the still is on sim tick {1} and its plate on {2}, so what differs "
                    + "between them is not only the effect", who, r.sim_tick, r.plate_sim_tick));
                Assert.That(r.pose_error_m, Is.LessThan(0.5f), "[ability-fx-captures-r3-2-2-g1] " + who + ": the camera was not where it was asked to be");
                Assert.That(r.night, Is.EqualTo(1), "[ability-fx-captures-r3-2-2-g1] " + who + ": the owner asked for the water at night");
                Assert.That(r.ambient_shells_per_min, Is.EqualTo(0f), "[ability-fx-captures-r3-2-2-g1] " + who + ": the barrage was not off");
            }
        }

        // [ability-fx-captures-r3-2-2-g1] "Bar outside the spread of three captures of the current build."
        [Test]
        public void EveryBar_IsAboveEveryBaselineRowOfItsBand()
        {
            var probe = Read("probe.json");
            var baseline = Read("baseline.json");
            Assert.That(baseline.rows, Is.Not.Null.And.Length.EqualTo(6),
                "[ability-fx-captures-r3-2-2-g1] baseline.json must hold the same six rows, captured from the unfixed build");

            foreach (var b in baseline.rows)
            {
                float foamBar = BarFor(probe.foam_bar, b.band), crownBar = BarFor(probe.crown_bar, b.band);
                Assert.That(foamBar, Is.GreaterThan(b.foam_frac), string.Format(CultureInfo.InvariantCulture,
                    "[ability-fx-captures-r3-2-2-g1] the {0} foam bar {1:0.0000} is inside the spread: baseline {2} read {3:0.0000}",
                    b.band, foamBar, b.name, b.foam_frac));
                Assert.That(crownBar, Is.GreaterThan(b.crown_spread), string.Format(CultureInfo.InvariantCulture,
                    "[ability-fx-captures-r3-2-2-g1] the {0} crown bar {1:0.000} is inside the spread: baseline {2} read {3:0.000}",
                    b.band, crownBar, b.name, b.crown_spread));
            }
        }

        // [ability-fx-captures-r3-2-2-g1] the throw the crown is drawn with, against hand sums
        [Test]
        public void CrownThrow_SplaysOutwardAndUp()
        {
            Assert.That(CombatFx.CrownSpokes, Is.EqualTo(6), "[ability-fx-captures-r3-2-2-g1] six spokes make the cone");

            Vector3 first = CombatFx.CrownThrow(0, 10f);
            Assert.That(first.x, Is.EqualTo(24f).Within(1e-3f), "[ability-fx-captures-r3-2-2-g1] spoke 0 at r 10: CrownOut 2.4 x 10 straight out");
            Assert.That(first.y, Is.EqualTo(5f).Within(1e-3f), "[ability-fx-captures-r3-2-2-g1] spoke 0 at r 10: CrownRise 0.5 x 10 up");
            Assert.That(first.z, Is.EqualTo(0f).Within(1e-3f), "[ability-fx-captures-r3-2-2-g1] spoke 0 is on the +x bearing");

            for (int k = 0; k < CombatFx.CrownSpokes; k++)
            {
                Vector3 v = CombatFx.CrownThrow(k, 4f);
                Assert.That(new Vector2(v.x, v.z).magnitude, Is.EqualTo(CombatFx.CrownOut * 4f).Within(1e-3f),
                    "[ability-fx-captures-r3-2-2-g1] spoke " + k + " goes out as far as every other: the cone is round");
                Assert.That(v.y, Is.EqualTo(CombatFx.CrownRise * 4f).Within(1e-3f),
                    "[ability-fx-captures-r3-2-2-g1] spoke " + k + " rises with the rest");
            }
            // opposite spokes are opposite, so the crown throws no net sideways push
            Vector3 sum = Vector3.zero;
            for (int k = 0; k < CombatFx.CrownSpokes; k++) sum += CombatFx.CrownThrow(k, 7f);
            Assert.That(new Vector2(sum.x, sum.z).magnitude, Is.LessThan(1e-3f),
                "[ability-fx-captures-r3-2-2-g1] the six spokes cancel sideways");
        }

        // [ability-fx-captures-r3-2-2-g1] the foam still covers the 400 m^2 of ground the r3-2-2 floor asks for
        [Test]
        public void FoamRings_WidenOutToTheOldFlatCard()
        {
            Assert.That(CombatFx.FoamRings, Is.EqualTo(3), "[ability-fx-captures-r3-2-2-g1] three rings");
            Assert.That(CombatFx.FoamWidth(0, 1f), Is.EqualTo(CombatFx.FoamFirst).Within(1e-4f), "[ability-fx-captures-r3-2-2-g1] ring 0 is FoamFirst r");
            Assert.That(CombatFx.FoamWidth(1, 1f), Is.EqualTo(7.5f).Within(1e-4f), "[ability-fx-captures-r3-2-2-g1] ring 1 is halfway: (5.0 + 10.0) / 2");
            Assert.That(CombatFx.FoamWidth(2, 1f), Is.EqualTo(CombatFx.FoamLast).Within(1e-4f), "[ability-fx-captures-r3-2-2-g1] ring 2 is FoamLast r, the outermost foam");
            Assert.That(CombatFx.FoamWidth(2, 3f), Is.EqualTo(CombatFx.FoamLast * 3f).Within(1e-4f), "[ability-fx-captures-r3-2-2-g1] every ring scales with r");
            Assert.That(CombatFx.FoamLife, Is.GreaterThan(1.2f), "[ability-fx-captures-r3-2-2-g1] foam outlives the crown");
            Assert.That(CombatFx.CrownOut, Is.GreaterThan(CombatFx.CrownRise), "[ability-fx-captures-r3-2-2-g1] the crown splays more than it rises, or it is a second column");
            Assert.That(CombatFx.CrownWidth, Is.LessThan(1f), "[ability-fx-captures-r3-2-2-g1] a spoke is a stub, not a second column: the Splash sheet draws a tall column");
        }

        [System.Serializable]
        class Row
        {
            public string name, band;
            public float foam_frac, crown_spread, pose_error_m, ambient_shells_per_min;
            public int night, sim_tick, plate_sim_tick, tick_delta;
        }

        [System.Serializable]
        class Bars { public float T1, O240; }

        [System.Serializable]
        class Doc { public Row[] rows; public Bars foam_bar, crown_bar; }
    }
}
