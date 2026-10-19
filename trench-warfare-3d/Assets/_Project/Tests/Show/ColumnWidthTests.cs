// Phase: found-compute-the-burst-column-s-width-once-not-tw-20261018-3 - the burst column's width was spelled out
// twice in CombatFx.cs (the ordinary column's Add re-derived it, and added the day factor the soil and cap branches
// never got). Now every branch draws CombatFx.ColumnWidth once, with the day factor folded in.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class ColumnWidthTests
    {
        const float Tol = 1e-4f;

        // [found-compute-the-burst-column-s-width-once-not-tw-20261018-3]
        [Test]
        public void Width_IsOneExpression_ForAllFourCases()
        {
            const float r = 8f, columnScale = 1f, lookColumn = 1f;

            float wet = CombatFx.ColumnWidth(r, true, columnScale, 1f, lookColumn, false);
            Assert.AreEqual(18.4f, wet, Tol, "[found-compute-the-burst-column-s-width-once-not-tw-20261018-3] wet: 8 * 2.3 - the water crown is wider than dry earth (ability-fx-captures-r3-2-2), not 60 % of it");

            float dryDay = CombatFx.ColumnWidth(r, false, columnScale, 1f, lookColumn, true);
            Assert.AreEqual(11.76f, dryDay, Tol, "[found-compute-the-burst-column-s-width-once-not-tw-20261018-3] dry day: 8 * 2.1 * 0.7 (DayColumnWidth)");

            float moonlitDry = CombatFx.ColumnWidth(r, false, columnScale, 0.8f, lookColumn, false);
            Assert.AreEqual(13.44f, moonlitDry, Tol, "[found-compute-the-burst-column-s-width-once-not-tw-20261018-3] moonlit dry: 8 * 2.1 * 0.8");

            // the soil heave's width is the same call with the same inputs: soil changes the card's life/height, not its width
            float moonlitSoil = CombatFx.ColumnWidth(r, false, columnScale, 0.8f, lookColumn, false);
            Assert.AreEqual(13.44f, moonlitSoil, Tol, "[found-compute-the-burst-column-s-width-once-not-tw-20261018-3] moonlit with soil: same width as moonlit dry");

            // a retune of columnScale moves all four together
            Assert.AreEqual(wet * 2f, CombatFx.ColumnWidth(r, true, 2f, 1f, lookColumn, false), Tol, "[found-compute-the-burst-column-s-width-once-not-tw-20261018-3] doubling columnScale doubles wet");
            Assert.AreEqual(dryDay * 2f, CombatFx.ColumnWidth(r, false, 2f, 1f, lookColumn, true), Tol, "[found-compute-the-burst-column-s-width-once-not-tw-20261018-3] doubling columnScale doubles dry day");
            Assert.AreEqual(moonlitDry * 2f, CombatFx.ColumnWidth(r, false, 2f, 0.8f, lookColumn, false), Tol, "[found-compute-the-burst-column-s-width-once-not-tw-20261018-3] doubling columnScale doubles moonlit dry");
            Assert.AreEqual(moonlitSoil * 2f, CombatFx.ColumnWidth(r, false, 2f, 0.8f, lookColumn, false), Tol, "[found-compute-the-burst-column-s-width-once-not-tw-20261018-3] doubling columnScale doubles moonlit with soil");
        }

        // [found-compute-the-burst-column-s-width-once-not-tw-20261018-3]
        [Test]
        public void Source_EveryBranchDrawsTheOneWidth()
        {
            string combat = System.IO.File.ReadAllText(System.IO.Path.Combine(Application.dataPath, "_Project", "Presentation", "Camera", "CombatFx.cs"));
            Assert.AreEqual(0, Count(combat, "(damp ? 2.3f : 2.1f)"), "[found-compute-the-burst-column-s-width-once-not-tw-20261018-3] the product is no longer spelled out in CombatFx.cs");
            Assert.AreEqual(0, Count(combat, "DayColumnWidth"), "[found-compute-the-burst-column-s-width-once-not-tw-20261018-3] the day factor is folded into ColumnWidth, not CombatFx.cs");
            Assert.AreEqual(3, Count(combat, ", p, columnWidth,"), "[found-compute-the-burst-column-s-width-once-not-tw-20261018-3] the soil, cap and ordinary Adds all draw columnWidth");
        }

        // [found-compute-the-burst-column-s-width-once-not-tw-20261018-3]
        [Test]
        public void DayFactor_CannotBiteOnAMoonlitField()
        {
            foreach (bool wet in new[] { false, true })
                foreach (float fire in new[] { 0f, 1f })
                    Assert.IsFalse(CombatFx.DayEarth(wet, fire, true),
                        "[found-compute-the-burst-column-s-width-once-not-tw-20261018-3] moonlit implies no day slimming (wet " + wet + ", fire " + fire + ")");
        }

        static int Count(string s, string what)
        {
            int n = 0;
            for (int i = s.IndexOf(what); i >= 0; i = s.IndexOf(what, i + what.Length)) n++;
            return n;
        }
    }
}
