// Phase: tooling (AOSA C64, 2026-09-26) - the hitch carrier pick. A card is written from "marker X was the largest in
// most hitches", so the pick must name the largest TW marker only, never a counter, never a NaN, the same way in every
// run, and name nobody at all on a release player, whose markers read 0.
using System.Collections.Generic;
using NUnit.Framework;
using TW.Perf;

namespace TW.Tests
{
    public class HitchAttributionTests
    {
        [Test]
        public void LargestPicksTheLargestEligibleValue()
        {
            var v = new[] { 3.0, 12.5, 40000.0, 7.0 };
            var markers = new[] { true, true, false, true };   // 40000 is a counter (gc_bytes), not a marker
            Assert.AreEqual(1, HitchAttribution.Largest(v, markers, v.Length));
        }

        [Test]
        public void LargestIgnoresNaNAndInfinity()
        {
            var v = new[] { double.NaN, 2.0, double.PositiveInfinity, 1.0 };
            Assert.AreEqual(1, HitchAttribution.Largest(v, new[] { true, true, true, true }, v.Length));
        }

        [Test]
        public void LargestTieGoesToTheLowerIndex()
        {
            var v = new[] { 1.0, 5.0, 5.0 };
            Assert.AreEqual(1, HitchAttribution.Largest(v, new[] { true, true, true }, v.Length));
        }

        [Test]
        public void LargestIsNoneWhenEveryMarkerReadsZero()
        {
            // a release player: the recorders are invalid or read 0, and the hitch has no carrier rather than a wrong one
            var v = new[] { 0.0, 0.0, 0.0 };
            Assert.AreEqual(-1, HitchAttribution.Largest(v, new[] { true, true, true }, v.Length));
            Assert.AreEqual(-1, HitchAttribution.Largest(new double[0], new bool[0], 0));
            Assert.AreEqual(-1, HitchAttribution.Largest(null, null, 3));
        }

        [Test]
        public void LargestReadsOnlyTheFirstN()
        {
            var v = new[] { 1.0, 2.0, 9.0 };
            Assert.AreEqual(1, HitchAttribution.Largest(v, new[] { true, true, true }, 2));
            Assert.AreEqual(2, HitchAttribution.Largest(v, new[] { true, true, true }, 99), "n past the arrays is clamped");
        }

        [Test]
        public void SummariseCountsAndTakesTheMedianShare()
        {
            var marker = new[] { "TW.Terrain.Update", "TW.Host.Update", "TW.Terrain.Update", null, "TW.Terrain.Update" };
            var ms = new[] { 20.0, 10.0, 10.0, 0.0, 30.0 };
            var hitch = new[] { 40.0, 50.0, 40.0, 35.0, 40.0 };
            List<HitchAttribution.Carrier> c = HitchAttribution.Summarise(marker, ms, hitch, marker.Length);
            Assert.AreEqual(2, c.Count, "the hitch with no carrier is left out");
            Assert.AreEqual("TW.Terrain.Update", c[0].Marker);
            Assert.AreEqual(3, c[0].Hitches);
            Assert.AreEqual(0.5, c[0].MedianShare, 1e-12, "shares 0.5, 0.25, 0.75");
            Assert.AreEqual("TW.Host.Update", c[1].Marker);
            Assert.AreEqual(1, c[1].Hitches);
            Assert.AreEqual(0.2, c[1].MedianShare, 1e-12);
        }

        [Test]
        public void SummariseOrdersByCountThenShareThenName()
        {
            var marker = new[] { "b", "a", "c", "c" };
            var ms = new[] { 10.0, 10.0, 5.0, 5.0 };
            var hitch = new[] { 40.0, 40.0, 40.0, 40.0 };
            var c = HitchAttribution.Summarise(marker, ms, hitch, marker.Length);
            Assert.AreEqual(new[] { "c", "a", "b" }, new[] { c[0].Marker, c[1].Marker, c[2].Marker });
        }

        [Test]
        public void SummariseOfNothingIsEmpty()
        {
            Assert.AreEqual(0, HitchAttribution.Summarise(new string[0], new double[0], new double[0], 0).Count);
            Assert.AreEqual(0, HitchAttribution.Summarise(null, null, null, 5).Count);
        }

        [Test]
        public void MedianOfEvenCountIsTheMeanOfTheMiddleTwo()
        {
            Assert.AreEqual(2.5, HitchAttribution.Median(new List<double> { 4, 1, 3, 2 }), 1e-12);
            Assert.IsTrue(double.IsNaN(HitchAttribution.Median(new List<double>())));
        }
    }
}
