// Phase: tooling (AOSA, 2026-09-25) - the bench string parses the way the AOSA loop writes it. A scenario that
// silently became "none", or a knobs= value cut in half by the bench's own separators, would be a measurement of
// something other than what the report says; the keys that were there before must still mean what they meant.
using NUnit.Framework;
using TW.Perf;

namespace TW.Tests
{
    public class BenchOptionsTests
    {
        [Test]
        public void ScenarioBarrageParses()
        {
            var o = BenchOptions.Parse("stress=1500 scenario=barrage ticks=400");
            Assert.AreEqual(BenchScenario.Barrage, o.Scenario);
            Assert.AreEqual("barrage", o.ScenarioRaw);
            Assert.AreEqual("barrage", BenchOptions.ScenarioName(o.Scenario));
        }

        [Test]
        public void EveryScenarioNameRoundTrips()
        {
            foreach (var s in new[] { BenchScenario.None, BenchScenario.Barrage, BenchScenario.Armour, BenchScenario.Vfx })
                Assert.AreEqual(s, BenchOptions.Parse("scenario=" + BenchOptions.ScenarioName(s)).Scenario, BenchOptions.ScenarioName(s));
            Assert.AreEqual(BenchScenario.Armour, BenchOptions.Parse("scenario=armor").Scenario, "the American spelling");
            Assert.AreEqual(BenchScenario.Vfx, BenchOptions.Parse("scenario=VFX").Scenario, "case does not matter");
        }

        [Test]
        public void UnknownScenarioRunsNoneAndIsRemembered()
        {
            var o = BenchOptions.Parse("scenario=fireworks stress=900");
            Assert.AreEqual(BenchScenario.None, o.Scenario);
            Assert.AreEqual("fireworks", o.ScenarioRaw, "the report says what was asked for");
            Assert.AreEqual(900, o.Stress, "and the rest of the string still parses");
        }

        [Test]
        public void NoScenarioIsNone()
        {
            var o = BenchOptions.Parse("stress=1500");
            Assert.AreEqual(BenchScenario.None, o.Scenario);
            Assert.AreEqual("", o.ScenarioRaw);
            Assert.AreEqual("", o.Knobs);
        }

        [Test]
        public void KnobsKeepTheirPipesAndCase()
        {
            var o = BenchOptions.Parse("stress=1500 knobs=a=1|b=2 ticks=300");
            Assert.AreEqual("a=1|b=2", o.Knobs);
            Assert.AreEqual(300, o.Ticks);
            o = BenchOptions.Parse("knobs=vat.lodDistance=90|fx.maxMarks=400,label=sweep");
            Assert.AreEqual("vat.lodDistance=90|fx.maxMarks=400", o.Knobs, "names keep their case; the comma ends the value");
            Assert.AreEqual("sweep", o.Label);
        }

        [Test]
        public void SeveralKnobsTokensAddUp()
        {
            var o = BenchOptions.Parse("knobs=a=1 knobs=b=2");
            Assert.AreEqual("a=1|b=2", o.Knobs);
        }

        [Test]
        public void ExistingKeysStillParse()
        {
            const string raw = "stress=1200 settle=1500 ticks=250 warm=60 ff=4 quality=3 vsync=1 w=1280 h=720 weather=30 " +
                               "zoom=18 yaw=10 pitch=30 subs=1 canary=0 label=base out=C:/runs/a.json shot=C:/runs/a.png quit=0";
            var o = BenchOptions.Parse(raw);
            Assert.AreEqual(1200, o.Stress);
            Assert.AreEqual(1500, o.SettleTicks);
            Assert.AreEqual(250, o.Ticks);
            Assert.AreEqual(60, o.Warm);
            Assert.AreEqual(4f, o.FastForward);
            Assert.AreEqual(3, o.Quality);
            Assert.IsTrue(o.VSync);
            Assert.AreEqual(1280, o.Width);
            Assert.AreEqual(720, o.Height);
            Assert.AreEqual(30f, o.Weather);
            Assert.AreEqual(18f, o.Zoom);
            Assert.AreEqual(10f, o.Yaw);
            Assert.AreEqual(30f, o.Pitch);
            Assert.IsTrue(o.Subscribers);
            Assert.AreEqual(0, o.Canary);
            Assert.AreEqual("base", o.Label);
            Assert.AreEqual("C:/runs/a.json", o.Out);
            Assert.AreEqual("C:/runs/a.png", o.Shot);
            Assert.IsFalse(o.Quit);
            Assert.AreEqual(raw, o.Raw);
            Assert.AreEqual(BenchScenario.None, o.Scenario);
            Assert.AreEqual("", o.Knobs);
        }

        [Test]
        public void EmptyStringKeepsTheDefaults()
        {
            var o = BenchOptions.Parse("");
            var d = new BenchOptions();
            Assert.AreEqual(d.Stress, o.Stress);
            Assert.AreEqual(d.Ticks, o.Ticks);
            Assert.AreEqual(BenchScenario.None, o.Scenario);
            Assert.AreEqual("", o.Knobs);
        }
    }
}
