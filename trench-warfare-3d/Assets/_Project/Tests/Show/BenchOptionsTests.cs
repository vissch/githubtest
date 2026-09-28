// Phase: tooling (AOSA, 2026-09-25) - the bench string parses the way the AOSA loop writes it. A scenario that
// silently became "none", or a knobs= value cut in half by the bench's own separators, would be a measurement of
// something other than what the report says; the keys that were there before must still mean what they meant.
using NUnit.Framework;
using TW.Perf;
using TW.Presentation;

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
            foreach (var s in new[] { BenchScenario.None, BenchScenario.Barrage, BenchScenario.Armour, BenchScenario.Vfx, BenchScenario.Beam, BenchScenario.Lineup })
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
        public void ShotTickDefaultsToTheWarmUpStillAndParses()
        {
            Assert.AreEqual(-1, BenchOptions.Parse("stress=1500 shot=C:/runs/a.png").ShotTick, "no shot_tick: the paused warm-up still");
            var o = BenchOptions.Parse("shot=C:/runs/a.png shot_tick=120 scenario=barrage");
            Assert.AreEqual(120, o.ShotTick);
            Assert.AreEqual("C:/runs/a.png", o.Shot, "shot_tick is not read as shot");
            Assert.AreEqual(BenchScenario.Barrage, o.Scenario);
        }

        [Test]
        public void ShotHudDefaultsOnAndParsesOff()
        {
            Assert.IsTrue(BenchOptions.Parse("shot=C:/runs/a.png shot_tick=120").ShotHud, "the HUD is in the still unless asked");
            Assert.IsFalse(BenchOptions.Parse("shot=C:/runs/a.png shot_tick=120 shot_hud=0").ShotHud);
            Assert.IsFalse(BenchOptions.Parse("shot_hud=false").ShotHud);
            Assert.AreEqual(120, BenchOptions.Parse("shot_tick=120 shot_hud=0").ShotTick, "shot_hud is not read as shot_tick");
        }

        [Test]
        public void ShotFramesDefaultsToOneStillAndNeverBelowOne()
        {
            Assert.AreEqual(1, BenchOptions.Parse("shot=C:/runs/a.png shot_tick=120").ShotFrames);
            Assert.AreEqual(8, BenchOptions.Parse("shot_tick=120 shot_frames=8").ShotFrames);
            Assert.AreEqual(1, BenchOptions.Parse("shot_frames=0").ShotFrames, "0 frames would be no still at all");
            Assert.AreEqual(120, BenchOptions.Parse("shot_tick=120 shot_frames=8").ShotTick, "shot_frames is not read as shot_tick");
        }

        // C33: the held clock. Time.time reaches the menu at whatever the splash took; the bench walks it to one value,
        // in steps Time.maximumDeltaTime cannot clamp, and every start must land on the SAME float Time.time, or every
        // shader's _Time (rain, fog, water, flames) starts each run somewhere else and the still does not repeat.
        [Test]
        public void HeldClockLandsOnTheSameTimeFromAnyStart()
        {
            float landed = -1f;
            foreach (double start in new[] { 0.0, 2.345678912, 7.1, 17.999999, 41.25, 62.9 })
            {
                double target = PerfBench.AlignTarget(start), now = start;
                Assert.AreEqual(64.0, target, "start " + start);
                int frames = 0;
                for (float step; (step = PerfBench.AlignStep(now, target)) > 0f; frames++)
                {
                    Assert.LessOrEqual(step, PerfBench.AlignMaxStep, "a step Time.maximumDeltaTime (0.333 s) could clamp");
                    now += step;   // what the engine adds: captureDeltaTime is a float
                    Assert.Less(frames, 2000, "the walk must end");
                }
                Assert.AreEqual(target, now, 1e-6, "start " + start);
                // then the held frames: Time.time (a float) must read the same on the still's frame from every start
                double shot = now;
                for (int k = 0; k < 900; k++) shot += PerfBench.HeldStep;
                if (landed < 0f) landed = (float)shot;
                Assert.AreEqual(landed, (float)shot, "start " + start + ": the still's Time.time differs");
            }
            Assert.AreEqual(128.0, PerfBench.AlignTarget(63.5), "less than a second short goes to the next multiple");
            Assert.AreEqual(1f / 64f, PerfBench.HeldStep, "a power of two, so the held frames add up exactly");
        }

        // C51: ground= picks the battlefield (and so its look). Absent, it is the night wood the bench always ran; an
        // unknown name is refused, never run as the wood under another label.
        [Test]
        public void NoGroundIsTheNightWood()
        {
            var o = BenchOptions.Parse("stress=1500 ticks=400 scenario=barrage");
            Assert.AreEqual(Ground.ShelledForest, o.Ground);
            Assert.AreEqual("", o.GroundRaw);
            Assert.IsFalse(o.GroundUnknown);
            Assert.AreEqual(Ground.ShelledForest, new BenchOptions().Ground, "the zero value, as MatchLaunch.Request's");
            Assert.AreEqual(new MatchLaunch.Request().Ground, o.Ground, "the bench's request is the one it always launched");
        }

        [Test]
        public void GroundParsesByNameAnyCaseAndByShortName()
        {
            var o = BenchOptions.Parse("stress=1500 ground=WinterLine shot_tick=100");
            Assert.AreEqual(Ground.WinterLine, o.Ground);
            Assert.AreEqual("WinterLine", o.GroundRaw);
            Assert.IsFalse(o.GroundUnknown);
            Assert.AreEqual(100, o.ShotTick, "and the rest of the string still parses");
            Assert.AreEqual(Ground.WinterLine, BenchOptions.Parse("ground=winter").Ground);
            Assert.AreEqual(Ground.WinterLine, BenchOptions.Parse("ground=WINTERLINE").Ground, "case does not matter");
            Assert.AreEqual(Ground.Landing, BenchOptions.Parse("ground=landing").Ground);
            Assert.AreEqual(Ground.ShelledForest, BenchOptions.Parse("ground=forest").Ground);
            var f = BenchOptions.Parse("ground=ShelledForest");
            Assert.AreEqual(Ground.ShelledForest, f.Ground);
            Assert.IsFalse(f.GroundUnknown, "naming the default is not an error");
            foreach (Ground g in System.Enum.GetValues(typeof(Ground)))
                Assert.AreEqual(g, BenchOptions.Parse("ground=" + g).Ground, "every Ground by its own name: " + g);
        }

        [Test]
        public void UnknownGroundIsRefusedNotRunAsTheWood()
        {
            var o = BenchOptions.Parse("ground=desert stress=900");
            Assert.IsTrue(o.GroundUnknown, "PerfBench refuses the run (exit 2)");
            Assert.AreEqual("desert", o.GroundRaw, "the error says what was asked for");
            Assert.AreEqual(900, o.Stress, "and the rest of the string still parses");
            Assert.IsTrue(BenchOptions.Parse("ground=").GroundUnknown, "an empty ground is not the default");
            Assert.IsTrue(BenchOptions.Parse("ground=1").GroundUnknown, "a number is not a name");
            Assert.IsTrue(BenchOptions.Parse("ground=day").GroundUnknown, "a time of day is not a ground");
            StringAssert.Contains("WinterLine", BenchOptions.GroundNames(), "the error lists the valid names");
        }

        [Test]
        public void OnlyTheWinterLineIsADayField()
        {
            // what makes C51 worth having: BiomeProfile.ForGround is the one place a ground gets its look
            foreach (Ground g in System.Enum.GetValues(typeof(Ground)))
            {
                bool day = !TW.Presentation.Terrain.BiomeProfile.For(TW.Presentation.Terrain.BiomeProfile.ForGround(g)).Dark;
                Assert.AreEqual(g == Ground.WinterLine, day, g + (day ? " is day" : " is night"));
            }
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
