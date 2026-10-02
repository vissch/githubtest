// Phase: C22 (AOSA, juice J03) - the stagger that spreads a tick's rifle shots across the tick. Pure logic, no scene:
// it stays inside the tick, it repeats, the knob at 0 is the old look, and on the held clock (1/64 s a frame, the
// bench's shot_tick stills) no frame shows more than half of a tick's shots, where before one frame showed all of them.
using NUnit.Framework;
using TW.Presentation;

namespace TW.Tests
{
    public class ShotStaggerTests
    {
        const float Tick = 0.05f;          // SimConfig: 20 Hz
        const float HeldStep = 1f / 64f;   // the held clock of a shot_tick run (C33)

        [SetUp] public void SetUp() => Knobs.Clear();
        [TearDown] public void TearDown() => Knobs.Clear();

        [Test]
        public void EveryDelayFallsInsideItsTick_AndRepeats()
        {
            for (uint tick = 0; tick < 400; tick++)
                for (int shooter = 0; shooter < 300; shooter += 7)
                {
                    float f = ShotStagger.Fraction(shooter, tick);
                    Assert.That(f, Is.GreaterThanOrEqualTo(0f).And.LessThan(1f));
                    Assert.AreEqual(f, ShotStagger.Fraction(shooter, tick), "the same shooter and tick must give the same place");
                    float d = ShotStagger.Delay(shooter, tick, Tick, 1f);
                    Assert.That(d, Is.GreaterThanOrEqualTo(0f).And.LessThan(Tick));
                }
        }

        [Test]
        public void SpreadZero_IsTheOldLook()
        {
            for (int shooter = 0; shooter < 50; shooter++)
                Assert.AreEqual(0f, ShotStagger.Delay(shooter, 123u, Tick, 0f));
            Assert.AreEqual(1f, ShotStagger.ReadSpread(), "nothing set: the whole tick");
            Knobs.Set(ShotStagger.Knob, "0");
            Assert.AreEqual(0f, ShotStagger.ReadSpread());
            Knobs.Set(ShotStagger.Knob, "-2");
            Assert.AreEqual(0f, ShotStagger.ReadSpread(), "never below 0");
        }

        [Test]
        public void NoHeldFrameShowsMoreThanHalfOfATicksShots()
        {
            // a volley: a firing line's slots are neighbours, and the same men fire again tick after tick. Each shot is
            // first drawn on the first held frame at or after its birth; a tick arrives on a frame, so frame k covers
            // delays in ((k-1) step, k step]. Before C22 every shot of a tick was drawn on frame 0.
            foreach (int men in new[] { 8, 40, 100 })
            {
                float sum = 0f;
                for (uint tick = 1800; tick < 2200; tick++)
                {
                    var perFrame = new int[8];
                    for (int k = 0; k < men; k++)
                    {
                        int shooter = 1200 + k;
                        float d = ShotStagger.Delay(shooter, tick, Tick, 1f);
                        perFrame[(int)System.Math.Ceiling(d / HeldStep)]++;
                        sum += ShotStagger.Fraction(shooter, tick);
                    }
                    for (int f = 0; f < perFrame.Length; f++)
                        Assert.That(perFrame[f], Is.LessThanOrEqualTo(men / 2), men + " men, tick " + tick + ": held frame " + f + " showed more than half of the tick's shots");
                }
                Assert.That(sum / (men * 400f), Is.InRange(0.45f, 0.55f), men + " men: the shots are not spread evenly over the tick");
            }
        }

        [Test]
        public void NeighboursInALineDoNotMoveTogether()
        {
            // the ripple needs neighbours at different places in the tick: count adjacent pairs within a twentieth of it
            int close = 0, pairs = 0;
            for (uint tick = 0; tick < 200; tick++)
                for (int s = 0; s < 60; s++, pairs++)
                    if (System.Math.Abs(ShotStagger.Fraction(s, tick) - ShotStagger.Fraction(s + 1, tick)) < 0.05f) close++;
            Assert.That(close / (float)pairs, Is.LessThan(0.05f), "neighbours share a place in the tick (independent hashes would give 0.0975)");
        }
    }
}
