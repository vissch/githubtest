// Phase: Playground (2026-10-13, lane/show/idea-the-playground-camera-shoots-on-the-bang) — the worked example of the shot on the bang
// The schedule of a playground shot taken on the event, driven by hand: no editor in Play, no rendering. The numbers are
// the goal's: five frames round the bang, one every 0.2 s, one of them kept from before it.
using NUnit.Framework;
using TW.Playground;

namespace TW.Tests.Playground
{
    public sealed class PlaygroundBangTests
    {
        static BangShot Shot() => new BangShot { Path = "C:/Captures/playground/demo_hit.png", Kind = "burst", Step = 0.2f, Pre = 1, Wait = 20f };

        [Test]
        public void PreRollTicksEveryStepUntilTheBang()
        {
            var s = Shot();
            Assert.IsTrue(s.PreRollDue(0.0f), "the first pre-roll frame is due at once");
            Assert.IsTrue(s.PreRollDue(0.2f));
            Assert.IsTrue(s.PreRollDue(0.4f));
            Assert.IsFalse(s.PreRollDue(0.5f), "half a step later nothing is owed yet");
            Assert.IsFalse(s.Fired);
            Assert.AreEqual(0, s.Taken, "a pre-roll frame is not part of the shot until a bang keeps it");
        }

        [Test]
        public void FiveFramesRoundTheBang()
        {
            var s = Shot();
            s.PreRollDue(0.0f); s.PreRollDue(0.2f); s.PreRollDue(0.4f);
            s.Fire(0.47f);
            Assert.AreEqual(2, s.Taken, "the kept pre-roll frame and the bang's own");
            Assert.IsFalse(s.Due(0.5f), "the next is owed at bang + 0.2");
            Assert.IsTrue(s.Due(0.67f));
            Assert.IsFalse(s.Done);
            Assert.IsTrue(s.Due(0.87f));
            Assert.IsTrue(s.Due(1.07f));
            Assert.IsTrue(s.Done, "five frames and the shot is finished");
            Assert.IsFalse(s.Due(1.27f), "nothing more is owed");

            string json = s.Manifest();
            StringAssert.Contains("\"bang\": true", json);
            foreach (float want in new[] { -0.07f, 0f, 0.2f, 0.4f, 0.6f })
                StringAssert.Contains(want.ToString("F3", System.Globalization.CultureInfo.InvariantCulture), json);
        }

        [Test]
        public void ManifestNamesFiveFramesInOrder()
        {
            var s = Shot();
            s.PreRollDue(0.0f);
            s.Fire(0.1f);
            s.Due(0.4f); s.Due(0.6f); s.Due(0.8f);   // late by a hair: a frame grabs when the step is PAST, not at it
            Assert.IsTrue(s.Done);
            string json = s.Manifest();
            int at = -1;
            for (int k = 0; k < 5; k++)
            {
                string name = "demo_hit_f" + (k + 1) + ".png";
                Assert.AreEqual(name, s.FrameName(k));
                int i = json.IndexOf(name, System.StringComparison.Ordinal);
                Assert.Greater(i, at, name + " comes after the frame before it");
                at = i;
            }
        }

        [Test]
        public void NoBangByTheWaitIsAShotThatSaysSo()
        {
            var s = Shot();
            s.PreRollDue(0.0f);
            Assert.IsFalse(s.Expired(19.9f));
            Assert.IsTrue(s.Expired(20.0f));
            StringAssert.Contains("\"bang\": false", s.Manifest());
        }
    }
}
