// Phase: C72 (AOSA) - the per-shot log of an image run. Pure logic, no scene: which held frame a tracer is first drawn
// on (CombatFx's own draw and prune tests, on the held clock), which firing line a shooter belongs to, and that the log
// keeps nothing unless an image run started it.
using NUnit.Framework;
using TW.Presentation;

namespace TW.Tests
{
    public class ShotLogTests
    {
        const float Step = 1f / 64f;    // PerfBench.HeldStep
        const float Life = 0.12f;       // CombatFx.TracerSeconds

        [TearDown] public void TearDown() => ShotLog.Clear();

        static float[] Frames(float first, int n)
        {
            var t = new float[n];
            for (int k = 0; k < n; k++) t[k] = first + k * Step;
            return t;
        }

        [Test]
        public void BirthFrame_IsTheFirstFrameThatDrawsIt()
        {
            var t = Frames(128f, 32);
            Assert.AreEqual(0, ShotLog.BirthFrame(t[0], t, 32, Step, Life), "born on frame 0's time: drawn on frame 0");
            Assert.AreEqual(0, ShotLog.BirthFrame(t[0] - Step * 0.5f, t, 32, Step, Life), "born between the frame before and frame 0");
            Assert.AreEqual(5, ShotLog.BirthFrame(t[5], t, 32, Step, Life));
            Assert.AreEqual(5, ShotLog.BirthFrame(t[4] + 0.001f, t, 32, Step, Life), "after frame 4's time: frame 4 skips it (now < Born)");
            Assert.AreEqual(31, ShotLog.BirthFrame(t[31], t, 32, Step, Life));
            Assert.AreEqual(ShotLog.After, ShotLog.BirthFrame(t[31] + 0.001f, t, 32, Step, Life));
        }

        [Test]
        public void BirthFrame_BeforeFrameZero_IsInFlightWhileCombatFxKeepsIt()
        {
            var t = Frames(128f, 8);
            Assert.AreEqual(ShotLog.InFlight, ShotLog.BirthFrame(t[0] - Step, t, 8, Step, Life), "drawn on the frame before: in flight");
            Assert.AreEqual(ShotLog.InFlight, ShotLog.BirthFrame(t[0] - Life, t, 8, Step, Life), "Prune keeps Born == now - life");
            Assert.AreEqual(ShotLog.Gone, ShotLog.BirthFrame(t[0] - Life - 0.001f, t, 8, Step, Life));
        }

        [Test]
        public void BirthFrame_OneFrameRun()
        {
            var t = Frames(64f, 1);
            Assert.AreEqual(0, ShotLog.BirthFrame(64f, t, 1, Step, Life));
            Assert.AreEqual(ShotLog.After, ShotLog.BirthFrame(64.01f, t, 1, Step, Life));
            Assert.AreEqual(ShotLog.After, ShotLog.BirthFrame(64f, null, 0, Step, Life));
        }

        [Test]
        public void Line_IsTrenchSectionOrOpenGrid()
        {
            Assert.AreEqual("0:t3.0", ShotLog.Line(0, 3, 0, 10f, 10f));
            Assert.AreEqual("0:t3.0", ShotLog.Line(0, 3, ShotLog.SectionCells - 1, 10f, 10f));
            Assert.AreEqual("0:t3.1", ShotLog.Line(0, 3, ShotLog.SectionCells, 99f, 99f), "the section follows the trench, not the position");
            Assert.AreEqual("1:t0.2", ShotLog.Line(1, 0, ShotLog.SectionCells * 2 + 3, 0f, 0f));
            Assert.AreEqual("1:o2.11", ShotLog.Line(1, -1, -1, 45f, 225f));
            Assert.AreEqual("0:o0.5", ShotLog.Line(0, 4, -1, 19.9f, 100f), "a trench cell the trench does not list falls back to the grid");
            Assert.AreEqual(ShotLog.Line(1, 2, 17, 5f, 6f), ShotLog.Line(1, 2, 17, 5f, 6f));
        }

        [Test]
        public void Log_KeepsNothingUntilBegun_AndWholeTicksFromItsFirst()
        {
            var e = new ShotLog.Entry { Tick = 100 };
            ShotLog.Add(e);
            Assert.AreEqual(0, ShotLog.Count, "off: nothing is kept");
            ShotLog.Begin(2, 100);
            ShotLog.Add(new ShotLog.Entry { Tick = 99 });
            Assert.AreEqual(0, ShotLog.Count, "a tick before FromTick is not kept");
            ShotLog.Add(e); ShotLog.Add(e); ShotLog.Add(e);
            Assert.AreEqual(2, ShotLog.Count);
            Assert.AreEqual(1, ShotLog.Dropped, "a full buffer counts what it could not keep");
            ShotLog.Stop();
            ShotLog.Add(e);
            Assert.AreEqual(2, ShotLog.Count, "stopped: nothing more is kept");
            ShotLog.Clear();
            Assert.IsFalse(ShotLog.On);
            Assert.AreEqual(0, ShotLog.Count);
        }
    }
}
