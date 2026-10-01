// Phase: SHOW (2026-10-01) — the lightning bolt at a close view. The frame holds only the last ten or fifteen metres
// of it, which used to be two straight segments (a hard white polyline that read as a debug line): the points must
// crowd toward the foot so that stretch is several short strokes.
using NUnit.Framework;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class StormBoltTests
    {
        [Test]
        public void TheBoltsPointsCrowdTowardTheGround()
        {
            Assert.AreEqual(0f, Storm.BoltFall(0), 1e-6f);
            Assert.AreEqual(1f, Storm.BoltFall(Storm.BoltSteps), 1e-6f);
            float last = 0f, lastStep = float.MaxValue;
            for (int k = 1; k <= Storm.BoltSteps; k++)
            {
                float f = Storm.BoltFall(k);
                Assert.Greater(f, last, "falling at " + k);
                Assert.LessOrEqual(f - last, lastStep + 1e-6f, "each segment no longer than the one above it");
                lastStep = f - last; last = f;
            }
        }

        [Test]
        public void TheLastTenMetresAreSeveralStrokes_NotTwo()
        {
            const float lowest = 150f;   // the shortest bolt BuildBolt makes; a taller one has longer segments
            foreach (float height in new[] { lowest, 210f })
            {
                int strokes = 0;
                for (int k = Storm.BoltSteps; k > 0 && (1f - Storm.BoltFall(k - 1)) * height <= 12f; k--) strokes++;
                Assert.GreaterOrEqual(strokes, 4, "a " + height + " m bolt in its last 12 m");
            }
            Assert.LessOrEqual((1f - Storm.BoltFall(Storm.BoltSteps - 1)) * 210f * Storm.BoltKink, 1.5f, "the last kink is a stroke's width across, not a jump");
        }
    }
}
