// Phase: A5 (docs/21 phase 5) — the packing of a line ability's heading, pattern and length into SimCommand.B:
// it round-trips, brings a heading into the circle, clamps a length to its byte, and leaves the high bits free.
using NUnit.Framework;
using TW.Sim;

namespace TW.Tests
{
    public class AbilityArgsTests
    {
        [Test]
        public void PackAndUnpackRoundTrip()
        {
            AbilityArgs.Unpack(AbilityArgs.Pack(270, 2, 60), out int h, out int p, out int l);
            Assert.AreEqual(270, h); Assert.AreEqual(2, p); Assert.AreEqual(60, l);
            AbilityArgs.Unpack(0, out h, out p, out l);
            Assert.AreEqual(0, h, "B = 0 is the plain ability"); Assert.AreEqual(0, p); Assert.AreEqual(0, l);
            AbilityArgs.Unpack(AbilityArgs.Pack(359, 15, 255), out h, out p, out l);
            Assert.AreEqual(359, h); Assert.AreEqual(15, p); Assert.AreEqual(255, l);
            Assert.AreEqual(2, AbilityArgs.PatternOf(AbilityArgs.Pack(45, 2, 30)));
        }

        [Test]
        public void AHeadingIsBroughtIntoTheCircleAndALengthIntoItsByte()
        {
            AbilityArgs.Unpack(AbilityArgs.Pack(725, 0, 300), out int h, out _, out int l);
            Assert.AreEqual(5, h); Assert.AreEqual(255, l);
            AbilityArgs.Unpack(AbilityArgs.Pack(-90, 17, -4), out h, out int p, out l);
            Assert.AreEqual(270, h); Assert.AreEqual(1, p, "a pattern keeps its low four bits"); Assert.AreEqual(0, l);
        }

        [Test]
        public void HeadingZeroIsUpTheFieldAndNinetyIsRight()
        {
            var up = AbilityArgs.Heading(0); var right = AbilityArgs.Heading(90); var back = AbilityArgs.Heading(180); var left = AbilityArgs.Heading(-90);
            Assert.AreEqual(1f, up.z, 1e-3f); Assert.AreEqual(0f, up.x, 1e-3f); Assert.AreEqual(0f, up.y);
            Assert.AreEqual(1f, right.x, 1e-3f); Assert.AreEqual(0f, right.z, 1e-3f);
            Assert.AreEqual(-1f, back.z, 1e-3f);
            Assert.AreEqual(-1f, left.x, 1e-3f);
        }

        [Test]
        public void ThePackedWordLeavesTheHighBitsFree()
        {
            Assert.Less(AbilityArgs.Pack(359, 15, 255), 1 << 21);
            Assert.GreaterOrEqual(AbilityArgs.Pack(0, 0, 0), 0);
        }
    }
}
