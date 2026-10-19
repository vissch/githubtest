// Phase: A5 — look look-code-sim-core-20261018: SimMath.Floor cast (int)x with no NaN or range guard, so
// WrapAngle -> Sin/Cos/DirFromYaw (every hashed yaw) was platform-dependent for a huge or non-finite angle
// (INT_MIN on x86, saturating on ARM). Nothing live reaches it — yaws come from Atan2 — but the file's header
// promises bit-identical results on every target CPU, so this holds the hole shut.
using NUnit.Framework;
using TW.Sim;

namespace TW.Tests
{
    public class SimMathTests
    {
        static bool IsFinite(float v) => !float.IsNaN(v) && !float.IsInfinity(v);

        [Test]
        public void SinAndCosOfAHugeOrNonFiniteAngleStayInRange()
        {
            float[] angles = { 1e30f, -1e30f, float.NaN, float.PositiveInfinity, float.NegativeInfinity };
            foreach (float a in angles)
            {
                float s = SimMath.Sin(a), c = SimMath.Cos(a);
                Assert.IsTrue(IsFinite(s), $"Sin({a}) is not finite: {s}");
                Assert.IsTrue(IsFinite(c), $"Cos({a}) is not finite: {c}");
                Assert.IsTrue(s >= -1f && s <= 1f, $"Sin({a}) is outside [-1, 1]: {s}");
                Assert.IsTrue(c >= -1f && c <= 1f, $"Cos({a}) is outside [-1, 1]: {c}");
            }
        }

        [Test]
        public void FloorOfAnUncastableFloatDoesNotGoThroughTheIntCast()
        {
            // NaN -> 0: the one value every CPU's float->int cast could agree on, and callers cast the result
            // straight to int (BattlefieldGenerator.Noise).
            Assert.AreEqual(0f, SimMath.Floor(float.NaN), "Floor(NaN) must be 0, not whatever (int)NaN gives");
            // A float of magnitude 2^23 or more is already a whole number, so it is its own floor.
            Assert.AreEqual(1e30f, SimMath.Floor(1e30f), "Floor(1e30) must be 1e30: it is already whole");
            Assert.AreEqual(-1e30f, SimMath.Floor(-1e30f), "Floor(-1e30) must be -1e30: it is already whole");
            Assert.AreEqual(float.PositiveInfinity, SimMath.Floor(float.PositiveInfinity), "floor(+inf) is +inf");
            Assert.AreEqual(float.NegativeInfinity, SimMath.Floor(float.NegativeInfinity), "floor(-inf) is -inf");

            // The ordinary path is unchanged.
            Assert.AreEqual(2f, SimMath.Floor(2.5f), "Floor(2.5)");
            Assert.AreEqual(-3f, SimMath.Floor(-2.5f), "Floor(-2.5)");
            Assert.AreEqual(-3f, SimMath.Floor(-3f), "Floor(-3)");
        }
    }
}
