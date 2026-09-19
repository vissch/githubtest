// Phase: P0 (implemented)
using NUnit.Framework;
using Unity.Collections;
using TW.Sim;

namespace TW.Tests
{
    public class SimHashTests
    {
        [Test]
        public void EqualArrays_HashEqual_DifferentArrays_HashDiffer()
        {
            using var a = new NativeArray<float>(new[] { 1f, 2f, 3f }, Allocator.Temp);
            using var b = new NativeArray<float>(new[] { 1f, 2f, 3f }, Allocator.Temp);
            using var c = new NativeArray<float>(new[] { 1f, 2f, 3.0001f }, Allocator.Temp);
            Assert.AreEqual(SimHash.Array(a, SimHash.Offset), SimHash.Array(b, SimHash.Offset));
            Assert.AreNotEqual(SimHash.Array(a, SimHash.Offset), SimHash.Array(c, SimHash.Offset));
        }

        [Test]
        public void SimRandom_IsStableForSameInputs()
        {
            var r1 = SimRandom.For(42, 10, SimRandom.SystemId.Test, 3);
            var r2 = SimRandom.For(42, 10, SimRandom.SystemId.Test, 3);
            Assert.AreEqual(r1.NextUInt(), r2.NextUInt());
            var r3 = SimRandom.For(42, 11, SimRandom.SystemId.Test, 3);
            Assert.AreNotEqual(SimRandom.For(42, 10, SimRandom.SystemId.Test, 3).NextUInt(), r3.NextUInt());
        }

        [Test]
        public void SimMath_TrigMatchesReferenceWithinTolerance()
        {
            for (int i = -720; i <= 720; i++)
            {
                float x = i * 0.5f * Unity.Mathematics.math.PI / 180f * 4f;
                Assert.AreEqual(System.Math.Sin(x), SimMath.Sin(x), 2e-5, $"sin({x})");
                Assert.AreEqual(System.Math.Cos(x), SimMath.Cos(x), 2e-5, $"cos({x})");
            }
            Assert.AreEqual(System.Math.Atan2(1, 2), SimMath.Atan2(1f, 2f), 1e-4);
            Assert.AreEqual(System.Math.Atan2(-3, -0.5), SimMath.Atan2(-3f, -0.5f), 1e-4);
        }
    }
}
