// Phase: tooling (perf pass, 2026-09-23) — the allocation instrument must see allocations before any test trusts it.
// Every allocation test here used to measure with GC.GetAllocatedBytesForCurrentThread, which reads 0 under Unity's
// Boehm GC whatever the code does, so for a day each of them "proved" a path allocation-free, including the one that
// built a string for every man on every tick. These pin the replacement to known answers: nothing is zero, one large
// array is one, ten thousand objects are ten thousand, and one formatted string (the shape of that per-man line) is
// seen. A probe that only noticed big blocks would pass the 1 MB case and miss exactly the bug that mattered.
using System;
using NUnit.Framework;
using TW.Perf;

namespace TW.Tests
{
    public class AllocProbeSanityTests
    {
        static object keep;
        enum Gait { Walk, Run }

        [Test]
        public void AnEmptyBlockAllocatesNothing()
        {
            Assert.That(AllocProbe.Available, "no profiler Recorder in the editor: the probe cannot run");
            Assert.That(AllocProbe.PerCall(() => { }, 5, 100), Is.EqualTo(0));
        }

        [Test]
        public void OneLargeArrayIsOneAllocation()
        {
            Assert.That(AllocProbe.PerCall(() => keep = new byte[1 << 20], 2, 10), Is.EqualTo(1));
        }

        [Test]
        public void TenThousandObjectsAreTenThousand()
        {
            Action many = () => { for (int i = 0; i < 10000; i++) keep = new object(); };
            many();
            Assert.That(AllocProbe.Count(many), Is.EqualTo(10000));
        }

        [Test]
        public void AFormattedStringIsSeen()
        {
            float speed = 1.7f; var gait = Gait.Run;
            double perCall = AllocProbe.PerCall(() => keep = "moves at " + speed.ToString("0.0") + " m/s: " + gait, 5, 100);
            Assert.That(perCall, Is.GreaterThanOrEqualTo(1), "a concatenated, formatted string allocates; a probe that says otherwise is blind");
        }

        /// <summary>Records what the old instrument says about a 1 MB allocation, so the reason it was replaced stays in
        /// the test log. It does not assert: if a Unity upgrade makes it work, nothing here should go red for it.</summary>
        [Test]
        public void TheOldInstrumentIsRecorded()
        {
            keep = new byte[16];
            long before = GC.GetAllocatedBytesForCurrentThread();
            keep = new byte[1 << 20];
            long after = GC.GetAllocatedBytesForCurrentThread();
            TestContext.WriteLine($"GC.GetAllocatedBytesForCurrentThread saw {after - before} bytes for a 1,048,576-byte array " +
                                  "(0 = the Boehm stub that made every earlier allocation test blind)");
            Assert.Pass();
        }
    }
}
