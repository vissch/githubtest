// Phase: T41 (cleanup, review 2026-10-04) — the 5,000-tick stress run
// (Stress_ThreeThousandUnits_AdvanceAcrossCorridor_StayInSync) must stay off the default tier: it is paid for on
// every gate run otherwise.
using System.Linq;
using System.Reflection;
using NUnit.Framework;

namespace TW.Tests
{
    public class TestTierGuardPlayTests
    {
        [Test]
        public void TheStressRunIsOnTheLongTier()
        {
            var m = typeof(LockstepLoopbackTests).GetMethod("Stress_ThreeThousandUnits_AdvanceAcrossCorridor_StayInSync",
                BindingFlags.Public | BindingFlags.Instance);
            bool tagged = m.GetCustomAttributes<CategoryAttribute>().Any(c => c.Name == "Long");
            Assert.That(tagged, Is.True, "[T41] the 5,000-tick stress run is off the Long tier");
        }
    }
}
