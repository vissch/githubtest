// Phase: T40 (cleanup, review 2026-10-04) — MatchLoopTests shares one static Defence (three ten-minute matches)
// across its tests; whichever runs first pays for them. Every test off the Explicit tier must be Category("Long")
// so the edit gate skips the whole half hour instead of an untagged test paying for it alone.
using System.Linq;
using System.Reflection;
using NUnit.Framework;

namespace TW.Tests
{
    public class TestTierGuardTests
    {
        [Test]
        public void EveryMatchLoopTestIsOnTheLongTier()
        {
            var untagged = typeof(MatchLoopTests).GetMethods(BindingFlags.Public | BindingFlags.Instance)
                .Where(m => m.GetCustomAttribute<TestAttribute>() != null && m.GetCustomAttribute<ExplicitAttribute>() == null)
                .Where(m => !m.GetCustomAttributes<CategoryAttribute>().Any(c => c.Name == "Long"))
                .Select(m => m.Name).ToList();
            Assert.That(untagged, Is.Empty,
                "[T40] a test off the Long tier pays for the shared ten-minute matches: " + string.Join(", ", untagged));
        }
    }
}
