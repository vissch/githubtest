// Phase: A5 — review C8: the IndirectFireSystem stub threw from Step and was never registered; it and its ScheduledImpact
// (which had no other user) are gone. This holds them gone.
using NUnit.Framework;
using TW.Sim.Combat;

namespace TW.Tests
{
    public class SimStubTests
    {
        // [C8]
        [Test]
        public void TheIndirectFireStubIsGone()
        {
            var sim = typeof(BlastSystem).Assembly;
            Assert.IsNull(sim.GetType("TW.Sim.Combat.IndirectFireSystem"),
                "[C8] IndirectFireSystem is back: an unregistered sim system whose Step throws");
            Assert.IsNull(sim.GetType("TW.Sim.Combat.ScheduledImpact"),
                "[C8] ScheduledImpact is back: it had no user but the deleted stub");
        }
    }
}
