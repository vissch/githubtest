// Phase: A5 — review C8 and U13: the IndirectFireSystem stub threw from Step and was never registered; it and its ScheduledImpact
// (which had no other user) are gone, and review U13 did the same for the six other never-registered stubs. This
// holds them gone, and holds the live types beside them alive.
using System.Reflection;
using NUnit.Framework;
using TW.Sim.Combat;

namespace TW.Tests
{
    public class SimStubTests
    {
        // [N8]
        [Test]
        public void TheUnusedPushXParameterIsGone()
        {
            var job = typeof(TW.Sim.Nav.MovementSystem).GetNestedType("MoveJob", BindingFlags.NonPublic);
            Assert.IsNotNull(job, "[N8] MoveJob is gone or renamed: cannot check CanEnter");
            var m = job.GetMethod("CanEnter", BindingFlags.NonPublic | BindingFlags.Instance);
            Assert.IsNotNull(m, "[N8] MoveJob.CanEnter is gone or renamed: cannot check its parameters");

            var parameters = m.GetParameters();
            Assert.IsFalse(System.Array.Exists(parameters, p => p.Name == "pushX"),
                "[N8] MoveJob.CanEnter still takes pushX: it is never read");
            Assert.AreEqual(6, parameters.Length,
                "[N8] MoveJob.CanEnter still takes pushX: it is never read");
        }

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

        // [U13]
        [Test]
        public void TheNeverRegisteredStubsAreGone()
        {
            var match = typeof(TW.Sim.Match.MissionScript).Assembly;   // TW.Sim.Match
            var units = typeof(TW.Sim.Units.UnitStats).Assembly;       // TW.Sim.Units

            foreach (var name in new[]
            {
                "TW.Sim.Units.GrenadeSystem",
                "TW.Sim.Units.StanceSystem",
            })
                Assert.IsNull(units.GetType(name),
                    "[U13] " + name + " is back: an unregistered sim system whose Step throws");

            foreach (var name in new[]
            {
                "TW.Sim.Match.LogisticsSystem",
                "TW.Sim.Match.WaveAiSystem",
                "TW.Sim.Match.Difficulty",
                "TW.Sim.Match.MissionRunner",
            })
                Assert.IsNull(match.GetType(name),
                    "[U13] " + name + " is back: an unregistered sim system whose Step throws (or its only user)");

            Assert.IsNotNull(match.GetType("TW.Sim.Match.MissionScript"),
                "[U13] MissionScript is baked mission data, not a stub: it must stay");
            Assert.IsNotNull(units.GetType("TW.Sim.Units.UnitStats"),
                "[U13] UnitStats is a data struct TW.Data bakes, not a stub: it must stay");
        }
    }
}
