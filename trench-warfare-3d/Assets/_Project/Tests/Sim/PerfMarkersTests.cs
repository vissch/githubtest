// Phase: tooling (perf pass, 2026-09-23) — proves PerfMarkers.ForSystem registers its name in PerfMarkers.Names.
using NUnit.Framework;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Tests
{
    public class PerfMarkersTests
    {
        [Test]
        public void ForSystem_RegistersEachSystemsMarkerName()
        {
            using var m = MatchSim.CreateGreybox(SimConfig.Default, combat: false);

            CollectionAssert.Contains(PerfMarkers.Names, "TW.Sim.Sys.MovementSystem");
            CollectionAssert.AllItemsAreUnique(PerfMarkers.Names);
            Assert.AreEqual("TW.Sim.Step", PerfMarkers.Names[0]);
        }
    }
}
