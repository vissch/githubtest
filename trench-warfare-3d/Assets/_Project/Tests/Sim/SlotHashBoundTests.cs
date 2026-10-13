// Phase: P0 (implemented)
// [U11] The per-slot arrays of BreakerSystem, SapperSystem and SupportSystem are MaxSlots long but only live up to
// SimWorld.HighWater. Hash() must stop at HighWater, like SimWorld.Hash() does, so the untouched tail costs nothing
// and cannot make two machines disagree.
using NUnit.Framework;
using Unity.Collections;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Units;

namespace TW.Tests
{
    public class SlotHashBoundTests
    {
        static MatchSim NewMatch()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            return MatchSim.CreateGreybox(cfg);
        }

        static void Step(MatchSim m, params SimCommand[] cmds)
        {
            using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
            m.Step(arr);
        }

        /// <summary>[U11] A slot above HighWater is not state: poking one must not move any of the three hashes.</summary>
        [Test]
        public void SlotsAboveHighWaterAreNotHashed()
        {
            using var match = NewMatch();
            var w = match.World;
            for (byte p = 0; p < 2; p++) Step(match, SimCommand.Deploy(w.Tick, p, 0), SimCommand.Deploy(w.Tick, p, 6));
            for (int t = 0; t < 10; t++) Step(match);

            int hw = w.HighWater;
            Assert.Greater(hw, 0, "[U11] nobody deployed, so there is no tail to test");
            Assert.Less(hw, w.Config.MaxSlots, "[U11] the world is full: no slot above HighWater to poke");

            var breaker = w.GetSystem<BreakerSystem>();
            var sapper = w.GetSystem<SapperSystem>();
            var support = w.GetSystem<SupportSystem>();

            ulong b0 = breaker.Hash(SimHash.Offset), s0 = sapper.Hash(SimHash.Offset), u0 = support.Hash(SimHash.Offset);

            breaker.Phase[hw] = 3;
            sapper.Charges[hw] = 7;
            support.MendTimer[hw] = 99;

            Assert.AreEqual(b0, breaker.Hash(SimHash.Offset), "[U11] BreakerSystem hashes a slot above HighWater");
            Assert.AreEqual(s0, sapper.Hash(SimHash.Offset), "[U11] SapperSystem hashes a slot above HighWater");
            Assert.AreEqual(u0, support.Hash(SimHash.Offset), "[U11] SupportSystem hashes a slot above HighWater");
        }
    }
}
