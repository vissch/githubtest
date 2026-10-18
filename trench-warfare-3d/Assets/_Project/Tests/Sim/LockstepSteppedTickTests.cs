// Phase: N1 (tests) — LastSteppedTick must name the tick the world last finished stepping, not the next one.
// Review fix [found-laststeppedtick-returns-the-next-tick-not-th-20261016-0]: SimWorld.Step increments Tick after
// the systems, so World.Tick is the next tick to step. LastSteppedTick used to return World.Tick unchanged, one
// tick too high in every state.
using NUnit.Framework;
using TW.Net;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Tests
{
    public class LockstepSteppedTickTests
    {
        sealed class ScriptedTransport : ILockstepTransport
        {
            readonly System.Collections.Generic.List<CommandFrame> queue = new System.Collections.Generic.List<CommandFrame>();
            public int LocalPlayer => 0;
            public int PeerCount => 2;
            public void Send(in CommandFrame frame) { queue.Add(frame); }
            public void Receive(System.Collections.Generic.List<CommandFrame> into) { into.AddRange(queue); queue.Clear(); }
            public void Pump(int simTick) { }
            public void Deliver(CommandFrame frame) => queue.Add(frame);
            public void DeliverPeerFrames(uint fromTick, uint toTick)
            {
                for (uint t = fromTick; t <= toTick; t++)
                    Deliver(new CommandFrame { Tick = t, Player = 1, Commands = System.Array.Empty<SimCommand>() });
            }
        }

        [Test] // [found-laststeppedtick-returns-the-next-tick-not-th-20261016-0]
        public void LastSteppedTickIsTheTickTheWorldJustStepped()
        {
            var cfg = SimConfig.Default;
            using var m = MatchSim.CreateGreybox(cfg);
            var net = new ScriptedTransport();
            var driver = new LockstepDriver(m.World, net);
            net.DeliverPeerFrames(0, 10);

            Assert.That(driver.LastSteppedTick, Is.EqualTo(uint.MaxValue),
                "[found-laststeppedtick-returns-the-next-tick-not-th-20261016-0] before any step, LastSteppedTick must say nothing was stepped");
            Assert.That(m.World.Tick, Is.EqualTo(0u),
                "[found-laststeppedtick-returns-the-next-tick-not-th-20261016-0] the world has not stepped yet");

            Assert.That(driver.TryStep(), Is.True,
                "[found-laststeppedtick-returns-the-next-tick-not-th-20261016-0] the first step stalled");
            Assert.That(driver.LastSteppedTick, Is.EqualTo(0u),
                "[found-laststeppedtick-returns-the-next-tick-not-th-20261016-0] tick 0 was just stepped");
            Assert.That(m.World.Tick, Is.EqualTo(1u),
                "[found-laststeppedtick-returns-the-next-tick-not-th-20261016-0] World.Tick is now the next tick to step");

            Assert.That(driver.TryStep(), Is.True,
                "[found-laststeppedtick-returns-the-next-tick-not-th-20261016-0] the second step stalled");
            Assert.That(driver.LastSteppedTick, Is.EqualTo(1u),
                "[found-laststeppedtick-returns-the-next-tick-not-th-20261016-0] tick 1 was just stepped");
            Assert.That(m.World.Tick, Is.EqualTo(2u),
                "[found-laststeppedtick-returns-the-next-tick-not-th-20261016-0] World.Tick is now the next tick to step");
        }
    }
}
