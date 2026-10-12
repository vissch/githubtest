// Phase: N1 (tests) — the lockstep driver drops a frame it must not step on.
// Review fix [S6] (CODE_REVIEW_2026-10-04, S6): the driver validated nothing it received. A frame from a player
// number no peer has completed a tick on its own, a frame for a tick already stepped was buffered forever, and a
// resend overwrote the frame the tick was going to step. These three tests feed such frames in through a scripted
// transport and check the driver drops them.
using System.Collections.Generic;
using NUnit.Framework;
using TW.Net;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Tests
{
    public class LockstepFrameValidationTests
    {
        /// <summary>A transport the test writes the "peer" frames of by hand. Like the loopback, a frame the driver
        /// sends is delivered back to it at once; everything queued is handed over on the next Receive.</summary>
        sealed class ScriptedTransport : ILockstepTransport
        {
            readonly List<CommandFrame> queue = new List<CommandFrame>();
            public readonly List<CommandFrame> Sent = new List<CommandFrame>();
            public int LocalPlayer => 0;
            public int PeerCount => 2;
            public void Send(in CommandFrame frame) { Sent.Add(frame); queue.Add(frame); }
            public void Receive(List<CommandFrame> into) { into.AddRange(queue); queue.Clear(); }
            public void Pump(int simTick) { }
            public void Deliver(CommandFrame frame) => queue.Add(frame);
            public void DeliverPeerFrames(uint fromTick, uint toTick)
            {
                for (uint t = fromTick; t <= toTick; t++)
                    Deliver(new CommandFrame { Tick = t, Player = 1, Commands = System.Array.Empty<SimCommand>() });
            }
        }

        static SimConfig Config() { var cfg = SimConfig.Default; cfg.StartingSilver = 3000; return cfg; }

        static int AliveOnTeam(SimWorld w, int team)
        {
            int n = 0;
            for (int i = 0; i < w.HighWater; i++) if (w.IsAlive(i) && w.Team[i] == team) n++;
            return n;
        }

        [Test] // [S6]
        public void AFrameFromAPlayerNoPeerHasCannotCompleteATick()
        {
            using var m = MatchSim.CreateGreybox(Config());
            var net = new ScriptedTransport();
            var driver = new LockstepDriver(m.World, net);
            net.Deliver(new CommandFrame { Tick = 0, Player = (byte)net.PeerCount, Commands = System.Array.Empty<SimCommand>() });
            Assert.That(driver.TryStep(), Is.False, "[S6] a frame from player 2 of a two-peer match completed tick 0 with no frame from the real peer");
            Assert.That(m.World.Tick, Is.EqualTo(0u), "[S6] the world stepped on a bogus player's frame");
        }

        [Test] // [S6]
        public void AResentFrameDoesNotReplaceTheOneTheTickWillStep()
        {
            using var m = MatchSim.CreateGreybox(Config());
            var net = new ScriptedTransport();
            var driver = new LockstepDriver(m.World, net);
            net.Deliver(new CommandFrame { Tick = 0, Player = 1, Commands = new[] { SimCommand.Deploy(0, 1, 0) } });
            net.Deliver(new CommandFrame { Tick = 0, Player = 1, Commands = System.Array.Empty<SimCommand>() }); // the same (tick, player), empty
            Assert.That(driver.TryStep(), Is.True, "[S6] tick 0 did not step although both peers had reported");
            Assert.That(AliveOnTeam(m.World, 1), Is.GreaterThan(0), "[S6] the duplicate empty frame erased the peer's deploy for tick 0");
        }

        [Test] // [S6]
        public void AFrameForATickAlreadySteppedIsNotBuffered()
        {
            using var mA = MatchSim.CreateGreybox(Config());
            using var mB = MatchSim.CreateGreybox(Config());
            var netA = new ScriptedTransport(); var netB = new ScriptedTransport();
            var driverA = new LockstepDriver(mA.World, netA);
            var driverB = new LockstepDriver(mB.World, netB);
            netA.DeliverPeerFrames(0, 30); netB.DeliverPeerFrames(0, 30);
            for (int i = 0; i < 5; i++)
            {
                Assert.That(driverA.TryStep(), Is.True, "[S6] the twin without the stale frame stalled");
                Assert.That(driverB.TryStep(), Is.True, "[S6] the twin fed the stale frame stalled");
            }
            Assert.That(mB.World.Tick, Is.EqualTo(5u));
            netB.Deliver(new CommandFrame { Tick = 0, Player = 1, Commands = System.Array.Empty<SimCommand>() }); // tick 0 is long stepped
            driverA.TryStep(); driverB.TryStep();
            Assert.That(driverB.BufferedTicks, Is.EqualTo(driverA.BufferedTicks), "[S6] the frame for the stepped tick 0 is still buffered: nothing ever removes it");
        }
    }
}
