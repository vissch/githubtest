// Phase: N1 (tests) — the tick a command frame's hash belongs to.
// Review fix [S9] (CODE_REVIEW_2026-10-04, S9): the frame field was called HashOfPreviousTick, but a frame for tick
// T is sent InputDelay ticks early, so the hash it carries is the hash of tick T - InputDelay - 1. With InputDelay 2
// the frame for tick 5 carries the hash of tick 2, never of tick 4. The field is now Hash plus HashTick, which says
// which tick it is the hash of; nothing reads it yet, so this is a label.
using System.Collections.Generic;
using NUnit.Framework;
using TW.Net;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Tests
{
    public class LockstepHashLabelTests
    {
        sealed class RecordingTransport : ILockstepTransport
        {
            readonly List<CommandFrame> queue = new List<CommandFrame>();
            public readonly List<CommandFrame> Sent = new List<CommandFrame>();
            public int LocalPlayer => 0;
            public int PeerCount => 2;
            public void Send(in CommandFrame frame) { Sent.Add(frame); queue.Add(frame); }
            public void Receive(List<CommandFrame> into) { into.AddRange(queue); queue.Clear(); }
            public void Pump(int simTick) { }
            public void DeliverPeerFrames(uint fromTick, uint toTick)
            {
                for (uint t = fromTick; t <= toTick; t++)
                    queue.Add(new CommandFrame { Tick = t, Player = 1, Commands = System.Array.Empty<SimCommand>() });
            }
        }

        [Test] // [S9]
        public void EveryFrameSaysWhichTickItsHashIsOf()
        {
            var cfg = SimConfig.Default; cfg.InputDelayTicks = 2;
            using var m = MatchSim.CreateGreybox(cfg);
            Assert.That(m.World.HashInterval, Is.EqualTo(1), "[S9] this test needs a world that hashes every tick");
            var net = new RecordingTransport();
            var driver = new LockstepDriver(m.World, net);
            net.DeliverPeerFrames(0, 40);
            var hashOf = new Dictionary<uint, ulong>();   // tick -> the hash of the world after that tick was stepped
            for (int i = 0; i < 10; i++)
            {
                Assert.That(driver.TryStep(), Is.True, "[S9] the driver stalled");
                hashOf[m.World.Tick - 1u] = m.World.LastHash;
            }
            Assert.That(net.Sent.Count, Is.GreaterThanOrEqualTo(10), "[S9] the driver sent no frames to check");
            int checkedWithAHash = 0;
            foreach (var f in net.Sent)
            {
                if (f.Tick <= (uint)cfg.InputDelayTicks)
                {
                    Assert.That(f.Hash, Is.EqualTo(0UL), $"[S9] the primed frame for tick {f.Tick} carries a hash although no tick was stepped yet");
                    continue;
                }
                uint expected = f.Tick - (uint)cfg.InputDelayTicks - 1u;   // the finding's formula
                Assert.That(f.HashTick, Is.EqualTo(expected), $"[S9] the frame for tick {f.Tick} is labelled as the hash of tick {f.HashTick}, but it carries the hash of tick {expected}");
                Assert.That(f.HashTick, Is.Not.EqualTo(f.Tick - 1u), $"[S9] the frame for tick {f.Tick} carries the hash of the PREVIOUS tick, which the old field name claimed");
                Assert.That(hashOf.ContainsKey(f.HashTick), Is.True, $"[S9] tick {f.HashTick} was never stepped by this world");
                Assert.That(f.Hash, Is.EqualTo(hashOf[f.HashTick]), $"[S9] the frame for tick {f.Tick} does not carry the hash of tick {f.HashTick}");
                checkedWithAHash++;
            }
            Assert.That(checkedWithAHash, Is.GreaterThan(0), "[S9] no frame with a hash was checked");
        }
    }
}
