// Phase: tooling (perf pass, 2026-09-23) — skipping the per-tick hash changes nothing but LastHash.
// SimWorld.HashInterval lets single player stop hashing the whole state every tick. That is only safe if the hash is
// a pure read: no system may depend on it, and a world that skipped it must be in exactly the state of one that did.
// These run the same commands through worlds hashing every tick, every tenth and never, and compare the final state.
using System;
using NUnit.Framework;
using Unity.Collections;
using TW.Net;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Tests
{
    public class HashIntervalTests
    {
        const int Ticks = 300;

        [Test]
        public void SkippingTheHashLeavesTheStateIdentical()
        {
            ulong every = Run(1, out var lastEvery), tenth = Run(10, out var lastTenth), never = Run(0, out var lastNever);
            Assert.That(tenth, Is.EqualTo(every), "hashing every tenth tick changed the state");
            Assert.That(never, Is.EqualTo(every), "never hashing changed the state");
            Assert.That(lastNever, Is.EqualTo(0UL), "a world that never hashes reports LastHash 0");
            Assert.That(lastTenth, Is.EqualTo(Ticks % 10 == 0 ? lastEvery : 0UL), "a tenth-tick world hashes on the tenth ticks only");
        }

        [Test]
        public void AReplayRecorderRefusesAWorldThatDoesNotHashEveryTick()
        {
            var cfg = SimConfig.Default;
            using var m = MatchSim.CreateGreybox(cfg);
            m.World.HashInterval = 0;
            var net = new LoopbackNetwork(0, 0, 0f, 1);
            Assert.Throws<ArgumentException>(() => new LockstepDriver(m.World, net.A, new ReplayRecorder(cfg, default, 1)));
        }

        static ulong Run(int interval, out ulong lastHash)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 3000;
            using var m = MatchSim.CreateGreybox(cfg);
            m.World.HashInterval = interval;
            for (uint t = 0; t < Ticks; t++)
            {
                var cmds = new NativeArray<SimCommand>(t % 15 == 0 ? 2 : 0, Allocator.Temp);
                if (cmds.Length > 0) { cmds[0] = SimCommand.Deploy(t, 0, (int)(t % 3)); cmds[1] = SimCommand.Deploy(t, 1, (int)(t % 2)); }
                m.World.Step(cmds);
                cmds.Dispose();
            }
            lastHash = m.World.LastHash;
            return m.World.Hash();
        }
    }
}
