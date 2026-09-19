// Phase: N1 (implemented) — two sims over the loopback transport with latency, jitter and loss stay hash-identical.
using NUnit.Framework;
using TW.Net;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Tests
{
    public class LockstepLoopbackTests
    {
        [Test]
        public void TwoPeers_StayInSync_UnderLatencyJitterAndLoss()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 5000;
            using var a = MatchSim.CreateGreybox(cfg);
            using var b = MatchSim.CreateGreybox(cfg);
            var net = new LoopbackNetwork(latencyTicks: 3, jitterTicks: 2, lossChance: 0.1f, seed: 7);
            var ra = new ReplayRecorder(cfg, default, 1);
            var rb = new ReplayRecorder(cfg, default, 1);
            var da = new LockstepDriver(a.World, net.A, ra);
            var db = new LockstepDriver(b.World, net.B, rb);
            const uint target = 1000;
            int guard = 200000;
            while ((a.World.Tick < target || b.World.Tick < target) && guard-- > 0)
            {
                if (a.World.Tick % 9 == 0) da.Issue(SimCommand.Deploy(0, 0, (int)(a.World.Tick % 5)));
                if (b.World.Tick % 13 == 0) db.Issue(SimCommand.Deploy(0, 1, (int)(b.World.Tick % 3)));
                if (a.World.Tick < target) da.TryStep();
                if (b.World.Tick < target) db.TryStep();
            }
            Assert.Greater(guard, 0, "lockstep deadlocked");
            Assert.AreEqual(target, a.World.Tick);
            Assert.AreEqual(target, b.World.Tick);
            for (int t = 0; t < (int)target; t++) Assert.AreEqual(ra.Hashes[t], rb.Hashes[t], $"peers diverged at tick {t}");
            Assert.Greater(a.World.AliveCount, 10, "commands from both peers were executed");
        }

        [Test]
        public void MissingPeerFrame_StallsInsteadOfStepping()
        {
            var cfg = SimConfig.Default;
            using var a = MatchSim.CreateGreybox(cfg);
            var net = new LoopbackNetwork(latencyTicks: 1000);
            var da = new LockstepDriver(a.World, net.A);
            for (int i = 0; i < 50; i++) da.TryStep();
            Assert.AreEqual(0u, a.World.Tick);
            Assert.Greater(da.StallTicks, 0);
        }
    }
}
