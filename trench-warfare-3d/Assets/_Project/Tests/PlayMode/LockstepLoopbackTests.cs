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

        /// <summary>M1 acceptance: 2,000 units, both garrisons sent over the top, 5,000 ticks over lossy loopback, identical hashes.</summary>
        [Test]
        public void M1_TwoThousandUnits_AdvanceAcrossCorridor_StayInSync()
        {
            const int perTeam = 1000;
            const uint target = 5000, advanceTick = 1500;
            var cfg = SimConfig.Default; cfg.StartingSilver = perTeam * 25 + 500;
            using var a = MatchSim.CreateGreybox(cfg, combat: false);
            using var b = MatchSim.CreateGreybox(cfg, combat: false);
            var net = new LoopbackNetwork(latencyTicks: 3, jitterTicks: 2, lossChance: 0.1f, seed: 11);
            var ra = new ReplayRecorder(cfg, default, 1);
            var rb = new ReplayRecorder(cfg, default, 1);
            var da = new LockstepDriver(a.World, net.A, ra);
            var db = new LockstepDriver(b.World, net.B, rb);
            int deployedA = 0, deployedB = 0;
            uint lastIssueA = uint.MaxValue, lastIssueB = uint.MaxValue;
            bool advancedA = false, advancedB = false;
            var sw = new System.Diagnostics.Stopwatch();
            long stepsA = 0;
            int guard = 2000000;
            while ((a.World.Tick < target || b.World.Tick < target) && guard-- > 0)
            {
                uint ta = a.World.Tick, tb = b.World.Tick;
                if (ta != lastIssueA)
                {
                    lastIssueA = ta;
                    for (int k = 0; k < 2 && deployedA < perTeam; k++, deployedA++) da.Issue(SimCommand.Deploy(0, 0, 0));
                    if (!advancedA && ta >= advanceTick) { advancedA = true; da.Issue(new SimCommand { Type = CommandType.TrenchAdvance, A = a.Fields.FrontTrench(0) }); }
                }
                if (tb != lastIssueB)
                {
                    lastIssueB = tb;
                    for (int k = 0; k < 2 && deployedB < perTeam; k++, deployedB++) db.Issue(SimCommand.Deploy(0, 1, 0));
                    if (!advancedB && tb >= advanceTick) { advancedB = true; db.Issue(new SimCommand { Type = CommandType.TrenchAdvance, A = b.Fields.FrontTrench(1) }); }
                }
                if (ta < target) { sw.Start(); if (da.TryStep()) stepsA++; sw.Stop(); }
                if (tb < target) db.TryStep();
            }
            Assert.Greater(guard, 0, "lockstep deadlocked");
            Assert.AreEqual(target, a.World.Tick);
            Assert.AreEqual(target, b.World.Tick);
            for (int t = 0; t < (int)target; t++) Assert.AreEqual(ra.Hashes[t], rb.Hashes[t], $"peers diverged at tick {t}");
            Assert.GreaterOrEqual(a.World.AliveCount, perTeam * 2 - 50, "deployments executed on both sides");
            int crossed0 = 0, crossed1 = 0;
            var size = a.Map.SizeMeters;
            for (int i = 0; i < a.World.HighWater; i++)
            {
                if (!a.World.IsAlive(i)) continue;
                if (a.World.Team[i] == 0 && a.World.Position[i].z > size.y * 0.5f) crossed0++;
                if (a.World.Team[i] == 1 && a.World.Position[i].z < size.y * 0.5f) crossed1++;
            }
            Assert.Greater(crossed0, perTeam / 2, "team 0 crossed the corridor after >>");
            Assert.Greater(crossed1, perTeam / 2, "team 1 crossed the corridor after >>");
            double msPerTick = sw.Elapsed.TotalMilliseconds / System.Math.Max(1, stepsA);
            UnityEngine.Debug.Log($"M1 stress: {a.World.AliveCount} units, {target} ticks, sim {msPerTick:F2} ms/tick (budget 3 ms, warm Burst)");
            Assert.Less(msPerTick, 25.0, "sim step time exploded (budget is 3 ms/tick on target hardware)");
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
