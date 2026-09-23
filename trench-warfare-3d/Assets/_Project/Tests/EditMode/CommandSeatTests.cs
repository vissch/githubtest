// Phase: tooling (perf pass, 2026-09-23) — the enemy seat without a world keeps the player's world stepping, and its
// orders land where a peer driver's would: InputDelay ticks after they were issued.
using NUnit.Framework;
using TW.Net;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Tests
{
    public class CommandSeatTests
    {
        [Test]
        public void ThePlayersWorldNeverStallsAndTheSeatsOrdersLandInputDelayLater()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 3000;
            using var m = MatchSim.CreateGreybox(cfg);
            var net = new LoopbackNetwork(0, 0, 0f, 1);
            var driver = new LockstepDriver(m.World, net.A);
            var seat = new CommandSeat(net.B, cfg.InputDelayTicks);
            uint issuedAt = 40;
            int spawnedAt = -1;
            for (int pass = 0; pass < 300; pass++)
            {
                uint t = m.World.Tick;
                if (t == issuedAt && pass == (int)issuedAt) seat.Issue(SimCommand.Deploy(t, 1, 0));
                seat.Pump(t);
                Assert.That(driver.TryStep(), Is.True, $"the player's world stalled at tick {t}: the seat's frame was not there");
                if (spawnedAt < 0)
                    for (int i = 0; i < m.World.HighWater; i++) if (m.World.IsAlive(i) && m.World.Team[i] == 1) { spawnedAt = (int)t; break; }
            }
            Assert.That(m.World.Tick, Is.EqualTo(300u));
            Assert.That(spawnedAt, Is.EqualTo((int)issuedAt + cfg.InputDelayTicks), "the seat's deploy executed on the wrong tick");
            Assert.That(seat.Player, Is.EqualTo(1));
        }
    }
}
