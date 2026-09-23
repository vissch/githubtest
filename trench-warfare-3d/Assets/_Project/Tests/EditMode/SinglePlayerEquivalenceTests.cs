// Phase: tooling (perf pass, 2026-09-23) — one world plays the same match the two-world lockstep played.
// Single player stopped simulating the match twice (owner decision, 2026-09-23): the enemy's seat now sends its
// orders without a world of its own, and its script reads the player's world. That is only a performance change if
// the match is the same, so this plays one match three ways and compares the full-state hash after EVERY tick:
//   one world + the enemy seat, zero-lag loopback          (single player now)
//   the determinism canary, two worlds, zero-lag loopback
//   the canary under the latency, jitter and loss SimHost used to run every session with
// with the scripted enemy deploying, locking and advancing trenches, calling barrages and gas, and the player deploying
// too. The third run is also the regression test for the double-issue the old host loop had under latency (a stalled
// peer re-ran the script at the same tick): with it, run three diverges from run one.
using NUnit.Framework;
using TW.Net;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Tests
{
    public class SinglePlayerEquivalenceTests
    {
        const int Ticks = 1000;

        [Test]
        public void OneWorldPlaysTheSameMatchAsTheTwoWorldCanary()
        {
            var one = Play(canary: false, 0, 0, 0f, out int oneEnemy, out bool oneDesync);
            var even = Play(canary: true, 0, 0, 0f, out int evenEnemy, out bool evenDesync);
            var lossy = Play(canary: true, 2, 1, 0.05f, out int lossyEnemy, out bool lossyDesync);

            Assert.That(oneEnemy, Is.GreaterThan(5), "the scripted enemy deployed nothing, so this compared two empty matches");
            Assert.That(evenDesync || lossyDesync || oneDesync, Is.False, "a canary run desynced against itself");
            for (int t = 0; t < Ticks; t++)
            {
                Assert.That(one[t], Is.EqualTo(even[t]), $"one world diverged from the zero-lag canary at tick {t}");
                Assert.That(lossy[t], Is.EqualTo(even[t]), $"the canary under latency diverged at tick {t}: the enemy's orders depend on the network again");
            }
            TestContext.WriteLine($"{Ticks} ticks hash-identical three ways; enemy men at the end: {oneEnemy} / {evenEnemy} / {lossyEnemy}");
        }

        static ulong[] Play(bool canary, int latency, int jitter, float loss, out int enemyAlive, out bool desync)
        {
            var cfg = SimConfig.Default;
            cfg.StartingSilver = 4000;
            using var session = new LockstepSession(() => MatchSim.CreatePlaytest(cfg), canary, latency, jitter, loss, cfg.Seed);
            session.Local.World.HashInterval = 1;   // this test compares every tick; single player would skip it
            // the stress preset too: it orders BOTH sides, and it once timed the player's orders by the enemy's tick
            var ai = new ScriptedEnemy { Enabled = true, DeployEveryTicks = 20, Attacks = true, AttackGarrison = 6, UsesSupport = true, SupportReserve = 100,
                                         StressUnits = 60, StressAdvanceDelayTicks = 120 };
            var hashes = new ulong[Ticks];
            uint lastOrder = uint.MaxValue;
            int guard = Ticks * 40;
            while (session.Local.World.Tick < Ticks && guard-- > 0)
            {
                uint t = session.Local.World.Tick;
                if (t % 30 == 0 && t != lastOrder) { lastOrder = t; session.LocalDriver.Issue(SimCommand.Deploy(t, 0, (int)(t / 30 % 3))); }
                if (session.StepOnce(ai)) hashes[session.Local.World.Tick - 1] = session.Local.World.LastHash;
            }
            Assert.That(guard, Is.GreaterThan(0), $"lockstep stalled (canary {canary}, latency {latency})");
            var w = session.Local.World;
            enemyAlive = 0;
            for (int i = 0; i < w.HighWater; i++) if (w.IsAlive(i) && w.Team[i] == 1) enemyAlive++;
            desync = session.Desync;
            return hashes;
        }
    }
}
