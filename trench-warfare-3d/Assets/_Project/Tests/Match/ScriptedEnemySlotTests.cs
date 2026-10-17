// Phase: perf (found 2026-10-17) - ArmedSlot/MixSlot/MachineSlot used to rescan the roster on every call; now they
// cache the plan per (world, side) the first time it is asked for, because the roster is fixed once FillRosters runs
// (MatchSim.cs:148). (a) pins the picks against the Brass table (FactionRoster.cs:32-34) so a rewrite that changes a
// pick goes red here, not quietly; (b) runs the stress preset and checks the roster was scanned at most twice (the
// two (world, side) pairs single player has), which is the test that goes red on the old, per-tick-scanning file.
using NUnit.Framework;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class ScriptedEnemySlotTests
    {
        [Test]
        public void SlotPicks_MatchTheBrassTable()
        {
            using var m = MatchSim.CreateBattlefield(SimConfig.Default, BattlefieldParams.ShelledForest(1917u));
            var w = m.World;
            var ai = new ScriptedEnemy();

            // Brass (side 1): 0 Rifle, 1 Assault, 2 Machinegunner, 3 Sniper, 4 Medic, 5 Jetpack, 6 Tusk, 7-9 vehicles.
            int[] expectedArmed = { 0, 1, 2, 3, 5, 0 };
            for (int n = 0; n < expectedArmed.Length; n++)
                Assert.That(ai.ArmedSlot(w, n), Is.EqualTo(expectedArmed[n]), $"ArmedSlot({n})");

            int[] expectedMix = { 0, 1, 0, 2, 0, 3, 0, 5, 0, 1 };
            for (int n = 0; n < expectedMix.Length; n++)
                Assert.That(ai.MixSlot(w, 1, n), Is.EqualTo(expectedMix[n]), $"MixSlot(1, {n})");

            Assert.That(ai.MachineSlot(w), Is.EqualTo(6));
        }

        [Test]
        public void StressPreset_ScansTheRosterAtMostOncePerWorldAndSide()
        {
            var cfg = SimConfig.Default;
            cfg.StartingSilver = 200 * 25;
            using var session = new LockstepSession(() => SimHost.StressPreset(MatchSim.CreateBattlefield(cfg, BattlefieldParams.ShelledForest(1917u))),
                                                      false, 0, 0, 0f, cfg.Seed);
            var ai = new ScriptedEnemy { StressUnits = 200, StressMix = true, DeploysTanks = true };
            int guard = 300 * 40;
            while (session.Local.World.Tick < 300 && guard-- > 0) session.StepOnce(ai);
            Assert.That(guard > 0, "lockstep stalled");
            Assert.That(ai.RosterScans, Is.LessThanOrEqualTo(2), "the roster should be scanned once per (world, side), not every tick");
        }
    }
}
