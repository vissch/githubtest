// Phase: review fix (2026-10-14) — LockstepSession.AlignWorlds steps the player's world outside StepOnce, so the
// enemy's script has to think for those ticks too; before the fix it did not and the enemy skipped them.
using NUnit.Framework;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Tests
{
    public class AlignWorldsTests
    {
        // [M5] every tick the player's world steps is a tick the enemy decided orders for, aligning included.
        [Test]
        public void TheEnemyThinksForTheTickAlignWorldsSteps()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 3000;
            using var session = new LockstepSession(() => MatchSim.CreatePlaytest(cfg), true, 2, 0, 0f, 12345u);
            var ai = new ScriptedEnemy { Side = 1 };
            for (int k = 0; k < 60; k++) session.StepOnce(ai);
            // the drift AlignWorlds exists for: the peer's world a tick ahead of the player's, as it is between frames
            for (int k = 0; k < 16 && session.Peer.World.Tick <= session.Local.World.Tick; k++) session.PeerDriver.TryStep();
            Assert.Less(session.Local.World.Tick, session.Peer.World.Tick, "[M5] the two worlds never drifted apart, so there is nothing to align");

            Assert.IsTrue(session.AlignWorlds(ai, null), "[M5] AlignWorlds could not bring the worlds together");
            Assert.AreEqual(session.Local.World.Tick, ai.DecidedAt, "[M5] the enemy AI skipped the tick AlignWorlds stepped");
        }
    }
}
