// Phase: P0 (implemented) — the sim's upgrade seam: the tiers a side brings to a match (SideUpgrades), carried in
// SimConfig and the replay header. The Home Front sells these for gold; before this seam nothing reached the match
// ([Z009.1], the SIM half). This file guards the seam itself; the per-kind effects are proved where they are applied.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Tests
{
    public class UpgradeSeamTests
    {
        static SimCommand[] Script(uint t)
        {
            if (t % 5 == 0) return new[] { SimCommand.Deploy(t, 0, (int)(t / 5) % 5), SimCommand.Deploy(t, 1, (int)(t / 5 + 2) % 5) };
            return System.Array.Empty<SimCommand>();
        }

        /// <summary>The tiers a side bought reach the match through the config, so they are in the lockstep handshake
        /// and in the replay header: a replay that does not carry them replays a different battle. [Z009.1-sim]</summary>
        [Test]
        public void TheReplayHeaderCarriesTheUpgradesEachSideBrings()
        {
            var cfg = SimConfig.Default;
            cfg.UpgradesA = SideUpgrades.Global(armour: 2, abilityCooldown: 1, abilityCost: 3);
            Assert.IsTrue(cfg.UpgradesA.SetTrack(InfantryArchetype.Rifle, health: 2, damage: 4, accuracy: 6));
            cfg.UpgradesB = SideUpgrades.Global(armour: 0, abilityCooldown: 0, abilityCost: 1);

            var back = ReplayPlayer.Parse(new ReplayRecorder(cfg, default, 1).Serialize()).Config;

            Assert.AreEqual(38, ReplayRecorder.FormatVersion, "a header that carries the upgrades is v38's: bump with the format");
            Assert.AreEqual(2, back.UpgradesOf(0).Armour, "[Z009.1-sim] the armour plate a side paid for must reach the match");
            Assert.AreEqual(1, back.UpgradesOf(0).AbilityCooldown, "[Z009.1-sim] signal discipline must reach the match");
            Assert.AreEqual(3, back.UpgradesOf(0).AbilityCost, "[Z009.1-sim] requisition must reach the match");
            Assert.AreEqual(2, back.UpgradesOf(0).TierOf(InfantryArchetype.Rifle, UpgradeTrack.Health), "[Z009.1-sim] the health track must reach the match");
            Assert.AreEqual(4, back.UpgradesOf(0).TierOf(InfantryArchetype.Rifle, UpgradeTrack.Damage), "[Z009.1-sim] the damage track must reach the match");
            Assert.AreEqual(6, back.UpgradesOf(0).TierOf(InfantryArchetype.Rifle, UpgradeTrack.Accuracy), "[Z009.1-sim] the accuracy track must reach the match");
            Assert.AreEqual(0, back.UpgradesOf(0).TierOf(InfantryArchetype.Sniper, UpgradeTrack.Health), "a kind that bought nothing is at tier 0");
            Assert.AreEqual(1, back.UpgradesOf(1).AbilityCost, "[Z009.1-sim] the other side's rows are its own");
            Assert.AreEqual(0, back.UpgradesOf(1).Tracks.Length, "a side that bought no track carries an empty list");
        }

        /// <summary>The per-tier sizes are the shop's, built from integers so both machines agree bit for bit.</summary>
        [Test]
        public void ATiersSizeIsTheOneTheShopStates()
        {
            Assert.AreEqual(1f, Upgrades.Scale(UpgradeTrack.Accuracy, 0), "[Z009.1-sim] tier 0 changes nothing");
            Assert.AreEqual(1.10f, Upgrades.Scale(UpgradeTrack.Accuracy, 2), "[Z009.1-sim] a track is +5 % a tier");
            Assert.AreEqual(1.10f, Upgrades.Scale(UpgradeTrack.Health, 2), "[Z009.1-sim] a track is +5 % a tier");
            Assert.AreEqual(4f, Upgrades.ArmourMm(2), "[Z009.1-sim] armour plate is +2 mm a tier");
            Assert.AreEqual(126, Upgrades.Ints(150, 2), "[Z009.1-sim] an ability costs 8 % less a tier (150 -> 126)");
            Assert.AreEqual(1008, Upgrades.Ints(1200, 2), "[Z009.1-sim] an ability is ready 8 % sooner a tier (1200 -> 1008)");
            Assert.AreEqual(150, Upgrades.Ints(150, 0), "[Z009.1-sim] tier 0 changes nothing");
            Assert.AreEqual(6, Upgrades.ClampTrack(9), "a track runs to tier 6");
            Assert.AreEqual(3, Upgrades.ClampGlobal(9), "a global row runs to tier 3");
        }

        /// <summary>A match nobody upgraded plays exactly as it did before the seam existed. Pinned from a run on the
        /// tree before this change (commit f19b38ba1); deliberately NOT tagged with the finding's id, because it is
        /// green on that tree too: it is the guard that zero tiers changed nothing, not a proof of the fix.</summary>
        [Test]
        public void AMatchWithNoUpgradesHashesAsItDidBeforeTheSeam()
        {
            using var match = MatchSim.CreateGreybox(SimConfig.Default);
            for (uint t = 0; t < 60; t++)
            {
                using var cmds = new NativeArray<SimCommand>(Script(t), Allocator.Temp);
                match.Step(cmds);
            }
            Assert.AreEqual(13516860766580246688UL, match.World.LastHash,
                "60 ticks of the greybox match with every upgrade tier at zero must hash as it did before the seam: " + match.World.LastHash);
        }
    }
}
