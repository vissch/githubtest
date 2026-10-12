// Phase: P0 (implemented) — the sim's upgrade seam: the tiers a side brings to a match (SideUpgrades), carried in
// SimConfig and the replay header. The Home Front sells these for gold; before this seam nothing reached the match
// ([Z009.1], the SIM half). This file guards the seam itself; the per-kind effects are proved where they are applied.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Sim.Units;

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


        // ---------------------------------------------------------------- what a tier does in the match
        static MatchSim Playtest(SideUpgrades a)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.Seed = 0xC0FFEE; cfg.UpgradesA = a;
            return MatchSim.CreatePlaytest(cfg);
        }

        /// <summary>A Maw of team 0, stopped, facing up the map (+Z) as TankTests spawns one: a round from +X meets its
        /// side plate square.</summary>
        static int SpawnMaw(MatchSim m, float3 at)
        {
            int tank = m.World.Spawn(0, VehicleArchetype.Maw, at, RosterEntry.Maw.Hp, 0f, true);
            var c = m.Map.NavCellOf(at);
            m.World.GoalId[tank] = m.Fields.GetGoal(GoalKey.Cell(m.Map.NavIndex(c.x, m.Map.NavLength - 6), NavMode.Tracked));
            return tank;
        }

        static void Step(MatchSim m, params SimCommand[] cmds)
        {
            using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
            m.Step(arr);
        }

        static void FlankShot(MatchSim m, int tank)
            => m.Gunnery.PendingHits.Add(new VehicleHit
            {
                Target = tank, Shooter = -1, Kind = VehicleHitKind.ArmourPiercing, PenMm = 10f, Damage = 650f,
                Pos = m.World.Position[tank], Dir = new float3(1f, 0f, 0f),
            });

        /// <summary>ARMOUR PLATE: a Maw's side is 8 mm (TankTests pins the profile at 12 / 8 / 6 / 6), so a 10 mm
        /// round holes it square on. Two tiers bolt on 4 mm more; the same round now rings off 12 mm. The shop sold
        /// this and nothing happened before the seam. [Z009.1-sim]</summary>
        [Test]
        public void ArmourPlate_StopsTheFlankShotThatHoledAtTierZero()
        {
            float3 at = new float3(150f, 0f, 100f);
            using (var bare = Playtest(default))
            {
                int tank = SpawnMaw(bare, at);
                Step(bare);
                FlankShot(bare, tank);
                Step(bare);
                Assert.AreEqual(1, bare.Modules.Penetrations, "[Z009.1-sim] 10 mm goes through a Maw's bare 8 mm side");
                Assert.AreEqual(0, bare.Modules.Ricochets, "[Z009.1-sim] 10 mm goes through a Maw's bare 8 mm side");
            }
            using (var plated = Playtest(SideUpgrades.Global(armour: 2, abilityCooldown: 0, abilityCost: 0)))
            {
                int tank = SpawnMaw(plated, at);
                Step(plated);
                FlankShot(plated, tank);
                Step(plated);
                Assert.AreEqual(1, plated.Modules.Ricochets, "[Z009.1-sim] two tiers of armour plate (+4 mm) must stop a 10 mm round on a 12 mm side");
                Assert.AreEqual(0, plated.Modules.Penetrations, "[Z009.1-sim] two tiers of armour plate (+4 mm) must stop a 10 mm round on a 12 mm side");
            }
        }

        static SimCommand Barrage(MatchSim m, byte player)
            => new SimCommand { Tick = m.World.Tick, Player = player, Type = CommandType.SupportFire, A = (int)OffMapAbilityId.HeBarrage, Pos = new float3(150f, 0f, 150f) };

        /// <summary>REQUISITION: an HE barrage costs 150 silver, so two tiers (-8 % a tier) spend 150 * 84 / 100 = 126.
        /// The other side bought nothing and pays the full price out of the same table. [Z009.1-sim]</summary>
        [Test]
        public void Requisition_MakesTheCallCostEightPercentLessATier()
        {
            using var m = Playtest(SideUpgrades.Global(armour: 0, abilityCooldown: 0, abilityCost: 2));
            Step(m);
            int before0 = m.World.Silver[0], before1 = m.World.Silver[1];
            Step(m, Barrage(m, 0), Barrage(m, 1));
            Assert.AreEqual(126, before0 - m.World.Silver[0], "[Z009.1-sim] two tiers of requisition must spend 126 of the 150 an HE barrage lists");
            Assert.AreEqual(150, before1 - m.World.Silver[1], "[Z009.1-sim] a side that bought no requisition pays the listed 150");
        }

        /// <summary>SIGNAL DISCIPLINE: an HE barrage waits 1200 ticks, so two tiers (-8 % a tier) wait
        /// 1200 * 84 / 100 = 1008. [Z009.1-sim]</summary>
        [Test]
        public void SignalDiscipline_MakesTheWaitEightPercentShorterATier()
        {
            using var m = Playtest(SideUpgrades.Global(armour: 0, abilityCooldown: 2, abilityCost: 0));
            Step(m);
            Step(m, Barrage(m, 0), Barrage(m, 1));
            Assert.AreEqual(1008, m.Abilities.CooldownOf(0, OffMapAbilityId.HeBarrage), "[Z009.1-sim] two tiers of signal discipline must cut the 1200 tick wait to 1008");
            Assert.AreEqual(1200, m.Abilities.CooldownOf(1, OffMapAbilityId.HeBarrage), "[Z009.1-sim] a side that bought no signal discipline waits the listed 1200");
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
