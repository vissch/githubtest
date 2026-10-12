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

        /// <summary>HEALTH: the rifleman's line is 100 hp (RosterEntry.Rifleman), so two tiers of the track the shop
        /// sells (+5 % a tier) put him in the field with 110. The slot's roster line carries it, so he brings it
        /// wherever he arrives (Deploy, SeaLift). [Z009.1-sim]</summary>
        [Test]
        public void HealthTrack_PutsTheRiflemanInTheFieldWithMoreHitPoints()
        {
            var ups = default(SideUpgrades);
            Assert.IsTrue(ups.SetTrack(InfantryArchetype.Rifle, health: 2, damage: 0, accuracy: 0));
            Assert.AreEqual(100f, RosterEntry.Rifleman.Hp, "the rifleman's line is 100 hp: the step below is read off that, not off a run");
            using var m = Playtest(ups);
            Step(m);
            Assert.AreEqual(InfantryArchetype.Rifle, m.World.Roster[0].Archetype, "setup: player 0's first slot is the rifleman");
            Assert.AreEqual(110f, m.World.Roster[0].Hp, 1e-3f, "[Z009.1-sim] two tiers of the health track must make the rifleman's slot 110 hp");
            Assert.AreEqual(100f, m.World.Roster[RosterEntry.SlotCount].Hp, 1e-3f, "[Z009.1-sim] the other side bought nothing and keeps the listed 100");

            int man = -1;
            Step(m, SimCommand.Deploy(m.World.Tick, 0, 0));
            for (int t = 0; t < 10 && man < 0; t++)
            {
                for (int i = 0; i < m.World.Config.MaxSlots; i++)
                    if ((m.World.Flags[i] & (uint)UnitFlags.Alive) != 0 && m.World.Team[i] == 0 && m.World.Archetype[i] == InfantryArchetype.Rifle) { man = i; break; }
                if (man < 0) Step(m);
            }
            Assert.Greater(man, -1, "setup: the deploy of slot 0 put a rifleman in the field");
            Assert.AreEqual(110f, m.World.MaxHp[man], 1e-3f, "[Z009.1-sim] the man the player paid for must stand up with 110 hit points, not 100");
            Assert.AreEqual(110f, m.World.Hp[man], 1e-3f, "[Z009.1-sim] and he arrives at full");
        }

        // ---------------------------------------------------------------- the damage and accuracy tracks, in a duel
        /// <summary>A rifleman of team 0 against an unarmed medic of team 1, nine metres off (past the eight at which
        /// somebody charges) and both too tough to die: every tick is a shot at the same mark, and the only thing that
        /// moves between two runs is the tier. The stray bombardment is switched off, as the flamethrower's rig does.</summary>
        sealed class Duel : System.IDisposable
        {
            public readonly MatchSim M;
            public readonly System.Collections.Generic.List<SimEvent> Log = new System.Collections.Generic.List<SimEvent>();
            public int Shooter, Target;
            public Duel(SideUpgrades a, uint seed)
            {
                var cfg = SimConfig.Default; cfg.Seed = seed; cfg.UpgradesA = a;
                M = MatchSim.CreatePlaytest(cfg);
                var stray = M.World.GetSystem<AmbientBombardmentSystem>();
                if (stray != null) stray.ShellsPerMinute = 0f;
                M.World.HashInterval = 1;
                float3 open = new float3(150f, 0f, 240f);
                Shooter = M.World.Spawn(0, InfantryArchetype.Rifle, open, 1e6f, 0f, false);
                Target = M.World.Spawn(1, InfantryArchetype.Medic, open + new float3(0f, 0f, 9f), 1e6f, 0f, false);
            }
            public void Run(int ticks)
            {
                for (int t = 0; t < ticks; t++)
                {
                    UpgradeSeamTests.Step(M);
                    var ev = M.World.Events.Events;
                    for (int k = 0; k < ev.Length; k++) Log.Add(ev[k]);
                }
            }
            /// <summary>The damage of the first round that landed on the mark, -1 when none did.</summary>
            public float FirstHit()
            {
                foreach (var e in Log) if (e.Type == SimEventType.Hit && e.A == Shooter && e.B == Target) return e.Scalar;
                return -1f;
            }
            public int Hits()
            {
                int n = 0;
                foreach (var e in Log) if (e.Type == SimEventType.Hit && e.A == Shooter && e.B == Target) n++;
                return n;
            }
            public void Dispose() => M.Dispose();
        }

        /// <summary>DAMAGE: the same duel, the same seed, so the same roll and the same 0.8-1.2 draw land the same
        /// round at both tiers; two tiers of the damage track (+5 % a tier) take exactly 1.10 times as much off him.
        /// The shop sold this and the round landed for the same hit points before the seam. [Z009.1-sim]</summary>
        [Test]
        public void DamageTrack_TakesTenPercentMoreOffHimAtTwoTiers()
        {
            var ups = default(SideUpgrades);
            Assert.IsTrue(ups.SetTrack(InfantryArchetype.Rifle, health: 0, damage: 2, accuracy: 0));
            using var bare = new Duel(default, 0xC0FFEE);
            using var paid = new Duel(ups, 0xC0FFEE);
            bare.Run(300); paid.Run(300);
            float d0 = bare.FirstHit(), d2 = paid.FirstHit();
            Assert.Greater(d0, 0f, "setup: the rifleman hit the medic inside 300 ticks");
            Assert.AreEqual(1.10f * d0, d2, 1e-3f * d0,
                "[Z009.1-sim] two tiers of the damage track must take 1.10 times as much off him (" + d0 + " -> " + d2 + ")");
        }

        /// <summary>ACCURACY: the step is the shop's (+5 % a tier, so 1.10 at two), and a better chance over the same
        /// 300 ticks of the same duel lands strictly more rounds. The seed is the first in 1..20 that shows it
        /// strictly; the exact step above is the rule, the count is the proof it reaches the shot. [Z009.1-sim]</summary>
        [Test]
        public void AccuracyTrack_LandsStrictlyMoreRoundsAtTwoTiers()
        {
            Assert.AreEqual(1.10f, Upgrades.Scale(UpgradeTrack.Accuracy, 2), "[Z009.1-sim] two tiers of accuracy is +10 %, the shop's step");
            var ups = default(SideUpgrades);
            Assert.IsTrue(ups.SetTrack(InfantryArchetype.Rifle, health: 0, damage: 0, accuracy: 2));
            for (uint seed = 1; seed <= 20; seed++)
            {
                int bareHits, paidHits;
                using (var bare = new Duel(default, seed))
                using (var paid = new Duel(ups, seed))
                {
                    bare.Run(300); paid.Run(300);
                    bareHits = bare.Hits(); paidHits = paid.Hits();
                }
                if (bareHits == 0) continue;                       // the duel must be a duel at tier 0 as well
                if (paidHits > bareHits) return;                   // [Z009.1-sim] strictly more rounds landed
            }
            Assert.Fail("[Z009.1-sim] no seed in 1..20 landed more rounds with two tiers of accuracy: the tier does not reach the shot");
        }

        /// <summary>A match recorded with tiers set re-simulates to the hashes it recorded: the tiers ride in the
        /// header, so the replay fights the upgraded battle and not the bare one. [Z009.1-sim]</summary>
        [Test]
        public void AReplayOfAnUpgradedMatchVerifies()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            cfg.UpgradesA = SideUpgrades.Global(armour: 3, abilityCooldown: 2, abilityCost: 1);
            Assert.IsTrue(cfg.UpgradesA.SetTrack(InfantryArchetype.Rifle, health: 2, damage: 3, accuracy: 4));
            cfg.UpgradesB = SideUpgrades.Global(armour: 1, abilityCooldown: 0, abilityCost: 2);
            Assert.IsTrue(cfg.UpgradesB.SetTrack(InfantryArchetype.Assault, health: 1, damage: 0, accuracy: 2));

            var recorder = new ReplayRecorder(cfg, default, TW.Sim.Terrain.GreyboxMapGenerator.MapId);
            using (var match = MatchSim.CreateGreybox(cfg))
            {
                match.World.HashInterval = 1;
                for (uint t = 0; t < 120; t++)
                {
                    using var cmds = new NativeArray<SimCommand>(Script(t), Allocator.Temp);
                    match.Step(cmds);
                    recorder.Record(cmds, match.World.LastHash);
                }
            }
            var player = ReplayPlayer.Parse(recorder.Serialize());
            Assert.AreEqual(120, player.TickCount);
            using var again = MatchSim.CreateGreybox(player.Config);
            again.World.HashInterval = 1;
            Assert.AreEqual(-1, player.Verify(again.World),
                "[Z009.1-sim] a replay of an upgraded match must re-simulate to the hashes it recorded");
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
