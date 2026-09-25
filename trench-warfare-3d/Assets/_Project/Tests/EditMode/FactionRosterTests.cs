// Phase: A3 (implemented 2026-09-25) — the faction seam: that the two faction tables reproduce the rosters every
// player had before factions existed, that the new archetype ids sit where the sim's masks and weapon ids allow,
// that the ability table knows the new ids, and that the v5 replay header carries the factions and the pity.
using NUnit.Framework;
using Unity.Collections;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Data;

namespace TW.Tests
{
    public class FactionRosterTests
    {
        static void AssertEntry(RosterEntry e, byte archetype, int cost, float hp, float speed, int cooldown, bool vehicle, string where)
        {
            Assert.AreEqual(archetype, e.Archetype, where + " archetype");
            Assert.AreEqual(cost, e.Cost, where + " cost");
            Assert.AreEqual(hp, e.Hp, where + " hp");
            Assert.AreEqual(speed, e.Speed, where + " speed");
            Assert.AreEqual(cooldown, e.CooldownTicks, where + " cooldown");
            Assert.AreEqual(vehicle, e.IsVehicle, where + " vehicle");
        }

        [Test]
        public void EachFactionFieldsItsOwnTen()
        {
            using var roster = new NativeArray<RosterEntry>(RosterEntry.SlotCount * 2, Allocator.Temp);
            FactionRoster.Fill(roster, 0, FactionId.Iron);
            FactionRoster.Fill(roster, RosterEntry.SlotCount, FactionId.Brass);
            for (int p = 0; p < 2; p++)
            {
                int o = p * RosterEntry.SlotCount;
                AssertEntry(roster[o + 0], InfantryArchetype.Rifle, 25, 100, 3.0f, 0, false, $"p{p} slot 0");
                AssertEntry(roster[o + 1], InfantryArchetype.Assault, 40, 90, 4.2f, 0, false, $"p{p} slot 1");
                AssertEntry(roster[o + 2], InfantryArchetype.Machinegunner, 60, 110, 2.2f, 0, false, $"p{p} slot 2");
            }
            AssertEntry(roster[3], InfantryArchetype.Officer, 120, 100, 3.0f, 400, false, "iron slot 3");
            AssertEntry(roster[4], InfantryArchetype.Shield, 70, 140, 3.2f, 0, false, "iron slot 4");
            AssertEntry(roster[5], InfantryArchetype.Repair, 90, 100, 2.8f, 300, false, "iron slot 5");
            AssertEntry(roster[6], VehicleArchetype.Maw, 350, 3600, 1.6f, 600, true, "iron tank");
            AssertEntry(roster[7], VehicleArchetype.Pincer, 320, 2300, 2.9f, 520, true, "iron walker 1");
            AssertEntry(roster[8], VehicleArchetype.Banner, 400, 1900, 2.4f, 700, true, "iron walker 2");
            AssertEntry(roster[9], VehicleArchetype.Breaker, 380, 2800, 2.2f, 650, true, "iron breaker");
            int b = RosterEntry.SlotCount;
            AssertEntry(roster[b + 3], InfantryArchetype.Sniper, 90, 80, 3.0f, 200, false, "brass slot 3");
            AssertEntry(roster[b + 4], InfantryArchetype.Medic, 80, 90, 3.2f, 200, false, "brass slot 4");
            AssertEntry(roster[b + 5], InfantryArchetype.Jetpack, 110, 85, 3.6f, 300, false, "brass slot 5");
            AssertEntry(roster[b + 6], VehicleArchetype.Tusk, 260, 2000, 2.4f, 450, true, "brass tank");
            AssertEntry(roster[b + 7], VehicleArchetype.Kettle, 270, 1600, 2.3f, 560, true, "brass walker 1");
            AssertEntry(roster[b + 8], VehicleArchetype.Censer, 240, 1500, 2.6f, 500, true, "brass walker 2");
            AssertEntry(roster[b + 9], VehicleArchetype.Redoubt, 330, 3200, 1.7f, 600, true, "brass walker 3");
        }

        /// <summary>
        /// Both sides keep the rifle, the assault man and the MG and nothing else: a shield bearer's 8 mm plate is holed
        /// by rifle fire (6 mm of penetration) and by an MG's (9), so a faction fielding neither could not answer a
        /// shield line at all. Above slot 2 the two tables share nothing, or the factions are a paint job.
        /// </summary>
        [Test]
        public void TheFactionsShareTheirFirstThreeSlotsAndNothingElse()
        {
            using var iron = new NativeArray<RosterEntry>(RosterEntry.SlotCount, Allocator.Temp);
            using var brass = new NativeArray<RosterEntry>(RosterEntry.SlotCount, Allocator.Temp);
            FactionRoster.Fill(iron, 0, FactionId.Iron);
            FactionRoster.Fill(brass, 0, FactionId.Brass);
            for (int s = 0; s < FactionRoster.SharedSlots; s++)
                Assert.AreEqual(iron[s].Archetype, brass[s].Archetype, $"slot {s} is the same on both sides");
            for (int s = FactionRoster.SharedSlots; s < RosterEntry.SlotCount; s++)
                for (int k = FactionRoster.SharedSlots; k < RosterEntry.SlotCount; k++)
                    Assert.AreNotEqual(iron[s].Archetype, brass[k].Archetype,
                        $"Iron slot {s} and Brass slot {k} field the same unit");
            int ironArmour = 0, brassArmour = 0;
            for (int s = 0; s < RosterEntry.SlotCount; s++)
            {
                if (iron[s].IsVehicle) ironArmour++;
                if (brass[s].IsVehicle) brassArmour++;
                Assert.Greater(iron[s].Hp, 0f, $"iron slot {s} is empty");
                Assert.Greater(brass[s].Hp, 0f, $"brass slot {s} is empty");
            }
            Assert.AreEqual(4, ironArmour, "the bar's ARMOUR group");
            Assert.AreEqual(4, brassArmour);
        }

        /// <summary>
        /// Six factions now: the two fictional ones and the four historical armies that stand beside them. Every one
        /// of them must field ten real units out of its own pool, or a player picks that faction and gets a bar with a
        /// hole in it. `Factions.Of` used to be `id == 0 ? Iron : Brass`, which turned every unknown byte off the wire
        /// into a legal-looking faction.
        /// </summary>
        [Test]
        public void EverySixFactionsFieldsTenUnitsOutOfItsOwnPool()
        {
            Assert.AreEqual(6, Factions.Count);
            using var roster = new NativeArray<RosterEntry>(RosterEntry.SlotCount, Allocator.Temp);
            for (int f = 0; f < Factions.Count; f++)
            {
                var faction = Factions.Of((byte)f);
                Assert.AreEqual(f, (int)faction, "a faction byte decodes to its own faction");
                Assert.IsNotEmpty(Factions.Name(faction));
                FactionRoster.Fill(roster, 0, faction);
                int armour = 0;
                for (int s = 0; s < RosterEntry.SlotCount; s++)
                {
                    var e = roster[s];
                    Assert.Greater(e.Hp, 0f, $"{faction} slot {s} is empty");
                    Assert.Greater(e.Cost, 0, $"{faction} slot {s} costs nothing");
                    Assert.IsTrue(FactionRoster.Fields(faction, e.Archetype),
                        $"{faction} fields archetype {e.Archetype} in slot {s} but it is not in the faction pool");
                    if (e.IsVehicle) armour++;
                }
                Assert.GreaterOrEqual(armour, 3, $"{faction} fields too few machines");
                Assert.LessOrEqual(armour, 4, $"{faction} fields more machines than the ARMOUR group holds");
            }
            Assert.AreEqual(FactionId.Iron, Factions.Of(200), "a byte past the end of the enum is Iron, not the last faction");
        }

        /// <summary>The crab machines and the jetpack are the fiction. A historical army that fielded them would read
        /// as Iron in a different hat, which is the thing the owner decided against when they were kept side by side.</summary>
        [Test]
        public void TheHistoricalArmiesFieldNoWalkersAndNoJetpack()
        {
            using var roster = new NativeArray<RosterEntry>(RosterEntry.SlotCount, Allocator.Temp);
            foreach (var faction in new[] { FactionId.British, FactionId.German, FactionId.French, FactionId.AustroHungarian })
            {
                FactionRoster.Fill(roster, 0, faction);
                for (int s = 0; s < RosterEntry.SlotCount; s++)
                {
                    Assert.AreNotEqual(ChassisKind.Legged, roster[s].Chassis, $"{faction} slot {s} is a walker");
                    Assert.AreNotEqual(InfantryArchetype.Jetpack, roster[s].Archetype, $"{faction} slot {s} is a jetpack man");
                }
                Assert.IsFalse(FactionRoster.MayCall(faction, FactionRoster.ParaDropBit), $"{faction} cannot drop paratroopers");
                Assert.IsTrue(FactionRoster.MayCall(faction, FactionRoster.HeBarrageBit), $"{faction} can call a barrage");
            }
        }

        /// <summary>Paratroopers are nobody's roster slot: they come down on Brass's off-map card, and Iron cannot
        /// call it. The Pavise is Iron's to swap in from the pool, not to field by default.</summary>
        [Test]
        public void ParatroopersAreBrassesCardAndNotASlot()
        {
            using var roster = new NativeArray<RosterEntry>(RosterEntry.SlotCount, Allocator.Temp);
            for (int f = 0; f < Factions.Count; f++)
            {
                var faction = Factions.Of((byte)f);
                FactionRoster.Fill(roster, 0, faction);
                for (int s = 0; s < RosterEntry.SlotCount; s++)
                    Assert.AreNotEqual(InfantryArchetype.Para, roster[s].Archetype, $"{faction} slot {s}");
            }
            Assert.IsTrue(FactionRoster.MayCall(FactionId.Brass, FactionRoster.ParaDropBit));
            Assert.IsFalse(FactionRoster.MayCall(FactionId.Iron, FactionRoster.ParaDropBit));
            Assert.IsTrue(FactionRoster.Fields(FactionId.Iron, VehicleArchetype.Pavise), "Iron's pool keeps the Pavise");
        }

        [Test]
        public void FillDefaultForwardsToTheFactionTables()
        {
            using var a = new NativeArray<RosterEntry>(RosterEntry.SlotCount * 2, Allocator.Temp);
            using var b = new NativeArray<RosterEntry>(RosterEntry.SlotCount * 2, Allocator.Temp);
            RosterEntry.FillDefault(a, 0); RosterEntry.FillDefault(a, RosterEntry.SlotCount);
            FactionRoster.Fill(b, 0, FactionId.Iron); FactionRoster.Fill(b, RosterEntry.SlotCount, FactionId.Brass);
            for (int s = 0; s < a.Length; s++)
            {
                Assert.AreEqual(b[s].Archetype, a[s].Archetype, $"slot {s}");
                Assert.AreEqual(b[s].Cost, a[s].Cost, $"slot {s}");
                Assert.AreEqual(b[s].Hp, a[s].Hp, $"slot {s}");
            }
        }

        [Test]
        public void TheDefaultConfigFieldsIronAgainstBrass()
        {
            var cfg = SimConfig.Default;
            Assert.AreEqual(FactionId.Iron, cfg.FactionOf(0));
            Assert.AreEqual(FactionId.Brass, cfg.FactionOf(1));
            Assert.AreEqual(0f, cfg.HeroPity0); Assert.AreEqual(0f, cfg.HeroPity1);
        }

        [Test]
        public void EveryArchetypeHasAnEntryAWeaponAndASpecAndStaysUnderTheOrderMask()
        {
            byte[] all = { InfantryArchetype.Rifle, InfantryArchetype.Assault, InfantryArchetype.Machinegunner, InfantryArchetype.Sniper,
                           InfantryArchetype.Officer, InfantryArchetype.Shield, InfantryArchetype.Medic, InfantryArchetype.Repair,
                           InfantryArchetype.Para, InfantryArchetype.Jetpack,
                           VehicleArchetype.Maw, VehicleArchetype.Tusk, VehicleArchetype.Pincer, VehicleArchetype.Kettle,
                           VehicleArchetype.Censer, VehicleArchetype.Pavise, VehicleArchetype.Banner, VehicleArchetype.Redoubt, VehicleArchetype.Breaker };
            foreach (var a in all)
            {
                Assert.LessOrEqual(a, InfantryArchetype.Max, $"archetype {a} is past the end of every table an archetype indexes");
                if (!ChassisKind.IsArmoured(RosterEntry.ForArchetype(a).Chassis))
                    Assert.AreNotEqual(0, OrderGroup.Of(a) & OrderGroup.All, $"archetype {a} belongs to no order group, so no advance order would reach him");
                var e = RosterEntry.ForArchetype(a);
                Assert.AreEqual(a, e.Archetype, $"ForArchetype({a})");
                Assert.Greater(e.Hp, 0f, $"archetype {a} has hit points");
                Assert.AreEqual(ChassisKind.IsArmoured(e.Chassis), e.IsVehicle, $"archetype {a} vehicle flag agrees with what it stands on");
                var w = CombatTables.WeaponFor(a);
                var spec = InfantrySpec.For(a);
                Assert.IsTrue(w.RangeMax > 0f || spec.HealPerSecond > 0f, $"archetype {a} either shoots or heals");
                Assert.IsTrue(FactionRoster.Fields(FactionId.Iron, a) || FactionRoster.Fields(FactionId.Brass, a), $"some faction fields archetype {a}");
            }
            Assert.AreEqual(ChassisKind.Tracked, RosterEntry.Breaker.Chassis, "the Breaker runs on tracks");
            // the explosion source space is banded now, so a machine's weapon id cannot collide with the fleet's or
            // with a hull cooking off however high the archetype ids go
            Assert.AreNotEqual(SourceId.Unit(VehicleArchetype.Breaker), SeaLandingSystem.ShipSource);
            Assert.Less(SourceId.Unit(255), SourceId.CookOff, "every archetype fits below the cook-off band");
            Assert.Greater(SourceId.Unit(0), SourceId.AbilityMax, "a unit's weapon can never read as an ability");
            Assert.AreEqual(VehicleArchetype.Breaker, SourceId.ArchetypeOf(SourceId.Unit(VehicleArchetype.Breaker)));
            Assert.IsTrue(SourceId.IsUnit(LeapSystem.LandingSource));
            Assert.AreEqual(0, RosterEntry.SlotCount % 1);
        }

        /// <summary>A campaign nation that has a roster of its own fields it; the rest fight as the nearest ally.</summary>
        [Test]
        public void EachNationWithARosterOfItsOwnFieldsIt()
        {
            Assert.AreEqual(FactionId.British, FactionMap.ToSim(Faction.British));
            Assert.AreEqual(FactionId.German, FactionMap.ToSim(Faction.German));
            Assert.AreEqual(FactionId.French, FactionMap.ToSim(Faction.French));
            Assert.AreEqual(FactionId.AustroHungarian, FactionMap.ToSim(Faction.AustroHungarian));
            Assert.AreEqual(FactionId.German, FactionMap.ToSim(Faction.Ottoman), "the other Central Powers fight as Germany");
            Assert.AreEqual(FactionId.British, FactionMap.ToSim(Faction.Italian), "and the rest of the Entente as Britain");
        }

        [Test]
        public void OnlyBrassMayDropParatroopers()
        {
            Assert.IsTrue(FactionRoster.MayCall(FactionId.Brass, (int)OffMapAbilityId.ParaDrop));
            Assert.IsFalse(FactionRoster.MayCall(FactionId.Iron, (int)OffMapAbilityId.ParaDrop));
            Assert.IsTrue(FactionRoster.MayCall(FactionId.Iron, (int)OffMapAbilityId.HeBarrage));
            Assert.IsTrue(FactionRoster.MayCall(FactionId.Brass, (int)OffMapAbilityId.ChlorineGas));
            Assert.AreEqual((int)OffMapAbilityId.ParaDrop, FactionRoster.ParaDropBit);
            Assert.Less((int)OffMapAbilityId.ParaDrop, OffMapAbilitySystem.AbilitySlots);
        }

        [Test]
        public void TheBreakerHasItsOwnSpecAndProfile()
        {
            var spec = TankSpec.For(VehicleArchetype.Breaker);
            Assert.Greater(spec.BreakerRange, 0f);
            Assert.AreEqual(0, spec.GunCount);
            Assert.Greater(spec.ClawReach, 0f);
            Assert.Greater(spec.ChargingTopMm, CombatTables.CloseAssaultPenMm, "a bundle on the deck bounces while it charges");
            Assert.AreEqual(0f, TankSpec.For(VehicleArchetype.Maw).BreakerRange, "nothing else is a breaker");
            var prof = VehicleProfile.ForArchetype(VehicleArchetype.Breaker);
            Assert.AreEqual(0f, prof.DitchChance);
            Assert.IsFalse(prof.Walker);
            Assert.Less(prof.HalfLength, VehicleProfile.Maw.HalfLength);
        }

        [Test]
        public void TheReplayHeaderCarriesTheFactionsAndThePity()
        {
            var cfg = SimConfig.Default;
            cfg.FactionA = (byte)FactionId.Brass; cfg.FactionB = (byte)FactionId.Iron; cfg.HeroPity0 = 0.42f; cfg.HeroPity1 = 0.07f;
            var rec = new ReplayRecorder(cfg, default, 1);
            var back = ReplayPlayer.Parse(rec.Serialize());
            Assert.AreEqual(5, ReplayRecorder.FormatVersion);
            Assert.AreEqual(FactionId.Brass, back.Config.FactionOf(0));
            Assert.AreEqual(FactionId.Iron, back.Config.FactionOf(1));
            Assert.AreEqual(0.42f, back.Config.HeroPity0);
            Assert.AreEqual(0.07f, back.Config.HeroPity1);
            Assert.AreEqual(cfg.EventCapacity, back.Config.EventCapacity);
        }

        [Test]
        public void ADeployCarriesItsVeteranRank()
        {
            var c = SimCommand.Deploy(7, 1, 3, 2);
            Assert.AreEqual(CommandType.DeployUnit, c.Type);
            Assert.AreEqual(3, c.A); Assert.AreEqual(2, c.B);
            Assert.AreEqual(0, SimCommand.Deploy(7, 1, 3).B, "nobody by default");
        }
    }
}
