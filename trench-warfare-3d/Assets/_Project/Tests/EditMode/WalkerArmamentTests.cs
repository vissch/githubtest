// Phase: A5c (implemented 2026-09-25) — what a machine carries is what its entry says, not what the switch fell
// through to.
//
// CombatTables.WeaponFor ends in `default: the rifleman's rifle`. The six walkers had no entries, the catalogue fills
// every id from that switch, TargetAcquisition reads a shooter's range out of it for vehicles as well as men, and
// DirectFire fires whatever it finds. So each crab carried a phantom rifle - 36 damage at 130 m, half a round a second
// - on top of the guns TankGunnery gives it, and the Redoubt, whose whole description is "no gun at all, armour and
// claws", was shooting.
//
// The behavioural test is the one that matters: a Redoubt facing a man at sixty metres must not hurt him.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;

namespace TW.Tests
{
    public class WalkerArmamentTests
    {
        static MatchSim NewMatch()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            return MatchSim.CreateGreybox(cfg);
        }

        static readonly byte[] Crabs =
        {
            VehicleArchetype.Pincer, VehicleArchetype.Kettle, VehicleArchetype.Censer,
            VehicleArchetype.Pavise, VehicleArchetype.Banner, VehicleArchetype.Redoubt,
        };

        [Test]
        public void NoWalkerCarriesSmallArms()
        {
            using var m = NewMatch();
            foreach (byte a in Crabs)
                Assert.AreEqual(0f, m.Catalogue.Weapon[a].RangeMax,
                    $"archetype {a} has a small-arms range, so it acquires and fires one");
        }

        /// <summary>
        /// The bug as it was felt: a blockhouse on legs with no gun, sixty metres from an enemy rifleman, taking his
        /// fire and returning none. Sixty metres is inside the phantom rifle's 130 m and far outside the claws' reach,
        /// and a hundred ticks is five seconds - not enough walking to close it.
        /// </summary>
        [Test]
        public void ARedoubtDoesNotShootTheManItCannotShoot()
        {
            using var m = NewMatch();
            var w = m.World;
            var redoubt = RosterEntry.Redoubt;
            int machine = w.Spawn(0, VehicleArchetype.Redoubt, new float3(60f, 0f, 70f), redoubt.Hp, redoubt.Speed, true);
            var rifleman = RosterEntry.Rifleman;
            int man = w.Spawn(1, InfantryArchetype.Rifle, new float3(60f, 0f, 130f), rifleman.Hp, rifleman.Speed, false);
            Assert.GreaterOrEqual(machine, 0); Assert.GreaterOrEqual(man, 0);

            float before = w.Hp[man];
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            for (int t = 0; t < 100; t++) m.Step(none);

            Assert.AreEqual(before, w.Hp[man], 0.001f, "the Redoubt has no gun, so the man it faces takes nothing from it");
        }

        /// <summary>An id no unit occupies must not read as armed, or a unit added with a roster line and no weapon
        /// looks armed too - the exact failure the definitions exist to prevent.</summary>
        [Test]
        public void AnIdNoUnitOccupiesIsUnarmed()
        {
            using var m = NewMatch();
            for (int a = 0; a < Archetypes.Count; a++)
            {
                if (RosterEntry.ForArchetype((byte)a).Hp > 0f) continue;
                Assert.AreEqual(0f, m.Catalogue.Weapon[a].RangeMax, $"archetype {a} is nobody, and nobody is unarmed");
                Assert.AreEqual(0f, m.Catalogue.Weapon[a].Damage, $"archetype {a} is nobody");
            }
        }

        /// <summary>A man is not a tank, so he must not be carrying a tank's hull. TankSpec.For ends in
        /// `default: return Maw`, so before this every infantry id held the Maw's armour, guns, crew and fuel risk -
        /// unread today only because every reader happens to gate on being a machine.</summary>
        [Test]
        public void NothingOnFootCarriesAHull()
        {
            using var m = NewMatch();
            for (int a = 0; a < Archetypes.Count; a++)
            {
                if (ChassisKind.IsArmoured(m.World.ChassisOf((byte)a))) continue;
                Assert.AreEqual(0f, m.Catalogue.Tank[a].Hull.FrontMm, $"archetype {a} is not a machine and has no glacis");
                Assert.AreEqual(0, m.Catalogue.Tank[a].Crew, $"archetype {a} is not a machine and has no crew");
                Assert.AreEqual(0, m.Catalogue.Tank[a].GunCount, $"archetype {a} is not a machine and has no guns");
            }
        }

        /// <summary>Every machine keeps the hull it had.</summary>
        [Test]
        public void EveryMachineStillHasItsOwnHull()
        {
            using var m = NewMatch();
            foreach (byte a in new byte[] { VehicleArchetype.Maw, VehicleArchetype.Tusk, VehicleArchetype.Breaker,
                                            VehicleArchetype.Pincer, VehicleArchetype.Kettle, VehicleArchetype.Censer,
                                            VehicleArchetype.Pavise, VehicleArchetype.Banner, VehicleArchetype.Redoubt })
            {
                var want = TankSpec.For(a);
                Assert.AreEqual(want.Hull.FrontMm, m.Catalogue.Tank[a].Hull.FrontMm, $"archetype {a} glacis");
                Assert.AreEqual(want.Crew, m.Catalogue.Tank[a].Crew, $"archetype {a} crew");
                Assert.AreEqual(want.GunCount, m.Catalogue.Tank[a].GunCount, $"archetype {a} guns");
            }
        }

        /// <summary>The men and the two tanks keep exactly what they had: this must change nothing about them.</summary>
        [Test]
        public void EveryUnitThatHadAWeaponStillHasTheSameOne()
        {
            using var m = NewMatch();
            foreach (byte a in new byte[] { InfantryArchetype.Rifle, InfantryArchetype.Assault, InfantryArchetype.Machinegunner,
                                            InfantryArchetype.Sniper, InfantryArchetype.Officer, InfantryArchetype.Shield,
                                            InfantryArchetype.Repair, InfantryArchetype.Para, InfantryArchetype.Jetpack,
                                            VehicleArchetype.Maw, VehicleArchetype.Tusk, VehicleArchetype.Breaker })
            {
                var want = CombatTables.WeaponFor(a);
                Assert.AreEqual(want.Damage, m.Catalogue.Weapon[a].Damage, $"archetype {a} damage");
                Assert.AreEqual(want.RangeMax, m.Catalogue.Weapon[a].RangeMax, $"archetype {a} range");
            }
        }
    }
}
