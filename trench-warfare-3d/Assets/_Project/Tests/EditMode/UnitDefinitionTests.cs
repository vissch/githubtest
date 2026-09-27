// Phase: A5c (implemented 2026-09-25) — a unit defined in one place lands in every table that decides how it behaves.
// The failure this guards against is the one the five old switches allowed: a unit half-added, with a roster line and
// no weapon, driving like a Maw because VehicleProfile.ForArchetype fell through to its default and said nothing.
using NUnit.Framework;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Nav;

namespace TW.Tests
{
    public class UnitDefinitionTests
    {
        static MatchSim NewMatch()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            return MatchSim.CreateGreybox(cfg);
        }

        /// <summary>An id inside the table that no faction fields, so writing over it cannot disturb a real unit.</summary>
        const byte Spare = 20;

        static UnitDef Fake() => new UnitDef
        {
            Archetype = Spare,
            Roster = new RosterEntry { Archetype = Spare, Cost = 137, Hp = 111f, Speed = 2.5f, CooldownTicks = 321, IsVehicle = true },
            Infantry = new InfantrySpec { HealRadius = 3f, HealPerSecond = 7f, Group = OrderGroup.Support },
            Weapon = new WeaponStats { Id = Spare, Damage = 13f, RangeMax = 77f, RoundsPerSecond = 2f, Accuracy = 0.5f, PenetrationMm = 11f },
            Machine = new TankSpec { Hull = new ArmorProfile { FrontMm = 42f, SideMm = 21f, RearMm = 12f, TopMm = 9f }, Crew = 3, ClawReach = 1.5f, ClawDamage = 99f },
            Drive = new VehicleProfile { TurnRateRad = 0.9f, TrenchCrossWidth = 2.5f, HalfLength = 2f, HalfWidth = 1.5f },
        };

        [Test]
        public void ADefinitionWritesIntoEveryTableThatDecidesHowAUnitBehaves()
        {
            using var m = NewMatch();
            ulong coreBefore = m.World.Units.Fingerprint, combatBefore = m.Catalogue.Fingerprint;

            UnitDefinitions.Apply(m.World, new[] { Fake() });

            Assert.AreEqual(137, m.World.Units.Roster[Spare].Cost, "the roster line");
            Assert.AreEqual(111f, m.World.Units.Roster[Spare].Hp);
            Assert.AreEqual(7f, m.World.Units.Infantry[Spare].HealPerSecond, "the spec");
            Assert.AreEqual(OrderGroup.Support, m.World.Units.Infantry[Spare].Group, "the order group travels with the spec");
            Assert.AreEqual(77f, m.Catalogue.Weapon[Spare].RangeMax, "the weapon");
            Assert.AreEqual(42f, m.Catalogue.Tank[Spare].Hull.FrontMm, "the hull");
            Assert.AreEqual(0.9f, m.Vehicles.Profiles[Spare].TurnRateRad, "the drive profile");

            Assert.AreNotEqual(coreBefore, m.World.Units.Fingerprint, "a changed table is a changed fingerprint");
            Assert.AreNotEqual(combatBefore, m.Catalogue.Fingerprint);
        }

        /// <summary>Applying is a write of fixed values, not an accumulation: MatchSim does it once, and a bake may redo it.</summary>
        [Test]
        public void ApplyingTheSameDefinitionsTwiceChangesNothingTheSecondTime()
        {
            using var m = NewMatch();
            var defs = new[] { Fake() };
            UnitDefinitions.Apply(m.World, defs);
            ulong core = m.World.Units.Fingerprint, combat = m.Catalogue.Fingerprint;
            UnitDefinitions.Apply(m.World, defs);
            Assert.AreEqual(core, m.World.Units.Fingerprint);
            Assert.AreEqual(combat, m.Catalogue.Fingerprint);
        }

        /// <summary>
        /// The shipped units are defined by the old per-type tables, and MatchSim applying the (currently empty) new
        /// definitions must not disturb them. This is the test that lets the file exist before it has content.
        /// </summary>
        [Test]
        public void TheUnitsThatAlreadyShippedAreUntouchedByTheNewFile()
        {
            using var m = NewMatch();
            for (int a = 0; a < Archetypes.Count; a++)
            {
                var entry = RosterEntry.ForArchetype((byte)a);
                Assert.AreEqual(entry.Cost, m.World.Units.Roster[a].Cost, $"archetype {a} cost");
                // Both combat switches end in a silent default - WeaponFor in the rifleman's rifle, TankSpec.For in the
                // MAW - so comparing every id against them asserted that an id nobody occupies is armed and that a
                // RIFLEMAN HAS SIX CREW. The catalogue gives a weapon only to a unit and a hull only to a machine.
                float wantDamage = entry.Hp > 0f ? CombatTables.WeaponFor((byte)a).Damage : 0f;
                int wantCrew = ChassisKind.IsArmoured(entry.Chassis) ? TankSpec.For((byte)a).Crew : 0;
                Assert.AreEqual(wantDamage, m.Catalogue.Weapon[a].Damage, $"archetype {a} damage");
                Assert.AreEqual(wantCrew, m.Catalogue.Tank[a].Crew, $"archetype {a} crew");
                Assert.AreEqual(VehicleProfile.ForArchetype((byte)a).TurnRateRad, m.Vehicles.Profiles[a].TurnRateRad, $"archetype {a} turn rate");
                Assert.AreEqual(OrderGroup.Of((byte)a), m.World.Units.Infantry[a].Group, $"archetype {a} order group");
            }
        }

        /// <summary>A world that fields a unit nobody defined would drive like a Maw and shoot like a rifleman without
        /// saying so; the ids either faction fields must all have a weapon or a reason not to.</summary>
        [Test]
        public void EveryFieldedUnitHasAWeaponOrAJobThatNeedsNone()
        {
            using var m = NewMatch();
            for (int f = 0; f < Factions.Count; f++)
                for (int s = 0; s < RosterEntry.SlotCount; s++)
                {
                    byte a = FactionRoster.Slot(Factions.Of((byte)f), s).Archetype;
                    var w = m.Catalogue.Weapon[a];
                    var spec = m.World.Units.Infantry[a];
                    bool unarmedOnPurpose = spec.HealPerSecond > 0f || spec.RepairPerSecond > 0f || m.Catalogue.Tank[a].GasStrength > 0f
                                            || m.Catalogue.Tank[a].ClawDamage > 0f;
                    Assert.IsTrue(w.RangeMax > 0f || unarmedOnPurpose, $"archetype {a} has neither a weapon nor a job that needs none");
                }
        }
    }
}
