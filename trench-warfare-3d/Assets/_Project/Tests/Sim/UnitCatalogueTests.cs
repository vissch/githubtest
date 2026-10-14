// Phase: A5c (implemented 2026-09-25) — the unit table is the same table on both machines, or the battle is not the
// same battle. These hold the two properties that make a data path safe to build on: the default table is exactly the
// compiled switches it replaced (so the commit that introduced it changed nothing), and a difference in the table
// shows up in the tick hash immediately rather than as a drift a hundred ticks later that reads as a physics bug.
using NUnit.Framework;
using Unity.Collections;
using TW.Sim;

namespace TW.Tests
{
    public class UnitCatalogueTests
    {
        static SimConfig.WorldInit Init() => new SimConfig.WorldInit
        {
            SpawnA = new Unity.Mathematics.float3(60f, 0f, 10f),
            SpawnB = new Unity.Mathematics.float3(60f, 0f, 200f),
        };

        [Test]
        public void TheDefaultTableIsTheCompiledSwitchesItReplaced()
        {
            using var c = UnitCatalogue.Default();
            Assert.AreEqual(Archetypes.Count, c.Roster.Length);
            Assert.AreEqual(Archetypes.Count, c.Infantry.Length);
            for (int a = 0; a < Archetypes.Count; a++)
            {
                var expectedRoster = RosterEntry.ForArchetype((byte)a);
                var expectedSpec = InfantrySpec.For((byte)a);
                Assert.AreEqual(expectedRoster.Archetype, c.Roster[a].Archetype, $"archetype {a} id");
                Assert.AreEqual(expectedRoster.Cost, c.Roster[a].Cost, $"archetype {a} cost");
                Assert.AreEqual(expectedRoster.Hp, c.Roster[a].Hp, $"archetype {a} hit points");
                Assert.AreEqual(expectedRoster.Speed, c.Roster[a].Speed, $"archetype {a} speed");
                Assert.AreEqual(expectedRoster.CooldownTicks, c.Roster[a].CooldownTicks, $"archetype {a} cooldown");
                Assert.AreEqual(expectedRoster.IsVehicle, c.Roster[a].IsVehicle, $"archetype {a} vehicle flag");
                Assert.AreEqual(expectedSpec.AuraRadius, c.Infantry[a].AuraRadius, $"archetype {a} aura");
                Assert.AreEqual(expectedSpec.HealPerSecond, c.Infantry[a].HealPerSecond, $"archetype {a} healing");
                Assert.AreEqual(expectedSpec.ShieldPlateMm, c.Infantry[a].ShieldPlateMm, $"archetype {a} plate");
                Assert.AreEqual(expectedSpec.JumpRange, c.Infantry[a].JumpRange, $"archetype {a} leap");
                Assert.AreEqual(expectedSpec.Braced, c.Infantry[a].Braced, $"archetype {a} braced");
            }

            // [T32] The loop above calls the very functions Default() fills the table with (UnitCatalogue.cs:45-48),
            // so it guards the lengths and the wiring but cannot fail on a number. These are hand-written pins, read
            // today from RosterEntry.cs:58 (Rifleman), :85-86 (Maw, Tusk) and InfantrySpec.cs (the officer's aura):
            // a tuning pass that moves them has to come past this test.
            Assert.AreEqual(25, c.Roster[InfantryArchetype.Rifle].Cost, "[T32] the rifleman costs 25 (RosterEntry.cs:58)");
            Assert.AreEqual(100f, c.Roster[InfantryArchetype.Rifle].Hp, 1e-4f, "[T32] the rifleman has 100 hit points (RosterEntry.cs:58)");
            Assert.AreEqual(350, c.Roster[VehicleArchetype.Maw].Cost, "[T32] the Maw costs 350 (RosterEntry.cs:85)");
            Assert.AreEqual(3600f, c.Roster[VehicleArchetype.Maw].Hp, 1e-4f, "[T32] the Maw has 3600 structure (RosterEntry.cs:85)");
            Assert.AreEqual(1.6f, c.Roster[VehicleArchetype.Maw].Speed, 1e-4f, "[T32] the Maw walks at 1.6 m/s (RosterEntry.cs:85)");
            Assert.AreEqual(600, c.Roster[VehicleArchetype.Maw].CooldownTicks, "[T32] the Maw reloads in 600 ticks (RosterEntry.cs:85)");
            Assert.IsTrue(c.Roster[VehicleArchetype.Maw].IsVehicle, "[T32] the Maw is a machine (RosterEntry.cs:85)");
            Assert.AreEqual(260, c.Roster[VehicleArchetype.Tusk].Cost, "[T32] the Tusk costs 260 (RosterEntry.cs:86)");
            Assert.AreEqual(2000f, c.Roster[VehicleArchetype.Tusk].Hp, 1e-4f, "[T32] the Tusk has 2000 structure (RosterEntry.cs:86)");
            Assert.AreEqual(15f, c.Infantry[InfantryArchetype.Officer].AuraRadius, 1e-4f,
                "[T32] the officer's aura reaches 15 m (InfantrySpec.cs, Base(Officer))");
        }

        [Test]
        public void EveryUnitEitherFactionFieldsHasAnEntryInTheTable()
        {
            using var c = UnitCatalogue.Default();
            using var roster = new NativeArray<RosterEntry>(RosterEntry.SlotCount, Allocator.Temp);
            for (int f = 0; f < Factions.Count; f++)
            {
                FactionRoster.Fill(roster, 0, Factions.Of((byte)f));
                for (int s = 0; s < RosterEntry.SlotCount; s++)
                {
                    byte a = roster[s].Archetype;
                    Assert.Less(a, Archetypes.Count, $"archetype {a} is past the end of the table");
                    Assert.Greater(c.Roster[a].Hp, 0f, $"archetype {a} is fielded but its table entry is empty");
                }
            }
        }

        [Test]
        public void TwoWorldsBuiltTheSameWayAgreeOnTheirUnits()
        {
            var cfg = SimConfig.Default;
            using var a = new SimWorld(cfg, Init());
            using var b = new SimWorld(cfg, Init());
            Assert.AreEqual(a.Units.Fingerprint, b.Units.Fingerprint);
            Assert.AreEqual(a.Hash(), b.Hash());
        }

        /// <summary>
        /// The point of the fingerprint: a machine whose medic heals faster is playing a different game, and the tick
        /// hash says so at tick 0 instead of letting the two worlds drift apart while the men are still walking.
        /// </summary>
        [Test]
        public void AWorldGivenADifferentTableDivergesAtOnce()
        {
            var cfg = SimConfig.Default;
            var tuned = UnitCatalogue.Default();
            var spec = tuned.Infantry[InfantryArchetype.Medic];
            spec.HealPerSecond += 1f;
            tuned.Infantry[InfantryArchetype.Medic] = spec;
            tuned.Seal();

            using var stock = new SimWorld(cfg, Init());
            using var modded = new SimWorld(cfg, Init(), tuned);   // the world disposes the table it was given
            Assert.AreNotEqual(stock.Units.Fingerprint, modded.Units.Fingerprint, "a changed table is a changed fingerprint");
            Assert.AreNotEqual(stock.Hash(), modded.Hash(), "and a changed fingerprint is a changed tick hash");
        }

        [Test]
        public void SealingTheSameNumbersTwiceGivesTheSameFingerprint()
        {
            using var a = UnitCatalogue.Default();
            using var b = UnitCatalogue.Default();
            ulong before = a.Fingerprint;
            a.Seal();
            Assert.AreEqual(before, a.Fingerprint, "sealing is not a counter: the same numbers hash the same");
            Assert.AreEqual(a.Fingerprint, b.Fingerprint);
        }
    }
}
