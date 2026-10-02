// Phase: A5c (implemented 2026-09-25) — the launch path's half of the chosen ten.
//
// The sim's half is LoadoutTests. This pins the one piece of judgement on the way in: what happens to a loadout the
// player could not really have chosen. A bad id is DROPPED, never clamped, because clamping would field a different
// unit from the one named and look deliberate.
using NUnit.Framework;
using TW.Presentation;
using TW.Sim;

namespace TW.Tests
{
    public class LaunchLoadoutTests
    {
        [Test]
        public void NoChoiceIsNoChoice()
        {
            Assert.AreEqual(0, SimHost.Loadout(null).Length, "null is the faction's default ten");
            Assert.AreEqual(0, SimHost.Loadout(new byte[0]).Length);
        }

        [Test]
        public void AChosenTenArrivesInOrder()
        {
            var l = SimHost.Loadout(new byte[] { InfantryArchetype.Sniper, VehicleArchetype.Maw, InfantryArchetype.Medic });
            Assert.AreEqual(3, l.Length);
            Assert.AreEqual(InfantryArchetype.Sniper, l[0]);
            Assert.AreEqual(VehicleArchetype.Maw, l[1]);
            Assert.AreEqual(InfantryArchetype.Medic, l[2]);
        }

        /// <summary>An id past the end of every table an archetype indexes cannot be fielded, so it is left out.</summary>
        [Test]
        public void AnIdPastTheTablesIsDroppedAndNotClamped()
        {
            var l = SimHost.Loadout(new byte[] { InfantryArchetype.Rifle, 200, InfantryArchetype.Sniper });
            Assert.AreEqual(2, l.Length, "the impossible id is gone, not turned into the last valid one");
            Assert.AreEqual(InfantryArchetype.Rifle, l[0]);
            Assert.AreEqual(InfantryArchetype.Sniper, l[1], "and the slot after it did not shift onto the bad id");
        }

        /// <summary>Ten slots is ten slots, however long a list the briefing hands over.</summary>
        [Test]
        public void MoreThanTenIsCutToTen()
        {
            var many = new byte[RosterEntry.SlotCount + 5];
            for (int i = 0; i < many.Length; i++) many[i] = InfantryArchetype.Rifle;
            Assert.AreEqual(RosterEntry.SlotCount, SimHost.Loadout(many).Length);
        }
    }
}
