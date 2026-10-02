// Phase: B5 (implemented) — the calibration of wearing the field away with small-arms fire (PropWear): how long one
// machine gun needs on a thing before it goes. These are the owner's feel numbers ("4-6 seconds for a sandbag stack
// or a timber wall section"), locked against the sim's real rates of fire, so that tuning either end shows up here.
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Sim.Combat;
using TW.Presentation.Tactical;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public sealed class PropWearTests
    {
        /// <summary>Seconds of one weapon held on a thing of this strength and material.</summary>
        static float Seconds(float hp, float erode, byte archetype = 2) =>
            PropDestruction.RoundsToWear(hp, erode) / CombatTables.WeaponFor(archetype).RoundsPerSecond;

        [Test]
        public void One_Machine_Gun_Chews_Through_Sacking_And_Timber_In_The_Seconds_The_Owner_Asked_For()
        {
            // the rules these strengths come from: kit.sandbag / kit.sandbags Hp 0.9, kit.TrenchWalls Hp 1.2, both Erode 1
            float sandbags = Seconds(0.9f, 1f), revetment = Seconds(1.2f, 1f);
            Assert.That(sandbags, Is.InRange(4f, 6f), "a sandbag stack goes in 4-6 s of one machine gun (was " + sandbags + " s)");
            Assert.That(revetment, Is.InRange(4f, 6f), "a timber wall section the same (was " + revetment + " s)");
        }

        [Test]
        public void A_Rifleman_Wears_The_Same_Thing_Away_Far_More_Slowly_Than_A_Machine_Gun()
        {
            // nothing in the wear itself knows what fired: a machine gun simply puts fourteen times the rounds into the
            // same ground (7 a second against a rifleman's 0.5), and that alone is what makes it the thing that strips a parapet
            float rifle = Seconds(0.9f, 1f, 0), mg = Seconds(0.9f, 1f, 2);
            Assert.Greater(rifle, mg * 10f, "one rifleman is no substitute for a machine gun");
            Assert.That(rifle, Is.GreaterThanOrEqualTo(60f), "one man with a rifle needs a solid minute on the same sacks, which nobody gets");
        }

        [Test]
        public void Everything_Wears_But_Stone_And_Concrete_Take_A_Committed_Gunner()
        {
            // the owner chose "everything, at different rates": every material erodes, none is exempt
            float houseBrick = Seconds(1.4f, .2f);      // a village wall chunk
            float concrete = Seconds(2.2f, .08f);       // a rear blockhouse chunk
            float boulder = Seconds(3.2f, .1f);
            float sheetIron = Seconds(1.0f, .35f);      // corrugated sheet
            foreach (var (what, seconds) in new[] { ("brick", houseBrick), ("concrete", concrete), ("a boulder", boulder), ("sheet iron", sheetIron) })
                Assert.That(seconds, Is.LessThan(600f), what + " does erode in the end, it is not exempt");
            Assert.Greater(concrete, 100f, "a blockhouse is not opened by a passing burst");
            Assert.Greater(concrete, Seconds(1.2f, 1f) * 15f, "concrete is a different order of thing from timber");
            Assert.Less(sheetIron, concrete, "a corrugated sheet gives long before poured concrete does");
        }

        [Test]
        public void What_A_Man_Drops_Is_Shot_Away_Almost_At_Once()
        {
            Assert.That(Seconds(0.2f, 1f), Is.LessThan(1.5f), "a helmet lying in the open does not survive being shot at");
            Assert.That(Seconds(0.3f, 1f), Is.LessThan(2f), "nor does a tuft of grass");
        }

        [Test]
        public void A_Shelter_Is_Stripped_Of_Its_Sandbags_First_And_Only_Then_Begins_To_Open()
        {
            float strip = PropDestruction.RoundsToStrip() / CombatTables.WeaponFor(2).RoundsPerSecond;
            float open = PropDestruction.RoundsToOpenShelter() / CombatTables.WeaponFor(2).RoundsPerSecond;
            Assert.That(strip, Is.InRange(30f, 90f), "a machine gun strips a dugout's parapet in about a minute (was " + strip + " s)");
            Assert.Greater(open, strip * 2f, "and the shelter itself takes far longer again, after the sacks are gone");
            Assert.That(open, Is.InRange(120f, 420f), "but it does open in the end, to fire nothing else in the game would do it with");
            // a single pass of fire must never blow a sack off by itself: that is what the gathered pool is for
            Assert.Greater(PropDestruction.WearPerBag, PropDestruction.WearPerRound * CombatTables.WeaponFor(2).RoundsPerSecond,
                "a second of machine-gun fire is less than one sack, so sacks come off one at a time and not in a rush");
        }

        [Test]
        public void Stray_Fire_Never_Goes_Looking_For_What_Is_Standing_Somewhere()
        {
            // finding what stands in a square is the expensive thing in the file (a spatial query a rule). A square earns
            // one only under fire that is actually sustained: a machine gun gets there in about a second, a lone rifleman
            // takes twenty, by which time he has almost certainly stopped or moved on.
            float mg = PropDestruction.MinRoundsForSweep / CombatTables.WeaponFor(2).RoundsPerSecond;
            float rifle = PropDestruction.MinRoundsForSweep / CombatTables.WeaponFor(0).RoundsPerSecond;
            Assert.That(mg, Is.LessThan(2f), "a machine gun opening up is sustained fire within a second or so");
            Assert.That(rifle, Is.GreaterThan(10f), "one man loosing off rounds is not, and never costs a sweep");
        }

        [Test]
        public void The_Pass_Is_Tied_To_The_Sim_Tick_And_Its_Debris_Is_Rationed()
        {
            float passesPerSecond = SimConfig.Default.TickRate / (float)PropDestruction.WearTicks;
            Assert.That(passesPerSecond, Is.InRange(3f, 10f), "wear is spent a few times a second, off the tick so it is the same on any machine");
            // DebrisRenderer's pools are ring buffers: too many pieces would not overflow, they would push a shell's own
            // debris out and leave bursts looking thin. The smallest pool is what bounds this.
            float piecesPerSecond = PropDestruction.SpallBudget * passesPerSecond;
            // only the kinds Spall can actually throw: it turns Plank and Plate into Shard and otherwise keeps the rule's
            // own piece, so wear only ever lands in these three. (The 64-deep pool is the tree crowns', which it never touches.)
            int smallest = int.MaxValue;
            foreach (var kind in new[] { DebrisRenderer.Piece.Shard, DebrisRenderer.Piece.Rubble, DebrisRenderer.Piece.Sandbag })
                smallest = Mathf.Min(smallest, DebrisRenderer.CapacityOf(kind));
            Assert.That(piecesPerSecond, Is.LessThan(smallest * 0.25f),
                "a second of wear takes a small share of the pools it uses, so a shell's own debris is never pushed out");
        }

        [Test]
        public void A_Knock_Is_Small_Enough_To_Read_As_A_Flinch_Rather_Than_A_Throw()
        {
            // a bullet nudges a helmet; it does not launch it out of the battle (that is Toss, for a shell). Identity is
            // safe whatever the size, because everything keys off PropWear.Home rather than off what is drawn — this is
            // only about how it looks.
            Assert.That(PropDestruction.JoltReach, Is.LessThan(PropDestruction.Quantum),
                "a knock moves a thing less than the grain the field is keyed at: it twitches where it lies");
            Assert.That(PropDestruction.JoltSeconds, Is.LessThan(0.5f), "and it is back down almost at once");
            Assert.That(PropDestruction.MaxJolts, Is.LessThanOrEqualTo(64), "with a bound on how many are moving at once");
        }
    }
}
