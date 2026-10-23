// Phase: VFX pass (goal fire-at-rifle-look) - the anti-tank rifle used to fall to the rifleman's default in
// CombatFx.KindOf, so its rare, heavy shot drew a rifleman's flare and tracer. These pin its own kind, its own muzzle
// book, and that its flash is big enough on screen: the goal asks for 2.5x a rifleman's AREA at the standard view,
// which is sqrt(2.5) = 1.5811 in width.
using NUnit.Framework;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Sim;

namespace TW.Tests
{
    public class AtRifleLookTests
    {
        const float AreaToWidth = 1.5811f;   // sqrt(2.5): the goal's 2.5x on-screen area as a width

        [Test]
        public void AtRifle_HasItsOwnKind()
        {
            Assert.AreEqual(CombatFx.ArmsKind.AtRifle, CombatFx.KindOf(InfantryArchetype.AtRifle), "the anti-tank rifle no longer fires as a rifleman");
        }

        [Test]
        public void AtRifle_DrawsTheLongGunsBlast()
        {
            Assert.AreEqual(FlipbookFx.Book.GunLong, CombatFx.MuzzleBookOf(CombatFx.ArmsKind.AtRifle));
        }

        [Test]
        public void AtRifle_RoundIsHeavierAndLongerThanARifles()
        {
            var at = CombatFx.ArmsFor(InfantryArchetype.AtRifle, 0u, 0);
            var rifle = CombatFx.ArmsFor(InfantryArchetype.Rifle, 0u, 0);
            Assert.Greater(at.Tracer, rifle.Tracer, "a heavier round");
            Assert.Greater(at.TracerLife, rifle.TracerLife, "it carries longer");
            Assert.Greater(at.Streak, rifle.Streak, "a longer streak");
        }

        [Test]
        public void AtRifle_FlashIsAtLeastTwoAndAHalfTimesARiflemansArea()
        {
            float at = CombatFx.ArmsFor(InfantryArchetype.AtRifle, 0u, 0).Flare * CombatFx.MuzzleScale(FlipbookFx.Book.GunLong, 0f);
            float rifle = CombatFx.ArmsFor(InfantryArchetype.Rifle, 0u, 0).Flare * CombatFx.MuzzleScale(FlipbookFx.Book.MuzzleRifle, 0f);
            Assert.GreaterOrEqual(at, AreaToWidth * rifle, "the anti-tank flash at the standard view");
        }

        [TestCase(InfantryArchetype.AtRifle, true, false, true, TestName = "AtRifle_Hull_NoStrike_Spurts")]
        [TestCase(InfantryArchetype.AtRifle, true, true, false, TestName = "AtRifle_Hull_Struck_NoSpurt")]
        [TestCase(InfantryArchetype.Rifle, true, false, false, TestName = "Rifle_Hull_NoSpurt")]
        [TestCase(InfantryArchetype.AtRifle, false, false, false, TestName = "AtRifle_AMan_NoSpurt")]
        public void AtRifle_AMissedShotAtAHullSpurtsPastIt(byte archetype, bool hullTarget, bool struckThisTick, bool expected)
        {
            Assert.AreEqual(expected, CombatFx.SpurtsPastHull(archetype, hullTarget, struckThisTick));
        }

        [TestCase(InfantryArchetype.AtRifle, true, false, true, TestName = "AtRifle_Hull_NoStrike_IsMarked")]
        [TestCase(InfantryArchetype.AtRifle, true, true, true, TestName = "AtRifle_Hull_Struck_IsMarked")]
        [TestCase(InfantryArchetype.Rifle, true, false, false, TestName = "Rifle_Hull_IsNotMarked")]
        [TestCase(InfantryArchetype.AtRifle, false, false, false, TestName = "AtRifle_AMan_IsNotMarked")]
        public void AtRifle_EveryHullShotIsMarkedOnce(byte archetype, bool hullTarget, bool struckThisTick, bool expectMarked)
        {
            bool spurts = CombatFx.SpurtsPastHull(archetype, hullTarget, struckThisTick);
            bool strikeAlreadyDrawn = hullTarget && struckThisTick && CombatFx.KindOf(archetype) == CombatFx.ArmsKind.AtRifle;
            Assert.AreEqual(expectMarked, spurts || strikeAlreadyDrawn, "a hull shot settles as exactly one mark, or none when it is not this arm's shot");
        }
    }
}
