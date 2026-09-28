// Phase: VFX pass (owner, 2026-09-28; tw3d-board catalogue IN-7) - CombatFx.RecipeFor, the drawn parts of a burst by what
// went off. fx.recipes 0 (the default) must hand every burst the old parts, so the AOSA-tuned column and smoke are untouched.
using NUnit.Framework;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class BurstRecipeTests
    {
        [SetUp] public void SetUp() => Knobs.Clear();
        [TearDown] public void TearDown() => Knobs.Clear();

        static void AssertOld(CombatFx.BurstRecipe r, string what)
        {
            Assert.IsTrue(r.Column && r.OldSmoke, what + ": the old column and smoke");
            Assert.IsFalse(r.Plume || r.Mortar || r.CookOff, what + ": nothing new");
        }

        [Test]
        public void Knob_DefaultIsTheOldBurst()
        {
            Assert.AreEqual("fx.recipes", CombatFx.RecipesKnob);
            Assert.AreEqual(0f, CombatFx.ReadRecipes());
            foreach (float shape in new[] { 0f, 1f, 2f })
            foreach (float lean in new[] { 0f, 0.7f })
            foreach (bool wet in new[] { false, true })
                AssertOld(CombatFx.RecipeFor(shape, lean, wet, CombatFx.ReadRecipes()), $"shape {shape}, lean {lean}, wet {wet}");
            Knobs.Set(CombatFx.RecipesKnob, "1");
            Assert.AreEqual(1f, CombatFx.ReadRecipes());
        }

        [Test]
        public void On_AShellInFlight_LeavesAPlumeNotPuffs()
        {
            var r = CombatFx.RecipeFor(0f, 0.8f, false, 1f);
            Assert.IsTrue(r.Column && r.Plume);
            Assert.IsFalse(r.OldSmoke || r.Mortar || r.CookOff);
        }

        [Test]
        public void On_ARoundWithNoLean_BurstsAsAMortar()
        {
            var r = CombatFx.RecipeFor(0f, 0f, false, 1f);
            Assert.IsTrue(r.Column && r.Plume && r.Mortar);
            Assert.IsFalse(r.OldSmoke || r.CookOff);
        }

        [Test]
        public void On_ACookOff_DrawsNoEarth()
        {
            var r = CombatFx.RecipeFor(2f, 0f, false, 1f);
            Assert.IsTrue(r.CookOff && r.Plume);
            Assert.IsFalse(r.Column || r.OldSmoke || r.Mortar);
        }

        [Test]
        public void On_OnlyABigShell_ThrowsAGroundRing()
        {
            Assert.IsTrue(CombatFx.RecipeFor(0f, 0.8f, false, 1f, CombatFx.RingRadius).Ring, "a shell of the ring's radius");
            Assert.IsFalse(CombatFx.RecipeFor(0f, 0.8f, false, 1f, CombatFx.RingRadius - 0.5f).Ring, "a smaller shell");
            Assert.IsFalse(CombatFx.RecipeFor(2f, 0f, false, 1f, 9f).Ring, "a cook-off");
            Assert.IsFalse(CombatFx.RecipeFor(0f, 0.8f, false, 0f, 9f).Ring, "fx.recipes 0");
        }

        [Test]
        public void Pack_AFlatCard_LiesOnTheGround()
        {
            var at = UnityEngine.Vector3.zero;
            Assert.AreEqual(2f, FlipbookFx.Pack(at, 1f, 1f, 0f, 1f, 1f, 0f, FlipbookFx.Kind.Flat).m12, "flat");
            Assert.AreEqual(2f, FlipbookFx.Pack(at, 1f, 1f, 0f, 1f, 1f, 0f, FlipbookFx.Kind.Flat | FlipbookFx.Kind.Upright).m12, "flat wins over upright");
            Assert.AreEqual(1f, FlipbookFx.Pack(at, 1f, 1f, 0f, 1f, 1f, 0f, FlipbookFx.Kind.Upright).m12, "upright as before");
            Assert.AreEqual(0f, FlipbookFx.Pack(at, 1f, 1f, 0f, 1f, 1f, 0f, FlipbookFx.Kind.None).m12, "facing the view as before");
        }

        [Test]
        public void On_WaterAndMasonry_KeepTheOldBurst()
        {
            AssertOld(CombatFx.RecipeFor(0f, 0.8f, true, 1f), "a shell in water");
            AssertOld(CombatFx.RecipeFor(2f, 0f, true, 1f), "a cook-off in water");
            AssertOld(CombatFx.RecipeFor(1f, 0f, false, 1f), "falling masonry");
        }
    }
}
