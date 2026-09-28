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
            Assert.IsFalse(r.Plume || r.Mortar || r.CookOff || r.Ring || r.Lean || r.Dust || r.Curtain, what + ": nothing new");
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
            Assert.IsTrue(r.Column && r.Plume && r.Lean, "the column, the plume, and the earth thrown on");
            Assert.IsFalse(r.OldSmoke || r.Mortar || r.CookOff);
        }

        [Test]
        public void On_ARoundWithNoLean_BurstsAsAMortar()
        {
            var r = CombatFx.RecipeFor(0f, 0f, false, 1f);
            Assert.IsTrue(r.Column && r.Plume && r.Mortar);
            Assert.IsFalse(r.OldSmoke || r.CookOff || r.Lean, "no flight, no lean");
        }

        [Test]
        public void On_ACookOff_DrawsNoEarth()
        {
            var r = CombatFx.RecipeFor(2f, 0f, false, 1f);
            Assert.IsTrue(r.CookOff && r.Plume);
            Assert.IsFalse(r.Column || r.OldSmoke || r.Mortar || r.Lean);
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
        public void On_WaterMasonryIncendiaryAndMines_KeepTheOldBurst()
        {
            AssertOld(CombatFx.RecipeFor(0f, 0.8f, true, 1f), "a shell in water");
            AssertOld(CombatFx.RecipeFor(2f, 0f, true, 1f), "a cook-off in water");
            AssertOld(CombatFx.RecipeFor(1f, 0f, false, 1f), "falling masonry");
            AssertOld(CombatFx.RecipeFor(3f, 0f, false, 1f, 9f), "an incendiary");
            AssertOld(CombatFx.RecipeFor(5f, 0f, false, 1f, 9f), "a mine (no lean, but not a mortar)");
        }

        [Test]
        public void HealGlint_OnlyWithRecipesNearAndOncePerMedicSpell()
        {
            Assert.IsFalse(CombatFx.HealGlint(10f, 0f, 30f, 0f), "fx.recipes 0: no glint");
            Assert.IsTrue(CombatFx.HealGlint(10f, 0f, 30f, 1f), "near, first sweep");
            Assert.IsFalse(CombatFx.HealGlint(10f, 10f + CombatFx.HealEvery, 30f, 1f), "his next sweep inside HealEvery");
            Assert.IsTrue(CombatFx.HealGlint(10f + CombatFx.HealEvery, 10f + CombatFx.HealEvery, 30f, 1f), "HealEvery on");
            Assert.IsFalse(CombatFx.HealGlint(10f, 0f, CombatFx.HealNearZoom, 1f), "the overview: none");
        }
        [Test]
        public void BloodFor_OnlyWithRecipesGoreAWoundAndNotFromTheOverview()
        {
            Assert.IsNull(CombatFx.BloodFor(24f, 1f, 30f, 0f), "fx.recipes 0: the dust as before");
            Assert.IsNull(CombatFx.BloodFor(24f, 0f, 30f, 1f), "GORE 0: none");
            Assert.IsNull(CombatFx.BloodFor(0f, 1f, 30f, 1f), "no wound: none");
            Assert.IsNull(CombatFx.BloodFor(24f, 1f, CombatFx.BloodFarZoom, 1f), "the overview: none");
            Assert.AreEqual(FlipbookFx.Book.BloodSpurt, CombatFx.BloodFor(24f, 1f, 30f, 1f), "a rifle");
            Assert.AreEqual(FlipbookFx.Book.BloodSnipe, CombatFx.BloodFor(95f, 1f, 30f, 1f), "the sniper");
        }
        [Test]
        public void Banks_MergeFromTheOverviewAndBoilInsideTheirFrames()
        {
            Assert.AreEqual(1, CombatFx.BankStep(30f), "close: a card a cell");
            Assert.AreEqual(2, CombatFx.BankStep(CombatFx.BankMergeZoom), "the overview: a card a 2 x 2 block");
            for (float t = 0f; t < 7f; t += 0.37f)
            {
                float f = CombatFx.BankFrame(17f, 28f, t);
                Assert.That(f, Is.InRange(17f, 28f), "the boil stays inside the bank's full frames");
            }
        }
        [Test]
        public void JetpackLanding_RingAndDustNoColumn()
        {
            int landing = TW.Sim.Combat.LeapSystem.LandingSource;
            var r = CombatFx.RecipeFor(0f, 0f, false, 1f, 4f, landing);
            Assert.IsTrue(r.Ring && r.Dust, "a ring and dust where he comes down");
            Assert.IsFalse(r.Column || r.OldSmoke || r.Plume || r.Mortar || r.Lean || r.CookOff, "no shell's parts");
            AssertOld(CombatFx.RecipeFor(0f, 0f, false, 0f, 4f, landing), "fx.recipes 0");
            Assert.IsFalse(CombatFx.RecipeFor(0f, 0f, false, 1f, 4f).Dust, "a shell has no landing dust");
        }
        [Test]
        public void CreepingBarrage_StandsACurtain_AndShellsComeInAhead()
        {
            int creeping = (int)TW.Sim.Match.OffMapAbilityId.CreepingBarrage, he = (int)TW.Sim.Match.OffMapAbilityId.HeBarrage;
            Assert.IsTrue(CombatFx.RecipeFor(0f, 0.5f, false, 1f, 8f, creeping).Curtain, "a creeping lift's shell");
            Assert.IsFalse(CombatFx.RecipeFor(0f, 0.5f, false, 1f, 8f, he).Curtain, "an ordinary barrage's shell");
            AssertOld(CombatFx.RecipeFor(0f, 0.5f, false, 0f, 8f, creeping), "fx.recipes 0");
            int shell = (int)TW.Sim.Match.PayloadKind.Shell;
            Assert.IsTrue(CombatFx.IsIncoming(shell, he));
            Assert.IsFalse(CombatFx.IsIncoming(shell, (int)TW.Sim.Match.OffMapAbilityId.StrafeRun), "a strafe's rounds come from the aircraft");
            Assert.IsFalse(CombatFx.IsIncoming((int)TW.Sim.Match.PayloadKind.GasSource, (int)TW.Sim.Match.OffMapAbilityId.ChlorineGas), "a canister");
            Assert.AreEqual(12u, CombatFx.IncomingLeadTicks(0.05f), "the streak lands 7/12 s in, at 20 ticks a second");
        }
        [Test]
        public void Smoulders_AboutAThirdOfPlaces_TheSameEveryTime()
        {
            int n = 0, total = 0;
            for (int x = 0; x < 40; x++)
            for (int z = 0; z < 40; z++, total++)
                if (CombatFx.Smoulders(x * 3.7f, z * 5.3f)) n++;
            Assert.That(n / (float)total, Is.InRange(0.25f, 0.35f), "30 % of heavy bursts smoulder");
            Assert.AreEqual(CombatFx.Smoulders(12.5f, 40.25f), CombatFx.Smoulders(12.5f, 40.25f), "by place, not by chance");
        }
    }
}
