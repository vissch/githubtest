// Phase: VFX pass (owner, 2026-09-28; tw3d-board catalogue IN-7) - CombatFx.RecipeFor, the drawn parts of a burst by what
// went off. The recipes are on by default (owner, 2026-10-07); fx.recipes 0 must hand every burst the old parts, so the
// AOSA-tuned column and smoke can be had back untouched.
using NUnit.Framework;
using UnityEngine;
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
        public void Knob_DefaultIsTheRecipes_ZeroIsTheOldBurst()
        {
            Assert.AreEqual("fx.recipes", CombatFx.RecipesKnob);
            Assert.AreEqual(1f, CombatFx.ReadRecipes(), "on by default (owner, 2026-10-07)");
            Assert.IsTrue(CombatFx.RecipeFor(0f, 0.8f, false, CombatFx.ReadRecipes()).Plume, "the default draws the plume");
            Knobs.Set(CombatFx.RecipesKnob, "0");
            Assert.AreEqual(0f, CombatFx.ReadRecipes());
            foreach (float shape in new[] { 0f, 1f, 2f })
            foreach (float lean in new[] { 0f, 0.7f })
            foreach (bool wet in new[] { false, true })
                AssertOld(CombatFx.RecipeFor(shape, lean, wet, CombatFx.ReadRecipes()), $"shape {shape}, lean {lean}, wet {wet}");
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
        [Test]
        public void ClassArms_EachClassItsOwnShot()
        {
            var rifle = CombatFx.ArmsFor(TW.Sim.InfantryArchetype.Rifle, 10u, 3);
            Assert.AreEqual(1f, rifle.Flare); Assert.AreEqual(1f, rifle.Tracer); Assert.IsTrue(rifle.Case, "the rifle is the base look");
            var sniper = CombatFx.ArmsFor(TW.Sim.InfantryArchetype.Sniper, 10u, 3);
            Assert.Greater(sniper.Tracer, rifle.Tracer); Assert.Greater(sniper.Flare, rifle.Flare);
            Assert.Less(CombatFx.ArmsFor(TW.Sim.InfantryArchetype.Shield, 10u, 3).Flare, rifle.Flare, "a pistol flares less");
            int bright = 0;
            for (uint t = 0; t < 30; t++) if (CombatFx.ArmsFor(TW.Sim.InfantryArchetype.Machinegunner, t, 7).Tracer > 1.2f) bright++;
            Assert.AreEqual(10, bright, "every third machine-gun round a tracer round");
            int flares = 0, puffs = 0;
            for (uint t = 0; t < 30; t++) { var mg = CombatFx.ArmsFor(TW.Sim.InfantryArchetype.Machinegunner, t, 7); if (mg.Flared) flares++; if (mg.Smoked) puffs++; }
            Assert.AreEqual(15, flares, "a machine gun's flare card on every second round"); Assert.AreEqual(10, puffs, "and a smoke puff on every third");
            Assert.IsTrue(rifle.Flared && rifle.Smoked && rifle.FlareLife == 0.18f, "a rifle: every round as before");
            // the machines' guns
            var maw = TankRenderer.GunFor(TW.Sim.VehicleArchetype.Maw);
            Assert.AreEqual(TankRenderer.GunLook.Old.Blast, maw.Blast, "the Maw's 6-pdr is the old shot");
            Assert.Less(TankRenderer.GunFor(TW.Sim.VehicleArchetype.Tusk).Blast, maw.Blast, "a 37 mm blasts smaller");
            Assert.Greater(TankRenderer.GunFor(TW.Sim.VehicleArchetype.Pavise).Blast, maw.Blast, "a long gun bigger");
            Assert.IsTrue(TankRenderer.GunFor(TW.Sim.VehicleArchetype.Kettle).Arc && !maw.Arc, "the mortar lobs, the tank gun does not");
            Vector3 a = new Vector3(0, 1, 0), b = new Vector3(60, 1, 0);
            Assert.AreEqual(a, TankRenderer.ArcPoint(a, b, 10f, 0f)); Assert.AreEqual(b, TankRenderer.ArcPoint(a, b, 10f, 1f));
            Assert.AreEqual(11f, TankRenderer.ArcPoint(a, b, 10f, 0.5f).y, 1e-4f, "the top of the arc stands apex over the line");
            Assert.IsFalse(CombatFx.ArmsFor(TW.Sim.VehicleArchetype.Maw, 10u, 3).Case, "no case out of a hull gun");
            Assert.AreEqual(1f, CombatFx.ReadClassArms(), "on by default");
        }
        [Test]
        public void ShellFire_InsideTheCloud_ByPlace()
        {
            Assert.Greater(CombatFx.ShellFireWidth(8f, 0f), CombatFx.ShellFireWidth(3f, 0f), "a bigger shell, a bigger fireball");
            Assert.Less(CombatFx.ShellFireWidth(8f, 1f), CombatFx.ShellFireWidth(8f, 0f), "smaller up close, as the flash is");
            Assert.Less(CombatFx.ShellFireWidth(8f, 0f), 8f * CombatFx.BurstWidth(false, 1f), "narrower than the cloud it burns in");
            Assert.AreEqual(2.6f, CombatFx.BurstWidth(true, 1f), "the night cloud as the AOSA tuned it");
            Assert.AreEqual(2.6f, CombatFx.BurstWidth(false, 0f), "fx.shellFire 0: the old cloud");
            Assert.AreEqual(Color.clear, CombatFx.EmberFor(false, 0f), "fx.shellFire 0: no ember");
            Assert.Greater(CombatFx.EmberFor(false, 1f).r, CombatFx.EmberFor(true, 1f).r, "dimmer at night");
            Assert.That(FlipbookFx.BurstWeight(0.6f), Is.InRange(0.85f, 0.9f), "the burst's cloud denser than the lingering smoke");
            Assert.AreEqual(1f, FlipbookFx.BurstWeight(1f), 1e-6f, "fx.smokeWeight 1: as it was");
            for (int x = 0; x < 30; x++)
            {
                var p = new Vector3(x * 7.3f, 1f, x * -4.1f);
                float r = 2f + (x % 8);
                var a = CombatFx.PocketOffset(p, 0, r); var b = CombatFx.PocketOffset(p, 1, r);
                foreach (var o in new[] { a, b })
                {
                    Assert.LessOrEqual(new Vector2(o.x, o.z).magnitude, 0.5f * r + 1e-4f, "within half the cloud's width");
                    Assert.That(o.y, Is.InRange(0.45f * r - 1e-4f, 0.95f * r + 1e-4f), "low in the cloud, off the ground");
                }
                Assert.Less(a.x * b.x + a.z * b.z, 0f, "the two on opposite sides");
                Assert.AreEqual(a, CombatFx.PocketOffset(p, 0, r), "by place, not by chance");
            }
            Assert.IsTrue(CombatFx.Masonry(1f) && !CombatFx.Masonry(0f) && !CombatFx.Masonry(2f), "only falling masonry has no fire");
            Assert.Greater(CombatFx.GritCount(8f), CombatFx.GritCount(3f), "a bigger shell sprays more grit");
            Assert.AreEqual(1f, CombatFx.ShellFarAt(30f), "the standard view: as drawn");
            Assert.Greater(CombatFx.ShellFarAt(70f), 1.5f, "z70: grown, not a pinprick");
            Assert.AreEqual(2.2f, CombatFx.ShellFarAt(400f), 1e-6f, "capped");
        }
        [Test]
        public void ARoundAtAWreck_IsATracerForTheUsualTime_NotGoneOnItsFirstFrame()
        {
            // review E.1: the wreck shot built its tracer without a Life, and the prune drops a tracer once now - Born > Life
            Assert.IsTrue(CombatFx.TracerSpent(10.2f, 10f, 0.12f), "past its time on screen");
            Assert.IsFalse(CombatFx.TracerSpent(10.1f, 10f, 0.12f), "inside it");
            Assert.IsTrue(CombatFx.TracerSpent(10f + 1f / 60f, 10f, 0f), "a tracer with no Life is gone a frame after it is born");
            Assert.IsTrue(CombatFx.WreckTracerDrawn(0.12f, 1f / 60f), "one frame after it is born the round at a wreck is still drawn");
            Assert.IsTrue(CombatFx.WreckTracerDrawn(0.12f, 0.11f), "and for the usual round's time (fx.tracerSeconds)");
            Assert.IsFalse(CombatFx.WreckTracerDrawn(0.12f, 0.13f), "then it is pruned as any round is");
        }
        [Test]
        public void TheJetpacksFlame_HangsFromWhereHeIsDrawn_NotFromThePresentersPoint()
        {
            // review E.2: the presenter's y is the sim's zero, so on ground above zero the jet burned under the ground.
            // On ground 1.2 m up: the middle of the Jet card (half its 1.2 m below his pack) and the puff (0.6 m below it)
            var standing = new Vector3(40f, 1.2f, 30f);
            Vector3 back = CombatFx.LeapBack(standing);
            Assert.AreEqual(1.2f + CombatFx.LeapBackUp, back.y, 1e-5f, "his pack, over where he is drawn standing");
            Assert.AreEqual(40f, back.x); Assert.AreEqual(30f, back.z);
            Assert.Greater(back.y - 0.6f, standing.y, "the flame and the puff are above the ground he stands on");
            // and the leap takes his place from drawnAt (the render ground sampled under him), not from the presenter
            string src = System.IO.File.ReadAllText(System.IO.Path.Combine(Application.dataPath, "_Project/Presentation/Camera/CombatFx.Support.cs"));
            int from = src.IndexOf("void TickLeaps(", System.StringComparison.Ordinal);
            Assert.GreaterOrEqual(from, 0, "CombatFx.Support.cs has no TickLeaps (renamed? this test reads it by name)");
            int to = src.IndexOf("\n        }", from, System.StringComparison.Ordinal);
            Assert.Greater(to, from, "TickLeaps has no end");
            string body = src.Substring(from, to - from);
            StringAssert.Contains("LeapBack(drawnAt(", body, "the jet is hung from where the man is drawn (drawnAt)");
            StringAssert.DoesNotContain("Presenter.Drawn(", body, "the presenter's y is the sim's zero: on ground above zero the jet is under the ground");
        }
        [Test]
        public void AJetpackLeap_EndsByTheSimsClock_AtTwiceTheSpeedAndInAPause()
        {
            // review E.3: LeapStarted's scalar is sim seconds. A leap of one second, begun at 50 s of sim and 10 s on the wall
            var leap = CombatFx.LeapStart(3, 50f, 10f, 1f);
            Assert.AreEqual(3, leap.Slot);
            Assert.AreEqual(10f, leap.NextFlame, "the flame is a cadence on the wall clock"); Assert.AreEqual(10f, leap.NextPuff, "and the puffs");
            Assert.IsFalse(CombatFx.LeapOver(leap, 50.9f), "in the air until the sim's second is up");
            Assert.IsTrue(CombatFx.LeapOver(leap, 51.05f), "landed: the jet stops with him");
            // at 2x the sim runs two seconds to the wall's one: he lands half a wall second on, and the jet stops then
            float wall = 10f, sim = 50f; int frames = 0;
            while (!CombatFx.LeapOver(leap, sim) && frames < 600) { wall += 1f / 60f; sim += 2f / 60f; frames++; }
            Assert.That(wall - 10f, Is.InRange(0.49f, 0.55f), "at 2x the leap is over half a wall second on");
            // paused, the sim's clock stands however long the wall clock runs: he hangs in the air and the leap is not over
            Assert.IsFalse(CombatFx.LeapOver(leap, 50.4f), "held in a pause he is still in the air");
            Assert.AreEqual(50.2f, CombatFx.LeapStart(3, 50f, 10f, 0f).Until, 1e-4f, "never shorter than 0.2 s");
        }
    }
}
