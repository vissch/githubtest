// Phase: VFX pass (owner, 2026-09-28: "have everything customized, add the extra effort zoom and quality level") -
// FxQuality's tiers, and what each class, gun and burst draws as its own (CombatFx.Close.cs, CombatFx.Bursts.cs,
// TankRenderer.Guns.cs). High must be the numbers the effects had before the tiers, so every measured picture stands.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Sim;
using TW.Sim.Combat;

namespace TW.Tests
{
    public class FxQualityTests
    {
        [SetUp] public void SetUp() { Knobs.Clear(); FxQuality.Set(FxTier.High); }
        [TearDown] public void TearDown() { Knobs.Clear(); FxQuality.Set(FxTier.High); }

        [Test]
        public void High_Is_The_Old_Numbers_And_The_Tiers_Climb()
        {
            var high = FxQuality.For(FxTier.High);
            Assert.AreEqual(24, high.ImpactsNear); Assert.AreEqual(8, high.ImpactsFar);
            Assert.AreEqual(40, high.HitsNear); Assert.AreEqual(10, high.HitsFar);
            Assert.AreEqual(700, high.Cap(700), "a pool's cap is the old one");
            Assert.AreEqual(1f, high.Reach); Assert.AreEqual(1f, high.Debris); Assert.AreEqual(1f, high.Grit);
            Assert.AreEqual(1, high.FlareEvery); Assert.AreEqual(TankRenderer.ArcSegments, high.ArcSegments);
            Assert.AreEqual(FxTier.High, FxQuality.Tier, "High until something sets another");
            for (int t = 1; t < 4; t++)
            {
                FxProfile lo = FxQuality.For((FxTier)(t - 1)), hi = FxQuality.For((FxTier)t);
                Assert.Greater(hi.Chunks, lo.Chunks, $"tier {t} spends more chunks than {t - 1}");
                Assert.GreaterOrEqual(hi.ImpactsNear, lo.ImpactsNear); Assert.GreaterOrEqual(hi.HitsNear, lo.HitsNear);
                Assert.LessOrEqual(hi.FlareEvery, lo.FlareEvery); Assert.GreaterOrEqual(hi.ArcSegments, lo.ArcSegments);
                Assert.GreaterOrEqual(hi.ExtraReach, lo.ExtraReach);
            }
            Assert.AreEqual(0f, FxQuality.For(FxTier.Low).Reach, "Low draws no case and no thread of smoke");
            Assert.AreEqual(0f, FxQuality.For(FxTier.Medium).ExtraReach, "the class pieces are High's and Epic's");
            Assert.IsTrue(FxQuality.For(FxTier.Epic).Epic && !high.Epic);
            Assert.AreEqual(4, FxQuality.Names.Length);
        }

        [Test]
        public void The_Tier_Comes_From_The_Knob_Then_The_Setting_Then_The_Quality_Level()
        {
            // the project's six levels: Very Low, Low, Medium, High, Very High, Ultra
            var want = new[] { FxTier.Low, FxTier.Low, FxTier.Medium, FxTier.High, FxTier.High, FxTier.Epic };
            for (int i = 0; i < 6; i++) Assert.AreEqual(want[i], FxQuality.FromUnity(i, 6), "quality level " + i);
            Assert.AreEqual(FxTier.High, FxQuality.FromUnity(0, 1), "one level: High");
            Assert.AreEqual(FxTier.Epic, FxQuality.Resolve(-1, 5, 6, -1), "AUTO follows the quality level");
            Assert.AreEqual(FxTier.Low, FxQuality.Resolve(0, 5, 6, -1), "the setting wins over the level");
            Assert.AreEqual(FxTier.Medium, FxQuality.Resolve(3, 5, 6, 1), "the knob wins over both");
            Assert.AreEqual(FxTier.Epic, FxQuality.Resolve(9, 0, 6, -1), "past the top is Epic");
            FxQuality.Set(FxTier.Low);
            Assert.AreEqual(FxTier.Low, FxQuality.Tier); Assert.AreEqual(FxQuality.For(FxTier.Low).Chunks, FxQuality.Now.Chunks);
        }

        [Test]
        public void The_Setting_Is_Saved_And_Clamped()
        {
            var s = GameSettings.Defaults();
            Assert.AreEqual(-1, s.Video.Effects, "AUTO until the player picks");
            s.Video.Effects = 7; s.Migrate(); Assert.AreEqual(3, s.Video.Effects, "clamped to Epic");
            s.Video.Effects = -5; s.Migrate(); Assert.AreEqual(-1, s.Video.Effects, "clamped to AUTO");
            s.Video.Effects = 1;
            Assert.AreEqual(1, GameSettings.FromJson(s.ToJson()).Video.Effects, "it survives settings.json");
            CollectionAssert.Contains(TW.UI.SettingsScreen.RequiredNames, "dropdown-effects");
        }

        [Test]
        public void Far_Muzzle_Cards_Thin_By_Tier_And_Zoom()
        {
            int Kept(FxTier tier, float toLook, float zoom)
            {
                var q = FxQuality.For(tier); int n = 0;
                for (uint r = 0; r < 3000; r++) if (FxQuality.FlareKept(q, toLook, zoom, r)) n++;
                return n;
            }
            foreach (FxTier t in new[] { FxTier.Low, FxTier.Medium, FxTier.High, FxTier.Epic })
                Assert.AreEqual(3000, Kept(t, 20f, 240f), t + ": a flare near the look point is always drawn");
            Assert.AreEqual(3000, Kept(FxTier.High, 200f, 40f), "High far off at the standard view: every flare, as before");
            Assert.AreEqual(3000, Kept(FxTier.Epic, 200f, 240f), "Epic never thins");
            Assert.AreEqual(1500, Kept(FxTier.High, 200f, 240f), 150, "High from the overview: about half");
            Assert.AreEqual(1000, Kept(FxTier.Low, 200f, 40f), 120, "Low far off: about a third");
            Assert.AreEqual(1500, Kept(FxTier.Medium, 200f, 40f), 150, "Medium far off: about half");
            for (uint x = 0; x < 200; x++) { float h = FxQuality.Hash01(x); Assert.That(h, Is.GreaterThanOrEqualTo(0f).And.LessThan(1f)); }
        }

        [Test]
        public void Each_Class_Its_Own_Weapon_And_Hit()
        {
            Assert.AreEqual(CombatFx.ArmsKind.Rifle, CombatFx.KindOf(InfantryArchetype.Rifle));
            Assert.AreEqual(CombatFx.ArmsKind.Smg, CombatFx.KindOf(InfantryArchetype.Assault));
            Assert.AreEqual(CombatFx.ArmsKind.Mg, CombatFx.KindOf(InfantryArchetype.Machinegunner));
            Assert.AreEqual(CombatFx.ArmsKind.Sniper, CombatFx.KindOf(InfantryArchetype.Sniper));
            Assert.AreEqual(CombatFx.ArmsKind.Carbine, CombatFx.KindOf(InfantryArchetype.Officer));
            Assert.AreEqual(CombatFx.ArmsKind.Pistol, CombatFx.KindOf(InfantryArchetype.Shield));
            Assert.AreEqual(CombatFx.ArmsKind.MachinePistol, CombatFx.KindOf(InfantryArchetype.Jetpack));
            Assert.AreEqual(CombatFx.ArmsKind.HullMg, CombatFx.KindOf(VehicleArchetype.Tusk));
            Assert.AreEqual(1f, CombatFx.HitFlashOf(CombatFx.ArmsKind.Rifle), "the rifle's hit is the old one");
            // each class its own muzzle drawing; the rifle keeps the old flare
            Assert.IsFalse(CombatFx.MuzzleBookOf(CombatFx.ArmsKind.Rifle).HasValue);
            Assert.AreEqual(FlipbookFx.Book.MuzzleBrake, CombatFx.MuzzleBookOf(CombatFx.ArmsKind.Sniper));
            Assert.AreEqual(FlipbookFx.Book.MuzzleStream, CombatFx.MuzzleBookOf(CombatFx.ArmsKind.Mg));
            Assert.AreEqual(FlipbookFx.Book.MuzzlePop, CombatFx.MuzzleBookOf(CombatFx.ArmsKind.Pistol));
            Assert.AreEqual(FlipbookFx.Book.MuzzleBurst, CombatFx.MuzzleBookOf(CombatFx.ArmsKind.Smg));
            Assert.AreEqual(FlipbookFx.Book.MuzzleCarbine, CombatFx.MuzzleBookOf(CombatFx.ArmsKind.Carbine), "the officer and the SMG no longer share a drawing");
            Assert.AreEqual(1f, CombatFx.MuzzleScale(FlipbookFx.Book.MuzzleBrake, 0f), "the standard view: the flare's own size");
            Assert.Greater(CombatFx.MuzzleScale(FlipbookFx.Book.MuzzleBrake, 1f), 1.5f, "up close a book is drawn well over its flare's size");
            Assert.Less(CombatFx.BookGrowNight, CombatFx.BookGrow, "smaller at night, where the glow adds to it");
            Assert.Less(CombatFx.ClassBookTallest * 2f, CombatFx.ClassBookMost, "capped across the barrel harder than along it");
            Assert.IsNotNull(Resources.Load<Texture2D>("VFX/MuzzleBrake"), "the sheet is in Resources/VFX");
            Assert.Greater(CombatFx.HitFlashOf(CombatFx.ArmsKind.Sniper), CombatFx.HitFlashOf(CombatFx.ArmsKind.Mg));
            Assert.Less(CombatFx.HitFlashOf(CombatFx.ArmsKind.Pistol), 1f);
        }

        [Test]
        public void Up_Close_The_Flare_Grows_And_Each_Round_Has_Its_Own_Life_And_Length()
        {
            for (float r = 0f; r <= 1f; r += 0.25f)
            {
                Assert.AreEqual(1.05f + r * 0.4f, CombatFx.FlareBase(r, 1f, false), 1e-5f, "fx.classArms 0: the old flare at any zoom");
                Assert.AreEqual(1.05f + r * 0.4f, CombatFx.FlareBase(r, 0f, true), 1e-5f, "the standard view: the old flare");
                Assert.Greater(CombatFx.FlareBase(r, 1f, true), 1.7f, "among the men: bigger");
            }
            Assert.AreEqual(0.18f, CombatFx.FlareLifeAt(0.18f, 0f, true), 1e-6f); Assert.Greater(CombatFx.FlareLifeAt(0.18f, 1f, true), 0.25f);
            Assert.AreEqual(1.6f, CombatFx.FlareGlow(false, 0f, true)); Assert.Greater(CombatFx.FlareGlow(false, 1f, true), 2.5f, "brighter on snow up close");
            Assert.AreEqual(1.3f, CombatFx.TracerWidthAt(1.3f, 0f)); Assert.AreEqual(1.3f * 1.15f, CombatFx.TracerWidthAt(1.3f, 1f), 1e-5f, "a heavy round keeps a little weight, not a beam");
            Assert.AreEqual(1f, CombatFx.DayStreakAt(true, 1f, true)); Assert.AreEqual(0.5f, CombatFx.DayStreakAt(false, 1f, true), 1e-6f, "by day up close: half");
            Assert.AreEqual(1f, CombatFx.DayStreakAt(false, 1f, false), "fx.classArms 0: as it was");
            Assert.IsTrue(CombatFx.DayFlare(false, 0.5f, true)); Assert.IsFalse(CombatFx.DayFlare(true, 1f, true) || CombatFx.DayFlare(false, 0f, true) || CombatFx.DayFlare(false, 1f, false));
            Assert.AreEqual(0.7f, CombatFx.TracerWidthAt(0.7f, 1f), "a light one thins as every round does");
            var rifle = CombatFx.ArmsLook.Rifle;
            Assert.AreEqual(1f, rifle.TracerLife); Assert.AreEqual(1f, rifle.Streak);
            var sniper = CombatFx.ArmsFor(InfantryArchetype.Sniper, 4u, 1);
            Assert.Greater(sniper.TracerLife, 2f, "a sniper's round lingers"); Assert.Greater(sniper.Flare, rifle.Flare, "and flares more than a rifle");
            Assert.Less(CombatFx.ArmsFor(InfantryArchetype.Shield, 4u, 1).Streak, 0.5f, "a pistol spits");
            int dashes = 0, tracerRounds = 0;
            for (uint t = 0; t < 30; t++)
            {
                var mg = CombatFx.ArmsFor(InfantryArchetype.Machinegunner, t, 5);
                if (mg.Streak < 1f && mg.TracerLife < 1f) dashes++; else if (mg.TracerLife > 1f) tracerRounds++;
            }
            Assert.AreEqual(20, dashes, "two machine-gun rounds in three are dashes");
            Assert.AreEqual(10, tracerRounds, "and the third a tracer round that hangs");
            Assert.AreNotEqual(CombatFx.ArmsFor(InfantryArchetype.Assault, 4u, 1).Flare, CombatFx.ArmsFor(InfantryArchetype.Jetpack, 4u, 1).Flare, "the SMG and the machine pistol differ");
        }

        [Test]
        public void The_Bundle_Is_Thrown_Onto_The_Hull()
        {
            Vector3 hand = new Vector3(0f, 1.8f, 0f), to = new Vector3(6f, 2.2f, 3f);
            float t = CombatFx.BundleFlight(Vector3.Distance(hand, to));
            Assert.That(t, Is.InRange(0.35f, 0.7f));
            // the chunks fall under 9.8: where the throw is after t seconds
            Vector3 v = CombatFx.BundleVelocity(hand, to, t), at = hand + v * t + 0.5f * t * t * Vector3.down * 9.8f;
            Assert.AreEqual(0f, Vector3.Distance(at, to), 1e-3f, "it lands on the hull");
            Assert.Greater(v.y, 0f, "thrown up and over, not along the ground");
            Assert.AreEqual(0.7f, CombatFx.BundleFlight(400f), "a long throw is capped");
        }

        [Test]
        public void Each_Burst_By_What_Made_It()
        {
            var shell = CombatFx.BurstBy((int)TW.Sim.Match.OffMapAbilityId.CreepingBarrage);
            Assert.AreEqual(1f, shell.Size); Assert.AreEqual(1f, shell.Fire); Assert.AreEqual(1f, shell.Column); Assert.AreEqual(1f, shell.Wings);
            Assert.IsFalse(shell.Mortar || shell.Rocket || shell.Mine || shell.Tripwire, "an ability's shell is the shell burst");
            var mortar = CombatFx.BurstBy(SourceId.Unit(VehicleArchetype.Kettle));
            Assert.IsTrue(mortar.Mortar); Assert.Greater(mortar.Wings, 1f, "a mortar bursts low and wide");
            var rocket = CombatFx.BurstBy(SourceId.Unit(VehicleArchetype.Salvo));
            Assert.IsTrue(rocket.Rocket); Assert.Greater(rocket.Fire, 1f, "a rocket burns");
            Assert.Less(CombatFx.BurstBy(SourceId.Unit(VehicleArchetype.Tusk)).Size, 1f, "a 37 mm shell is small");
            Assert.Greater(CombatFx.BurstBy(SourceId.Unit(VehicleArchetype.Pavise)).Size, 1f, "a long gun's is big");
            Assert.AreEqual(1f, CombatFx.BurstBy(SourceId.Unit(VehicleArchetype.Maw)).Size, "a 6-pdr's is the shell burst");
            var mine = CombatFx.BurstBy(MineSystem.SourceBase + (int)MineKind.Mine);
            Assert.IsTrue(mine.Mine); Assert.AreEqual(0f, mine.Fire, "a buried charge throws earth, no fireball"); Assert.Less(mine.Flash, 0.5f, "and no white-out");
            var wire = CombatFx.BurstBy(MineSystem.SourceBase + (int)MineKind.Tripwire);
            Assert.IsTrue(wire.Tripwire); Assert.AreEqual(0f, wire.Column, "a tripwire stands no column"); Assert.Greater(wire.Wings, 1f);
            Assert.IsFalse(CombatFx.BurstBy(SourceId.CookOff).Mine, "a cook-off is not a mine");
        }

        [Test]
        public void The_Salvo_Ripples_By_Tier()
        {
            Assert.AreEqual(1, TankRenderer.RocketsOf(FxTier.Low));
            Assert.AreEqual(2, TankRenderer.RocketsOf(FxTier.Medium));
            Assert.AreEqual(4, TankRenderer.RocketsOf(FxTier.High));
            Assert.AreEqual(4, TankRenderer.RocketsOf(FxTier.Epic));
            Assert.Less(TankRenderer.RocketApex, 1f, "a rocket flies flatter than a mortar round");
        }

        [Test]
        public void Each_Gun_Blast_Is_Its_Calibre_Drawing()
        {
            Assert.AreEqual(FlipbookFx.Book.GunCrack, TankRenderer.BlastBookOf(TW.Sim.VehicleArchetype.Tusk), "the 37 mm: a crack");
            Assert.AreEqual(FlipbookFx.Book.GunLong, TankRenderer.BlastBookOf(TW.Sim.VehicleArchetype.Pavise), "the long gun");
            Assert.AreEqual(FlipbookFx.Book.GunLong, TankRenderer.BlastBookOf(TW.Sim.VehicleArchetype.Banner));
            Assert.AreEqual(FlipbookFx.Book.GunCrack, TankRenderer.BlastBookOf(TW.Sim.VehicleArchetype.Salvo), "the Salvo's launch: a crack, not a white disc (r19)");
            Assert.AreEqual(FlipbookFx.Book.GunBlast, TankRenderer.BlastBookOf(TW.Sim.VehicleArchetype.Kettle), "the rest keep the shared blast");
            Assert.AreNotEqual(TankRenderer.BlastBookOf(TW.Sim.VehicleArchetype.Tusk), TankRenderer.BlastBookOf(TW.Sim.VehicleArchetype.Pavise), "critique r15: they read alike");
        }
    }
}
