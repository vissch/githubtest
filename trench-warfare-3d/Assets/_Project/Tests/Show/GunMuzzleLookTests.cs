// Phase: the Banner, legs and fire (owner, 2026-10-12, on its daylight film: "the firing vfx and the rifle
// animation can be improved") - the near smoke of a machine's shot, TankRenderer.MuzzleLook.
// In that film the Banner's shot was one flat flame and one beige puff that covered its own front leg. The cause was
// measured, not guessed: the Banner's GunLook.Blast is 6.0, so the Puff card came out 4.8 m wide at alpha 0.7, 1.2 s
// long and only 0.6 m off the muzzle, while the three Smoke cards drifted ALONG the barrel (2.5, 1.9, 1.3 m/s) with
// just 0.5 m/s up, so they hung on the machine. The Banner's two front legs stand about 2.9 m apart at the knees
// (measured on the posed meshes in BannerFrontLegsTests, commit fda931814), so any card near the muzzle wider than
// that covers a leg. These are the claims of the new look, and that the machines the owner did not complain about
// still draw exactly the old numbers.
using NUnit.Framework;
using TW.Sim;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class GunMuzzleLookTests
    {
        const float LegGap = 3.0f;      // a card this wide near the muzzle would cover a front leg
        const float NearMuzzle = 2f;    // "near": within 2 m of the muzzle along the barrel

        [Test]
        public void TheBannersFlashIsAShortCrack()
        {
            var fx = TankRenderer.MuzzleLook(VehicleArchetype.Banner);
            Assert.LessOrEqual(fx.FlashLife, 0.12f, "the Banner's flash stands in the air too long");
        }

        [Test]
        public void NoCardNearTheBannersMuzzleCoversAFrontLeg()
        {
            var fx = TankRenderer.MuzzleLook(VehicleArchetype.Banner);
            if (fx.PuffPush < NearMuzzle)
                Assert.Less(fx.PuffWidth, LegGap, $"the Puff is {fx.PuffWidth:0.00} m wide {fx.PuffPush:0.00} m off the muzzle");
            for (int k = 0; k < 3; k++)
                if (fx.SmokePushOf(k) < NearMuzzle)
                    Assert.Less(fx.SmokeSizeOf(k), LegGap, $"smoke card {k} is {fx.SmokeSizeOf(k):0.00} m wide {fx.SmokePushOf(k):0.00} m off the muzzle");
        }

        [Test]
        public void TheBannersSmokeGoesUpAndThins()
        {
            var fx = TankRenderer.MuzzleLook(VehicleArchetype.Banner);
            for (int k = 0; k < 3; k++)
                Assert.Greater(fx.SmokeUp, System.Math.Abs(fx.SmokeAlongOf(k)), $"smoke card {k} drifts along the barrel more than up");
            Assert.Less(fx.SmokeAlphaOf(2), fx.SmokeAlphaOf(0), "the smoke chain does not thin as it goes out");
        }

        // Every machine the owner did not complain about: the numbers the renderer drew before MuzzleLook existed.
        [TestCase("Maw", VehicleArchetype.Maw, 4.5f, 1f)]
        [TestCase("Tusk", VehicleArchetype.Tusk, 3.0f, 0.8f)]
        [TestCase("Pavise", VehicleArchetype.Pavise, 6.0f, 1.35f)]
        [TestCase("Kettle", VehicleArchetype.Kettle, 2.6f, 1.2f)]
        [TestCase("Salvo", VehicleArchetype.Salvo, 3.2f, 1.6f)]
        public void TheOtherMachinesKeepTheOldLook(string name, byte archetype, float blast, float smoke)
        {
            var fx = TankRenderer.MuzzleLook(archetype);
            Assert.AreEqual(1.6f, fx.FlashWidth, 1e-4f, name);
            Assert.AreEqual(3.2f, fx.FlashWidthPlain, 1e-4f, name);
            Assert.AreEqual(0.1f, fx.FlashLife, 1e-4f, name);
            Assert.AreEqual(blast * 0.8f, fx.PuffWidth, 1e-4f, name);
            Assert.AreEqual(1.2f, fx.PuffLife, 1e-4f, name);
            Assert.AreEqual(0.7f, fx.PuffAlpha, 1e-4f, name);
            Assert.AreEqual(0.6f, fx.PuffPush, 1e-4f, name);
            for (int k = 0; k < 3; k++)
            {
                Assert.AreEqual((1.2f + k * 0.4f) * smoke, fx.SmokeSizeOf(k), 1e-4f, $"{name} smoke {k} size");
                Assert.AreEqual(2.5f + k * 0.5f, fx.SmokeLifeOf(k), 1e-4f, $"{name} smoke {k} life");
                Assert.AreEqual(0.55f, fx.SmokeAlphaOf(k), 1e-4f, $"{name} smoke {k} alpha");
                Assert.AreEqual(2.5f - k * 0.6f, fx.SmokeAlongOf(k), 1e-4f, $"{name} smoke {k} drift along");
                Assert.AreEqual(0.6f + k * 0.7f, fx.SmokePushOf(k), 1e-4f, $"{name} smoke {k} push");
            }
            Assert.AreEqual(0.5f, fx.SmokeUp, 1e-4f, name);
            Assert.AreEqual(1.6f, fx.SmokeGrow, 1e-4f, name);
            Assert.AreEqual(0.03f, fx.SmokeDelayStep, 1e-4f, name);
        }
    }
}
