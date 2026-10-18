// Phase: found-impact-dust-lands-on-raw-ground-ignoring-the-20261018-0 (the review, 2026-10-18). Cause: the Hit
// case's feet Spurt resampled RenderGround instead of using the chest he was actually drawn with (a fire-step, a
// bridge, a roof prop). DustFeetY/ChestHeight (CombatFx.Bodies.cs) recover his feet from his socketed chest.
using NUnit.Framework;
using TW.Sim;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class ImpactDustFeetTests
    {
        [Test]
        public void SocketedChest_PutsTheDustOnTheStepHeStandsOn()
        {
            // bare terrain at 0, a man standing on a 1.0 m fire-step, scale 1: units.Sockets gives chest y 1.0 + 1.2 = 2.2
            float chestY = 2.2f;
            float feetY = CombatFx.DustFeetY(chestY, CombatFx.ChestHeight(Stance.Standing, false), 1f);

            Assert.AreEqual(1.0f, feetY, 0.02f, "[found-impact-dust-lands-on-raw-ground-ignoring-the-20261018-0] the dust must land on the fire-step, not the bare ground under it");
            Assert.Greater(feetY, 0f + 0.5f, "[found-impact-dust-lands-on-raw-ground-ignoring-the-20261018-0] the old line drew it at the raw ground sample; the fix must sit well above that");
        }

        [Test]
        public void AFallbackChest_StillLandsOnTheTerrain()
        {
            // EstimateChest's own geometry: ground 3.5, scale 1.3, standing: chest = 3.5 + 1.2 * 1.3 = 5.06
            float ground = 3.5f, scale = 1.3f;
            float chestY = ground + CombatFx.ChestHeight(Stance.Standing, false) * scale;
            float feetY = CombatFx.DustFeetY(chestY, CombatFx.ChestHeight(Stance.Standing, false), scale);

            Assert.AreEqual(ground, feetY, 0.02f, "[found-impact-dust-lands-on-raw-ground-ignoring-the-20261018-0] the fallback chest's own math must still return to the terrain it was built from");
        }

        [Test]
        public void ChestHeights_AreTheDrawnStances()
        {
            Assert.AreEqual(1.2f, CombatFx.ChestHeight(Stance.Standing, false), 0.001f, "[found-impact-dust-lands-on-raw-ground-ignoring-the-20261018-0] standing chest height must not move");
            Assert.AreEqual(0.8f, CombatFx.ChestHeight(Stance.Crouch, false), 0.001f, "[found-impact-dust-lands-on-raw-ground-ignoring-the-20261018-0] crouch chest height must not move");
            Assert.AreEqual(0.3f, CombatFx.ChestHeight(Stance.Prone, false), 0.001f, "[found-impact-dust-lands-on-raw-ground-ignoring-the-20261018-0] prone chest height must not move");
            Assert.AreEqual(0.3f, CombatFx.ChestHeight(Stance.Pinned, false), 0.001f, "[found-impact-dust-lands-on-raw-ground-ignoring-the-20261018-0] pinned chest height must not move");
            Assert.AreEqual(1.4f, CombatFx.ChestHeight(Stance.Standing, true), 0.001f, "[found-impact-dust-lands-on-raw-ground-ignoring-the-20261018-0] vehicle chest height must not move");
        }
    }
}
