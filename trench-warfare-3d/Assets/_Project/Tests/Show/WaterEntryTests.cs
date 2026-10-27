// Phase: water-entry-splash (2026-10-19, harsh critic: "a man or body dying in water, a flung body landing in
// water, or a dropped item makes no splash; only shells ring the water"). Proves WaterEntry.Throw rings the water
// by mass and nothing on dry ground, with books null throughout: no scene, no GPU.
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class WaterEntryTests
    {
        List<Vector3> rings;

        [SetUp]
        public void SetUp()
        {
            rings = new List<Vector3>();
            SceneHooks.AddRing = (x, z, size) => rings.Add(new Vector3(x, z, size));
            WaterEntry.Clear();
        }

        [TearDown]
        public void TearDown()
        {
            SceneHooks.IsWater = null;
            SceneHooks.WaterY = null;
            SceneHooks.AddRing = null;
            WaterEntry.Clear();
        }

        [Test]
        public void DeathInWaterThrowsSplash()
        {
            SceneHooks.IsWater = (x, z) => true;
            var sp = WaterEntry.Throw(null, new Vector3(12f, 0.1f, 20f), WaterEntry.ManKg);

            Assert.IsTrue(sp.Any, "a man dying in water splashes");
            Assert.AreEqual(1, rings.Count, "one ring, sized by mass");
            Assert.AreEqual(2.8f, sp.Ring, 1e-4f, "a man's own mass opens his named ring (ManRing, raised for r6)");
            Assert.AreEqual(1.6f, sp.Card, 1e-4f, "and a crown card to go with it (ManCard, lowered for r7-g1)");
            Assert.Less(sp.Ring, 3f, "under the smallest a shell opens (WaterRings.OnSimEvent)");
        }

        [Test]
        public void WaterSplashSitsOnTheSurface()
        {
            // knee-deep in a river: the surface stands at 0.5, the bed he dies on at -0.6
            SceneHooks.IsWater = (x, z) => true;
            SceneHooks.WaterY = (x, z) => 0.5f;
            var sp = WaterEntry.Throw(null, new Vector3(12f, -0.6f, 20f), WaterEntry.ManKg);

            Assert.IsTrue(sp.Any);
            Assert.AreEqual(0.5f, sp.Y, 0.02f, "the card sits on the surface, not on the bed");
            Assert.Greater(sp.Y, -0.6f, "above the bed he died on");

            // no water sheet (no hook): the point's own y, so nothing regresses
            SceneHooks.WaterY = null;
            sp = WaterEntry.Throw(null, new Vector3(12f, -0.6f, 20f), WaterEntry.ManKg);
            Assert.AreEqual(-0.6f, sp.Y, 1e-4f, "with no hook the card keeps the point's own height");

            // a painted puddle above the water table: the ground there is already its surface
            SceneHooks.WaterY = (x, z) => 0.1f;
            sp = WaterEntry.Throw(null, new Vector3(12f, 0.4f, 20f), WaterEntry.ManKg);
            Assert.AreEqual(0.4f, sp.Y, 1e-4f, "a puddle above the table keeps its own ground height");
        }

        [Test]
        public void DeathOnDryGroundThrowsNoSplash()
        {
            SceneHooks.IsWater = (x, z) => false;
            var sp = WaterEntry.Throw(null, new Vector3(12f, 0.1f, 20f), WaterEntry.ManKg);

            Assert.IsFalse(sp.Any, "dry ground throws no splash");
            Assert.AreEqual(0f, sp.Ring);
            Assert.AreEqual(0f, sp.Card);
            Assert.AreEqual(0, rings.Count, "no ring opened");

            // a map with no water at all (IsWater never wired up) is the same as dry ground
            SceneHooks.IsWater = null;
            sp = WaterEntry.Throw(null, new Vector3(12f, 0.1f, 20f), WaterEntry.ManKg);
            Assert.IsFalse(sp.Any);
            Assert.AreEqual(0, rings.Count);
        }

        [Test]
        public void AHelmetRingsSmallerThanAMan()
        {
            SceneHooks.IsWater = (x, z) => true;
            float rifleRing = WaterEntry.Ring(WaterEntry.RifleKg);
            float helmetRing = WaterEntry.Ring(WaterEntry.HelmetKg);

            // worked by hand: cbrt(4.3/75) = 0.3859, cbrt(1.2/75) = 0.2520, times ManRing (2.8)
            Assert.AreEqual(1.081f, rifleRing, 0.01f, "the dropped rifle's ring, sized by its mass");
            Assert.AreEqual(0.706f, helmetRing, 0.01f, "the helmet's ring, smaller still");
            Assert.Less(rifleRing, WaterEntry.ManRing, "sized by mass: lighter than a man");
            Assert.Less(helmetRing, WaterEntry.ManRing, "sized by mass: lighter than a man");
        }

        [Test]
        public void ACrownTwoWingsAndTheFallenFoam()
        {
            // r7-g1: the owner read r7 as "a brighter copy of the old wisp", and the mask measured 3.4 m wide by
            // 5.9 m tall. Worked by hand from the constants: crown = ManCard 1.6 x scale 1; wings = WingSize 0.70 x
            // 1.6 = 1.12, sitting WingOut 0.80 x ManRing 2.8 = 2.24 m either side (a 5.6 m span against 1.6 m
            // tall); the foam = FoamAcross 1.8 x ManRing 2.8 = 5.04 m, lying flat.
            var buf = new WaterEntry.CardPlan[4];
            Assert.AreEqual(4, WaterEntry.Cards(WaterEntry.ManKg, 1f, buf), "a crown, two wings and the fallen foam");

            Assert.AreEqual(0f, buf[0].Dx, 1e-4f, "the crown stands over the entry point");
            Assert.AreEqual(1.6f, buf[0].Size, 1e-4f, "ManCard x scale 1");
            Assert.AreEqual(-2.24f, buf[1].Dx, 1e-3f, "the left wing, 0.80 x the ring out");
            Assert.AreEqual(2.24f, buf[2].Dx, 1e-3f, "and the right wing the other side");
            Assert.AreEqual(1.12f, buf[1].Size, 1e-3f, "a wing is 0.70 of the crown");
            Assert.AreEqual(1.12f, buf[2].Size, 1e-3f);
            Assert.IsFalse(buf[1].Mirror, "one wing as the sheet draws it");
            Assert.IsTrue(buf[2].Mirror, "the other flipped, so it is not the same picture twice");

            // the spray reaches wider than the crown stands tall: a crown, not a column
            float span = (buf[2].Dx + buf[2].Size * 0.5f) - (buf[1].Dx - buf[1].Size * 0.5f);
            Assert.Greater(span, buf[0].Size, "wider than it is tall, which is what the owner could not see in r7");

            // the fourth card is the fallen foam: on the water, flat, and wider than the ring it fills
            Assert.AreEqual(0f, buf[3].Dx, 1e-4f, "the foam lies over the entry point");
            Assert.AreEqual(5.04f, buf[3].Size, 1e-3f, "FoamAcross 1.8 x the ring");
            Assert.IsTrue(buf[3].Flat, "it lies ON the water, it does not stand up");
            Assert.IsFalse(buf[0].Flat, "the crown stands");
            Assert.Greater(buf[3].Size, WaterEntry.Ring(WaterEntry.ManKg), "wider than the ring");
            Assert.Greater(WaterEntry.FoamLife, WaterEntry.CardLife, "the foam outlives the crown that threw it");

            // half-size thing (scale 0.5): the crown halves, and the wings follow it, not the ring
            Assert.AreEqual(4, WaterEntry.Cards(WaterEntry.ManKg, 0.5f, buf));
            Assert.AreEqual(0.8f, buf[0].Size, 1e-4f, "the crown takes the caller's scale");
            Assert.AreEqual(0.56f, buf[1].Size, 1e-3f, "0.70 of that crown");
            Assert.AreEqual(-2.24f, buf[1].Dx, 1e-3f, "the reach is the ring's, which scale does not touch");
            Assert.AreEqual(5.04f, buf[3].Size, 1e-3f, "and so is the foam's width");
        }

        [Test]
        public void TheFoamIsDimmerThanTheCrownAboveIt()
        {
            // r7 won crown_contrast_post (the crown reads over the water a third of a second after the entry) by
            // lifting the crown's VALUE. The foam takes the same lift with a smaller share of it, so that win holds.
            var splash = new Color(0.60f, 0.66f, 0.76f);
            var crown = WaterEntry.CrownTint(splash);
            var foam = WaterEntry.FoamTint(splash);
            float Luma(Color c) => 0.2126f * c.r + 0.7152f * c.g + 0.0722f * c.b;

            Assert.Greater(Luma(foam), Luma(splash), "the foam still steps above the water it lies on");
            Assert.Less(Luma(foam), Luma(crown), "but under the crown, which must read as the brighter of the two");
            Assert.Less(foam.r, foam.b, "the biome's hue survives in the foam too");
            Assert.AreEqual(1f, foam.a, 1e-4f, "the tint carries no alpha of its own");
        }

        [Test]
        public void TheCrownKeepsTheBiomesHueAndLiftsItsValue()
        {
            // harsh critic r1: the crown was blue-white on blue water and measured a 0.03 value step over it. The
            // bar itself is the capture's (0.15 at land); this only holds the direction and the hue.
            var splash = new Color(0.60f, 0.66f, 0.76f);   // the Splash book's own tint (FlipbookFx.Sheets)
            var crown = WaterEntry.CrownTint(splash);

            float lumaIn = 0.2126f * splash.r + 0.7152f * splash.g + 0.0722f * splash.b;
            float lumaOut = 0.2126f * crown.r + 0.7152f * crown.g + 0.0722f * crown.b;
            Assert.Greater(lumaOut, lumaIn * 1.35f, "the crown is a clear value step over the water's own splash tint");

            // the hue survives: the channels keep their order, so a desert ford is still not a flooded trench
            Assert.Less(crown.r, crown.g, "blue-green water stays blue-green, only brighter");
            Assert.Less(crown.g, crown.b);
            var warm = WaterEntry.CrownTint(new Color(0.76f, 0.66f, 0.50f));
            Assert.Greater(warm.r, warm.b, "a warm, silty ford's crown is warm too");
            Assert.AreEqual(1f, crown.a, 1e-4f, "the tint carries no alpha of its own");
        }

        [Test]
        public void WaterEntryRingWaitsForLanding()
        {
            SceneHooks.IsWater = (x, z) => true;
            var sp = WaterEntry.Throw(null, new Vector3(12f, 0.1f, 20f), WaterEntry.ManKg, 0.8f);

            Assert.IsTrue(sp.Any, "a flung body landing in water still splashes");
            Assert.AreEqual(0, rings.Count, "the ring does not open at the throw");

            WaterEntry.Pump(0.4f); // half the flight in, nothing yet
            Assert.AreEqual(0, rings.Count, "half the flight in, nothing yet");

            // r7-g1: the ring opens Lead (0.06 s) BEFORE the landing, so 0.70 s in is still too early and 0.75 s
            // in - past 0.8 - 0.06 = 0.74 - is not. That lead is what the body beat photographs.
            WaterEntry.Pump(0.3f); // 0.70 s total
            Assert.AreEqual(0, rings.Count, "0.70 s in, still short of the 0.74 s the lead moves it to");

            WaterEntry.Pump(0.05f); // 0.75 s total, past the lead-adjusted landing
            Assert.AreEqual(1, rings.Count, "the ring opens as his feet break the water, a lead before the landing");
            Assert.AreEqual(WaterEntry.ManRing, rings[0].z, 1e-4f, "sized the same as an immediate ring");

            WaterEntry.Pump(1f);
            Assert.AreEqual(1, rings.Count, "it opens once, not every frame after");

            // a flight shorter than the lead rings at once: nothing is ever queued for a moment already past
            rings.Clear();
            WaterEntry.Throw(null, new Vector3(12f, 0.1f, 20f), WaterEntry.ManKg, WaterEntry.Lead * 0.5f);
            Assert.AreEqual(1, rings.Count, "a flight shorter than the lead rings at the throw");
        }
    }
}
