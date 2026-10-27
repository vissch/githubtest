// Phase: water-entry-splash (2026-10-19, harsh critic: "a man or body dying in water, a flung body landing in
// water, or a dropped item makes no splash; only shells ring the water"). Proves WaterEntry.Throw rings the water
// by mass and nothing on dry ground, with books null throughout: no scene, no GPU.
using System.Collections.Generic;
using System.Reflection;
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
            Assert.AreEqual(2.4f, sp.Card, 1e-4f, "and a crown card to go with it (ManCard, raised for r6)");
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
        public void ACrownAndTwoWingsGoUp()
        {
            // r6, harsh critic r1: one 1.1 m card measured 326 px at T1 against a bar of 1000. Worked by hand from
            // the constants: crown = ManCard 2.4 x scale 1; wings = WingSize 0.55 x 2.4 = 1.32, sitting
            // WingOut 0.45 x ManRing 2.8 = 1.26 m either side.
            var buf = new WaterEntry.CardPlan[3];
            Assert.AreEqual(3, WaterEntry.Cards(WaterEntry.ManKg, 1f, buf), "a crown and two wings");

            Assert.AreEqual(0f, buf[0].Dx, 1e-4f, "the crown stands over the entry point");
            Assert.AreEqual(2.4f, buf[0].Size, 1e-4f, "ManCard x scale 1");
            Assert.AreEqual(-1.26f, buf[1].Dx, 1e-3f, "the left wing, 0.45 x the ring out");
            Assert.AreEqual(1.26f, buf[2].Dx, 1e-3f, "and the right wing the other side");
            Assert.AreEqual(1.32f, buf[1].Size, 1e-3f, "a wing is 0.55 of the crown");
            Assert.AreEqual(1.32f, buf[2].Size, 1e-3f);
            Assert.IsFalse(buf[1].Mirror, "one wing as the sheet draws it");
            Assert.IsTrue(buf[2].Mirror, "the other flipped, so it is not the same picture twice");

            // half-size thing (scale 0.5): the crown halves, and the wings follow it, not the ring
            Assert.AreEqual(3, WaterEntry.Cards(WaterEntry.ManKg, 0.5f, buf));
            Assert.AreEqual(1.2f, buf[0].Size, 1e-4f, "the crown takes the caller's scale");
            Assert.AreEqual(0.66f, buf[1].Size, 1e-3f, "0.55 of that crown");
            Assert.AreEqual(-1.26f, buf[1].Dx, 1e-3f, "the reach is the ring's, which scale does not touch");
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
        public void TheCrownIsDownByThePostFrame()
        {
            // The owner on r7: "post.png at +0.3 s still holds a white sprite in the water." The capture's post
            // moment is land + 0.3 s, so 0.30 s is the spec moment itself, not an arithmetic fraction of the old
            // 0.7 s. The wings stand 1.32 m and sit 1.26 m out, inside the crown box the capture measures, so they
            // have to fall too - a little later, so the water falls back from the middle outwards.
            Assert.LessOrEqual(WaterEntry.CrownLife, 0.30f, "the crown is down by the post frame (land + 0.3 s)");
            Assert.LessOrEqual(WaterEntry.WingLife, 0.45f, "and the wings with it");
            Assert.Less(WaterEntry.CrownLife, WaterEntry.WingLife, "the crown goes first, the wings after it");

            // and Throw wires each card to its own life, not one life to all three
            SceneHooks.IsWater = (x, z) => true;
            var fx = new FlipbookFx();
            try
            {
                Assume.That(fx.Ready, "the flipbook shader and sheets load in the editor");
                WaterEntry.Throw(fx, new Vector3(12f, 0.1f, 20f), WaterEntry.ManKg);
                Assert.AreEqual(3, fx.Alive, "a crown and two wings");
                Assert.AreEqual(FlipbookFx.Book.Crown, fx.CardBook(0), "the first card is the crown");
                Assert.AreEqual(WaterEntry.CrownLife, fx.CardLife(0), 1e-6f, "the crown card takes the crown's life");
                for (int i = 1; i < 3; i++)
                {
                    Assert.AreEqual(FlipbookFx.Book.Wings, fx.CardBook(i), "the other two are wings");
                    Assert.AreEqual(WaterEntry.WingLife, fx.CardLife(i), 1e-6f, "a wing takes the wing's life");
                }
                Assert.Less(fx.CardLife(0), fx.CardLife(1), "the crown's own card is the shorter-lived one");
            }
            finally
            {
                // DestroyImmediate: Dispose's Object.Destroy is not allowed in edit mode (ColumnPlayTests)
                var mats = (Material[])typeof(FlipbookFx).GetField("mats", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(fx);
                foreach (var m in mats) if (m != null) Object.DestroyImmediate(m);
                var quad = (Mesh)typeof(FlipbookFx).GetField("quad", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(fx);
                if (quad != null) Object.DestroyImmediate(quad);
            }
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

            WaterEntry.Pump(0.45f); // 0.85 s total, past the 0.8 s landing
            Assert.AreEqual(1, rings.Count, "the ring opens once the body lands");
            Assert.AreEqual(WaterEntry.ManRing, rings[0].z, 1e-4f, "sized the same as an immediate ring");

            WaterEntry.Pump(1f);
            Assert.AreEqual(1, rings.Count, "it opens once, not every frame after");
        }
    }
}
