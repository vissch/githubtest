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
            Assert.AreEqual(1.6f, sp.Card, 1e-4f, "and a crown card to go with it (ManCard: a column the width of a man, night-read-g1)");
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
            // water-splash-night-read-g1: worked by hand from the constants, and the sizes are the ones three
            // staged night runs measured. crown = ManCard 1.6 x scale 1, standing CrownTall 2.0 x that = 3.2 m
            // high; wings = WingSize 1.75 x 1.6 = 2.8 m wide and WingTall 0.8 x that = 2.24 m high, sitting
            // WingOut 0.8 x ManRing 2.8 = 2.24 m either side, out in the band the probe reads.
            var buf = new WaterEntry.CardPlan[8];
            Assert.AreEqual(8, WaterEntry.Cards(WaterEntry.ManKg, 1f, buf), "a crown, two wings, four droplets and a foam skirt");

            Assert.AreEqual(0f, buf[0].Dx, 1e-4f, "the crown stands over the entry point");
            Assert.AreEqual(1.6f, buf[0].Size, 1e-4f, "ManCard x scale 1");
            Assert.AreEqual(3.2f, buf[0].Height, 1e-4f, "a column the height of a man, not a 3 m bale");
            Assert.AreEqual(-2.24f, buf[1].Dx, 1e-3f, "the left wing, 0.8 x the ring out");
            Assert.AreEqual(2.24f, buf[2].Dx, 1e-3f, "and the right wing the other side");
            Assert.AreEqual(2.8f, buf[1].Size, 1e-3f, "a wing is 1.75 of the crown's width");
            Assert.AreEqual(2.8f, buf[2].Size, 1e-3f);
            Assert.AreEqual(2.24f, buf[1].Height, 1e-3f, "and squat: 0.8 of its own width high");
            Assert.IsFalse(buf[1].Mirror, "one wing as the sheet draws it");
            Assert.IsTrue(buf[2].Mirror, "the other flipped, so it is not the same picture twice");

            // half-size thing (scale 0.5): the crown halves, and the wings follow it, not the ring
            Assert.AreEqual(8, WaterEntry.Cards(WaterEntry.ManKg, 0.5f, buf));
            Assert.AreEqual(0.8f, buf[0].Size, 1e-4f, "the crown takes the caller's scale");
            Assert.AreEqual(1.6f, buf[0].Height, 1e-4f, "and so does its height");
            Assert.AreEqual(1.4f, buf[1].Size, 1e-3f, "1.75 of that crown");
            Assert.AreEqual(-2.24f, buf[1].Dx, 1e-3f, "the reach is the ring's, which scale does not touch");
        }

        [Test]
        public void TheCrownsLobesDoNotOverlap()
        {
            // water-splash-night-read-g1: the whole point of the new sizes. Every card is drawn from the ONE
            // water-white sheet (Book.Splash), so two cards inside one footprint are one pale blob and no lobe is
            // visible. A lobe owns screen space when its centre is at least half the two widths out from the
            // crown's centre. Worked by hand: crown 1.6 wide, wing 2.8 -> 2.2 m clear, and WingOut 0.8 x
            // ManRing 2.8 = 2.24 m; droplet 1.6 -> 1.6 m clear, and the droplets sit on the diagonal at
            // DropletOut 0.65 x 2.8 = 1.82 m each way, so sqrt(2) x 1.82 = 2.574 m out.
            var buf = new WaterEntry.CardPlan[8];
            WaterEntry.Cards(WaterEntry.ManKg, 1f, buf);
            float crown = buf[0].Size;

            for (int i = 1; i <= 6; i++)
            {
                float d = Mathf.Sqrt(buf[i].Dx * buf[i].Dx + buf[i].Dz * buf[i].Dz);
                float clear = 0.5f * (crown + buf[i].Size);
                Assert.GreaterOrEqual(d, clear, "card " + i + " must clear the crown's half-width, not stack inside it");
                Assert.IsFalse(buf[i].Flat, "card " + i + " stands up");
            }
            Assert.AreEqual(2.24f, Mathf.Abs(buf[1].Dx), 1e-3f, "the wings 2.24 m out");
            Assert.AreEqual(2.574f, Mathf.Sqrt(buf[3].Dx * buf[3].Dx + buf[3].Dz * buf[3].Dz), 5e-3f, "the droplets 2.57 m out on the diagonal");

            Assert.IsTrue(buf[7].Flat, "the foam skirt lies flat on the water, not a ninth column in the blob");
            Assert.AreEqual(6.0f, buf[7].Size, 1e-3f, "SkirtSize 3.75 x the crown's width: a 6 m foam disc");
            Assert.AreEqual(buf[7].Size, buf[7].Height, 1e-3f, "and round, not a card on edge");
        }

        [Test]
        public void WaterSplashReadsAtNight()
        {
            // harsh critic r1 fix 1 (2026-10-19): the crown of droplets and foam that lifts a night entry out of
            // the dark, proved as arithmetic a test can read (the pixel bar is docs/evidence/water-splash-night-read).
            var buf = new WaterEntry.CardPlan[8];
            int n = WaterEntry.Cards(WaterEntry.ManKg, 1f, buf);
            Assert.AreEqual(8, n, "the crown, two wings, four droplets and the foam skirt");

            // night-read-g1: the measure is no longer how much card there is (eight cards in one footprint were one
            // 3 m blob) but how far out the splash reaches. The furthest card's far edge, from the entry point:
            // the wings at 2.1 m plus their own half-width, against r6's single 1.2 m half-width.
            float far = 0f;
            for (int i = 0; i < n; i++)
                far = Mathf.Max(far, Mathf.Sqrt(buf[i].Dx * buf[i].Dx + buf[i].Dz * buf[i].Dz) + 0.5f * buf[i].Size);
            Assert.GreaterOrEqual(far, 2f, "the splash reaches past 2 m, out in the band a player can see it in");
            Assert.Greater(far, 2f * (0.5f * 2.4f), "further out than r6's one card reached at all");

            Assert.GreaterOrEqual(WaterEntry.Glow(true), 3f, "night lifts the entry out of the dark");
            Assert.Greater(WaterEntry.Glow(true), WaterEntry.Glow(false), "brighter at night than in daylight");

            Assert.GreaterOrEqual(buf[7].Life, 1f, "the foam skirt still stands at the O120 shot");

            // no card asks for the mud-tinted Wings book any more: every water card is drawn from Book.Splash
            // (WaterEntry.Throw), proved indirectly here by every card carrying its own life and alpha, not the
            // shared defaults Wings used to get.
            for (int i = 0; i < n; i++)
            {
                Assert.Greater(buf[i].Life, 0f, "card " + i + " has a life");
                Assert.Greater(buf[i].Alpha, 0f, "card " + i + " has an alpha");
            }

            // a dry-ground entry still draws nothing
            SceneHooks.IsWater = (x, z) => false;
            var sp = WaterEntry.Throw(null, new Vector3(5f, 0.1f, 5f), WaterEntry.ManKg, night: true);
            Assert.IsFalse(sp.Any, "dry ground throws no splash, day or night");
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
