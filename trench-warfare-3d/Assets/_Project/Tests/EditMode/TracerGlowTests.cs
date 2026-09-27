// Phase: C107 (AOSA) - the night halo's weight under the smoke: fx.tracerGlow (its colour gain) and fx.tracerHaloDepth
// (whether the in-smoke halo writes depth and so cuts the cloud behind it out). Neither set is today's look bit for bit.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class TracerGlowTests
    {
        [SetUp] public void SetUp() => Knobs.Clear();
        [TearDown] public void TearDown() => Knobs.Clear();

        // the halo colours CombatFx.Start makes, written out
        static readonly Color HaloA = new Color(0.06f, 0.36f, 0.12f), HaloB = new Color(0.50f, 0.07f, 0.05f);

        [Test]
        public void Unset_TheGainIsOne_AndTheColourIsUntouched()
        {
            Assert.AreEqual("fx.tracerGlow", TracerLook.GlowKnob);
            Assert.AreEqual(TracerLook.OldGlow, TracerLook.DefaultGlow);
            Assert.AreEqual(1f, TracerLook.ReadGlow());
            Assert.AreEqual("1", Knobs.Read["fx.tracerGlow"]);
            foreach (var c in new[] { HaloA, HaloB })
            {
                var at = TracerLook.HaloColor(c, TracerLook.ReadGlow());
                Assert.IsTrue(at.r == c.r && at.g == c.g && at.b == c.b && at.a == c.a, "gain 1 is the colour bit for bit");
            }
            Knobs.Set(TracerLook.GlowKnob, "1");
            Assert.AreEqual(1f, TracerLook.ReadGlow(), "set to 1 is the same");
        }

        [Test]
        public void Unset_TheInSmokeHaloStillWritesDepth_AndTheOldOrderNever()
        {
            Assert.AreEqual("fx.tracerHaloDepth", TracerLook.DepthKnob);
            Assert.AreEqual(1f, TracerLook.ReadDepth());
            Assert.AreEqual("1", Knobs.Read["fx.tracerHaloDepth"]);
            // C104's two-argument answer is the one-argument one for both orders
            foreach (bool on in new[] { false, true })
                Assert.AreEqual(TracerLook.HaloZWrite(on), TracerLook.HaloZWrite(on, TracerLook.ReadDepth()), "order " + on);
            Assert.AreEqual(0f, TracerLook.HaloZWrite(false, 1f));
            Assert.AreEqual(1f, TracerLook.HaloZWrite(true, 1f));
        }

        [Test]
        public void DepthOff_TheInSmokeHaloNoLongerWritesDepth_ButKeepsItsQueue()
        {
            Knobs.Set(TracerLook.DepthKnob, "0");
            float depth = TracerLook.ReadDepth();
            Assert.AreEqual(0f, TracerLook.HaloZWrite(true, depth), "no longer cuts the smoke and bursts behind it");
            Assert.AreEqual(0f, TracerLook.HaloZWrite(false, depth), "the old order never wrote depth");
            Assert.Less(TracerLook.HaloQueue(true), 3010, "still before the smoke books: a cloud in front still dims it");
            Knobs.Set(TracerLook.DepthKnob, "5");
            Assert.AreEqual(1f, TracerLook.ReadDepth(), "kept in [0, 1]");
            Knobs.Set(TracerLook.DepthKnob, "-2");
            Assert.AreEqual(0f, TracerLook.ReadDepth());
        }

        [Test]
        public void Gain_ScalesTheColourKeepsItsHueAndAlpha_AndIsClamped()
        {
            Knobs.Set(TracerLook.GlowKnob, "2.5");
            float g = TracerLook.ReadGlow();
            Assert.AreEqual(2.5f, g);
            foreach (var c in new[] { HaloA, HaloB })
            {
                var at = TracerLook.HaloColor(c, g);
                Assert.AreEqual(c.r * 2.5f, at.r, 1e-6f); Assert.AreEqual(c.g * 2.5f, at.g, 1e-6f); Assert.AreEqual(c.b * 2.5f, at.b, 1e-6f);
                Assert.AreEqual(c.a, at.a, "alpha is not the gain's (the halo is One One additive)");
                Assert.Greater(Mathf.Max(at.r, at.g, at.b), 0.85f, "2.5 takes both sides over the bloom threshold (Atmosphere, 0.85)");
            }
            Assert.Less(Mathf.Max(HaloA.r, HaloA.g, HaloA.b), 0.85f, "today neither halo blooms on its own");
            Assert.Less(Mathf.Max(HaloB.r, HaloB.g, HaloB.b), 0.85f);
            Knobs.Set(TracerLook.GlowKnob, "9");
            Assert.AreEqual(TracerLook.MaxGlow, TracerLook.ReadGlow());
            Knobs.Set(TracerLook.GlowKnob, "-1");
            Assert.AreEqual(0f, TracerLook.ReadGlow());
        }

        [Test]
        public void TheKnobsDoNotTouchTheShape()
        {
            // the gain and the depth write are material state only: every tracer matrix is the same with them set
            var from = new Vector3(12f, 1.3f, 40f); var to = new Vector3(80f, 1.1f, 95f);
            Vector3 d = to - from; float len = d.magnitude;
            var before = new Matrix4x4[3];
            for (int side = 0; side < 3; side++) before[side] = TracerLook.Matrix(from, d, len, 0.4f, true, side, 0f, 0f);
            Knobs.Set(TracerLook.GlowKnob, "2"); Knobs.Set(TracerLook.DepthKnob, "0");
            TracerLook.ReadGlow(); TracerLook.ReadDepth();
            for (int side = 0; side < 3; side++) Assert.IsTrue(TracerLook.Matrix(from, d, len, 0.4f, true, side, 0f, 0f).Equals(before[side]), "side " + side);
        }
    }
}
