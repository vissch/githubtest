// Phase: C1 (unit look, look-05/look-06, 2026-10-06) — who threw the streak, and the shape of the stream.
//
// look-06: the jet is ONE LONG CARD (FlameJetCard), a ribbon from the nozzle to the target, and the three shape
// tests below measure that ribbon. They cannot compile against the chain this replaced - there is no
// FlameJetCard there - which is how they were seen to fail on look-05's code.
//
// look-03's close shot (unit-look/flame/after_close.jpg) has a thin pink streak running from the flamethrower man
// toward the trench, and the master read it as the flame man still throwing a round. He is not: CombatFx.OnSimEvent
// breaks out of the Shot case before any tracer is added when the shooter's weapon SetsBurning (FlameRouteTests
// proves the route). A tracer's colour at night is its SIDE's — tracerNightA green for team 0, tracerNightB dark red
// for team 1 — so the streak's TEAM names its shooter, and no amount of looking at the picture does.
//
// CombatFx.TracerReport is how a capture rig reads that off a live frame. This test is what makes the report
// trustworthy: on the code before it, Tools/flamefight had nothing to print and the question could only be answered
// by eye.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class FlameShapeTests
    {
        [Test]
        public void TracerReportNamesEachStreaksTeam()
        {
            var go = new GameObject("combatfx-report");
            try
            {
                var fx = go.AddComponent<CombatFx>();
                Assert.AreEqual("tracers=0", fx.TracerReport(), "an empty frame throws nothing");

                fx.AddTracer(new Vector3(60f, 1f, 60f), new Vector3(70f, 1f, 60f), team: 1);
                string one = fx.TracerReport();
                StringAssert.StartsWith("tracers=1", one);
                StringAssert.Contains("team=1", one, "the streak must say whose side it is");

                fx.AddTracer(new Vector3(60f, 1f, 60f), new Vector3(70f, 1f, 60f), team: 0);
                StringAssert.StartsWith("tracers=2", fx.TracerReport());
            }
            finally
            {
                Object.DestroyImmediate(go);
            }
        }

        // The SHAPE of the stream. FlipbookFx.Add early-returns unless the pack is loaded, which EditMode cannot
        // make true, and the card is a mesh EditMode cannot render either, so what is testable here is the geometry
        // the stream is built on - FlameJetCard.Spine, HalfAt and Across - and that is exactly where the master's
        // complaint lives: "a thin pale-yellow stick from the muzzle and a separate blob of orange fire a few metres
        // out, with no taper joining them".
        //
        // These three replace TheEnvelopeWidensFromMouthToHead and NeighbouringLinksOverlapAndTheLastOneLandsOnThe-
        // Target, which measured the chain of links the card replaces (Flamethrower.Link/Span are gone).

        [Test]
        public void OneCardRunsUnbrokenFromMouthToHead()
        {
            const int segs = FlameJetCard.Segments;
            const float len = 11f;
            float prevFar = -1f;
            for (int i = 0; i < segs; i++)
            {
                FlameJetCard.Spine(i, segs, len, out float near, out float far, out _);
                if (i == 0) Assert.AreEqual(0f, near, 1e-4f, "the card starts AT the nozzle, not a fifth of the way out");
                else Assert.AreEqual(prevFar, near, 1e-4f,
                    "segment " + i + " must start exactly where " + (i - 1) + " ended: a gap is a waist, and a waist is "
                    + "what made the chain read as a stick and a separate blob");
                Assert.Greater(far, near, "every segment runs downrange");
                prevFar = far;
            }
            Assert.AreEqual(len, prevFar, 1e-4f, "and it ends ON the target");
        }

        [Test]
        public void TheCardWidensFromMouthToHead()
        {
            const int segs = FlameJetCard.Segments;
            FlameJetCard.Spine(0, segs, 11f, out _, out _, out float atMouth);
            FlameJetCard.Spine(segs, segs, 11f, out _, out _, out float atHead);
            Assert.AreEqual(1.55f, atMouth * 2f, 0.05f, "the owner's width at the mouth");
            Assert.AreEqual(3.90f, atHead * 2f, 0.05f, "the owner's width at the head");
            float last = -1f;
            for (int i = 0; i <= segs; i++)
            {
                FlameJetCard.Spine(i, segs, 11f, out _, out _, out float half);
                Assert.GreaterOrEqual(half, last, "the taper never narrows: fuel spreads as it burns");
                last = half;
            }
        }

        [Test]
        public void TheCardHasASideEvenWhenAimedAtTheEye()
        {
            // A stream fired straight down the line of sight is what the old fallback chain of round fireballs
            // existed for: a card in the screen plane has no length left to draw along. The ribbon is a mesh and
            // Across falls back off the collapsed cross product, so there is always a side to give it - which is
            // why the fallback could go.
            Vector3 aim = Vector3.forward;
            foreach (var eye in new[] { Vector3.forward, Vector3.back, aim * 3f, Vector3.zero })
            {
                Vector3 across = FlameJetCard.Across(aim, eye);
                Assert.AreEqual(1f, across.magnitude, 1e-3f, "a side, even looking down the barrel: eye " + eye);
                Assert.Less(Mathf.Abs(Vector3.Dot(across, aim.normalized)), 1e-3f, "and it is across the run, not along it");
            }
            Vector3 side = FlameJetCard.Across(aim, Vector3.right);
            Assert.AreEqual(1f, side.magnitude, 1e-3f);
        }

        [Test]
        public void SmokeHangsOverTheOuterRun()
        {
            Assert.GreaterOrEqual(Flamethrower.JetSmokes, 3, "one puff at the impact is the old behaviour");
            float last = -1f;
            for (int s = 0; s < Flamethrower.JetSmokes; s++)
            {
                Flamethrower.SmokeAt(s, out float u, out float hang, out float size);
                Assert.GreaterOrEqual(u, 0.4f, "the smoke belongs over the outer run, where the fuel has burnt");
                Assert.LessOrEqual(u, 1f);
                Assert.Greater(u, last, "the stations spread along the run rather than stacking");
                last = u;
                Assert.Greater(hang, 1f, "ABOVE the stream: dark over bright is what makes the core read by day");
                Assert.Greater(size, 2f);
            }
        }

        // ------------------------------------------------------------------------------------------------------
        // look-09. The master rejected look-06's card for its SHAPE, not for its reach: "a RULER-STRAIGHT wedge,
        // its top and bottom edges straight lines for hundreds of pixels", and "at night the edges show as hard
        // straight red-orange bands like a laser". The band measure (Tools/flameband.py) was green on that picture
        // and stayed green, so what was missing was a measure of STRAIGHTNESS. These four are it, in EditMode.
        //
        // Each was seen RED on look-06's card before the fix: see the comment on each for the number it gave.
        // ------------------------------------------------------------------------------------------------------

        /// <summary>Pixels to a metre in the side shot Tools/flamefight takes: an 11 m run across about 700 px.</summary>
        const float PxPerMetre = 700f / 11f;

        [Test]
        public void TheSpineDroopsAndKeepsFalling()
        {
            // look-06: Sag(u) = sin(u^0.78 * pi), so Sag(1) == 0 to seven places - the stream came back to the
            // straight chord exactly at the head, which is where the eye is. Seen red on that line: the assert
            // below read "still falling at the head" against 0.000.
            Assert.Greater(FlameJetCard.Sag(1f), 0.5f, "the head is still FALLING: thrown fuel does not come back up");
            Assert.AreEqual(0f, FlameJetCard.Sag(0f), 1e-4f, "and it leaves the nozzle on the aim");

            float a = FlameJetCard.Sag(0f), b = FlameJetCard.Sag(1f), worst = 0f;
            for (int i = 0; i <= 64; i++)
            {
                float u = i / 64f;
                worst = Mathf.Max(worst, Mathf.Abs(FlameJetCard.Sag(u) - Mathf.Lerp(a, b, u)));
            }
            Assert.Greater(worst, 0.08f, "the spine is a curve, not a line: worst departure from the chord " + worst);
        }

        [Test]
        public void TheEdgeIsNoiseBrokenAtFullBrightness()
        {
            // The same statistic Tools/flameband.py --wiggle takes off a photograph, taken off the shader's own edge
            // function instead: 700 samples along the run (a side shot's length in pixels), a 150-sample window slid
            // along it, a least-squares line fitted inside each window, and the SMALLEST residual RMS of any window.
            // One ruler-straight stretch anywhere fails it, which is exactly the complaint.
            //
            // Seen RED on look-06's edge (rim = 0.40 + 0.58*tear, no cut): 1.17 px at the worst of the six phases
            // below and 1.35 px at the best, against a measured 1.59 px on flame3/after_side.jpg - the picture the
            // master rejected. The mirror and the photograph agree about the old card, which is what makes the
            // threshold worth anything; 2.5 px is flameband's own, so one number judges the shader and the picture.
            TestContext.WriteLine("edge constants: RimBase=" + FlameJetCard.RimBase + " RimTear=" + FlameJetCard.RimTear
                                  + " RimSoft=" + FlameJetCard.RimSoft + " CutMetres=" + FlameJetCard.CutMetres
                                  + " FineA=" + FlameJetCard.FineA + " FineB=" + FlameJetCard.FineB
                                  + " RimPad=" + FlameJetCard.RimPad);
            foreach (float phase in new[] { 0f, 0.37f, 1.3f, 2.9f, 4.1f, 5.5f })
            {
                float wiggle = StraightestWindow(phase, out float thinnest);
                TestContext.WriteLine("phase " + phase + ": wiggle " + wiggle.ToString("F2")
                                      + " px, thinnest edge " + thinnest.ToString("F3") + " of the half width");
                Assert.Greater(wiggle, 2.5f,
                    "the contour is ruler-straight somewhere at phase " + phase + ": the straightest 150 px window "
                    + "has a residual of only " + wiggle.ToString("F2") + " px");
                Assert.Greater(thinnest, 0.01f,
                    "the noise may NOTCH the stream but never sever it: at phase " + phase + " the edge reaches "
                    + thinnest.ToString("F3") + " of the half width, and a flat zero is a straight line again");
            }
        }

        /// <summary>
        /// The smallest straight-line residual RMS, in pixels, of any 150-sample window of FlameJetCard.EdgeV along
        /// the run - and, on the way, how thin that edge ever gets. The pixel scale is the side shot's.
        /// </summary>
        static float StraightestWindow(float phase, out float thinnest)
        {
            const int n = 700, w = 150;
            var px = new float[n];
            thinnest = 9f;
            for (int k = 0; k < n; k++)
            {
                float u = k / (float)(n - 1);
                float e = FlameJetCard.EdgeV(u, 0f, phase, 1f);
                thinnest = Mathf.Min(thinnest, e);
                px[k] = e * FlameJetCard.HalfAt(u) * PxPerMetre;
            }
            float best = float.MaxValue;
            for (int i = 0; i + w <= n; i++)
            {
                double sx = 0, sy = 0, sxx = 0, sxy = 0;
                for (int k = i; k < i + w; k++) { sx += k; sy += px[k]; sxx += (double)k * k; sxy += (double)k * px[k]; }
                double slope = (w * sxy - sx * sy) / (w * sxx - sx * sx);
                double icept = (sy - slope * sx) / w;
                double sum = 0;
                for (int k = i; k < i + w; k++) { double r = px[k] - (slope * k + icept); sum += r * r; }
                best = Mathf.Min(best, (float)System.Math.Sqrt(sum / w));
            }
            return best;
        }

        [Test]
        public void TheCardWidensOnlyBeyondSixtyMetres()
        {
            // look-06 had no FarWiden at all: the symbol does not exist there, so this test does not COMPILE against
            // that card - the same way look-06's own three shape tests did not compile against the chain they
            // replaced. A missing widening at 120 m is what flame3/after_std.jpg has instead of a tongue.
            Assert.AreEqual(1f, FlameJetCard.FarWiden(0f), 1e-4f, "nothing changes at the zooms the owner judged");
            Assert.AreEqual(1f, FlameJetCard.FarWiden(40f), 1e-4f, "nor at 40 m");
            Assert.AreEqual(1f, FlameJetCard.FarWiden(60f), 1e-4f, "nor right up to 60 m");
            Assert.GreaterOrEqual(FlameJetCard.FarWiden(120f), 2.2f, "at 120 m the whole run is sixty pixels");
            Assert.Greater(FlameJetCard.FarWiden(120f), FlameJetCard.FarWiden(90f), "and it grows with the distance");

            Assert.AreEqual(1.55f, FlameJetCard.HalfAt(0f) * 2f, 0.01f, "the mouth is still the owner's 1.55 m");
            Assert.AreEqual(3.90f, FlameJetCard.HalfAt(1f) * 2f, 0.01f, "and the head still his 3.90 m");
        }

        [Test]
        public void TheStreamWavesSidewaysWithoutMovingTheNozzle()
        {
            Assert.AreEqual(0f, FlameJetCard.Waver(0f, 0f, 0f), 1e-4f, "the mouth is bolted to the weapon");
            float worst = 0f;
            for (int i = 0; i <= 64; i++) worst = Mathf.Max(worst, Mathf.Abs(FlameJetCard.Waver(i / 64f, 0f, 0.4f)));
            Assert.Greater(worst, 0.15f, "and the free fuel wanders off the axis by about a third of a metre");
            Assert.Less(worst, 0.6f, "a waver, not a flail");
            Assert.AreNotEqual(FlameJetCard.Waver(0.6f, 0f, 0f), FlameJetCard.Waver(0.6f, 1.7f, 0f),
                               "and it scrolls, so a still frame of two bursts is never the same picture");
        }
    }
}
