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
    }
}
