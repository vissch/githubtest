// Phase: C1 (unit look, look-05, 2026-10-06) — who threw the streak.
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

        // The SHAPE of the envelope. FlipbookFx.Add early-returns unless the pack is loaded, which EditMode cannot
        // make true, so what is testable here is the geometry the chain is laid on - Flamethrower.Link and Span -
        // and that is where the master's complaint lives: "no shaped stream from muzzle to target".

        [Test]
        public void TheEnvelopeWidensFromMouthToHead()
        {
            const int links = 4;
            float last = -1f, mouth = 0f, head = 0f;
            for (int i = 0; i < links; i++)
            {
                Flamethrower.Link(i, links, 11f, out float u, out float along, out float thick);
                Assert.AreEqual(i / (links - 1f), u, 1e-4f, "u runs 0 at the mouth to 1 at the head");
                Assert.Greater(thick, last, "link " + i + " must be no thinner than the one behind it: fuel spreads as it burns");
                last = thick;
                if (i == 0) mouth = thick;
                if (i == links - 1) head = thick;
                Assert.Greater(along, 0f, "every link sits downrange of the mouth");
            }
            float ratio = head / mouth;
            // The band the file has argued itself to over three rounds: under 2 the stream is a pipe and reads as a
            // glow round the man, over 3.5 the head swallows the run and it reads as a tadpole.
            Assert.That(ratio, Is.InRange(2.0f, 3.5f), "head/mouth was " + ratio);
        }

        [Test]
        public void NeighbouringLinksOverlapAndTheLastOneLandsOnTheTarget()
        {
            const int links = 4;
            const float len = 11f;
            for (int i = 0; i < links - 1; i++)
            {
                Flamethrower.Link(i, links, len, out _, out float a0, out _);
                Flamethrower.Link(i + 1, links, len, out _, out float a1, out _);
                float reach = (Flamethrower.Span(i, links, len) + Flamethrower.Span(i + 1, links, len)) * 0.5f;
                Assert.Less(a1 - a0, reach, "links " + i + " and " + (i + 1) + " must fuse: a gap is a waist, and a waist is a string of beads");
            }
            Flamethrower.Link(links - 1, links, len, out _, out float end, out _);
            Assert.That(end, Is.InRange(len * 0.85f, len * 1.05f), "the head lands on the target, not short of it: " + end);
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
