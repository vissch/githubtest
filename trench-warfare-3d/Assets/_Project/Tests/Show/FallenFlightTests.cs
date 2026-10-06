// Phase: deaths (2026-09-28, implemented) — the path a gagged body takes (FallenFlight): with one arc and nothing else it
// is the old throw exactly; every landing is continuous; he never goes under the ground he lands on; he comes to rest
// where the plan says, when it says, and stays there; every arc lands on a whole turn; a heap moving his rest re-solves
// the last arc instead of popping him; water stops the bounce; a claw's hold lifts and drops him; the squash code
// always fits its six bits. Pure arithmetic: no scene, no GPU.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Units;

namespace TW.Tests
{
    public class FallenFlightTests
    {
        struct Flat : IGroundHeight { public float Y; public float At(float x, float z) => Y; }
        struct Slope : IGroundHeight { public float At(float x, float z) => 0.05f * x - 0.02f * z; }

        const float G = 14f, NoWater = -10000f;
        static readonly Vector2 Map = new Vector2(600f, 1200f);
        static readonly Vector3 Here = new Vector3(100f, 0f, 200f);

        static FallenFlight.Plan Make(Vector3 fly, int flips = 0, int rolls = 0, int bounces = 0, float skid = 0f, float delay = 0f, float lift = 0f, float water = NoWater)
        {
            var ground = new Flat { Y = 0f };
            return FallenFlight.Make(Here, fly, flips, rolls, bounces, skid, Vector3.zero, delay, lift, 0f, G, Map, water, ref ground);
        }

        [Test]
        public void OneArcAndNothingElseIsTheOldThrow()
        {
            var fly = new Vector3(4f, 2.5f, -3f);
            var p = Make(fly);
            // the old arc (VATRenderer.Fallen): land on the ground less 2 cm, top = the higher end + fly.y, lerp across
            Vector3 land = Here + new Vector3(fly.x, 0f, fly.z); land.y = -0.02f;
            float top = Mathf.Max(Here.y, land.y) + fly.y, up = Mathf.Sqrt(2f * (top - Here.y) / G), down = Mathf.Sqrt(2f * (top - land.y) / G);
            Assert.AreEqual(1, p.Arcs);
            Assert.AreEqual(up + down, p.Arrive, 1e-4f);
            for (int k = 0; k <= 100; k++)
            {
                float age = (up + down) * k / 100f;
                Vector3 old = Vector3.Lerp(Here, land, age / (up + down)); old.y = top - 0.5f * G * (age - up) * (age - up);
                Assert.Less(Vector3.Distance(old, FallenFlight.At(p, age)), 1e-4f, "at " + age);
            }
        }

        [Test]
        public void EveryLandingIsContinuous()
        {
            var p = Make(new Vector3(6f, 4f, 0f), 2, 0, 2, 1.5f);
            Assert.AreEqual(3, p.Arcs);
            float[] joins = { p.A0.End, p.A1.End, p.A2.End, p.SkidStart + p.SkidDur };
            foreach (float j in joins)
            {
                var before = FallenFlight.At(p, j - 1e-4f); var after = FallenFlight.At(p, j + 1e-4f);
                Assert.Less(Vector3.Distance(before, after), 0.01f, "no jump at " + j);
            }
        }

        [Test]
        public void HeNeverGoesUnderTheGroundHeLandsOn()
        {
            var p = Make(new Vector3(-8f, 6f, 5f), 3, 1, 2, 2f);
            for (int k = 0; k <= 400; k++)
            {
                float age = p.Arrive * k / 400f;
                Assert.GreaterOrEqual(FallenFlight.At(p, age).y, -0.02f - 1e-4f, "at " + age);
            }
        }

        [Test]
        public void HeComesToRestWhereThePlanSaysWhenItSaysAndStaysThere()
        {
            var p = Make(new Vector3(5f, 3f, 0f), 1, 0, 2, 1f, 0.3f);
            float arcs = p.A0.Dur + p.A1.Dur + p.A2.Dur;
            Assert.AreEqual(0.3f + arcs + p.SkidDur, p.Arrive, 1e-4f, "the delay, the three arcs and the skid");
            Assert.AreEqual(FallenFlight.SkidBase + FallenFlight.SkidPerMetre * 1f, p.SkidDur, 1e-5f);
            foreach (float after in new[] { 0f, 0.5f, 10f, 30f })
                Assert.Less(Vector3.Distance(p.Rest, FallenFlight.At(p, p.Arrive + after)), 1e-4f);
            Assert.Greater(p.Rest.x, Here.x + 5f, "the bounces and the skid carry him on the way he was thrown");
            Assert.AreEqual(Here, FallenFlight.At(p, 0.1f), "held where he fell until the launch");
        }

        [Test]
        public void EveryArcLandsOnAWholeTurn()
        {
            for (int flips = 1; flips <= 5; flips++)
            {
                var p = Make(new Vector3(3f, 5f, 0f), flips, 3, 2);
                foreach (var a in new[] { p.A0, p.A1 })
                {
                    float nearEnd = p.Delay + a.Start + a.Dur * 0.999f;
                    Assert.AreEqual(0, FallenFlight.PitchStep(p, nearEnd, 0), "pitch level as arc lands (" + flips + " flips)");
                    Assert.AreEqual(0, FallenFlight.RollStep(p, nearEnd), "roll level as arc lands");
                }
                float mid = p.Delay + p.A0.Dur * (0.5f / flips);
                Assert.AreEqual(16, FallenFlight.PitchStep(p, mid, 0), "upside down half way through his first turn");
                Assert.AreEqual(3, FallenFlight.PitchStep(p, p.Arrive + 1f, 3), "and his heap's tilt once he lies");
            }
        }

        [Test]
        public void AHeapMovingHisRestReSolvesTheLastArc()
        {
            var p = Make(new Vector3(4f, 2f, 0f), 1, 0, 1);
            var rest = p.Rest + new Vector3(0.4f, 0.3f, -0.2f);
            float startOfLast = p.A1.Start;
            var atStart = FallenFlight.At(p, startOfLast + 1e-4f);
            FallenFlight.EndAt(ref p, rest);
            Assert.Less(Vector3.Distance(rest, FallenFlight.At(p, p.Arrive)), 1e-3f, "he lands where the heap put him");
            Assert.Less(Vector3.Distance(atStart, FallenFlight.At(p, startOfLast + 1e-4f)), 0.01f, "the arc still leaves from where the first landed");
            Assert.Less(Vector3.Distance(rest, FallenFlight.At(p, p.Arrive + 5f)), 1e-4f);

            var still = Make(Vector3.zero);
            FallenFlight.EndAt(ref still, Here + Vector3.up * 0.28f);
            Assert.AreEqual(Here + Vector3.up * 0.28f, FallenFlight.At(still, 0f), "a body with no path is laid where the heap says at once");
        }

        [Test]
        public void WaterStopsTheBounceAndTheSkid()
        {
            var p = Make(new Vector3(5f, 3f, 0f), 1, 0, 2, 2f, 0f, 0f, water: 0.5f);
            Assert.AreEqual(1, p.Arcs, "he lands in the water and stays");
            Assert.AreEqual(0f, p.SkidDur);
        }

        [Test]
        public void AClawLiftsHimHoldsHimAndLetsHimFall()
        {
            var held = Make(Vector3.zero, delay: 0.3f, lift: 2.2f);
            Assert.AreEqual(2.2f, FallenFlight.At(held, 0.2f).y, 1e-4f, "held up");
            Assert.AreEqual(1, held.Arcs, "let go with no throw, he drops");
            Assert.AreEqual(-0.02f, FallenFlight.At(held, held.Arrive).y, 1e-4f, "to the ground he was lifted from");
            var flung = Make(new Vector3(10f, 6f, 0f), 0, 2, 0, 0f, 0.3f, 2.2f);
            Assert.AreEqual(2.2f, flung.A0.From.y, 1e-4f, "a fling leaves from where the claw held him");
        }

        [Test]
        public void TheMapsEdgeHoldsHim()
        {
            var ground = new Flat { Y = 0f };
            var p = FallenFlight.Make(new Vector3(2f, 0f, 2f), new Vector3(-20f, 5f, -20f), 1, 0, 2, 3f, Vector3.zero, 0f, 0f, 0f, G, Map, NoWater, ref ground);
            Assert.GreaterOrEqual(p.Rest.x, 0.5f); Assert.GreaterOrEqual(p.Rest.z, 0.5f);
        }

        [Test]
        public void HeLandsOnTheGroundWhereHeComesDownNotWhereHeLeft()
        {
            var ground = new Slope();
            var p = FallenFlight.Make(Here, new Vector3(10f, 3f, 0f), 1, 0, 1, 0f, Vector3.zero, 0f, 0f, 0f, G, Map, NoWater, ref ground);
            Assert.AreEqual(ground.At(p.A0.To.x, p.A0.To.z) - 0.02f, p.A0.To.y, 1e-4f);
            Assert.AreEqual(ground.At(p.Rest.x, p.Rest.z) - 0.02f, p.Rest.y, 1e-4f);
        }

        [Test]
        public void TheSquashAlwaysFitsItsSixBits()
        {
            var rng = new System.Random(7);
            for (int n = 0; n < 200; n++)
            {
                var fly = new Vector3((float)rng.NextDouble() * 30f - 15f, (float)rng.NextDouble() * 20f, (float)rng.NextDouble() * 30f - 15f);
                var p = Make(fly, rng.Next(0, 6), rng.Next(0, 5), rng.Next(0, 3), (float)rng.NextDouble() * 4f, (float)rng.NextDouble() * 0.6f);
                p.PulseQ = (sbyte)rng.Next(-32, 32); p.PulseDur = 0.2f; p.RestQ = (sbyte)rng.Next(-32, 32);
                for (int k = 0; k <= 50; k++)
                {
                    int q = FallenFlight.Squash(p, (p.Arrive + 1f) * k / 50f, 2f);
                    Assert.GreaterOrEqual(q, VatTint.SquashMin); Assert.LessOrEqual(q, VatTint.SquashMax);
                }
            }
        }

        [Test]
        public void ALandingSquashesHimAndHeSpringsBack()
        {
            var p = Make(new Vector3(0f, 4f, 0f));
            Assert.Less(FallenFlight.Squash(p, p.A0.End + 0.001f, 1f), 0, "flattened as he lands");
            Assert.AreEqual(0, FallenFlight.Squash(p, p.A0.End + FallenFlight.WobbleSeconds + 0.01f, 1f), "and himself again after the wobble");
            Assert.Greater(FallenFlight.Squash(p, p.A0.Up * 0.2f, 1f), 0, "stretched on the way up");
        }
    }
}
