// Phase: deaths (2026-09-28, implemented) — a machine's absurd death (VehicleGags, drawn by TankRenderer.Deaths): at
// fx.deathAbsurd 0 nothing leaps or hops; the turret goes straight up at 17-21 m/s at 1 and comes down, bounces and
// all, within 1.5 hull lengths, turning in whole flips; at 2 it goes higher, never past the cap; the hull hops a metre
// and comes back down; a wheel rolls 8-14 m. The flight is simulated with TankRenderer.FlyDebris' own rules.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class VehicleDeathTests
    {
        /// <summary>A piece flown as FlyDebris flies it: gravity, and on each landing 45 % of its drift and a share of its
        /// fall (the leap's own for its first bounces, then 0.25), at rest below 0.6 m/s. Returns how far it went.</summary>
        static float Fly(Vector3 vel, float height, int bounces, float bounce)
        {
            Vector3 pos = new Vector3(0f, height, 0f);
            const float dt = 1f / 120f;
            for (int step = 0; step < 120 * 60; step++)
            {
                vel += Vector3.down * VehicleGags.Gravity * dt;
                pos += vel * dt;
                if (pos.y < 0f)
                {
                    pos.y = 0f;
                    float keep = bounces > 0 ? bounce : 0.25f; bounces--;
                    vel = new Vector3(vel.x * 0.45f, -vel.y * keep, vel.z * 0.45f);
                    if (vel.magnitude < 0.6f) break;
                }
            }
            return new Vector2(pos.x, pos.z).magnitude;
        }

        [Test]
        public void AtZeroNothingLeapsOrHops()
        {
            var leap = VehicleGags.TurretLeap(0f, 6f, Vector3.forward, Vector3.right, 0.5f, 0.5f, 0.5f);
            Assert.AreEqual(0f, leap.Vel.y); Assert.AreEqual(0f, leap.Air);
            Assert.AreEqual(0f, VehicleGags.HopSpeed(0f));
            Assert.AreEqual(0f, VehicleGags.Hop(VehicleGags.HopSpeed(0f), 0.4f));
            Assert.AreEqual(0f, VehicleGags.HopSeconds(0f));
        }

        [Test]
        public void TheTurretGoesStraightUpAndLandsWithinOneAndAHalfHullLengths()
        {
            foreach (float hull in new[] { 4f, 6.6f })
                foreach (float a in new[] { 1f, 2f })
                    for (float r = 0f; r <= 1f; r += 0.25f)
                    {
                        var leap = VehicleGags.TurretLeap(a, hull, new Vector3(0.6f, 0f, 0.8f), Vector3.right, r, r, r);
                        if (a == 1f) Assert.That(leap.Vel.y, Is.InRange(VehicleGags.TurretUpMin, VehicleGags.TurretUpMax), "15-18 m/s at 1");
                        Assert.LessOrEqual(leap.Vel.y, VehicleGags.TurretUpCap, "never past the cap");
                        Assert.Less(new Vector2(leap.Vel.x, leap.Vel.z).magnitude, leap.Vel.y * 0.25f, "straight up, drifting a little");
                        float went = Fly(leap.Vel, 3f, VehicleGags.TurretBounces, VehicleGags.TurretBounce);
                        Assert.LessOrEqual(went, VehicleGags.LandWithin * hull, $"hull {hull}, intensity {a}, dice {r}: it came down {went:0.0} m away");
                    }
        }

        [Test]
        public void ItTurnsInWholeFlipsAndMoreOfThemWhenLudicrous()
        {
            var one = VehicleGags.TurretLeap(1f, 6f, Vector3.forward, Vector3.right, 0.5f, 0.5f, 0.99f);
            Assert.AreEqual(one.Flips * 2f * Mathf.PI, one.Spin.magnitude * one.Air, 1e-3f, "whole flips over its flight");
            Assert.AreEqual(1f, Mathf.Abs(Vector3.Dot(one.Spin.normalized, Vector3.right)), 1e-5f, "end over end about the hull's side");
            Assert.That(one.Flips, Is.InRange(1, 2));
            var two = VehicleGags.TurretLeap(2f, 6f, Vector3.forward, Vector3.right, 0.5f, 0.5f, 0.99f);
            Assert.Greater(two.Vel.y, one.Vel.y, "ludicrous goes higher");
            Assert.GreaterOrEqual(two.Flips, one.Flips);
        }

        [Test]
        public void TheHullHopsAMetreAndComesBackDown()
        {
            float v = VehicleGags.HopSpeed(1f);
            float top = 0f;
            for (float t = 0f; t < VehicleGags.HopSeconds(v); t += 0.005f) { float h = VehicleGags.Hop(v, t); Assert.GreaterOrEqual(h, -1e-4f); top = Mathf.Max(top, h); }
            Assert.AreEqual(VehicleGags.HopMetres, top, 0.02f, "a metre at 1");
            Assert.AreEqual(0f, VehicleGags.Hop(v, VehicleGags.HopSeconds(v) + 0.01f), "and down again");
            float ludicrous = 0f;
            for (float t = 0f; t < 3f; t += 0.005f) ludicrous = Mathf.Max(ludicrous, VehicleGags.Hop(VehicleGags.HopSpeed(5f), t));
            Assert.LessOrEqual(ludicrous, VehicleGags.HopCap + 0.02f, "never past the cap");
        }

        [Test]
        public void AWheelRollsEightToFourteenMetres()
        {
            foreach (float r in new[] { 0f, 0.5f, 1f })
            {
                float d = VehicleGags.RollDistance(r);
                Assert.That(d, Is.InRange(VehicleGags.RollMin, VehicleGags.RollMax));
                float speed = VehicleGags.RollSpeed(d), went = 0f;
                const float dt = 1f / 120f;
                while (speed > 0.5f) { went += speed * dt; speed = Mathf.Max(0f, speed - VehicleGags.RollDecel * dt); }
                Assert.AreEqual(d, went, 0.2f, "it rolls as far as it was sent (to within the last half metre a second)");
                Assert.Greater(VehicleGags.RollSeconds(d), 0f);
            }
        }

        [Test]
        public void AHoverMachineDropsOntoItsSkirtAndAWalkerOntoItsBelly()
        {
            float up = VehicleGags.HopSpeed(1f);
            for (float t = 0f; t < 2f; t += 0.01f)
                Assert.AreEqual(VehicleGags.Hop(up, t), VehicleGags.Drop(0f, 0f, up, t), 1e-4f, "on level ground the drop is the hop");
            foreach (var (from, to, u) in new[] { (0.35f, 0f, up), (2.4f, 0.9f, VehicleGags.FlopUp) })
            {
                Assert.AreEqual(from, VehicleGags.Drop(from, to, u, 0f), 1e-5f, "it leaves from where it stood");
                float first = VehicleGags.DropFirst(from, to, u), all = VehicleGags.DropSeconds(from, to, u);
                Assert.Greater(first, 0f); Assert.Greater(all, first, "and bounces once");
                Assert.AreEqual(to, VehicleGags.Drop(from, to, u, first), 1e-3f, "its first landing is on its skirt, its belly");
                for (float t = 0f; t < all + 0.5f; t += 0.005f)
                {
                    float h = VehicleGags.Drop(from, to, u, t);
                    Assert.GreaterOrEqual(h, to - 1e-4f, "never through the ground");
                    if (t > first) Assert.LessOrEqual(h, to + (u * u + 2f * VehicleGags.Gravity * (from - to)) * VehicleGags.HopBounce * VehicleGags.HopBounce / (2f * VehicleGags.Gravity) + 1e-3f, "the bounce is a small one");
                }
                Assert.AreEqual(to, VehicleGags.Drop(from, to, u, all + 0.01f), "and there it lies");
            }
        }

        [Test]
        public void AWalkersLegsSplayFlatInAFifthOfASecond()
        {
            Assert.AreEqual(0f, VehicleGags.Splay(0f)); Assert.AreEqual(0f, VehicleGags.Splay(-1f));
            Assert.AreEqual(1f, VehicleGags.Splay(VehicleGags.SplaySeconds), 1e-5f);
            float was = 0f;
            for (float t = 0f; t < 0.5f; t += 0.01f) { float s = VehicleGags.Splay(t); Assert.GreaterOrEqual(s, was); was = s; }
            Assert.LessOrEqual(VehicleGags.SplaySeconds, 0.2f);
        }

        /// <summary>A fan glided as GlideFan glides it, from 2 m up until it meets level ground: how far, how long.</summary>
        static (float far, float air) Glide(VehicleGags.Glide g)
        {
            Vector3 pos = new Vector3(0f, 2f, 0f), vel = g.Vel;
            const float dt = 1f / 120f;
            float t = 0f;
            while (pos.y > 0f && t < VehicleGags.FanGlideCap) { VehicleGags.GlideStep(ref pos, ref vel, g.Curve, dt); t += dt; }
            return (new Vector2(pos.x, pos.z).magnitude, t);
        }

        [Test]
        public void TheFanGlidesEighteenToTwentySevenMetres()
        {
            var none = VehicleGags.FanThrow(0f, Vector3.back, 0.5f, 0.5f, 0.5f);
            Assert.AreEqual(Vector3.zero, none.Vel, "at 0 nothing is thrown");
            foreach (float r in new[] { 0f, 0.5f, 1f })
            {
                var g = VehicleGags.FanThrow(1f, Vector3.back, r, r, r);
                Assert.Greater(Vector3.Dot(g.Vel, Vector3.back), 0f, "astern");
                var (far, air) = Glide(g);
                Assert.That(far, Is.InRange(16f, 29f), $"dice {r}: it came down {far:0.0} m off");
                Assert.Less(air, VehicleGags.FanGlideCap, "down before its glide runs out");
                Assert.Greater(air, 2f, "a glide, not a throw");
                var ludicrous = Glide(VehicleGags.FanThrow(2f, Vector3.back, r, r, r));
                Assert.Greater(ludicrous.far, far, "further when ludicrous");
                Assert.LessOrEqual(VehicleGags.FanThrow(9f, Vector3.back, r, r, r).Vel.y, VehicleGags.FanUpCap + 1e-4f, "never past the cap");
            }
        }

        [Test]
        public void AFizzerStaysByItsMachineAndPops()
        {
            Assert.AreEqual(0, VehicleGags.FizzCount(0f, 16, 0.5f), "at 0 nothing fizzes");
            Assert.AreEqual(0, VehicleGags.FizzCount(1f, 0, 0.5f), "nor off a machine with no tubes");
            for (float r = 0f; r <= 1f; r += 0.25f)
            {
                Assert.That(VehicleGags.FizzCount(1f, 16, r), Is.InRange(VehicleGags.FizzMin, VehicleGags.FizzMax));
                Assert.LessOrEqual(VehicleGags.FizzCount(2f, 16, r), VehicleGags.FizzCap);
                Assert.LessOrEqual(VehicleGags.FizzCount(2f, 3, r), 3, "never more than its tubes");
            }
            var rng = new System.Random(7);
            const float dt = 1f / 60f;
            for (int k = 0; k < 400; k++)
            {
                var dir = new Vector3((float)rng.NextDouble() - 0.5f, 0.4f + (float)rng.NextDouble(), (float)rng.NextDouble() - 0.5f);
                var f = VehicleGags.Fizz(dir, (float)rng.NextDouble(), (float)rng.NextDouble(), (float)rng.NextDouble(), (float)rng.NextDouble(), (float)rng.NextDouble());
                Assert.That(f.Life, Is.InRange(VehicleGags.FizzLifeMin, VehicleGags.FizzLifeMax));
                Assert.That(f.Delay, Is.InRange(0f, VehicleGags.FizzLaunchSpread));
                Vector3 home = new Vector3(0f, 3f, 0f), pos = home, vel = f.Vel, axis = f.Axis;
                for (float t = 0f; t < f.Life; t += dt)
                {
                    bool near = VehicleGags.FizzStep(ref pos, ref vel, ref axis, f.Turn, f.Wander, dt, 0f, home);
                    Assert.GreaterOrEqual(pos.y, 0f, "it skips off the ground, never into it");
                    Vector3 off = pos - home; off.y = 0f;
                    Assert.LessOrEqual(off.magnitude, VehicleGags.FizzReach + VehicleGags.FizzSpeed * 2f * dt, "it pops before it strays from its machine");
                    if (!near) break;
                    Assert.That(vel.magnitude, Is.InRange(VehicleGags.FizzSpeed * 0.5f, VehicleGags.FizzSpeed * 1.5f), "its motor holds its speed");
                }
            }
        }

        [Test]
        public void AFizzerIsDrawnOnACorkscrew()
        {
            foreach (var way in new[] { Vector3.up, new Vector3(0.6f, 0.2f, 0.77f).normalized, Vector3.right })
                for (float t = 0f; t < 1f; t += 0.05f)
                {
                    var off = VehicleGags.Helix(way, t, 0.7f, out var turning);
                    Assert.AreEqual(VehicleGags.FizzHelixRadius, off.magnitude, 1e-3f, "always the radius off its path");
                    Assert.AreEqual(0f, Vector3.Dot(off, way), 1e-3f, "square to it");
                    Assert.AreEqual(0f, Vector3.Dot(off, turning), 1e-2f, "and going round it");
                }
            var a = VehicleGags.Helix(Vector3.up, 0f, 0f, out _);
            var b = VehicleGags.Helix(Vector3.up, 2f * Mathf.PI / VehicleGags.FizzHelixSpin, 0f, out _);
            Assert.Less((a - b).magnitude, 1e-3f, "a whole loop in 2 pi / spin seconds");
            Assert.GreaterOrEqual(VehicleGags.FizzHelixSpin / (2f * Mathf.PI), 2f, "two loops a second at least");
        }

        [Test]
        public void ATrackPaysOutInOneAndAHalfSeconds()
        {
            Assert.AreEqual(0f, VehicleGags.Unspool(0f)); Assert.AreEqual(1f, VehicleGags.Unspool(VehicleGags.UnspoolSeconds), 1e-5f);
            Assert.AreEqual(1f, VehicleGags.Unspool(9f), "and there it lies");
            float was = 0f;
            for (float t = 0f; t < 2f; t += 0.02f) { float k = VehicleGags.Unspool(t); Assert.GreaterOrEqual(k, was); was = k; }
            Assert.Less(VehicleGags.UnspoolFlat, 0.25f, "laid flat"); Assert.Greater(VehicleGags.UnspoolStretch, 1.8f, "and long");
        }
    }
}
