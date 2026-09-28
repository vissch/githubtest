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
                        if (a == 1f) Assert.That(leap.Vel.y, Is.InRange(VehicleGags.TurretUpMin, VehicleGags.TurretUpMax), "17-21 m/s at 1");
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
    }
}
