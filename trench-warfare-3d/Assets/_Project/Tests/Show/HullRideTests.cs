// Phase: A5c (2026-09-29, the Dust Front lessons: vehicle weight) — HullRide, the pure maths of a machine's weight.
// Where the weight layer replaces a way of doing it, the test measures the old way too and shows it failing: the
// explicit spring blowing up on a long frame, the frame-differenced acceleration spiking on a presenter's stall.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;
using TW.Sim;
using TW.Sim.Combat;

namespace TW.Tests
{
    public class HullRideTests
    {
        [Test]
        public void TheSpringIsExact_TenShortStepsLandWhereOneLongStepDoes()
        {
            foreach (var (omega, zeta) in new[] { (4.5f, 0.8f), (7f, 1f), (6f, 0.5f), (14f, 0.35f), (3f, 0.3f) })
            {
                float a = 0.3f, av = -1.2f, b = a, bv = av;
                for (int k = 0; k < 10; k++) HullRide.Solve(ref a, ref av, 0.05f, 0.04f, omega, zeta);
                HullRide.Solve(ref b, ref bv, 0.05f, 0.4f, omega, zeta);
                Assert.AreEqual(b, a, 1e-4f, $"value, omega {omega} zeta {zeta}");
                Assert.AreEqual(bv, av, 1e-3f, $"velocity, omega {omega} zeta {zeta}");
            }
        }

        [Test]
        public void ALongFrameCannotBlowTheRideUp_WhereAnExplicitStepDoes()
        {
            float x = 0f, v = 1f, ex = 0f, ev = 1f;
            const float omega = 7f, zeta = 0.8f, h = 0.34f;   // an editor stall: omega h = 2.4, past explicit Euler's limit
            for (int k = 0; k < 20; k++)
            {
                HullRide.Solve(ref x, ref v, 0f, h, omega, zeta);
                ev += (-2f * zeta * omega * ev - omega * omega * ex) * h; ex += ev * h;   // the step it replaces, unsplit
            }
            Assert.Less(Mathf.Abs(x), 1e-3f, "the exact solve has settled");
            Assert.IsFalse(float.IsNaN(x) || float.IsInfinity(x));
            Assert.Greater(Mathf.Abs(ex), 1f, "the explicit step has blown up: this is what the exact solve is for");
        }

        [Test]
        public void TheFeltAccelerationDoesNotChatter_WhereTheFrameDifferencedOneSpikes()
        {
            // a machine at a steady 1.6 m/s: the sim's speed is sampled at 20 Hz with 8 % noise; frames come every 11-25 ms
            // and one frame in twenty the presenter stalls (the drawn hull does not move, then catches up next frame)
            var rng = new System.Random(7);
            float felt = 1.6f, feltRate = 0f, drawnSpeed = 1.6f, oldAccel = 0f;
            float t = 0f, nextTick = 0f, sim = 1.6f;
            double sumNew = 0, sumNew2 = 0, sumOld = 0, sumOld2 = 0; int n = 0;
            bool owed = false;
            for (int f = 0; f < 4000; f++)
            {
                float dt = 1f / 90f + (float)rng.NextDouble() * (1f / 40f - 1f / 90f);
                t += dt;
                if (t >= nextTick) { nextTick += 0.05f; sim = 1.6f * (1f + 0.08f * (float)(rng.NextDouble() * 2 - 1)); }
                bool stall = rng.NextDouble() < 0.05;
                float accel = HullRide.Follow(ref felt, ref feltRate, sim, dt, HullRide.FollowOmega(4.5f));
                // the old way: a speed off the drawn position, smoothed at 10, and its difference smoothed at 6
                float frameSpeed = stall ? 0f : owed ? 2f * sim : sim;
                owed = stall;
                float was = drawnSpeed;
                drawnSpeed = Mathf.Lerp(drawnSpeed, frameSpeed, 1f - Mathf.Exp(-dt * 10f));
                oldAccel = Mathf.Lerp(oldAccel, (drawnSpeed - was) / dt, 1f - Mathf.Exp(-dt * 6f));
                if (f < 400) continue;
                sumNew += accel; sumNew2 += accel * accel; sumOld += oldAccel; sumOld2 += oldAccel * oldAccel; n++;
            }
            double sdNew = System.Math.Sqrt(sumNew2 / n - (sumNew / n) * (sumNew / n));
            double sdOld = System.Math.Sqrt(sumOld2 / n - (sumOld / n) * (sumOld / n));
            Assert.Less(sdNew, 0.25, $"a steady machine feels steady (sd {sdNew:F3} m/s^2)");
            Assert.Greater(sdOld, 3 * sdNew, $"the frame-differenced acceleration it replaces spikes on the stalls (sd {sdOld:F3})");
        }

        [Test]
        public void BrakingIsFeltAsBraking_AndStartingAsStarting()
        {
            // a Maw braking from 1.6 m/s at its 1.2 m/s^2, then pulling away at its 0.5 m/s^2 (VehicleProfile.Maw)
            float felt = 1.6f, rate = 0f, sim = 1.6f, least = 0f, most = 0f;
            for (int f = 0; f < 600; f++)
            {
                float dt = 1f / 60f, time = f * dt;
                sim = time < 3f ? Mathf.Max(0f, 1.6f - 1.2f * time) : Mathf.Min(1.6f, 0.5f * (time - 3f));
                float a = HullRide.Follow(ref felt, ref rate, sim, dt, HullRide.FollowOmega(4.5f));
                if (time < 3f) least = Mathf.Min(least, a); else most = Mathf.Max(most, a);
            }
            Assert.Less(least, -0.9f, "braking at 1.2 m/s^2 is felt as nearly that (the squat tips the nose down)");
            Assert.Greater(most, 0.35f, "pulling away at 0.5 m/s^2 is felt too");
        }

        [Test]
        public void TheShotsAreWeighedByTheirGuns()
        {
            float tusk = HullRide.ShotWeight(TankSpec.For(VehicleArchetype.Tusk).Gun(0), false);
            float maw = HullRide.ShotWeight(TankSpec.For(VehicleArchetype.Maw).Gun(0), false);
            float kettle = HullRide.ShotWeight(TankSpec.For(VehicleArchetype.Kettle).Gun(0), false);
            Assert.AreEqual(1f, maw, 0.05f, "the Maw's six-pounder is the unit");
            Assert.Less(tusk, maw, "the Tusk's 37 mm is lighter");
            Assert.Greater(kettle, maw, "the Kettle's mortar bomb is heavier");
            Assert.AreEqual(0.4f, HullRide.ShotWeight(default, true), "a rack's rockets kick one at a time");
        }

        [Test]
        public void AHeavierShotGoesFurtherBackAndTakesLongerToRunHome()
        {
            Assert.Greater(HullRide.RecoilScale(1.45f), HullRide.RecoilScale(1f));
            Assert.Greater(HullRide.RecoilScale(1f), HullRide.RecoilScale(0.65f));
            Assert.Greater(HullRide.ReturnSeconds(1f), HullRide.ReturnSeconds(0.65f));
            Assert.Greater(HullRide.ReturnSeconds(0.65f), 0.385f, "even the light gun returns slower than today's 0.385 s for all");
            float T = HullRide.ReturnSeconds(1f);
            Assert.AreEqual(0f, HullRide.Kick(1f, T), 1e-6f, "nothing yet on the frame of the shot");
            float snapped = HullRide.Kick(1f - HullRide.RecoilSnapSeconds / T, T);
            Assert.Greater(snapped, 0.9f, "back in 35 ms");
            float prev = snapped;
            for (float r = 1f - HullRide.RecoilSnapSeconds / T - 0.02f; r > 0f; r -= 0.02f)
            {
                float k = HullRide.Kick(r, T);
                Assert.LessOrEqual(k, prev + 1e-5f, "then home without bouncing");
                prev = k;
            }
            Assert.AreEqual(0f, HullRide.Kick(0f, T), 1e-6f);
        }

        [Test]
        public void AKickTipsTheHullAsFarAsAsked()
        {
            foreach (var (omega, zeta) in new[] { (4.5f, 0.8f), (7f, 1f), (6f, 0.5f), (14f, 0.35f) })
            {
                float x = 0f, v = HullRide.KickFor(2f, omega, zeta), peak = 0f;
                for (int k = 0; k < 2400; k++) { HullRide.Solve(ref x, ref v, 0f, 1f / 1200f, omega, zeta); peak = Mathf.Max(peak, x); }
                Assert.AreEqual(2f, peak * Mathf.Rad2Deg, 0.04f, $"omega {omega} zeta {zeta}");
            }
        }

        [Test]
        public void AWalkerKeepsItsKick_WhereItsGaitUsedToWipeIt()
        {
            // a shot's kick on a walker (0.35 rad/s): today its tilt is set straight off its feet and the kick is gone
            float cap = HullRide.KickCap(0.42f), value = 0f, velocity = 0.35f * HullRide.WalkerKickShare, peak = 0f;
            for (int f = 0; f < 9; f++) { HullRide.StepKick(ref value, ref velocity, 1f / 60f, cap); peak = Mathf.Max(peak, Mathf.Abs(value)); }
            Assert.Greater(peak * Mathf.Rad2Deg, 0.4f, "it rocks within 0.15 s");
            for (int f = 0; f < 180; f++) HullRide.StepKick(ref value, ref velocity, 1f / 60f, cap);
            Assert.Less(Mathf.Abs(value) * Mathf.Rad2Deg, 0.05f, "and is level again three seconds on");
            value = 0f; velocity = 50f;
            for (int f = 0; f < 60; f++) { HullRide.StepKick(ref value, ref velocity, 1f / 60f, cap); Assert.LessOrEqual(Mathf.Abs(value), cap + 1e-6f, "never past half of what its legs can take"); }
            Assert.LessOrEqual(HullRide.KickCap(10f), HullRide.WalkerKickMaxDeg * Mathf.Deg2Rad + 1e-6f);
        }

        [Test]
        public void OnlyASlowTurretSettles_AndNeverPastTheKnob()
        {
            Assert.AreEqual(0f, HullRide.SettleDegrees(50f, 0.6f), "the Tusk's 50 deg/s turret stops dead");
            Assert.AreEqual(0.012f * 35f, HullRide.SettleDegrees(35f, 0.6f), 1e-6f, "the Maw's 35 deg/s sponsons swing a little past");
            Assert.AreEqual(0.1f, HullRide.SettleDegrees(35f, 0.1f), 1e-6f, "never past the knob");
        }

        [Test]
        public void ThePlainRideIsTheOneEveryMachineRodeBefore()
        {
            // with tank.weight off every machine rides PlainStyle: it has to be the ride of the integration branch
            var p = TankRenderer.PlainStyle;
            Assert.AreEqual(7f, p.Omega); Assert.AreEqual(1f, p.Zeta); Assert.AreEqual(10f, p.HeaveOmega);
            Assert.AreEqual(0.006f, p.RumbleAmp); Assert.AreEqual(0.01f, p.RumbleThrottle); Assert.AreEqual(41f, p.RumbleRate);
            Assert.AreEqual(0f, p.Squat); Assert.AreEqual(0f, p.Lean); Assert.AreEqual(0f, p.Clatter); Assert.AreEqual(0f, p.Chug);
            Assert.AreEqual(0f, p.Rev); Assert.AreEqual(0f, p.Stomp); Assert.AreEqual(1f, p.Puff); Assert.AreEqual(1f, p.Swing); Assert.AreEqual(1f, p.Arc);
        }
    }
}
