// Phase: A5c (2026-09-29, the Dust Front lessons: vehicle weight) — depends on: TankSpec (TankGun).
// The pure maths of a machine's weight, kept out of TankRenderer so tests can hold it:
//  - the exact spring solve, critically damped and under-damped: a long frame cannot blow it up (the stepped Euler it
//    replaces was stable only while its pieces stayed small, and an explicit Euler spring once drew a Maw 3 km up);
//  - the felt acceleration: the sim's speed, sampled once a tick, is chased by a critically damped follower whose own
//    velocity is what the ride answers, so a presenter hitch (the drawn hull freezes, then jumps) does not spike it;
//  - a gun's shot weight (the Maw's six-pounder is 1) and what it does to the barrel's recoil and to the hull;
//  - a walker's kick layer: kicks are added on top of the gait's tilt (which is not sprung twice) and die away;
//  - the settle of a slow turret coming to rest.
using UnityEngine;
using TW.Sim.Combat;

namespace TW.Presentation.Tactical
{
    public static class HullRide
    {
        /// <summary>Exact solve of x'' = -2 zeta omega x' - omega^2 (x - target) over h seconds. zeta at or above 1 is
        /// solved as critically damped (no ride here is over-damped).</summary>
        public static void Solve(ref float value, ref float velocity, float target, float h, float omega, float zeta)
        {
            if (h <= 0f || omega <= 0f) return;
            float x = value - target, v = velocity;
            if (zeta >= 0.999f)
            {
                float e = Mathf.Exp(-omega * h), j = v + omega * x;
                value = target + (x + j * h) * e;
                velocity = (v - omega * j * h) * e;
                return;
            }
            float wd = omega * Mathf.Sqrt(1f - zeta * zeta), ex = Mathf.Exp(-zeta * omega * h);
            float c = Mathf.Cos(wd * h), s = Mathf.Sin(wd * h);
            value = target + ex * (x * c + (v + zeta * omega * x) * s / wd);
            velocity = ex * (v * c - (omega * omega * x + zeta * omega * v) * s / wd);
        }

        /// <summary>The first peak of a spring at rest after a velocity kick of 1: a kick of v peaks at v times this.</summary>
        public static float PeakPerKick(float omega, float zeta)
        {
            if (zeta >= 0.999f) return 1f / (omega * 2.7182818f);
            float wd = omega * Mathf.Sqrt(1f - zeta * zeta), t = Mathf.Atan2(wd, zeta * omega) / wd;
            return Mathf.Exp(-zeta * omega * t) * Mathf.Sin(wd * t) / wd;
        }

        /// <summary>The kick (rad/s) that tips a spring of (omega, zeta) by <paramref name="degrees"/> at its first peak.</summary>
        public static float KickFor(float degrees, float omega, float zeta)
            => degrees * Mathf.Deg2Rad / Mathf.Max(1e-5f, PeakPerKick(omega, zeta));

        // ---- the felt acceleration ----

        /// <summary>How quickly the felt speed follows the sim's (rad/s): the ride's own stiffness, kept in a band a
        /// 20 Hz step cannot chatter through ((omega / 63)^2 of tick-rate noise gets through).</summary>
        public static float FollowOmega(float rideOmega) => Mathf.Clamp(rideOmega, 2f, 7f);

        /// <summary>One frame of the follower: the felt speed chases the sim's speed; the returned value (its velocity)
        /// is the felt acceleration in m/s^2 of sim time. h is the frame in sim time (dt times the time scale).</summary>
        public static float Follow(ref float felt, ref float feltRate, float simSpeed, float h, float omega)
        {
            Solve(ref felt, ref feltRate, simSpeed, h, omega, 1f);
            return feltRate;
        }

        // ---- a gun's shot ----

        /// <summary>How heavy a gun's shot is, against the Maw's six-pounder (1): armour-piercing damage for a gun laid
        /// on its mark, the high explosive for a lobbed one. A rack's rockets kick one at a time and light.</summary>
        public static float ShotWeight(in TankGun g, bool rack)
        {
            if (rack) return 0.4f;
            float w = g.Indirect ? g.HeDamage / 220f : g.ApDamage / 650f;
            return Mathf.Clamp(w, 0.3f, 2f);
        }

        /// <summary>How far back the barrel goes, as a share of its part's travel (0.45 m a gun, 0.30 m a sponson).</summary>
        public static float RecoilScale(float weight) => 0.6f + 0.4f * weight;

        /// <summary>How long a barrel takes to run back into battery (today every gun takes 0.385 s).</summary>
        public static float ReturnSeconds(float weight) => 0.25f + 0.4f * weight;

        public const float RecoilSnapSeconds = 0.035f;

        /// <summary>The recoil's shape: r runs from 1 (the shot) to 0 over the return. A snap back in 35 ms whatever the
        /// gun, then a smooth ease into battery (written out by hand: Mathf.SmoothStep is not GLSL's smoothstep).</summary>
        public static float Kick(float r, float returnSeconds)
        {
            if (r <= 0f) return 0f;
            float snap = Mathf.Sin(Mathf.Clamp01((1f - r) * returnSeconds / RecoilSnapSeconds) * Mathf.PI * 0.5f);
            return snap * r * r * (3f - 2f * r);
        }

        // ---- a walker's kick layer ----

        /// <summary>The share of a kick a walker takes into its layer, its spring, and how far it may lean on it.</summary>
        public const float WalkerKickShare = 0.6f, KickOmega = 6f, KickZeta = 0.5f, WalkerKickMaxDeg = 2.5f;

        /// <summary>How far a walker's kick layer may tip it: 2.5 degrees, or half of what its legs can take.</summary>
        public static float KickCap(float gaitCap) => Mathf.Min(WalkerKickMaxDeg * Mathf.Deg2Rad, 0.5f * Mathf.Max(0f, gaitCap));

        /// <summary>One frame of the layer: it springs back to level; clamped to its cap.</summary>
        public static void StepKick(ref float value, ref float velocity, float dt, float cap)
        {
            Solve(ref value, ref velocity, 0f, dt, KickOmega, KickZeta);
            if (value > cap) { value = cap; if (velocity > 0f) velocity = 0f; }
            else if (value < -cap) { value = -cap; if (velocity < 0f) velocity = 0f; }
        }

        // ---- a turret coming to rest ----

        public const float SettleOmega = 14f, SettleZeta = 0.35f, SettleAboveDegPerSec = 40f;

        /// <summary>How far past its mark a turret swings when it stops (degrees): a slow, heavy one a little, never more
        /// than the knob allows, and nothing for one quicker than SettleAboveDegPerSec.</summary>
        public static float SettleDegrees(float traverseDegPerSec, float maxDegrees)
            => traverseDegPerSec > SettleAboveDegPerSec ? 0f : Mathf.Min(maxDegrees, 0.012f * traverseDegPerSec);
    }
}
