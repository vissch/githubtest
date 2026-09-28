// Phase: deaths (2026-09-28, implemented) — the flight of a machine's absurd death (TankRenderer.Deaths), pure and
// tested (VehicleDeathTests): the turret's leap and flips, the hull's hop, a wheel's roll. Intensity is fx.deathAbsurd
// (DeathGags.Intensity): 0 is no gag at all, 1 the new look, 2 ludicrous. Seeded by the caller: every random here is
// a 0..1 the caller drew from DebrisRng.
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public static class VehicleGags
    {
        public const float Gravity = 9.81f;
        /// <summary>The turret's leap at intensity 1 (m/s up), grown with the square root of the intensity and capped.</summary>
        public const float TurretUpMin = 17f, TurretUpMax = 21f, TurretUpCap = 26f;
        /// <summary>Where the turret comes down, in hull lengths from where it left, bounces and all.</summary>
        public const float LandWithin = 1.5f;
        /// <summary>The share of its fall speed the turret keeps on each of its first TurretBounces landings.</summary>
        public const float TurretBounce = 0.5f;
        public const int TurretBounces = 2;
        /// <summary>The hull's hop at intensity 1, and the most it ever hops; the share it keeps on its one bounce.</summary>
        public const float HopMetres = 1f, HopCap = 1.8f, HopBounce = 0.3f;
        /// <summary>A walker holds its death pose this long before it drops.</summary>
        public const float WalkerFreeze = 0.35f;
        /// <summary>How far a wheel rolls (m), how hard the ground slows it (m/s²), how many roll away at most.</summary>
        public const float RollMin = 8f, RollMax = 14f, RollDecel = 2.5f;
        public const int MaxRollers = 4;

        public struct Leap { public Vector3 Vel; public Vector3 Spin; public float Air; public int Flips; }

        /// <summary>The turret's leap: straight up, drifting a little the way `away` points (horizontal) so that it lands,
        /// bounces and all, within LandWithin hull lengths, turning end over end about `axis` (horizontal, unit) in whole
        /// flips over its first flight. r0..r2 are the caller's dice.</summary>
        public static Leap TurretLeap(float intensity, float hullLength, Vector3 away, Vector3 axis, float r0, float r1, float r2)
        {
            float a = Mathf.Max(0f, intensity);
            float up = Mathf.Min(TurretUpCap, Mathf.Lerp(TurretUpMin, TurretUpMax, r0) * Mathf.Sqrt(a));
            float air = 2f * up / Gravity;
            // the first flight carries it drift x air; every bounce after keeps TurretBounce of the lift and 45 % of the
            // drift (TankRenderer.FlyDebris), so the whole way is drift x air x (1 + 0.45 b + (0.45 b)^2)
            float k = 0.45f * TurretBounce, total = 1f + k + k * k;
            float drift = air > 0f ? LandWithin * hullLength * (0.4f + 0.5f * r1) / (air * total) : 0f;
            away.y = 0f;
            Vector3 dir = away.sqrMagnitude > 1e-6f ? away.normalized : Vector3.forward;
            int flips = 1 + Mathf.FloorToInt(r2 * (1f + a));   // one or two at 1, up to three at 2
            float spin = air > 0f ? flips * 2f * Mathf.PI / air : 0f;
            return new Leap { Vel = dir * drift + Vector3.up * up, Spin = axis.normalized * spin, Air = air, Flips = flips };
        }

        /// <summary>The hull's hop: its launch speed at an intensity, and its height t seconds after, with one bounce.</summary>
        public static float HopSpeed(float intensity) => Mathf.Sqrt(2f * Gravity * Mathf.Min(HopCap, HopMetres * Mathf.Max(0f, intensity)));

        public static float Hop(float speed, float t)
        {
            if (speed <= 0f || t <= 0f) return 0f;
            float first = 2f * speed / Gravity;
            if (t < first) return speed * t - 0.5f * Gravity * t * t;
            float second = speed * HopBounce, s = t - first;
            if (s < 2f * second / Gravity) return second * s - 0.5f * Gravity * s * s;
            return 0f;
        }

        /// <summary>How long the hop lasts, bounce and all.</summary>
        public static float HopSeconds(float speed) => speed <= 0f ? 0f : 2f * speed * (1f + HopBounce) / Gravity;

        /// <summary>A wheel's roll: the speed it leaves at to roll `distance` metres against RollDecel, and how long.</summary>
        public static float RollSpeed(float distance) => Mathf.Sqrt(2f * RollDecel * Mathf.Max(0f, distance));
        public static float RollSeconds(float distance) => RollSpeed(distance) / RollDecel;
        public static float RollDistance(float r) => Mathf.Lerp(RollMin, RollMax, r);
    }
}
