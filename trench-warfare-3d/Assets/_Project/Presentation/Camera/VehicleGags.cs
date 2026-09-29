// Phase: deaths (2026-09-28, implemented) — the flight of a machine's absurd death (TankRenderer.Deaths), pure and
// tested (VehicleDeathTests): the turret's leap and flips, the hull's hop, a wheel's roll, a body's drop (a hover
// machine onto its skirt, a walker onto its belly), the fan's glide, a fizzer's loops, a track paying out. Intensity is fx.deathAbsurd
// (DeathGags.Intensity): 0 is no gag at all, 1 the new look, 2 ludicrous. Seeded by the caller: every random here is
// a 0..1 the caller drew from DebrisRng.
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public static class VehicleGags
    {
        public const float Gravity = 9.81f;
        /// <summary>The turret's leap at intensity 1 (m/s up), grown with the square root of the intensity and capped.</summary>
        public const float TurretUpMin = 15f, TurretUpMax = 18f, TurretUpCap = 23f;   // 11-16 m up (critic rounds 1 and 2: 20 m left the frame, 9 m looked short)
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
        public const float RollMin = 10f, RollMax = 16f, RollDecel = 2.5f;   // critic round 3: 8 m read as "barely travels" beside a Maw
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
            // it comes down clear of the hull, never on it (critic round 1: at 0.4 it landed on the deck and fused with it)
            float drift = air > 0f ? LandWithin * hullLength * (0.6f + 0.3f * r1) / (air * total) : 0f;
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

        // ------------------------------------------------------------------ a body dropped: a hull's hop, a walker's flop
        /// <summary>Height at t of a body sent up at `up` m/s from `from`, falling to `to` (lower or level), bouncing once
        /// there with HopBounce of its landing speed; `to` after. Hop is this from level ground.</summary>
        public static float Drop(float from, float to, float up, float t)
        {
            if (t <= 0f) return from;
            up = Mathf.Max(0f, up);
            float fall = from - Mathf.Min(from, to);
            // up, over, and down onto `to`: the time the first flight takes, from the quadratic
            float first = (up + Mathf.Sqrt(up * up + 2f * Gravity * fall)) / Gravity;
            if (t < first) return from + up * t - 0.5f * Gravity * t * t;
            float hit = Gravity * first - up, second = hit * HopBounce, s = t - first;
            if (s < 2f * second / Gravity) return to + second * s - 0.5f * Gravity * s * s;
            return to;
        }

        /// <summary>When Drop first comes down on `to`.</summary>
        public static float DropFirst(float from, float to, float up)
        {
            up = Mathf.Max(0f, up);
            return (up + Mathf.Sqrt(up * up + 2f * Gravity * (from - Mathf.Min(from, to)))) / Gravity;
        }

        /// <summary>How long Drop takes, bounce and all.</summary>
        public static float DropSeconds(float from, float to, float up)
        {
            float first = DropFirst(from, to, up);
            return first + 2f * (Gravity * first - Mathf.Max(0f, up)) * HopBounce / Gravity;
        }

        /// <summary>A walker's belly-flop: it holds WalkerFreeze, pops up at FlopUp m/s, and its legs splay out flat over
        /// SplaySeconds as it comes down on its belly.</summary>
        public const float FlopUp = 4.5f, SplaySeconds = 0.18f;   // a pop of a metre: at 2.2 (25 cm) no still caught it
        /// <summary>How far below level a splayed leg points at most (the sine): nearly flat, whatever the hip's height.</summary>
        public const float SplayDown = 0.3f;
        /// <summary>How far a splayed leg is out (0 as it stood, 1 flat out) t seconds after the pop.</summary>
        public static float Splay(float t) => t <= 0f ? 0f : Mathf.SmoothStep(0f, 1f, t / SplaySeconds);

        // ------------------------------------------------------------------ the Skimmer's fan, thrown like a frisbee
        /// <summary>The fan leaves up and astern (m/s at intensity 1, grown with its square root, capped), spinning about
        /// its hub, and lies over flat as it goes: while it keeps FanLiftSpeed it holds up FanLift of its weight, the air
        /// slows it by FanDrag a second and bends its way by up to FanCurve rad/s. It comes down 18-27 m off at 1.</summary>
        public const float FanUpMin = 4f, FanUpMax = 5.5f, FanUpCap = 7f, FanBackMin = 10f, FanBackMax = 13f, FanBackCap = 16f;
        public const float FanLift = 0.8f, FanLiftSpeed = 10f, FanDrag = 0.3f, FanCurve = 0.35f, FanSpin = 28f, FanTilt = 0.35f;
        /// <summary>Longest it glides before FlyDebris takes it (a last safety: it is down well before).</summary>
        public const float FanGlideCap = 8f;

        public struct Glide { public Vector3 Vel; public float Curve; }

        /// <summary>The fan's throw: astern is the way the machine's tail points (horizontal).</summary>
        public static Glide FanThrow(float intensity, Vector3 astern, float r0, float r1, float r2)
        {
            float a = Mathf.Sqrt(Mathf.Max(0f, intensity));
            astern.y = 0f;
            Vector3 dir = astern.sqrMagnitude > 1e-6f ? astern.normalized : Vector3.back;
            float up = Mathf.Min(FanUpCap, Mathf.Lerp(FanUpMin, FanUpMax, r0) * a);
            float back = Mathf.Min(FanBackCap, Mathf.Lerp(FanBackMin, FanBackMax, r1) * a);
            return new Glide { Vel = dir * back + Vector3.up * up, Curve = (r2 * 2f - 1f) * FanCurve };
        }

        /// <summary>One step of a glide: lift while it is fast, drag, the curve. The caller checks the ground.</summary>
        public static void GlideStep(ref Vector3 pos, ref Vector3 vel, float curve, float dt)
        {
            Vector3 flat = new Vector3(vel.x, 0f, vel.z);
            float lift = FanLift * Mathf.Clamp01(flat.magnitude / FanLiftSpeed);
            flat = Quaternion.AngleAxis(curve * Mathf.Rad2Deg * dt, Vector3.up) * flat * Mathf.Exp(-FanDrag * dt);
            vel = new Vector3(flat.x, vel.y - Gravity * (1f - lift) * dt, flat.z);
            pos += vel * dt;
        }

        // ------------------------------------------------------------------ the Salvo's last rockets, fizzing off
        /// <summary>How many of a dead rack's rockets fizz off (at intensity 1; half again at 2, never more than its tubes
        /// or FizzCap), how fast they fly, how long each burns before it pops, how far from the rack one may get (past it,
        /// it pops early: a fizzer is harmless and stays by its machine), how hard it turns and its turn wanders.</summary>
        public const int FizzMin = 4, FizzMax = 7, FizzCap = 11;
        public const float FizzSpeed = 12f, FizzLifeMin = 0.9f, FizzLifeMax = 2.2f, FizzReach = 22f;
        public const float FizzTurnMin = 3f, FizzTurnMax = 7f, FizzWander = 2f, FizzGravity = 0.35f, FizzLaunchSpread = 1.4f;

        public struct Fizzer { public Vector3 Vel, Axis; public float Turn, Wander, Life, Delay; }

        /// <summary>The corkscrew a fizzer is drawn on: FizzHelixRadius about its path, turning FizzHelixSpin rad/s (two
        /// loops a second), so its smoke is a helix (critic round 3: the path's own loops read as straight lines).</summary>
        public const float FizzHelixRadius = 1.1f, FizzHelixSpin = 13f;

        /// <summary>Where a fizzer is drawn off its path t seconds into its flight (way: its path's direction, unit), and
        /// that offset's own velocity. Always FizzHelixRadius from the path, square to it.</summary>
        public static Vector3 Helix(Vector3 way, float t, float phase, out Vector3 turning)
        {
            Vector3 u = Vector3.Cross(way, Mathf.Abs(way.y) < 0.9f ? Vector3.up : Vector3.right).normalized;
            Vector3 w = Vector3.Cross(way, u);
            float a = phase + FizzHelixSpin * t;
            turning = (-Mathf.Sin(a) * u + Mathf.Cos(a) * w) * (FizzHelixRadius * FizzHelixSpin);
            return (Mathf.Cos(a) * u + Mathf.Sin(a) * w) * FizzHelixRadius;
        }

        public static int FizzCount(float intensity, int tubes, float r)
        {
            if (intensity <= 0f || tubes <= 0) return 0;
            int n = Mathf.RoundToInt(Mathf.Lerp(FizzMin, FizzMax, r) * Mathf.Min(1.5f, 0.5f + 0.5f * intensity));
            return Mathf.Clamp(n, 1, Mathf.Min(tubes, FizzCap));
        }

        /// <summary>One fizzer: out of its tube (dir, unit) and off at once in a wandering loop. r0..r4 are dice.</summary>
        public static Fizzer Fizz(Vector3 dir, float r0, float r1, float r2, float r3, float r4)
        {
            dir = dir.sqrMagnitude > 1e-6f ? dir.normalized : Vector3.up;
            float yaw = r0 * Mathf.PI * 2f;
            Vector3 axis = new Vector3(Mathf.Sin(yaw), (r1 - 0.5f) * 0.6f, Mathf.Cos(yaw));
            return new Fizzer
            {
                Vel = dir * FizzSpeed, Axis = axis.normalized,
                Turn = Mathf.Lerp(FizzTurnMin, FizzTurnMax, r2) * (r3 < 0.5f ? -1f : 1f),
                Wander = (r3 * 2f - 1f) * FizzWander,
                Life = Mathf.Lerp(FizzLifeMin, FizzLifeMax, r4),
                Delay = Mathf.Repeat(r0 + r2, 1f) * FizzLaunchSpread,
            };
        }

        /// <summary>One step of a fizzer: its way turns about its axis, the axis wanders about the vertical, the motor
        /// holds it near FizzSpeed against a little gravity, and it skips off the ground. False once it is past FizzReach
        /// of `home` (it pops there).</summary>
        public static bool FizzStep(ref Vector3 pos, ref Vector3 vel, ref Vector3 axis, float turn, float wander, float dt, float ground, Vector3 home)
        {
            vel = Quaternion.AngleAxis(turn * Mathf.Rad2Deg * dt, axis) * vel;
            axis = Quaternion.AngleAxis(wander * Mathf.Rad2Deg * dt, Vector3.up) * axis;
            vel.y -= Gravity * FizzGravity * dt;
            float speed = vel.magnitude;
            if (speed > 1e-4f) vel *= (speed + (FizzSpeed - speed) * Mathf.Min(1f, 3f * dt)) / speed;
            pos += vel * dt;
            if (pos.y < ground) { pos.y = ground; vel.y = Mathf.Abs(vel.y) * 0.6f; }
            Vector3 off = pos - home; off.y = 0f;
            return off.sqrMagnitude <= FizzReach * FizzReach;
        }

        // ------------------------------------------------------------------ a track paying out
        /// <summary>A track comes off and pays out flat along the ground over UnspoolSeconds: its middle comes to lie UnspoolOut of its own half-widths out past the hull's side, lies down to UnspoolFlat of its height and stretches to UnspoolStretch of its length
        /// (the belt laid out), its links running at UnspoolLinks a second as it goes.</summary>
        public const float UnspoolSeconds = 1.4f, UnspoolOut = 2.4f, UnspoolFlat = 0.14f, UnspoolStretch = 2.6f, UnspoolLinks = 9f;

        /// <summary>How far through paying out it is at t (0..1, eased at both ends).</summary>
        public static float Unspool(float t) => Mathf.SmoothStep(0f, 1f, Mathf.Clamp01(t / UnspoolSeconds));
    }
}
