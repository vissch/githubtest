// Phase: unit motion (implemented 2026-09-30) — the arithmetic behind the troops' and machines' motion that the sim
// states but does not draw: the jetpack's arc over its straight 12 m/s line, where each paratrooper comes down (the
// sim's own dice, read ahead of the landing so the canopies can fall to those points), a canopy's descent, the
// Kettle's mortar at a lobbing angle, the Skimmer settling onto its pods when it is knocked out, and the Breaker's
// cycle (wind-up, charge, strike, withdraw) as a pose. Pure functions: TroopMotionFx, AnimationController,
// VATRenderer and TankRenderer.Motion.cs call them, UnitMotionTests pin them.
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Terrain;

namespace TW.Presentation
{
    public static class UnitMotion
    {
        // ------------------------------------------------------------------ the jetpack's leap
        /// <summary>The top of the drawn arc for a leap of `metres`: about 6 m for the full 28 m jump (0.21 of it), never
        /// under 2 m (a short hop still leaves the ground) nor over 8 m.</summary>
        public static float LeapPeak(float metres) => math.clamp(0.21f * metres, 2f, 8f);

        /// <summary>How far along a leap a point is, 0 at the take-off and 1 on the landing, by where the point lies on
        /// the line (the sim flies him along it at a steady speed, so this is the drawn position's own clock).</summary>
        public static float LeapAlong(float2 from, float2 to, float2 at)
        {
            float2 d = to - from;
            float len2 = math.lengthsq(d);
            return len2 < 1e-4f ? 1f : math.saturate(math.dot(at - from, d) / len2);
        }

        /// <summary>Metres above the ground under him at `at` on a leap from `from` to `to` that tops out at `peak`: a
        /// parabola over the line, nothing at either end.</summary>
        public static float ArcHeight(float2 from, float2 to, float2 at, float peak)
        {
            float u = LeapAlong(from, to, at);
            return 4f * peak * u * (1f - u);
        }

        // ------------------------------------------------------------------ the Kettle's mortar
        /// <summary>
        /// A mortar's barrel angle (radians above the horizontal) to lob a round `metres` off when its longest throw is
        /// `rangeMax`: the HIGH solution of the ballistic range, sin(2a) = d / rangeMax, so 45 degrees at full range and
        /// steeper as the target comes in, held between 45 and 80 degrees (a real mortar does not fire flatter or more
        /// upright). 80 degrees at the Kettle's 46 m minimum, 45 at its 300 m.
        /// </summary>
        public static float MortarPitch(float metres, float rangeMax)
        {
            float s = math.saturate(metres / math.max(1f, rangeMax));
            float a = 0.5f * (math.PI - math.asin(s));
            return math.clamp(a, math.radians(45f), math.radians(80f));
        }

        /// <summary>Where a mortar rests with nothing to shoot at: up at the middle of its arc, ready.</summary>
        public static float MortarRest => math.radians(55f);

        // ------------------------------------------------------------------ the Skimmer knocked out
        public const float SettleDrop = 0.35f;      // it comes down the height of its cushion (TankRenderer.HoverLift)
        public const float SettleSeconds = 0.6f;    // over this long
        public const float SkidTau = 0.45f;         // s: how quickly the skid runs out
        public const float SkidMax = 4f;            // m: the most it slides
        public const float SettleTilt = 0.075f;     // rad (4.3 deg): it rests tilted onto one pod

        /// <summary>
        /// A hovercraft whose engine has died `t` seconds ago, going `speed` m/s then: how far its cushion has let it down
        /// (0..SettleDrop, eased), how much of its bob is left (1..0), how far it has slid on along its way (m; the
        /// slide runs out as friction takes it, speed x SkidTau at most, capped at SkidMax), and how far it has tipped
        /// onto a pod (rad). A wreck holds the last of these.
        /// </summary>
        public static void SkimmerSettle(float t, float speed, out float drop, out float bob, out float skid, out float tilt)
        {
            t = math.max(0f, t);
            float k = math.smoothstep(0f, SettleSeconds, t);
            drop = SettleDrop * k;
            bob = 1f - k;
            float s = math.abs(speed);
            skid = math.min(SkidMax, s * SkidTau * (1f - math.exp(-t / SkidTau)));
            tilt = SettleTilt * math.smoothstep(0.25f, SettleSeconds + 0.3f, t);
        }

        // ------------------------------------------------------------------ the Breaker's cycle
        /// <summary>What the Breaker's hull does on top of its ride in one phase of its cycle.</summary>
        public struct BreakerLook
        {
            public float Pitch;   // rad, nose up positive (TankRenderer's convention)
            public float Heave;   // m
            public float Lunge;   // m along its nose
            public float Shake;   // m of shiver, drawn as a fast wobble of that size
            public float Smoke;   // 0..1: how hard its exhaust is working (black puffs)
            public float Dust;    // 0..1: the wall of dust it throws
        }

        /// <summary>
        /// The Breaker's pose `t` seconds into a phase (BreakerSystem.Phase, 0 approach .. 4 withdraw) that lasts
        /// `length` seconds. Wind-up: it squats on its tail (nose up 3.5 deg, 0.12 m down) and shivers harder and harder,
        /// the exhaust pouring black. Charge: the nose lifts (5 deg, settling to 3) and a wall of dust goes up. Strike:
        /// it lunges 0.9 m into the trench in a tenth of a second, the nose dips as it hits and it shakes, then eases
        /// back. Withdraw: it backs out nose-down a little, exhaust working. Approach and rest: nothing.
        /// </summary>
        public static BreakerLook Breaker(int phase, float t, float length)
        {
            var b = new BreakerLook();
            float k = length > 0.01f ? math.saturate(t / length) : 1f;
            switch (phase)
            {
                case 1:   // wind-up
                {
                    float squat = math.smoothstep(0f, 0.3f, k);
                    b.Pitch = math.radians(3.5f) * squat;
                    b.Heave = -0.12f * squat;
                    b.Shake = 0.012f + 0.05f * k * k;
                    b.Smoke = 0.4f + 0.6f * k;
                    break;
                }
                case 2:   // charge
                {
                    float lift = math.smoothstep(0f, 0.25f, t);
                    b.Pitch = math.radians(math.lerp(5f, 3f, math.saturate(t / 1.5f))) * lift;
                    b.Heave = 0.04f * lift;
                    b.Shake = 0.015f;
                    b.Smoke = 1f;
                    b.Dust = 1f;
                    break;
                }
                case 3:   // strike
                {
                    b.Lunge = 0.9f * StrikePulse(t);
                    b.Pitch = -math.radians(4f) * StrikePulse(t);
                    b.Shake = 0.07f * math.exp(-t * 4f);
                    b.Smoke = 0.5f;
                    b.Dust = math.exp(-t * 2f);
                    break;
                }
                case 4:   // withdraw
                {
                    float ease = math.smoothstep(0f, 0.5f, t);
                    b.Pitch = -math.radians(2f) * ease;
                    b.Smoke = 0.7f;
                    b.Shake = 0.008f;
                    break;
                }
            }
            return b;
        }

        /// <summary>The strike's lunge: up to 1 in the first 0.12 s, back to 0 by 0.7 s.</summary>
        public static float StrikePulse(float t)
        {
            if (t <= 0f) return 0f;
            if (t < 0.12f) return math.smoothstep(0f, 0.12f, t);
            return 1f - math.smoothstep(0.12f, 0.7f, t);
        }

        // ------------------------------------------------------------------ the paratroopers
        public const float DropAltitude = 32f;      // m over the drop point the aircraft flies its run at
        public const float DropPlaneSpeed = 55f;    // m/s
        public const float DropOverAfter = 1.6f;    // s after the call it is over the drop point
        public const float DropCarry = 10f;         // m a man carries on along the run after he leaves the aircraft
        public const float DropOpenAfter = 0.4f;    // s of fall before his canopy opens

        /// <summary>
        /// Where each of the `men` paratroopers of a drop called at `pos` by `player` will stand on `landTick`: the sim's
        /// own dice (OffMapAbilitySystem.Land: SimRandom AirDrop per man, a disc of `radius`, clamped to the map, moved to
        /// the nearest open ground), so a canopy can fall to that point before the sim puts him there. `pos` is the drop
        /// point as the DropInbound event gives it (already clamped). Returns how many points it wrote.
        /// </summary>
        public static int ParaLandings(SimWorld w, MapData map, int player, float3 pos, uint landTick, int men, float radius, float3[] into)
        {
            int n = math.min(men, into.Length);
            for (int k = 0; k < n; k++)
            {
                var rng = SimRandom.For(w.Config.Seed, landTick, SimRandom.SystemId.AirDrop, (uint)(player * 64 + k));
                float angle = rng.NextFloat(0f, 2f * math.PI);
                float r = radius * SimMath.Sqrt(rng.NextFloat());
                float3 at = w.ClampToMap(pos + new float3(SimMath.Cos(angle) * r, 0f, SimMath.Sin(angle) * r));
                if (map != null) at = TW.Sim.Units.VehicleModulesSystem.OpenGround(map, at);
                into[k] = at;
            }
            return n;
        }

        /// <summary>The way a side's aircraft flies: towards the enemy (player 0 attacks up the field, +Z).</summary>
        public static float3 DropHeading(int player) => new float3(0f, 0f, player == 1 ? -1f : 1f);

        /// <summary>Seconds after the call a man leaves the aircraft: when it is DropCarry short of his landing point along
        /// its run, so the stick goes out in the order it lands and nobody drifts back against the run.</summary>
        public static float ParaExit(float3 dropPoint, float3 heading, float3 landing)
            => DropOverAfter + (math.dot(landing - dropPoint, heading) - DropCarry) / DropPlaneSpeed;

        /// <summary>
        /// A paratrooper `t` seconds after the call, on a drop that lands `land` seconds after it: his feet's offset from
        /// the ground under him (m) and how far he has come from where he left the aircraft to where he lands (0..1,
        /// along the ground). Out of the door he falls; the canopy opens after DropOpenAfter and he comes down steadily,
        /// slowing as he flares, on the ground at `land`. `open` is 0 before the canopy fills and 1 once it has (it
        /// fills over a quarter second).
        /// </summary>
        public static void ParaFall(float t, float exit, float land, out float height, out float along, out float open)
        {
            float top = DropAltitude - 2f;
            if (t <= exit) { height = top; along = 0f; open = 0f; return; }
            float opens = exit + DropOpenAfter;
            if (t < opens)
            {
                float f = t - exit;
                height = top - 4.9f * f * f;
                along = 0.35f * f / DropOpenAfter;
                open = 0f;
                return;
            }
            float h0 = top - 4.9f * DropOpenAfter * DropOpenAfter;
            float u = land > opens ? math.saturate((t - opens) / (land - opens)) : 1f;
            height = h0 * (1f - u) * (1f - 0.45f * u);   // a steady fall that eases as he flares
            along = math.lerp(0.35f, 1f, math.smoothstep(0f, 1f, u));
            open = math.saturate((t - opens) / 0.25f);
        }
    }
}
