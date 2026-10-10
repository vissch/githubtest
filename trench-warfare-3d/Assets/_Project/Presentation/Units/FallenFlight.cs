// Phase: deaths (2026-09-28, implemented) — the path a gagged body takes (DeathGags), planned once when he falls and
// read back every frame without a ground sample: an optional hold before the launch (a walker's claw lifting him, a
// machine gun jigging him on the spot), up to three arcs (the throw and two bounces, each shorter and lower, each
// landing on the drawn ground), a skid along the ground, then rest. Every arc turns whole flips and rolls, so he lands
// as his death clip left him. Pure arithmetic: FallenFlightTests holds it without a GPU or a scene.
// A body with no gag never comes here: VATRenderer.Fallen keeps its own single arc, exactly as before.
using UnityEngine;

namespace TW.Presentation.Units
{
    /// <summary>The height of the drawn ground at a point (RenderGround in the game, a plane in the tests).</summary>
    public interface IGroundHeight { float At(float x, float z); }

    public static class FallenFlight
    {
        /// <summary>A bounce goes this share of the throw's distance (clamped) and this share of its height (clamped).</summary>
        public const float BounceFar = 0.35f, BounceFarMin = 0.8f, BounceFarMax = 5f;
        public const float BounceHigh = 0.22f, BounceHighMin = 0.35f, BounceHighMax = 2.4f;
        /// <summary>The second bounce is this share of the first.</summary>
        public const float SecondBounce = 0.4f;
        /// <summary>A balloon's hops (MakeHops): the second and third go this share of the first hop's distance and height.
        /// The design's hops are 6 / 4 / 2.5 m far and 3 / 2.2 / 1.2 m high, and the first is what the controller throws.</summary>
        public const float HopFar2 = 4f / 6f, HopFar3 = 2.5f / 6f, HopHigh2 = 2.2f / 3f, HopHigh3 = 1.2f / 3f;
        /// <summary>A first bounce higher than this turns him over once more.</summary>
        public const float BounceFlipHeight = 0.6f;
        /// <summary>A skid of d metres lasts SkidBase + SkidPerMetre x d seconds, easing out.</summary>
        public const float SkidBase = 0.35f, SkidPerMetre = 0.12f;
        /// <summary>How long the claw takes to lift him, and how high a machine gun's jig hops him (life-size m).</summary>
        public const float LiftSeconds = 0.12f, JigHop = 0.12f, JigHz = 12f;
        /// <summary>Stretch per m/s of vertical speed in flight (times the intensity), and its cap as a height factor.</summary>
        public const float StretchPerSpeed = 0.022f, StretchMax = 1.36f;
        /// <summary>The landing's squash: the height factor at the touch is 1 - LandSquash x the speed he lands with
        /// (clamped), and it springs back through a damped wobble over WobbleSeconds.</summary>
        public const float LandSquash = 0.04f, LandSquashMin = 0.45f, LandSquashMax = 0.92f, WobbleSeconds = 0.22f, WobbleDecay = 0.06f, WobblePeriod = 0.16f;
        /// <summary>A resting squash (a pancake, a wilt) is reached over this long.</summary>
        public const float RestSquashSeconds = 0.15f;
        public const int Steps = 32;   // VATRenderer.PitchSteps: a whole turn

        public struct Arc
        {
            public Vector3 From, To;
            /// <summary>When it starts (seconds after the delay), how long it lasts, the time to its top, the top's height
            /// (world y), and the height above the higher end it was solved for.</summary>
            public float Start, Dur, Up, Top, Height;
            public byte Flips, Rolls;
            public float End => Start + Dur;
        }

        public struct Plan
        {
            /// <summary>Where he fell (the hold and jig happen here).</summary>
            public Vector3 Origin;
            public Arc A0, A1, A2;
            public byte Arcs;
            public Vector3 SkidFrom, SkidTo;
            public float SkidStart, SkidDur;
            /// <summary>Where he lies once it is all over.</summary>
            public Vector3 Rest;
            /// <summary>Seconds before the launch; metres the claw lifts him in that time; the jig's yaw in radians (0 none).</summary>
            public float Delay, HoldLift, Jig;
            /// <summary>A stiff topple about his feet on the spot (a gassed man going over like a plank): steps of pitch and seconds.</summary>
            public byte Topple; public float ToppleDur;
            /// <summary>A squash pulse at the start (a headshot's jolt, a gasp) and the squash he lies at, as VatTint codes.</summary>
            public sbyte PulseQ, RestQ; public float PulseDur;
            /// <summary>Seconds (after the delay) until he lies still.</summary>
            public float Settled;
            /// <summary>A body that shrinks as he goes (a balloon): his size on each arc and the size he lies at, as a factor
            /// of his own. All zero for every other gag, and SizeAt then answers 1 throughout.</summary>
            public float Size0, Size1, Size2, SizeRest;
            public float Gravity;
            /// <summary>Seconds from his death until he lies still.</summary>
            public float Arrive => Delay + Settled;
            public float ArcsEnd => Arcs == 0 ? 0f : Arcs == 1 ? A0.End : Arcs == 2 ? A1.End : A2.End;
        }

        public static Arc Solve(Vector3 from, Vector3 to, float height, float start, float gravity)
        {
            float g = Mathf.Max(1f, gravity), top = Mathf.Max(from.y, to.y) + Mathf.Max(0f, height);
            float up = Mathf.Sqrt(2f * (top - from.y) / g), down = Mathf.Sqrt(2f * (top - to.y) / g);
            return new Arc { From = from, To = to, Start = start, Dur = up + down, Up = up, Top = top, Height = height };
        }

        static Vector3 OnGround<G>(Vector3 p, Vector2 size, ref G ground) where G : struct, IGroundHeight
        {
            p.x = Mathf.Clamp(p.x, 0.5f, Mathf.Max(0.5f, size.x - 0.5f)); p.z = Mathf.Clamp(p.z, 0.5f, Mathf.Max(0.5f, size.y - 0.5f));
            p.y = ground.At(p.x, p.z) - 0.02f;
            return p;
        }

        /// <summary>
        /// The whole path. from: where he fell, on the ground. fly: the throw as the controller writes it (xz how far, y
        /// how high above the higher end; zero for no throw). flips/rolls: turns on the throw. bounces: 0..2. skid: metres
        /// along skidDir (the throw's way when skidDir is zero). water: a first landing below it neither bounces nor skids.
        /// </summary>
        public static Plan Make<G>(Vector3 from, Vector3 fly, int flips, int rolls, int bounces, float skid, Vector3 skidDir, float delay, float holdLift, float jig, float gravity, Vector2 mapSize, float water, ref G ground) where G : struct, IGroundHeight
        {
            var p = new Plan { Origin = from, Delay = Mathf.Max(0f, delay), HoldLift = Mathf.Max(0f, holdLift), Jig = jig, Gravity = Mathf.Max(1f, gravity) };
            Vector3 at = from + Vector3.up * p.HoldLift;
            Vector3 way = new Vector3(fly.x, 0f, fly.z);
            float far = way.magnitude, t = 0f;
            Vector3 dir = far > 0.01f ? way / far : Vector3.zero;
            if (fly.y > 0.05f)
            {
                var land = OnGround(at + way, mapSize, ref ground);
                p.A0 = Solve(at, land, fly.y, 0f, p.Gravity);
                p.A0.Flips = (byte)Mathf.Clamp(flips, 0, 15); p.A0.Rolls = (byte)Mathf.Clamp(rolls, 0, 15);
                p.Arcs = 1; t = p.A0.End; at = land;
                bool wet = land.y < water - 0.05f;
                if (wet) { bounces = 0; skid = 0f; }
                if (bounces > 0)
                {
                    float d1 = far > 0.1f ? Mathf.Clamp(BounceFar * far, BounceFarMin, BounceFarMax) : 0f;
                    float h1 = Mathf.Clamp(BounceHigh * fly.y, BounceHighMin, BounceHighMax);
                    var l1 = OnGround(at + dir * d1, mapSize, ref ground);
                    p.A1 = Solve(at, l1, h1, t, p.Gravity);
                    p.A1.Flips = (byte)(h1 > BounceFlipHeight ? 1 : 0);
                    p.Arcs = 2; t = p.A1.End; at = l1;
                    if (bounces > 1)
                    {
                        var l2 = OnGround(at + dir * (d1 * SecondBounce), mapSize, ref ground);
                        p.A2 = Solve(at, l2, h1 * SecondBounce, t, p.Gravity);
                        p.Arcs = 3; t = p.A2.End; at = l2;
                    }
                }
            }
            else if (p.HoldLift > 0f)
            {
                // held up and let go with no throw: he drops back where he was lifted
                var land = OnGround(from, mapSize, ref ground);
                p.A0 = Solve(at, land, 0f, 0f, p.Gravity);
                p.Arcs = 1; t = p.A0.End; at = land;
            }
            if (skid > 0.01f)
            {
                Vector3 sd = skidDir; sd.y = 0f;
                if (sd.sqrMagnitude < 1e-4f) sd = dir;
                if (sd.sqrMagnitude > 1e-4f)
                {
                    sd.Normalize();
                    p.SkidFrom = at; p.SkidTo = OnGround(at + sd * skid, mapSize, ref ground);
                    p.SkidStart = t; p.SkidDur = SkidBase + SkidPerMetre * skid;
                    t += p.SkidDur; at = p.SkidTo;
                }
            }
            p.Rest = at; p.Settled = t;
            return p;
        }

        /// <summary>
        /// A balloon's path: up to three hops, each on its own bearing (the first along fly, each one after it turned
        /// hopTurn radians off the last, alternating the way it turns) and each shorter and lower than the last by the Hop
        /// shares above. No bounce, no skid: every hop lands on the drawn ground and the last is where he lies. A hop that
        /// comes down below the water ends it there. Make is left alone, so no other gag changes.
        /// </summary>
        public static Plan MakeHops<G>(Vector3 from, Vector3 fly, int hops, float hopTurn, float delay, float gravity, Vector2 mapSize, float water, ref G ground) where G : struct, IGroundHeight
        {
            var p = new Plan { Origin = from, Delay = Mathf.Max(0f, delay), Gravity = Mathf.Max(1f, gravity), Rest = from };
            Vector3 way = new Vector3(fly.x, 0f, fly.z);
            float far = way.magnitude;
            if (hops < 1 || fly.y <= 0.05f || far <= 0.01f) return p;
            Vector3 dir = way / far, at = from;
            float t = 0f;
            int n = Mathf.Min(hops, 3);
            for (int k = 0; k < n; k++)
            {
                if (k > 0) dir = Turn(dir, (k & 1) == 1 ? hopTurn : -hopTurn);
                float d = far * (k == 0 ? 1f : k == 1 ? HopFar2 : HopFar3);
                float h = fly.y * (k == 0 ? 1f : k == 1 ? HopHigh2 : HopHigh3);
                var land = OnGround(at + dir * d, mapSize, ref ground);
                var arc = Solve(at, land, h, t, p.Gravity);
                if (k == 0) p.A0 = arc; else if (k == 1) p.A1 = arc; else p.A2 = arc;
                p.Arcs = (byte)(k + 1); t = arc.End; at = land;
                if (land.y < water - 0.05f) break;   // down in the water: the air is out of him and he stays there
            }
            p.Rest = at; p.Settled = t;
            return p;
        }

        /// <summary>A flat bearing turned by this many radians.</summary>
        static Vector3 Turn(Vector3 dir, float radians)
        {
            float s = Mathf.Sin(radians), c = Mathf.Cos(radians);
            return new Vector3(dir.x * c + dir.z * s, 0f, dir.z * c - dir.x * s);
        }

        /// <summary>How big he is drawn now, as a factor of his own size: himself until the launch, the hop's size while he
        /// is on it, and the last one once he lies. 1 throughout for a path with no sizes (every gag but the balloon).</summary>
        public static float SizeAt(in Plan p, float age)
        {
            if (p.Size0 <= 0f) return 1f;
            if (age - p.Delay < 0f) return 1f;
            switch (ArcAt(p, age))
            {
                case 0: return p.Size0;
                case 1: return p.Size1 > 0f ? p.Size1 : p.Size0;
                case 2: return p.Size2 > 0f ? p.Size2 : p.Size1;
            }
            return p.SizeRest > 0f ? p.SizeRest : p.Size2 > 0f ? p.Size2 : p.Size0;
        }

        /// <summary>Moves where he comes to rest (a heap lifts and nudges him), re-solving the last part of the path so he
        /// lands there rather than popping to it.</summary>
        public static void EndAt(ref Plan p, Vector3 rest)
        {
            if (p.SkidDur > 0f) p.SkidTo = rest;
            else if (p.Arcs > 0)
            {
                ref Arc last = ref p.Arcs == 1 ? ref p.A0 : ref p.Arcs == 2 ? ref p.A1 : ref p.A2;
                byte flips = last.Flips, rolls = last.Rolls;
                last = Solve(last.From, rest, last.Height, last.Start, p.Gravity);
                last.Flips = flips; last.Rolls = rolls;
                p.Settled = last.End;
            }
            else p.Origin = rest;
            p.Rest = rest;
        }

        /// <summary>Where he is, age seconds after he died (not yet sunk: the renderer sinks him at the end of his time).</summary>
        public static Vector3 At(in Plan p, float age)
        {
            float t = age - p.Delay;
            if (t < 0f)
            {
                float lift = p.HoldLift * Mathf.Clamp01(age / LiftSeconds);
                float hop = p.Jig != 0f ? JigHop * Mathf.Abs(Mathf.Sin(Mathf.PI * JigHz * 0.5f * age)) : 0f;
                return p.Origin + Vector3.up * (lift + hop);
            }
            if (p.Arcs > 0 && t < p.A0.End) return OnArc(p.A0, t, p.Gravity);
            if (p.Arcs > 1 && t < p.A1.End) return OnArc(p.A1, t, p.Gravity);
            if (p.Arcs > 2 && t < p.A2.End) return OnArc(p.A2, t, p.Gravity);
            if (p.SkidDur > 0f && t < p.SkidStart + p.SkidDur)
            {
                float u = Mathf.Clamp01((t - p.SkidStart) / p.SkidDur);
                return Vector3.Lerp(p.SkidFrom, p.SkidTo, 1f - (1f - u) * (1f - u));
            }
            return p.Arcs == 0 && p.SkidDur <= 0f ? p.Origin : p.Rest;
        }

        static Vector3 OnArc(in Arc a, float t, float gravity)
        {
            float tt = Mathf.Max(0f, t - a.Start);
            Vector3 at = Vector3.Lerp(a.From, a.To, a.Dur > 0f ? tt / a.Dur : 1f);
            float fromTop = tt - a.Up;
            at.y = a.Top - 0.5f * gravity * fromTop * fromTop;
            return at;
        }

        /// <summary>Which arc he is on (0..2), or -1 before, between none, and after them all.</summary>
        public static int ArcAt(in Plan p, float age)
        {
            float t = age - p.Delay;
            if (t < 0f || p.Arcs == 0) return -1;
            if (t < p.A0.End) return 0;
            if (p.Arcs > 1 && t < p.A1.End) return 1;
            if (p.Arcs > 2 && t < p.A2.End) return 2;
            return -1;
        }

        static Arc ArcN(in Plan p, int k) => k == 0 ? p.A0 : k == 1 ? p.A1 : p.A2;

        /// <summary>End over end: whole turns on each arc (so every landing is level), the topple on the spot, then rest.</summary>
        public static int PitchStep(in Plan p, float age, int restPitch)
        {
            int k = ArcAt(p, age);
            if (k >= 0)
            {
                var a = ArcN(p, k);
                return a.Flips == 0 ? 0 : Mathf.RoundToInt(a.Flips * Steps * ((age - p.Delay - a.Start) / a.Dur)) & (Steps - 1);
            }
            if (p.Topple > 0 && p.ToppleDur > 0f)
            {
                float u = Mathf.Clamp01((age - p.Delay) / p.ToppleDur);
                return Mathf.RoundToInt(p.Topple * u * u) & (Steps - 1);   // slow at first, then it goes: a plank
            }
            return age - p.Delay < 0f ? 0 : restPitch & (Steps - 1);
        }

        /// <summary>Side over side: whole turns on each arc, level between.</summary>
        public static int RollStep(in Plan p, float age)
        {
            int k = ArcAt(p, age);
            if (k < 0) return 0;
            var a = ArcN(p, k);
            return a.Rolls == 0 ? 0 : Mathf.RoundToInt(a.Rolls * Steps * ((age - p.Delay - a.Start) / a.Dur)) & (Steps - 1);
        }

        /// <summary>
        /// The squash code (VatTint) at this moment: a pulse at the start, stretched by his speed in the air, squashed where
        /// each arc lands and wobbling back, then the squash he lies at. intensity scales the stretch and the landings.
        /// </summary>
        public static int Squash(in Plan p, float age, float intensity)
        {
            float t = age - p.Delay;
            if (t < 0f) return 0;
            if (p.PulseDur > 0f && t < p.PulseDur) return Mathf.RoundToInt(p.PulseQ * Mathf.Sin(Mathf.PI * t / p.PulseDur));
            int k = ArcAt(p, age);
            if (k >= 0)
            {
                var a = ArcN(p, k);
                float vy = p.Gravity * (a.Up - (t - a.Start));
                float s = Mathf.Min(StretchMax, 1f + StretchPerSpeed * intensity * Mathf.Abs(vy));
                return VatTint.SquashOf(s);
            }
            // the last landing still ringing
            for (int n = p.Arcs - 1; n >= 0; n--)
            {
                var a = ArcN(p, n);
                float tau = t - a.End;
                if (tau < 0f || tau >= WobbleSeconds) continue;
                float vLand = p.Gravity * (a.Dur - a.Up);
                float s0 = Mathf.Clamp(1f - LandSquash * intensity * vLand, LandSquashMin, LandSquashMax);
                float s = 1f + (s0 - 1f) * Mathf.Exp(-tau / WobbleDecay) * Mathf.Cos(2f * Mathf.PI * tau / WobblePeriod);
                return VatTint.SquashOf(s);
            }
            if (p.RestQ != 0)
            {
                float since = t - (p.Arcs > 0 || p.SkidDur > 0f ? p.Settled : p.PulseDur);
                return Mathf.RoundToInt(p.RestQ * Mathf.Clamp01(since / RestSquashSeconds));
            }
            return 0;
        }

        /// <summary>Seconds of the flight (the arcs only): he turns in the air, not along the ground.</summary>
        public static float Airborne(in Plan p, float age) => Mathf.Clamp(age - p.Delay, 0f, p.ArcsEnd);
    }
}
