// Phase: VFX pass (owner, 2026-09-28: "each class unit has a different vfx specific to that class ... think of mortars") —
// part of TankRenderer. Every machine's gun drew the same shot: one 4.5 m blast, one heavy straight tracer, the same smoke,
// whether it was the Tusk's 37 mm, the Pavise's long gun or the Kettle's mortar lobbing over a trench. GunFor sizes the
// blast, the round and the smoke by the machine, and an indirect gun (the Kettle's mortar, the Salvo's rockets) throws its
// round in an ARC: a short streak up and over onto the burst. The sim fires and bursts on consecutive ticks (the round has
// no flight: TankGunnery adds the impact the tick it fires), so the arc is quick (ArcSeconds), and it never holds the burst
// back: men do not die before the shell is seen to land. fx.classArms 0 draws every gun as before.
// Then (owner, 2026-09-28: "have everything customized, add the extra effort zoom and quality level"): the Salvo's box goes
// off as a RIPPLE of rockets, each with its own smoke trail, launch and pop (as many as the effects' tier allows); the
// arcs are drawn in the tier's pieces; and near the eye each gun has its own piece (GunExtras): a long gun's brake throws
// its blast out sideways in the dust, the Kettle's tube coughs its smoke straight up, the Salvo's rack blasts back.
using UnityEngine;
using TW.Sim;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed partial class TankRenderer
    {
        /// <summary>A machine's gun as drawn: its blast's width (m), the round's tracer width, the smoke's size (x the old),
        /// and whether the round goes up and over (an indirect gun).</summary>
        public struct GunLook
        {
            public float Blast, Tracer, Smoke; public bool Arc;
            public static readonly GunLook Old = new GunLook { Blast = 4.5f, Tracer = 2.2f, Smoke = 1f };
        }

        public const float ArcSeconds = 0.16f;   // the arc's whole flight on screen
        public const int ArcSegments = 8;        // at High; FxQuality.Now.ArcSegments by the tier
        public const float RippleSeconds = 0.18f, RippleSpread = 3f;   // the Salvo: a rocket every 0.18 s, landing within 3 m of the first
        public const float RocketApex = 0.6f;    // a rocket flies flatter than a mortar round
        public const float RocketTrail = 2.4f, RocketTrailLife = 8f;   // its smoke along the way: wide and long enough to read as a trail (critique lin5)

        /// <summary>How many rockets of a Salvo's ripple are drawn at a tier (the sim's burst is the first one's).</summary>
        static readonly int WindId = Shader.PropertyToID("_TWWind");

        public static int RocketsOf(FxTier tier) => tier >= FxTier.High ? 4 : tier == FxTier.Medium ? 2 : 1;

        public static GunLook GunFor(byte archetype)
        {
            switch (archetype)
            {
                case VehicleArchetype.Tusk: return new GunLook { Blast = 3.0f, Tracer = 1.6f, Smoke = 0.8f };                 // a 37 mm
                case VehicleArchetype.Pavise: case VehicleArchetype.Banner: return new GunLook { Blast = 6.0f, Tracer = 2.8f, Smoke = 1.35f };   // the long guns
                case VehicleArchetype.Kettle: return new GunLook { Blast = 2.6f, Tracer = 1.4f, Smoke = 1.2f, Arc = true };   // the mortar over its back
                case VehicleArchetype.Salvo: return new GunLook { Blast = 3.2f, Tracer = 1.2f, Smoke = 1.6f, Arc = true };    // a box of rockets
                default: return GunLook.Old;   // the Maw's and the Pincer's 6-pdrs, and anything not named
            }
        }

        /// <summary>The point `t` (0..1) of the arc from `from` to `to`: a parabola whose top stands `apex` over the straight line.</summary>
        public static Vector3 ArcPoint(Vector3 from, Vector3 to, float apex, float t) => Vector3.Lerp(from, to, t) + Vector3.up * (4f * apex * t * (1f - t));

        /// <summary>How high an indirect round climbs over a throw of `distance` m.</summary>
        public static float ArcApex(float distance) => Mathf.Clamp(distance * 0.3f, 6f, 40f);

        /// <summary>The indirect round's arc as ArcSegments short tracers, each shown a little after the one before, so a
        /// bright streak runs up and over onto the burst in ArcSeconds.</summary>
        void ThrowArc(Vector3 from, Vector3 to, byte team, float width, bool rocket, uint salt)
        {
            var fx = Fx(); if (fx == null) return;
            var q = FxQuality.Now;
            int segments = Mathf.Max(2, q.ArcSegments), rounds = rocket ? RocketsOf(q.Tier) : 1;
            bool drawn = books != null && books.Ready;
            Vector4 wind = Shader.GetGlobalVector(WindId); Vector3 wake = new Vector3(wind.x, 0f, wind.y) * 3.5f;   // _TWWind at 0.034 per m/s, as the smoke reads it
            for (int j = 0; j < rounds; j++)
            {
                float lag = j * RippleSeconds;
                Vector3 end = to;
                if (j > 0)
                {
                    float a = FxQuality.Hash01(salt + (uint)j) * 6.2832f, d = Mathf.Lerp(1.2f, RippleSpread, FxQuality.Hash01(salt + 17u + (uint)j));
                    end += new Vector3(Mathf.Cos(a) * d, 0f, Mathf.Sin(a) * d); end.y = Ground(end.x, end.z);
                }
                float apex = ArcApex(Vector3.Distance(from, end)) * (rocket ? RocketApex : 1f);
                for (int i = 0; i < segments; i++)
                {
                    float a = i / (float)segments, b = (i + 1) / (float)segments;
                    fx.AddTracer(ArcPoint(from, end, apex, a), ArcPoint(from, end, apex, b), team, Mathf.Min(width, 0.6f), lag + a * ArcSeconds);   // a thread, not a laser: the head and the smoke carry it (r10)
                    // a rocket leaves its smoke hanging along the way it went, behind a burning head; a mortar round a thin grey thread
                    if (drawn && i > 0)
                    {
                        Vector3 at = ArcPoint(from, end, apex, a);
                        if (rocket)
                        {
                            books.Add(FlipbookFx.Book.WreckSmoke, at, RocketTrail, RocketTrailLife, (i & 1) == 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                                velocity: wake, grow: 1.4f, alpha: 0.85f, delay: lag + a * ArcSeconds);   // dark (the wreck's book: the Smoke book read as snow haze), it hangs and leans down wind
                        }
                        else
                            books.Add(FlipbookFx.Book.WreckSmoke, at, 1.4f, 2.4f, (i & 1) == 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                                velocity: Vector3.up * 0.2f, grow: 1.2f, alpha: 0.5f, delay: lag + a * ArcSeconds);   // grey-dark, so it survives the day
                    }
                }
                if (rocket && drawn)
                    books.Add(FlipbookFx.Book.Muzzle, ArcPoint(from, end, apex, 0.1f), 2.5f, ArcSeconds * 0.9f, velocity: (end - ArcPoint(from, end, apex, 0.1f)) / ArcSeconds,
                        roll: 0f, glow: SceneMood.Night ? 4.5f : 3f, delay: lag);
                if (rocket && drawn && j > 0)
                {
                    // the rocket's burning head: one flame card flying the chord of its arc (a card flies straight), over the flight
                    // each rocket after the first: its own flash at the rack, and its own pop where it lands
                    books.Add(FlipbookFx.Book.Flash, from, 2.4f, 0.08f, roll: FxQuality.Hash01(salt + 31u + (uint)j) * 6.2832f, glow: SceneMood.Night ? 3.5f : 1.8f, pop: 0.5f, delay: lag);
                    books.Add(FlipbookFx.Book.Flash, end + Vector3.up * 0.4f, 2.2f, 0.1f, roll: FxQuality.Hash01(salt + 41u + (uint)j) * 6.2832f, glow: SceneMood.Night ? 4f : 1.8f, pop: 0.5f, delay: lag + ArcSeconds);
                    books.Add(FlipbookFx.Book.DustPuff, end, 1.8f, 1.4f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | ((j & 1) == 0 ? FlipbookFx.Kind.Mirror : 0), alpha: 0.7f, delay: lag + ArcSeconds);
                }
            }
        }

        /// <summary>The gun's own piece near the eye (FxQuality: High and Epic, inside twice ExtraReach: a machine is big).</summary>
        void GunExtras(byte archetype, Vector3 muzzle, Vector3 dir, float blast)
        {
            var q = FxQuality.Now;
            if (books == null || !books.Ready || q.ExtraReach <= 0f || CameraShake.DistanceToLook(muzzle) > q.ExtraReach * 2f) return;
            var ground = FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored;
            float g = Ground(muzzle.x, muzzle.z);
            Vector3 flat = new Vector3(dir.x, 0f, dir.z); flat = flat.sqrMagnitude > 1e-4f ? flat.normalized : Vector3.forward;
            switch (archetype)
            {
                case VehicleArchetype.Pavise: case VehicleArchetype.Banner: case VehicleArchetype.Maw: case VehicleArchetype.Pincer:
                {
                    // the muzzle brake throws the blast out sideways: a jet of dust along the ground either side of the barrel
                    Vector3 side = Vector3.Cross(Vector3.up, flat);
                    for (int s = -1; s <= 1; s += 2)
                        books.Add(FlipbookFx.Book.DustPuff, new Vector3(muzzle.x, g, muzzle.z) + side * (s * 1.2f) + flat * 0.8f, blast * 0.9f, 1.6f, ground | (s < 0 ? FlipbookFx.Kind.Mirror : 0),
                            velocity: side * (s * 2.2f) + Vector3.up * 0.3f, grow: 0.5f, alpha: 0.8f);
                    break;
                }
                case VehicleArchetype.Kettle:   // the mortar's tube coughs its smoke straight up
                    books.Add(FlipbookFx.Book.Smoke, muzzle + Vector3.up * 0.6f, 1.6f, 2.8f, velocity: Vector3.up * 2.2f, grow: 1.8f, alpha: 0.6f);
                    books.Add(FlipbookFx.Book.Smoke, muzzle + Vector3.up * 1.4f, 1.2f, 2.4f, FlipbookFx.Kind.Mirror, velocity: Vector3.up * 1.6f, grow: 1.6f, alpha: 0.45f, delay: 0.08f);
                    break;
                case VehicleArchetype.Salvo:    // the rack's backblast, and the dust it lifts behind the truck
                    books.Add(FlipbookFx.Book.Smoke, muzzle - flat * 2.5f, 2.4f, 3f, velocity: -flat * 3f + Vector3.up * 0.6f, grow: 1.8f, alpha: 0.55f);
                    books.Add(FlipbookFx.Book.DustPuff, new Vector3(muzzle.x, g, muzzle.z) - flat * 3.5f, 3.5f, 1.8f, ground, velocity: -flat * 1.5f, grow: 0.5f, alpha: 0.6f);
                    break;
            }
        }
    }
}
