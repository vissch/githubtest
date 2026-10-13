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
        public const float RocketTrail = 2.4f, RocketTrailLife = 8f;
        public const float RocketHeadWidth = 4f;   // the head book's card: head to tail, a fire drawing (it reads on snow by day)   // its smoke along the way: wide and long enough to read as a trail (critique lin5)

        /// <summary>How many rockets of a Salvo's ripple are drawn at a tier (the sim's burst is the first one's).</summary>
        static readonly int WindId = Shader.PropertyToID("_TWWind");

        /// <summary>Which drawing a machine's gun blast is: the Tusk's 37 mm a sharp crack, the long guns a cone with
        /// brake jets and a fireball ahead, the rest the shared GunBlast (critique r15: the Tusk and Pavise read alike at z40).</summary>
        public static FlipbookFx.Book BlastBookOf(byte archetype)
        {
            switch (archetype)
            {
                case VehicleArchetype.Tusk: case VehicleArchetype.Salvo: return FlipbookFx.Book.GunCrack;   // the Salvo's launch a crack too (r19: a white disc, no book)
                case VehicleArchetype.Pavise: case VehicleArchetype.Banner: return FlipbookFx.Book.GunLong;
                default: return FlipbookFx.Book.GunBlast;
            }
        }

        /// <summary>A calibre book's width x the gun's Blast: the 37 mm small and sharp, the long gun big (r17: scale contrast,
        /// the two read as one flash at z40).</summary>
        public static float CalibreScale(FlipbookFx.Book book) => book == FlipbookFx.Book.GunCrack ? 1.4f : book == FlipbookFx.Book.GunLong ? 1.2f : 1.6f;   // r19: the crack up so its spikes read, the long gun's fireball down (1.8 was a sign in the air)
        /// <summary>A calibre book's glow by day: its drawn values, no more (1.4 bloomed it into a white disc on snow, r17).</summary>
        public const float OwnBlastDayGlow = 1.0f;
        /// <summary>The glow card over the day light of a gun with its own blast drawing (Flash's default 2.6 is a white disc).</summary>
        public const float OwnBlastLightCard = 0.35f;

        public static int RocketsOf(FxTier tier) => tier >= FxTier.High ? 4 : tier == FxTier.Medium ? 2 : 1;

        /// <summary>A shot's near smoke as drawn: the small flash over the blast, the Puff ring the muzzle leaves, and the
        /// three Smoke cards that follow it out along the barrel. All widths in metres, lives in seconds, pushes in metres
        /// along the barrel from the muzzle; the three cards are card k = 0..2 of SmokeSizeOf/LifeOf/AlongOf/AlphaOf.
        /// (Owner, 2026-10-12, on the Banner's day film: "the firing vfx ... can be improved". The Banner's Blast 6.0 gave
        /// a 4.8 m Puff at alpha 0.7 sitting 0.6 m off the muzzle - the beige puff over its own front leg - and three smoke
        /// cards drifting ALONG the barrel with only +0.5 up, which kept them on the machine. The Banner now gets a short
        /// bright flash, a thinner Puff pushed clear, and smoke that goes up, thins per card and leaves the machine.)</summary>
        public struct MuzzleFx
        {
            public float FlashWidth, FlashWidthPlain, FlashLife;     // the flash under its own blast drawing, and without one
            public float PuffWidth, PuffLife, PuffAlpha, PuffPush;
            public float SmokeSize0, SmokeSizeStep, SmokeLife0, SmokeLifeStep;
            public float SmokeAlpha, SmokeAlphaStep, SmokeGrow, SmokeUp, SmokeAlong0, SmokeAlongStep;
            public float SmokePush0, SmokePushStep, SmokeDelayStep;

            public float SmokeSizeOf(int k) => SmokeSize0 + k * SmokeSizeStep;
            public float SmokeLifeOf(int k) => SmokeLife0 + k * SmokeLifeStep;
            public float SmokeAlongOf(int k) => SmokeAlong0 + k * SmokeAlongStep;
            public float SmokePushOf(int k) => SmokePush0 + k * SmokePushStep;
            /// <summary>Card k's alpha: the chain thins as it goes (the old look held one alpha for all three).</summary>
            public float SmokeAlphaOf(int k) => Mathf.Max(0.05f, SmokeAlpha + k * SmokeAlphaStep);
        }

        /// <summary>How a machine's shot smokes near the muzzle. Pure: the only input is the archetype, through GunFor.</summary>
        public static MuzzleFx MuzzleLook(byte archetype)
        {
            var gun = GunFor(archetype);
            var fx = new MuzzleFx
            {
                FlashWidth = 1.6f, FlashWidthPlain = 3.2f, FlashLife = 0.1f,
                PuffWidth = gun.Blast * 0.8f, PuffLife = 1.2f, PuffAlpha = 0.7f, PuffPush = 0.6f,
                SmokeSize0 = 1.2f * gun.Smoke, SmokeSizeStep = 0.4f * gun.Smoke,
                SmokeLife0 = 2.5f, SmokeLifeStep = 0.5f,
                SmokeAlpha = 0.55f, SmokeAlphaStep = 0f, SmokeGrow = 1.6f,
                SmokeUp = 0.5f, SmokeAlong0 = 2.5f, SmokeAlongStep = -0.6f,
                SmokePush0 = 0.6f, SmokePushStep = 0.7f, SmokeDelayStep = 0.03f,
            };
            if (archetype == VehicleArchetype.Banner)
            {
                fx.FlashLife = 0.08f;                                   // a crack, not a flame standing in the air
                fx.FlashWidthPlain = 2.0f;                              // a 3.2 m card could only start at the mouth by standing 1.6 m out in front of it; under the 3.0 m front-leg gap
                fx.PuffWidth = 2.6f; fx.PuffLife = 0.8f; fx.PuffAlpha = 0.45f; fx.PuffPush = 2.2f;   // thinner, shorter, out past the leg
                fx.SmokeSize0 = 1.3f; fx.SmokeSizeStep = 0.45f;         // no card wide enough to cover a front leg (they stand 2.9 m apart)
                fx.SmokeLife0 = 2f; fx.SmokeLifeStep = 0.4f;
                fx.SmokeAlpha = 0.4f; fx.SmokeAlphaStep = -0.08f; fx.SmokeGrow = 2f;   // it thins as the chain goes out
                fx.SmokeUp = 2.4f; fx.SmokeAlong0 = 0.8f; fx.SmokeAlongStep = -0.2f;   // mostly up: it rises off the machine
            }
            return fx;
        }

        /// <summary>One drawn card of a machine's shot, as the recipe branch of the VehicleFired handler draws it:
        /// which book, how far along the barrel from the muzzle its centre is pushed, how wide and how long it lives.
        /// RootLeft: the drawing starts at the card's left edge (the fire books' root_left), so the flame leaves the
        /// card's near edge. Carried: the card travels with the machine while it lives, instead of standing in the
        /// world where the shot went off. (Owner, 2026-10-13, on the Banner's film: "the vfx is still off. look at
        /// the fire it clips over the weapon." Measured on this tree: the plain Flash is 3.2 m wide pushed 0.4 m, so
        /// its near edge sat 1.2 m BEHIND the muzzle plane - inside the barrel and the top - and the 1 s GunBlast card
        /// stood in the world while the walker drove 2.4 m through it. BannerFireFitTests holds both numbers.)</summary>
        public struct FireCard
        {
            public FlipbookFx.Book Book;
            public float Push, Width, Life;
            public bool RootLeft, Carried;
            public int Smoke;   // -1, or which of the three near-smoke cards this is

            /// <summary>Where the card's near edge sits along the barrel, from the muzzle plane: below zero is inside
            /// the gun.</summary>
            public float NearEdge => Push - Width * 0.5f;
        }

        /// <summary>No shot draws more cards than this (the recipe branch: one blast, one flash, three smokes).</summary>
        public const int MaxFireCards = 8;
        /// <summary>A card that lives longer than this is carried with the machine: shorter ones are gone before it moves.</summary>
        public const float CarriedOver = 0.2f;
        /// <summary>The near edge of a shot's card may not sit behind the muzzle plane by more than this, nor ahead of
        /// it by more than FireGapMax: the fire starts at the mouth of the barrel and goes forward.</summary>
        public const float FireBehindMax = 0.05f, FireGapMax = 1.2f;

        /// <summary>Every card a machine's shot draws, in the order the handler adds them. Pure: the archetype is the
        /// only input, so a test reads exactly what the battle draws.</summary>
        public static int FireCards(byte archetype, FireCard[] into)
        {
            var gun = GunFor(archetype);
            var muz = MuzzleLook(archetype);
            int n = 0;
            // a big gun's blast, rooted at the card's left edge, so its centre is half its width out along the barrel
            into[n++] = new FireCard { Book = FlipbookFx.Book.GunBlast, Push = gun.Blast * 0.5f, Width = gun.Blast, Life = 1f, RootLeft = true, Smoke = -1 };
            // the small flash at the mouth under it
            into[n++] = new FireCard { Book = FlipbookFx.Book.Flash, Push = 0.4f, Width = muz.FlashWidthPlain, Life = muz.FlashLife, Smoke = -1 };
            // and the three smoke cards that follow the shot out along the barrel
            for (int k = 0; k < 3; k++)
                into[n++] = new FireCard { Book = FlipbookFx.Book.Smoke, Push = muz.SmokePushOf(k), Width = muz.SmokeSizeOf(k), Life = muz.SmokeLifeOf(k), Smoke = k };
            // Only the Banner's shot is refitted: the owner complained of its fire and of no other machine's, and the
            // rest keep exactly the picture they had (GunMuzzleLookTests).
            if (archetype != VehicleArchetype.Banner) return n;
            for (int i = 0; i < n; i++)
            {
                var c = into[i];
                // the fire starts at the mouth of the barrel and goes forward: a centred card is pushed out at least
                // half its own width, so none of it is drawn back inside the gun (the Banner's 3.2 m Flash sat 1.2 m
                // inside the barrel and under the top, 2026-10-13)
                if (!c.RootLeft) c.Push = Mathf.Max(c.Push, c.Width * 0.5f);
                // and a card that outlives the machine's next step travels with it, instead of standing in the world
                // while the walker drives through it (the 1 s blast card slid 2.40 m back over the barrel)
                c.Carried = c.Life > CarriedOver;
                into[i] = c;
            }
            return n;
        }

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

        readonly FireCard[] fireCards = new FireCard[MaxFireCards];

        /// <summary>Draws every card of a shot from FireCards, so no width, push or life of the shot lives in two
        /// places. The look of each card (its glow, its roll, the smoke's drift and delay) stays here; where and how
        /// big it is comes from the recipe. A carried card is given the machine's own travel, so the fire goes with
        /// the gun while it lives instead of the walker driving through it.</summary>
        void DrawFireCards(View v, Vector3 muzzle, Vector3 dir, float roll, Vector3 fwd)
        {
            var muz = MuzzleLook(v.Archetype);
            Vector3 carry = fwd * v.Felt.Value;
            int n = FireCards(v.Archetype, fireCards);
            for (int i = 0; i < n; i++)
            {
                var c = fireCards[i];
                Vector3 at = muzzle + dir * c.Push;
                Vector3 vel = c.Carried ? carry : Vector3.zero;
                if (c.Book == FlipbookFx.Book.GunBlast)
                    books.Add(c.Book, at, c.Width, c.Life, velocity: vel, roll: roll, glow: SceneMood.Night ? 2.2f : 1.4f);   // 12 frames at 12 fps
                else if (c.Book == FlipbookFx.Book.Flash)
                    books.Add(c.Book, at, c.Width, c.Life, velocity: vel, roll: UnityEngine.Random.value * 6.28f, glow: SceneMood.Night ? 4f : 2f, pop: 0.5f);
                else
                {
                    int k = Mathf.Max(0, c.Smoke);
                    books.Add(c.Book, at, c.Width, c.Life, (k & 1) == 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                        velocity: vel + dir * muz.SmokeAlongOf(k) + Vector3.up * muz.SmokeUp, grow: muz.SmokeGrow, alpha: muz.SmokeAlphaOf(k), delay: k * muz.SmokeDelayStep);
                }
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
                    // no tracer on an arc, day or night: a team-green tube read as a laser at night, a wire through the puffs by day
                    // (r19-r21); the smoke, the rocket head and the mortar's ember carry it   // a thread, not a laser: the head and the smoke carry it (r10)
                    // a rocket leaves its smoke hanging along the way it went, behind a burning head; a mortar round a thin grey thread
                    if (drawn && i > 0)
                    {
                        Vector3 at = ArcPoint(from, end, apex, a);
                        if (rocket)
                        {
                            // two puffs a segment, each its own size, place and life, the older end thinner: evenly spaced equal
                            // puffs read as beads on a string (r17)
                            // as many puffs as it takes to overlap (spaced at most 0.6 of a puff's width): two a segment still sat
                            // in clumps along a long arc (r22)
                            float segLen = Vector3.Distance(ArcPoint(from, end, apex, a), ArcPoint(from, end, apex, b));
                            int puffs = Mathf.Clamp(Mathf.CeilToInt(segLen / (RocketTrail * 0.6f)), 2, q.Epic ? 5 : q.Tier == FxTier.High ? 3 : 2);   // the tier caps the cards (8 s each)
                            for (int n = 0; n < puffs; n++)
                            {
                                uint hh = FxQuality.Hash(salt * 131u + (uint)(j * 256 + i * 8 + n));
                                float u = Mathf.Lerp(a, b, (n + 0.5f) / puffs + (FxQuality.Hash01(hh) - 0.5f) * 0.4f / puffs);
                                Vector3 jit = new Vector3(FxQuality.Hash01(hh + 1u) - 0.5f, FxQuality.Hash01(hh + 2u) - 0.5f, FxQuality.Hash01(hh + 3u) - 0.5f) * 0.6f;
                                books.Add(FlipbookFx.Book.Smoke, ArcPoint(from, end, apex, u) + jit, RocketTrail * Mathf.Lerp(0.7f, 1.3f, FxQuality.Hash01(hh + 4u)),   // the filled grey puff: the wreck's book is ink rings, a chain in a line (r20); big enough that neighbours merge (r21)
                                    RocketTrailLife * Mathf.Lerp(0.75f, 1.25f, FxQuality.Hash01(hh + 5u)), (hh & 1u) != 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                                    velocity: wake, grow: 2.2f, alpha: 0.6f * Mathf.Lerp(0.6f, 1f, u), delay: lag + u * ArcSeconds);   // dark (the wreck's book: the Smoke book read as snow haze), it hangs and leans down wind
                            }
                        }
                        else
                            books.Add(FlipbookFx.Book.WreckSmoke, at, 1.8f, 2.4f, (i & 1) == 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                                velocity: Vector3.up * 0.2f, grow: 1.2f, alpha: 0.8f, delay: lag + a * ArcSeconds);   // grey-dark, so it survives the day
                    }
                }
                if (!rocket && drawn)   // the mortar round: a small ember flying the chord, in place of the tracer (a fire drawing: its soot rim reads on snow)
                {
                    Vector3 start = ArcPoint(from, end, apex, 0.1f), chord = end - start;
                    var cam = Camera.main;
                    books.Add(FlipbookFx.Book.RocketHead, start, RocketHeadWidth * (SceneMood.Night ? 0.5f : 0.75f), ArcSeconds * 0.9f, velocity: chord / ArcSeconds,   // by day larger: lost on snow at 0.5 (r22)
                        roll: cam != null ? FlipbookFx.ScreenRoll(cam, chord) : 0f, glow: SceneMood.Night ? 1.8f : 1.2f, delay: lag);
                }
                if (rocket && drawn)
                {
                    // its burning head: the RocketHead book (a hot head, the tail back to the root) laid along the chord it flies
                    // (loop 2: the additive Muzzle card rolled flat read as a spark, not a rocket)
                    Vector3 start = ArcPoint(from, end, apex, 0.1f), chord = end - start;
                    var cam = Camera.main;
                    float roll = cam != null ? FlipbookFx.ScreenRoll(cam, chord) : 0f;
                    books.Add(FlipbookFx.Book.RocketHead, start, RocketHeadWidth, ArcSeconds * 0.9f, velocity: chord / ArcSeconds,
                        roll: roll, glow: SceneMood.Night ? 2.4f : 1.5f, delay: lag);
                }
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
