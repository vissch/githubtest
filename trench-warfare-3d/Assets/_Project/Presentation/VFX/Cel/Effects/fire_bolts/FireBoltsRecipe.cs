// Phase: CelFx job (2026-11-09, lane/show/cel-fire-bolts; b04: 36 bolts, 8 already in flight at the fire, lanes spread in height; small round heads with long thin flame streaks; orange spark starbursts on the dummy; torso-tall ground fires; tall leaning smoke columns on ground hits; b03 beach round: 28 bolts in four low lanes from the left edge,
// fatter flame bodies, no lit disc on the dummy, wide ground bursts that flicker out, smoke as inked Lobe columns; b02: round heads with flaring tails, opening
// bolts already in flight at the fire, spark starbursts instead of a disc on the dummy, tall pointed ground flames,
// smoke as chains of round lobes) - fire bolts (lab only): a volley of comet fire bolts from
// the upper left on slightly arcing paths, staggered in height, depth and flight time, onto the dummy and the ground
// beside it. Each is a small white-hot head with a soft yellow-orange bloom, a flame tail that breaks into thin red
// streaks, and a long wavy white-grey smoke ribbon, all one Comet record (CometKind). Each hit: a small star flash, a
// starburst of thin yellow spark streaks and orange-yellow flame tongues; ground hits burst into flames about 0.9 m
// tall that flicker out, then thin curling grey smoke strands (Comet strand mode) rise, drift right along the shot,
// grow and break up. A dim brown-warm pool of light (Comet pool mode) lies on the ground just left of and in front
// of the dummy, ramping up with the volley and fading smoothly. Reference: fire_hitting_cel (spec/fire_bolts.json).
//
// Every bolt is two CelFx.Play calls: its comet (aimed a little above the hit so its gravity brings it down onto the
// hit: an arc) and its hit recipe, both shifted by the bolt's launch time, so the whole volley is spawned at the
// fire and nothing runs per frame. Kinds: Comet, Streak, Glow (three draws, no new kind).
// LOD: Close = everything; Mid = one strand, fewer sparks, tongues and flames; Far = the comet, a flash and the pool
// (and every other bolt, see FireBoltsRange).
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class FireBolts
    {
        /// <summary>Mean seconds from launch to hit.</summary>
        public const float Flight = 1.15f;   // p26: slower (REF 40-45 px/frame, p25 was 45-52)   // p21: slower (REF ~45 px/frame, 15-17 frames across)
        /// <summary>The comet's gravity (m/s2): the Range aims it ArcLift(flight) above its hit so it lands on it.</summary>
        public const float ArcGravity = 3.4f;   // p26: 5.0 x (0.95/1.15)^2: the same arc at the slower flight   // p21: REF's paths bend down harder near the hit   // b04: 2.5 -> 3.2 (the high lanes arc down)   // b03: 4 -> 2.5 (lower lanes: the reference's paths are nearly level, bending slightly down)
        public static float ArcLift(float flight) { return 0.5f * ArcGravity * flight * flight; }

        /// <summary>Launch times (s after the fire) and whether the bolt lands on the ground (else on the dummy): an
        /// opening flurry, a thinner middle, and a dense late burst landing about 2.9 s after the fire (the
        /// reference's peak, frame 90).</summary>
        public static readonly float[] Launch = MakeLaunch();
        public static readonly bool[] OnGround = MakeGround();
        /// <summary>Each bolt's flight time (s): staggered so the volley never flies as a rigid row.</summary>
        public static readonly float[] FlightOf = MakeFlight();

        // b03: 28 bolts every ~0.1 s from the fire to 2.7 s (b02: 22 with a thin middle): 4-6 in flight over the
        // reference's whole plateau (f19-f85); two in five land on the ground
        // b04: 36 bolts: the first 8 within 0.2 s of the fire (already in flight, AlongAt), then one every ~0.087 s to
        // 2.6 s, so 3+ are on screen in the first frame and 5+ by the reference's f25; no ground hits launched after
        // 2.25 s (their fires and smoke must be gone by f120)
        const int Count = 21, Opening = 3;   // p21: 8 groups of 3 (REF launches in pairs/trios with gaps)   // p06: REF has ~5 in flight at its peak (f60)   // p01 (by eye): REF has 2-4 bolts in flight at once, never a band of 8   // d10: 42 (two late dummy hits ~f108-f120, the ref still bursts on the dummy there)   // b05: 40 bolts, 12 already in flight at the fire
        // Entry frames measured from the reference: pairs and isolated shots, with gaps
        // through the middle. Regular trios filled the screen with an unbroken fan.
        static float LaunchAt(int i) => Launch[i];
        static float[] MakeLaunch() => new float[]
        {
            0f, 0.05f, 0.10f, 0.11f, 0.19f, 0.32f, 0.45f,
            0.65f, 0.90f, 0.95f, 1.15f, 1.32f, 1.53f, 1.57f,
            1.70f, 1.90f, 2.07f, 2.36f, 2.40f, 2.60f, 2.44f,
        };
        static bool[] MakeGround()
        {
            var a = new bool[Count];
            for (int i = 0; i < Count; i++) a[i] = ((i % 5) == 1 || (i % 5) == 3) && LaunchAt(i) < 2.4f;   // c01: 2.25 -> 2.7 (smoke still up at the ref's f113-f121)
            return a;
        }
        static float[] MakeFlight()
        {
            var a = new float[Count];
            for (int i = 0; i < Count; i++) a[i] = Flight + ((i * 3) % 7 - 3) * 0.05f - (i < Opening ? 0.08f : 0f);
            return a;
        }
        /// <summary>The opening bolts are already part way along their line at the fire (the reference shows three in
        /// flight with full tails by its f19): bolt i starts AlongAt[i] of the way, flies the rest of its line in
        /// (1 - AlongAt[i]) of its flight time, and draws its tail behind its birth (negative Stretch).</summary>
        public static readonly float[] AlongAt = { 0.3f, 0.18f, 0.06f };   // p21: 3 opening bolts, entering from the left edge   // p01: 6 opening bolts   // b05: 12 (b04: 8)   // b04: 8 (b03: 5)
        /// <summary>Bolt i's actual flight time (s): FlightOf shortened for the opening bolts.</summary>
        public static readonly float[] Flights = new float[Launch.Length];

        static FireBolts()
        {
            // additive light on the dark field: c3 is what the pool adds at its middle (dim brown-warm, #c16a42-ish on the ground)
            CelPalette.Add("fb_pool", CelPalette.Hex(0x2A1C14), CelPalette.Hex(0x4A3424), CelPalette.Hex(0x5A4232), CelPalette.Hex(0x86664C),
                CelPalette.Hex(0x86664C), 1.1f, 4, CelPalette.Hex(0x000000));
            // the comet: deep red at the streaks' tips, red-orange, orange, yellow behind the head, a white-hot core
            CelPalette.Add("fb_comet", CelPalette.Hex(0xC58C78), CelPalette.Hex(0xFA8E7C), CelPalette.Hex(0xFBC69A), CelPalette.Hex(0xFFF1C2),   // p21b: pre-compensated for the kit's .linear store on a gamma project (wanted 853F2D / E14130 / E2874E / F9CF81)   // p21: the REF's own k-means colours (red, salmon, pale orange)   // p13: the saturated red-orange of the REF 4x crops   // d09: toward the ref #c16a42 / #f3cf86
                CelPalette.Hex(0xFFFFD2), 0.84f, 4, CelPalette.Hex(0x1E0806));   // p21b: (wanted FDEA99)   // p21: a pale yellow head, not white
            CelPalette.Add("fb_ribbon", CelPalette.Hex(0xC8B8B4), CelPalette.Hex(0xD8CCCA), CelPalette.Hex(0xE4DAD8), CelPalette.Hex(0xF0E8E4),   // p31c: pale grey smoke with a warm hint (REF), pre-compensated   // p23b: pale pink-cream (wanted A88880 / C8A8A0)   // p21b: pre-compensated (wanted 6A4E4C / 8E6A66 / B59590 / DCC8C0)   // p21: the REF's smear is a dusty pink-white line   // c05: soft grey (was near-white)
               
                CelPalette.Hex(0xFCF8F4), 1.1f, 3, CelPalette.Hex(0x3A3438));
            // hit sparks: yellow-orange, a pale yellow core (never white)
            CelPalette.Add("fb_spark", CelPalette.Hex(0x9A3A10), CelPalette.Hex(0xE0681C), CelPalette.Hex(0xFFA02E), CelPalette.Hex(0xFFD04A),
                CelPalette.Hex(0xFFE27A), 0.9f, 3, CelPalette.Hex(0x2A0C06));
            // p27h: hit dashes: bright orange to white, pre-compensated for the kit's .linear store (fb_spark's E0681C rendered B02102: a dim brown-red dash)
            CelPalette.Add("fb_dash", CelPalette.Hex(0xFFC17C), CelPalette.Hex(0xFFDF8D), CelPalette.Hex(0xFFF5B6), CelPalette.Hex(0xFFFFD6),
                CelPalette.Hex(0xFFFFFF), 1.0f, 3, CelPalette.Hex(0x2A0C06));   // wanted F08030 / FFB040 / FFD870 / FFF0A0, core white
            // grey wispy smoke (#726369 body)
            CelPalette.Add("fb_smoke", CelPalette.Hex(0x5A4F55), CelPalette.Hex(0x726369), CelPalette.Hex(0x8C8086), CelPalette.Hex(0xA59AA0),   // c05: the ref's #726369 grey (was near-white)
                CelPalette.Hex(0xB0A6AA), 1.1f, 2, CelPalette.Hex(0x1C171A));   // b03: two bands, a near-black ink (the reference's smoke is inked)
            // d01: the path-ribbon wisp: pale white-grey lit side, the ref's #726369 grey shade, a dark grey ink
            CelPalette.Add("fb_wisp", CelPalette.Hex(0x5E565C), CelPalette.Hex(0x7A7076), CelPalette.Hex(0xA29BA0), CelPalette.Hex(0xCBC6C8),
                CelPalette.Hex(0xE4E0E0), 1.1f, 3, CelPalette.Hex(0x2E282C));
            Comets = new CelFxRecipe[Launch.Length]; CometsMid = new CelFxRecipe[Launch.Length]; CometsFar = new CelFxRecipe[Launch.Length];
            Hits = new CelFxRecipe[Launch.Length];
            Smokes = new CelFxRecipe[Launch.Length];
            SmokesMid = new CelFxRecipe[Launch.Length];
            int gfires = 0;
            for (int i = 0; i < Launch.Length; i++)
            {
                bool pre = i < AlongAt.Length;
                Flights[i] = pre ? FlightOf[i] * (1f - AlongAt[i]) : FlightOf[i];
                float bw = OnGround[i] ? (i % 3 == 0 ? 1f : 0.7f) : 0.55f;   // Measured reference head size and flame length. p28c: only every third ground lane is the REF's wide near bolt; the rest are slender streaks   // p28b
                Comets[i] = MakeComet(Launch[i], Flights[i], pre, 1f, bw); CometsMid[i] = MakeComet(Launch[i], Flights[i], pre, 1.7f, bw); CometsFar[i] = MakeComet(Launch[i], Flights[i], pre, 2.6f, bw);   // d49: bigger bolts at Mid/Far (O120/O240 lost them)
                Hits[i] = MakeHit(Launch[i] + Flights[i], OnGround[i], OnGround[i] && Launch[i] + Flights[i] > 1.15f && (gfires++ & 1) == 0, DummyLightShare(i));
                Smokes[i] = MakeSmoke(Launch[i] + Flights[i], i);   // played on ground hits only (the film: the lobe chain)
                SmokesMid[i] = MakeSmoke(Launch[i] + Flights[i], -1);   // c01: T1 plays the b05 strand (no Lobe draw added at T1)
            }
        }

        // templates (the tests find and validate these)
        public static readonly CelFxRecipe Comet = MakeComet(0f, Flight, false);
        public static readonly CelFxRecipe DummyHit = MakeHit(Flight, false);
        public static readonly CelFxRecipe GroundHit = MakeHit(Flight, true);
        /// <summary>The soft pool under the whole volley: brightest ~2.95 s after the fire, gone by ~4.5 s.</summary>
        public static readonly CelFxRecipe Pool = MakePool(0f);
        /// <summary>The volley: bolt i's comet and its hit, shifted by Launch[i].</summary>
        public static readonly CelFxRecipe[] Comets, Hits, CometsMid, CometsFar;
        /// <summary>b03: bolt i's smoke column (the Range plays it on ground hits only), with dir = up and a little
        /// screen-right, so the column leans along the shot.</summary>
        public static readonly CelFxRecipe[] Smokes, SmokesMid;
        public static readonly CelFxRecipe Smoke = MakeSmoke(Flight, 0);
        public static readonly CelFxRecipe SmokeMid = MakeSmoke(Flight, -1);

        const float PoolLife = 5.4f, PoolRadius = 7.0f;   // p06: a broader wash   // c01: peak 3.25 s after a 0.5 s fire = the ref's f90

        static CelFxRecipe MakePool(float start)
        {
            return new CelFxRecipe("fire_bolts_pool", start + PoolLife,
                new CelLayer
                {
                    Name = "warm ground light", Kind = CometKind.KindName, Start = start, Life = PoolLife, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 1, Size = new CelCurve(0.85f * PoolRadius, PoolRadius, 0.5f), SizeJitter = 0f,
                    Palette = "fb_pool", Core = 1f, Opacity = 0.85f,   // p01: softer (REF's ground light is a faint wash)
                    // pool mode, falloff power 1.2, peak at 2.95 s into its life (f90), fade from 3.15 s (f95) to f137
                    Extra = new Vector4(-1f, 1.2f, 3.25f / PoolLife, 3.45f / PoolLife),
                });
        }

        static CelFxRecipe MakeComet(float t0, float flight, bool preflown, float k = 1f, float w = 1f)   // p28b: w scales the head (dummy bolts 0.6: REF's are thin)   // d49: k scales the bolt for the Mid/Far tiers
        {
            return new CelFxRecipe("fire_bolt_comet", t0 + flight + Hold,
                new CelLayer
                {
                    Name = "comet", Kind = CometKind.KindName, Start = t0, Life = flight + Hold, LifeJitter = 0f, Arrive = flight / (flight + Hold),   // p27j: lives Hold s past the hit (the tail streams in, shader BOLT_HOLD), still arrives at `flight`
                    Close = 1, Mid = 1, Far = 1, Aim = CelAim.Target, Speed = 0f, Spread = 0f, Spin = 4.5f, Gravity = ArcGravity,   // p21: Spin = the swirl's rate (rad/s)
                    // b01 (beach, 19 m camera): ribbon 0.2 m wide, 0.75 s of its speed long (a third to half the frame)
                    // b02: a negative Stretch draws the tail behind the birth (the opening bolts were in flight before the fire)
                    Stretch = preflown ? -0.72f : 0.55f, Size = new CelCurve(0.14f * k * (0.75f + 0.2f * Mathf.Clamp01((w - 0.45f) / 0.55f)), 0.15f * k * (0.75f + 0.2f * Mathf.Clamp01((w - 0.45f) / 0.55f))), SizeJitter = 0.2f, // Deposited history; the last volley clears its shorter trails sooner.
                      // c01: 0.2 -> 0.26-0.3 m (the smoke trail frays wider)
                    Erode = new CelCurve(0.06f, 0.2f),   // d18: stays whole (d17 dashes lost)   // c03: the ribbon breaks into pieces (was whole)
                    Palette = "fb_comet", CoolTo = "fb_ribbon", Core = 1.0f,   // p21: a pale yellow head (REF), no white blaze   // p02: the head blazes   // b03: 2.6 -> 2.2 (the bodies read white-hot)
                    Opacity = 0.9f, Ink = 0f,   // p32c: grey 140 at the core (p32b measured 134)   // p32b: alpha-blended now   // p32: grey 140 on 42 at the band's core (REF)   // p31c: translucent grey (REF)   // p31: the strands carry their own translucency   // p29c: pale grey, not white   // p29: semi-transparent (REF)   // p27e   // p23b: a denser smear   // p01: translucent trail, NO ink on the bolt (REF has none)   // d18: ribbon 75% (d16 opaque bands lost)
                    // b02: a round white-yellow head 0.2 m, a flame body that flares and tapers over 2.2 m, thin red
                    // streaks fading out to 5 m, a ribbon waving 0.08 m over 5.5 m
                    Extra = new Vector4(0.2f * k * w, 5.0f * k, 0.22f * w, t0 > 2.3f ? 0.34f : 0.5f), // Head radius, body length, path swirl, smoke fade seconds.
                });
        }

        /// <summary>Seconds a comet lives past its hit, its head frozen there while the smoke clears from its oldest end (= shader BOLT_HOLD).</summary>
        public const float Hold = 0.5f; // Must match shader BOLT_HOLD.

        // Overlapping hits share one reference-sized illumination peak. The GPU
        // sprites still vary independently; several point lights must not wash the board out.
        static float DummyLightShare(int index)
        {
            float at = Launch[index] + Flights[index], sum = 0f;
            for (int j = 0; j < Launch.Length; j++)
            {
                if (OnGround[j]) continue;
                float f = j < AlongAt.Length ? FlightOf[j] * (1f - AlongAt[j]) : FlightOf[j];
                float proximity = Mathf.Max(0f, 1f - Mathf.Abs(at - Launch[j] - f) / 0.4f);
                sum += proximity * proximity;
            }
            return 1f / Mathf.Max(1f, sum);
        }

        static CelFxRecipe MakeHit(float hit, bool ground, bool bigFire = true, float lightShare = 1f)
        {
            // b02: no glowing disc on the dummy (the critic read it as a flat white plate): a dummy hit is a starburst of
            // 24 thin bright spark streaks (6-10 frames) and a few flame tongues; a ground hit keeps a small short flash
            var r = new CelFxRecipe(ground ? "fire_bolt_ground" : "fire_bolt_dummy", hit + 2f,
                new CelLayer
                {
                    Name = "spark starburst", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit, Duration = 0.10f, Life = ground ? 0.42f : 0.65f, LifeJitter = 0.25f,
                    Close = ground ? 22 : 6, Mid = ground ? 22 : 6,   // p28e: a control film with 0 showed the spray IS this layer (the dash counter was fooled by the thin bolts' lines): REF has 10-15 bold dashes   // p28d: 100 counted 56-70 a frame beside the thin bolts' lines (REF 11-19)   // p27m: 130 counted 21-35 (REF 11-19)   // p27k: 160 counted 20-34 a frame (REF 7-19)   // p27i: the close film sits at zoom 30 = the Mid blend point, so Mid IS the count there (6 drew six dashes)   // p27g: 280 per hit wrapped the Streak ring pool in ~0.25 s and killed the live dashes   // p16: dense short dashes on the dummy   // p04: REF's ground hits are spark starbursts   // d06 (from d04): denser sprays   // c03: a fuller radial spray / dummy spark fan
                    Far = 0, Shape = CelShape.Shell, ShapeSize = new Vector3(ground ? 0.10f : 0.05f, 0f, 0f), Aim = ground ? CelAim.Up : CelAim.Back, Spread = ground ? 58f : 62f,
                    Speed = ground ? 2.6f : 3.2f, SpeedJitter = 0.45f, UpBias = 0f, Gravity = 2.4f, Drag = ground ? 1.5f : 1.3f,   // p27j: 9 m/s flew out of the crop in 8 frames (REF: within 150 px, 12+ frames)   // p27i: a fast burst that brakes hard (REF: out of the ball in 2 frames, then slow)   // p27e: drag 2.5 shrank them to dots by 8 frames   // p27d: slower (REF)   // c01: born on a shell (no pile-up disc at the middle), ground sparks fan upward
                    // b04: short orange dots and streaks over the torso's width (b03 lost its palette, erosion and core
                    // to a comment, so 28 default-white streaks piled up into a disc at the chest)
                    Size = new CelCurve(ground ? 0.10f : 0.16f, ground ? 0.035f : 0.06f), SizeJitter = 0.35f, Stretch = ground ? 0.065f : 0.045f,   // p28e: bolder, longer (REF)   // p27l: 4 px (REF 3-4)   // p27j: 3-4 px wide, 13 px long at birth, 6 once braked (REF 6-11)   // p27i: 9 m/s x 0.05 s = 30 px at first, 12 px once braked (REF)   // p27f: 2.6 px wide (1.5 px AA'd away), 0.14 s x 3.5 m/s = 32 px   // p27e: Size is the WIDTH (1.5 px); length = speed x 0.07 s = 8-32 px (REF 10-40)   // p27c: Stretch is seconds of motion (0.3 s drew 4 m rods)   // p22: bolder dashes   // p06: REF's dummy sparks are short bright dashes   // c01: thinner, longer streaks (b05: 0.05, 0.09)
                    Erode = new CelCurve(0.02f, 0.45f), Palette = "fb_comet", Core = 1.35f, Extra = new Vector4(0.45f, 0.07f, 0.08f, 0.35f), Ink = 0f,   // p27l: brighter (REF: yellow-white)   // p27j: min length 0.03 m   // p27e: a 60% tail (0 drew darts), less flicker   // p27d: yellow-white   // p10: no ink (REF's sparks are soft bright dashes)
                },
                new CelLayer
                {
                    Name = "flame tongues", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit + (ground ? 0f : 0.14f), Duration = 0.08f, Life = ground ? 0.24f : 0.45f, LifeJitter = 0.25f,
                    Close = ground ? 4 : 2, Mid = ground ? 4 : 2, Far = 0,   // p27m: fewer, bigger   // p27j   // p27i: Mid = Close (zoom 30)   // c01: one tongue on the dummy (three lit a disc on the chest)
                    Shape = CelShape.Sphere, ShapeSize = new Vector3(0.1f, 0f, 0f), Aim = ground ? CelAim.Up : CelAim.Back, Spread = ground ? 50f : 68f, Speed = ground ? 1.1f : 3.2f, SpeedJitter = 0.4f, Drag = ground ? 0f : 1.0f, Buoyancy = ground ? 0.6f : 0f,   // p27l: grow out 0.15 m and stop (REF: attached spikes)
                    Size = new CelCurve(ground ? 0.07f : 0.20f, ground ? 0.02f : 0.02f, 0.8f), SizeJitter = 0.3f, Stretch = ground ? 0.16f : 0.12f,   // p27m: 0.17 m, 11 px at the base   // p27l: 0.12 m wedges, 8 px at the base   // p27j: 0.08 m wedges   // p27e: thin spikes 16 px long Erode = new CelCurve(0.05f, 0.5f), Palette = ground ? "fire" : "fb_dash", Core = ground ? 1.3f : 1.7f, Ink = 0f, Extra = new Vector4(ground ? 0f : 1f, ground ? 0.12f : 0.1f, 0.35f, ground ? 0f : 0.9f),   // p27l: min 0.1 m, brighter   // p27j: a wedge wide at the ball, pointed outward (Streak extra.w)   // p27: rays round the fireball; the p05 comment had swallowed everything after Size since p05
                    Erode = new CelCurve(0.05f, 0.5f), Palette = "fb_comet", Core = 1.25f, Ink = 0f, Extra = new Vector4(ground ? 0f : 1f, 0.10f, 0.20f, ground ? 0f : 0.9f),
                },
                new CelLayer
                {
                    Name = "light", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = hit, Life = 0.4f,
                    Close = 1, Mid = 1, Far = 0, Palette = "fb_comet", Core = ground ? 0.35f : 0.28f * lightShare, Extra = new Vector4(4f, ground ? 0f : 0.04f, 0f, 0f), // Share the warm target pulse; suppress the stock flash card.
                });
            r.Layers.Add(new CelLayer
            {
                Name = "hit flash", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Start = hit, Life = 0.17f, LifeJitter = 0.04f,
                Close = 0, Mid = 0, Far = 0, Size = new CelCurve(0.20f, 0.31f, 0.5f), SizeJitter = 0.15f,
                Palette = "fb_comet", Core = 1.2f, Opacity = 0.4f, Extra = new Vector4(-4f, 0f, 0f, 0f),
            });
            for (int h = 0; h < 1; h++) r.Layers.Add(new CelLayer   // p22: a HARD ragged cel splash over the soft flash (Comet mode -4, extra.y = 1): the harsh half of a hit
            {
                Name = "hit splash hard", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0.06f * h - 0.03f, ground ? 0.15f : 0.04f * h, 0f), Start = hit + 0.04f * h, Life = ground ? 0.4f - 0.08f * h : (hit > 3.5f ? 1.19f : 0.95f) - 0.08f * h, LifeJitter = 0.1f,
                Close = 1, Mid = 1, Far = 0, Size = ground ? new CelCurve(0.56f - 0.13f * h, 0.30f, 0.5f) : new CelCurve(1.45f - 0.22f * h, 0.90f, 0.5f), SizeJitter = 0.15f,   // p28: a 114 px quad: the splat's base 34 px, the white blob 18 px, fingers to 80 px (REF)   // p27k: 0.95 blew out   // p27j: r 35-42 px (REF)   // p22c: a big splash on the board
                Palette = "fb_comet", Core = 1.15f, Opacity = 1f, Extra = new Vector4(-4f, 1f, ground ? 1f : 0f, 0f),
            });
            if (!ground && hit > 3.5f) r.Layers.Add(new CelLayer
            {
                Name = "final flame remnants", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.2f, 0f),
                Start = hit + 1.0f, Life = 0.42f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.60f, 0.16f),
                Palette = "fb_comet", Core = 0.7f, Opacity = 0.68f,
                Extra = new Vector4(-4f, 1f, 0f, 1f),
            });
            // d27 (stall fracture: dummy hit -> its lingering burn; new primitive, Comet mode extra.x = -3): an orange
            // cel-blob flame behind the hit star (d28: the star rays stay on top), then a small ember that burns to ~1.4 s
            if (!ground) r.Layers.Add(new CelLayer   // p16: a soft lingering glow on the board instead of the cel-blob flame wrap (it read as orange lumps)
            {
                Name = "board glow", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.05f, 0.08f), Start = hit + 0.1f, Life = 0.55f, LifeJitter = 0.2f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.22f, 0.14f, 0.7f), SizeJitter = 0.2f,
                Palette = "fb_spark", Core = 1.2f, Opacity = 0.08f, Extra = new Vector4(-4f, 0f, 0f, 0f),   // p26: fainter still (a ring on the board)   // p18: faint (it read as a target ring)
            });
            if (!ground) r.Layers.Add(new CelLayer   // p26: a small standing flame stays on the board after the hit (REF f33-40)
            {
                Name = "board flame", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.05f, 0.08f), Start = hit + 0.08f, Life = 0.55f, LifeJitter = 0.2f,
                Close = 0, Mid = 0, Far = 0, Aim = CelAim.None, Speed = 0f, Gravity = 0.05f, Drag = 0f, Buoyancy = 0f, Spin = 0f,
                Size = new CelCurve(0.06f, 0.16f), SizeJitter = 0.2f, Stretch = -0.1f, Erode = new CelCurve(0.06f, 0.2f),
                Palette = "fb_comet", CoolTo = "fb_ribbon", Core = 1.7f, Opacity = 0.25f, Ink = 0f,
                Extra = new Vector4(0.14f, 0.45f, 0.02f, -0.8f),
            });
            if (!ground) for (int b = 0; b < 0; b++) r.Layers.Add(new CelLayer
            {
                Name = "flame wrap", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0.05f * b - 0.03f, 0.02f + 0.1f * b, 0.08f), Start = hit + 0.06f + 0.06f * b, Life = 0.45f + 0.35f * b, LifeJitter = 0.15f,   // p01: REF's dummy is clean ~0.5 s after its last hit
                Close = 1, Mid = 1, Far = 0, Aim = CelAim.Up, Spread = 20f, Speed = 0.25f, SpeedJitter = 0.3f, Drag = 0.5f, Gravity = 0f, Buoyancy = 0.4f, Spin = 0f,
                Size = new CelCurve(0.14f, 0.36f - 0.24f * b, 0.5f), SizeJitter = 0.2f, Stretch = 0f, Erode = new CelCurve(0f, 0.75f, 1.3f),   // d42: a fan-shaped flame splash on the board (REF f96-f121), torn by noise
                Palette = "fire", Core = 1.6f, Opacity = 0.45f, Ink = 0f, Extra = new Vector4(-3f, 2.1f, 2.8f, 1.3f),   // p04: a yellow-white flash on the board
            });
            if (ground)
            {
                r.Layers.Add(new CelLayer
                {
                    Name = "hit star", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.15f, 0f), Start = hit, Life = 0.16f, LifeJitter = 0.04f,
                    Close = 1, Mid = 1, Far = 1, Size = new CelCurve(0.26f, 0.42f, 0.5f), SizeJitter = 0.15f,
                    Palette = "fb_comet", Core = 1.5f, Opacity = 0.9f, Extra = new Vector4(-4f, 0f, 0f, 0f),
                });
                // b03: a ragged burst of wide flame tongues about the dummy's torso tall, fanned out (b02's were thin
                // upright slivers), then a smaller fire that flickers on and dies about 1 s after the hit
                r.Layers.Add(new CelLayer
                {
                    Name = "ground burst", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit, Duration = 0.08f, Life = 0.25f, LifeJitter = 0.25f,
                    Close = 6, Mid = 6, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.16f, 0f, 0f), Aim = CelAim.Up, Spread = 55f, Speed = 1.8f, SpeedJitter = 0.4f,   // p11: a spray of thin flame spikes Buoyancy = 0.8f,
                    Size = new CelCurve(0.16f, 0.04f, 0.6f),   // p22: bolder spikes   // p12   // p04: small ragged tongues (REF: a spark pop with a small flame, not a yellow fan)   // c05: bigger round bursts (the ref's pop at the landing point)
                    SizeJitter = 0.4f, Stretch = 0.22f,   // p12: long spikes   // c03: round flicker flames on the ground (c01 shot carrots up into the air)   // b04: narrower fan, taller (about the torso)
                    Erode = new CelCurve(0.05f, 0.55f), Palette = "fb_comet", Core = 1.25f, Ink = 0f, Extra = new Vector4(0f, 0.12f, 0.2f, 0f),
                });
                for (int f = 0; f < 2; f++) r.Layers.Add(new CelLayer   // p16: a standing flame (Comet fire mode: extra.w < 0; a tiny gravity points its head down, so the flame body rises)
                {
                    Name = "ground flame", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0.13f * f - 0.05f, 0.10f, 0.04f * f), Start = hit + 0.05f + 0.18f * f, Life = bigFire ? 1.05f - 0.2f * f : 0.46f, LifeJitter = 0.18f,
                    Close = 1, Mid = 1, Far = 0, Aim = CelAim.None, Speed = 0f, Gravity = 0.05f, Drag = 0f, Buoyancy = 0f, Spin = 0f,
                    Size = new CelCurve(0.08f, 0.22f), SizeJitter = 0.2f, Stretch = -0.1f, Erode = new CelCurve(0.06f, 0.2f),
                    Palette = "fb_comet", Core = 1.35f, Opacity = 0.60f, Ink = 0f,
                    Extra = new Vector4((bigFire ? 0.25f : 0.18f) * (f == 0 ? 1f : 0.72f), (bigFire ? 0.82f : 0.58f) * (f == 0 ? 1f : 0.8f), 0.02f, -0.8f),
                });
                if (bigFire) r.Layers.Add(new CelLayer   // d23: the lasting fire body on every other ground hit after ~f40 only (ref: 1-2 fires at once)
                {
                    Name = "ground fire", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit + 0.12f, Duration = 0.75f, Life = 0.6f, LifeJitter = 0.3f,
                    Close = 0, Mid = 0, Far = 0,   // p16: off, the standing flame replaces it Shape = CelShape.Disc, ShapeSize = new Vector3(0.3f, 0f, 0f), Aim = CelAim.Up, Spread = 50f, SpeedJitter = 0.35f, Buoyancy = 0.6f,
                    Speed = 1.2f, Size = new CelCurve(0.42f, 0.1f, 0.7f),   // p12: thin licking tongues   // p04: smaller lasting fire   // d22: low wide dome-then-shreds (ref f87-f101 zoom), not tall tongues   // d21: compact crater fire of long-lived flame tongues (16-20 f each), Mid near Close (LOD trap)   // c03: low round flames ~0.5 dummy tall (c01: 3.7 m/s carrots in a column)   // c01: as tall as the torso (b05: 0.65 m, speed 3.0)   // b04: about the torso tall (b03: under half)
                    SizeJitter = 0.3f, Stretch = 0.25f, Erode = new CelCurve(0.1f, 0.6f), Palette = "fire", Core = 1.7f, Ink = 0f, Extra = new Vector4(0f, 0.15f, 0.4f, 0f),
                });
                if (bigFire) r.Layers.Add(new CelLayer   // d38: a crater ember (cel blob, Comet mode -3): a low licking fire that outlives the burst and dies ~1.2 s after the hit (ref keeps a red ember in its smoke to ~f104)
                {
                    Name = "crater ember", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.12f, 0f), Start = hit + 0.2f, Life = 1.4f, LifeJitter = 0.15f,
                    Close = 0, Mid = 0, Far = 0,   // p10: off - it read as a flat orange leaf floating beside the fire Aim = CelAim.Up, Spread = 10f, Speed = 0.1f, SpeedJitter = 0.3f, Drag = 0.5f, Gravity = 0f, Buoyancy = 0.15f, Spin = 0f,
                    Size = new CelCurve(0.55f, 0.25f, 0.8f), SizeJitter = 0.2f, Stretch = 0f, Erode = new CelCurve(0f, 0.6f, 1.4f),
                    Palette = "fire", Core = 1.0f, Opacity = 0.9f, Ink = 2f, Extra = new Vector4(-3f, 1.6f, 2.6f, 0.9f),
                });
                r.Layers.Add(new CelLayer
                {
                    Name = "fire core", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.2f, 0f), Start = hit, Life = 0.52f, LifeJitter = 0.15f,
                    Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.32f, 0.14f, 0.6f), SizeJitter = 0.15f,
                    Palette = "fb_comet", Core = 1.6f, Opacity = 0.7f,
                    Extra = new Vector4(-4f, 0f, 0f, 0f),   // p12: a smooth white-hot base   // b04: the flash mode b03 lost to a comment: a white-hot base
                });
                r.Layers.Add(new CelLayer
                {
                    Name = "fire glow", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Start = hit, Life = 1.0f, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.9f, 0.7f), SizeJitter = 0.1f,
                    Palette = "fb_pool", Core = 1f, Opacity = 0.12f, Extra = new Vector4(-1f, 1.6f, 0.08f, 0.35f),   // p11: the red disc under the fire was far too strong
                });
            }
            return r;
        }

        // b03: a smoke column per ground hit, from the kit's Lobe kind (b02's puff chains read as single beans): a
        // stream of inked lobes, lit from above in the palette's two grey bands, rising about 1.5 dummy heights, leaning
        // along dir (up and screen-right), growing and eroding away until gone ~1.3 s after the hit
        // c01: the smoke column is a chain of overlapping inked lobes laid along one S-curve (b05's strand broke into
        // flat pale squiggle pieces with no visible ink): the lobes' ink hulls merge into one outlined column that
        // rises from the fire (each lobe is born when the column's front reaches its height), leans screen-right,
        // widens and then thins toward its top, and breaks up from the top down near the end. Close only: Mid keeps
        // the b05 strand (no Lobe draw at T1), Far none.
        const int SmokeLinks = 10;   // c03: separate soft puffs curling up (c01: 26 merged into lumpy columns)
        const float SmokeHeight = 2.3f, SmokeRise = 2.4f, SmokeEnd = 2.2f;   // c03: ~1.3 dummy heights, lasting to the ref f134
        static CelFxRecipe MakeSmoke(float hit, int variant)
        {
            var r = new CelFxRecipe("fire_bolt_smoke", hit + 2.4f);
            if (variant < 0)
            {
                r.Layers.Add(new CelLayer
                {
                    Name = "smoke ribbon (mid)", Kind = CometKind.KindName, Anchor = CelAnchor.Origin, Offset = new Vector3(0f, 0.1f, 0.2f), Start = hit + 0.1f, Life = 1.6f, LifeJitter = 0.1f,
                    Close = 1, Mid = 1, Far = 0, Aim = CelAim.Up, Spread = 6f, Speed = 1.7f, SpeedJitter = 0.15f, Drag = 0f, Gravity = 0f, Buoyancy = 0.7f, Spin = 2.2f,
                    Size = new CelCurve(0.22f, 0.62f, 0.8f), SizeJitter = 0.2f, Stretch = 4f, Erode = new CelCurve(0f, 0.9f, 2.2f),
                    Palette = "fb_smoke", CoolTo = "fb_smoke", Core = 1f, Opacity = 0.7f, Ink = 2f,
                    Extra = new Vector4(0f, -1f, 0.22f, 1.5f),
                });
                return r;
            }
            // d01: smoke as path ribbons (Comet mode extra.x = -2): two soft grey wisps per smoked hit, each a strip
            // bent along a curling path that climbs ~1.4 dummy heights, drifts screen-right, widens from a thin
            // thread to a frayed band, tears into pieces and fades (replaces the Lobe bead columns at Close)
            for (int k = 0; k < 1; k++)
            {
                r.Layers.Add(new CelLayer
                {
                    Name = "smoke wisp", Kind = CometKind.KindName, Anchor = CelAnchor.Origin, Offset = new Vector3(0.12f * k - 0.05f, 0.05f, 0.1f * k), Start = hit + 0.06f + 0.14f * k, Life = Mathf.Min(1.95f - 0.22f * k, Mathf.Max(0.65f, 4.8f - hit - 0.14f * k)), LifeJitter = 0.04f,
                    Close = 1, Mid = 1, Far = 0, Aim = CelAim.None, Speed = 0f, Gravity = 0f, Drag = 0f, Buoyancy = 0f, Spin = 1.1f + 0.3f * k,
                    Size = new CelCurve(0.10f, 3.2f, 1.25f), SizeJitter = 0.15f, Stretch = 3.3f,
                    Erode = new CelCurve(0.02f, 0.62f, 2.8f),
                    Palette = "fb_wisp", Core = 1f, Opacity = 0.82f, Ink = 0f,
                    Extra = new Vector4(-2f, 0.23f, 0.38f, 2.4f + 0.4f * k),   // d32: one broad leaning curl, not a tight corkscrew   // d07: gentler S, more lean (the wider puffs fold on tight curls)
                });
            }
            if (r.Layers.Count > 0) return r;
            float phase = variant * 1.7f, sway = 0.42f + 0.1f * (variant % 3);   // c01_3: a wider S (0.22 m was lost in the column's width)
            for (int k = 0; k < SmokeLinks; k++)
            {
                float u = k / (float)(SmokeLinks - 1), h = u * SmokeHeight;
                // lean right about 0.55 m per metre (c05: 0.3), an S of 0.22-0.34 m that grows with height
                float x = 0.55f * h + sway * Mathf.Sin(h * 2.2f + phase) * (0.3f + 0.7f * u);   // c03: tighter curl
                float born = 0.08f + h / SmokeRise;
                float life = Mathf.Max(0.45f, SmokeEnd - born);
                // radius: 0.12 m at the fire, widest (0.25 m) at 55% of the height, thinning to 0.11 m at its top
                float rad = Mathf.Lerp(0.22f, 0.42f, Mathf.Clamp01(u / 0.5f)) * (u > 0.5f ? Mathf.Lerp(1f, 0.7f, (u - 0.5f) / 0.5f) : 1f);   // c03: dummy-wide puffs
                r.Layers.Add(new CelLayer
                {
                    Name = "smoke link", Kind = CelKinds.LobeName, Anchor = CelAnchor.Origin, Offset = new Vector3(x, 0.05f + h, 0f), Start = hit + born, Life = life, LifeJitter = 0.05f,
                    Close = 1, Mid = 1, Far = 0, Shape = CelShape.Point, Aim = CelAim.Up, Spread = 25f, Speed = 0.35f, SpeedJitter = 0.3f, Drag = 0.4f, Buoyancy = 0.3f, Spin = 1.0f,
                    Size = new CelCurve(rad * 0.55f, rad * 1.15f, 0.5f), SizeJitter = 0.12f, Stretch = 0f,
                    // the top breaks up first: its erosion starts higher and the whole column goes in the last third
                    Erode = new CelCurve(0.1f * u, 1f, 3.6f - 1.2f * u), Feather = 0.2f,   // c03: softer edge
                    Palette = "fb_smoke", Core = 1f, Ink = 4f, Extra = new Vector4(1.0f, 0.12f, 0.9f, 0f),   // c03: smoother puffs (lump 0.22 read as cauliflower)   // c01_3: rounder billows (lump amp 0.16 -> 0.22)
                });
            }
            return r;
        }
    }
}
