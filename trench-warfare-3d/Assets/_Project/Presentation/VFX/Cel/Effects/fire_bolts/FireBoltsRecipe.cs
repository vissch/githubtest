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
        public const float Flight = 0.8f;
        /// <summary>The comet's gravity (m/s2): the Range aims it ArcLift(flight) above its hit so it lands on it.</summary>
        public const float ArcGravity = 3.2f;   // b04: 2.5 -> 3.2 (the high lanes arc down)   // b03: 4 -> 2.5 (lower lanes: the reference's paths are nearly level, bending slightly down)
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
        const int Count = 26, Opening = 6;   // p06: REF has ~5 in flight at its peak (f60)   // p01 (by eye): REF has 2-4 bolts in flight at once, never a band of 8   // d10: 42 (two late dummy hits ~f108-f120, the ref still bursts on the dummy there)   // b05: 40 bolts, 12 already in flight at the fire
        static float LaunchAt(int i) { return i < Opening ? i * 0.03f : 0.2f + (i - Opening) * 0.165f + ((i * 7) % 5 - 2) * 0.02f; }   // p01: one every ~0.185 s to 2.6 s (3-4 in flight)   // c01: to 3.1 s (bolts arrive to the ref's f100; the stage fires at 0.5 s)
        static float[] MakeLaunch()
        {
            var a = new float[Count];
            for (int i = 0; i < Count; i++) a[i] = LaunchAt(i);
            return a;
        }
        static bool[] MakeGround()
        {
            var a = new bool[Count];
            for (int i = 0; i < Count; i++) a[i] = ((i % 5) == 1 || (i % 5) == 3) && LaunchAt(i) < 2.7f;   // c01: 2.25 -> 2.7 (smoke still up at the ref's f113-f121)
            return a;
        }
        static float[] MakeFlight()
        {
            var a = new float[Count];
            for (int i = 0; i < Count; i++) a[i] = 0.8f + ((i * 3) % 7 - 3) * 0.04f;
            return a;
        }
        /// <summary>The opening bolts are already part way along their line at the fire (the reference shows three in
        /// flight with full tails by its f19): bolt i starts AlongAt[i] of the way, flies the rest of its line in
        /// (1 - AlongAt[i]) of its flight time, and draws its tail behind its birth (negative Stretch).</summary>
        public static readonly float[] AlongAt = { 0.62f, 0.4f, 0.2f, 0.5f, 0.3f, 0.1f };   // p01: 6 opening bolts   // b05: 12 (b04: 8)   // b04: 8 (b03: 5)
        /// <summary>Bolt i's actual flight time (s): FlightOf shortened for the opening bolts.</summary>
        public static readonly float[] Flights = new float[Launch.Length];

        static FireBolts()
        {
            // additive light on the dark field: c3 is what the pool adds at its middle (dim brown-warm, #c16a42-ish on the ground)
            CelPalette.Add("fb_pool", CelPalette.Hex(0x2A1C14), CelPalette.Hex(0x4A3424), CelPalette.Hex(0x5A4232), CelPalette.Hex(0x86664C),
                CelPalette.Hex(0x86664C), 1.1f, 4, CelPalette.Hex(0x000000));
            // the comet: deep red at the streaks' tips, red-orange, orange, yellow behind the head, a white-hot core
            CelPalette.Add("fb_comet", CelPalette.Hex(0xC8281A), CelPalette.Hex(0xEE4E26), CelPalette.Hex(0xF59238), CelPalette.Hex(0xFFD266),   // p13: the saturated red-orange of the REF 4x crops   // d09: toward the ref #c16a42 / #f3cf86
                CelPalette.Hex(0xFFF6D0), 0.84f, 4, CelPalette.Hex(0x1E0806));
            CelPalette.Add("fb_ribbon", CelPalette.Hex(0x5E585E), CelPalette.Hex(0x847F86), CelPalette.Hex(0xA9A5AB), CelPalette.Hex(0xC8C4C6),   // c05: soft grey (was near-white)
               
                CelPalette.Hex(0xFFFFFF), 1.1f, 3, CelPalette.Hex(0x3A3438));
            // hit sparks: yellow-orange, a pale yellow core (never white)
            CelPalette.Add("fb_spark", CelPalette.Hex(0x9A3A10), CelPalette.Hex(0xE0681C), CelPalette.Hex(0xFFA02E), CelPalette.Hex(0xFFD04A),
                CelPalette.Hex(0xFFE27A), 0.9f, 3, CelPalette.Hex(0x2A0C06));
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
                Comets[i] = MakeComet(Launch[i], Flights[i], pre); CometsMid[i] = MakeComet(Launch[i], Flights[i], pre, 1.7f); CometsFar[i] = MakeComet(Launch[i], Flights[i], pre, 2.6f);   // d49: bigger bolts at Mid/Far (O120/O240 lost them)
                Hits[i] = MakeHit(Launch[i] + Flights[i], OnGround[i], OnGround[i] && Launch[i] + Flights[i] > 1.15f && (gfires++ & 1) == 0);
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
                    Palette = "fb_pool", Core = 1f, Opacity = 0.7f,   // p01: softer (REF's ground light is a faint wash)
                    // pool mode, falloff power 1.2, peak at 2.95 s into its life (f90), fade from 3.15 s (f95) to f137
                    Extra = new Vector4(-1f, 1.2f, 3.25f / PoolLife, 3.45f / PoolLife),
                });
        }

        static CelFxRecipe MakeComet(float t0, float flight, bool preflown, float k = 1f)   // d49: k scales the bolt for the Mid/Far tiers
        {
            return new CelFxRecipe("fire_bolt_comet", t0 + flight,
                new CelLayer
                {
                    Name = "comet", Kind = CometKind.KindName, Start = t0, Life = flight, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 1, Aim = CelAim.Target, Speed = 0f, Spread = 0f, Spin = 3f, Gravity = ArcGravity,
                    // b01 (beach, 19 m camera): ribbon 0.2 m wide, 0.75 s of its speed long (a third to half the frame)
                    // b02: a negative Stretch draws the tail behind the birth (the opening bolts were in flight before the fire)
                    Size = new CelCurve(0.26f * k, 0.46f * k), SizeJitter = 0.2f, Stretch = preflown ? -0.7f : 0.7f,   // p13: a wide fuzzy band (REF 4x)   // p09: as wide as the body (REF)   // p06: a soft wide pale smear   // p01: the smoke trail is a THIN pale line in REF, half the old width   // d18: wider (mask diff) but unbroken and semi-transparent   // c05: a longer, thin grey trail (the ref's are lines)   // c03: the smoke ribbon ~1.5x the flame (0.75 ran to the frame edge)
                      // c01: 0.2 -> 0.26-0.3 m (the smoke trail frays wider)
                    Erode = new CelCurve(0.06f, 0.2f),   // d18: stays whole (d17 dashes lost)   // c03: the ribbon breaks into pieces (was whole)
                    Palette = "fb_comet", CoolTo = "fb_ribbon", Core = 1.9f,   // p02: the head blazes   // b03: 2.6 -> 2.2 (the bodies read white-hot)
                    Opacity = 0.32f, Ink = 0f,   // p01: translucent trail, NO ink on the bolt (REF has none)   // d18: ribbon 75% (d16 opaque bands lost)
                    // b02: a round white-yellow head 0.2 m, a flame body that flares and tapers over 2.2 m, thin red
                    // streaks fading out to 5 m, a ribbon waving 0.08 m over 5.5 m
                    Extra = new Vector4(0.28f * k, 7.0f * k, 0.02f, 3.0f),   // p13: tails run half the frame   // p04: smaller head, longer tail   // p03: a nearly straight trail   // p01: head 0.3 m (teardrop in the shader), trail wave 0.06 m over 2 m   // d34: head 0.28 -> 0.4 m (every paper: a dot head on a tube)   // d09: head 0.24 -> 0.28 m   // c05: head 0.3 -> 0.24 m (the teardrop flame is about half the old width), licks 3.6 m, trail wave 0.05 m over 3 m   // c03: ribbon wave 0.11 m over 2.2 m (wavy, not a straight line)   // c01: head 0.3 m, ribbon wave 0.04 m (it follows the flame's path)   // b04: head 0.26 m, the flame a long tapering streak (shader)   // b03: head 0.2 -> 0.27 m (the body is twice as wide in the shader)
                });
        }

        static CelFxRecipe MakeHit(float hit, bool ground, bool bigFire = true)
        {
            // b02: no glowing disc on the dummy (the critic read it as a flat white plate): a dummy hit is a starburst of
            // 24 thin bright spark streaks (6-10 frames) and a few flame tongues; a ground hit keeps a small short flash
            var r = new CelFxRecipe(ground ? "fire_bolt_ground" : "fire_bolt_dummy", hit + 2f,
                new CelLayer
                {
                    Name = "spark starburst", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit, Duration = 0.05f, Life = 0.34f, LifeJitter = 0.25f,
                    Close = ground ? 48 : 46, Mid = 6,   // p04: REF's ground hits are spark starbursts   // d06 (from d04): denser sprays   // c03: a fuller radial spray / dummy spark fan
                    Far = 0, Shape = CelShape.Shell, ShapeSize = new Vector3(ground ? 0.1f : 0.16f, 0f, 0f), Aim = CelAim.Radial,
                    Speed = ground ? 11f : 8f, SpeedJitter = 0.5f, UpBias = ground ? 0.55f : 0.15f, Gravity = 3f, Drag = 1.4f,   // c01: born on a shell (no pile-up disc at the middle), ground sparks fan upward
                    // b04: short orange dots and streaks over the torso's width (b03 lost its palette, erosion and core
                    // to a comment, so 28 default-white streaks piled up into a disc at the chest)
                    Size = new CelCurve(0.05f, 0.02f), Stretch = ground ? 0.22f : 0.09f,   // p06: REF's dummy sparks are short bright dashes   // c01: thinner, longer streaks (b05: 0.05, 0.09)
                    Erode = new CelCurve(0.02f, 0.6f), Palette = "fb_spark", Core = 1.4f, Ink = 0f, Extra = new Vector4(0f, 0.08f, 0.3f, 0f),   // p10: no ink (REF's sparks are soft bright dashes)
                },
                new CelLayer
                {
                    Name = "flame tongues", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit, Duration = 0.15f, Life = 0.22f, LifeJitter = 0.3f,
                    Close = ground ? 6 : 1, Mid = ground ? 3 : 1, Far = 0,   // c01: one tongue on the dummy (three lit a disc on the chest)
                    Shape = CelShape.Sphere, ShapeSize = new Vector3(0.12f, 0f, 0f), Aim = CelAim.Up, Spread = 60f, Speed = 1.1f, SpeedJitter = 0.3f, Buoyancy = 0.6f,
                    Size = new CelCurve(0.07f, 0.02f, 0.8f),   // p05: no floating orange leaves SizeJitter = 0.2f, Stretch = 0.16f, Erode = new CelCurve(0.05f, 0.5f), Palette = ground ? "fire" : "fb_spark", Core = ground ? 1.8f : 1f, Ink = 0f, Extra = new Vector4(0f, 0.12f, 0.35f, 0f),
                },
                new CelLayer
                {
                    Name = "light", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = hit, Life = 0.4f,
                    Close = 1, Mid = 1, Far = 0, Palette = "fire", Core = ground ? 0.5f : 0f, Extra = new Vector4(3f, 0f, 0f, 0f),   // b03: no light on the dummy (even 0.06 lit a disc on the board)
                });
            r.Layers.Add(new CelLayer
            {
                Name = "hit flash", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Start = hit, Life = 0.22f, LifeJitter = 0.04f,   // p12: a smooth soft flash (Comet mode -4), no bands
                Close = ground ? 0 : 1, Mid = ground ? 0 : 1, Far = 0, Size = new CelCurve(0.4f, 0.8f, 0.5f), SizeJitter = 0.2f,
                Palette = "fb_spark", Core = 2.2f, Opacity = 1.3f, Extra = new Vector4(-4f, 0f, 0f, 0f),   // p11: round and soft (REF: a bright soft burst, the sparks are the lines)
            });
            // d27 (stall fracture: dummy hit -> its lingering burn; new primitive, Comet mode extra.x = -3): an orange
            // cel-blob flame behind the hit star (d28: the star rays stay on top), then a small ember that burns to ~1.4 s
            if (!ground) for (int b = 0; b < 2; b++) r.Layers.Add(new CelLayer
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
                    Name = "hit star", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.15f, 0f), Start = hit, Life = 0.18f, LifeJitter = 0.04f,
                    Close = 1, Mid = 1, Far = 1, Size = new CelCurve(0.35f, 0.6f, 0.5f), SizeJitter = 0.15f,
                    Palette = "fire", Core = 2.2f, Opacity = 1.3f, Extra = new Vector4(-4f, 0f, 0f, 0f),   // p12: soft flash   // p11: round and soft
                });
                // b03: a ragged burst of wide flame tongues about the dummy's torso tall, fanned out (b02's were thin
                // upright slivers), then a smaller fire that flickers on and dies about 1 s after the hit
                r.Layers.Add(new CelLayer
                {
                    Name = "ground burst", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit, Duration = 0.12f, Life = 0.32f, LifeJitter = 0.3f,
                    Close = 24, Mid = 14, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.25f, 0f, 0f), Aim = CelAim.Up, Spread = 75f, Speed = 2.6f, SpeedJitter = 0.45f,   // p11: a spray of thin flame spikes Buoyancy = 0.8f,
                    Size = new CelCurve(0.22f, 0.06f, 0.6f),   // p12   // p04: small ragged tongues (REF: a spark pop with a small flame, not a yellow fan)   // c05: bigger round bursts (the ref's pop at the landing point)
                    SizeJitter = 0.4f, Stretch = 0.38f,   // p12: long spikes   // c03: round flicker flames on the ground (c01 shot carrots up into the air)   // b04: narrower fan, taller (about the torso)
                    Erode = new CelCurve(0.05f, 0.55f), Palette = "fire", Core = 1.6f, Ink = 0f, Extra = new Vector4(0f, 0.15f, 0.4f, 0f),
                });
                if (bigFire) r.Layers.Add(new CelLayer   // d23: the lasting fire body on every other ground hit after ~f40 only (ref: 1-2 fires at once)
                {
                    Name = "ground fire", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit + 0.12f, Duration = 0.75f, Life = 0.6f, LifeJitter = 0.3f,
                    Close = 9, Mid = 8, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.3f, 0f, 0f), Aim = CelAim.Up, Spread = 50f, SpeedJitter = 0.35f, Buoyancy = 0.6f,
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
                    Name = "fire core", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.2f, 0f), Start = hit, Life = 0.8f, LifeJitter = 0.15f,
                    Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.55f, 0.3f, 0.6f), SizeJitter = 0.15f,
                    Palette = "fire", Core = 2.2f, Opacity = 1.0f,
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
            for (int k = 0; k < 2; k++)
            {
                r.Layers.Add(new CelLayer
                {
                    Name = "smoke wisp", Kind = CometKind.KindName, Anchor = CelAnchor.Origin, Offset = new Vector3(0.12f * k - 0.05f, 0.05f, 0.1f * k), Start = hit + 0.06f + 0.18f * k, Life = 2.45f - 0.3f * k, LifeJitter = 0.1f,
                    Close = 1, Mid = 1, Far = 0, Aim = CelAim.None, Speed = 0f, Gravity = 0f, Drag = 0f, Buoyancy = 0f, Spin = 1.1f + 0.3f * k,
                    Size = new CelCurve(0.12f, 2.8f - 0.3f * k, 2.0f), SizeJitter = 0.15f, Stretch = 4.0f - 0.6f * k,   // p09: a thin strand for the first half of its life   // p06: a thin strand first (REF f60), a cloud late   // p02: bigger, taller puffs
                    Erode = new CelCurve(0.02f, 0.7f, 2.6f),
                    Palette = "fb_wisp", Core = 1f, Opacity = 0.5f, Ink = 0f,   // p01: softer, less inked smoke
                    Extra = new Vector4(-2f, 0.13f, 0.7f, 1.5f + 0.4f * k),   // d32: one broad leaning curl, not a tight corkscrew   // d07: gentler S, more lean (the wider puffs fold on tight curls)
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
