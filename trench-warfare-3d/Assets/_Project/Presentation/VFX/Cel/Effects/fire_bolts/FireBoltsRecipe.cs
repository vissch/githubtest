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
        const int Count = 40, Opening = 12;   // b05: 40 bolts, 12 already in flight at the fire
        static float LaunchAt(int i) { return i < Opening ? i * 0.02f : 0.26f + (i - Opening) * 0.104f + ((i * 7) % 5 - 2) * 0.01f; }   // c01: to 3.1 s (bolts arrive to the ref's f100; the stage fires at 0.5 s)
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
        public static readonly float[] AlongAt = { 0.62f, 0.5f, 0.4f, 0.3f, 0.55f, 0.2f, 0.45f, 0.12f, 0.68f, 0.35f, 0.58f, 0.25f };   // b05: 12 (b04: 8)   // b04: 8 (b03: 5)
        /// <summary>Bolt i's actual flight time (s): FlightOf shortened for the opening bolts.</summary>
        public static readonly float[] Flights = new float[Launch.Length];

        static FireBolts()
        {
            // additive light on the dark field: c3 is what the pool adds at its middle (dim brown-warm, #c16a42-ish on the ground)
            CelPalette.Add("fb_pool", CelPalette.Hex(0x2A1C14), CelPalette.Hex(0x4A3424), CelPalette.Hex(0x5A4232), CelPalette.Hex(0x86664C),
                CelPalette.Hex(0x86664C), 1.1f, 4, CelPalette.Hex(0x000000));
            // the comet: deep red at the streaks' tips, red-orange, orange, yellow behind the head, a white-hot core
            CelPalette.Add("fb_comet", CelPalette.Hex(0x8E1A0C), CelPalette.Hex(0xD03A16), CelPalette.Hex(0xF5912A), CelPalette.Hex(0xFFD458),
                CelPalette.Hex(0xFFFFF0), 0.84f, 4, CelPalette.Hex(0x1E0806));
            CelPalette.Add("fb_ribbon", CelPalette.Hex(0x6A666C), CelPalette.Hex(0x9A989E), CelPalette.Hex(0xC4C2C6), CelPalette.Hex(0xE8E6E6),
                CelPalette.Hex(0xFFFFFF), 1.1f, 3, CelPalette.Hex(0x3A3438));
            // hit sparks: yellow-orange, a pale yellow core (never white)
            CelPalette.Add("fb_spark", CelPalette.Hex(0x9A3A10), CelPalette.Hex(0xE0681C), CelPalette.Hex(0xFFA02E), CelPalette.Hex(0xFFD04A),
                CelPalette.Hex(0xFFE27A), 0.9f, 3, CelPalette.Hex(0x2A0C06));
            // grey wispy smoke (#726369 body)
            CelPalette.Add("fb_smoke", CelPalette.Hex(0x7A7478), CelPalette.Hex(0x968F92), CelPalette.Hex(0xB4AEAE), CelPalette.Hex(0xD0CACA),
                CelPalette.Hex(0xDAD4D2), 1.1f, 2, CelPalette.Hex(0x1C171A));   // b03: two bands, a near-black ink (the reference's smoke is inked)
            Comets = new CelFxRecipe[Launch.Length];
            Hits = new CelFxRecipe[Launch.Length];
            Smokes = new CelFxRecipe[Launch.Length];
            SmokesMid = new CelFxRecipe[Launch.Length];
            for (int i = 0; i < Launch.Length; i++)
            {
                bool pre = i < AlongAt.Length;
                Flights[i] = pre ? FlightOf[i] * (1f - AlongAt[i]) : FlightOf[i];
                Comets[i] = MakeComet(Launch[i], Flights[i], pre);
                Hits[i] = MakeHit(Launch[i] + Flights[i], OnGround[i]);
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
        public static readonly CelFxRecipe[] Comets, Hits;
        /// <summary>b03: bolt i's smoke column (the Range plays it on ground hits only), with dir = up and a little
        /// screen-right, so the column leans along the shot.</summary>
        public static readonly CelFxRecipe[] Smokes, SmokesMid;
        public static readonly CelFxRecipe Smoke = MakeSmoke(Flight, 0);
        public static readonly CelFxRecipe SmokeMid = MakeSmoke(Flight, -1);

        const float PoolLife = 5.4f, PoolRadius = 6.0f;   // c01: peak 3.25 s after a 0.5 s fire = the ref's f90

        static CelFxRecipe MakePool(float start)
        {
            return new CelFxRecipe("fire_bolts_pool", start + PoolLife,
                new CelLayer
                {
                    Name = "warm ground light", Kind = CometKind.KindName, Start = start, Life = PoolLife, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 1, Size = new CelCurve(0.85f * PoolRadius, PoolRadius, 0.5f), SizeJitter = 0f,
                    Palette = "fb_pool", Core = 1f, Opacity = 1.3f,
                    // pool mode, falloff power 1.2, peak at 2.95 s into its life (f90), fade from 3.15 s (f95) to f137
                    Extra = new Vector4(-1f, 1.2f, 3.25f / PoolLife, 3.45f / PoolLife),
                });
        }

        static CelFxRecipe MakeComet(float t0, float flight, bool preflown)
        {
            return new CelFxRecipe("fire_bolt_comet", t0 + flight,
                new CelLayer
                {
                    Name = "comet", Kind = CometKind.KindName, Start = t0, Life = flight, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 1, Aim = CelAim.Target, Speed = 0f, Spread = 0f, Spin = 3f, Gravity = ArcGravity,
                    // b01 (beach, 19 m camera): ribbon 0.2 m wide, 0.75 s of its speed long (a third to half the frame)
                    // b02: a negative Stretch draws the tail behind the birth (the opening bolts were in flight before the fire)
                    Size = new CelCurve(0.26f, 0.3f), SizeJitter = 0.2f, Stretch = preflown ? -0.75f : 0.75f,   // c01: 0.2 -> 0.26-0.3 m (the smoke trail frays wider)
                    Erode = new CelCurve(0f, 0f),
                    Palette = "fb_comet", CoolTo = "fb_ribbon", Core = 2.0f,   // b03: 2.6 -> 2.2 (the bodies read white-hot)
                    Opacity = 1f, Ink = 2f,
                    // b02: a round white-yellow head 0.2 m, a flame body that flares and tapers over 2.2 m, thin red
                    // streaks fading out to 5 m, a ribbon waving 0.08 m over 5.5 m
                    Extra = new Vector4(0.3f, 4.2f, 0.04f, 5.5f),   // c01: head 0.3 m, ribbon wave 0.04 m (it follows the flame's path)   // b04: head 0.26 m, the flame a long tapering streak (shader)   // b03: head 0.2 -> 0.27 m (the body is twice as wide in the shader)
                });
        }

        static CelFxRecipe MakeHit(float hit, bool ground)
        {
            // b02: no glowing disc on the dummy (the critic read it as a flat white plate): a dummy hit is a starburst of
            // 24 thin bright spark streaks (6-10 frames) and a few flame tongues; a ground hit keeps a small short flash
            var r = new CelFxRecipe(ground ? "fire_bolt_ground" : "fire_bolt_dummy", hit + 2f,
                new CelLayer
                {
                    Name = "spark starburst", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit, Duration = 0.05f, Life = 0.34f, LifeJitter = 0.25f,
                    Close = ground ? 20 : 24, Mid = 6, Far = 0, Shape = CelShape.Shell, ShapeSize = new Vector3(ground ? 0.1f : 0.16f, 0f, 0f), Aim = CelAim.Radial,
                    Speed = 8.5f, SpeedJitter = 0.45f, UpBias = ground ? 0.55f : 0.15f, Gravity = 3f, Drag = 1.4f,   // c01: born on a shell (no pile-up disc at the middle), ground sparks fan upward
                    // b04: short orange dots and streaks over the torso's width (b03 lost its palette, erosion and core
                    // to a comment, so 28 default-white streaks piled up into a disc at the chest)
                    Size = new CelCurve(0.035f, 0.02f), Stretch = ground ? 0.08f : 0.12f,   // c01: thinner, longer streaks (b05: 0.05, 0.09)
                    Erode = new CelCurve(0.02f, 0.6f), Palette = "fb_spark", Core = 1.15f, Extra = new Vector4(0f, 0.08f, 0.3f, 0f),
                },
                new CelLayer
                {
                    Name = "flame tongues", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit, Duration = 0.15f, Life = 0.3f, LifeJitter = 0.3f,
                    Close = ground ? 6 : 1, Mid = ground ? 3 : 1, Far = 0,   // c01: one tongue on the dummy (three lit a disc on the chest)
                    Shape = CelShape.Sphere, ShapeSize = new Vector3(0.12f, 0f, 0f), Aim = CelAim.Up, Spread = 60f, Speed = 1.9f, SpeedJitter = 0.3f, Buoyancy = 1f,
                    Size = new CelCurve(0.12f, 0.03f, 0.8f), SizeJitter = 0.2f, Stretch = 0.16f, Erode = new CelCurve(0.05f, 0.5f), Palette = ground ? "fire" : "fb_spark", Core = ground ? 1.8f : 1f, Ink = 2f, Extra = new Vector4(0f, 0.12f, 0.35f, 0f),
                },
                new CelLayer
                {
                    Name = "light", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = hit, Life = 0.4f,
                    Close = 1, Mid = 1, Far = 0, Palette = "fire", Core = ground ? 0.5f : 0f, Extra = new Vector4(3f, 0f, 0f, 0f),   // b03: no light on the dummy (even 0.06 lit a disc on the board)
                });
            if (ground)
            {
                r.Layers.Add(new CelLayer
                {
                    Name = "hit star", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Start = hit, Life = 0.1f, LifeJitter = 0.04f,
                    Close = 1, Mid = 1, Far = 1, Size = new CelCurve(0.2f, 0.4f, 0.5f), SizeJitter = 0.15f, Erode = new CelCurve(0.03f, 0.9f, 1.5f),
                    Palette = "fire", Core = 2f, Spin = 2f, Extra = new Vector4(2f, 1.1f, 8f, 5f),
                });
                // b03: a ragged burst of wide flame tongues about the dummy's torso tall, fanned out (b02's were thin
                // upright slivers), then a smaller fire that flickers on and dies about 1 s after the hit
                r.Layers.Add(new CelLayer
                {
                    Name = "ground burst", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit, Duration = 0.12f, Life = 0.32f, LifeJitter = 0.3f,
                    Close = 16, Mid = 5, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.3f, 0f, 0f), Aim = CelAim.Up, Spread = 35f, Speed = 3.6f, SpeedJitter = 0.35f, Buoyancy = 2.2f,
                    Size = new CelCurve(0.7f, 0.12f, 0.6f), SizeJitter = 0.3f, Stretch = 0.32f,   // b04: narrower fan, taller (about the torso)
                    Erode = new CelCurve(0.05f, 0.55f), Palette = "fire", Core = 1.7f, Ink = 2f, Extra = new Vector4(0f, 0.15f, 0.4f, 0f),
                });
                r.Layers.Add(new CelLayer
                {
                    Name = "ground fire", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit + 0.12f, Duration = 0.8f, Life = 0.32f, LifeJitter = 0.3f,
                    Close = 22, Mid = 4, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.3f, 0f, 0f), Aim = CelAim.Up, Spread = 22f, SpeedJitter = 0.35f, Buoyancy = 1.8f,
                    Speed = 3.7f, Size = new CelCurve(0.8f, 0.14f, 0.7f),   // c01: as tall as the torso (b05: 0.65 m, speed 3.0)   // b04: about the torso tall (b03: under half)
                    SizeJitter = 0.3f, Stretch = 0.34f, Erode = new CelCurve(0.1f, 0.6f), Palette = "fire", Core = 1.7f, Ink = 2f, Extra = new Vector4(0f, 0.15f, 0.4f, 0f),
                });
                r.Layers.Add(new CelLayer
                {
                    Name = "fire core", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.15f, 0f), Start = hit, Life = 0.7f, LifeJitter = 0.15f,
                    Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.6f, 0.25f, 0.6f), SizeJitter = 0.15f, Erode = new CelCurve(0.05f, 0.8f, 1.5f),
                    Palette = "fire", Core = 2.6f,   // c01: a bigger white-hot base (0.5 m, 2.1)
                    Extra = new Vector4(2f, 1.4f, 0f, 0f),   // b04: the flash mode b03 lost to a comment: a white-hot base
                });
                r.Layers.Add(new CelLayer
                {
                    Name = "fire glow", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Start = hit, Life = 1.0f, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.9f, 0.7f), SizeJitter = 0.1f,
                    Palette = "fb_pool", Core = 1f, Opacity = 0.3f, Extra = new Vector4(-1f, 1.6f, 0.08f, 0.35f),
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
        const int SmokeLinks = 26;
        const float SmokeHeight = 3.4f, SmokeRise = 3.2f, SmokeEnd = 1.85f;
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
            float phase = variant * 1.7f, sway = 0.42f + 0.1f * (variant % 3);   // c01_3: a wider S (0.22 m was lost in the column's width)
            for (int k = 0; k < SmokeLinks; k++)
            {
                float u = k / (float)(SmokeLinks - 1), h = u * SmokeHeight;
                // lean right about 0.3 m per metre, an S of 0.22-0.34 m that grows with height
                float x = 0.28f * h + sway * Mathf.Sin(h * 1.5f + phase) * (0.3f + 0.7f * u);
                float born = 0.08f + h / SmokeRise;
                float life = Mathf.Max(0.45f, SmokeEnd - born);
                // radius: 0.12 m at the fire, widest (0.25 m) at 55% of the height, thinning to 0.11 m at its top
                float rad = Mathf.Lerp(0.2f, 0.48f, Mathf.Clamp01(u / 0.55f)) * (u > 0.55f ? Mathf.Lerp(1f, 0.45f, (u - 0.55f) / 0.45f) : 1f);
                r.Layers.Add(new CelLayer
                {
                    Name = "smoke link", Kind = CelKinds.LobeName, Anchor = CelAnchor.Origin, Offset = new Vector3(x, 0.05f + h, 0f), Start = hit + born, Life = life, LifeJitter = 0.05f,
                    Close = 1, Mid = 1, Far = 0, Shape = CelShape.Point, Aim = CelAim.Up, Spread = 20f, Speed = 0.25f, SpeedJitter = 0.3f, Drag = 0.4f, Buoyancy = 0.25f, Spin = 1.0f,
                    Size = new CelCurve(rad * 0.55f, rad * 1.15f, 0.5f), SizeJitter = 0.12f, Stretch = 0f,
                    // the top breaks up first: its erosion starts higher and the whole column goes in the last third
                    Erode = new CelCurve(0.18f * u, 1f, 3.2f - 1.2f * u), Feather = 0.12f,
                    Palette = "fb_smoke", Core = 1f, Ink = 4f, Extra = new Vector4(1.1f, 0.22f, 0.9f, 0f),   // c01_3: rounder billows (lump amp 0.16 -> 0.22)
                });
            }
            return r;
        }
    }
}
