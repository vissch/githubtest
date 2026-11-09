// Phase: CelFx job (2026-11-09, lane/show/cel-fire-bolts; b03 beach round: 28 bolts in four low lanes from the left edge,
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
        public const float ArcGravity = 2.5f;   // b03: 4 -> 2.5 (lower lanes: the reference's paths are nearly level, bending slightly down)
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
        const int Count = 28;
        static float[] MakeLaunch()
        {
            var a = new float[Count];
            for (int i = 0; i < Count; i++) a[i] = i * 0.098f + ((i * 7) % 5 - 2) * 0.012f + (i == 0 ? 0.024f : 0f);
            return a;
        }
        static bool[] MakeGround()
        {
            var a = new bool[Count];
            for (int i = 0; i < Count; i++) a[i] = (i % 5) == 1 || (i % 5) == 3;
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
        public static readonly float[] AlongAt = { 0.6f, 0.52f, 0.45f, 0.36f, 0.25f };
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
            for (int i = 0; i < Launch.Length; i++)
            {
                bool pre = i < AlongAt.Length;
                Flights[i] = pre ? FlightOf[i] * (1f - AlongAt[i]) : FlightOf[i];
                Comets[i] = MakeComet(Launch[i], Flights[i], pre);
                Hits[i] = MakeHit(Launch[i] + Flights[i], OnGround[i]);
                Smokes[i] = MakeSmoke(Launch[i] + Flights[i]);   // played on ground hits only
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
        public static readonly CelFxRecipe[] Smokes;
        public static readonly CelFxRecipe Smoke = MakeSmoke(Flight);

        const float PoolLife = 5.1f, PoolRadius = 6.0f;

        static CelFxRecipe MakePool(float start)
        {
            return new CelFxRecipe("fire_bolts_pool", start + PoolLife,
                new CelLayer
                {
                    Name = "warm ground light", Kind = CometKind.KindName, Start = start, Life = PoolLife, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 1, Size = new CelCurve(0.85f * PoolRadius, PoolRadius, 0.5f), SizeJitter = 0f,
                    Palette = "fb_pool", Core = 1f, Opacity = 1.3f,
                    // pool mode, falloff power 1.2, peak at 2.95 s into its life (f90), fade from 3.15 s (f95) to f137
                    Extra = new Vector4(-1f, 1.2f, 2.95f / PoolLife, 3.15f / PoolLife),
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
                    Size = new CelCurve(0.2f, 0.2f), SizeJitter = 0.2f, Stretch = preflown ? -0.75f : 0.75f, Erode = new CelCurve(0f, 0f),
                    Palette = "fb_comet", CoolTo = "fb_ribbon", Core = 2.2f,   // b03: 2.6 -> 2.2 (the bodies read white-hot)
                    Opacity = 1f, Ink = 2f,
                    // b02: a round white-yellow head 0.2 m, a flame body that flares and tapers over 2.2 m, thin red
                    // streaks fading out to 5 m, a ribbon waving 0.08 m over 5.5 m
                    Extra = new Vector4(0.27f, 5.0f, 0.08f, 5.5f),   // b03: head 0.2 -> 0.27 m (the body is twice as wide in the shader)
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
                    Close = ground ? 18 : 28, Mid = 6, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.05f, 0f, 0f), Aim = CelAim.Radial,
                    Speed = 9f, SpeedJitter = 0.4f, UpBias = 0.15f, Gravity = 3f, Drag = 2f,
                    Size = new CelCurve(0.06f, 0.025f), Stretch = 0.13f,   // b03: longer, a little wider (a visible star) Erode = new CelCurve(0.02f, 0.6f), Palette = "fb_spark", Core = 3f, Extra = new Vector4(0f, 0.18f, 0f, 0f),
                },
                new CelLayer
                {
                    Name = "flame tongues", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit, Duration = 0.15f, Life = 0.3f, LifeJitter = 0.3f,
                    Close = 6, Mid = 3, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.08f, 0f, 0f), Aim = CelAim.Up, Spread = 60f, Speed = 1.9f, SpeedJitter = 0.3f, Buoyancy = 1f,
                    Size = new CelCurve(0.14f, 0.03f, 0.8f), SizeJitter = 0.2f, Stretch = 0.16f, Erode = new CelCurve(0.05f, 0.5f), Palette = "fire", Core = 1.8f, Ink = 2f, Extra = new Vector4(0f, 0.12f, 0.35f, 0f),
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
                    Close = 16, Mid = 5, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.3f, 0f, 0f), Aim = CelAim.Up, Spread = 50f, Speed = 3.0f, SpeedJitter = 0.35f, Buoyancy = 1.6f,
                    Size = new CelCurve(0.9f, 0.15f, 0.6f), SizeJitter = 0.3f, Stretch = 0.36f, Erode = new CelCurve(0.05f, 0.55f), Palette = "fire", Core = 2.4f, Ink = 2f, Extra = new Vector4(0f, 0.15f, 0.4f, 0f),
                });
                r.Layers.Add(new CelLayer
                {
                    Name = "ground fire", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit + 0.15f, Duration = 0.75f, Life = 0.3f, LifeJitter = 0.3f,
                    Close = 12, Mid = 3, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.22f, 0f, 0f), Aim = CelAim.Up, Spread = 30f, Speed = 1.6f, SpeedJitter = 0.35f, Buoyancy = 1.2f,
                    Size = new CelCurve(0.4f, 0.08f, 0.7f), SizeJitter = 0.3f, Stretch = 0.3f, Erode = new CelCurve(0.1f, 0.6f), Palette = "fire", Core = 1.8f, Ink = 2f, Extra = new Vector4(0f, 0.15f, 0.4f, 0f),
                });
                r.Layers.Add(new CelLayer
                {
                    Name = "fire core", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.15f, 0f), Start = hit, Life = 0.35f, LifeJitter = 0.15f,
                    Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.65f, 0.25f, 0.6f), SizeJitter = 0.15f, Erode = new CelCurve(0.05f, 0.7f, 1.5f),
                    Palette = "fire", Core = 2.8f,   // b03: a bigger white-hot core Extra = new Vector4(2f, 1.4f, 0f, 0f),
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
        static CelFxRecipe MakeSmoke(float hit)
        {
            return new CelFxRecipe("fire_bolt_smoke", hit + 2f,
                new CelLayer
                {
                    Name = "smoke column", Kind = CelKinds.LobeName, Anchor = CelAnchor.Origin, Offset = new Vector3(0f, 0f, 0.2f), Start = hit + 0.1f, Duration = 0.7f, Life = 1.0f, LifeJitter = 0.15f,
                    Close = 20, Mid = 5, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.06f, 0f, 0f), Aim = CelAim.Dir, Spread = 9f, Speed = 2.3f, SpeedJitter = 0.2f, Drag = 0.35f, Buoyancy = 0.5f, Spin = 1.2f,
                    Size = new CelCurve(0.1f, 0.3f, 0.6f), SizeJitter = 0.25f, Stretch = 0f, Erode = new CelCurve(0f, 1f, 2.2f), Feather = 0.1f,
                    Palette = "fb_smoke", Core = 1f, Ink = 2f, Extra = new Vector4(1.6f, 0.22f, 0.9f, 0f),
                });
        }
    }
}
