// Phase: CelFx job (2026-11-09, lane/show/cel-fire-bolts) - fire bolts (lab only): a volley of comet fire bolts from
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
        public const float ArcGravity = 2.4f;
        public static float ArcLift(float flight) { return 0.5f * ArcGravity * flight * flight; }

        /// <summary>Launch times (s after the fire) and whether the bolt lands on the ground (else on the dummy): an
        /// opening flurry, a thinner middle, and a dense late burst landing about 2.9 s after the fire (the
        /// reference's peak, frame 90).</summary>
        public static readonly float[] Launch = { 0f, 0.07f, 0.18f, 0.3f, 0.42f, 0.55f, 0.7f, 0.9f, 1.08f, 1.25f, 1.42f, 1.6f,
            1.85f, 1.98f, 2.08f, 2.18f, 2.28f, 2.38f, 2.46f, 2.54f, 2.62f, 2.72f };
        public static readonly bool[] OnGround = { false, true, false, false, true, false, true, false, true, false, true, false,
            true, false, true, false, true, false, true, false, false, false };
        /// <summary>Each bolt's flight time (s): staggered so the volley never flies as a rigid row.</summary>
        public static readonly float[] FlightOf = { 0.8f, 0.7f, 0.92f, 0.76f, 0.86f, 0.68f, 0.9f, 0.74f, 0.82f, 0.95f, 0.7f, 0.84f,
            0.78f, 0.9f, 0.72f, 0.86f, 0.8f, 0.68f, 0.92f, 0.76f, 0.84f, 0.8f };

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
            CelPalette.Add("fb_smoke", CelPalette.Hex(0x6A6064), CelPalette.Hex(0x84797C), CelPalette.Hex(0xA49A9A), CelPalette.Hex(0xC0B8B6),
                CelPalette.Hex(0xCCC4C2), 1.1f, 3, CelPalette.Hex(0x1A1416));
            Comets = new CelFxRecipe[Launch.Length];
            Hits = new CelFxRecipe[Launch.Length];
            for (int i = 0; i < Launch.Length; i++) { Comets[i] = MakeComet(Launch[i], FlightOf[i]); Hits[i] = MakeHit(Launch[i] + FlightOf[i], OnGround[i]); }
        }

        // templates (the tests find and validate these)
        public static readonly CelFxRecipe Comet = MakeComet(0f, Flight);
        public static readonly CelFxRecipe DummyHit = MakeHit(Flight, false);
        public static readonly CelFxRecipe GroundHit = MakeHit(Flight, true);
        /// <summary>The soft pool under the whole volley: brightest ~2.95 s after the fire, gone by ~4.5 s.</summary>
        public static readonly CelFxRecipe Pool = MakePool(0f);
        /// <summary>The volley: bolt i's comet and its hit, shifted by Launch[i].</summary>
        public static readonly CelFxRecipe[] Comets, Hits;

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

        static CelFxRecipe MakeComet(float t0, float flight)
        {
            return new CelFxRecipe("fire_bolt_comet", t0 + flight,
                new CelLayer
                {
                    Name = "comet", Kind = CometKind.KindName, Start = t0, Life = flight, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 1, Aim = CelAim.Target, Speed = 0f, Spread = 0f, Spin = 3f, Gravity = ArcGravity,
                    // b01 (beach, 19 m camera): ribbon 0.2 m wide, 0.75 s of its speed long (a third to half the frame)
                    Size = new CelCurve(0.16f, 0.16f), SizeJitter = 0.2f, Stretch = 0.75f, Erode = new CelCurve(0f, 0f),
                    Palette = "fb_comet", CoolTo = "fb_ribbon", Core = 2.6f, Opacity = 1f, Ink = 2f,
                    // head 0.19 m (small: the ref's heads are ~1/70 frame), a long thin flame tail 6.5 m breaking into streaks, a ribbon waving 0.08 m over 5.5 m
                    Extra = new Vector4(0.19f, 6.5f, 0.08f, 5.5f),
                });
        }

        static CelFxRecipe MakeHit(float hit, bool ground)
        {
            var r = new CelFxRecipe(ground ? "fire_bolt_ground" : "fire_bolt_dummy", hit + 2f,
                new CelLayer
                {
                    Name = "hit star", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Start = hit, Life = 0.12f, LifeJitter = 0.05f,
                    Close = 1, Mid = 1, Far = 1, Size = new CelCurve(0.2f, 0.45f, 0.5f), SizeJitter = 0.15f, Erode = new CelCurve(0.03f, 0.9f, 1.5f),
                    Palette = "fire", Core = 1.8f, Spin = 2f, Extra = new Vector4(2f, 1.1f, 8f, 5f),
                },
                new CelLayer
                {
                    Name = "spark starburst", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit, Duration = 0.04f, Life = 0.26f, LifeJitter = 0.2f,
                    Close = 16, Mid = 5, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.05f, 0f, 0f), Aim = CelAim.Radial,
                    Speed = 10f, SpeedJitter = 0.35f, UpBias = 0.25f, Gravity = 4f, Drag = 3.5f,
                    Size = new CelCurve(0.045f, 0.02f), Stretch = 0.12f, Erode = new CelCurve(0.02f, 0.5f), Palette = "fb_spark", Core = 1.3f, Extra = new Vector4(0f, 0.15f, 0f, 0f),
                },
                new CelLayer
                {
                    Name = "flame tongues", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit, Duration = 0.2f, Life = 0.32f, LifeJitter = 0.3f,
                    Close = 7, Mid = 3, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.08f, 0f, 0f), Aim = CelAim.Up, Spread = 55f, Speed = 1.9f, SpeedJitter = 0.3f, Buoyancy = 1f,
                    Size = new CelCurve(0.16f, 0.04f, 0.8f), SizeJitter = 0.2f, Stretch = 0.16f, Erode = new CelCurve(0.05f, 0.5f), Palette = "fire", Core = 1.6f, Ink = 2f, Extra = new Vector4(0f, 0.12f, 0.35f, 0f),
                },
                new CelLayer
                {
                    Name = "light", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = hit, Life = 0.4f,
                    Close = 1, Mid = 1, Far = 0, Palette = "fire", Core = ground ? 0.5f : 0.12f, Extra = new Vector4(3f, 0f, 0f, 0f),
                });
            if (ground)
            {
                // thin curling grey smoke strands after each ground burst (strand mode: extra.y < 0): they rise, drift
                // right along the shot, grow wider and break up into pieces; none over the dummy
                r.Layers.Add(new CelLayer
                {
                    Name = "smoke strands", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.3f, 0.12f), Start = hit + 0.1f, Duration = 0.4f, Life = 1.6f, LifeJitter = 0.15f,
                    Close = 7, Mid = 1, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.25f, 0f, 0f), Aim = CelAim.Radial, UpBias = 1.6f, Speed = 2.2f, SpeedJitter = 0.3f, Drag = 0.2f, Buoyancy = 2.6f, Spin = 2.0f,
                    Size = new CelCurve(0.22f, 0.5f, 0.8f), SizeJitter = 0.25f, Stretch = 2.0f, Erode = new CelCurve(0f, 0.92f, 3.0f),
                    Palette = "fb_smoke", CoolTo = "fb_smoke", Core = 1f, Opacity = 1f, Ink = 0f, Extra = new Vector4(0f, -1f, 0.13f, 1.1f),
                });
                r.Layers.Add(new CelLayer
                {
                    Name = "ground flames", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit + 0.03f, Duration = 0.9f, Life = 0.34f, LifeJitter = 0.25f,
                    Close = 16, Mid = 6, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.18f, 0f, 0f), Aim = CelAim.Up, Spread = 30f, Speed = 2.6f, SpeedJitter = 0.3f, Buoyancy = 1f,
                    Size = new CelCurve(0.42f, 0.09f, 0.7f), SizeJitter = 0.25f, Stretch = 0.18f, Erode = new CelCurve(0.05f, 0.5f), Palette = "fire", Core = 1.8f, Ink = 2f, Extra = new Vector4(0f, 0.12f, 0.4f, 0f),
                });
                r.Layers.Add(new CelLayer
                {
                    Name = "fire core", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.25f, 0f), Start = hit, Life = 0.6f, LifeJitter = 0.15f,
                    Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.65f, 0.28f, 0.6f), SizeJitter = 0.15f, Erode = new CelCurve(0.05f, 0.7f, 1.5f),
                    Palette = "fire", Core = 1.4f, Extra = new Vector4(2f, 1.4f, 0f, 0f),
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
    }
}
