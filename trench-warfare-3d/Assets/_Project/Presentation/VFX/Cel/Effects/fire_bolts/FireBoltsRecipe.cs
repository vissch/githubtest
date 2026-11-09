// Phase: CelFx job (2026-11-09, lane/show/cel-fire-bolts) - fire bolts (lab only): a volley of comet fire bolts from
// the upper left, angled down onto the dummy and the ground beside it, each a white-yellow head with a soft yellow
// bloom, a flame tail banded yellow, orange, deep red that tapers smoothly to a point (inked on its lower edge) and a
// thin wavy white-grey smoke ribbon, all one Comet record (CometKind). Each hit: a small star flash, a starburst of
// thin yellow spark streaks and orange-yellow flame tongues; ground hits burst into flames about 1 m tall that flicker
// out. Then grey cel smoke puffs (Comet records with no head) curl and rise about 3 m and are gone. A soft warm pool of
// light (Comet pool mode: a smooth gradient, no bands) lies on the ground left of and in front of the dummy, rising
// with the volley to its brightest at the late burst and fading smoothly. Reference: fire_hitting_cel (spec/fire_bolts.json).
//
// Every bolt is one CelFx.Play of a recipe whose layers are shifted by the bolt's launch time (FireBolts.Volley), so
// the whole volley is spawned at the fire and nothing runs per frame. The comet aims at the target with Speed 0, so it
// arrives after Flight seconds; the hit layers start then. Kinds: Comet, Streak, Glow (no Lobe: three draws).
// LOD: Close = everything; Mid = one wisp, fewer sparks, tongues and flames; Far = the comet, a flash and the pool
// (and every other bolt, see FireBoltsRange).
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class FireBolts
    {
        /// <summary>Seconds from launch to hit (every bolt: speed follows the distance).</summary>
        public const float Flight = 0.8f;

        /// <summary>Launch times (s after the fire) and whether the bolt lands on the ground (else on the dummy): an
        /// opening flurry, a thinner middle, and a dense late burst whose bolts are in flight and landing about 2.9 s
        /// after the fire (the reference's peak, frame 90).</summary>
        public static readonly float[] Launch = { 0f, 0.07f, 0.18f, 0.3f, 0.42f, 0.55f, 0.7f, 0.9f, 1.08f, 1.25f, 1.42f, 1.6f,
            1.85f, 1.98f, 2.08f, 2.18f, 2.28f, 2.38f, 2.46f, 2.54f, 2.62f, 2.72f };
        public static readonly bool[] OnGround = { false, true, false, false, true, false, true, false, true, false, true, false,
            true, false, true, false, true, false, true, false, false, false };

        static FireBolts()
        {
            // additive light on the dark field: c3 is what the pool adds at its middle (dull warm orange on the ground)
            CelPalette.Add("fb_pool", CelPalette.Hex(0x2A1C14), CelPalette.Hex(0x4A3424), CelPalette.Hex(0x62442E), CelPalette.Hex(0xB8804A),
                CelPalette.Hex(0xB8804A), 1.1f, 4, CelPalette.Hex(0x000000));
            // the comet: deep red at the tail's tip, red-orange, orange, yellow behind the head, a white-yellow core in the head
            CelPalette.Add("fb_comet", CelPalette.Hex(0x8E1A0C), CelPalette.Hex(0xD8441A), CelPalette.Hex(0xF5912A), CelPalette.Hex(0xFFD458),
                CelPalette.Hex(0xFFFFF0), 0.84f, 4, CelPalette.Hex(0x1E0806));
            CelPalette.Add("fb_ribbon", CelPalette.Hex(0x4A4A50), CelPalette.Hex(0x8A8A90), CelPalette.Hex(0xB8B8BC), CelPalette.Hex(0xE0E0E0),
                CelPalette.Hex(0xFFFFFF), 1.1f, 3, CelPalette.Hex(0x3A3438));
            // hit sparks: yellow-orange, a pale yellow core (never white)
            CelPalette.Add("fb_spark", CelPalette.Hex(0x9A3A10), CelPalette.Hex(0xE0681C), CelPalette.Hex(0xFFA02E), CelPalette.Hex(0xFFD04A),
                CelPalette.Hex(0xFFE27A), 0.9f, 3, CelPalette.Hex(0x2A0C06));
            // grey cel smoke, lit from above (#726369 body), dark ink
            CelPalette.Add("fb_smoke", CelPalette.Hex(0x5A4E54), CelPalette.Hex(0x7E7276), CelPalette.Hex(0xA49A9C), CelPalette.Hex(0xC8C0BE),
                CelPalette.Hex(0xC8C0BE), 1.1f, 3, CelPalette.Hex(0x1A1416));
            Volley = new CelFxRecipe[Launch.Length];
            for (int i = 0; i < Launch.Length; i++) Volley[i] = MakeBolt(Launch[i], OnGround[i]);
        }

        // templates (the tests find and validate these)
        public static readonly CelFxRecipe DummyBolt = MakeBolt(0f, false);
        public static readonly CelFxRecipe GroundBolt = MakeBolt(0f, true);
        /// <summary>The soft pool under the whole volley: brightest ~2.95 s after the fire, gone by ~4.45 s.</summary>
        public static readonly CelFxRecipe Pool = MakePool(0f);
        /// <summary>The volley: bolt i, shifted by Launch[i].</summary>
        public static readonly CelFxRecipe[] Volley;

        /// <summary>The volley and the pool with every time `dt` seconds later (made once per shot when the Range has
        /// to start the volley late; see FireBoltsRange). Allocates: lab only, never per frame.</summary>
        public static void Shifted(float dt, out CelFxRecipe[] volley, out CelFxRecipe pool)
        {
            volley = new CelFxRecipe[Launch.Length];
            for (int i = 0; i < Launch.Length; i++) volley[i] = MakeBolt(Launch[i] + dt, OnGround[i]);
            pool = MakePool(dt);
        }

        const float PoolLife = 5.0f, PoolRadius = 5.2f;

        static CelFxRecipe MakePool(float start)
        {
            return new CelFxRecipe("fire_bolts_pool", start + PoolLife,
                new CelLayer
                {
                    Name = "warm ground light", Kind = CometKind.KindName, Start = start, Life = PoolLife, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 1, Size = new CelCurve(0.8f * PoolRadius, PoolRadius, 0.5f), SizeJitter = 0f,
                    Palette = "fb_pool", Core = 1f, Opacity = 1.15f,
                    // pool mode, falloff power 1.3, peak at 2.95 s into its life (f90), fade from 3.16 s (f95) to f132
                    Extra = new Vector4(-1f, 1.3f, 3.0f / PoolLife, 3.3f / PoolLife),
                });
        }

        static CelFxRecipe MakeBolt(float t0, bool ground)
        {
            float hit = t0 + Flight;
            var r = new CelFxRecipe(ground ? "fire_bolt_ground" : "fire_bolt_dummy", hit + 2f,
                // ---------------------------------------------------------------- in flight
                new CelLayer
                {
                    Name = "comet", Kind = CometKind.KindName, Start = t0, Life = Flight, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 1, Aim = CelAim.Target, Speed = 0f, Spread = 0f, Spin = 3f,
                    Size = new CelCurve(0.1f, 0.1f), SizeJitter = 0f, Stretch = 0.4f, Erode = new CelCurve(0f, 0f),
                    Palette = "fb_comet", CoolTo = "fb_ribbon", Core = 2.6f, Opacity = 0.7f, Ink = 3f,
                    // head 0.21 m, a streaky flame tail 3 m (about 7 head widths), a thin strand waving 0.04 m
                    Extra = new Vector4(0.24f, 3.4f, 0.05f, 3f),
                },
                // ---------------------------------------------------------------- the hit
                new CelLayer
                {
                    Name = "hit star", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Start = hit, Life = 0.12f, LifeJitter = 0.05f,
                    Close = 1, Mid = 1, Far = 1, Size = new CelCurve(0.12f, 0.28f, 0.5f), SizeJitter = 0.15f, Erode = new CelCurve(0.03f, 0.9f, 1.5f),
                    Palette = "fire", Core = 1.8f, Spin = 2f, Extra = new Vector4(2f, 1.1f, 8f, 5f),
                },
                new CelLayer
                {
                    Name = "spark starburst", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit, Duration = 0.04f, Life = 0.26f, LifeJitter = 0.2f,
                    Close = 12, Mid = 5, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.05f, 0f, 0f), Aim = CelAim.Radial,
                    Speed = 7.5f, SpeedJitter = 0.35f, UpBias = 0.25f, Gravity = 4f, Drag = 3.5f,
                    Size = new CelCurve(0.03f, 0.015f), Stretch = 0.11f, Erode = new CelCurve(0.02f, 0.5f), Palette = "fb_spark", Core = 1.3f, Extra = new Vector4(0f, 0.15f, 0f, 0f),
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
                    Close = 1, Mid = 1, Far = 0, Palette = "fire", Core = ground ? 0.6f : 0.12f, Extra = new Vector4(3f, 0f, 0f, 0f),
                });
            if (ground)
            {
                // grey cel smoke after each ground burst: round inked puffs that rise in curls, drift right along the
                // shot, grow and break up (none over the dummy: its hits leave no column)
                r.Layers.Add(new CelLayer
                {
                    Name = "smoke puffs", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.25f, 0f), Start = hit + 0.15f, Duration = 0.3f, Life = 1.25f, LifeJitter = 0.12f,
                    Close = 2, Mid = 1, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.25f, 0f, 0f), Aim = CelAim.Up, Spread = 22f, Speed = 3.0f, SpeedJitter = 0.25f, Drag = 0.55f, Buoyancy = 0.7f, Spin = 2.5f,
                    Size = new CelCurve(0.25f, 0.7f, 0.7f), SizeJitter = 0.2f, Stretch = 0.7f, Erode = new CelCurve(0f, 0.97f, 1.4f),
                    Palette = "fb_smoke", CoolTo = "fb_smoke", Core = 1f, Opacity = 0.95f, Ink = 2f, Extra = new Vector4(0f, 0f, 0.13f, 1.8f),
                });
                r.Layers.Add(new CelLayer
                {
                    Name = "ground flames", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit + 0.03f, Duration = 1.0f, Life = 0.34f, LifeJitter = 0.25f,
                    Close = 16, Mid = 6, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.16f, 0f, 0f), Aim = CelAim.Up, Spread = 30f, Speed = 2.1f, SpeedJitter = 0.3f, Buoyancy = 1f,
                    Size = new CelCurve(0.3f, 0.07f, 0.7f), SizeJitter = 0.25f, Stretch = 0.13f, Erode = new CelCurve(0.05f, 0.5f), Palette = "fire", Core = 1.8f, Ink = 2f, Extra = new Vector4(0f, 0.12f, 0.4f, 0f),
                });
                r.Layers.Add(new CelLayer
                {
                    Name = "fire core", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.22f, 0f), Start = hit, Life = 0.7f, LifeJitter = 0.15f,
                    Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.42f, 0.2f, 0.6f), SizeJitter = 0.15f, Erode = new CelCurve(0.05f, 0.7f, 1.5f),
                    Palette = "fire", Core = 1.3f, Extra = new Vector4(2f, 1.4f, 0f, 0f),
                });
                r.Layers.Add(new CelLayer
                {
                    Name = "fire glow", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Start = hit, Life = 1.2f, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 0, Size = new CelCurve(1.3f, 1.1f), SizeJitter = 0.1f,
                    Palette = "fb_pool", Core = 1f, Opacity = 0.5f, Extra = new Vector4(-1f, 1.6f, 0.08f, 0.35f),
                });
            }
            return r;
        }
    }
}
