// Phase: CelFx job (2026-11-09, lane/show/cel-fire-bolts) - fire bolts (lab only): a volley of comet fire bolts from
// the upper left, each a small orange-white head with a banded flame tail (yellow, orange, deep red, tapering to a
// point, inked on its lower side) and a thin wavy white-grey smoke ribbon, all one Comet record (CometKind). They land
// on the dummy and on the ground around and behind it: a small star flash, a starburst of thin yellow sparks, small
// flame tongues, then thin grey-white smoke wisps (Comet records with no head) that curl and rise; ground hits leave a
// small fire that flickers and goes out. A wide warm pool of light lies on the ground in front of and left of the
// dummy, rising with the volley to its brightest with the late burst. Reference: fire_hitting_cel (spec/fire_bolts.json).
//
// Every bolt is one CelFx.Play of a recipe whose layers are shifted by the bolt's launch time (FireBolts.Volley), so
// the whole volley is spawned at the fire and nothing runs per frame. The comet aims at the target with Speed 0, so it
// arrives after Flight seconds; the hit layers start then. Kinds: Comet, Streak, Glow (no Lobe: three draws).
// LOD: Close = everything; Mid = one wisp, fewer sparks, tongues and flames; Far = the comet, a flash and the pools
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
            // additive light on the dark field: the deltas that turn the plate's ground into the reference's lit browns
            CelPalette.Add("fb_pool", CelPalette.Hex(0x442C0E), CelPalette.Hex(0x684418), CelPalette.Hex(0x8C5E22), CelPalette.Hex(0xB07A2C),
                CelPalette.Hex(0xB07A2C), 1.1f, 4, CelPalette.Hex(0x000000));
            // the comet: deep red at the rim and the tail's tip, red-orange, orange, yellow, an orange-white core in the head
            CelPalette.Add("fb_comet", CelPalette.Hex(0x9A1E0E), CelPalette.Hex(0xE0501A), CelPalette.Hex(0xF8962A), CelPalette.Hex(0xFFD45A),
                CelPalette.Hex(0xFFF0C8), 0.84f, 4, CelPalette.Hex(0x2A0C08));
            CelPalette.Add("fb_ribbon", CelPalette.Hex(0x4A4A50), CelPalette.Hex(0x8A8A90), CelPalette.Hex(0xB8B8BC), CelPalette.Hex(0xE0E0E0),
                CelPalette.Hex(0xFFFFFF), 1.1f, 3, CelPalette.Hex(0x4A4448));
            CelPalette.Add("fb_smoke", CelPalette.Hex(0x6E6A6C), CelPalette.Hex(0x9A9496), CelPalette.Hex(0xBDB7B4), CelPalette.Hex(0xDCD6D0),
                CelPalette.Hex(0xDCD6D0), 1.1f, 3, CelPalette.Hex(0x3A3436));
            Volley = new CelFxRecipe[Launch.Length];
            for (int i = 0; i < Launch.Length; i++) Volley[i] = MakeBolt(Launch[i], OnGround[i]);
        }

        // templates (the tests find and validate these)
        public static readonly CelFxRecipe DummyBolt = MakeBolt(0f, false);
        public static readonly CelFxRecipe GroundBolt = MakeBolt(0f, true);
        /// <summary>The wide pool under the whole volley, and the brighter one the late burst adds.</summary>
        public static readonly CelFxRecipe Pool = MakePool(0.05f, 3.45f, 4.8f, 1f);
        public static readonly CelFxRecipe PoolLate = MakePool(1.9f, 1.6f, 3.2f, 0.7f);
        /// <summary>The volley: bolt i, shifted by Launch[i].</summary>
        public static readonly CelFxRecipe[] Volley;

        /// <summary>The volley and the two pools with every time `dt` seconds later (made once per shot when the Range
        /// has to start the volley late; see FireBoltsRange). Allocates: lab only, never per frame.</summary>
        public static void Shifted(float dt, out CelFxRecipe[] volley, out CelFxRecipe pool, out CelFxRecipe poolLate)
        {
            volley = new CelFxRecipe[Launch.Length];
            for (int i = 0; i < Launch.Length; i++) volley[i] = MakeBolt(Launch[i] + dt, OnGround[i]);
            pool = MakePool(0.05f + dt, 3.45f, 4.8f, 1f);
            poolLate = MakePool(1.9f + dt, 1.6f, 3.2f, 0.7f);
        }

        static CelFxRecipe MakePool(float start, float life, float radius, float opacity)
        {
            return new CelFxRecipe("fire_bolts_pool", start + life,
                new CelLayer
                {
                    Name = "warm ground light", Kind = CelKinds.GlowName, Start = start, Life = life, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 1, Size = new CelCurve(0.6f * radius, radius, 0.4f), SizeJitter = 0f, Erode = new CelCurve(0.03f, 0.45f, 10f),
                    Palette = "fb_pool", Core = 1f, Opacity = opacity, Extra = new Vector4(0f, 0.8f, 0f, 0f),
                },
                new CelLayer
                {
                    Name = "hot centre", Kind = CelKinds.GlowName, Start = start + 0.25f, Life = life - 0.5f, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.35f * radius, 0.6f * radius, 0.5f), SizeJitter = 0f, Erode = new CelCurve(0.05f, 0.5f, 8f),
                    Palette = "fb_pool", Core = 1f, Opacity = 0.5f * opacity, Extra = new Vector4(0f, 1.2f, 0f, 0f),
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
                    Size = new CelCurve(0.16f, 0.16f), SizeJitter = 0f, Stretch = 0.5f, Erode = new CelCurve(0f, 0f),
                    Palette = "fb_comet", CoolTo = "fb_ribbon", Core = 1.4f, Opacity = 0.75f, Ink = 3f,
                    Extra = new Vector4(0.2f, 2f, 0.07f, 3f),
                },
                new CelLayer
                {
                    Name = "head halo", Kind = CelKinds.GlowName, Start = t0, Life = Flight, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 0, Aim = CelAim.Target, Speed = 0f, Spread = 0f,
                    Size = new CelCurve(0.36f, 0.36f), SizeJitter = 0.1f, Erode = new CelCurve(0.05f, 0.05f),
                    Palette = "fire", Core = 1f, Opacity = 0.16f, Extra = new Vector4(2f, 1.6f, 0f, 0f),
                },
                // ---------------------------------------------------------------- the hit
                new CelLayer
                {
                    Name = "hit star", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Start = hit, Life = 0.2f, LifeJitter = 0.1f,
                    Close = 1, Mid = 1, Far = 1, Size = new CelCurve(0.18f, 0.42f, 0.5f), SizeJitter = 0.15f, Erode = new CelCurve(0.03f, 0.9f, 1.5f),
                    Palette = "fire", Core = 2.2f, Spin = 2f, Extra = new Vector4(2f, 1.1f, 8f, 5f),
                },
                new CelLayer
                {
                    Name = "spark lines", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit, Duration = 0.05f, Life = 0.3f,
                    Close = ground ? 9 : 12, Mid = 4, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.06f, 0f, 0f), Aim = CelAim.Radial,
                    Speed = 6.5f, SpeedJitter = 0.4f, UpBias = 0.3f, Gravity = 5f, Drag = 3f,
                    Size = new CelCurve(0.022f, 0.012f), Stretch = 0.07f, Erode = new CelCurve(0.05f, 0.5f), Palette = "spark", Core = 2.4f, Extra = new Vector4(0f, 0.1f, 0f, 0f),
                },
                new CelLayer
                {
                    Name = "flame tongues", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit, Duration = 0.22f, Life = 0.3f, LifeJitter = 0.3f,
                    Close = 6, Mid = 2, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.07f, 0f, 0f), Aim = CelAim.Up, Spread = 40f, Speed = 1.5f, SpeedJitter = 0.3f, Buoyancy = 1f,
                    Size = new CelCurve(0.12f, 0.03f, 0.8f), SizeJitter = 0.2f, Stretch = 0.14f, Erode = new CelCurve(0.05f, 0.5f), Palette = "fire", Core = 2f, Extra = new Vector4(0f, 0.12f, 0.3f, 0f),
                },
                new CelLayer
                {
                    Name = "smoke wisps", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.05f, 0f), Start = hit + 0.12f, Duration = 0.3f, Life = ground ? 1.2f : 1.0f, LifeJitter = 0.1f,
                    Close = ground ? 3 : 2, Mid = 1, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.15f, 0f, 0f), Aim = CelAim.Up, Spread = 10f, Speed = 2.1f, SpeedJitter = 0.15f, Drag = 0.35f, Buoyancy = 0.4f, Spin = 2.5f,
                    Size = new CelCurve(0.2f, 0.08f), SizeJitter = 0.15f, Stretch = 1.3f, Erode = new CelCurve(0f, 0.8f, 2.2f),
                    Palette = "fb_smoke", CoolTo = "fb_smoke", Core = 1f, Opacity = 0.8f, Ink = 2f, Extra = new Vector4(0f, 0f, 0.17f, 1.3f),
                },
                new CelLayer
                {
                    Name = "hit light", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, ground ? 0f : -0.9f, 0f), Start = hit, Life = ground ? 1.2f : 0.6f, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 0, Size = new CelCurve(1.2f, 0.8f, 0.5f), SizeJitter = 0.1f, Erode = new CelCurve(0.03f, 0.8f, 3f),
                    Palette = "fb_pool", Core = 1f, Opacity = 0.7f, Extra = new Vector4(0f, 1.3f, 0f, 0f),
                },
                new CelLayer
                {
                    Name = "light", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = hit, Life = 0.45f,
                    Close = 1, Mid = 1, Far = 0, Palette = "fire", Core = ground ? 0.8f : 0.3f, Extra = new Vector4(3.5f, 0f, 0f, 0f),
                });
            if (ground)
            {
                r.Layers.Add(new CelLayer
                {
                    Name = "ground flames", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit + 0.05f, Duration = 1.05f, Life = 0.3f, LifeJitter = 0.25f,
                    Close = 14, Mid = 6, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.2f, 0f, 0f), Aim = CelAim.Up, Spread = 15f, Speed = 1.6f, Buoyancy = 1f,
                    Size = new CelCurve(0.15f, 0.04f, 0.7f), SizeJitter = 0.25f, Stretch = 0.14f, Erode = new CelCurve(0.05f, 0.5f), Palette = "fire", Core = 2.2f, Extra = new Vector4(0f, 0.12f, 0.4f, 0f),
                });
            }
            return r;
        }
    }
}
