// Phase: CelFx job (2026-11-09, lane/show/cel-fire-bolts) - fire bolts (lab only): a volley of comet fire bolts from
// the upper left, each a white-hot head with an orange halo, a red-orange flame tail and a long white-grey smoke
// ribbon, landing on the dummy (a star flash, sparks, a small flame burst, wisps of smoke) and on the ground around and
// behind it (a flash, a small ground fire that flickers and goes out, curling smoke). Warm light pools on the ground
// under the volley. Reference: fire_hitting_cel (spec/fire_bolts.json). Built only from the kit's Streak, Lobe and
// Glow kinds plus a Light, so the effect adds no new kind.
//
// Every bolt is one CelFx.Play of a recipe whose layers are shifted by the bolt's launch time (FireBolts.Volley), so
// the whole volley is spawned at the fire and nothing runs per frame. Flight layers aim at the target with Speed 0 and
// no spread, so head, tail and ribbon arrive together after Flight seconds; the hit layers start then.
// LOD: Close = everything; Mid = no lobes (no smoke, no burst), fewer sparks and flames; Far = head + tail, a flash
// and the pools (and every other bolt, see FireBoltsRange).
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class FireBolts
    {
        /// <summary>Seconds from launch to hit (every bolt: speed follows the distance).</summary>
        public const float Flight = 0.9f;

        /// <summary>Launch times (s after the fire) and whether the bolt lands on the ground (else on the dummy).</summary>
        public static readonly float[] Launch = { 0f, 0.1f, 0.3f, 0.45f, 0.6f, 0.8f, 0.95f, 1.1f, 1.3f, 1.42f, 1.6f, 1.75f, 1.9f, 2.05f, 2.2f, 2.32f, 2.45f, 2.6f, 2.7f, 2.82f, 2.95f, 3.05f };
        public static readonly bool[] OnGround = { false, true, false, false, true, false, true, false, true, false, false, true, false, true, false, true, false, true, false, true, false, true };

        static FireBolts()
        {
            // additive light on the dark field: the deltas that turn the plate's ground into the reference's lit browns
            CelPalette.Add("fb_pool", CelPalette.Hex(0x3A1606), CelPalette.Hex(0x54220A), CelPalette.Hex(0x70300E), CelPalette.Hex(0x904014),
                CelPalette.Hex(0x904014), 1.1f, 4, CelPalette.Hex(0x000000));
            CelPalette.Add("fb_ribbon", CelPalette.Hex(0x4A4A50), CelPalette.Hex(0x7A7A80), CelPalette.Hex(0xA8A8AC), CelPalette.Hex(0xD8D8D8),
                CelPalette.Hex(0xFFFFFF), 1.1f, 3, CelPalette.Hex(0x000000));
            CelPalette.Add("fb_smoke", CelPalette.Hex(0x6E6A6C), CelPalette.Hex(0x9A9496), CelPalette.Hex(0xBDB7B4), CelPalette.Hex(0xDCD6D0),
                CelPalette.Hex(0xDCD6D0), 1.1f, 3, CelPalette.Hex(0x3A3436));
            Volley = new CelFxRecipe[Launch.Length];
            for (int i = 0; i < Launch.Length; i++) Volley[i] = MakeBolt(Launch[i], OnGround[i]);
        }

        // templates (the tests find and validate these)
        public static readonly CelFxRecipe DummyBolt = MakeBolt(0f, false);
        public static readonly CelFxRecipe GroundBolt = MakeBolt(0f, true);
        public static readonly CelFxRecipe Pool = MakePool(0f, 3.6f, 2.4f);
        public static readonly CelFxRecipe PoolLate = MakePool(0.12f, 3.3f, 2.0f);
        /// <summary>The volley: bolt i, shifted by Launch[i].</summary>
        public static readonly CelFxRecipe[] Volley;

        static CelFxRecipe MakePool(float start, float life, float radius)
        {
            return new CelFxRecipe("fire_bolts_pool", start + life,
                new CelLayer
                {
                    Name = "warm ground light", Kind = CelKinds.GlowName, Start = start, Life = life, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 1, Size = new CelCurve(0.55f * radius, radius, 0.35f), SizeJitter = 0f, Erode = new CelCurve(0.03f, 0.5f, 6f),
                    Palette = "fb_pool", Core = 1f, Opacity = 1f, Extra = new Vector4(0f, 0.9f, 0f, 0f),
                },
                new CelLayer
                {
                    Name = "hot centre", Kind = CelKinds.GlowName, Start = start + 0.3f, Life = life - 0.6f, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.35f * radius, 0.55f * radius, 0.5f), SizeJitter = 0f, Erode = new CelCurve(0.05f, 0.6f, 5f),
                    Palette = "fb_pool", Core = 1f, Opacity = 0.55f, Extra = new Vector4(0f, 1.2f, 0f, 0f),
                },
                new CelLayer
                {
                    Name = "warm light", Kind = CelKinds.LightName, Offset = new Vector3(0f, 1f, 0f), Start = start, Life = life,
                    Close = 1, Mid = 1, Far = 1, Palette = "fire", Core = 2.6f, Extra = new Vector4(radius * 3f, 0f, 0f, 0f),
                });
        }

        static CelFxRecipe MakeBolt(float t0, bool ground)
        {
            float hit = t0 + Flight;
            var r = new CelFxRecipe(ground ? "fire_bolt_ground" : "fire_bolt_dummy", hit + 2.4f,
                // ---------------------------------------------------------------- in flight
                new CelLayer
                {
                    Name = "flame tail", Kind = CelKinds.StreakName, Start = t0, Life = Flight, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 1, Aim = CelAim.Target, Speed = 0f, Spread = 0f,
                    Size = new CelCurve(0.55f, 0.55f), SizeJitter = 0.15f, Stretch = 0.22f, Erode = new CelCurve(0.04f, 0.04f),
                    Palette = "fire", Core = 3f, Extra = new Vector4(0.1f, 0.8f, 0.15f, 0f),
                },
                new CelLayer
                {
                    Name = "smoke ribbon", Kind = CelKinds.StreakName, Start = t0, Life = Flight, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 0, Aim = CelAim.Target, Speed = 0f, Spread = 0f,
                    Size = new CelCurve(0.15f, 0.15f), SizeJitter = 0.2f, Stretch = 0.6f, Erode = new CelCurve(0.06f, 0.06f),
                    Palette = "fb_ribbon", Core = 1f, Opacity = 0.6f, Extra = new Vector4(0.35f, 1f, 0f, 0f),
                },
                new CelLayer
                {
                    Name = "head", Kind = CelKinds.GlowName, Start = t0, Life = Flight, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 1, Aim = CelAim.Target, Speed = 0f, Spread = 0f,
                    Size = new CelCurve(0.25f, 0.25f), SizeJitter = 0.1f, Erode = new CelCurve(0.03f, 0.03f),
                    Palette = "fire_hot", Core = 5f, Extra = new Vector4(2f, 0.8f, 0f, 0f),
                },
                new CelLayer
                {
                    Name = "head halo", Kind = CelKinds.GlowName, Start = t0, Life = Flight, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 0, Aim = CelAim.Target, Speed = 0f, Spread = 0f,
                    Size = new CelCurve(0.42f, 0.42f), SizeJitter = 0.1f, Erode = new CelCurve(0.05f, 0.05f),
                    Palette = "fire", Core = 1f, Opacity = 0.25f, Extra = new Vector4(2f, 1.6f, 0f, 0f),
                },
                // ---------------------------------------------------------------- the hit
                new CelLayer
                {
                    Name = "hit flash", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Start = hit, Life = 0.22f, LifeJitter = 0.1f,
                    Close = 1, Mid = 1, Far = 1, Size = new CelCurve(0.4f, 0.8f, 0.5f), SizeJitter = 0.15f, Erode = new CelCurve(0.03f, 0.9f, 1.5f),
                    Palette = "fire_hot", Core = 5f, Spin = 2f, Extra = new Vector4(2f, 1.1f, 9f, 5f),
                },
                new CelLayer
                {
                    Name = "sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit, Duration = 0.06f, Life = 0.38f,
                    Close = ground ? 7 : 12, Mid = 4, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.12f, 0f, 0f), Aim = CelAim.Radial,
                    Speed = 5.5f, SpeedJitter = 0.5f, UpBias = 0.35f, Gravity = 6f, Drag = 2f,
                    Size = new CelCurve(0.045f, 0.025f), Stretch = 0.05f, Erode = new CelCurve(0.05f, 0.5f), Palette = "spark", Core = 4f, Extra = new Vector4(0f, 0.12f, 0f, 0f),
                },
                new CelLayer
                {
                    Name = "flame burst", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Start = hit, Duration = 0.08f, Life = 0.7f,
                    Close = 2, Mid = 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.12f, 0f, 0f), Aim = CelAim.Radial, Speed = 0.9f, UpBias = 0.6f, Drag = 3f, Buoyancy = 1.4f, Spin = 2f,
                    Size = new CelCurve(0.14f, 0.24f, 0.5f), Erode = new CelCurve(0f, 1f, 2f), Feather = 0.1f,
                    Palette = "fire", CoolTo = "fb_smoke", CoolFrom = 0.45f, Core = 2f, Ink = 2f, Extra = new Vector4(1.3f, 0.12f, 0.3f, 0.45f),
                },
                new CelLayer
                {
                    Name = "smoke wisps", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.15f, 0f), Start = hit + 0.15f, Duration = ground ? 1.6f : 1.0f, Life = 1.7f,
                    Close = ground ? 5 : 3, Mid = 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.12f, 0f, 0f), Aim = CelAim.Up, Spread = 20f, Speed = 0.5f, Buoyancy = 0.7f, Drag = 0.6f, Spin = 1.2f,
                    Size = new CelCurve(0.08f, 0.2f, 0.6f), Stretch = 1.6f, Erode = new CelCurve(0.1f, 1f, 1.5f), Feather = 0.05f,
                    Palette = "fb_smoke", Core = 1f, Ink = 1f, Extra = new Vector4(1.2f, 0.1f, 0.8f, 0f),
                },
                new CelLayer
                {
                    Name = "hit light", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, ground ? 0f : -0.9f, 0f), Start = hit, Life = ground ? 1.6f : 0.8f, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 0, Size = new CelCurve(1.4f, 1.0f, 0.5f), SizeJitter = 0.1f, Erode = new CelCurve(0.03f, 0.8f, 3f),
                    Palette = "fb_pool", Core = 1f, Opacity = 0.9f, Extra = new Vector4(0f, 1.3f, 0f, 0f),
                },
                new CelLayer
                {
                    Name = "light", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = hit, Life = 0.5f,
                    Close = 1, Mid = 1, Far = 0, Palette = "fire", Core = 2f, Extra = new Vector4(6f, 0f, 0f, 0f),
                });
            if (ground)
            {
                r.Layers.Add(new CelLayer
                {
                    Name = "ground flames", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit + 0.05f, Duration = 1.5f, Life = 0.32f,
                    Close = 14, Mid = 6, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.28f, 0f, 0f), Aim = CelAim.Up, Spread = 12f, Speed = 1.8f, Buoyancy = 1f,
                    Size = new CelCurve(0.2f, 0.06f, 0.7f), Stretch = 0.14f, Erode = new CelCurve(0.05f, 0.5f), Palette = "fire", Core = 3f, Extra = new Vector4(0f, 0.15f, 0.4f, 0f),
                });
                r.Layers.Add(new CelLayer
                {
                    Name = "ground fire", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Start = hit + 0.05f, Duration = 1.3f, Life = 0.45f,
                    Close = 7, Mid = 0, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.22f, 0f, 0f), Aim = CelAim.Up, Spread = 10f, Speed = 0.6f, Buoyancy = 1.5f, Spin = 3f,
                    Size = new CelCurve(0.15f, 0.05f, 0.8f), Stretch = 1.5f, Erode = new CelCurve(0f, 0.8f, 2f), Feather = 0.06f,
                    Palette = "fire", Core = 2.5f, Ink = 1.5f, Extra = new Vector4(1.8f, 0.2f, 0.2f, 0f),
                });
            }
            return r;
        }
    }
}
