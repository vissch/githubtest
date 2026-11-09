// Phase: CelFx job, flamethrower (2026-11-09, lane/show/cel-flamethrower) - the cel flamethrower after the owner's
// reference clip flamethrower_nolora: a thick jet from the lower left that widens onto the target (FlameJet), tongues
// licking off it, flame splashing round the target and building into a fireball, a black ink smoke column of round
// lobes rising above it, a big orange light pool on the ground, embers and sparks; when the jet is cut the fire at
// the target burns on, cools to smoke and the column drifts up and goes.
// Times are seconds after the fire (the Range fires at 0.8 s; the clip's jet is already half way out by then, so the
// jet starts half grown). Clip, in shot seconds: jet 0.33-3.9, fireball 1.1-4.8, smoke 1.9-6.5, gone 6.7.
// The Range hook: FlamethrowerRange.cs. LOD: Close all of it; Mid fewer pieces; Far the jet and the pool.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class FlamethrowerRecipe
    {
        public const string Flame = "ft_flame", Burn = "ft_burn", SmokeInk = "ft_smoke", Pool = "ft_pool";

        static FlamethrowerRecipe() { AddPalettes(); }

        /// <summary>The effect's palettes (sRGB picked off the reference frames).</summary>
        public static void AddPalettes()
        {
            CelPalette.Add(Flame, CelPalette.Hex(0xC2410F), CelPalette.Hex(0xEC7A22), CelPalette.Hex(0xFBB53B), CelPalette.Hex(0xF9E272),
                CelPalette.Hex(0xFFF4D8), 0.84f, 4, CelPalette.Hex(0x5A1606));
            // the fire round the target: the same colours, a smaller white heart (it reads as flame, not a blast)
            CelPalette.Add(Burn, CelPalette.Hex(0xA8300C), CelPalette.Hex(0xE0641A), CelPalette.Hex(0xF8A030), CelPalette.Hex(0xFCD860),
                CelPalette.Hex(0xFFF4D8), 0.96f, 4, CelPalette.Hex(0x3A0E06));
            CelPalette.Add(SmokeInk, CelPalette.Hex(0x1E1512), CelPalette.Hex(0x35261F), CelPalette.Hex(0x543D31), CelPalette.Hex(0x7A5A45),
                CelPalette.Hex(0x7A5A45), 1.1f, 3, CelPalette.Hex(0x070404));
            CelPalette.Add(Pool, CelPalette.Hex(0x3A2006), CelPalette.Hex(0x704010), CelPalette.Hex(0xA8641C), CelPalette.Hex(0xD88A2C),
                CelPalette.Hex(0xE8A040), 1.1f, 4, CelPalette.Hex(0x000000));
        }

        // the flame reaches the target and holds for JetLife seconds (the clip: 3.1 s after our fire)
        const float JetLife = 3.15f;

        public static readonly CelFxRecipe Recipe = new CelFxRecipe("flamethrower", 6.2f,
            new CelLayer
            {
                Name = "jet", Kind = "FlameJet", Start = 0f, Life = JetLife, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Shape = CelShape.Point, Aim = CelAim.Target, Speed = 0f, Spread = 0f, Arrive = 1f, SpeedJitter = 0f,
                Size = new CelCurve(0.25f, 3.0f, 1.4f), SizeJitter = 0f, Stretch = 0.08f,
                Erode = new CelCurve(0.35f, 0.45f, 0.9f), Feather = 0.5f,
                Palette = Flame, Core = 3.5f, Ink = 1.5f, Extra = new Vector4(9f, 1.2f, 0.8f, 0f),
            },
            new CelLayer
            {
                Name = "tongues", Kind = CelKinds.LobeName, Start = 0.05f, Duration = 3.0f, Life = 0.6f, LifeJitter = 0.3f,
                Close = 24, Mid = 8, Far = 0, Offset = new Vector3(0f, 0.15f, 3.6f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.5f, 0f, 0f),
                Aim = CelAim.Dir, Speed = 5f, Spread = 16f, Drag = 1.5f, Buoyancy = 1.2f, Spin = 2.5f,
                Size = new CelCurve(0.2f, 0.45f, 0.6f), Erode = new CelCurve(0f, 1f, 2.5f), Feather = 0.1f,
                Palette = Flame, Core = 3f, Ink = 1.5f, Extra = new Vector4(1.5f, 0.18f, 0.2f, 0f),
            },
            new CelLayer
            {
                Name = "splash", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.4f), Start = 0.25f, Duration = 1.1f, Life = 0.9f,
                Close = 24, Mid = 8, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.4f, 0f, 0f), Aim = CelAim.Radial, Speed = 2.5f, UpBias = 0.5f, Drag = 2.5f, Buoyancy = 1.6f, Spin = 2f,
                Size = new CelCurve(0.25f, 0.55f, 0.5f), Erode = new CelCurve(0f, 1f, 2.5f), Feather = 0.1f,
                Palette = Burn, CoolTo = SmokeInk, CoolFrom = 0.6f, Core = 3.5f, Ink = 2f, Extra = new Vector4(1.4f, 0.16f, 0.3f, 0.6f),
            },
            new CelLayer
            {
                Name = "fireball", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.5f, 0f), Start = 1.1f, Duration = 2.0f, Life = 1.1f,
                Close = 45, Mid = 14, Far = 1, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.6f, 0f, 0f), Aim = CelAim.Radial, Speed = 2f, UpBias = 0.8f, Drag = 2f, Buoyancy = 2f, Spin = 2f,
                Size = new CelCurve(0.35f, 0.8f, 0.5f), Erode = new CelCurve(0f, 1f, 2.5f), Feather = 0.1f,
                Palette = Burn, CoolTo = SmokeInk, CoolFrom = 0.5f, Core = 2.5f, Ink = 2.2f, Extra = new Vector4(1.4f, 0.16f, 0.3f, 0.5f),
            },
            new CelLayer
            {
                Name = "afterburn", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.6f, 0f), Start = 3.0f, Duration = 0.9f, Life = 1.0f,
                Close = 22, Mid = 8, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.6f, 0f, 0f), Aim = CelAim.Radial, Speed = 1.5f, UpBias = 1f, Drag = 1.5f, Buoyancy = 1.6f, Spin = 1.5f,
                Size = new CelCurve(0.35f, 0.7f, 0.5f), Erode = new CelCurve(0f, 1f, 2f), Feather = 0.1f,
                Palette = Burn, CoolTo = SmokeInk, CoolFrom = 0.4f, Core = 3f, Ink = 2.2f, Extra = new Vector4(1.4f, 0.16f, 0.3f, 0.4f),
            },
            new CelLayer
            {
                Name = "smoke column", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 1.3f, 0.5f), Start = 1.0f, Duration = 3.0f, Life = 2.1f,
                Close = 70, Mid = 22, Far = 1, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.9f, 0f, 0f), Aim = CelAim.Up, Speed = 1.6f, Spread = 22f, Drag = 0.5f, Buoyancy = 0.7f, Spin = 0.6f,
                Size = new CelCurve(0.55f, 1.3f, 0.6f), Erode = new CelCurve(0f, 1f, 4f), Feather = 0.1f,
                Palette = SmokeInk, Core = 1f, Ink = 2.5f, Extra = new Vector4(1.3f, 0.16f, 0.8f, 0f),
            },
            new CelLayer
            {
                Name = "pool target", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.95f, 0f), Start = 0f, Life = 4.0f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Size = new CelCurve(5.0f, 7.0f, 0.3f), SizeJitter = 0f, Erode = new CelCurve(0.02f, 0.6f, 6f),
                Palette = Pool, Core = 1f, Opacity = 1f, Extra = new Vector4(0f, 0.6f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "pool jet", Kind = CelKinds.GlowName, Offset = new Vector3(0f, -1.3f, 3.0f), Start = 0f, Life = 3.3f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(4.8f, 5.0f), SizeJitter = 0f, Erode = new CelCurve(0.02f, 0.5f, 6f),
                Palette = Pool, Core = 1f, Opacity = 0.9f, Extra = new Vector4(0f, 0.9f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "pool afterburn", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.95f, 0f), Start = 3.0f, Life = 1.8f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(3.0f, 2.0f), SizeJitter = 0f, Erode = new CelCurve(0.02f, 0.9f, 1.5f),
                Palette = Pool, Core = 1f, Opacity = 0.8f, Extra = new Vector4(0f, 0.9f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "sparks", Kind = CelKinds.StreakName, Offset = new Vector3(0f, 0f, 1.5f), Start = 0f, Duration = 3.0f, Life = 0.6f,
                Close = 80, Mid = 20, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.4f, 0f, 0f), Aim = CelAim.Dir, Speed = 11f, SpeedJitter = 0.4f, Spread = 18f, Drag = 0.8f, Buoyancy = 1f,
                Size = new CelCurve(0.05f, 0.03f), Stretch = 0.03f, Erode = new CelCurve(0.05f, 0.5f), Palette = "spark", Core = 3.5f, Extra = new Vector4(0.2f, 0.05f, 0.3f, 0f),
            },
            new CelLayer
            {
                Name = "embers", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = 0.4f, Duration = 4.0f, Life = 1.8f,
                Close = 140, Mid = 40, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(1.3f, 0f, 0f), Aim = CelAim.Radial, Speed = 3f, SpeedJitter = 0.5f, UpBias = 1f, Drag = 0.8f, Buoyancy = 1f,
                Size = new CelCurve(0.06f, 0.035f), Stretch = 0.035f, Erode = new CelCurve(0.05f, 0.6f, 2f), Palette = "ember", Core = 3f, Extra = new Vector4(1f, 0.06f, 0.5f, 0f),
            },
            new CelLayer
            {
                Name = "light", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = 0.1f, Life = 3.8f,
                Close = 1, Mid = 1, Far = 1, Palette = Flame, Core = 3f, Extra = new Vector4(16f, 0f, 0f, 0f),
            });

        /// <summary>Fire the flamethrower from `nozzle` onto `target`.</summary>
        public static void Play(Vector3 nozzle, Vector3 target, uint seed) => CelFx.Play(Recipe, nozzle, target - nozzle, target, seed);
    }
}
