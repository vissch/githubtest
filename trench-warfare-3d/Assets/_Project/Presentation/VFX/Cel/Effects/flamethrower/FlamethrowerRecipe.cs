// Phase: CelFx job, flamethrower (2026-11-09, lane/show/cel-flamethrower) - the cel flamethrower after the owner's
// reference clip flamethrower_nolora: a broad cone of flame from the lower left that widens onto the target and
// engulfs it (FlameJet, its upper edge licking up in tongues, a long white-yellow core), flame splashing sideways
// round the target and climbing it in licking tongues (more FlameJet ribbons, no round lobes), a black ink smoke
// column of round lobes that grows from above the target up and downstream and thins to grey, a big orange light
// pool on the ground, embers and sparks; when the jet is cut the fire at the target burns down to a small remnant.
// Times are seconds after the fire (the Range fires at 0.8 s; the clip's jet is already half way out by then, so the
// jet starts half grown). Clip, in seconds after our fire: jet front on the target 0.45, jet cut 3.15-3.45, burning
// remnant to 4.0, smoke from 1.2 (small), column to 4.5, grey and gone by 5.9.
// The effect's target is the dummy's head (Play aims 0.7 m above the chest), so the jet's axis runs over the board
// like the clip's; the target layers' offsets are from there (the foot is 1.65 m below it).
// The Range hook: FlamethrowerRange.cs. LOD: Close all of it; Mid fewer pieces; Far the jet, the pool, one smoke lobe.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class FlamethrowerRecipe
    {
        public const string Flame = "ft_flame", Burn = "ft_burn", SmokeInk = "ft_smoke", SmokeGrey = "ft_smoke_grey", Pool = "ft_pool";

        static FlamethrowerRecipe() { AddPalettes(); }

        /// <summary>The effect's palettes (sRGB picked off the reference frames).</summary>
        public static void AddPalettes()
        {
            CelPalette.Add(Flame, CelPalette.Hex(0xC8460F), CelPalette.Hex(0xE97A24), CelPalette.Hex(0xFBB840), CelPalette.Hex(0xF8E671),
                CelPalette.Hex(0xFFF6DC), 0.8f, 4, CelPalette.Hex(0x5A1606));
            // the fire round the target: the same colours, a smaller white heart
            CelPalette.Add(Burn, CelPalette.Hex(0xB23A0E), CelPalette.Hex(0xE0701E), CelPalette.Hex(0xF8A838), CelPalette.Hex(0xFAD866),
                CelPalette.Hex(0xFFF4D8), 0.9f, 4, CelPalette.Hex(0x3A0E06));
            CelPalette.Add(SmokeInk, CelPalette.Hex(0x1E1512), CelPalette.Hex(0x35261F), CelPalette.Hex(0x4E392E), CelPalette.Hex(0x6A4E3C),
                CelPalette.Hex(0x6A4E3C), 1.1f, 3, CelPalette.Hex(0x070404));
            // the column thinning: a cold grey-blue like the clip's last frames
            CelPalette.Add(SmokeGrey, CelPalette.Hex(0x26272E), CelPalette.Hex(0x383A45), CelPalette.Hex(0x4E5262), CelPalette.Hex(0x666B7E),
                CelPalette.Hex(0x666B7E), 1.1f, 3, CelPalette.Hex(0x14141A));
            CelPalette.Add(Pool, CelPalette.Hex(0x3A2006), CelPalette.Hex(0x704010), CelPalette.Hex(0xA8641C), CelPalette.Hex(0xD88A2C),
                CelPalette.Hex(0xE8A040), 1.1f, 4, CelPalette.Hex(0x000000));
        }

        /// <summary>How far above the chest the jet is aimed (the dummy's head), m.</summary>
        public const float AimLift = 0.7f;

        // the flame reaches the target and holds for JetLife seconds (the clip: cut at 3.15-3.45 s after our fire)
        const float JetLife = 3.45f;

        public static readonly CelFxRecipe Recipe = new CelFxRecipe("flamethrower", 6.2f,
            new CelLayer
            {
                // the cone: 0.15 m at the nozzle (on screen at the lower left), 3.6 m at the dummy, 30 % on past it.
                // Extra: flow 9 m/s, tongue frequency 1.6 /m, core streaks 0.8, upper-edge licking 1
                Name = "jet", Kind = "FlameJet", Start = 0f, Life = JetLife, LifeJitter = 0f, Offset = new Vector3(0f, 0.25f, 0.9f),
                Close = 1, Mid = 1, Far = 1, Shape = CelShape.Point, Aim = CelAim.Target, Speed = 0f, Spread = 0f, Arrive = 1f, SpeedJitter = 0f,
                Size = new CelCurve(0.15f, 3.6f, 0.9f), SizeJitter = 0f, Stretch = 0.3f,
                Erode = new CelCurve(0.4f, 0.32f, 0.6f), Feather = 0.45f,
                Palette = Flame, Core = 3.5f, Ink = 1.5f, Extra = new Vector4(9f, 1.6f, 0.8f, 1f),
            },
            new CelLayer
            {
                // tongues climbing the dummy: short ribbons rising from the board's foot, wide at the base, pointed at the tip;
                // each one grows up, then its base lifts off and runs up into the tip (a lick)
                Name = "climb", Kind = "FlameJet", Anchor = CelAnchor.Target, Offset = new Vector3(0f, -1.25f, 0.1f), Start = 0.35f, Duration = 2.75f, Life = 0.85f, LifeJitter = 0.3f,
                Close = 16, Mid = 5, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(1.6f, 0.5f, 1.2f), Aim = CelAim.Up, Speed = 3.0f, SpeedJitter = 0.3f, Spread = 30f,
                Size = new CelCurve(1.3f, 0.25f, 1f), SizeJitter = 0.25f, Stretch = 0f,
                Erode = new CelCurve(0.25f, 0.4f, 0.65f), Feather = 0f,
                Palette = Burn, Core = 3f, Ink = 1.5f, Extra = new Vector4(3f, 1.8f, 0.4f, 0f),
            },
            new CelLayer
            {
                // the splash: flame thrown sideways along the ground round the board
                Name = "splash", Kind = "FlameJet", Anchor = CelAnchor.Target, Offset = new Vector3(0f, -1.45f, -0.2f), Start = 0.4f, Duration = 2.6f, Life = 0.75f, LifeJitter = 0.3f,
                Close = 10, Mid = 3, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.4f, 0f, 0f), Aim = CelAim.Radial, Speed = 2.4f, SpeedJitter = 0.3f, UpBias = 0.35f,
                Size = new CelCurve(1.0f, 0.45f, 1f), SizeJitter = 0.25f, Stretch = 0f,
                Erode = new CelCurve(0.2f, 0.35f, 0.6f), Feather = 0f,
                Palette = Burn, Core = 3f, Ink = 1.5f, Extra = new Vector4(3f, 1.8f, 0.3f, 0f),
            },
            new CelLayer
            {
                // after the cut: the fire on the dummy burns on, lower and lower
                Name = "afterburn", Kind = "FlameJet", Anchor = CelAnchor.Target, Offset = new Vector3(0f, -1.45f, 0f), Start = 3.15f, Duration = 0.55f, Life = 0.7f, LifeJitter = 0.25f,
                Close = 8, Mid = 3, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(1.0f, 0.3f, 0.8f), Aim = CelAim.Up, Speed = 2.0f, SpeedJitter = 0.3f, Spread = 22f,
                Size = new CelCurve(1.0f, 0.2f, 1f), SizeJitter = 0.25f, Stretch = 0f,
                Erode = new CelCurve(0.2f, 0.35f, 0.6f), Feather = 0f,
                Palette = Burn, Core = 2.5f, Ink = 1.5f, Extra = new Vector4(2.5f, 2f, 0.3f, 0f),
            },
            new CelLayer
            {
                Name = "remnant", Kind = "FlameJet", Anchor = CelAnchor.Target, Offset = new Vector3(0f, -1.6f, 0f), Start = 3.6f, Duration = 0.45f, Life = 0.5f, LifeJitter = 0.25f,
                Close = 5, Mid = 2, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(0.6f, 0.1f, 0.5f), Aim = CelAim.Up, Speed = 1.2f, SpeedJitter = 0.3f, Spread = 18f,
                Size = new CelCurve(0.55f, 0.1f, 1f), SizeJitter = 0.25f, Stretch = 0f,
                Erode = new CelCurve(0.15f, 0.25f, 0.5f), Feather = 0f,
                Palette = Burn, Core = 2f, Ink = 1.2f, Extra = new Vector4(2f, 2.2f, 0.2f, 0f),
            },
            new CelLayer
            {
                // the column's base: lobes that are fire for a moment, then smoke (fire showing in the column's foot)
                Name = "smoke base", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.3f, 0.5f), Start = 1.0f, Duration = 2.3f, Life = 1.0f,
                Close = 10, Mid = 3, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.5f, 0f, 0f), Aim = CelAim.Up, Speed = 1.0f, Spread = 25f, Drag = 0.6f, Buoyancy = 0.6f, Spin = 0.8f,
                Size = new CelCurve(0.25f, 0.55f, 0.6f), Erode = new CelCurve(0f, 1f, 3f), Feather = 0.1f,
                Palette = Burn, CoolTo = SmokeInk, CoolFrom = 0.2f, Core = 2f, Ink = 2.2f, Extra = new Vector4(1.3f, 0.16f, 0.6f, 0.2f),
            },
            new CelLayer
            {
                // the column: round ink lobes born small just over the flame, growing and drifting up and downstream
                // (Aim Dir + buoyancy = up and to the right on screen), cooling to grey and eroding away
                Name = "smoke column", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.8f, 0.8f), Start = 1.0f, Duration = 2.5f, Life = 2.2f, LifeJitter = 0.12f,
                Close = 36, Mid = 12, Far = 1, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.6f, 0f, 0f), Aim = CelAim.Dir, Speed = 0.45f, Spread = 35f, Drag = 0.2f, Buoyancy = 0.35f, Spin = 0.5f,
                Size = new CelCurve(0.3f, 0.85f, 0.45f), SizeJitter = 0.25f, Erode = new CelCurve(0f, 1f, 3.5f), Feather = 0.1f,
                Palette = SmokeInk, CoolTo = SmokeGrey, CoolFrom = 0.55f, Core = 1f, Ink = 2.5f, Extra = new Vector4(1.3f, 0.16f, 0.8f, 0.55f),
            },
            new CelLayer
            {
                Name = "pool target", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -1.65f, 0f), Start = 0f, Life = 4.0f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Size = new CelCurve(5.5f, 7.0f, 0.3f), SizeJitter = 0f, Erode = new CelCurve(0.02f, 0.6f, 6f),
                Palette = Pool, Core = 1f, Opacity = 1f, Extra = new Vector4(0f, 0.6f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "pool jet", Kind = CelKinds.GlowName, Offset = new Vector3(0f, -1.3f, 3.0f), Start = 0f, Life = 3.5f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(4.8f, 5.0f), SizeJitter = 0f, Erode = new CelCurve(0.02f, 0.5f, 6f),
                Palette = Pool, Core = 1f, Opacity = 0.9f, Extra = new Vector4(0f, 0.9f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "pool afterburn", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -1.65f, 0f), Start = 3.2f, Life = 1.4f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(3.0f, 1.5f), SizeJitter = 0f, Erode = new CelCurve(0.02f, 0.9f, 1.5f),
                Palette = Pool, Core = 1f, Opacity = 0.8f, Extra = new Vector4(0f, 0.9f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "sparks", Kind = CelKinds.StreakName, Offset = new Vector3(0f, 0.2f, 2.0f), Start = 0f, Duration = 3.2f, Life = 0.6f,
                Close = 80, Mid = 20, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.5f, 0f, 0f), Aim = CelAim.Dir, Speed = 11f, SpeedJitter = 0.4f, Spread = 18f, Drag = 0.8f, Buoyancy = 1f,
                Size = new CelCurve(0.05f, 0.03f), Stretch = 0.03f, Erode = new CelCurve(0.05f, 0.5f), Palette = "spark", Core = 3.5f, Extra = new Vector4(0.2f, 0.05f, 0.3f, 0f),
            },
            new CelLayer
            {
                Name = "embers", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.4f, 0f), Start = 0.4f, Duration = 4.0f, Life = 1.8f,
                Close = 140, Mid = 40, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(1.3f, 0f, 0f), Aim = CelAim.Radial, Speed = 3f, SpeedJitter = 0.5f, UpBias = 1f, Drag = 0.8f, Buoyancy = 1f,
                Size = new CelCurve(0.06f, 0.035f), Stretch = 0.035f, Erode = new CelCurve(0.05f, 0.6f, 2f), Palette = "ember", Core = 3f, Extra = new Vector4(1f, 0.06f, 0.5f, 0f),
            },
            new CelLayer
            {
                Name = "light", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.7f, 0f), Start = 0.1f, Life = 3.8f,
                Close = 1, Mid = 1, Far = 1, Palette = Flame, Core = 3f, Extra = new Vector4(16f, 0f, 0f, 0f),
            });

        /// <summary>Fire the flamethrower from `nozzle` onto `target` (a body's chest).</summary>
        public static void Play(Vector3 nozzle, Vector3 target, uint seed) => Play(Recipe, nozzle, target, seed);

        /// <summary>Fire a version of the recipe (FlamethrowerRange plays a time-stretched copy).</summary>
        public static void Play(CelFxRecipe recipe, Vector3 nozzle, Vector3 target, uint seed)
        {
            Vector3 aim = target + Vector3.up * AimLift;
            CelFx.Play(recipe, nozzle, aim - nozzle, aim, seed);
        }

        /// <summary>A copy of `src` played `k` times slower (all times x k, speeds / k, accelerations / k^2) and `delay`
        /// seconds later. FlameJet's grow/cut seconds (Erode from/to) and flow speed (Extra.x) and a streak's trail
        /// seconds (Stretch) scale with it.</summary>
        public static CelFxRecipe TimeScaled(CelFxRecipe src, float k, float delay)
        {
            var dst = new CelFxRecipe(src.Name, src.Length * k + delay);
            float k2 = k * k;
            foreach (var s in src.Layers)
            {
                var l = JsonUtility.FromJson<CelLayer>(JsonUtility.ToJson(s));
                l.Start = l.Start * k + delay; l.Duration *= k; l.TimeJitter *= k; l.Life *= k;
                l.Speed /= k; l.Gravity /= k2; l.Buoyancy /= k2; l.Drag /= k; l.Spin /= k;
                if (l.Kind == "FlameJet") { l.Erode = new CelCurve(l.Erode.From * k, l.Erode.To * k, l.Erode.Pow); l.Extra.x /= k; }
                if (l.Kind == CelKinds.StreakName) l.Stretch *= k;
                dst.Layers.Add(l);
            }
            return dst;
        }
    }
}
