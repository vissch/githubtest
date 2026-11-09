// Phase: CelFx job, flamethrower (2026-11-09, lane/show/cel-flamethrower) - the cel flamethrower after the owner's
// reference clip flamethrower_nolora: a broad cone of flame from the lower left that widens onto the target and
// engulfs it (FlameJet, its edge a row of attached tongues licking up, banded white, yellow, orange, a thin red rim),
// round curling fire lobes rolling up and downstream off the target and growing to the cut, a black ink smoke
// column of round lobes that grows from above the target up and downstream and thins to grey, a big orange light
// pool on the ground, embers and sparks; when the jet is cut the fire at the target burns down to a small remnant.
// Times are seconds after the fire (the Range fires at 0.8 s; the clip's jet is already half way out by then, so the
// jet starts half grown). Clip, in seconds after our fire: jet front on the target 0.45, jet cut 3.15-3.45, burning
// smoulder to 4.2, smoke from 1.2 (small), column to 4.5, grey and gone by 5.9.
// The effect's target is the dummy's upper chest (Play aims 0.45 m above the chest), so the jet's axis runs over the
// board like the clip's; the target layers' offsets are from there (the foot is 1.4 m below it).
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
            // the jet: a thin red-orange rim, orange, a gold band, the clip's yellow (#f8e671), a white core
            CelPalette.Add(Flame, CelPalette.Hex(0xC24A12), CelPalette.Hex(0xEC8A2C), CelPalette.Hex(0xF9C04A), CelPalette.Hex(0xF8E671),
                CelPalette.Hex(0xFFF6DC), 0.84f, 4, CelPalette.Hex(0x5A1606));
            // the fire round the target: the same colours, a smaller white heart
            CelPalette.Add(Burn, CelPalette.Hex(0xD0601A), CelPalette.Hex(0xEC8A2C), CelPalette.Hex(0xF9B840), CelPalette.Hex(0xF8DC68),
                CelPalette.Hex(0xFFF4D8), 0.94f, 4, CelPalette.Hex(0x3A0E06));
            // the smoke: near black with a brown lit crescent on each lobe, so the lobes read one by one
            CelPalette.Add(SmokeInk, CelPalette.Hex(0x1A1210), CelPalette.Hex(0x2E211B), CelPalette.Hex(0x4A362B), CelPalette.Hex(0x735544),
                CelPalette.Hex(0x735544), 1.1f, 3, CelPalette.Hex(0x050303));
            // the column thinning: a cold grey-blue like the clip's last frames
            CelPalette.Add(SmokeGrey, CelPalette.Hex(0x26272E), CelPalette.Hex(0x383A45), CelPalette.Hex(0x4E5262), CelPalette.Hex(0x666B7E),
                CelPalette.Hex(0x666B7E), 1.1f, 3, CelPalette.Hex(0x14141A));
            CelPalette.Add(Pool, CelPalette.Hex(0x3A2006), CelPalette.Hex(0x704010), CelPalette.Hex(0xA8641C), CelPalette.Hex(0xD88A2C),
                CelPalette.Hex(0xE8A040), 1.1f, 4, CelPalette.Hex(0x000000));
        }

        /// <summary>How far above the chest the jet is aimed (the dummy's upper chest), m.</summary>
        public const float AimLift = 0.45f;

        // the flame reaches the target and holds for JetLife seconds (the clip: cut at 3.15-3.45 s after our fire)
        const float JetLife = 3.45f;

        // Offsets of the target layers are from the aim point: the chest is 0.45 m below it, the foot 1.4 m.
        public static readonly CelFxRecipe Recipe = new CelFxRecipe("flamethrower", 6.2f,
            new CelLayer
            {
                // the cone: 0.12 m at the nozzle (on screen at the lower left), 3.5 m at the dummy, 30 % on past it; its
                // edge is a row of attached tongues (ragged on top). Extra: flow 9 m/s, tongue frequency 1.0 /m (about
                // ten tongues along the top), core streaks 0.8, upper-edge licking 1
                Name = "jet", Kind = "FlameJet", Start = 0f, Life = JetLife, LifeJitter = 0f, Offset = new Vector3(0f, 0.25f, 0.9f),
                Close = 1, Mid = 1, Far = 1, Shape = CelShape.Point, Aim = CelAim.Target, Speed = 0f, Spread = 0f, Arrive = 1f, SpeedJitter = 0f,
                Size = new CelCurve(0.12f, 3.5f, 1.0f), SizeJitter = 0f, Stretch = 0.3f,
                Erode = new CelCurve(0.4f, 0.32f, 0.55f), Feather = 0.45f,
                Palette = Flame, Core = 3.5f, Ink = 1.5f, Extra = new Vector4(9f, 1.0f, 0.8f, 1f),
            },
            // the fire at the target: round curling lobes that roll up and downstream off the dummy, in three waves of
            // growing size so the burning mass keeps growing to the cut (the clip's peak, ~2.7 s), cooling to smoke
            new CelLayer
            {
                Name = "fire a", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.65f, 0.1f), Start = 0.5f, Duration = 2.6f, Life = 0.8f, LifeJitter = 0.25f,
                Close = 26, Mid = 8, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.45f, 0f, 0f), Aim = CelAim.Dir, Speed = 1.6f, SpeedJitter = 0.3f, Spread = 55f, Drag = 1.2f, Buoyancy = 1.0f, Spin = 1.2f,
                Size = new CelCurve(0.25f, 0.6f, 0.6f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 2.5f), Feather = 0.12f,
                Palette = Burn, CoolTo = SmokeInk, CoolFrom = 0.75f, Core = 2.5f, Ink = 2.2f, Extra = new Vector4(1.4f, 0.18f, 0.5f, 0.75f),
            },
            new CelLayer
            {
                Name = "fire b", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.35f, 0.3f), Start = 1.3f, Duration = 1.85f, Life = 0.9f, LifeJitter = 0.25f,
                Close = 22, Mid = 6, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.55f, 0f, 0f), Aim = CelAim.Dir, Speed = 1.8f, SpeedJitter = 0.3f, Spread = 50f, Drag = 1.1f, Buoyancy = 1.0f, Spin = 1.0f,
                Size = new CelCurve(0.4f, 0.9f, 0.6f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 2.5f), Feather = 0.12f,
                Palette = Burn, CoolTo = SmokeInk, CoolFrom = 0.75f, Core = 2.5f, Ink = 2.4f, Extra = new Vector4(1.3f, 0.18f, 0.5f, 0.75f),
            },
            new CelLayer
            {
                Name = "fire c", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.1f, 0.4f), Start = 1.9f, Duration = 1.2f, Life = 1.0f, LifeJitter = 0.2f,
                Close = 16, Mid = 4, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.6f, 0f, 0f), Aim = CelAim.Dir, Speed = 1.8f, SpeedJitter = 0.3f, Spread = 45f, Drag = 1.0f, Buoyancy = 1.0f, Spin = 0.9f,
                Size = new CelCurve(0.55f, 1.15f, 0.6f), SizeJitter = 0.25f, Erode = new CelCurve(0f, 1f, 2.5f), Feather = 0.12f,
                Palette = Burn, CoolTo = SmokeInk, CoolFrom = 0.75f, Core = 2.5f, Ink = 2.5f, Extra = new Vector4(1.2f, 0.18f, 0.5f, 0.75f),
            },
            new CelLayer
            {
                // after the cut: a small smoulder at the board's foot, out before 4.3 s
                Name = "smoulder", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -1.25f, 0f), Start = 3.1f, Duration = 0.5f, Life = 0.55f, LifeJitter = 0.2f,
                Close = 10, Mid = 3, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.35f, 0f, 0f), Aim = CelAim.Up, Speed = 0.6f, Spread = 30f, Drag = 0.8f, Buoyancy = 0.8f, Spin = 1f,
                Size = new CelCurve(0.3f, 0.12f, 1f), SizeJitter = 0.25f, Erode = new CelCurve(0f, 1f, 2f), Feather = 0.1f,
                Palette = Burn, CoolTo = SmokeInk, CoolFrom = 0.7f, Core = 2f, Ink = 2f, Extra = new Vector4(1.4f, 0.16f, 0.5f, 0.7f),
            },
            new CelLayer
            {
                // the column's base: lobes that are fire for a moment, then smoke (fire glowing in the column's foot)
                Name = "smoke base", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.4f, 0.5f), Start = 0.9f, Duration = 2.4f, Life = 0.9f,
                Close = 12, Mid = 4, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.6f, 0f, 0f), Aim = CelAim.Up, Speed = 1.2f, Spread = 30f, Drag = 0.6f, Buoyancy = 1.2f, Spin = 0.8f,
                Size = new CelCurve(0.35f, 0.8f, 0.6f), Erode = new CelCurve(0f, 1f, 3f), Feather = 0.1f,
                Palette = Burn, CoolTo = SmokeInk, CoolFrom = 0.15f, Core = 2f, Ink = 2.5f, Extra = new Vector4(1.3f, 0.16f, 0.6f, 0.2f),
            },
            new CelLayer
            {
                // the column: big round ink lobes born over the fire, rising fast (buoyancy) and drifting downstream
                // (Aim Dir = right on screen), at the frame's top by ~2.8 s, kept to ~4.7 s, cooling to grey and eroding away
                Name = "smoke column", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.9f, 0.6f), Start = 0.9f, Duration = 2.5f, Life = 2.1f, LifeJitter = 0.15f,
                Close = 30, Mid = 10, Far = 1, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.7f, 0f, 0f), Aim = CelAim.Dir, Speed = 1.4f, Spread = 30f, Drag = 0.5f, Buoyancy = 1.6f, Spin = 0.4f,
                Size = new CelCurve(0.55f, 1.35f, 0.5f), SizeJitter = 0.25f, Erode = new CelCurve(0f, 1f, 3.5f), Feather = 0.1f,
                Palette = SmokeInk, CoolTo = SmokeGrey, CoolFrom = 0.6f, Core = 1f, Ink = 3f, Extra = new Vector4(1.2f, 0.15f, 0.8f, 0.6f),
            },
            new CelLayer
            {
                Name = "pool target", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -1.4f, 0f), Start = 0f, Life = 4.0f, LifeJitter = 0f,
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
                Name = "pool afterburn", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -1.4f, 0f), Start = 3.2f, Life = 1.1f, LifeJitter = 0f,
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
                // a dense spray of small embers thrown up off the target, arcing over it and falling
                Name = "ember arcs", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.2f, 0.2f), Start = 0.6f, Duration = 3.0f, Life = 1.3f, LifeJitter = 0.4f,
                Close = 300, Mid = 60, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.8f, 0f, 0f), Aim = CelAim.Radial, Speed = 5f, SpeedJitter = 0.5f, UpBias = 1.6f, Gravity = 3.5f, Drag = 0.5f,
                Size = new CelCurve(0.09f, 0.05f), Stretch = 0.04f, Erode = new CelCurve(0.05f, 0.6f, 2f), Palette = "ember", Core = 4f, Extra = new Vector4(1f, 0.05f, 0.5f, 0f),
            },
            new CelLayer
            {
                // embers floating up with the smoke, on after the cut
                Name = "embers", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.45f, 0f), Start = 1.0f, Duration = 3.8f, Life = 2.0f,
                Close = 90, Mid = 25, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(1.4f, 0f, 0f), Aim = CelAim.Radial, Speed = 2.5f, SpeedJitter = 0.5f, UpBias = 1f, Drag = 0.8f, Buoyancy = 1f,
                Size = new CelCurve(0.06f, 0.035f), Stretch = 0.035f, Erode = new CelCurve(0.05f, 0.6f, 2f), Palette = "ember", Core = 3.5f, Extra = new Vector4(1f, 0.06f, 0.5f, 0f),
            },
            new CelLayer
            {
                Name = "light", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.45f, 0f), Start = 0.1f, Life = 3.8f,
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
