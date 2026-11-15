// Phase: CelFx ice_bolt (2026-11-15, lane/show/cel-ice-bolt) - the rapid ice bolt of the owner's clip ice_bolt_nolora
// (H3 round 6, 8 s at 24 fps): a volley of ice javelins fired from off-screen at the bottom left into the dummy's chest
// (~9 a second, each crossing the frame in six frames), a braid of pale mist ribbons in their wake, a frosty white
// bloom on the board that opens with a crisp starburst, holds, thins while darts glance off to the right as shards,
// opens again with a second starburst, and fades into frost mist; white chips throughout.
// Every number below was read off the clip (celfx job folder: spec/eye/ice_bolt.md): the corridor from a fit to the
// Range's camera (blue_laser's corridor: the Range's own muzzle raised 0.38 m), the times from its frames (the comments
// name them; the effect is filmed with t0 = 0.25 s, frame 6), the sizes from the camera's metres per pixel at the
// dummy (71 px/m) and near the frame edge (150 px/m), the colours from its pixels taken back through the night grade.
// LAB ONLY: no weapon fires it; the Range calls Play.
using UnityEngine;
using TW.Presentation;

namespace TW.Presentation.VFX.Cel
{
    public static class IceBoltRecipe
    {
        /// <summary>The shot time the effect is filmed at (tools/t0_expected.json) and the seconds after the fire at
        /// which a frame of the clip falls.</summary>
        public const float T0 = 0.25f;
        static float AtFrame(float frame) => frame / 24f - T0;

        /// <summary>Metres the Range's muzzle is raised: the clip's corridor enters the frame's left edge at y 722 and
        /// runs through (517, 523) to the chest.</summary>
        public const float MuzzleLift = 0.38f;
        /// <summary>A dart's length (share of the 12.9 m corridor: 1.3 m, 90-240 px near the camera) and its life:
        /// its head runs from -0.05 to DartHeadEnd, so it flies at ~3.7 shares (47 m/s) a second and crosses the
        /// frame in six frames (clip: in at f8, on the board at f14).</summary>
        public const float DartWindow = 0.1f, DartLife = 0.32f;
        const float DartHeadStart = -0.05f, DartHeadEnd = 1f + DartWindow * 1.3f;
        /// <summary>Shares a second a ribbon's head flies (the darts' speed) and the ribbons' wavelength (m).</summary>
        public const float HeadSpeed = (DartHeadEnd - DartHeadStart) / DartLife, Wavelength = 3.2f;
        /// <summary>The hit sits a little toward the shooter of the chest (the clip's bloom is centred ~12 px left of
        /// it): right, up, along the shot (m).</summary>
        static readonly Vector3 HitOffset = new Vector3(-0.1f, 0.02f, -0.08f);
        /// <summary>The beach Range's night vignette as it films at the close shot (NeonAcidRange: fitted to a film of
        /// flat swatches), and its smoothness.</summary>
        const float Vignette = 0.372f, VignetteSmooth = 0.5f;

        // A colour as the frame buffer has to hold it (the project is gamma and the palette stores .linear, so this
        // hands the value over unchanged): each is the clip's colour (in the comment) taken back through the Range's
        // night grade, fitted to the cyan_beam e00cal swatch film (runs/ice_bolt/_ref/look/grade_model.json).
        static Color W(float r, float g, float b) => new Color(Mathf.LinearToGammaSpace(r), Mathf.LinearToGammaSpace(g), Mathf.LinearToGammaSpace(b), 1f);

        // the darts: rim (115,140,175), lower facet (150,177,206), body (169,197,229), upper facet (194,222,252),
        // highlight (222,242,254)
        public static readonly int DartRow = CelPalette.Add("ice_dart",
            W(0.36f, 0.45f, 0.58f), W(0.475f, 0.559f, 0.653f), W(0.532f, 0.625f, 0.766f), W(0.615f, 0.731f, 0.898f),
            W(0.70f, 0.84f, 0.97f), 1.1f, 4, W(0.30f, 0.38f, 0.50f));
        // the ribbons, as they are before their opacity lays them over the ground (the clip shows (107,128,154) there)
        public static readonly int RibbonRow = CelPalette.Add("ice_ribbon",
            W(0.34f, 0.41f, 0.51f), W(0.42f, 0.50f, 0.62f), W(0.52f, 0.62f, 0.76f), W(0.62f, 0.74f, 0.90f),
            W(0.72f, 0.86f, 0.98f), 1.1f, 4, W(0.30f, 0.36f, 0.45f));
        // the bloom: body (166,194,226), lit (190,218,246), core (196,224,252); c1 is the halo it adds round itself
        public static readonly int BloomRow = CelPalette.Add("ice_bloom",
            W(0.30f, 0.38f, 0.50f), W(0.10f, 0.15f, 0.24f), W(0.523f, 0.615f, 0.747f), W(0.60f, 0.71f, 0.87f),
            W(0.66f, 0.80f, 0.95f), 1.1f, 4, W(0.30f, 0.38f, 0.50f));
        // the starbursts, chips and specks: pale ice to white
        public static readonly int HitRow = CelPalette.Add("ice_hit",
            W(0.40f, 0.50f, 0.64f), W(0.48f, 0.57f, 0.70f), W(0.56f, 0.67f, 0.82f), W(0.64f, 0.77f, 0.93f),
            W(0.72f, 0.87f, 0.98f), 1.1f, 4, W(0.30f, 0.38f, 0.50f));
        // the frost mist
        public static readonly int FrostRow = CelPalette.Add("ice_frost",
            W(0.30f, 0.36f, 0.45f), W(0.38f, 0.45f, 0.56f), W(0.47f, 0.56f, 0.68f), W(0.58f, 0.69f, 0.84f),
            W(0.66f, 0.78f, 0.92f), 1.1f, 4, W(0.30f, 0.36f, 0.45f));

        static readonly int LensId = Shader.PropertyToID("_TWIceLens");

        // the lens handed over is a global of the shader: a session that ends puts it back to none
        static IceBoltRecipe() { SceneStatics.Register(nameof(IceBoltRecipe), () => Shader.SetGlobalVector(LensId, Vector4.zero)); }

        /// <summary>The scene's lens vignette (URP Vignette's intensity and smoothness; 0 for none): the ice is written
        /// brighter where the vignette will darken it, so it shows the clip's colours across the frame.</summary>
        public static void Lens(float intensity, float smoothness) =>
            Shader.SetGlobalVector(LensId, new Vector4(Mathf.Max(0f, intensity) * 3f, Mathf.Max(0.01f, smoothness) * 5f, 0f, 0f));

        // ---------------------------------------------------------------- the corridor (played from the muzzle)
        /// <summary>A stream of darts: born on a sphere round the muzzle (the clip's lanes are ~0.3 m apart near the
        /// camera) and all landing on the chest.</summary>
        static CelLayer Darts(string name, int close, int mid, float start, float duration, float width, float spread)
        {
            return new CelLayer
            {
                Name = name, Kind = IceBoltKind.KindName, Start = start, Duration = duration, TimeJitter = 0.06f, Life = DartLife, LifeJitter = 0.08f,
                Close = close, Mid = mid, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(spread, 0f, 0f), Aim = CelAim.Target, Speed = 0f, Spread = 0f, Arrive = 1f,
                Size = new CelCurve(width, width), SizeJitter = 0.35f, Stretch = DartWindow, Erode = new CelCurve(0f, 0f), Feather = 0f,
                Palette = "ice_dart", Core = 1f, Opacity = 1f, Ink = 1f, Extra = new Vector4(IceBoltKind.Dart, 0f, DartHeadStart, DartHeadEnd),
            };
        }

        /// <summary>Mist strands in the darts' wake: the head flies with the darts and stops in the target, the strand
        /// stays where it was drawn, waves across the corridor and fades.</summary>
        static CelLayer Ribbons(string name, int close, int mid, float start, float duration, float life, float width, float amp, float opacity, float spread)
        {
            return new CelLayer
            {
                Name = name, Kind = IceBoltKind.KindName, Start = start, Duration = duration, TimeJitter = 0.05f, Life = life, LifeJitter = 0.3f,
                Close = close, Mid = mid, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(spread, 0f, 0f), Aim = CelAim.Target, Speed = 0f, Spread = 0f, Arrive = 1f,
                Size = new CelCurve(width, width * 0.8f), SizeJitter = 0.55f, Stretch = Wavelength, Erode = new CelCurve(0f, 0.5f, 1.5f), Feather = 0f,
                Spin = 0.8f, Palette = "ice_ribbon", Core = 1f, Opacity = opacity, Ink = 1f, Extra = new Vector4(IceBoltKind.Ribbon, amp, -0.03f, HeadSpeed),
            };
        }

        public static readonly CelFxRecipe Volley = new CelFxRecipe("ice_bolt_volley", 6f,
            // i01: the braid first (drawn under the darts): thin strands and a few wide faint sheets
            Ribbons("ribbons", 64, 24, AtFrame(8) - 0.04f, AtFrame(114) - AtFrame(8), 0.55f, 0.05f, 0.14f, 0.62f, 0.3f),
            Ribbons("ribbon sheets", 20, 6, AtFrame(9) - 0.04f, AtFrame(112) - AtFrame(9), 0.5f, 0.12f, 0.1f, 0.3f, 0.25f),
            // i01: ~40 darts from f8 (in at the left edge) to f114 (the last in), landing f14..f120
            Darts("darts", 40, 20, AtFrame(8) - 0.035f, AtFrame(114) - AtFrame(8), 0.16f, 0.3f));

        // ---------------------------------------------------------------- the hit (played at the chest)
        /// <summary>A dot of the IceBolt kind at the hit (Stretch is minus its minimum px at Far; 0.001 = none).</summary>
        static CelLayer Dot(string name, string palette, Vector4 extra, float start, float duration, float life, int close, int mid, CelCurve size, float pull = 0.6f)
        {
            return new CelLayer
            {
                Name = name, Kind = IceBoltKind.KindName, Anchor = CelAnchor.Target, Offset = HitOffset, Start = start, Duration = duration, TimeJitter = 0.02f, Life = life, LifeJitter = 0.15f,
                Close = close, Mid = mid, Far = 0, Shape = CelShape.Point, Aim = CelAim.None, Speed = 0f, Size = size, SizeJitter = 0.12f, Stretch = -0.001f,
                Erode = new CelCurve(0f, 0.9f, 6f), Feather = pull, Palette = palette, Core = 1f, Opacity = 1f, Extra = extra,
            };
        }

        static CelLayer Dim(CelLayer l, float opacity) { l.Opacity = opacity; return l; }
        static CelLayer At(CelLayer l, Vector3 offset) { l.Offset = offset; return l; }
        static CelLayer Around(CelLayer l, float radius) { l.Shape = CelShape.Sphere; l.ShapeSize = new Vector3(radius, 0f, 0f); return l; }
        static CelLayer Fade(CelLayer l, float pow) { l.Erode = new CelCurve(0f, 0.9f, pow); return l; }

        public static readonly CelFxRecipe Hit = new CelFxRecipe("ice_bolt_hit", 7f,
            // i01: the first bloom f16.5 -> f77, its bright body 26 px in radius at f18, 33 at f40, 40 at f55, thinning
            // from f62 and gone by f76 (the body is 0.74 of the quad, so these are quad radii)
            Dot("bloom", "ice_bloom", new Vector4(IceBoltKind.Bloom, 0.16f, 0.45f, 1f), AtFrame(16.5f), 0f, AtFrame(77f) - AtFrame(16.5f), 1, 1, new CelCurve(0.46f, 0.78f, 0.45f)),
            // the second bloom f89 -> f124: 30 px at f92, 40 at f100-f110, 24 at f118
            Fade(Dot("bloom 2", "ice_bloom", new Vector4(IceBoltKind.Bloom, 0.16f, 0.45f, 1f), AtFrame(89f), 0f, AtFrame(125f) - AtFrame(89f), 1, 1, new CelCurve(0.55f, 0.78f, 0.35f)), 3f),
            // it flickers frame to frame: short brighter pulses over the holds
            Dim(Dot("bloom pulse", "ice_bloom", new Vector4(IceBoltKind.Bloom, 0.2f, 0.55f, 0.5f), AtFrame(18f), AtFrame(70f) - AtFrame(18f), 0.1f, 36, 10, new CelCurve(0.38f, 0.5f)), 0.55f),
            Dim(Dot("bloom pulse 2", "ice_bloom", new Vector4(IceBoltKind.Bloom, 0.2f, 0.55f, 0.5f), AtFrame(91f), AtFrame(116f) - AtFrame(91f), 0.1f, 18, 6, new CelCurve(0.4f, 0.52f)), 0.55f),
            // the two crisp starbursts that open the blooms (f16, f88): ~15 uneven spikes 60 px across
            Dot("star", "ice_hit", new Vector4(IceBoltKind.Star, 0.07f, 15f, 0.2f), AtFrame(15.6f), 0f, 0.11f, 1, 1, new CelCurve(0.5f, 0.62f, 0.5f), 0.7f),
            Dot("star 2", "ice_hit", new Vector4(IceBoltKind.Star, 0.07f, 15f, 0.2f), AtFrame(87.6f), 0f, 0.11f, 1, 1, new CelCurve(0.52f, 0.64f, 0.5f), 0.7f),
            // smaller stars flash at the rim through the holds as darts land
            Dim(Around(Dot("rim stars", "ice_hit", new Vector4(IceBoltKind.Star, 0.05f, 11f, 0.14f), AtFrame(19f), AtFrame(70f) - AtFrame(19f), 0.07f, 16, 6, new CelCurve(0.3f, 0.36f), 0.7f), 0.2f), 0.85f),
            Dim(Around(Dot("rim stars 2", "ice_hit", new Vector4(IceBoltKind.Star, 0.05f, 11f, 0.14f), AtFrame(92f), AtFrame(116f) - AtFrame(92f), 0.07f, 8, 3, new CelCurve(0.3f, 0.36f), 0.7f), 0.2f), 0.85f),
            new CelLayer
            {
                // white chips thrown out of the hit within ~100 px, falling (f16-f140)
                Name = "chips", Kind = IceBoltKind.KindName, Anchor = CelAnchor.Target, Offset = HitOffset, Start = AtFrame(16f), Duration = AtFrame(128f) - AtFrame(16f), TimeJitter = 0.03f, Life = 0.5f, LifeJitter = 0.35f,
                Close = 110, Mid = 40, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.3f, 0f, 0f), Aim = CelAim.Radial, Speed = 3.2f, SpeedJitter = 0.6f, UpBias = 0.25f, Gravity = 5f, Drag = 1.2f,
                Size = new CelCurve(0.022f, 0.014f), SizeJitter = 0.5f, Stretch = -0.001f, Erode = new CelCurve(0f, 0.4f, 2f), Feather = 0.7f,
                Palette = "ice_hit", Core = 1f, Opacity = 1f, Extra = new Vector4(IceBoltKind.Fleck, 2.2f, 0f, 0f),
            },
            new CelLayer
            {
                // the gap (f64-f92): darts break on the board and glance off to the right and up-right as shards
                Name = "shards", Kind = IceBoltKind.KindName, Anchor = CelAnchor.Target, Offset = HitOffset, Start = AtFrame(63f), Duration = AtFrame(92f) - AtFrame(63f), TimeJitter = 0.04f, Life = 0.42f, LifeJitter = 0.3f,
                Close = 22, Mid = 8, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.25f, 0f, 0f), Aim = CelAim.Dir, Speed = 9f, SpeedJitter = 0.4f, Spread = 40f, Gravity = 3f, Drag = 1.5f, Spin = 6f,
                Size = new CelCurve(0.075f, 0.06f), SizeJitter = 0.45f, Stretch = -0.001f, Erode = new CelCurve(0f, 0.3f, 2f), Feather = 0.5f,
                Palette = "ice_dart", Core = 1f, Opacity = 1f, Extra = new Vector4(IceBoltKind.Shard, 3.2f, 0f, 0f),
            },
            // frost mist left of the dummy as the second bloom holds (f104-f132) and right of it as it fades (f120-f152)
            At(Dot("frost left", "ice_frost", new Vector4(IceBoltKind.Frost, 4.5f, 1.4f, 0f), AtFrame(102f), 0.1f, 1.25f, 2, 1, new CelCurve(0.45f, 0.62f, 0.5f), 0.5f), new Vector3(-1.2f, 0.05f, -0.2f)),
            At(Dot("frost right", "ice_frost", new Vector4(IceBoltKind.Frost, 4.5f, 1.4f, 0f), AtFrame(118f), 0.15f, 1.5f, 2, 1, new CelCurve(0.42f, 0.6f, 0.5f), 0.5f), new Vector3(0.9f, 0.05f, 0f)),
            Around(Dot("frost rim", "ice_frost", new Vector4(IceBoltKind.Frost, 5.5f, 1.2f, 0f), AtFrame(98f), AtFrame(112f) - AtFrame(98f), 0.9f, 6, 2, new CelCurve(0.3f, 0.42f, 0.5f), 0.6f), 0.45f));

        /// <summary>Fire it: the volley from the muzzle to the target, the hit at the target. LAB ONLY.</summary>
        public static void Play(Vector3 from, Vector3 to, uint seed)
        {
            Vector3 dir = to - from;
            CelFx.Play(Volley, from, dir, to, seed);
            CelFx.Play(Hit, to, dir, to, seed + 1u);
        }

        /// <summary>The Range shot: from the raised muzzle (the clip's darts enter at the frame's lower-left edge) to
        /// the chest. The close film counts at Close (the Range's own zoom reads as Mid) and the vignette is handed over.</summary>
        [CelRangeEffect("ice_bolt")]
        static void Range(RangeShot s)
        {
            CelFx.ZoomOverride = s.Lod == RangeLod.Close ? CelFx.CloseZoom : s.Zoom;
            Lens(Vignette, VignetteSmooth);
            Play(s.Muzzle + Vector3.up * MuzzleLift, s.Target, (uint)s.Seed);
        }
    }
}
