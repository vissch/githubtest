// Phase: CelFx ice_bolt (2026-11-15, lane/show/cel-ice-bolt) - the rapid ice bolt of the owner's clip ice_bolt_nolora
// (H3 round 6, 8 s at 24 fps): a volley of ice javelins fired from off-screen at the bottom left into the dummy's chest
// (a shot every ~4 frames, one to three darts to a shot, each crossing the frame in six frames), a braid of pale mist
// ribbons in their wake, a frosty white bloom on the board that opens with a crisp starburst, holds, thins while darts
// glance off to the right as shards, opens again with a second starburst, and fades into frost mist; white chips
// throughout.
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
        /// <summary>A dart's length (share of the 12.9 m corridor: ~1.9 m, 90-240 px near the camera), its life and
        /// its width at the barbs (m; 12-30 px near the camera): its head runs from -0.05 to DartHeadEnd, so it flies
        /// at ~3.9 shares (50 m/s) a second and crosses the frame in six frames (clip: in at f8, on the board at f14).</summary>
        public const float DartWindow = 0.15f, DartLife = 0.32f, DartWidth = 0.22f;
        const float DartHeadStart = -0.05f, DartHeadEnd = 1f + DartWindow * 1.3f;
        /// <summary>Shares a second a ribbon's head flies (the darts' speed) and the ribbons' wavelength (m: the clip's
        /// strands bend once over 200-300 px).</summary>
        public const float HeadSpeed = (DartHeadEnd - DartHeadStart) / DartLife, Wavelength = 4.4f;
        /// <summary>The hit sits a little toward the shooter of the chest (the clip's bloom is centred ~12 px left of
        /// it): right, up, along the shot (m).</summary>
        static readonly Vector3 HitOffset = new Vector3(-0.1f, 0.02f, -0.08f);
        /// <summary>The beach Range's night vignette as it films at the close shot (NeonAcidRange: fitted to a film of
        /// flat swatches), and its smoothness.</summary>
        const float Vignette = 0.372f, VignetteSmooth = 0.5f;

        // A colour as the frame buffer has to hold it (the project is gamma and the palette stores .linear, so this
        // hands the value over unchanged): each is the clip's colour (in the comment) taken back through the Range's
        // night grade, fitted to the cyan_beam e00cal swatch film (runs/ice_bolt/_ref/look/grade_model.json; its blue
        // tops out near 247, so the brightest blues are written at 0.95-1).
        static Color W(float r, float g, float b) => new Color(Mathf.LinearToGammaSpace(r), Mathf.LinearToGammaSpace(g), Mathf.LinearToGammaSpace(b), 1f);

        // the darts: rim (115,140,175), lower facet (150,177,206), body (169,197,229), upper facet (194,222,252),
        // highlight (222,242,254); i01 filmed them a little grey in blue (mean b 218 for the clip's 226)
        public static readonly int DartRow = CelPalette.Add("ice_dart",
            W(0.36f, 0.45f, 0.60f), W(0.465f, 0.555f, 0.68f), W(0.525f, 0.625f, 0.80f), W(0.60f, 0.725f, 0.94f),
            W(0.68f, 0.83f, 1f), 1.1f, 4, W(0.30f, 0.38f, 0.50f));
        // the ribbons, as they are before their opacity lays them over the ground (the clip shows (96,116,141) there)
        public static readonly int RibbonRow = CelPalette.Add("ice_ribbon",
            W(0.36f, 0.43f, 0.54f), W(0.44f, 0.53f, 0.66f), W(0.54f, 0.645f, 0.80f), W(0.64f, 0.76f, 0.93f),
            W(0.72f, 0.86f, 1f), 1.1f, 4, W(0.30f, 0.36f, 0.45f));
        // the bloom (its pixels solved for a colour over an opacity on three grounds, f22-f112): cloud (175,205,240),
        // lit (186,216,248), core (197,226,254); c1 is the faint halo it adds round itself
        public static readonly int BloomRow = CelPalette.Add("ice_bloom",
            W(0.30f, 0.38f, 0.50f), W(0.08f, 0.12f, 0.2f), W(0.54f, 0.645f, 0.92f), W(0.585f, 0.70f, 0.95f),
            W(0.60f, 0.74f, 1f), 1.1f, 4, W(0.30f, 0.38f, 0.50f));
        // the starbursts, chips and specks: pale ice to white
        public static readonly int HitRow = CelPalette.Add("ice_hit",
            W(0.40f, 0.50f, 0.64f), W(0.48f, 0.57f, 0.72f), W(0.56f, 0.67f, 0.86f), W(0.62f, 0.75f, 0.96f),
            W(0.68f, 0.82f, 1f), 1.1f, 4, W(0.30f, 0.38f, 0.50f));
        // the frost mist
        public static readonly int FrostRow = CelPalette.Add("ice_frost",
            W(0.30f, 0.36f, 0.45f), W(0.38f, 0.45f, 0.56f), W(0.47f, 0.56f, 0.70f), W(0.58f, 0.69f, 0.86f),
            W(0.66f, 0.78f, 0.95f), 1.1f, 4, W(0.30f, 0.36f, 0.45f));

        static readonly int LensId = Shader.PropertyToID("_TWIceLens");

        // the lens handed over is a global of the shader: a session that ends puts it back to none
        static IceBoltRecipe() { SceneStatics.Register(nameof(IceBoltRecipe), () => Shader.SetGlobalVector(LensId, Vector4.zero)); }

        /// <summary>The scene's lens vignette (URP Vignette's intensity and smoothness; 0 for none): the ice is written
        /// brighter where the vignette will darken it, so it shows the clip's colours across the frame.</summary>
        public static void Lens(float intensity, float smoothness) =>
            Shader.SetGlobalVector(LensId, new Vector4(Mathf.Max(0f, intensity) * 3f, Mathf.Max(0.01f, smoothness) * 5f, 0f, 0f));

        // ---------------------------------------------------------------- the volley, shot by shot (from the muzzle)
        /// <summary>The clip's rhythm: a shot every 3.2-4.8 frames from f8 (the first point in at the left edge) to
        /// f114 (the last), one dart (50 %), two (35 %) or three (15 %) to a shot on lanes up to LaneSpread apart near
        /// the camera, all landing on the chest; each dart drags its own wake of mist on its lane, born with it.</summary>
        const float FirstShot = 8f, LastShot = 114f, ShotGapMin = 3.2f, ShotGapMax = 4.8f, LaneSpread = 0.3f;
        /// <summary>A wake's life (s), its width (m: 8-13 px soft strands near the camera), its wave's amplitude (m)
        /// and its opacity.</summary>
        const float WakeLife = 0.62f, WakeWidth = 0.13f, WakeAmp = 0.12f, WakeOpacity = 0.6f;
        /// <summary>The gap (f66-f90): the bloom is gone and the darts glance off the board, so a share of the wakes
        /// runs on past the dummy to the upper right (the clip's f80 strands reach ~(1100, 250)).</summary>
        const float GlanceFrom = 66f, GlanceTo = 90f, GlanceShare = 0.45f;

        /// <summary>Every shot of the volley: the wakes in one pass (drawn under), the darts in the next (on top); both
        /// draw the same numbers, so a dart and its wakes share a lane. Thinned by the zoom and the quality like a
        /// layer's count (Mid: every other shot and one wake in two darts, Far: one shot in four, no wakes).</summary>
        static void Shots(CelFxRenderer renderer, int kind, float now, Vector3 from, Vector3 to, float lod, float quality, uint seed, bool darts)
        {
            var rng = new CelRng(seed * 977u + 13u);
            CelFx.Frame(to - from, out _, out Vector3 right, out Vector3 up);
            bool mid = lod >= 0.5f, far = lod >= 1.5f;
            float keep = (far ? 0.25f : mid ? 0.5f : 1f) * quality, acc = 0.9999f;
            for (float f = FirstShot; f <= LastShot; f += rng.Range(ShotGapMin, ShotGapMax))
            {
                float g = rng.Next();
                int n = g < 0.5f ? 1 : g < 0.85f ? 2 : 3;
                Vector3 centre = rng.InSphere() * LaneSpread;
                acc += keep;
                bool on = acc >= 1f;
                if (on) acc -= 1f;
                for (int d = 0; d < n; d++)
                {
                    float at = AtFrame(f + (d == 0 ? 0f : rng.Range(0.4f, 1.8f))) - 0.035f;
                    Vector3 lane = centre + (d == 0 ? Vector3.zero : (right * rng.Range(-1f, 1f) + up * rng.Range(-1f, 1f)) * 0.12f);
                    float life = rng.Jitter(DartLife, 0.06f), width = rng.Jitter(DartWidth, 0.3f), dseed = rng.Next();
                    int wakes = far ? 0 : mid ? (rng.Next() < 0.5f ? 1 : 0) : (rng.Next() < 0.7f ? 1 : 2);
                    if (on && darts) renderer.Spawn(kind, Dart(now + at, from + lane, to, life, width, dseed));
                    for (int w = 0; w < 2; w++)
                    {
                        float wAt = at + rng.Range(0f, 0.02f);
                        Vector3 wo = (right * rng.Range(-1f, 1f) + up * rng.Range(-1f, 1f)) * 0.06f;
                        float wLife = rng.Jitter(WakeLife, 0.25f), wWidth = rng.Jitter(WakeWidth, 0.4f), wAmp = rng.Jitter(WakeAmp, 0.4f);
                        float wDrift = rng.Jitter(0.8f, 0.5f), wSeed = rng.Next();
                        bool glance = f >= GlanceFrom && f <= GlanceTo && rng.Next() < GlanceShare;
                        Vector3 end = to + right * rng.Range(3.2f, 4.5f) + up * rng.Range(1.8f, 2.6f);
                        if (on && !darts && w < wakes) renderer.Spawn(kind, Wake(now + wAt, from + lane + wo, glance ? end : to, wLife, wWidth, wAmp, wDrift, wSeed));
                    }
                }
            }
        }

        /// <summary>One dart: a window that runs down the span from the muzzle lane into the chest and stops there.</summary>
        static CelRecord Dart(float born, Vector3 p, Vector3 to, float life, float width, float seed) => new CelRecord
        {
            Pos = p, Born = born, Vel = (to - p) / life, Life = life, Axis = Vector3.up, Seed = seed,
            Size = new Vector4(width, width, 1f, DartWindow), Motion = Vector4.zero, Erode = new Vector4(0f, 0f, 1f, 0f),
            Tone = new Vector4(CelRecord.PackPalette(DartRow), 1f, 1f, 1f),
            Extra = new Vector4(IceBoltKind.Dart, 0f, DartHeadStart, DartHeadEnd),
        };

        /// <summary>One wake: a mist strand whose head flies with its dart and stops in the target; the strand stays
        /// where it was drawn, waves across the corridor and fades.</summary>
        static CelRecord Wake(float born, Vector3 p, Vector3 to, float life, float width, float amp, float drift, float seed) => new CelRecord
        {
            Pos = p, Born = born, Vel = (to - p) / life, Life = life, Axis = Vector3.up, Seed = seed,
            Size = new Vector4(width, width * 0.8f, 1f, Wavelength), Motion = new Vector4(0f, 0f, drift, 0f),
            Erode = new Vector4(0f, 0.5f, 1.5f, 0f),
            Tone = new Vector4(CelRecord.PackPalette(RibbonRow), 1f, WakeOpacity, 1f),
            Extra = new Vector4(IceBoltKind.Ribbon, amp, DartHeadStart - 0.04f, HeadSpeed),
        };

        /// <summary>Mist sheets: wide faint strands over the whole volley, not tied to a dart.</summary>
        static CelLayer Sheets(string name, int close, int mid, float start, float duration, float life, float width, float amp, float opacity, float spread)
        {
            return new CelLayer
            {
                Name = name, Kind = IceBoltKind.KindName, Start = start, Duration = duration, TimeJitter = 0.05f, Life = life, LifeJitter = 0.3f,
                Close = close, Mid = mid, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(spread, 0f, 0f), Aim = CelAim.Target, Speed = 0f, Spread = 0f, Arrive = 1f,
                Size = new CelCurve(width, width * 0.8f), SizeJitter = 0.45f, Stretch = Wavelength, Erode = new CelCurve(0f, 0.5f, 1.5f), Feather = 0f,
                Spin = 0.6f, Palette = "ice_ribbon", Core = 1f, Opacity = opacity, Ink = 1f, Extra = new Vector4(IceBoltKind.Ribbon, amp, -0.06f, HeadSpeed),
            };
        }

        /// <summary>The volley's layers beside the shots: the wide faint sheets the strands lie in.</summary>
        public static readonly CelFxRecipe Volley = new CelFxRecipe("ice_bolt_volley", 6f,
            Sheets("ribbon sheets", 16, 5, AtFrame(9) - 0.04f, AtFrame(112) - AtFrame(9), 0.6f, 0.3f, 0.1f, 0.2f, 0.25f));

        // ---------------------------------------------------------------- the hit (played at the chest)
        /// <summary>A dot of the IceBolt kind at the hit (Stretch is minus its minimum px at Far; 0.001 = none).</summary>
        static CelLayer Dot(string name, string palette, Vector4 extra, float start, float duration, float life, int close, int mid, CelCurve size, float pull = 0.6f, int far = 0)
        {
            return new CelLayer
            {
                Name = name, Kind = IceBoltKind.KindName, Anchor = CelAnchor.Target, Offset = HitOffset, Start = start, Duration = duration, TimeJitter = 0.02f, Life = life, LifeJitter = 0.15f,
                Close = close, Mid = mid, Far = far, Shape = CelShape.Point, Aim = CelAim.None, Speed = 0f, Size = size, SizeJitter = 0.12f, Stretch = -0.001f,
                Erode = new CelCurve(0f, 0.9f, 6f), Feather = pull, Palette = palette, Core = 1f, Opacity = 1f, Extra = extra,
            };
        }

        static CelLayer Dim(CelLayer l, float opacity) { l.Opacity = opacity; return l; }
        static CelLayer At(CelLayer l, Vector3 offset) { l.Offset = offset; return l; }
        static CelLayer Around(CelLayer l, float radius) { l.Shape = CelShape.Sphere; l.ShapeSize = new Vector3(radius, 0f, 0f); return l; }
        static CelLayer Fade(CelLayer l, float pow) { l.Erode = new CelCurve(0f, 0.9f, pow); return l; }
        static CelLayer Exact(CelLayer l) { l.TimeJitter = 0f; l.LifeJitter = 0f; l.SizeJitter = 0f; return l; }

        public static readonly CelFxRecipe Hit = new CelFxRecipe("ice_bolt_hit", 7f,
            // i02: the first bloom f16.5 -> f77 as the clip's radial profile (f22-f64): an opaque core ~27 px in
            // radius that keeps its size (0.655 of its birth quad, at most half the quad), fading into a frosty cloud
            // at 0.6 opacity that reaches 60 px at f18, 72 at f22, 90 at f30, 100 at f40, 110 at f56; it thins from
            // f62 (the core first, gone by f72) and is gone by f78
            Exact(Dot("bloom", "ice_bloom", new Vector4(IceBoltKind.Bloom, 0.6f, 0.655f, 1f), AtFrame(16.5f), 0f, AtFrame(77f) - AtFrame(16.5f), 1, 1, new CelCurve(0.55f, 1.64f, 0.4f), 0.9f, 1)),
            // the second bloom f89 -> f138: 75 px at f92, 96-108 at f104-f120, its core fading from f108, the cloud by f136
            Exact(Fade(Dot("bloom 2", "ice_bloom", new Vector4(IceBoltKind.Bloom, 0.6f, 0.62f, 1f), AtFrame(89f), 0f, AtFrame(138f) - AtFrame(89f), 1, 1, new CelCurve(0.52f, 1.74f, 0.4f), 0.9f, 1), 3.2f)),
            // its body boils: denser clumps of cloud come and go inside it
            Dim(Around(Dot("bloom clumps", "ice_bloom", new Vector4(IceBoltKind.Bloom, 0.55f, 0f, 0.5f), AtFrame(19f), AtFrame(70f) - AtFrame(19f), 0.14f, 40, 12, new CelCurve(0.45f, 0.6f), 0.95f), 0.4f), 0.6f),
            Dim(Around(Dot("bloom clumps 2", "ice_bloom", new Vector4(IceBoltKind.Bloom, 0.55f, 0f, 0.5f), AtFrame(91f), AtFrame(126f) - AtFrame(91f), 0.14f, 30, 8, new CelCurve(0.5f, 0.65f), 0.95f), 0.45f), 0.6f),
            // the two crisp starbursts that open the blooms (f16, f88): ~12 fat uneven spikes 60-70 px across, a white core
            Dot("star", "ice_hit", new Vector4(IceBoltKind.Star, 0.2f, 12f, 0.3f), AtFrame(15.6f), 0f, 0.11f, 1, 1, new CelCurve(0.5f, 0.6f, 0.5f), 1f, 1),
            Dot("star 2", "ice_hit", new Vector4(IceBoltKind.Star, 0.2f, 13f, 0.3f), AtFrame(87.6f), 0f, 0.11f, 1, 1, new CelCurve(0.55f, 0.66f, 0.5f), 1f, 1),
            // the forming bloom's splash: a few spiky stars at its rim f17-f26
            Dim(Around(Dot("rim splash", "ice_hit", new Vector4(IceBoltKind.Star, 0.12f, 9f, 0.18f), AtFrame(17f), AtFrame(26f) - AtFrame(17f), 0.08f, 6, 2, new CelCurve(0.3f, 0.36f), 1f), 0.35f), 0.85f),
            new CelLayer
            {
                // white chips (4-8 px petals at 1x) thrown out of the hit to ~100-150 px, some falling (f16-f140)
                Name = "chips", Kind = IceBoltKind.KindName, Anchor = CelAnchor.Target, Offset = HitOffset, Start = AtFrame(16f), Duration = AtFrame(128f) - AtFrame(16f), TimeJitter = 0.03f, Life = 0.55f, LifeJitter = 0.35f,
                Close = 220, Mid = 70, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.45f, 0f, 0f), Aim = CelAim.Radial, Speed = 3.6f, SpeedJitter = 0.6f, UpBias = 0.25f, Gravity = 5f, Drag = 1.2f,
                Size = new CelCurve(0.04f, 0.028f), SizeJitter = 0.5f, Stretch = -0.001f, Erode = new CelCurve(0f, 0.4f, 2f), Feather = 1f,
                Palette = "ice_hit", Core = 1f, Opacity = 1f, Extra = new Vector4(IceBoltKind.Fleck, 2.1f, 0f, 0f),
            },
            new CelLayer
            {
                // glitter: tiny white points that wink in and out over the cloud through the holds
                Name = "glitter", Kind = IceBoltKind.KindName, Anchor = CelAnchor.Target, Offset = HitOffset, Start = AtFrame(18f), Duration = AtFrame(72f) - AtFrame(18f), TimeJitter = 0.04f, Life = 0.18f, LifeJitter = 0.4f,
                Close = 90, Mid = 20, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(1.25f, 0f, 0f), Aim = CelAim.None, Speed = 0f,
                Size = new CelCurve(0.018f, 0.014f), SizeJitter = 0.5f, Stretch = -0.001f, Erode = new CelCurve(0f, 0.5f, 2f), Feather = 1.4f,
                Palette = "ice_hit", Core = 1f, Opacity = 1f, Extra = new Vector4(IceBoltKind.Speck, 0f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "glitter 2", Kind = IceBoltKind.KindName, Anchor = CelAnchor.Target, Offset = HitOffset, Start = AtFrame(92f), Duration = AtFrame(128f) - AtFrame(92f), TimeJitter = 0.04f, Life = 0.18f, LifeJitter = 0.4f,
                Close = 60, Mid = 14, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(1.35f, 0f, 0f), Aim = CelAim.None, Speed = 0f,
                Size = new CelCurve(0.018f, 0.014f), SizeJitter = 0.5f, Stretch = -0.001f, Erode = new CelCurve(0f, 0.5f, 2f), Feather = 1.4f,
                Palette = "ice_hit", Core = 1f, Opacity = 1f, Extra = new Vector4(IceBoltKind.Speck, 0f, 0f, 0f),
            },
            new CelLayer
            {
                // the gap (f64-f92): darts break on the board and glance off to the right and up-right as shards,
                // two-tone javelins 20-60 px long (the biggest ~90)
                Name = "shards", Kind = IceBoltKind.KindName, Anchor = CelAnchor.Target, Offset = HitOffset, Start = AtFrame(63f), Duration = AtFrame(92f) - AtFrame(63f), TimeJitter = 0.04f, Life = 0.42f, LifeJitter = 0.3f,
                Close = 30, Mid = 10, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.3f, 0f, 0f), Aim = CelAim.Dir, Speed = 9f, SpeedJitter = 0.4f, Spread = 40f, Gravity = 3f, Drag = 1.5f, Spin = 5f,
                Size = new CelCurve(0.1f, 0.08f), SizeJitter = 0.5f, Stretch = -0.001f, Erode = new CelCurve(0f, 0.3f, 2f), Feather = 0.8f,
                Palette = "ice_dart", Core = 1f, Opacity = 1f, Extra = new Vector4(IceBoltKind.Shard, 3.4f, 0f, 0f),
            },
            new CelLayer
            {
                // a few more as the second bloom fades (f116-f136)
                Name = "shards 2", Kind = IceBoltKind.KindName, Anchor = CelAnchor.Target, Offset = HitOffset, Start = AtFrame(116f), Duration = AtFrame(136f) - AtFrame(116f), TimeJitter = 0.04f, Life = 0.4f, LifeJitter = 0.3f,
                Close = 10, Mid = 4, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.3f, 0f, 0f), Aim = CelAim.Dir, Speed = 7f, SpeedJitter = 0.4f, Spread = 55f, Gravity = 3f, Drag = 1.5f, Spin = 5f,
                Size = new CelCurve(0.08f, 0.065f), SizeJitter = 0.5f, Stretch = -0.001f, Erode = new CelCurve(0f, 0.3f, 2f), Feather = 0.8f,
                Palette = "ice_dart", Core = 1f, Opacity = 1f, Extra = new Vector4(IceBoltKind.Shard, 3.2f, 0f, 0f),
            },
            // frost mist left of the dummy as the second bloom holds (f104-f132) and right of it as it fades (f120-f152)
            At(Dot("frost left", "ice_frost", new Vector4(IceBoltKind.Frost, 4.5f, 1.4f, 0f), AtFrame(100f), 0.1f, 1.3f, 2, 1, new CelCurve(0.45f, 0.58f, 0.5f), 0.5f), new Vector3(-1.45f, 0.55f, -0.2f)),
            At(Dot("frost right", "ice_frost", new Vector4(IceBoltKind.Frost, 4.5f, 1.4f, 0f), AtFrame(120f), 0.12f, 1.3f, 2, 1, new CelCurve(0.42f, 0.56f, 0.5f), 0.5f), new Vector3(1.05f, 0.05f, 0f)),
            Around(Dot("frost rim", "ice_frost", new Vector4(IceBoltKind.Frost, 5.5f, 1.2f, 0f), AtFrame(98f), AtFrame(112f) - AtFrame(98f), 0.9f, 6, 2, new CelCurve(0.3f, 0.42f, 0.5f), 0.6f), 0.45f));

        /// <summary>Fire it: the volley from the muzzle to the target (its sheets, then every shot's wakes, then its
        /// darts), the hit at the target. LAB ONLY. No allocation.</summary>
        public static void Play(Vector3 from, Vector3 to, uint seed)
        {
            Vector3 dir = to - from;
            CelFx.Play(Volley, from, dir, to, seed);
            var renderer = CelFx.Renderer;
            int kind = renderer.KindId(IceBoltKind.KindName);
            if (kind >= 0)
            {
                float now = CelFx.Now, lod = CelFx.LodBlend(CelFx.Zoom), q = CelFx.QualityShare(FxQuality.Tier);
                Shots(renderer, kind, now, from, to, lod, q, seed, false);
                Shots(renderer, kind, now, from, to, lod, q, seed, true);
            }
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
