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
        /// runs through (517, 523) to the chest (i09: its bright pixels f20-f60 lie 8-28 px below the 0.38 m of i01-i08,
        /// most near the camera).</summary>
        public const float MuzzleLift = 0.24f;
        /// <summary>A dart's length (share of the 12.9 m corridor: ~1.9 m, 90-240 px near the camera), its life and
        /// its width at the barbs (m; 12-30 px near the camera): its head runs from -0.05 to DartHeadEnd, so it flies
        /// at ~3.9 shares (50 m/s) a second and crosses the frame in six frames (clip: in at f8, on the board at f14).</summary>
        // i11: 0.28 m at the barbs (0.22): i10's darts read twice the clip's slenderness near the camera (~15:1 for
        // its ~8:1); i12 a 0.145 window (i11 0.135) for the barbed arrow
        // i16: 0.135 and 0.26 (i15 0.145, 0.28): the clip's darts median 104 x 18 px, i15's 129 x 24
        // (i17 0.24: i16 filmed 124 x 23)
        public const float DartWindow = 0.135f, DartLife = 0.32f, DartWidth = 0.24f;
        const float DartHeadStart = -0.05f, DartHeadEnd = 1f + DartWindow * 1.3f;
        /// <summary>Shares a second a ribbon's head flies (the darts' speed) and the ribbons' wavelength (m: the clip's
        /// strands bend once over 200-300 px; i12 5.5: through the holds f30-f60 they run nearly straight).</summary>
        public const float HeadSpeed = (DartHeadEnd - DartHeadStart) / DartLife, Wavelength = 5.5f;
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

        // the darts (i09, the clip's at 3x f11-f48: a sky-blue crystal, bluer than the pale grey-white of the strands,
        // which i08's pale lavender darts melted into; i12 whiter - at the film's size the clip's read as white-blue
        // ice, i11's deep-blue lower facet dulled ours): deep (118,158,215), lower facet (158,198,243), mid
        // (188,224,252), upper facet (208,237,255), glint (232,248,255); i13 halfway back to i11's sky blue: i12's
        // filmed (179,206,235) with tops (221,238,242), as white as the streaks' lit edges, for the clip's
        // (170,198,229) and (211,234,254)
        // i16: the mid tones less blue (i15 filmed lum p50 (156,189,234) and p75 (177,209,249) for the clip's
        // (153,180,213) and (169,199,233); p10 and p90 matched)
        public static readonly int DartRow = CelPalette.Add("ice_dart",
            // (i17: i16 filmed the lower half (152,181,221) for the clip's (152,179,210))
            W(0.345f, 0.46f, 0.675f), W(0.465f, 0.575f, 0.77f), W(0.55f, 0.68f, 0.83f), W(0.625f, 0.78f, 0.92f),
            W(0.74f, 0.925f, 1f), 1.1f, 4, W(0.30f, 0.38f, 0.50f));
        // the ribbons, as they are before their opacity lays them over the ground (the clip shows (96,116,141) there;
        // i10 a little greyer: i09 filmed them sat 0.36 for the clip's 0.31, and the darts are the blue thing)
        public static readonly int RibbonRow = CelPalette.Add("ice_ribbon",
            W(0.37f, 0.43f, 0.53f), W(0.45f, 0.53f, 0.645f), W(0.555f, 0.65f, 0.785f), W(0.655f, 0.765f, 0.905f),
            W(0.73f, 0.86f, 0.98f), 1.1f, 4, W(0.30f, 0.36f, 0.45f));
        // the bloom (its pixels solved for a colour over an opacity on three grounds, f22-f112): cloud (175,205,240),
        // lit (186,216,248), core (197,226,254); c1 is the faint halo it adds round itself (i14 a little less blue:
        // i13 filmed the hit hue 218, mean (181,204,243), for the clip's 212, (173,202,234))
        public static readonly int BloomRow = CelPalette.Add("ice_bloom",
            W(0.30f, 0.38f, 0.50f), W(0.08f, 0.12f, 0.2f), W(0.55f, 0.66f, 0.88f), W(0.595f, 0.715f, 0.915f),
            W(0.575f, 0.715f, 0.93f), 1.1f, 4, W(0.30f, 0.38f, 0.50f));
        // the starbursts, chips and specks: pale ice to white
        public static readonly int HitRow = CelPalette.Add("ice_hit",
            W(0.40f, 0.50f, 0.64f), W(0.48f, 0.57f, 0.72f), W(0.56f, 0.67f, 0.86f), W(0.62f, 0.75f, 0.96f),
            W(0.68f, 0.82f, 1f), 1.1f, 4, W(0.30f, 0.38f, 0.50f));
        // the frost mist
        public static readonly int FrostRow = CelPalette.Add("ice_frost",
            W(0.30f, 0.34f, 0.40f), W(0.384f, 0.44f, 0.512f), W(0.476f, 0.533f, 0.612f), W(0.586f, 0.659f, 0.789f),
            W(0.697f, 0.784f, 0.95f), 1.1f, 4, W(0.30f, 0.34f, 0.40f));

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
        /// the camera, landing up to LandSpread off the chest (i09: the clip's darts fly 65-80 px apart (p10-p90)
        /// all the way into the hit, i08's 30-50); each dart drags its own wake of mist on its lane, a dart behind it.</summary>
        // i12: shots every 2.2-3.2 frames to f118 (the clip shows 2+ darts in every frame f8-f123, i11 none f116-f123)
        // and a shot's darts GroupSpread apart (i11's 0.12 m merged them into one sliver on screen).
        // (i14: lanes 0.72 and groups 0.46: the clip's darts also ride the stream's upper and lower edges)
        // i16: shots every 1.4-2.0 frames, 1.9 darts a shot (i15 2.2-3.2, 1.75): the clip shows 5.1 darts a frame,
        // i15 2.6 (twice i15's from x 300 to x 800)
        const float FirstShot = 8f, LastShot = 118f, ShotGapMin = 1.4f, ShotGapMax = 2.0f, LaneSpread = 0.72f, LandSpread = 0.3f, GroupSpread = 0.46f;
        /// <summary>A wake's life (s), its width (m: 8-13 px soft strands near the camera), its wave's amplitude (m)
        /// and its opacity (i09: dimmer, the clip's strands top out at ~(110-160, 125-180, 150-210) where i08's
        /// overlapping ones burned to (200, 222, 252) round the darts).</summary>
        // i12: straighter (amp 0.09) and brighter (0.62): the clip's streaks top out at (147-166, 165-182, 189-211),
        // i11's at (107, 128, 160)
        // i13 0.55: with i12's denser volley the near-camera bundle filmed 1.6x the clip's px
        // i16 0.8 on a peaked profile: the clip's strands peak ~1.45x i15's over the ground (lum +90 for +62)
        // (i17 0.75 on the hard/soft profile: its plateau runs at full height where i16's peak sat at 0.8; i18 0.78
        // without the plateau: i17 filmed the clip's peak, +95 for +96)
        const float WakeLife = 0.62f, WakeWidth = 0.21f, WakeAmp = 0.09f, WakeOpacity = 0.78f;
        /// <summary>The gap (f66-f90): the bloom is gone and the darts glance off the board, so a share of the wakes
        /// runs on past the dummy to the upper right (the clip's f80 strands reach ~(1100, 250)); the darts themselves
        /// fly on down the corridor into the board (i12: i11's flew the glancing path and left the corridor empty) and
        /// the shards layer throws the pieces off.</summary>
        // (i13 0.45: i12's 0.8 of a denser volley streamed heavily over the dummy)
        const float GlanceFrom = 64f, GlanceTo = 88f, GlanceShare = 0.45f;
        /// <summary>From the gap on the streaks curl (the clip f72-f116: twisting, crossing, breaking; nearly straight
        /// f30-f60): a wake born after CurlFrom waves up to CurlAmp x as wide and drifts CurlDrift x as fast by CurlTo.</summary>
        const float CurlFrom = 58f, CurlTo = 78f, CurlAmp = 2.4f, CurlDrift = 2f;

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
                int n = g < 0.35f ? 1 : g < 0.75f ? 2 : 3;
                Vector3 centre = rng.InSphere() * LaneSpread;
                acc += keep;
                bool on = acc >= 1f;
                if (on) acc -= 1f;
                for (int d = 0; d < n; d++)
                {
                    float at = AtFrame(f + (d == 0 ? 0f : rng.Range(0.4f, 1.8f))) - 0.035f;
                    Vector3 lane = centre + (d == 0 ? Vector3.zero : (right * rng.Range(-1f, 1f) + up * rng.Range(-1f, 1f)) * GroupSpread);
                    float life = rng.Jitter(DartLife, 0.06f), width = rng.Jitter(DartWidth, 0.3f), dseed = rng.Next();
                    bool glance = f >= GlanceFrom && f <= GlanceTo && rng.Next() < GlanceShare;
                    Vector3 land = (right * rng.Range(-1f, 1f) + up * rng.Range(-1f, 1f)) * LandSpread;
                    Vector3 end = to + land, wEnd = glance ? to + right * rng.Range(3.0f, 4.2f) + up * rng.Range(2.2f, 3.0f) : end;
                    // (i16: 0.64 wakes a dart, i15 1.1, so the denser volley draws as many wakes a frame)
                    float wr = rng.Next();
                    int wakes = far ? 0 : mid ? (wr < 0.3f ? 1 : 0) : (wr < 0.4f ? 0 : wr < 0.96f ? 1 : 2);
                    float speed = (DartHeadEnd - DartHeadStart) / life;
                    if (on && darts) renderer.Spawn(kind, Dart(now + at, from + lane, end, life, width, dseed));
                    for (int w = 0; w < 2; w++)
                    {
                        float wAt = at + rng.Range(0f, 0.02f);
                        Vector3 wo = (right * rng.Range(-1f, 1f) + up * rng.Range(-1f, 1f)) * 0.06f;
                        float wLife = rng.Jitter(WakeLife, 0.25f), wWidth = rng.Jitter(WakeWidth, 0.4f), wAmp = rng.Jitter(WakeAmp, 0.4f);
                        float curl = Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(CurlFrom, CurlTo, f));
                        wAmp *= Mathf.Lerp(1f, CurlAmp, curl);
                        float wDrift = rng.Jitter(0.8f, 0.5f) * Mathf.Lerp(1f, CurlDrift, curl), wSeed = rng.Next();
                        if (on && !darts && w < wakes) renderer.Spawn(kind, Wake(now + wAt, from + lane + wo, wEnd, wLife, wWidth, wAmp, wDrift, speed, wSeed));
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

        /// <summary>One wake: a mist strand whose head flies a dart's length behind its dart, at its speed, and stops
        /// in the target (i09: i08's head ran 0.04 behind the dart's, so its brightest part lay over the dart); the
        /// strand stays where it was drawn, waves across the corridor and fades.</summary>
        static CelRecord Wake(float born, Vector3 p, Vector3 to, float life, float width, float amp, float drift, float speed, float seed) => new CelRecord
        {
            Pos = p, Born = born, Vel = (to - p) / life, Life = life, Axis = Vector3.up, Seed = seed,
            Size = new Vector4(width, width * 0.8f, 1f, Wavelength), Motion = new Vector4(0f, 0f, drift, 0f),
            Erode = new Vector4(0f, 0.5f, 1.5f, 0f),
            Tone = new Vector4(CelRecord.PackPalette(RibbonRow), 1f, WakeOpacity, 1f),
            Extra = new Vector4(IceBoltKind.Ribbon, amp, DartHeadStart - DartWindow, speed),
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
            // (i16 0.14: the clip's corridor glows between its strands; i17 0.1 again - the strands' soft sides
            // now glow past their edges)
            Sheets("ribbon sheets", 16, 5, AtFrame(9) - 0.04f, AtFrame(112) - AtFrame(9), 0.6f, 0.3f, 0.1f, 0.1f, 0.25f));

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
        static CelLayer Erode(CelLayer l, float end, float pow) { l.Erode = new CelCurve(0f, end, pow); return l; }
        /// <summary>Where the second bloom sits (the clip's is ~8 px left of the first) and the spray past the dummy's
        /// arm (the clip's ~75 px right of the hit), right/up/along the shot like HitOffset.</summary>
        static readonly Vector3 Hit2Offset = new Vector3(-0.21f, 0.04f, -0.08f), SprayOffset = new Vector3(0.84f, -0.08f, -0.08f);

        public static readonly CelFxRecipe Hit = new CelFxRecipe("ice_bolt_hit", 7f,
            // i03: the blooms fitted to the clip's opacity up and down from the hit (rings every 8 px, f16-f132) in a
            // model of the BLOOM fragment over the clip's own plate (checked against the i02 film): opaque at a core a
            // quarter of the quad, falling as a power to the edge; the first ~45 px at f18, ~72 by f26 that holds
            // to f42 and widens to ~100 as its centre thins (f62-f66), gone by ~f84; the second the same from f88,
            // fading f118-f132
            Exact(Erode(Dot("bloom", "ice_bloom", new Vector4(IceBoltKind.Bloom, 2.2f, 0.337f, 1f), AtFrame(16f), 0f, AtFrame(84.7f) - AtFrame(16f), 1, 1, new CelCurve(0.601f, 1.831f, 0.565f), 0.9f, 1), 1f, 4.09f)),
            At(Exact(Erode(Dot("bloom 2", "ice_bloom", new Vector4(IceBoltKind.Bloom, 2.0f, 0.275f, 1f), AtFrame(87.6f), 0f, AtFrame(134.3f) - AtFrame(87.6f), 1, 1, new CelCurve(0.688f, 2.186f, 0.543f), 0.9f, 1), 1f, 2.62f)), Hit2Offset),
            // its body boils: small denser lumps of cloud come and go at its rim
            Dim(Around(Dot("bloom clumps", "ice_bloom", new Vector4(IceBoltKind.Bloom, 1.6f, 0f, 0.5f), AtFrame(19f), AtFrame(66f) - AtFrame(19f), 0.14f, 30, 10, new CelCurve(0.35f, 0.48f), 0.95f), 0.5f), 0.5f),
            Dim(Around(Dot("bloom clumps 2", "ice_bloom", new Vector4(IceBoltKind.Bloom, 1.6f, 0f, 0.5f), AtFrame(91f), AtFrame(120f) - AtFrame(91f), 0.14f, 24, 8, new CelCurve(0.38f, 0.5f), 0.95f), 0.55f), 0.5f),
            // the frost the impact sprays on past the dummy's arm in the darts' direction (the clip f34-f84 and
            // f98-f128: 0.6-0.7 opacity ~75 px right of the hit, ~45 px across, glittering)
            Exact(At(Erode(Dot("spray", "ice_bloom", new Vector4(IceBoltKind.Bloom, 1.3f, 0f, 1f), AtFrame(32f), 0f, AtFrame(84f) - AtFrame(32f), 1, 1, new CelCurve(0.42f, 0.66f, 0.5f), 0.9f), 1f, 3f), SprayOffset)),
            Exact(At(Erode(Dot("spray 2", "ice_bloom", new Vector4(IceBoltKind.Bloom, 1.3f, 0f, 1f), AtFrame(98f), 0f, AtFrame(130f) - AtFrame(98f), 1, 1, new CelCurve(0.48f, 0.7f, 0.5f), 0.9f), 1f, 4f), SprayOffset)),
            // the two crisp starbursts that open the blooms (f16, f88): ~12 fat uneven spikes 60-70 px across, a white core
            Exact(Dot("star", "ice_hit", new Vector4(IceBoltKind.Star, 0.15f, 15f, 0.3f), AtFrame(15.6f), 0f, 0.1f, 1, 1, new CelCurve(0.48f, 0.56f, 0.5f), 1f, 1)),
            Exact(Erode(Dot("star 2", "ice_hit", new Vector4(IceBoltKind.Star, 0.14f, 16f, 0.2f), AtFrame(87f), 0f, 0.24f, 1, 1, new CelCurve(0.5f, 0.86f, 0.6f), 1f, 1), 1f, 3f)),
            // (i14: the rim splash of petal daisies f17-f26 is gone - the clip's forming cloud radiates frost streaks,
            // no flowers)
            new CelLayer
            {
                // white chips thrown out of the hit to ~100-150 px, some falling (f16-f140): i14 small slivers and flakes
                // (the clip's at 3x are 2-4 px wide, 5-10 long; i13's teardrops were twice that), a few more
                Name = "chips", Kind = IceBoltKind.KindName, Anchor = CelAnchor.Target, Offset = HitOffset, Start = AtFrame(16f), Duration = AtFrame(118f) - AtFrame(16f), TimeJitter = 0.03f, Life = 0.6f, LifeJitter = 0.4f,
                Close = 130, Mid = 42, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.45f, 0f, 0f), Aim = CelAim.Radial, Speed = 4.2f, SpeedJitter = 0.85f, UpBias = 0.25f, Gravity = 5f, Drag = 1.2f,
                Size = new CelCurve(0.022f, 0.016f), SizeJitter = 0.6f, Stretch = -0.001f, Erode = new CelCurve(0f, 0.4f, 2f), Feather = 1f,
                Palette = "ice_hit", Core = 1f, Opacity = 1f, Extra = new Vector4(IceBoltKind.Fleck, 2.9f, 0f, 0f),
            },
            new CelLayer
            {
                // i15: the gap's darts breaking on the board throw a shower of white chips round the dummy (the clip's
                // f72), faster and wider than the hold's
                Name = "chips gap", Kind = IceBoltKind.KindName, Anchor = CelAnchor.Target, Offset = HitOffset, Start = AtFrame(62f), Duration = AtFrame(92f) - AtFrame(62f), TimeJitter = 0.03f, Life = 0.5f, LifeJitter = 0.4f,
                Close = 50, Mid = 16, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.35f, 0f, 0f), Aim = CelAim.Radial, Speed = 6f, SpeedJitter = 0.7f, UpBias = 0.35f, Gravity = 5f, Drag = 1.1f,
                Size = new CelCurve(0.024f, 0.017f), SizeJitter = 0.6f, Stretch = -0.001f, Erode = new CelCurve(0f, 0.4f, 2f), Feather = 1f,
                Palette = "ice_hit", Core = 1f, Opacity = 1f, Extra = new Vector4(IceBoltKind.Fleck, 2.9f, 0f, 0f),
            },
            new CelLayer
            {
                // glitter: tiny white points that wink in and out over the cloud through the holds
                Name = "glitter", Kind = IceBoltKind.KindName, Anchor = CelAnchor.Target, Offset = HitOffset, Start = AtFrame(18f), Duration = AtFrame(72f) - AtFrame(18f), TimeJitter = 0.04f, Life = 0.18f, LifeJitter = 0.4f,
                Close = 50, Mid = 12, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.7f, 0f, 0f), Aim = CelAim.None, Speed = 0f,
                Size = new CelCurve(0.018f, 0.014f), SizeJitter = 0.5f, Stretch = -0.001f, Erode = new CelCurve(0f, 0.5f, 2f), Feather = 1.4f,
                Palette = "ice_hit", Core = 1f, Opacity = 1f, Extra = new Vector4(IceBoltKind.Speck, 0f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "glitter 2", Kind = IceBoltKind.KindName, Anchor = CelAnchor.Target, Offset = HitOffset, Start = AtFrame(92f), Duration = AtFrame(128f) - AtFrame(92f), TimeJitter = 0.04f, Life = 0.18f, LifeJitter = 0.4f,
                Close = 34, Mid = 8, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.75f, 0f, 0f), Aim = CelAim.None, Speed = 0f,
                Size = new CelCurve(0.018f, 0.014f), SizeJitter = 0.5f, Stretch = -0.001f, Erode = new CelCurve(0f, 0.5f, 2f), Feather = 1.4f,
                Palette = "ice_hit", Core = 1f, Opacity = 1f, Extra = new Vector4(IceBoltKind.Speck, 0f, 0f, 0f),
            },
            new CelLayer
            {
                // the gap (f64-f92): darts break on the board and glance off to the right and up-right as shards,
                // two-tone javelins 20-60 px long (the biggest ~90)
                Name = "shards", Kind = IceBoltKind.KindName, Anchor = CelAnchor.Target, Offset = HitOffset, Start = AtFrame(63f), Duration = AtFrame(92f) - AtFrame(63f), TimeJitter = 0.04f, Life = 0.42f, LifeJitter = 0.3f,
                // (i12: 28, i11 17 - the clip's f72 has ~10 in the air at once, i11's ~5)
                Close = 28, Mid = 9, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.3f, 0f, 0f), Aim = CelAim.Dir, Speed = 14f, SpeedJitter = 0.35f, Spread = 40f, UpBias = 0.5f, Gravity = 3f, Drag = 0.8f, Spin = 5f,
                Size = new CelCurve(0.115f, 0.095f), SizeJitter = 0.45f, Stretch = -0.001f, Erode = new CelCurve(0f, 0.3f, 2f), Feather = 0.8f,
                Palette = "ice_dart", Core = 1f, Opacity = 1f, Extra = new Vector4(IceBoltKind.Shard, 3.6f, 0f, 0f),
            },
            new CelLayer
            {
                // a few more as the second bloom fades (f116-f136)
                Name = "shards 2", Kind = IceBoltKind.KindName, Anchor = CelAnchor.Target, Offset = HitOffset, Start = AtFrame(116f), Duration = AtFrame(136f) - AtFrame(116f), TimeJitter = 0.04f, Life = 0.4f, LifeJitter = 0.3f,
                Close = 10, Mid = 4, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.3f, 0f, 0f), Aim = CelAim.Dir, Speed = 12f, SpeedJitter = 0.35f, Spread = 55f, UpBias = 0.5f, Gravity = 3f, Drag = 0.8f, Spin = 5f,
                Size = new CelCurve(0.08f, 0.065f), SizeJitter = 0.5f, Stretch = -0.001f, Erode = new CelCurve(0f, 0.3f, 2f), Feather = 0.8f,
                Palette = "ice_dart", Core = 1f, Opacity = 1f, Extra = new Vector4(IceBoltKind.Shard, 3.2f, 0f, 0f),
            },
            // frost mist left of the dummy as the second bloom holds (f104-f132) and right of it as it fades (f120-f152)
            At(Erode(Dot("frost left", "ice_frost", new Vector4(IceBoltKind.Frost, 3.6f, 1.4f, 0f), AtFrame(90f), 0.08f, 1.85f, 2, 1, new CelCurve(0.5f, 0.62f, 0.5f), 0.5f), 0.9f, 1.8f), new Vector3(-1.29f, 0.75f, -0.2f)),
            At(Erode(Dot("frost right", "ice_frost", new Vector4(IceBoltKind.Frost, 3.6f, 1.4f, 0f), AtFrame(114f), 0.1f, 1.4f, 2, 1, new CelCurve(0.55f, 0.68f, 0.5f), 0.5f), 0.9f, 2.2f), new Vector3(1.0f, 0.05f, 0f)),
            // (i11: the frost rim is gone: its clumps fell on the chest inside the second bloom, a grey speckle there
            // f98-f112 where the clip's bloom is clean white)
            At(Erode(Dot("frost right low", "ice_frost", new Vector4(IceBoltKind.Frost, 3.6f, 1.3f, 0f), AtFrame(126f), 0.1f, 1.35f, 1, 1, new CelCurve(0.42f, 0.5f, 0.5f), 0.5f), 0.9f, 2.5f), new Vector3(1.22f, -0.42f, 0f)));

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
