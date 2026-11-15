// Phase: CelFx red_lasers (2026-11-15, lane/show/cel-red-lasers) - the rapid-fire red laser bolts of the owner's clip
// red_lasers_nolora_00001_ (H3 round 6, 8 s at 24 fps; what it is: celfx spec\eye\red_lasers.md). Two barrels
// off-screen at the eye's lower left fire long thin red laser bolts into the dummy's board, each about every 3.2
// frames for 3.3 s (15 a second in all); the dummy is lit red through the volley with a flicker, a white-hot starburst
// sits on the bullseye and red sparks fly out of it; from frame 61 some shots go wide right past the dummy and off
// into the distance, one burning the ground; two ground hits right of the dummy glow red and send up soft grey smoke
// that lingers to frame 136.
// Built from electric_bolt (its Play: every record spawned at once on the clip's clock, the light found from the
// scene's depth, the kit Streak sparks) and blue_laser (the smoke puff). The numbers were read off the clip: the two
// lanes and the misses from 3D fits through the beach Range's camera (0.5 px and 6-10 px rms), the launch frames from
// a streak tracker on each lane, the sizes from the camera's metres per pixel (72 px/m at the dummy, 120-150 near the
// eye), the colours from its pixels taken back through the Range's night grade (celfx runs\red_lasers\_ref\scripts:
// fit_lane.py, schedule.py, fit_miss.py, colours.py).
// LAB ONLY: no weapon fires it yet; the Range calls Play. Energy weapon, so red light is allowed.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class RedLasersRecipe
    {
        // ---------------------------------------------------------------- the clip's clock
        /// <summary>The shot time the effect is filmed at (tools/t0_expected.json: frame 12) and the seconds after Play
        /// at which a frame of the clip falls.</summary>
        public const float T0 = 0.5f;
        static float At(float frame) => frame / 24f - T0;

        // ---------------------------------------------------------------- the bolts
        /// <summary>m/s (the lanes' fit: 43 m/s, the 12.9 m to the board in 7.1 frames), a bolt's length (m: 6-7 m in
        /// flight, about half the corridor) and its body's full width (m: the clip's red body is 10-12 px wide near the
        /// eye and 6 by the dummy).</summary>
        public const float Speed = 43.3f, BoltLength = 6.0f, BoltWidth = 0.085f;
        /// <summary>The launch frames of the two barrels (a streak tracker on each lane; the few it lost where two bolts
        /// merged are put in their gaps). The last bolts land on frame 100 and their tails are in by 103.</summary>
        public static readonly float[] LaunchA = { 13.8f, 17.5f, 20.5f, 24f, 26.9f, 30.7f, 33.8f, 37.2f, 40.1f, 43.5f, 46.1f, 49.1f, 52.8f, 55.5f, 57.6f, 60.5f, 62.8f, 64.6f, 68.7f, 71.5f, 74.7f, 78.2f, 81.7f, 84f, 87.1f, 90f, 93.1f };
        public static readonly float[] LaunchB = { 17.1f, 20.6f, 24.6f, 27.6f, 31f, 34.1f, 36.8f, 39.6f, 42.7f, 44.5f, 47.7f, 50f, 52.2f, 56.3f, 59.1f, 62f, 64.6f, 67.7f, 71.3f, 74f, 77.1f, 81f, 83.8f, 87.1f, 90f, 93.3f };

        // ---------------------------------------------------------------- where things are, in the shot's frame
        // (metres from the target's chest: along the level shot from the muzzle, to its side (the eye's right), up). From
        // the clip's screen positions through the Range camera (celfx runs\red_lasers\_ref\scripts).
        /// <summary>Lane A: from the muzzle (raised 0.06 m) to 0.18 m over the chest - enters the frame at (0, 759), lands
        /// at (834, 387). Lane B: from the muzzle raised 0.62 m to 0.46 m over the chest - (0, 688) to (834, 368).</summary>
        static readonly Vector3 FromA = new Vector3(-12.885f, 0f, 0.116f), ToA = new Vector3(0f, 0f, 0.18f);
        static readonly Vector3 FromB = new Vector3(-12.885f, 0f, 0.676f), ToB = new Vector3(0f, 0f, 0.46f);
        /// <summary>How far a bolt's ends wander (half-ranges, m: to the side, up) where it leaves and where it lands (the
        /// clip's lane A is a tight line, lane B a little looser: p10-p90 of 6 and 22 px near the eye, 8 and 19 by the
        /// dummy).</summary>
        static readonly Vector2 StartSpreadA = new Vector2(0.03f, 0.025f), EndSpreadA = new Vector2(0.04f, 0.055f);
        static readonly Vector2 StartSpreadB = new Vector2(0.08f, 0.09f), EndSpreadB = new Vector2(0.1f, 0.13f);
        static readonly Vector3 Eye = new Vector3(-15.846f, 9.042f, 5.399f);          // the close camera
        /// <summary>The hit's white-hot middle (827, 388), a little in front of the board so the board never hides it.</summary>
        static readonly Vector3 Blob = new Vector3(-0.308f, 0.055f, 0.24f);
        /// <summary>The two ground hits right of the dummy: G1 (908, 470) about a metre right of its foot, G2 (1055, 442)
        /// further right and back (put on the Range's own ground along the camera's ray through them).</summary>
        static readonly Vector3 G1 = new Vector3(1.138f, 0.626f, -1.244f), G2 = new Vector3(3.673f, 1.853f, -1.244f);

        /// <summary>The misses: launch frame, where it leaves (frame), which way it flies (frame), how far (m; 0: to G2).
        /// From fits of the tracked heads at the volley's speed (6-10 px rms): shots from a little nearer the eye than the
        /// barrels, nearly along the main shot, that pass in front of and right of the dummy and go off into the distance
        /// (lane D enters the frame's bottom at x ~600, lane C at x ~225).</summary>
        static readonly Vector4[] Misses =
        {
            new Vector4(61.0f, -13.797f, 3.702f, 0.157f), new Vector4(0.9991f, -0.0404f, 0.0091f, 25f),   // D1 (f64-f74)
            new Vector4(66.6f, -14.472f, 3.284f, 1.297f), new Vector4(0.9945f, -0.0929f, -0.0481f, 24f),  // C1
            new Vector4(70.9f, -13.686f, 5.6f, 1.982f), new Vector4(0.9751f, -0.1756f, -0.1355f, 0f),     // D2: lands on G2 (f79-80)
            new Vector4(74.6f, -14.472f, 3.284f, 1.297f), new Vector4(0.9935f, -0.1029f, -0.0481f, 26f),  // C2 (f76-f89)
            new Vector4(79.6f, -14.3f, 3.35f, 1.25f), new Vector4(0.9938f, -0.1f, -0.05f, 25f),            // C3
            new Vector4(85.8f, -14.4f, 3.2f, 1.32f), new Vector4(0.993f, -0.108f, -0.047f, 26f),           // C4 (f87-f97)
        };

        /// <summary>The frames the dummy is lit full red (the clip's wash: lit on most frames, dim for one frame every 3-5
        /// frames between hits); a dim red glow holds under it from 21 to 105.</summary>
        static readonly Vector2[] LitRuns =
        {
            new Vector2(21, 22), new Vector2(24, 27), new Vector2(29, 31), new Vector2(33, 35), new Vector2(37, 39), new Vector2(41, 43),
            new Vector2(45, 51), new Vector2(53, 55), new Vector2(57, 59), new Vector2(61, 67), new Vector2(69, 71), new Vector2(73, 75),
            new Vector2(77, 79), new Vector2(81, 83), new Vector2(85, 87), new Vector2(89, 91), new Vector2(93, 94), new Vector2(96, 99),
            new Vector2(103, 105),
        };
        /// <summary>The ground hits' burning windows (frames): G1 45-62 and 83-89, G2 80-89 and 96-99.</summary>
        static readonly Vector2[] BurnG1 = { new Vector2(45, 62), new Vector2(83, 89) };
        static readonly Vector2[] BurnG2 = { new Vector2(80, 89), new Vector2(96, 99) };

        // ---------------------------------------------------------------- palettes
        // Colours as the framebuffer has to hold them for the Range's night grade to show the clip's (the clip's colours
        // taken back through the grade fitted to the cyan_beam e00cal swatch film, ice_bolt's grade_model.json); the
        // project is gamma and the palette stores .linear, so S() hands over a stored value as the colour whose .linear
        // it is. Opaque colours are as written at the middle of the frame (the shader's Unvignette undoes the lens
        // elsewhere); added light is what has to go on top of the clip's plate there.
        static Color S(float r, float g, float b) => new Color(Mathf.LinearToGammaSpace(r), Mathf.LinearToGammaSpace(g), Mathf.LinearToGammaSpace(b), 1f);

        /// <summary>The bolts: the centre (255,229,219), the core's edge (252,194,182), the inner band (224,119,121) and
        /// the outer band (193,64,74) to the body's edge; the light added round it: c1 the halo, c0 the glow (red light
        /// added over the plate of (21,23,32): +152 at 1 px out of the body, +52 at 10, +20 at 30).</summary>
        public static readonly int BoltRow = CelPalette.Add("red_lasers_bolt",
            S(0.218f, 0.05f, 0.035f), S(0.326f, 0.075f, 0.055f), S(0.622f, 0.259f, 0.262f), S(0.745f, 0.426f, 0.4f), S(0.947f, 0.813f, 0.73f), 1.1f, 4, S(0.914f, 0.663f, 0.587f));
        /// <summary>The hit's starburst and its pink bloom, added over the lit board ((232,105,99) as written 0.78, 0.39,
        /// 0.34): the white-hot core (252,242,227) needs mostly green and blue on top of it.</summary>
        public static readonly int HitRow = CelPalette.Add("red_lasers_hit",
            S(0.1f, 0.03f, 0.03f), S(0.15f, 0.05f, 0.05f), S(0.26f, 0.09f, 0.08f), S(0.42f, 0.36f, 0.3f), S(0.5f, 0.62f, 0.5f), 1.1f, 4, S(0f, 0f, 0f));
        /// <summary>The ground hits' flash, added over the ground's (28,32,43): white-hot (248,244,246) in the middle, red
        /// (203,89,89) round it.</summary>
        public static readonly int FlashRow = CelPalette.Add("red_lasers_flash",
            S(0.1f, 0.03f, 0.03f), S(0.2f, 0.06f, 0.05f), S(0.55f, 0.2f, 0.15f), S(0.25f, 0.3f, 0.28f), S(0.78f, 0.83f, 0.76f), 1.1f, 4, S(0f, 0f, 0f));
        /// <summary>The sparks (kit Streaks: additive bands; they fly by the dummy, near the middle): pink-red (183,117,133)
        /// with brighter centres (211,148,163) over the ground's (30,35,45).</summary>
        public static readonly int SparkRow = CelPalette.Add("red_lasers_spark",
            S(0.22f, 0.07f, 0.08f), S(0.36f, 0.17f, 0.18f), S(0.49f, 0.3f, 0.32f), S(0.58f, 0.4f, 0.42f), S(0.66f, 0.5f, 0.52f), 0.8f, 3, S(0.05f, 0.01f, 0.01f));
        /// <summary>The smoke: its shadow (65,64,76) and its light (93-99,93-99,105-111); the median of the clip's smoke
        /// is (85,84,95).</summary>
        public static readonly int SmokeRow = CelPalette.Add("red_lasers_smoke",
            S(0.17f, 0.17f, 0.2f), S(0.19f, 0.2f, 0.23f), S(0.215f, 0.222f, 0.253f), S(0.30f, 0.31f, 0.335f), S(0.33f, 0.34f, 0.36f), 1.1f, 4, S(0f, 0f, 0f));

        // ---------------------------------------------------------------- the light
        /// <summary>The dummy lit red (multiplied into what it falls on: lit = unlit x (1 + Into) at its heart, the
        /// dummy keeps its own pattern). The clip's dummy over its own colours (88,80,68) as written: x2.67, 1.40, 1.45 on a
        /// lit frame, x1.27, 0.99, 1.05 on a dim one; most of the dummy sits at 0.6-0.8 of the light's heart.</summary>
        static readonly Vector3 LitInto = new Vector3(1.9f, 0.55f, 0.55f), DimInto = new Vector3(0.4f, 0f, 0.07f);
        static readonly Vector3 LitAdd = new Vector3(0.02f, 0.004f, 0.004f);
        /// <summary>A ground hit's pool on the ground (added: the clip's red light falls from (0.7, 0.37, 0.29) 0.2 m out to
        /// (0.12, 0.04, 0.03) 1.25 m out, as written): a wide light and a tight one, a little multiplied in.</summary>
        static readonly Vector3 PoolAdd = new Vector3(0.55f, 0.175f, 0.12f), PoolCoreAdd = new Vector3(0.21f, 0.1f, 0.08f), PoolInto = new Vector3(1.2f, 0.4f, 0.25f);

        static readonly int LensId = Shader.PropertyToID("_TWRedLaserLens");

        static RedLasersRecipe() { SceneStatics.Register(nameof(RedLasersRecipe), () => Shader.SetGlobalVector(LensId, Vector4.zero)); }

        /// <summary>The scene's lens vignette (URP Vignette's intensity and smoothness; 0 for none), for whoever shows the
        /// effect to hand over: the bolts are written brighter where the vignette will darken them.</summary>
        public static void Lens(float intensity, float smoothness) =>
            Shader.SetGlobalVector(LensId, new Vector4(Mathf.Max(0f, intensity) * 3f, Mathf.Max(0.01f, smoothness) * 5f, 0f, 0f));

        // ---------------------------------------------------------------- the sparks (kit Streaks, through CelFx.Play)
        /// <summary>The hit's sparks: small pink-red dots and short dashes 2-3 px thick and 5-8 px long thrown out of the
        /// starburst to 150-220 px, some falling below the board (the clip's 9-30 pieces a frame, f22-f104).</summary>
        public static readonly CelFxRecipe Sparks = new CelFxRecipe("red_lasers_sparks", 5f,
            new CelLayer
            {
                Name = "sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = At(21.5f), Duration = At(104f) - At(21.5f), TimeJitter = 0.02f, Life = 0.42f, LifeJitter = 0.4f,
                Close = 230, Mid = 80, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.12f, 0f, 0f), Aim = CelAim.Radial, Speed = 4.2f, SpeedJitter = 0.6f, UpBias = 0.15f, Gravity = 3.5f, Drag = 2f,
                Size = new CelCurve(0.034f, 0.022f), SizeJitter = 0.35f, Stretch = 0.022f, Erode = new CelCurve(0.05f, 0.5f), Palette = "red_lasers_spark", Core = 1f, Extra = new Vector4(0.35f, 0.04f, 0.35f, 0f),
            });

        /// <summary>A ground hit's sparks: thin red sparks fountaining up out of it.</summary>
        static CelFxRecipe Fountain(string name, float from, float to, int close)
        {
            return new CelFxRecipe(name, 5f,
                new CelLayer
                {
                    Name = "fountain", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = At(from), Duration = At(to) - At(from), TimeJitter = 0.02f, Life = 0.3f, LifeJitter = 0.4f,
                    Close = close, Mid = close / 3, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.08f, 0f, 0f), Aim = CelAim.Radial, Speed = 3.6f, SpeedJitter = 0.5f, UpBias = 1.6f, Gravity = 6f, Drag = 1.4f,
                    Size = new CelCurve(0.022f, 0.012f), SizeJitter = 0.3f, Stretch = 0.045f, Erode = new CelCurve(0.05f, 0.5f), Palette = "red_lasers_spark", Core = 1.2f, Extra = new Vector4(0.1f, 0.05f, 0.3f, 0f),
                });
        }
        static readonly CelFxRecipe FountainG1a = Fountain("red_lasers_fountain_g1a", 45.5f, 62f, 34);
        static readonly CelFxRecipe FountainG1b = Fountain("red_lasers_fountain_g1b", 83.5f, 89f, 14);
        static readonly CelFxRecipe FountainG2a = Fountain("red_lasers_fountain_g2a", 80f, 89f, 24);
        static readonly CelFxRecipe FountainG2b = Fountain("red_lasers_fountain_g2b", 96f, 99f, 10);

        // ---------------------------------------------------------------- playing it
        /// <summary>
        /// Fire the volley from the barrels at `muzzle` into the target's chest `target` (whose foot is on the ground at
        /// `ground`). Everything is spawned now on the clip's clock (the light it throws too: the shader finds what it falls
        /// on in the camera's depth texture), nothing is left to do per frame. No allocation.
        /// </summary>
        public static void Play(Vector3 muzzle, Vector3 target, Vector3 ground, uint seed)
        {
            var renderer = CelFx.Renderer;
            int kind = renderer.KindId(RedLasersKind.KindName);
            if (kind < 0) throw new System.InvalidOperationException("RedLasers kind is not registered");
            float lod = CelFx.LodBlend(CelFx.Zoom);
            bool mid = lod >= 0.5f, far = lod >= 1.5f;
            float quality = CelFx.QualityShare(FxQuality.Tier);
            float now = CelFx.Now;
            var rng = new CelRng(seed);
            Vector3 along = target - muzzle;
            along.y = 0f;
            along = along.sqrMagnitude > 1e-6f ? along.normalized : Vector3.forward;
            Vector3 side = new Vector3(along.z, 0f, -along.x);
            Vector3 eye = Place(target, along, side, Eye);
            Vector3 g1 = OnGround(eye, Place(target, along, side, G1) + Vector3.up * 0.3f, ground.y);
            Vector3 g2 = OnGround(eye, Place(target, along, side, G2) + Vector3.up * 0.3f, ground.y);
            Vector3 blob = Place(target, along, side, Blob);

            // ---- the dummy lit red (under everything): a dim glow from the first hit to the last, the full red on the lit
            // runs (the flicker), and the ground round the foot with it
            if (!far)
            {
                Vector3 heart = target + (eye - target).normalized * 0.12f + Vector3.up * 0.1f;
                Light(renderer, kind, now, At(20.6f), At(105.5f), heart, side, 1.6f, 2.6f, 1f, 0.75f, 1f, DimInto, Vector3.zero, 0.02f, 0.06f, false);
                for (int i = 0; i < LitRuns.Length; i++)
                    Light(renderer, kind, now, At(LitRuns[i].x - 0.5f), At(LitRuns[i].y + 0.5f), heart, side, 1.6f, 2.6f, 1f, 0.75f, 1f, LitInto, LitAdd, 0.008f, 0.012f, false);
            }

            // ---- the two barrels: a bolt every ~3.2 frames on each lane from frame 13.8 to 93.3
            float share0 = far ? 0.35f : mid ? 0.7f : 1f;
            Barrel(renderer, kind, now, LaunchA, FromA, ToA, StartSpreadA, EndSpreadA, target, along, side, share0, quality, ref rng);
            Barrel(renderer, kind, now, LaunchB, FromB, ToB, StartSpreadB, EndSpreadB, target, along, side, share0, quality, ref rng);

            // ---- the misses: wide right past the dummy into the distance (one burns the ground at G2)
            if (!far)
            {
                for (int i = 0; i + 1 < Misses.Length; i += 2)
                {
                    Vector4 a = Misses[i], b = Misses[i + 1];
                    Vector3 start = Place(target, along, side, new Vector3(a.y, a.z, a.w));
                    Vector3 dir = (along * b.x + side * b.y + Vector3.up * b.z).normalized;
                    Vector3 end = b.w > 0f ? start + dir * b.w : g2;
                    Bolt(renderer, kind, now, a.x, start, end, b.w > 0f, ref rng);
                }
            }

            // ---- the hit: a white-hot starburst redrawn every frame from the first landing (21) to the last (105), big
            // on the lit frames and small on the dim ones, with a pink bloom round it
            {
                int lit = 0;
                for (int f = 21; f <= 105; f++)
                {
                    bool on = IsLit(f, ref lit);
                    float size = on ? rng.Range(0.3f, 0.4f) : rng.Range(0.2f, 0.27f);
                    Vector3 at = blob + (side * rng.Range(-0.04f, 0.04f) + Vector3.up * rng.Range(-0.04f, 0.04f));
                    Dot(renderer, kind, now, At(f - 0.45f), 0.9f / 24f, at, HitRow, RedLasersKind.Rays, new Vector3(size, size, 1f),
                        rng.Range(0.06f, 0.09f), Mathf.Round(rng.Range(9f, 14f)), on ? rng.Range(0.42f, 0.52f) : 0.42f, 1f, ref rng);
                    if (!far)
                        Dot(renderer, kind, now, At(f - 0.45f), 0.9f / 24f, blob, HitRow, RedLasersKind.Bloom, new Vector3(size * 1.7f, size * 1.7f, 1f),
                            3.5f, 0f, 0f, on ? 0.7f : 1f, ref rng);
                }
            }

            // ---- the ground hits: a flickering white-hot flash with a red pool on the ground round it, and its smoke
            if (!far)
            {
                Burn(renderer, kind, now, g1, BurnG1, ref rng);
                Burn(renderer, kind, now, g2, BurnG2, ref rng);
                // the smoke: S1 over G1 from frame 45 (gone by 126), S2 over G2 from frame 80 (gone by 136)
                Smoke(renderer, kind, now, g1, along, side, 45.5f, 88f, Count(22, mid ? 0.5f : 1f, quality), 2.2f, 1.2f, 1.6f, ref rng);
                Smoke(renderer, kind, now, g2, along, side, 80.5f, 100f, Count(13, mid ? 0.5f : 1f, quality), 1.8f, 1.1f, 1.5f, ref rng);
            }

            // ---- the sparks (kit Streaks): the hit's, the ground hits' fountains
            Vector3 dirFwd = target - muzzle;
            CelFx.Play(Sparks, muzzle, dirFwd, blob, seed + 1u);
            if (!far)
            {
                CelFx.Play(FountainG1a, muzzle, dirFwd, g1 + Vector3.up * 0.05f, seed + 2u);
                CelFx.Play(FountainG1b, muzzle, dirFwd, g1 + Vector3.up * 0.05f, seed + 3u);
                CelFx.Play(FountainG2a, muzzle, dirFwd, g2 + Vector3.up * 0.05f, seed + 4u);
                CelFx.Play(FountainG2b, muzzle, dirFwd, g2 + Vector3.up * 0.05f, seed + 5u);
            }
        }

        /// <summary>Is frame f one of the lit runs (walks `cursor` forward; frames asked in order).</summary>
        static bool IsLit(int f, ref int cursor)
        {
            while (cursor < LitRuns.Length && f > LitRuns[cursor].y) cursor++;
            return cursor < LitRuns.Length && f >= LitRuns[cursor].x;
        }

        /// <summary>One barrel's bolts: each launched on its clip frame from `from` (frame, jittered) to `to` (frame, jittered).</summary>
        static void Barrel(CelFxRenderer renderer, int kind, float now, float[] launches, Vector3 from, Vector3 to, Vector2 startSpread, Vector2 endSpread,
            Vector3 target, Vector3 along, Vector3 side, float share, float quality, ref CelRng rng)
        {
            int n = Count(launches.Length, share, quality);
            for (int i = 0; i < n; i++)
            {
                int j = n >= launches.Length ? i : Mathf.Min(launches.Length - 1, Mathf.RoundToInt(i * (launches.Length - 1) / (float)Mathf.Max(1, n - 1)));
                Vector3 start = Place(target, along, side, from) + side * rng.Range(-startSpread.x, startSpread.x) + Vector3.up * rng.Range(-startSpread.y, startSpread.y);
                Vector3 end = Place(target, along, side, to) + side * rng.Range(-endSpread.x, endSpread.x) + Vector3.up * rng.Range(-endSpread.y, endSpread.y);
                Bolt(renderer, kind, now, launches[j] + rng.Range(-0.15f, 0.15f), start, end, false, ref rng);
            }
        }

        /// <summary>A laser bolt launched on clip frame `launch` from `start` to `end`: its head runs at Speed from the start
        /// (share 0) on past the end until its tail has gone in; a miss (`fade`) thins out as it goes off into the distance.</summary>
        static void Bolt(CelFxRenderer renderer, int kind, float now, float launch, Vector3 start, Vector3 end, bool fade, ref CelRng rng)
        {
            float span = Mathf.Max((end - start).magnitude, 0.1f);
            float share = rng.Jitter(BoltLength, 0.12f) / span;
            float life = span / Speed * (1f + share);
            var r = Rec(now, At(launch), life, start, BoltRow, 1f, ref rng);
            r.Vel = (end - start) / life;          // the span is vel x life; the head runs at Speed
            float w = rng.Jitter(BoltWidth, 0.08f);
            r.Size = new Vector4(w, w, 1f, share);
            r.Motion = new Vector4(0f, 0f, 0f, 0.35f);   // the tail's taper
            r.Erode = fade ? new Vector4(0f, 1f, 3f, 0f) : new Vector4(0f, 0f, 1f, 0f);
            r.Tone.y = 1f; r.Tone.w = 1f;          // the bolt stops at its end
            r.Extra = new Vector4(1f, 0.06f, 0f, 1f + share);
            renderer.Spawn(kind, r);
        }

        /// <summary>A ground hit: in each window a white-hot flash redrawn every frame (uneven, it pulses) with a small ray
        /// star, and the red pool it throws on the ground (a wide light and a tight one).</summary>
        static void Burn(CelFxRenderer renderer, int kind, float now, Vector3 at, Vector2[] windows, ref CelRng rng)
        {
            Vector3 p = at + Vector3.up * 0.1f;
            for (int w = 0; w < windows.Length; w++)
            {
                Vector2 win = windows[w];
                Vector3 lp = at + Vector3.up * 0.6f;
                Light(renderer, kind, now, At(win.x - 0.5f), At(win.y + 0.5f), lp, Vector3.right, 1.6f, 1f, 1f, 0.8f, 1f, PoolInto, PoolAdd, 0.03f, 0.1f, true);
                Light(renderer, kind, now, At(win.x - 0.5f), At(win.y + 0.5f), lp, Vector3.right, 0.7f, 1f, 1f, 0.8f, 1f, Vector3.zero, PoolCoreAdd, 0.02f, 0.06f, true);
                for (int f = (int)win.x; f <= (int)win.y; f++)
                {
                    float k = rng.Range(0.35f, 1f);
                    float s = Mathf.Lerp(0.14f, 0.3f, k);
                    Dot(renderer, kind, now, At(f - 0.45f), 0.9f / 24f, p + rng.InSphere() * 0.03f, FlashRow, RedLasersKind.Bloom, new Vector3(s, s, 1f), 2.5f, 0f, 0.45f, 1f, ref rng);
                    if (k > 0.6f)
                        Dot(renderer, kind, now, At(f - 0.45f), 0.9f / 24f, p, FlashRow, RedLasersKind.Rays, new Vector3(s * 1.4f, s * 1.4f, 1f), 0.06f, 7f, 0.3f, 0.8f, ref rng);
                }
            }
        }

        /// <summary>A column of soft grey smoke over a ground hit: `n` puffs born from frame `from` to `to`, each rising
        /// (it reaches about `rise` m over the ground), swelling and fading over `lifeLo`..`lifeHi` s.</summary>
        static void Smoke(CelFxRenderer renderer, int kind, float now, Vector3 at, Vector3 along, Vector3 side, float from, float to, int n,
            float rise, float lifeLo, float lifeHi, ref CelRng rng)
        {
            for (int i = 0; i < n; i++)
            {
                float t = Mathf.Lerp(from, to, n > 1 ? i / (float)(n - 1) : 0f) + rng.Range(-0.4f, 0.4f);
                float life = rng.Range(lifeLo, lifeHi);
                Vector3 p = at + Vector3.up * 0.25f + side * rng.Range(-0.15f, 0.15f) + along * rng.Range(-0.1f, 0.1f);
                var r = Rec(now, At(t), life, p, SmokeRow, 0.95f, ref rng);
                // up at about rise m in its life (drag 1: v x 0.78 + buoyancy), a sway to the side, a drift away
                r.Vel = Vector3.up * rng.Jitter(rise * 0.95f, 0.15f) + side * rng.Range(-0.3f, 0.3f) + along * rng.Range(0f, 0.25f);
                r.Motion = new Vector4(0f, 1f, 0f, 0.3f);
                float s0 = rng.Range(0.2f, 0.27f);
                r.Size = new Vector4(s0, s0 * rng.Range(1.45f, 1.75f), 0.55f, -0.001f);
                r.Erode = new Vector4(0f, 1f, 2.2f, 0f);
                r.Extra = new Vector4(RedLasersKind.Puff, 0.42f, 0.16f, 0f);
                renderer.Spawn(kind, r);
            }
        }

        /// <summary>A layer's count at a LOD share and the quality's (never more going out; at least one when the share
        /// is above zero).</summary>
        static int Count(int close, float share, float quality) => share <= 0f ? 0 : Mathf.Max(1, Mathf.FloorToInt(close * share * Mathf.Max(0f, quality) + 0.5f));

        /// <summary>A point given in the shot's frame (along, side, up from the target's chest).</summary>
        static Vector3 Place(Vector3 target, Vector3 along, Vector3 side, Vector3 at) => target + along * at.x + side * at.y + Vector3.up * at.z;

        /// <summary>A soft dot that stays put (a bloom, a ray star).</summary>
        static void Dot(CelFxRenderer renderer, int kind, float now, float at, float life, Vector3 pos, int palette, float mode, Vector3 size,
            float y, float z, float w, float opacity, ref CelRng rng, float hdr = 1f)
        {
            var r = Rec(now, at, life, pos, palette, opacity, ref rng);
            r.Tone.y = hdr;
            r.Size = new Vector4(size.x, size.y, size.z > 0f ? size.z : 1f, -0.001f);
            r.Extra = new Vector4(mode, y, z, w);
            r.Erode = new Vector4(0f, 0f, 1f, 0f);
            renderer.Spawn(kind, r);
        }

        /// <summary>A light on what stands behind its card: `alongReach` m along `axis`, `up` m up and down, `farShare` of
        /// the near reach on the far side, `nearShare` x along square to the axis; Into is multiplied in at its heart, Add
        /// added on top; up over `rise` s, down over `fall` s at the end.</summary>
        static void Light(CelFxRenderer renderer, int kind, float now, float born, float gone, Vector3 at, Vector3 axis,
            float alongReach, float up, float farShare, float nearShare, float strength, Vector3 into, Vector3 add, float rise, float fall, bool level)
        {
            var own = new CelRng(0x5EEDu);   // a light has nothing random in it: the effect's own draws stay as they are
            var r = Rec(now, born, gone - born, at, 0, strength, ref own);
            r.Axis = axis;
            r.Vel = into;
            r.Size = new Vector4(alongReach, up, farShare, -1f);
            r.Motion = new Vector4(nearShare, 0f, 0f, 0f);
            r.Erode = new Vector4(add.x, add.y, add.z, 0f);
            r.Extra = new Vector4(RedLasersKind.Light, rise, fall, 0f);
            r.Tone.y = level ? 1f : 0f;
            renderer.Spawn(kind, r);
        }

        static CelRecord Rec(float now, float at, float life, Vector3 pos, int palette, float opacity, ref CelRng rng)
        {
            return new CelRecord
            {
                Pos = pos, Born = now + Mathf.Max(0f, at), Life = Mathf.Max(0.02f, life), Axis = Vector3.right, Seed = rng.Next(),
                Size = new Vector4(1f, 1f, 1f, -0.001f), Erode = new Vector4(0f, 0f, 1f, 0f),
                Tone = new Vector4(CelRecord.PackPalette(palette), 1f, opacity, 0f),
            };
        }

        /// <summary>The ground under a point (the Range's height map; `fallback` when there is none).</summary>
        static float GroundAt(Vector3 p, float fallback) => RenderGround.Map != null ? RenderGround.Sample(RenderGround.Map, p.x, p.z) : fallback;

        /// <summary>Where the ray from `eye` through `guess` meets the ground (so a point given by the clip's screen
        /// position lands on the Range's own ground there); the guess on the ground under it when it never does.</summary>
        static Vector3 OnGround(Vector3 eye, Vector3 guess, float fallbackY)
        {
            if (RenderGround.Map == null) { guess.y = fallbackY; return guess; }
            Vector3 d = guess - eye;
            float len = d.magnitude;
            if (len < 0.01f) return guess;
            d /= len;
            float lo = len * 0.5f, hi = len * 2f;
            Vector3 pl = eye + d * lo, ph = eye + d * hi;
            float al = pl.y - GroundAt(pl, fallbackY), ah = ph.y - GroundAt(ph, fallbackY);
            if (al < 0f || ah > 0f) { guess.y = GroundAt(guess, fallbackY); return guess; }
            for (int i = 0; i < 28; i++)
            {
                float m = (lo + hi) * 0.5f;
                Vector3 pm = eye + d * m;
                if (pm.y - GroundAt(pm, fallbackY) > 0f) lo = m; else hi = m;
            }
            return eye + d * ((lo + hi) * 0.5f);
        }

        // ---------------------------------------------------------------- the Range
        /// <summary>The beach Range's night vignette as it films the close shot (neon_acid's fit to a film of flat
        /// swatches), and its smoothness.</summary>
        const float Vignette = 0.372f, VignetteSmooth = 0.5f;

        /// <summary>The Range shot: filmed with t0 = 0.5 (frame 12); At() puts every launch on its clip frame. The close
        /// film counts at Close (the Range's own zoom reads as Mid) and the vignette is handed over.</summary>
        [CelRangeEffect("red_lasers")]
        static void Range(RangeShot s)
        {
            CelFx.ZoomOverride = s.Lod == RangeLod.Close ? CelFx.CloseZoom : s.Zoom;
            Lens(Vignette, VignetteSmooth);
            Play(s.Muzzle, s.Target, s.Ground, (uint)s.Seed);
        }
    }
}
