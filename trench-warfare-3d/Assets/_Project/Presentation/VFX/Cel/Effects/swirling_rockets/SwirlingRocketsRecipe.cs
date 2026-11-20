// Phase: CelFx swirling rockets (2026-11-15, r01: built by eye against the clip). Six rockets fly in from the eye's
// left, each on its own looping path, trailing a flame and a beaded smoke trail and throwing spark dashes; two fly into
// another's flame and are gone, four strike the dummy and burst into a lobed fireball that cools to charred smoke and
// rises, throwing debris sticks and sparks and leaving embers on the ground; the ground is lit under the flight and
// under the burst.
// Reference: swirling_rockets_nolora_00001_.mp4, 192 f at 24 fps, drawn on ONES; the effect starts on its frame 12
// (the Range's t0 0.5). The paths are SwirlingRocketsPaths (the clip's own rockets, through the Range's camera); sizes
// are read off the clip at 71.4 px to the metre at the dummy; colours are the clip's pixels written through the
// Range's grade (the neon_acid session's grade.py inverse, scratchpad sr/palettes.py).
//
// It draws through the kind's own shader (CelSwirlingRockets_URP): 0 a stretch of trail (the flame and the smoke on
// one spline), 1 a fleck (spark dashes, debris sticks, embers), 2 the hit's star, 3 the burst (one card), 4 a rocket's
// body, 7 the light on the ground.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class SwirlingRocketsRecipe
    {
        /// <summary>Seconds the whole effect is alive: frame 12 to the last ember near frame 166.</summary>
        public const float Length = 6.45f;

        /// <summary>The clip's effect starts on its frame 12. AtFrame gives the seconds after the call at which one of the
        /// clip's frames falls; `late` is how much later than the clip's this play starts.</summary>
        const float FirstFrame = 12f;
        static float AtFrame(float late, float frame) => (frame - FirstFrame) / 24f - 0.004f + late;
        /// <summary>The clip's frame at `t` seconds after the call (AtFrame's inverse).</summary>
        static float FrameAt(float late, float t) => FirstFrame + (t - late + 0.004f) * 24f;

        // ---- sizes, in metres (71.4 px to the metre at the dummy)
        /// <summary>The flame runs back this far along the path from the nozzle (130 px at the dummy).</summary>
        public const float FlameLength = 2.2f;   // r27 (1.8)
        /// <summary>A stretch of trail's half-width: the largest lump (0.167 m x 1.45 swell, raised, with a bump on top
        /// and its ink), the flame and the glints round it (out to 0.33 m), with room (r11).</summary>
        const float StripHalf = 0.55f;
        /// <summary>The body: 75 x 17 px at the dummy.</summary>
        // r21: the clip's rockets ~0.77 m nozzle to tip, ~0.17 m thick (sr/look/r20_rk.jpg)
        public const float BodyLength = 0.78f, BodyRadius = 0.085f;
        /// <summary>The longest a puff of the trail lives (the shader deflates each section at 0.55-2.65 s).</summary>
        const float SmokeLife = 2.7f;
        /// <summary>The burst at its peak (frame 103: 190 x 210 px) centred a little left of the chest; drawn toward the
        /// eye so the ground in front of the dummy does not cut its foot; it rises as it turns to smoke.</summary>
        const float CloudHalfWidth = 1.40f, CloudHalfHeight = 1.50f, CloudPull = 1.6f, CloudRise = 1.4f;

        // A colour as the film shows it. The project renders in gamma space and the palette stores .linear, so a colour
        // handed over as picked would come out one gamma too dark: this takes it back first (FireballRecipe.C).
        static Color C(int rgb)
        {
            Color c = CelPalette.Hex(rgb);
            return new Color(Mathf.LinearToGammaSpace(c.r), Mathf.LinearToGammaSpace(c.g), Mathf.LinearToGammaSpace(c.b), 1f);
        }

        // ---- colours: the values to WRITE for the Range's grade to show the clip's (comments: what the clip shows),
        // through the MEASURED grade (r02: the cyan_beam e00cal swatch film, scratchpad sr/grade3; grade2.json's model
        // overshot every bright colour and turned the flame green). The body's are the clip's own clusters (f76-f88).
        // the flame: deep orange edge (236,128,66), orange (245,170,108), yellow-orange (248,200,131), pale yellow
        // (252,229,147), the white-hot core (253,246,200)
        static readonly int Flame = CelPalette.Add("sr_flame", C(0xE86C3A), C(0xFF8D59), C(0xFFAB6B), C(0xE6CF7F), C(0xE6EEC1), 1.1f, 4, C(0xAD4F25));
        // the smoke, r07, the clip's trail profile (top to bottom over every level stretch) and at 6x: sr_smoke is c0 the belly
        // (98,82,76), c1 the base (114,96,87) and c2 the puffs' lighter tops (140,119,106) where no flame is near, c3
        // the base (130,100,86) and core the tops (182,131,100) in the flames' light, ink near black (16,9,9);
        // sr_smoke2 c0 the belly in the flames' light (112,85,76), c1 the pale glow right behind the jet
        // (238,185,135), c2 the warm dark ink on a lit top (64,30,14)
        // r08 (r07's film against the clip's profile): c0 (90,75,70), c1 (108,91,83), c2 (132,112,100), c3 (124,96,84),
        // core (155,116,94); sr_smoke2 c0 (106,81,72)
        // r11 (the clip's trail body pixels f60-f124): c0 (92,77,72), c1 (110,93,86), c2 (148,128,116), c3 (120,93,81),
        // core (150,113,92)
        static readonly int Smoke = CelPalette.Add("sr_smoke", C(0x514641), C(0x5E524A), C(0x7D6C5F), C(0x675246), C(0x7F614F), 1.1f, 4, C(0x0D0507));
        static readonly int Smoke2 = CelPalette.Add("sr_smoke2", C(0x5B4940), C(0xE89B6D), C(0x3A1F0C), C(0x3A1F0C), C(0xE89B6D), 1.1f, 4, C(0x3A1F0C));
        // the body, lit from below (R5 f82/f84 at 8x): c0 the warm underside (232,145,100), c1 the warm middle
        // (168,110,90), c2 the greyish mauve top (138,104,93), c3 its dark top edge (112,86,84); core = the nose's
        // top (100,70,72); ink = its outline (42,18,16)
        static readonly int Body = CelPalette.Add("sr_body", C(0xDC7953), C(0x8E5F4C), C(0x745B4F), C(0x604D49), C(0x574140), 1.1f, 4, C(0x28110E));
        // the burst's fire: rim (205,112,58), orange (238,170,99), yellow mortar (248,218,88), pale (249,240,165),
        // white (253,252,246); ink = the red-brown line round a hot lobe (150,74,44)
        static readonly int Fire = CelPalette.Add("sr_fire", C(0x7D411C), C(0xE7752F), C(0xEBC04D), C(0xD8E294), C(0xFFFFFF), 1.1f, 4, C(0x4F311C));   // r30: the clip's deep orange
        // what it cools to: char (44,30,26), the crease between lobes (64,48,41), the smoke as it comes (58,45,40)
        // and as it goes (70,56,49)
        static readonly int Charred = CelPalette.Add("sr_char", C(0x362A23), C(0x251F1A), C(0x322B25), C(0x40352F), C(0x4D3F34), 1.1f, 4, C(0x2B1F1A));   // r30: + lit tops; r33 darker
        // the spark dashes: orange (214,151,108), (232,180,126), a pale middle (250,222,160)
        static readonly int Spark = CelPalette.Add("sr_spark", C(0xC07D59), C(0xC07D59), C(0xDA9666), C(0xE8C588), C(0xE8C588), 1.1f, 4, C(0x4E2813));
        // the embers on the ground: (167,117,74), (206,150,96), a pale middle (246,215,168)
        static readonly int Ember = CelPalette.Add("sr_ember", C(0x8D6440), C(0x8D6440), C(0xB67D50), C(0xE5BB8C), C(0xE5BB8C), 1.1f, 4, C(0x371F0E));
        // the burst's debris sticks: near black (30,22,20)
        static readonly int Debris = CelPalette.Add("sr_debris", C(0x1E1613), C(0x1E1613), C(0x231A15), C(0x271F1A), C(0x271F1A), 1.1f, 4, C(0x130C0A));
        // the hit's star and rays: white (255,250,236) with a warm edge (255,200,150)
        static readonly int Ray = CelPalette.Add("sr_ray", C(0xFFB17C), C(0xFFB17C), C(0xFEC29D), C(0xFFFFFF), C(0xFFFFFF), 1.1f, 4, C(0x4E2813));

        /// <summary>The light the flight and the fire throw on the ground: most of it multiplied into the ground (it
        /// keeps its pattern), the rest added on top (FireballRecipe's).</summary>
        // r24: warm, no blue (the clip's lit ground rises (20, 10, 2), r23's (7, 4, 4))
        static readonly Vector3 GlowInto = new Vector3(1.40f, 0.60f, 0.0f), GlowAdd = new Vector3(0.22f, 0.09f, 0.0f);   // r25: browner
        // r27: the burst's light whiter (the clip's lit ground under it (84,48,19))
        static readonly Vector3 BurstInto = new Vector3(1.40f, 0.70f, 0.14f), BurstAdd = new Vector3(0.22f, 0.10f, 0.02f);   // r28: gold

        static readonly int LensId = Shader.PropertyToID("_TWAcidLens");
        static SwirlingRocketsRecipe() { SceneStatics.Register(nameof(SwirlingRocketsRecipe), () => Shader.SetGlobalVector(LensId, Vector4.zero)); }

        /// <summary>The scene's lens vignette (URP Vignette's intensity and smoothness; 0 for none), for whoever shows the
        /// effect to hand over: the colours are written brighter where the vignette will darken them.</summary>
        public static void Lens(float intensity, float smoothness) =>
            Shader.SetGlobalVector(LensId, new Vector4(Mathf.Max(0f, intensity) * 3f, Mathf.Max(0.01f, smoothness) * 5f, 0f, 0f));

        /// <summary>How many rockets fly.</summary>
        public static int Rockets => SwirlingRocketsPaths.All.Length;

        /// <summary>Where rocket `rocket` is on clip frame `frame` (clamped to its life), for a target and the eye's
        /// level right.</summary>
        public static Vector3 PathPoint(Vector3 target, Vector3 right, int rocket, int frame)
        {
            var path = SwirlingRocketsPaths.All[rocket];
            int f = Mathf.Clamp(frame, path.First, path.Last) - path.First;
            Basis(right, out Vector3 r, out Vector3 away);
            return target + r * path.P[f * 3] + Vector3.up * path.P[f * 3 + 1] + away * path.P[f * 3 + 2];
        }

        static void Basis(Vector3 right, out Vector3 r, out Vector3 away)
        {
            right.y = 0f;
            r = right.sqrMagnitude > 0.001f ? right.normalized : Vector3.right;
            away = Vector3.Cross(r, Vector3.up);   // level, from the eye
        }

        // one rocket's keyframes (every second clip frame and its last), reused: a play allocates nothing
        const int MaxKeys = 64;
        static readonly Vector3[] keyP = new Vector3[MaxKeys];
        static readonly float[] keyT = new float[MaxKeys], keyS = new float[MaxKeys];

        static Vector3 Spline(Vector3 a, Vector3 b, Vector3 c, Vector3 d, float u)
        {
            float u2 = u * u, u3 = u2 * u;
            return 0.5f * (2f * b + (c - a) * u + (2f * a - 5f * b + 4f * c - d) * u2 + (3f * b - a - 3f * c + d) * u3);
        }

        static Vector3 SplineD(Vector3 a, Vector3 b, Vector3 c, Vector3 d, float u) =>
            0.5f * ((c - a) + 2f * (2f * a - 5f * b + 4f * c - d) * u + 3f * (3f * b - a - 3f * c + d) * u * u);

        /// <summary>The path length the head has flown at `t` seconds after the call (keys 0..n-1).</summary>
        static float HeadAt(int n, float t)
        {
            if (t <= keyT[0]) return 0f;
            for (int i = 0; i < n - 1; i++)
                if (t < keyT[i + 1]) return keyS[i] + (keyS[i + 1] - keyS[i]) * (t - keyT[i]) / Mathf.Max(keyT[i + 1] - keyT[i], 1e-5f);
            return keyS[n - 1];
        }

        /// <summary>
        /// Play the swirling rockets onto `target` (the dummy's chest), whose foot stands on the ground at `ground`;
        /// `right` is the eye's level right (the clip's rockets come from the eye's left). `delay` holds it for that
        /// long after the call. Everything is spawned now, the light it throws on the ground too: nothing is left to
        /// do per frame. No allocation.
        /// </summary>
        public static void Play(Vector3 target, Vector3 ground, Vector3 right, uint seed, float delay = 0f)
        {
            Basis(right, out right, out Vector3 away);
            var renderer = CelFx.Renderer;
            int kind = renderer.KindId(SwirlingRocketsKind.KindName);
            if (kind < 0) throw new System.InvalidOperationException("SwirlingRockets kind is not registered");
            float lod = CelFx.LodBlend(CelFx.Zoom);
            bool mid = lod >= 0.5f, far = lod >= 1.5f;
            float now = CelFx.Now;
            float late = delay;
            ground.y = Ground(ground);
            // the burst's middle: a little left of the chest (the clip's fireball is centred at (825,398))
            Vector3 heart = target - right * 0.12f;

            // ---- the light on the ground, under everything: pools under the swirl while the rockets pass, then the
            // big one under the burst. A light has nothing random in it: its own generator, so the parts' draws stay put.
            {
                var lights = new CelRng(seed ^ 0x5A17u);
                // r24: 2.6x (the clip's ground under the swirl rises ~3x r23's)
                // r25: over 10 m, the last to f102 (the clip's ground still lit at f96); r26: 0.35x r25's (3x too
                // strong in front)
                FlightLight(renderer, kind, now, late, target, right, 22, 44, 0.31f, ref lights);
                FlightLight(renderer, kind, now, late, target, right, 40, 64, 0.35f, ref lights);
                FlightLight(renderer, kind, now, late, target, right, 56, 82, 0.40f, ref lights);
                FlightLight(renderer, kind, now, late, target, right, 74, 102, 0.45f, ref lights);
                Vector3 pool = ground;
                pool.y = Ground(pool) + 1f;
                // r24: the clip's burst light peaks f100-106 and is gone by f115
                Glow(renderer, kind, now, AtFrame(late, 96f), AtFrame(late, 113f), pool, right, 10f, 10f, 10f, 1f, far ? 2.6f : 4.2f,
                    0.25f, 0.30f, true, ref lights, BurstInto, BurstAdd);   // r27: whiter, over 10 m   // r26: gone by f113 (the clip's at f112 (12,4,0))
            }

            // ---- the rockets: two frames of path a stretch, each stretch one trail record and one body record, and
            // a few spark dashes thrown off its flame (the trail draws the glints along itself: no records)
            var sparkRng = new CelRng(seed ^ 0x2C9Bu);
            int sparksPer = far ? 0 : mid ? 1 : 3;
            for (int j = 0; j < SwirlingRocketsPaths.All.Length; j++)
            {
                var path = SwirlingRocketsPaths.All[j];
                int n = 0;
                for (int f = path.First; n < MaxKeys; f += 2)
                {
                    int ff = Mathf.Min(f, path.Last);
                    keyP[n] = PathPoint(target, right, j, ff);
                    keyT[n] = AtFrame(late, ff);
                    n++;
                    if (ff >= path.Last) break;
                }
                if (n < 2) continue;
                // path lengths at the keys, measured along the spline
                keyS[0] = 0f;
                for (int i = 0; i < n - 1; i++)
                {
                    Vector3 a = i > 0 ? keyP[i - 1] : 2f * keyP[0] - keyP[1];
                    Vector3 d = i + 2 < n ? keyP[i + 2] : 2f * keyP[n - 1] - keyP[n - 2];
                    float len = 0f;
                    Vector3 prev = keyP[i];
                    for (int k = 1; k <= 8; k++)
                    {
                        Vector3 p = Spline(a, keyP[i], keyP[i + 1], d, k / 8f);
                        len += (p - prev).magnitude;
                        prev = p;
                    }
                    keyS[i + 1] = keyS[i] + Mathf.Max(len, 1e-3f);
                }
                float end = keyT[n - 1];
                float rocketSeed = (j + 0.5f) / SwirlingRocketsPaths.All.Length;
                for (int i = 0; i < n - 1; i++)
                {
                    Vector3 a = i > 0 ? keyP[i - 1] : 2f * keyP[0] - keyP[1];
                    Vector3 d = i + 2 < n ? keyP[i + 2] : 2f * keyP[n - 1] - keyP[n - 2];
                    float born = keyT[i], T = Mathf.Max(keyT[i + 1] - keyT[i], 1e-3f), len = keyS[i + 1] - keyS[i];
                    bool last = i == n - 2;
                    // the trail: its flame while the head is near, its smoke for as long as a puff lives
                    var t = Rec(now, born, Mathf.Min(T + SmokeLife, Length - born + late), SwirlingRocketsKind.Trail, keyP[i], Flame, 1f, rocketSeed);
                    t.Vel = keyP[i + 1];
                    t.Axis = a;
                    t.Erode = new Vector4(d.x, d.y, d.z, keyS[i]);
                    t.Tone.x = CelRecord.PackPalette(Flame, Smoke);
                    t.Tone.w = CelRecord.PackPalette(Spark, Smoke2);
                    // the clip clears the swirl's old loops: a trail laid before ~frame 62 is gone by frame 78-92, a
                    // stretch of a few metres at a time (r07; the clip's trails born f40-63 die at f85-90 at the median);
                    // a later one lives its own life (the shader's)
                    float bornF = FrameAt(late, born);
                    // r23: 3 frames later (the clip's loops laid f41-55 half there at f85-92); R1's (path 0) oldest
                    // trail goes ~1.5 s after it is laid (the clip's half gone by f60-68)
                    float wipeF = 88f + 5f * Mathf.Sin(keyS[i] * 0.7f + j * 2.1f) + 2.5f * Mathf.Sin(keyS[i] * 2.3f + j) + 4f * Mathf.Max(0f, bornF - 58f);
                    if (j == 0) wipeF = Mathf.Min(wipeF, bornF + 36f + 60f * Smooth(38f, 46f, bornF));
                    float wipe = Mathf.Max((wipeF - bornF) / 24f, 0.35f);
                    // r10: a rocket gone in the air (into another's flame): the last 0.65 s of its trail goes 0.40-0.65 s
                    // after it (the clip's R2: laid f64-76, gone by f83-89)
                    if (!path.Hits && j != 0 && end - born < 0.65f)             // r23: not R1 (its trail stays)
                        wipe = Mathf.Min(wipe, end - born + 0.40f + 0.25f * Mathf.Repeat(keyS[i] * 1.37f, 1f));
                    t.Size = new Vector4(T, len, StripHalf, wipe);
                    float after = keyT[i + 1];
                    t.Motion = new Vector4(HeadAt(n, after + 2f / 24f), HeadAt(n, after + 4f / 24f), HeadAt(n, after + 6f / 24f), HeadAt(n, after + 8f / 24f));
                    // r12: + 2 x how much its smoke swells as it ages (the clip's swirl trails, laid before ~frame 60,
                    // do not; its final approach's, laid from ~72, swell to 1.45x)
                    // r23: + 512 for R1, whose last trail keeps no end rule (the clip keeps it to f100)
                    t.Extra.y = (last ? 1f : 0f) + 2f * Mathf.Round(255f * Smooth(60f, 72f, bornF)) + (j == 0 ? 512f : 0f);
                    t.Extra.z = end - born;
                    // the light the flames throw on this stretch's smoke over its life (r06: the clip's puffs are lit
                    // warm while a flame is within ~3 m of them, cool grey-tan once none is): six samples 0.42 s apart
                    Vector3 mid3 = Spline(a, keyP[i], keyP[i + 1], d, 0.5f);
                    float fb = FrameAt(late, born);
                    t.Tone.y = Pack3(LightAt(target, right, heart, mid3, fb), LightAt(target, right, heart, mid3, fb + 10.08f),
                        LightAt(target, right, heart, mid3, fb + 20.16f));
                    t.Tone.z = Pack3(LightAt(target, right, heart, mid3, fb + 30.24f), LightAt(target, right, heart, mid3, fb + 40.32f),
                        LightAt(target, right, heart, mid3, fb + 50.40f));
                    renderer.Spawn(kind, t);
                    // the body at the head while it flies this stretch (not seen this far off)
                    if (!far)
                    {
                        var b = Rec(now, born, T, SwirlingRocketsKind.Body, keyP[i], Body, 1f, rocketSeed);
                        b.Vel = keyP[i + 1];
                        b.Axis = a;
                        b.Erode = new Vector4(d.x, d.y, d.z, 0f);
                        b.Size = new Vector4(T, BodyLength, BodyRadius, 0f);
                        renderer.Spawn(kind, b);
                    }
                    // spark dashes off the flame, flung on along the path and a little out (the clip's lie within
                    // 15-30 degrees of the path and 0.1-0.3 m from it, 1-2 px wide and 6-18 px long)
                    float speed = len / T;
                    for (int s = 0; s < sparksPer; s++)
                    {
                        float u = sparkRng.Next();
                        Vector3 p = Spline(a, keyP[i], keyP[i + 1], d, u);
                        Vector3 tg = SplineD(a, keyP[i], keyP[i + 1], d, u).normalized;
                        Vector3 side = Vector3.Cross(tg, Vector3.up);
                        if (side.sqrMagnitude < 1e-4f) side = right;
                        side.Normalize();
                        Vector3 up2 = Vector3.Cross(side, tg);
                        float ang = sparkRng.Range(0f, Mathf.PI * 2f);
                        Vector3 outward = side * Mathf.Cos(ang) + up2 * Mathf.Sin(ang);
                        Vector3 at = p - tg * sparkRng.Range(0.20f, 1.30f) + outward * sparkRng.Range(0.02f, 0.16f);
                        var sp = Rec(now, born + u * T + sparkRng.Range(0f, 0.03f), sparkRng.Range(0.20f, 0.60f), SwirlingRocketsKind.Drop, at, Spark, 1f, sparkRng.Next());
                        sp.Vel = tg * speed * sparkRng.Range(0.12f, 0.45f) + outward * sparkRng.Range(0.25f, 1.1f);
                        sp.Motion = new Vector4(1.5f, 2.2f, 0f, 0f);
                        sp.Size = new Vector4(0.011f, 0.007f, 1f, sparkRng.Range(0.020f, 0.038f));
                        sp.Extra.y = 2f;             // a leaf: a short dash pointed at both ends
                        renderer.Spawn(kind, sp);
                    }
                }
            }

            // ---- the hits: a small white flash where each early rocket strikes (R3 f93, R5 f95), then the star
            var hitRng = new CelRng(seed ^ 0x7E11u);
            if (!far)
            {
                for (int j = 0; j < SwirlingRocketsPaths.All.Length; j++)
                {
                    var path = SwirlingRocketsPaths.All[j];
                    if (!path.Hits || path.Last > 96) continue;
                    var r = Rec(now, AtFrame(late, path.Last), 0.09f, SwirlingRocketsKind.Splat, PathPoint(target, right, j, path.Last), Ray, 1f, hitRng.Next());
                    r.Size = new Vector4(0.45f, 0.45f, 1f, 1f);
                    r.Extra.y = 0.3f;
                    renderer.Spawn(kind, r);
                }
                var star = Rec(now, AtFrame(late, 96f), 0.09f, SwirlingRocketsKind.Splat, heart, Ray, 1f, hitRng.Next());
                star.Size = new Vector4(1.5f, 1.5f, 1f, 1f);
                star.Extra.y = CloudPull + 0.3f;
                renderer.Spawn(kind, star);
            }

            // ---- THE BURST: one card. Fire, char, smoke, on the clip's frame clock from frame 96.
            {
                var r = Rec(now, AtFrame(late, 96f), 2.40f, SwirlingRocketsKind.Cloud, heart, Fire, 1f, hitRng.Next());
                r.Tone.x = CelRecord.PackPalette(Fire, Charred);
                r.Size = new Vector4(CloudHalfWidth, CloudHalfHeight, CloudPull, far ? 6f : mid ? 11f : 16f);   // r29: fewer, bigger (28)
                r.Extra.y = CloudRise;
                renderer.Spawn(kind, r);
            }

            // ---- the white rays out of the hit (frames 96-101): needles thrown out all round, gone in two frames
            int rays = far ? 0 : mid ? 3 : 6;          // r29: the clip's few (30)
            for (int i = 0; i < rays; i++)
            {
                float turn = hitRng.Range(-0.15f, 1.15f) * Mathf.PI;
                Vector3 dd = (right * Mathf.Cos(turn) + Vector3.up * Mathf.Sin(turn) + away * hitRng.Range(-0.25f, 0.25f)).normalized;
                var r = Rec(now, AtFrame(late, 96f + hitRng.Range(0f, 2f)), 0.10f, SwirlingRocketsKind.Drop, heart + dd * hitRng.Range(0.5f, 1.2f), Ray, 1f, hitRng.Next());
                r.Vel = dd * hitRng.Range(5f, 9f);
                r.Size = new Vector4(0.035f, 0.025f, 1f, hitRng.Range(0.025f, 0.045f));
                r.Extra.y = 2f;
                renderer.Spawn(kind, r);
            }

            // ---- debris sticks thrown out of the fireball (frames 103-116): thin black bars
            int sticks = far ? 0 : mid ? 4 : 10;
            for (int i = 0; i < sticks; i++)
            {
                float turn = hitRng.Range(-0.2f, 1.2f) * Mathf.PI;
                Vector3 dd = (right * Mathf.Cos(turn) + Vector3.up * Mathf.Sin(turn) + away * hitRng.Range(-0.3f, 0.3f)).normalized;
                var r = Rec(now, AtFrame(late, hitRng.Range(102f, 108f)), hitRng.Range(0.35f, 0.65f), SwirlingRocketsKind.Drop,
                    heart + dd * hitRng.Range(1.3f, 1.9f), Debris, 1f, hitRng.Next());   // r30: out of the mass
                r.Vel = dd * hitRng.Range(4f, 8f) + Vector3.up * hitRng.Range(1f, 3f);
                r.Motion = new Vector4(9f, 0.3f, 0f, 0f);
                r.Size = new Vector4(0.016f, 0.013f, 1f, hitRng.Range(0.04f, 0.07f));
                r.Extra.y = 3f;                  // a stick
                renderer.Spawn(kind, r);
            }

            // ---- sparks out of the fireball (frames 102-124): short dashes thrown out that fall
            int sparks = far ? 0 : mid ? 60 : 150;      // r30: the clip's many (70)
            for (int i = 0; i < sparks; i++)
            {
                float turn = hitRng.Range(-0.1f, 1.1f) * Mathf.PI;
                Vector3 dd = (right * Mathf.Cos(turn) + Vector3.up * Mathf.Sin(turn) + away * hitRng.Range(-0.5f, 0.5f)).normalized;
                var r = Rec(now, AtFrame(late, hitRng.Range(102f, 130f)), hitRng.Range(0.8f, 1.8f), SwirlingRocketsKind.Drop,   // r31
                    heart + dd * hitRng.Range(0.6f, 1.5f), Spark, 1f, hitRng.Next());
                r.Vel = dd * hitRng.Range(3f, 9f) + Vector3.up * hitRng.Range(0.5f, 3.5f);
                r.Motion = new Vector4(10f, 0.8f, 0f, 0f);
                r.Size = new Vector4(0.024f, 0.018f, 1f, hitRng.Range(0.006f, 0.014f));   // r31: the clip's 3-5 px
                r.Extra.y = 2f;
                renderer.Spawn(kind, r);
            }

            // ---- the embers left on the ground round the foot (frames 116-160): small glowing dots that go out one by one
            int coals = far ? 0 : mid ? 60 : 150;
            for (int i = 0; i < coals; i++)
            {
                Vector2 on = hitRng.InDisc();
                Vector3 at = ground + right * (on.x * 2.3f) + away * (on.y * 0.8f - 0.25f);
                at.y = Ground(at) + 0.04f;
                float rad = hitRng.Range(0.022f, 0.045f);
                var r = Rec(now, AtFrame(late, hitRng.Range(114f, 126f)), hitRng.Range(0.9f, 1.8f), SwirlingRocketsKind.Drop, at, Ember, 1f, hitRng.Next());
                r.Size = new Vector4(rad, rad * 0.6f, 1f, 0.004f);
                r.Extra.y = 1f;                  // a round fleck
                renderer.Spawn(kind, r);
            }
        }

        /// <summary>How much of the flames' light falls on the smoke at `m` on clip frame `frame` (0..1): every flame
        /// in the air lights it fully within a metre or so, half at 3 m, none past 4.2 m (the clip's trail tops against
        /// the nearest flame's distance), and the burst while it burns (frames 96-112) within 3-5 m; the lights add as
        /// 1 - the product of what each leaves dark.</summary>
        static float LightAt(Vector3 target, Vector3 right, Vector3 heart, Vector3 m, float frame)
        {
            float dark = 1f;
            for (int j = 0; j < SwirlingRocketsPaths.All.Length; j++)
            {
                var path = SwirlingRocketsPaths.All[j];
                if (frame < path.First || frame > path.Last) continue;
                int f0 = Mathf.FloorToInt(frame);
                Vector3 p = Vector3.Lerp(PathPoint(target, right, j, f0), PathPoint(target, right, j, f0 + 1), frame - f0);
                float dist = (p - m).magnitude;
                dark *= 1f - Mathf.Clamp01((1f - Smooth(2.6f, 4.2f, dist)) * (1f - 0.12f * dist));
            }
            if (frame >= 96f)
                // r29: to f135 within 4.5-7.5 m (the clip's trails by the burst lit to f130)
                dark *= 1f - (1f - Smooth(118f, 150f, frame)) * (1f - Smooth(4.5f, 7.5f, (heart - m).magnitude));
            return 1f - dark;
        }

        static float Smooth(float a, float b, float x)
        {
            float t = Mathf.Clamp01((x - a) / (b - a));
            return t * t * (3f - 2f * t);
        }

        /// <summary>Three 0..1 values in one float, 8 bits each (exact: 24 bits of mantissa).</summary>
        static float Pack3(float a, float b, float c) =>
            Mathf.Round(Mathf.Clamp01(a) * 255f) + 256f * Mathf.Round(Mathf.Clamp01(b) * 255f) + 65536f * Mathf.Round(Mathf.Clamp01(c) * 255f);

        /// <summary>A pool of light under where the rockets are between clip frames `from` and `to`: under the middle
        /// of their places then, at the ground.</summary>
        static void FlightLight(CelFxRenderer renderer, int kind, float now, float late, Vector3 target, Vector3 right,
            int from, int to, float strength, ref CelRng rng)
        {
            Vector3 sum = Vector3.zero;
            int count = 0;
            for (int j = 0; j < SwirlingRocketsPaths.All.Length; j++)
            {
                var path = SwirlingRocketsPaths.All[j];
                for (int f = Mathf.Max(from, path.First); f <= Mathf.Min(to, path.Last); f += 4)
                {
                    sum += PathPoint(target, right, j, f);
                    count++;
                }
            }
            if (count == 0) return;
            Vector3 at = sum / count;
            at.y = Ground(at) + 1f;
            Glow(renderer, kind, now, AtFrame(late, from), AtFrame(late, to), at, right, 10.0f, 10.0f, 10.0f, 1f, strength, 0.25f, 0.3f, true, ref rng);   // r24: 7 m; r25: 10 m
        }

        static void Glow(CelFxRenderer renderer, int kind, float now, float born, float gone, Vector3 at, Vector3 axis,
            float along, float up, float behind, float ahead, float strength, float rise, float fall, bool level, ref CelRng rng,
            Vector3 into = default, Vector3 add = default)
        {
            var r = Rec(now, born, gone - born, SwirlingRocketsKind.Glow, at, Fire, strength, rng.Next());
            r.Tone.y = level ? 1f : 0f;
            r.Axis = axis;
            r.Vel = into == default ? GlowInto : into;
            r.Size = new Vector4(along, up, ahead, behind / along);
            Vector3 ad = add == default ? GlowAdd : add;
            r.Erode = new Vector4(ad.x, ad.y, ad.z, 0f);
            r.Extra.y = rise;
            r.Extra.z = fall;
            renderer.Spawn(kind, r);
        }

        static CelRecord Rec(float now, float at, float life, float mode, Vector3 pos, int palette, float opacity, float seed)
        {
            return new CelRecord
            {
                Pos = pos, Born = now + at, Life = life, Axis = Vector3.right, Seed = seed,
                Size = new Vector4(1f, 1f, 1f, 1f), Erode = new Vector4(0f, 0f, 1f, 0f),
                Tone = new Vector4(CelRecord.PackPalette(palette), 1f, opacity, 0f),
                Extra = new Vector4(mode, 0f, 0f, at),
            };
        }

        static float Ground(Vector3 p) => RenderGround.Map != null
            ? RenderGround.Sample(RenderGround.Map, p.x, p.z) : p.y;
    }
}
