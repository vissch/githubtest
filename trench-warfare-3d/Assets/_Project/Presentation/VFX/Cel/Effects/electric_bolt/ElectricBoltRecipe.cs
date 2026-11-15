// Phase: CelFx electric bolt (2026-11-15, lane/show/cel-electric-bolt) - the rapid-fire electric bolt of the owner's
// clip electric_bolt_cel_00001_ (8 s at 24 fps; what it is: celfx spec\eye\electric_bolt.md). A stream of short
// electric comets from off-screen at the eye's lower left into the target's board, about eleven a second for four
// seconds on two lanes; each landing throws a white-hot blob and a spray of thin sparks; the bolts carry on through the
// target as two jagged lightning arcs to ground flares behind and right of it; a big cyan light lies on the ground round
// it. Nothing is left after it (no smoke, no scorch).
// Built from blue_laser (the LightningBeam ribbon, its soft dots and spark layers) and neon_acid (the light found from
// the scene's depth). The numbers were read off the clip: the bolts' line and speed from a 3D fit of three tracked heads
// through the beach Range's camera (7 px rms), the arcs' ends from where the clip's flares sit on the Range's ground,
// the times from its frames (the comments name them), the colours from its pixels (taken back through the Range's
// grade by a first guess, then by films).
// LAB ONLY: no weapon fires it yet; the Range calls Play. Energy weapon, so blue light is allowed.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class ElectricBoltRecipe
    {
        // ---------------------------------------------------------------- the clip's clock
        /// <summary>The clip's frame the effect is played on (the Range fires it at t0 0.35 s): the frames below are the
        /// clip's, and At gives the seconds after Play at which one falls.</summary>
        const float FirstFrame = 8.4f;
        static float At(float frame) => (frame - FirstFrame) / 24f;

        // ---------------------------------------------------------------- the bolts
        /// <summary>m/s, and seconds from the launch to the board (the fit: 21.9 m/s on a straight, nearly level line).</summary>
        public const float Speed = 21.9f, Flight = 0.6f;
        /// <summary>The first bolt lands on frame 22.96, the last on frame 116 (one every 2.2 frames, 4-6 in flight).</summary>
        const float FirstLand = 22.96f, LastLand = 116f;
        const int Bolts = 43;
        /// <summary>A bolt's length (m) and its head's full width (m): 100-200 px long and 14-17 px wide near the eye,
        /// 55-80 px and 8-10 px by the dummy.</summary>
        const float BoltLength = 1.5f, BoltWidth = 0.11f;
        /// <summary>The second lane: this far under the first where they leave (m); four bolts in ten take it.</summary>
        const float LaneDrop = 0.28f, LaneShare = 0.4f;

        // ---------------------------------------------------------------- where things are, in the bolts' frame
        // (metres from the target's chest: along the bolts' level flight, to its side (the eye's right), up). From the
        // clip's screen positions through the Range camera; the flares' points are put on the Range's own ground along
        // the close camera's ray through them, so they sit where the clip shows them whatever the ground does there.
        static readonly Vector3 Eye = new Vector3(-13.3f, 12.489f, 5.399f);          // the close camera
        static readonly Vector3 Blob = new Vector3(-0.217f, 0.311f, 0.097f);          // the hit's white-hot middle (840, 400)
        static readonly Vector3 UpRoot = new Vector3(0.397f, 0.262f, 0.144f);         // the upper arc leaves the board (868, 388)
        static readonly Vector3 UpTip = new Vector3(9.05f, -1.707f, 0.008f);          // its flare on the ground behind (1100, 292)
        static readonly Vector3 LowRoot = new Vector3(0.561f, 0.658f, -0.556f);       // the lower arc leaves the board (895, 440)
        static readonly Vector3 LowTip = new Vector3(3.749f, 1.902f, -1.101f);        // its flare on the ground right (1100, 452)
        static readonly Vector3 FootLight = new Vector3(0.217f, 0.734f, -1.345f);     // the ground light's middle (880, 500)
        static readonly Vector3 PathLight = new Vector3(-5.825f, -1.224f, -1.768f);   // the light under the bolts (430, 600)
        static readonly Vector3 Haze = new Vector3(-0.903f, 0.477f, -1.438f);         // the violet haze before the first hit (810, 520)

        // ---------------------------------------------------------------- palettes
        // Colours as the framebuffer has to hold them for the Range's post stack (exposure 1.62, the neutral tonemap,
        // the LUT) to show the clip's: the clip's colour classes (celfx runs\electric_bolt\_ref\look\colours_ref.json)
        // taken back through neon_acid's fit of that stack (grade.py inverse). The project is gamma and the palette
        // stores .linear, so S() hands over a stored value as the colour whose .linear it is. Opaque colours are taken
        // back at the middle of the frame (the shader's Unvignette undoes the lens elsewhere); added light is what has
        // to go on top of the clip's plate there.
        static Color S(float r, float g, float b) => new Color(Mathf.LinearToGammaSpace(r), Mathf.LinearToGammaSpace(g), Mathf.LinearToGammaSpace(b), 1f);

        /// <summary>The bolts: c2 the cyan-blue edge band (116,173,219), c3 pale cyan (175,226,252), the white core
        /// (224,252,255) over 0.62; the bloom added round them is c1, a pale rim right at the body, and c0, the wide
        /// saturated blue (fitted to a near bolt's glow over the clip's plate of (16,15,22): see the shader's bloom).</summary>
        public static readonly int BoltRow = CelPalette.Add("electric_bolt",
            S(0.082f, 0.232f, 0.406f), S(0.239f, 0.253f, 0.148f), S(0.359f, 0.535f, 0.698f), S(0.509f, 0.757f, 0.876f), S(0.574f, 0.929f, 0.925f), 0.62f, 2, S(0f, 0f, 0f));
        /// <summary>The arcs: greyer than the bolts (the clip's (119,172,204), (183,223,242), a core of (236,250,255));
        /// their bloom (c1 the rim, c0 the wide part) fitted to (86,142,178) next to them, (46,77,104) further out, over
        /// the ground's (26,28,39).</summary>
        public static readonly int ArcRow = CelPalette.Add("electric_bolt_arc",
            S(0.145f, 0.288f, 0.316f), S(0.119f, 0.13f, 0.174f), S(0.368f, 0.532f, 0.638f), S(0.54f, 0.742f, 0.818f), S(0.611f, 0.915f, 0.925f), 0.62f, 2, S(0f, 0f, 0f));
        /// <summary>The hit's flickering blob and the flares (all added on top): a white-hot core (the clip's
        /// (239,252,255) over the board's (43,45,55), with the steady glow under it), c2 + c3 the light round it.</summary>
        public static readonly int HitRow = CelPalette.Add("electric_bolt_hit",
            S(0.05f, 0.1f, 0.12f), S(0.08f, 0.15f, 0.17f), S(0.12f, 0.2f, 0.2f), S(0.08f, 0.1f, 0.08f), S(0.5f, 0.75f, 0.72f), 0.8f, 3, S(0f, 0f, 0f));
        /// <summary>The steady glow on the board through the volley (added): (124,173,198) at its middle, (52,81,111) at
        /// its edge.</summary>
        public static readonly int BoardRow = CelPalette.Add("electric_bolt_board",
            S(0.04f, 0.09f, 0.11f), S(0.07f, 0.14f, 0.17f), S(0.1f, 0.2f, 0.24f), S(0.12f, 0.16f, 0.16f), S(0.3f, 0.45f, 0.48f), 0.9f, 3, S(0f, 0f, 0f));
        /// <summary>The lavender flash on the board (frame 23-25, added): (184,193,234) at its middle, (93,103,143) at its
        /// edge, over the board's (39,45,56); the violet haze under it uses the same row, faint.</summary>
        public static readonly int LavenderRow = CelPalette.Add("electric_bolt_lavender",
            S(0.1f, 0.1f, 0.15f), S(0.2f, 0.2f, 0.28f), S(0.333f, 0.324f, 0.439f), S(0.389f, 0.385f, 0.449f), S(0.449f, 0.779f, 0.718f), 0.9f, 3, S(0f, 0f, 0f));
        /// <summary>The sparks (kit Streaks, not unvignetted; they fly by the dummy, near the middle): white (250,252,255)
        /// with a pale blue edge (175,215,245).</summary>
        public static readonly int SparkRow = CelPalette.Add("electric_bolt_spark",
            S(0.25f, 0.44f, 0.62f), S(0.545f, 0.717f, 0.847f), S(0.6f, 0.82f, 0.9f), S(0.619f, 0.897f, 0.939f), S(0.662f, 0.951f, 0.949f), 0.7f, 3, S(0.02f, 0.05f, 0.1f));

        // ---------------------------------------------------------------- the light on the ground
        /// <summary>The light the bolts throw on the ground round the target: most of it is multiplied into what it falls
        /// on (lit = unlit x (1 + Into) at its heart: the ground keeps its pattern), a little is added on top. The clip's
        /// lit ground at the foot: (106,173,201) over a plate of (40,46,58), stored 2.4, 3.2, 2.9 times the plate; at
        /// its edge (51,80,101) over (33,38,49), 1.7, 2.0, 1.75 times.</summary>
        static readonly Vector3 Into = new Vector3(1.3f, 2f, 1.7f), Add = new Vector3(0.02f, 0.04f, 0.045f);

        static readonly int LensId = Shader.PropertyToID("_TWBoltLens");

        static ElectricBoltRecipe() { SceneStatics.Register(nameof(ElectricBoltRecipe), () => Shader.SetGlobalVector(LensId, Vector4.zero)); }

        /// <summary>The scene's lens vignette (URP Vignette's intensity and smoothness; 0 for none), for whoever shows the
        /// effect to hand over: the bolts are written brighter where the vignette will darken them.</summary>
        public static void Lens(float intensity, float smoothness) =>
            Shader.SetGlobalVector(LensId, new Vector4(Mathf.Max(0f, intensity) * 3f, Mathf.Max(0.01f, smoothness) * 5f, 0f, 0f));

        // ---------------------------------------------------------------- the sparks (kit Streaks, through CelFx.Play)
        /// <summary>The hit's spray: dozens of short thin white dashes thrown out of the blob at once (the clip's f40,
        /// f69, f72, f104), out to about 100 px.</summary>
        public static readonly CelFxRecipe Sparks = new CelFxRecipe("electric_bolt_sparks", 4.5f,
            new CelLayer
            {
                Name = "spray", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = At(26f), Duration = At(111f) - At(26f), TimeJitter = 0.02f, Life = 0.16f, LifeJitter = 0.4f,
                Close = 420, Mid = 140, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.12f, 0f, 0f), Aim = CelAim.Radial, Speed = 6.5f, SpeedJitter = 0.55f, UpBias = 0.15f, Gravity = 2f, Drag = 2.2f,
                Size = new CelCurve(0.026f, 0.012f), SizeJitter = 0.4f, Stretch = 0.04f, Erode = new CelCurve(0.05f, 0.5f), Palette = "electric_bolt_spark", Core = 1f, Extra = new Vector4(0.15f, 0.05f, 0.3f, 0f),
            },
            new CelLayer
            {
                // a few longer ones, flung further (the clip's spray has a few dashes out past the rest)
                Name = "spray long", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = At(27f), Duration = At(110f) - At(27f), TimeJitter = 0.03f, Life = 0.22f, LifeJitter = 0.3f,
                Close = 90, Mid = 30, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.15f, 0f, 0f), Aim = CelAim.Radial, Speed = 9f, SpeedJitter = 0.4f, UpBias = 0.25f, Gravity = 3f, Drag = 1.6f,
                Size = new CelCurve(0.03f, 0.014f), SizeJitter = 0.3f, Stretch = 0.06f, Erode = new CelCurve(0.05f, 0.5f), Palette = "electric_bolt_spark", Core = 1f, Extra = new Vector4(0.15f, 0.05f, 0.2f, 0f),
            });

        /// <summary>A ground flare's sparks, played at the flare: white with a trace of orange.</summary>
        static CelFxRecipe FlareSparks(string name, float from, float to, int close, int warm)
        {
            return new CelFxRecipe(name, 4.5f,
                new CelLayer
                {
                    Name = "flare sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = At(from), Duration = At(to) - At(from), TimeJitter = 0.03f, Life = 0.2f, LifeJitter = 0.4f,
                    Close = close, Mid = close / 3, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.12f, 0f, 0f), Aim = CelAim.Radial, Speed = 4.5f, SpeedJitter = 0.5f, UpBias = 0.9f, Gravity = 6f, Drag = 1.5f,
                    Size = new CelCurve(0.02f, 0.01f), SizeJitter = 0.3f, Stretch = 0.05f, Erode = new CelCurve(0.05f, 0.5f), Palette = "electric_bolt_spark", Core = 1f, Extra = new Vector4(0.15f, 0.05f, 0.3f, 0f),
                },
                new CelLayer
                {
                    Name = "flare warm", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = At(from), Duration = At(to) - At(from), TimeJitter = 0.03f, Life = 0.3f, LifeJitter = 0.3f,
                    Close = warm, Mid = warm / 3, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.1f, 0f, 0f), Aim = CelAim.Radial, Speed = 3.5f, SpeedJitter = 0.5f, UpBias = 1.2f, Gravity = 7f, Drag = 1.2f,
                    Size = new CelCurve(0.016f, 0.008f), SizeJitter = 0.3f, Stretch = 0.05f, Erode = new CelCurve(0.05f, 0.5f), Palette = "spark", Core = 2f, Extra = new Vector4(0.2f, 0.05f, 0.2f, 0f),
                });
        }
        static readonly CelFxRecipe UpFlareSparks = FlareSparks("electric_bolt_flare_up", 30f, 121f, 70, 14);
        static readonly CelFxRecipe LowFlareSparks = FlareSparks("electric_bolt_flare_low", 72f, 116f, 36, 8);

        // ---------------------------------------------------------------- playing it
        /// <summary>
        /// Fire the stream: the bolts leave `from` (off-screen, 13 m before the board) and land on `aim` (the board, at
        /// the target's chest `target`, whose foot is on the ground at `ground`); `delay` holds the first launch back.
        /// Everything is spawned now (the light it throws on the ground too: the shader finds what it falls on in the
        /// camera's depth texture), nothing is left to do per frame. No allocation.
        /// </summary>
        public static void Play(Vector3 from, Vector3 aim, Vector3 target, Vector3 ground, uint seed, float delay = 0f)
        {
            var renderer = CelFx.Renderer;
            int kind = renderer.KindId(ElectricBoltKind.KindName);
            if (kind < 0) throw new System.InvalidOperationException("ElectricBolt kind is not registered");
            float lod = CelFx.LodBlend(CelFx.Zoom);
            bool mid = lod >= 0.5f, far = lod >= 1.5f;
            float quality = CelFx.QualityShare(FxQuality.Tier);
            float now = CelFx.Now + delay;
            var rng = new CelRng(seed);
            Vector3 along = aim - from;
            along.y = 0f;
            along = along.sqrMagnitude > 1e-6f ? along.normalized : Vector3.forward;
            Vector3 side = new Vector3(along.z, 0f, -along.x);
            Vector3 eye = Place(target, along, side, Eye);

            // ---- the light on the ground (under everything): round the foot from the first bolts' approach (frames
            // 14-26), gone with the last arc (frames 116-122); under the bolts' way while they fly; at the flares
            {
                Vector3 foot = Place(target, along, side, FootLight);
                foot.y = GroundAt(foot, ground.y) + 1f;
                // (its card is a metre over the ground: an up reach as long as its reach keeps most of it on the ground)
                Light(renderer, kind, now, At(14f), At(121f), foot, side, 3.6f, 3.6f, 1f, 0.75f, far ? 0.6f : 1f, Into, Add, 0.45f, 0.22f, true);
                if (!far)
                {
                    // the target itself, lit by the hit (not on level ground only)
                    Light(renderer, kind, now, At(25f), At(113f), Place(target, along, side, Blob) + (eye - target).normalized * 0.4f, side, 1.1f, 1.4f, 1f, 1f, 0.8f,
                        new Vector3(0.35f, 0.9f, 1.1f), new Vector3(0.03f, 0.07f, 0.09f), 0.06f, 0.15f, false);
                    Vector3 path = Place(target, along, side, PathLight);
                    path.y = GroundAt(path, path.y) + 1f;
                    Light(renderer, kind, now, At(11f), At(118f), path, along, 6.5f, 1.8f, 1f, 0.3f, 0.45f, Into, Add, 0.2f, 0.2f, true);
                }
            }

            // ---- the bolts: one every 2.2 frames, on two lanes, landing frames 23-116
            int bolts = Count(Bolts, far ? 0.2f : mid ? 0.55f : 1f, quality);
            for (int i = 0; i < bolts; i++)
            {
                float land = Mathf.Lerp(FirstLand, LastLand, bolts > 1 ? i / (float)(bolts - 1) : 0f) + rng.Range(-0.6f, 0.6f) * (i > 0 ? 1f : 0f);
                bool lane2 = rng.Next() < LaneShare;
                Vector3 start = from + rng.InSphere() * 0.14f - (lane2 ? Vector3.up * LaneDrop + side * 0.1f : Vector3.zero);
                Vector3 end = aim + rng.InSphere() * 0.06f;
                float length = rng.Jitter(BoltLength, 0.28f);
                float span = (end - start).magnitude;
                float share = length / Mathf.Max(span, 0.1f);
                float flight = span / Speed;
                float life = flight * (1f + share);
                var r = Rec(now, At(land) - flight, life, start, BoltRow, 1f, ref rng);
                r.Vel = (end - start) / life;          // the span is vel x life; the head runs at Speed
                float w = rng.Jitter(BoltWidth, 0.2f);
                r.Size = new Vector4(w, w, 1f, share);
                r.Motion = new Vector4(0f, 0f, 0f, ElectricBoltKind.Comet);
                r.Erode = new Vector4(0.04f, 0.04f, 1f, 0f);
                r.Tone.y = 1f; r.Tone.w = 1f;          // white core; the bolt stops at the board
                // the head runs from the launch (share 0) on past the board until its tail has gone in: from 0 to 1 + share
                r.Extra = new Vector4(rng.Range(0.6f, 1.1f), rng.Range(0.9f, 1.7f), 0f, 1f + share);
                renderer.Spawn(kind, r);
            }

            // ---- the hit: the lavender flash before the first contact (frames 23-25), then the white-hot blob, the lit
            // board and the steady glow through the volley (frames 26-112)
            Vector3 blob = Place(target, along, side, Blob) + (eye - target).normalized * 0.25f;
            if (!far)
            {
                Dot(renderer, kind, now, At(23.2f), 0.1f, blob, LavenderRow, ElectricBoltKind.Wash, new Vector3(0.78f, 0.78f, 1f), 0.15f, 0.72f, 0.85f, 0.9f, ref rng);
                Vector3 haze = Place(target, along, side, Haze);
                haze.y = GroundAt(haze, haze.y) + 0.05f;
                var hz = Rec(now, At(20f), At(26f) - At(20f), haze, LavenderRow, 0.15f, ref rng);
                hz.Size = new Vector4(1.3f, 1.5f, 1f, -0.001f);
                hz.Extra = new Vector4(ElectricBoltKind.Pool, 3.2f, 0f, 0f);
                hz.Erode = new Vector4(0f, 0.6f, 2f, 0f);
                renderer.Spawn(kind, hz);
            }
            Dot(renderer, kind, now, At(25.5f), At(112f) - At(25.5f), blob, BoardRow, ElectricBoltKind.Bloom, new Vector3(0.75f, 0.8f, 1f), 2.4f, 0f, 0f, 1f, ref rng);
            if (!far) Dot(renderer, kind, now, At(25.5f), At(112f) - At(25.5f), blob, BoardRow, ElectricBoltKind.Wash, new Vector3(0.78f, 0.78f, 1f), 0.15f, 0.72f, 0.85f, 0.25f, ref rng);
            {
                // the blob: a new white-hot bloom every frame or so, uneven in size (it flickers)
                int n = Count(90, far ? 0.15f : mid ? 0.5f : 1f, quality);
                float t0 = At(25.5f), t1 = At(112f);
                for (int i = 0; i < n; i++)
                {
                    float t = Mathf.Lerp(t0, t1, n > 1 ? i / (float)(n - 1) : 0f) + rng.Range(-0.02f, 0.02f);
                    float size = rng.Range(0.28f, 0.46f);
                    Dot(renderer, kind, now, Mathf.Max(t0, t), rng.Range(0.05f, 0.09f), blob + rng.InSphere() * 0.06f, HitRow, ElectricBoltKind.Bloom,
                        new Vector3(size, size * 1.05f, 1f), 2.6f, 0f, rng.Range(0.3f, 0.5f), 0.95f, ref rng);
                }
                // and thin white rays over it
                int m = far ? 0 : Count(40, mid ? 0.5f : 1f, quality);
                for (int i = 0; i < m; i++)
                {
                    float t = Mathf.Lerp(t0, t1, m > 1 ? i / (float)(m - 1) : 0f) + rng.Range(-0.03f, 0.03f);
                    float size = rng.Range(0.45f, 0.65f);
                    Dot(renderer, kind, now, Mathf.Max(t0, t), rng.Range(0.05f, 0.08f), blob, HitRow, ElectricBoltKind.Rays,
                        new Vector3(size, size, 1f), 0.025f, 9f, 0.1f, 0.6f, ref rng);
                }
            }

            // ---- the arcs: the bolts carry on through the target to a flare on the ground ~9 m behind it (frames
            // 26-121), and from frame 70 a second arc runs along the ground to the right (frames 70-117, a flash at 50)
            Vector3 upRoot = Place(target, along, side, UpRoot), upTip = OnGround(eye, Place(target, along, side, UpTip), ground.y);
            Vector3 lowRoot = Place(target, along, side, LowRoot), lowTip = OnGround(eye, Place(target, along, side, LowTip), ground.y);
            Arcs(renderer, kind, now, At(26f), At(121f), upRoot, upTip, eye, ground.y, Count(52, far ? 0.15f : mid ? 0.5f : 1f, quality), 0.085f, 0.42f, ref rng);
            if (!far)
            {
                Arcs(renderer, kind, now, At(70f), At(117f), lowRoot, lowTip, eye, ground.y, Count(28, mid ? 0.5f : 1f, quality), 0.08f, 0.38f, ref rng);
                Arcs(renderer, kind, now, At(49.5f), At(52f), lowRoot, lowTip, eye, ground.y, 2, 0.07f, 0.35f, ref rng);
            }

            // ---- the flares where the arcs touch the ground: a flickering white-hot star with sparks and a little light
            Flare(renderer, kind, now, At(30f), At(121f), upTip, Count(40, far ? 0.15f : mid ? 0.5f : 1f, quality), 0.26f, ref rng);
            if (!far) Flare(renderer, kind, now, At(72f), At(116f), lowTip, Count(22, mid ? 0.5f : 1f, quality), 0.24f, ref rng);
            if (!far)
            {
                Vector3 lu = upTip + Vector3.up * 0.6f, ll = lowTip + Vector3.up * 0.6f;
                Light(renderer, kind, now, At(30f), At(121f), lu, side, 1.4f, 1f, 1f, 1f, 0.7f, Into, Add, 0.1f, 0.1f, true);
                Light(renderer, kind, now, At(72f), At(116f), ll, side, 1.3f, 1f, 1f, 1f, 0.7f, Into, Add, 0.1f, 0.1f, true);
            }

            // ---- the sparks (kit Streaks): the hit's spray, the flares'
            Vector3 dirFwd = aim - from;
            PlayAt(Sparks, from, dirFwd, blob, seed + 1u, delay);
            PlayAt(UpFlareSparks, from, dirFwd, upTip, seed + 2u, delay);
            if (!far) PlayAt(LowFlareSparks, from, dirFwd, lowTip, seed + 3u, delay);
        }

        /// <summary>A kit recipe played `delay` seconds late (its layers' Start are moved by it).</summary>
        static void PlayAt(CelFxRecipe recipe, Vector3 origin, Vector3 dir, Vector3 target, uint seed, float delay)
        {
            if (delay <= 0f) { CelFx.Play(recipe, origin, dir, target, seed); return; }
            float keep = CelFx.Clock;
            CelFx.Clock = CelFx.Now + delay;
            try { CelFx.Play(recipe, origin, dir, target, seed); }
            finally { CelFx.Clock = keep; }
        }

        /// <summary>A layer's count at a LOD share and the quality's (never more going out; at least one when the share
        /// is above zero).</summary>
        static int Count(int close, float share, float quality) => share <= 0f ? 0 : Mathf.Max(1, Mathf.FloorToInt(close * share * Mathf.Max(0f, quality) + 0.5f));

        /// <summary>A point given in the bolts' frame (along, side, up from the target's chest).</summary>
        static Vector3 Place(Vector3 target, Vector3 along, Vector3 side, Vector3 at) => target + along * at.x + side * at.y + Vector3.up * at.z;

        /// <summary>Lightning arcs from root to tip, re-drawn: `n` overlapping records 0.1-0.28 s long, the tip moved a
        /// little each time (and put back on the ground), each shooting out from the root over its first fifth.</summary>
        static void Arcs(CelFxRenderer renderer, int kind, float now, float t0, float t1, Vector3 root, Vector3 tip, Vector3 eye, float groundY, int n, float width, float kinks, ref CelRng rng)
        {
            for (int i = 0; i < n; i++)
            {
                float life = rng.Range(0.1f, 0.28f);
                float born = Mathf.Lerp(t0, Mathf.Max(t0, t1 - life), n > 1 ? i / (float)(n - 1) : 0f) + rng.Range(-0.03f, 0.03f);
                born = Mathf.Max(t0, born);
                Vector3 a = root + rng.InSphere() * 0.06f;
                Vector3 b = tip + new Vector3(rng.Range(-0.45f, 0.45f), 0f, rng.Range(-0.45f, 0.45f));
                b = OnGround(eye, b + Vector3.up * 0.3f, groundY);
                var r = Rec(now, born, life, a, ArcRow, 1f, ref rng);
                r.Vel = (b - a) / life;
                float w = rng.Jitter(width, 0.25f);
                r.Size = new Vector4(w, w * 0.8f, 1f, i < 2 ? 0.45f : 0.2f);     // the first ones take longer to reach their tip
                r.Motion = new Vector4(0f, 0f, 0f, ElectricBoltKind.Arc);
                r.Erode = new Vector4(0.04f, 0.1f, 2f, 0f);
                r.Tone.y = 1f; r.Tone.w = 1f;
                r.Extra = new Vector4(rng.Jitter(kinks, 0.3f), 1.2f, Mathf.Round(rng.Range(4f, 7f)), 24f);
                renderer.Spawn(kind, r);
            }
        }

        /// <summary>A ground flare: a flickering white-hot bloom with a small ray star, `n` records over t0..t1.</summary>
        static void Flare(CelFxRenderer renderer, int kind, float now, float t0, float t1, Vector3 at, int n, float size, ref CelRng rng)
        {
            Vector3 p = at + Vector3.up * 0.12f;
            for (int i = 0; i < n; i++)
            {
                float t = Mathf.Lerp(t0, t1, n > 1 ? i / (float)(n - 1) : 0f) + rng.Range(-0.02f, 0.02f);
                float s = rng.Jitter(size, 0.3f);
                Dot(renderer, kind, now, Mathf.Max(t0, t), rng.Range(0.06f, 0.11f), p + rng.InSphere() * 0.05f, HitRow, ElectricBoltKind.Bloom,
                    new Vector3(s, s * 0.9f, 1f), 2.8f, 0f, 0.35f, 0.9f, ref rng);
                if ((i & 1) == 0)
                    Dot(renderer, kind, now, Mathf.Max(t0, t), rng.Range(0.05f, 0.08f), p, HitRow, ElectricBoltKind.Rays,
                        new Vector3(s * 1.6f, s * 1.6f, 1f), 0.03f, 7f, 0.12f, 0.55f, ref rng);
            }
        }

        /// <summary>A soft dot that stays put (a bloom, a wash, a ray star).</summary>
        static void Dot(CelFxRenderer renderer, int kind, float now, float at, float life, Vector3 pos, int palette, float mode, Vector3 size,
            float y, float z, float w, float opacity, ref CelRng rng)
        {
            var r = Rec(now, at, life, pos, palette, opacity, ref rng);
            r.Size = new Vector4(size.x, size.y, size.z > 0f ? size.z : 1f, -0.001f);
            r.Extra = new Vector4(mode, y, z, w);
            r.Erode = new Vector4(0f, 0.3f, 2f, 0f);
            renderer.Spawn(kind, r);
        }

        /// <summary>A light on what stands behind its card: `along` m along `axis`, `up` m up and down, `farShare` of the
        /// near reach on the far side, `nearShare` x along square to the axis; Into is multiplied in at its heart, Add
        /// added on top; up over `rise` s, down over `fall` s at the end.</summary>
        static void Light(CelFxRenderer renderer, int kind, float now, float born, float gone, Vector3 at, Vector3 axis,
            float along, float up, float farShare, float nearShare, float strength, Vector3 into, Vector3 add, float rise, float fall, bool level)
        {
            var own = new CelRng(0x5EEDu);   // a light has nothing random in it: the effect's own draws stay as they are
            var r = Rec(now, born, gone - born, at, 0, strength, ref own);
            r.Axis = axis;
            r.Vel = into;
            r.Size = new Vector4(along, up, farShare, -1f);
            r.Motion = new Vector4(nearShare, 0f, 0f, 0f);
            r.Erode = new Vector4(add.x, add.y, add.z, 0f);
            r.Extra = new Vector4(ElectricBoltKind.Light, rise, fall, 0f);
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
        /// <summary>Filmed with t0 = 0.35 (frame 8.4 = FirstFrame): At() already puts the first launch on frame 8.56.</summary>
        const float Delay = 0f;

        /// <summary>The Range shot: the bolts' line from the 3D fit, held in the shot's own frame (metres from the
        /// target's chest along the Range's level fire direction, to its side, up), so the mid and far stills keep it.</summary>
        [CelRangeEffect("electric_bolt")]
        static void Range(RangeShot s)
        {
            CelFx.ZoomOverride = s.Lod == RangeLod.Close ? CelFx.CloseZoom : s.Zoom;
            Vector3 along = s.Direction;
            along.y = 0f;
            along = along.sqrMagnitude > 1e-6f ? along.normalized : Vector3.forward;
            Vector3 side = new Vector3(along.z, 0f, -along.x);
            Vector3 aim = s.Target + along * -0.143f + side * -0.055f + Vector3.up * -0.284f;
            Vector3 from = s.Target + along * -12.892f + side * -3.113f + Vector3.up * -1.183f;
            Lens(Vignette, VignetteSmooth);
            Play(from, aim, s.Target, s.Ground, (uint)s.Seed, Delay);
        }
    }
}
