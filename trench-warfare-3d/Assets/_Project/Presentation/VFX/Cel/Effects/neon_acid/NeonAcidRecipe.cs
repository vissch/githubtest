// Phase: CelFx neon acid (2026-11-14) - a jet of neon acid arcs onto a target, heaps over it, pools round its foot,
// bubbles and leaves green smoke (reference clip neon_acid_nolora, 8 s at 24 fps, drawn on twos).
// Every number below was read off the clip: the arc and its speed from a 3D fit to the beach Range's camera
// (the head crosses the frame in six frames), the times from its frames (the comments name them, the effect
// starting on frame 12), the sizes from rays cast on the ground at the dummy's foot, the colours from its pixels
// taken back through the night grade (a film of flat swatches gave the grade).
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class NeonAcidRecipe
    {
        /// <summary>Seconds the nozzle is open, seconds from the nozzle to the target, and the fall of the arc
        /// (the clip draws a cartoon arc: 42 m/s and nine times gravity).</summary>
        public const float JetSeconds = 2.975f, Flight = 0.3634f, Gravity = 90.92f;
        /// <summary>The band's half-width at the nozzle, through the middle of the flight and at the target (m): a
        /// point at the nozzle that opens at once, holds one width over the first half of the flight and opens
        /// again as it comes down (the clip: 0.6 m across from where it enters the frame to half way, 1.4 m where
        /// it lands). The lanes beside it are placed in these units.</summary>
        public const float HalfNozzle = 0.03f, HalfMiddle = 0.30f, HalfTarget = 0.75f;
        /// <summary>The thin ribbon that flies under the band keeps this far under its middle (m, the same three,
        /// over its place of -1.9): clear of the band at first, inside it where it lands.</summary>
        const float RibbonNozzle = 0.20f, RibbonMiddle = 0.26f, RibbonTarget = 0.32f;
        /// <summary>Metres the jet is lifted before it comes down: the clip's arc is fuller than a parabola there.</summary>
        public const float Lift = 0.16f;
        /// <summary>The puddle's half-extent across (the card; the longest fingers reach its edge: 4.5 m to the side
        /// the jet came from) and its depth over its width (the clip: a body 4.6 m across and 2.8 m deep, round
        /// the target's foot; the Range's ground falls toward the eye there, which draws it out on the screen).</summary>
        public const float PuddleReach = 5.0f, PuddleDepth = 0.61f;
        /// <summary>The goo heaped over the target: the card's width (m; a pillar 1.5 m across that stands 2.4 m,
        /// its tongues to 3.6 m) and its height over its width.</summary>
        public const float MoundWidth = 4.2f, MoundTall = 1.1f;

        // A colour as the shader has to write it (the project is gamma, the palette stores .linear). The values are
        // the clip's colours taken back through the Range's night grade; the comments give what they show as.
        static Color S(float r, float g, float b) => new Color(Mathf.LinearToGammaSpace(r), Mathf.LinearToGammaSpace(g), Mathf.LinearToGammaSpace(b), 1f);

        // the heap and the puddle, each tone as the film shows it (the clip's, clustered; the stored values make up
        // for what a film of the first set showed): ripples #4E8A30, a tone down #74D93D, body #8EF240, light
        // #A6F64F, pale streaks #C4F968 (the palest, #E8FC86, is mixed from it in the shader)
        static readonly int Acid = CelPalette.Add("neon_acid",
            S(0.248f, 0.429f, 0.192f), S(0.311f, 0.685f, 0.229f), S(0.391f, 0.828f, 0.243f), S(0.462f, 0.857f, 0.285f), S(0.573f, 0.883f, 0.370f), 1.1f, 4, S(0.173f, 0.328f, 0.176f));
        // the jet: a tone down #7EDE33, body #9CF648, its light streaks #B6F95A and #CFFA6E
        static readonly int Jet = CelPalette.Add("neon_acid_jet",
            S(0.311f, 0.551f, 0.242f), S(0.393f, 0.731f, 0.221f), S(0.432f, 0.897f, 0.271f), S(0.512f, 0.915f, 0.315f), S(0.591f, 0.910f, 0.369f), 1.1f, 4, S(0.173f, 0.328f, 0.176f));
        // a bubble: rim #35642B, shade #659D41, body #94F741, light #B2F95C, glint #EBFFD7
        static readonly int Bubble = CelPalette.Add("neon_acid_bubble",
            S(0.173f, 0.328f, 0.176f), S(0.312f, 0.481f, 0.236f), S(0.418f, 0.875f, 0.260f), S(0.487f, 0.893f, 0.324f), S(0.605f, 0.951f, 0.713f), 1.1f, 4, S(0.173f, 0.328f, 0.176f));
        // the smoke, seen through (a puff is FumeOpacity; where two lie over one another, 0.9 over the night ground):
        // thin #3E6937, a tone down #4D7F3F, body #629F4E, light #6FBB4C, lit by the acid under it #7ED351
        static readonly int Fume = CelPalette.Add("neon_acid_fume",
            S(0.215f, 0.363f, 0.208f), S(0.263f, 0.428f, 0.232f), S(0.325f, 0.526f, 0.276f), S(0.372f, 0.633f, 0.276f), S(0.425f, 0.740f, 0.295f), 1.1f, 4, S(0.173f, 0.328f, 0.176f));
        const float FumeOpacity = 0.78f;

        /// <summary>The light the acid throws on the ground and on what stands there. The clip's lit ground is three
        /// and a half times its unlit ground in green and keeps the ground's own pattern: most of the light is
        /// multiplied into what it falls on (GlowInto: lit = unlit x (1 + GlowInto) at its heart, as much as one
        /// blend can), the rest is added on top (GlowAdd, as written, before the grade). Pools of it, laid out by
        /// the jet's own track so they hold from any view, fitted to the clip's ground over the Range's own in
        /// 64 px cells (a mean error of 4 levels): one under the first third of the jet and one at the target's
        /// foot while the jet is on (a third pool was tried and measured no better), a smaller one at the foot
        /// after it.</summary>
        static readonly Vector3 GlowInto = new Vector3(0.87f, 1.136f, 0.18f), GlowAdd = new Vector3(0.117f, 0.234f, 0.045f);
        /// <summary>The pool under the jet: the share of the flight it lies under, metres to the left of the track,
        /// its reach across the track and back toward the nozzle (m), its reach on toward the target over that,
        /// and how much more than the others it adds.</summary>
        const float UnderShare = 0.292f, UnderAside = 2.75f, UnderAcross = 6.95f, UnderBehind = 6.42f, UnderAhead = 1f, UnderAdds = 1.7f;
        /// <summary>The pool at the foot while the jet lands: metres back along the track from the target and to
        /// the left of it, its strength, its reach (m), and its reach on past the target over that.</summary>
        const float FootBehind = 0.95f, FootAside = -0.36f, FootStrength = 0.65f, FootReach = 7.12f, FootAhead = 0.77f;
        /// <summary>The pool the puddle leaves at the foot after the jet: the same (fitted on frames 92-96).</summary>
        const float AfterBehind = 0.85f, AfterAside = -1.25f, AfterStrength = 0.66f, AfterReach = 5.25f, AfterAhead = 0.97f;
        /// <summary>The target itself while the goo is on it: its own colours turned the acid's green (the clip's
        /// chest goes from grey 0.17 to (0.43, 0.80, 0.26) as written), its rings still showing.</summary>
        static readonly Vector3 CoatInto = new Vector3(0.30f, 1f, 0f), CoatAdd = new Vector3(0.10f, 0.33f, 0.05f);
        const float CoatReach = 1.8f;

        // a strand of the jet: its place across it (up the screen, toward the eye; 1 is the band's edge), its
        // half-width at the nozzle and at the target (m), how much of its length is drawn, the seconds one stretch
        // (the band: one slit) takes, and what it is (0 a lane, 1 a lane with light streaks, 2 the band).
        // A thin ribbon that flies under the band, the stray dashes alongside, the two lanes that roughen the
        // band's edges, the band.
        const int StrandStride = 7;
        static readonly float[] Strands =
        {
            -1.90f,  0.2f, 0.022f, 0.050f, 0.55f, 0.30f,  0f,
            -1.30f,  0.3f, 0.020f, 0.040f, 0.16f, 0.12f,  0f,
             1.28f, -0.2f, 0.020f, 0.038f, 0.14f, 0.11f,  0f,
            -1.10f, -0.5f, 0.026f, 0.050f, 0.22f, 0.15f,  1f,
            -0.85f,  0.1f, 0.030f, 0.075f, 0.50f, 0.21f,  0f,
             0.85f, -0.1f, 0.030f, 0.075f, 0.45f, 0.19f,  0f,
             0.00f,  0.0f, HalfNozzle, HalfTarget, 1f, 0.085f, 2f,
        };

        // a bubble on the ground: metres to the eye's right of the foot, metres toward the eye (on the Range's own
        // ground, which falls toward the eye: set where a film of them showed the clip's stand), the radius it swells
        // to, and the clip's frames it lives over
        const int BubbleStride = 5;
        static readonly float[] Bubbles =
        {
             2.19f, -0.45f, 0.24f, 42f, 54f,
             1.81f,  0.45f, 0.23f, 42f, 54f,
            -3.74f,  0.65f, 0.37f, 46f, 61f,
            -1.30f,  1.92f, 0.23f, 44f, 62f,
             2.83f,  0.11f, 0.19f, 50f, 59f,
            -3.24f,  0.42f, 0.26f, 62f, 74f,
             2.56f,  0.56f, 0.23f, 62f, 78f,
            -1.46f,  3.00f, 0.46f, 66f, 82f,
            -0.91f,  1.68f, 0.27f, 95f, 111f,
        };

        // the smoke that stands close round the target as the heap comes down: right, toward the eye, up, size (m)
        const int BurstStride = 4;
        static readonly float[] Burst =
        {
            -1.40f, -0.20f, 1.30f, 0.85f,
            -0.90f,  0.30f, 0.55f, 0.75f,
            -1.95f,  0.10f, 0.80f, 0.70f,
             1.55f, -0.20f, 1.25f, 0.90f,
             1.20f,  0.40f, 0.45f, 0.80f,
            -1.10f,  0.95f, 0.35f, 0.65f,
            -0.30f, -0.60f, 1.60f, 0.70f,
        };

        /// <summary>The clip's effect starts on its frame 12 and its nozzle opens ClipDelay seconds after that. The
        /// frames named below are the clip's: AtFrame gives the seconds after the call at which one falls, `late`
        /// being how much later than the clip's this play's nozzle opens (delay - ClipDelay).</summary>
        const float FirstFrame = 12f, ClipDelay = 0.039f;
        static float AtFrame(float late, float frame) => (frame - FirstFrame) / 24f - 0.004f + late;
        /// <summary>A time moved onto the drawing it falls in (the effect is drawn on twos), a hair early so the
        /// record is alive on that drawing's first frame.</summary>
        static float OnTwos(float seconds) => Mathf.Round(seconds * 12f) / 12f - 0.004f;

        /// <summary>The launch velocity that carries the jet from the nozzle to the target in Flight seconds.</summary>
        public static Vector3 Launch(Vector3 nozzle, Vector3 target) => (target - nozzle) / Flight + Vector3.up * (0.5f * Gravity * Flight);

        /// <summary>Where the fluid that left the nozzle s seconds ago is.</summary>
        public static Vector3 Along(Vector3 nozzle, Vector3 target, float s) => nozzle + Launch(nozzle, target) * s + Vector3.down * (0.5f * Gravity * s * s);

        static readonly int LensId = Shader.PropertyToID("_TWAcidLens");

        // the lens handed over is a global of the shader: a session that ends puts it back to none
        static NeonAcidRecipe() { SceneStatics.Register(nameof(NeonAcidRecipe), () => Shader.SetGlobalVector(LensId, Vector4.zero)); }

        /// <summary>
        /// The scene's lens vignette (URP Vignette's intensity and smoothness; 0 for none), for whoever shows the
        /// effect to hand over: the acid is written brighter where the vignette will darken it, so it shows the
        /// clip's colours all the way into the corners of the frame (as far as white allows).
        /// </summary>
        public static void Lens(float intensity, float smoothness) =>
            Shader.SetGlobalVector(LensId, new Vector4(Mathf.Max(0f, intensity) * 3f, Mathf.Max(0.01f, smoothness) * 5f, 0f, 0f));

        /// <summary>
        /// Play the jet: from the nozzle onto the target (its chest), whose foot stands on the ground at `ground`;
        /// `right` is the eye's level right (the puddle and the bubbles are laid out by it). `delay` holds
        /// the nozzle shut for that long after the call. Everything is spawned now, the light it throws on the
        /// ground too (the shader finds what it falls on in the camera's depth texture): nothing is left to do per
        /// frame. No allocation.
        /// </summary>
        public static void Play(Vector3 nozzle, Vector3 target, Vector3 ground, Vector3 right, uint seed, float delay = 0f)
        {
            right.y = 0f;
            right = right.sqrMagnitude > 0.001f ? right.normalized : Vector3.right;
            Vector3 away = Vector3.Cross(right, Vector3.up);   // from the eye
            var renderer = CelFx.Renderer;
            int kind = renderer.KindId(NeonAcidKind.KindName);
            if (kind < 0) throw new System.InvalidOperationException("NeonAcid kind is not registered");
            float lod = CelFx.LodBlend(CelFx.Zoom);
            bool mid = lod >= 0.5f, far = lod >= 1.5f;
            float now = CelFx.Now;
            var rng = new CelRng(seed);
            Vector3 vel = Launch(nozzle, target);
            Vector3 dir = new Vector3(vel.x, 0f, vel.z).normalized;
            Vector3 across = Vector3.Cross(Vector3.up, dir);
            float late = delay - ClipDelay;      // the clip's frames, moved with this play's nozzle
            float hit = delay + Flight;          // the head lands (frame 22)
            float shut = delay + JetSeconds;     // the nozzle closes (frame 84.7)
            float dry = shut + Flight;           // the last of it lands (frame 93.4)
            ground.y = Ground(ground);
            Vector3 stand = ground - away * 0.3f;   // the goo stands on the target's foot (and is drawn in front of the target)
            stand.y = Ground(stand) + 0.03f;

            // ---- the light (under everything). Under the jet: on as the head goes over (frames 14-16), off behind
            // the tail (frames 86-88).
            {
                float over = Flight * UnderShare;
                Vector3 under = Vector3.Lerp(nozzle, target, UnderShare) - across * UnderAside;
                under.y = Ground(under) + 1f;
                // (from further off it reads as a patch of light away from the jet: less of it there)
                Glow(renderer, kind, now, delay + over - 0.08f, shut + over + 0.02f, under, across, UnderAcross, UnderAcross, UnderBehind, UnderAhead,
                    far ? 0.4f : mid ? 0.6f : 1f, GlowInto, GlowAdd * UnderAdds, 0.10f, 0.12f, false, seed);
                // At the foot, on the ground only (the clip's target is its own colours whenever the goo is off
                // it): up as the goo heaps there (frames 22-25; the ground at the foot is dark until the head has
                // landed), gone as the last of the jet lands (frames 86-90).
                Vector3 foot = ground - dir * FootBehind - across * FootAside;
                foot.y = Ground(foot) + 1f;
                Glow(renderer, kind, now, AtFrame(late, 22f), AtFrame(late, 90.5f), foot, across, FootReach, FootReach, FootReach, FootAhead, FootStrength, GlowInto, GlowAdd, 0.125f, 0.19f, true, seed);
                // What the puddle leaves on the ground after that, a smaller pool: it holds until frame 98 and is
                // gone as the last of the puddle dries (frame 128).
                Vector3 after = ground - dir * AfterBehind - across * AfterAside;
                after.y = Ground(after) + 1f;
                Glow(renderer, kind, now, AtFrame(late, 86f), AtFrame(late, 128f), after, across, AfterReach, AfterReach, AfterReach, AfterAhead, AfterStrength, GlowInto, GlowAdd, 0.19f, 1.25f, true, seed);
                // The target: green as the jet comes at it (the clip's chest: a little on frame 16, most of it on
                // frame 18, all of it on frame 20), and while the goo is on it: until the last of the heap has
                // run off it (the clip's target is still green on frame 86 and its own colours on frame 90).
                if (!far) Glow(renderer, kind, now, AtFrame(late, 15f), AtFrame(late, 88.5f), target, across, CoatReach, CoatReach, CoatReach, 1f, 1f, CoatInto, CoatAdd, 0.21f, 0.15f, false, seed);
            }

            // ---- the puddle (under everything else), round the target's foot: out by frame 44, a star of spikes for
            // two drawings as the heap comes down (frames 80-83), a small pool by frame 86, dry by frame 110
            {
                float born = OnTwos(hit + 0.25f);                        // frame 28
                float gone = OnTwos(dry + 0.69f);                        // frame 110
                var r = Rec(now, born, gone - born, NeonAcidKind.Puddle, ground, Acid, 1f, ref rng);
                r.Axis = right;
                r.Size = new Vector4(PuddleReach, PuddleReach, 1f, PuddleDepth);
                r.Motion = Slope(ground);
                r.Motion.z = Vector3.Dot(dir, right) > 0f ? -1f : 1f;   // its longest fingers run back to the side the jet came from
                r.Extra.y = 0.67f;
                r.Extra.z = OnTwos(shut - 0.17f) - born;                 // frame 80
                renderer.Spawn(kind, r);
            }

            // ---- the goo heaped over the target (frames 26-85), and the last splash at its foot (frames 85-93)
            if (!far)
            {
                float born = OnTwos(hit + 0.1667f);                      // frame 26
                float gone = AtFrame(late, 87f);                         // gone as the last of the jet comes in
                var r = Rec(now, born, gone - born, NeonAcidKind.Mound, stand, Acid, 1f, ref rng);
                r.Axis = dir;
                r.Size = new Vector4(MoundWidth, MoundWidth, 1f, MoundTall);
                r.Motion = new Vector4(0.4f, 1f, 0.6f, 0f);              // at its full height at once (a broad mass at first)
                r.Extra.z = 0.33f;                                       // it thins to strands from frame 79
                renderer.Spawn(kind, r);

                var last = Rec(now, AtFrame(late, 85f), 0.354f, NeonAcidKind.Mound, stand, Acid, 1f, ref rng);
                last.Axis = dir;
                last.Size = new Vector4(3.4f, 3.4f, 1f, 0.87f);
                last.Motion = new Vector4(0.125f, 0.4f, 0.6f, 0f);       // up on frame 88
                last.Extra.y = 1f;
                last.Extra.z = 0.21f;                                    // down over frames 88-93, hot at its heart
                renderer.Spawn(kind, last);
            }

            // ---- the jet's strands
            int strands = Strands.Length / StrandStride;
            int first = far ? strands - 1 : mid ? strands - 3 : 0;
            for (int i = first; i < strands; i++)
            {
                int o = i * StrandStride;
                bool band = Strands[o + 6] > 1.5f;
                var r = Rec(now, delay, JetSeconds + Flight + 0.05f, NeonAcidKind.Strand, nozzle, Jet, 1f, ref rng);
                r.Vel = vel;
                r.Motion = new Vector4(Gravity, rng.Range(0.015f, 0.035f), rng.Range(0.03f, 0.10f), Lift);   // the head's and the tail's taper
                bool ribbon = Strands[o] < -1.5f;
                r.Size = new Vector4(Strands[o + 2], Strands[o + 3], band ? HalfMiddle : (Strands[o + 2] + Strands[o + 3]) * 0.5f, Flight);
                r.Tone.w = ribbon ? RibbonMiddle : HalfMiddle;
                r.Axis = new Vector3(Strands[o], Strands[o + 1], rng.Range(0f, 6.2832f));
                r.Erode = new Vector4(ribbon ? RibbonNozzle : HalfNozzle, ribbon ? RibbonTarget : HalfTarget, Strands[o + 4], Strands[o + 5]);
                r.Extra.y = JetSeconds;
                r.Extra.z = Strands[o + 6];
                renderer.Spawn(kind, r);
            }

            // ---- the first hit: a star of spikes for one drawing (frames 24-25)
            if (!far)
            {
                var r = Rec(now, OnTwos(hit + 0.0833f), 0.08f, NeonAcidKind.Splat, target + Vector3.up * 0.25f, Acid, 1f, ref rng);
                r.Size = new Vector4(2.2f, 2.2f, 1f, 1f);
                r.Extra.y = 0.8f;
                renderer.Spawn(kind, r);
            }

            // ---- drops shed along the jet: leaves half a metre to a metre long that fly with it, most of them
            // just under it
            int shed = far ? 0 : mid ? 32 : 80;
            for (int i = 0; i < shed; i++)
            {
                bool early = i < shed * 3 / 10;                              // three in ten of them round its head, and bigger
                float at = delay + (early ? rng.Range(0f, 0.6f) : rng.Range(0f, JetSeconds - 0.02f));
                float rad = rng.Range(0.030f, 0.055f) * (early ? 1.3f : 1f);
                var r = Rec(now, at, Flight * rng.Range(0.8f, 1.1f), NeonAcidKind.Drop, nozzle, Jet, 1f, ref rng);
                r.Vel = vel + across * rng.Range(-1.4f, 1.4f) + Vector3.up * rng.Range(-4.5f, 1.5f) + dir * rng.Range(-4f, 1f);
                r.Motion = new Vector4(Gravity, 0f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.8f, 1f, rng.Range(0.007f, 0.014f));
                r.Extra.y = 2f;
                renderer.Spawn(kind, r);
            }

            // ---- the splash where it lands: thrown on past the target and up, all the while the jet lands
            int splash = far ? 0 : mid ? 36 : 130;
            for (int i = 0; i < splash; i++)
            {
                bool firstSpray = i < 40;
                float at = firstSpray ? OnTwos(hit + 0.0833f) + rng.Range(0f, 0.17f) : rng.Range(hit + 0.17f, dry - 0.05f);
                Vector3 d = rng.OnSphere();
                d.y = Mathf.Abs(d.y) * 0.9f + 0.35f;
                d = (d + dir * (firstSpray ? 0.55f : 0.25f)).normalized;
                float rad = rng.Range(0.032f, 0.075f);
                var r = Rec(now, at, rng.Range(0.35f, 0.65f), NeonAcidKind.Drop,
                    target + Vector3.up * rng.Range(-0.5f, 0.9f) + across * rng.Range(-0.4f, 0.4f) - away * 0.9f, Acid, 1f, ref rng);
                r.Vel = d * (firstSpray ? rng.Range(6f, 12.5f) : rng.Range(6f, 13f));
                r.Motion = new Vector4(22f, 0.4f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.55f, 1f, rng.Range(0.010f, 0.016f));   // short leaves and beads
                r.Extra.y = i % 3 == 0 ? 1f : 2f;
                renderer.Spawn(kind, r);
            }

            // ---- the spray as the heap comes down (frames 79-88): leaves and beads thrown out all round the target
            int spray = far ? 0 : mid ? 12 : 44;
            for (int i = 0; i < spray; i++)
            {
                Vector3 d = rng.OnSphere();
                d.y = Mathf.Abs(d.y) * 0.8f + 0.15f;
                float rad = rng.Range(0.020f, 0.045f);
                var r = Rec(now, AtFrame(late, 79f) + rng.Range(0f, 0.36f), rng.Range(0.30f, 0.5f), NeonAcidKind.Drop,
                    target + Vector3.up * rng.Range(-0.8f, 0.6f) - away * 0.9f, Acid, 1f, ref rng);
                r.Vel = d.normalized * rng.Range(4f, 10f);
                r.Motion = new Vector4(22f, 0.4f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.55f, 1f, rng.Range(0.010f, 0.016f));
                r.Extra.y = 2f;
                renderer.Spawn(kind, r);
            }

            // ---- bubbles on the ground round the puddle
            if (!mid)
                for (int i = 0; i < Bubbles.Length; i += BubbleStride)
                {
                    Vector3 at = ground + right * Bubbles[i] - away * Bubbles[i + 1];
                    at.y = Ground(at);
                    float born = AtFrame(late, Bubbles[i + 3]), gone = AtFrame(late, Bubbles[i + 4] + 2f);
                    var r = Rec(now, born, gone - born, NeonAcidKind.Bubble, at, Bubble, 1f, ref rng);
                    r.Size = new Vector4(Bubbles[i + 2], Bubbles[i + 2], 1f, 1f);
                    r.Extra.y = 0.085f;                                  // the pop: one drawing
                    renderer.Spawn(kind, r);
                }

            // ---- the smoke: soft puffs that lie over one another, lit from below in their first moments. A column
            // off the heap's top while the jet lands (frames 36-88): wisps at first, a thick one by the end ...
            Vector3 top = ground + Vector3.up * 2.8f;
            int column = far ? 3 : mid ? 9 : 18;
            for (int i = 0; i < column; i++)
            {
                float at = OnTwos(Mathf.Lerp(1.0f, shut + 0.2f, i / (float)(column - 1)));
                float grown = Mathf.Clamp01((at - 1.0f) / 2.0f);
                float size = Mathf.Lerp(0.24f, 0.60f, grown) * rng.Range(0.85f, 1.15f);
                var r = Rec(now, at, rng.Range(1.2f, 1.6f), NeonAcidKind.Puff,
                    top + right * rng.Range(-0.25f, 0.3f) + Vector3.up * rng.Range(-0.15f, 0.2f) - away * 0.5f, Fume, FumeOpacity * 0.8f, ref rng);
                r.Vel = Vector3.up * rng.Range(1.5f, 2.3f) + right * rng.Range(0.1f, 0.5f);
                r.Motion = new Vector4(0f, 0.5f, 0f, 0f);
                r.Size = new Vector4(size, size * 2.0f, 0.8f, 1f);
                r.Extra.y = 0.45f;
                r.Extra.z = 0.10f;
                renderer.Spawn(kind, r);
            }
            // ... big ones at its sides, low and a tone down (frames 56-84) ...
            int sides = mid ? 0 : 6;
            for (int i = 0; i < sides; i++)
            {
                float sgn = (i & 1) == 0 ? -1f : 1f;
                float at = OnTwos(rng.Range(1.8f, shut));
                var r = Rec(now, at, rng.Range(0.9f, 1.3f), NeonAcidKind.Puff,
                    ground + right * sgn * rng.Range(0.9f, 1.6f) + Vector3.up * rng.Range(0.5f, 1.5f) - away * 0.6f, Fume, FumeOpacity, ref rng);
                r.Vel = right * sgn * rng.Range(0.4f, 0.8f) + Vector3.up * rng.Range(0.3f, 0.7f);
                r.Motion = new Vector4(0f, 0.8f, 0f, 0f);
                r.Size = new Vector4(0.48f, 0.95f, 0.8f, 1f);
                r.Extra.y = 0.4f;
                r.Extra.z = 0.2f;
                renderer.Spawn(kind, r);
            }
            // ... banks of it close round the target as the heap comes down (frames 83-134), each a big puff and a
            // smaller one grown into it: lit all their life, they stand where they are and fray away ...
            if (!far)
                for (int i = 0; i < Burst.Length * 2; i += BurstStride)
                {
                    int o = i % Burst.Length;
                    bool second = i >= Burst.Length;
                    Vector3 off = right * Burst[o] - away * Burst[o + 1];
                    float size = Burst[o + 3] * (second ? 0.75f : 1.15f);
                    Vector3 nudge = second ? (right * rng.Range(-0.55f, 0.55f) + Vector3.up * rng.Range(-0.25f, 0.5f)) * Burst[o + 3] : Vector3.zero;
                    var r = Rec(now, AtFrame(late, 83f) + rng.Range(0f, 0.25f), rng.Range(2.0f, 2.4f), NeonAcidKind.Puff,
                        ground + off + Vector3.up * Burst[o + 2] + nudge, Fume, 0.85f, ref rng);
                    r.Vel = off.normalized * 0.12f + Vector3.up * 0.10f;
                    r.Motion = new Vector4(0f, 0.8f, 0f, 0f);
                    r.Size = new Vector4(size * 0.6f, size * 1.2f, 0.6f, 1f);
                    r.Extra.y = 0.55f;
                    r.Extra.z = 0.9f;
                    renderer.Spawn(kind, r);
                }
            // ... and the plume that stands over the target after it (frames 86-150): it leaves the target's head,
            // then its far shoulder as its source drifts off to the right and sinks, a little less of it all the
            // while; whatever is left of it goes together by frame 156
            int plume = far ? 2 : mid ? 8 : 16;
            float end = AtFrame(late, 156f);
            for (int i = 0; i < plume; i++)
            {
                float u = i / (float)(plume - 1);
                float at = OnTwos(Mathf.Lerp(dry - 0.3f, AtFrame(late, 138f), u));
                float less = 1f - 0.5f * u;
                var r = Rec(now, at, Mathf.Min(rng.Range(2.2f, 2.6f), end - at), NeonAcidKind.Puff,
                    ground + Vector3.up * (Mathf.Lerp(2.4f, 1.5f, u) + rng.Range(-0.2f, 0.2f)) + right * (0.8f * u + rng.Range(-0.3f, 0.3f)) - away * 0.4f,
                    Fume, FumeOpacity * (0.6f + 0.4f * less), ref rng);
                r.Vel = Vector3.up * rng.Range(1.0f, 1.4f) + right * rng.Range(-0.15f, 0.15f);
                r.Motion = new Vector4(0f, 0.35f, 0f, 0f);
                r.Size = new Vector4(0.80f * less, 1.45f * less, 0.8f, 1f);
                r.Extra.y = 0.35f;
                r.Extra.z = 0.25f;
                renderer.Spawn(kind, r);
            }

            // ---- the fizz: flecks rising off the target as it shows again (frames 90-112)
            int fizz = mid ? 0 : 26;
            for (int i = 0; i < fizz; i++)
            {
                float rad = rng.Range(0.022f, 0.038f);
                var r = Rec(now, rng.Range(dry - 0.15f, dry + 0.9f), rng.Range(0.35f, 0.6f), NeonAcidKind.Drop,
                    ground + right * rng.Range(-0.9f, 0.9f) + Vector3.up * rng.Range(0.3f, 2.0f) - away * rng.Range(0.3f, 0.9f), Acid, 1f, ref rng);
                r.Vel = Vector3.up * rng.Range(0.5f, 1.4f) + right * rng.Range(-0.2f, 0.2f);
                r.Motion = new Vector4(-0.6f, 0f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.5f, 1f, 0f);
                r.Extra.y = 1f;
                renderer.Spawn(kind, r);
            }
        }

        /// <summary>One light (the shader's glow): alive from `born` to `gone` seconds after the call, coming up over
        /// `rise` seconds and going over the last `fall`. Its reach: `along` its level `axis`, `up` and down,
        /// `behind` square to the axis on the near side (with the axis square to the jet's track: back toward the
        /// nozzle), `ahead` of that on the far side. `into` is multiplied into what it lights, `add` added. A
        /// `level` light falls on level ground only: whatever stands on that ground is left alone.</summary>
        static void Glow(CelFxRenderer renderer, int kind, float now, float born, float gone, Vector3 at, Vector3 axis,
            float along, float up, float behind, float ahead, float strength, Vector3 into, Vector3 add, float rise, float fall, bool level, uint seed)
        {
            var own = new CelRng(seed);   // a light has nothing random in it: the effect's own draws stay as they were
            var r = Rec(now, born, gone - born, NeonAcidKind.Glow, at, Acid, strength, ref own);
            r.Tone.y = level ? 1f : 0f;
            r.Axis = axis;
            r.Vel = into;
            r.Size = new Vector4(along, up, ahead, behind / along);
            r.Erode = new Vector4(add.x, add.y, add.z, 0f);
            r.Extra.y = rise;
            r.Extra.z = fall;
            renderer.Spawn(kind, r);
        }

        static CelRecord Rec(float now, float at, float life, float mode, Vector3 pos, int palette, float opacity, ref CelRng rng)
        {
            return new CelRecord
            {
                Pos = pos, Born = now + at, Life = life, Axis = Vector3.right, Seed = rng.Next(),
                Size = new Vector4(1f, 1f, 1f, 1f), Erode = new Vector4(0f, 0f, 1f, 0f),
                Tone = new Vector4(CelRecord.PackPalette(palette), 1f, opacity, 0f),
                Extra = new Vector4(mode, 0f, 0f, at),
            };
        }

        /// <summary>The ground's slope at a spot (rise per metre along x and z), for a card that lies on it.</summary>
        static Vector4 Slope(Vector3 at)
        {
            if (RenderGround.Map == null) return Vector4.zero;
            return new Vector4(
                (Ground(at + Vector3.right * 3f) - Ground(at - Vector3.right * 3f)) / 6f,
                (Ground(at + Vector3.forward * 3f) - Ground(at - Vector3.forward * 3f)) / 6f, 0f, 0f);
        }

        static float Ground(Vector3 p) => RenderGround.Map != null
            ? RenderGround.Sample(RenderGround.Map, p.x, p.z) : p.y;
    }
}
