// Phase: CelFx acid ball (2026-11-15) - a ball of acid flies in from the eye's near left, bursts on the target in a
// white-hot flash with green blades, throws up a crown of goo, sends a ring of it out over the ground with a ring of
// smoke riding it, and leaves a glowing green cloud over a bubbling puddle that dries to a bright-rimmed stain.
// Reference: acid_ball_cel_00001_.mp4, 192 f at 24 fps, drawn on ones; the ball is in frame on its frame 7, lands on
// frame 25.8 and bursts on 26. Sizes and places are read off the clip through the beach Range's camera (71 px to the
// metre at the dummy); colours are the clip's, taken back through the Range's night grade (fitted to neon_acid's
// palette pairs). Built from NeonAcidRecipe (puddle, bubbles, smoke, light) and FireballRecipe (the comet).
//
// It draws through the kind's own shader (CelAcidBall_URP): 0 the ball and its tail, 1 drops and flecks, 2 the burst,
// 3 the crown, 4 the puddle, 5 bubbles, 6 smoke and the glowing cloud, 7 the light, 8 the ring on the ground.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class AcidBallRecipe
    {
        /// <summary>Seconds from the launch to the hit (clip frames 6.6 to 25.8), and the fall of the path (m/s2).</summary>
        public const float Flight = 0.80f, Gravity = 3.0f;
        /// <summary>The ball's radius (m): 44 px at the dummy on frame 23.</summary>
        public const float BallRadius = 0.555f;
        /// <summary>The tail as it shows across the view (m): 250 px behind the ball at 100 px/m on frame 16.</summary>
        const float TailLength = 3.0f;
        /// <summary>Seconds the whole effect is alive: launch on frame 6.6, the last of the stain gone by frame 160.</summary>
        public const float Length = 6.5f;

        /// <summary>The clip's effect starts on its frame 6 and the ball leaves ClipDelay after that. AtFrame gives the
        /// seconds after the call at which one of the clip's frames falls; `late` is how much later than the clip's
        /// this play's launch is (delay - ClipDelay).</summary>
        const float FirstFrame = 6f, ClipDelay = 0.025f;
        static float AtFrame(float late, float frame) => (frame - FirstFrame) / 24f - 0.004f + late;

        // ---- sizes, in metres (the clip's pixels at the dummy's depth)
        /// <summary>The burst's white core (2.5 m across on frame 27) and the reach of its longest blade (to the top of
        /// the frame on frame 28: 6 m over the hit).</summary>
        const float CoreHalf = 1.3f, BladeReach = 6.2f;
        /// <summary>The crown: a net of goo streams out of a 3.4 m foot 0.55 m right of the target's (the clip's
        /// frames 36-44: 6.4 m across 2.9 m up, the tallest 5.5 m up); the card's height over the foot, the streams'
        /// reach.</summary>
        const float CrownTall = 6.6f, CrownReach = 5.4f;
        /// <summary>The ring on the ground, out of the hit on frame 34: its near edge under the foot at y 583, 623, 668,
        /// 687, 705 on frames 36, 38, 40, 44, 48 - fast to frame 40, then hardly moving: a radius of 7.27 m that runs 63 %
        /// of the way in 0.12 s, squashed toward the eye to 0.72 of its width.</summary>
        const float RingReach = 7.27f, RingTau = 0.12f, RingBorn = 34f, RingDepth = 0.72f;
        /// <summary>The puddle at its widest (7.5 m across on frame 56; the fingers reach the card's edge).</summary>
        const float PuddleReach = 7.0f, PuddleDepth = 0.62f;

        // A colour as the shader has to write it (the project is gamma, the palette stores .linear). The comments give
        // what the film should show (the clip's, clustered).
        static Color S(float r, float g, float b) => new Color(Mathf.LinearToGammaSpace(r), Mathf.LinearToGammaSpace(g), Mathf.LinearToGammaSpace(b), 1f);

        // the ball: inside dark (55,112,20), shade (106,196,36), body (150,236,59), light (193,250,69), pale (228,252,125)
        static readonly int Ball = CelPalette.Add("acid_ball",
            S(0.179f, 0.360f, 0.082f), S(0.294f, 0.614f, 0.147f), S(0.421f, 0.799f, 0.227f), S(0.560f, 0.910f, 0.266f), S(0.601f, 0.929f, 0.419f), 1.1f, 4, S(0.131f, 0.289f, 0.074f));
        // the burst: green (113,199,75), lime (172,244,93), yellow (242,248,75), cream (249,246,147), white (253,250,246)
        static readonly int Flash = CelPalette.Add("acid_ball_burst",
            S(0.306f, 0.624f, 0.277f), S(0.474f, 0.842f, 0.326f), S(0.900f, 0.930f, 0.240f), S(0.960f, 0.950f, 0.520f), S(1.000f, 1.000f, 0.960f), 1.1f, 4, S(0.194f, 0.381f, 0.164f));
        // the goo (crown, ring, puddle, drops): dark (51,91,40), shade (77,150,31), body (119,204,41), light (165,237,52), pale (210,247,85)
        static readonly int Goo = CelPalette.Add("acid_ball_goo",
            S(0.166f, 0.298f, 0.164f), S(0.245f, 0.462f, 0.127f), S(0.321f, 0.641f, 0.168f), S(0.459f, 0.804f, 0.222f), S(0.592f, 0.875f, 0.301f), 1.1f, 4, S(0.131f, 0.246f, 0.102f));
        // the smoke riding the ring: pale grey-green (95,128,74) .. (150,196,118)
        static readonly int Smoke = CelPalette.Add("acid_ball_smoke",
            S(0.224f, 0.312f, 0.227f), S(0.276f, 0.402f, 0.275f), S(0.309f, 0.467f, 0.310f), S(0.370f, 0.552f, 0.354f), S(0.421f, 0.614f, 0.396f), 1.1f, 4, S(0.163f, 0.230f, 0.182f));
        // the glowing cloud over the target: dark (40,74,30), shade (55,112,28), body (78,155,42), light (100,200,60),
        // pale (128,228,83) - the clip's f60 (light, pale) to f96 (dark, shade)
        static readonly int Dome = CelPalette.Add("acid_ball_dome",
            S(0.131f, 0.243f, 0.123f), S(0.170f, 0.328f, 0.111f), S(0.224f, 0.429f, 0.156f), S(0.284f, 0.627f, 0.228f), S(0.348f, 0.760f, 0.296f), 1.1f, 4, S(0.114f, 0.213f, 0.090f));
        // a bubble: rim, shade (90,160,45), body (120,200,50), light - its rim - (200,240,80), glint (a63: the clip's glossy lime-yellow)
        static readonly int Bubble = CelPalette.Add("acid_ball_bubble",
            S(0.173f, 0.328f, 0.176f), S(0.268f, 0.491f, 0.182f), S(0.324f, 0.627f, 0.211f), S(0.580f, 0.818f, 0.288f), S(0.605f, 0.951f, 0.713f), 1.1f, 4, S(0.173f, 0.328f, 0.176f));

        /// <summary>The light on the ground: what it multiplies in at its heart (lit = unlit x (1 + into)) and what it
        /// adds on top. Under the ball as it comes in the near ground goes 1.5x in red and 1.8x in green (frames 14-18);
        /// the burst takes the near ground to 2.6-4.9x red, 3.3-6.7x green, 1.7-2.5x blue on frame 27, back to the night's
        /// own by frame 36.</summary>
        static readonly Vector3 UnderInto = new Vector3(0.45f, 0.85f, 0.15f), UnderAdd = new Vector3(0.02f, 0.05f, 0.01f);
        static readonly Vector3 BurstInto = new Vector3(2.0f, 2.3f, 0.65f), BurstAdd = new Vector3(0.10f, 0.18f, 0.03f);
        static readonly Vector3 PoolInto = new Vector3(0.7f, 1.3f, 0.2f), PoolAdd = new Vector3(0.03f, 0.08f, 0.02f);
        /// <summary>The target while the goo is on it: its own colours turned the acid's green.</summary>
        static readonly Vector3 CoatInto = new Vector3(0.6f, 2.4f, 0.1f), CoatAdd = new Vector3(0.16f, 0.45f, 0.05f);
        /// <summary>The target as the ball comes at it: lit lime-yellow from its near side.</summary>
        static readonly Vector3 ApproachInto = new Vector3(0.8f, 1.4f, 0.2f), ApproachAdd = new Vector3(0.05f, 0.10f, 0.02f);

        static readonly int LensId = Shader.PropertyToID("_TWAcidLens");

        static AcidBallRecipe() { SceneStatics.Register(nameof(AcidBallRecipe), () => Shader.SetGlobalVector(LensId, Vector4.zero)); }

        /// <summary>The scene's lens vignette (URP Vignette's intensity and smoothness; 0 for none): the acid is written
        /// brighter where the vignette will darken it.</summary>
        public static void Lens(float intensity, float smoothness) =>
            Shader.SetGlobalVector(LensId, new Vector4(Mathf.Max(0f, intensity) * 3f, Mathf.Max(0.01f, smoothness) * 5f, 0f, 0f));

        /// <summary>The launch velocity that carries the ball from where it starts to `to` in Flight seconds.</summary>
        public static Vector3 Launch(Vector3 from, Vector3 to) => (to - from) / Flight + Vector3.up * (0.5f * Gravity * Flight);

        /// <summary>Where the ball that left `from` for `to` s seconds ago is.</summary>
        public static Vector3 Along(Vector3 from, Vector3 to, float s) => from + Launch(from, to) * s + Vector3.down * (0.5f * Gravity * s * s);

        /// <summary>
        /// Play the acid ball: it starts at `from` and lands on `target` (the dummy's chest), whose foot stands on the
        /// ground at `ground`; `right` is the eye's level right. `delay` holds it for that long after the call.
        /// Everything is spawned now (the light, the puddle and the ring find what they fall on in the camera's depth
        /// texture): nothing is left to do per frame. No allocation.
        /// </summary>
        public static void Play(Vector3 from, Vector3 target, Vector3 ground, Vector3 right, uint seed, float delay = 0f)
        {
            right.y = 0f;
            right = right.sqrMagnitude > 0.001f ? right.normalized : Vector3.right;
            Vector3 away = Vector3.Cross(right, Vector3.up);   // from the eye
            var renderer = CelFx.Renderer;
            int kind = renderer.KindId(AcidBallKind.KindName);
            if (kind < 0) throw new System.InvalidOperationException("AcidBall kind is not registered");
            float lod = CelFx.LodBlend(CelFx.Zoom);
            bool mid = lod >= 0.5f, far = lod >= 1.5f;
            float now = CelFx.Now;
            var rng = new CelRng(seed);
            float late = delay - ClipDelay;
            float hit = delay + Flight;                          // clip frame 25.8
            ground.y = Ground(ground);
            // it lands on the dummy's face, a little under its chest
            Vector3 land = target - away * 0.6f - Vector3.up * 0.14f;
            Vector3 vel = Launch(from, land);
            Vector3 foot = ground - away * 0.3f;
            foot.y = Ground(foot) + 0.02f;

            // ---- the light (under everything): under the ball's path as it comes in, the burst over everything
            // round it, the crown and the ring on the level ground, the puddle's own after it, the target's coat
            {
                Vector3 under1 = Vector3.Lerp(from, land, 0.58f);
                under1.y = Ground(under1) + 1f;
                Glow(renderer, kind, now, AtFrame(late, 8f), AtFrame(late, 21f), under1, right, 4.0f, 4.0f, 4.0f, 1f,
                    far ? 0.6f : 1f, UnderInto, UnderAdd, 0.12f, 0.20f, true, seed);
                Vector3 under2 = Vector3.Lerp(from, land, 0.88f);
                under2.y = Ground(under2) + 1f;
                Glow(renderer, kind, now, AtFrame(late, 14f), AtFrame(late, 26.5f), under2, right, 4.0f, 4.0f, 4.0f, 1f,
                    far ? 0.6f : 1f, UnderInto, UnderAdd, 0.15f, 0.08f, true, seed);
                Vector3 burst = ground + Vector3.up * 1.0f;
                Glow(renderer, kind, now, AtFrame(late, 25.8f), AtFrame(late, 35.5f), burst + right * 1.5f, right, 22f, 18f, 22f, 1f,
                    0.78f, BurstInto, BurstAdd, 0.03f, 0.26f, false, seed);
                Glow(renderer, kind, now, AtFrame(late, 33f), AtFrame(late, 62f), burst, right, 7.0f, 7.0f, 7.0f, 1f,
                    0.5f, PoolInto, PoolAdd, 0.10f, 0.45f, true, seed);
                Glow(renderer, kind, now, AtFrame(late, 50f), AtFrame(late, 126f), burst, right, 5.0f, 5.0f, 5.0f, 1f,
                    0.8f, PoolInto, PoolAdd, 0.30f, 0.80f, true, seed);
                if (!far) Glow(renderer, kind, now, AtFrame(late, 28f), AtFrame(late, 114f), target, right, 1.8f, 1.8f, 1.8f, 1f,
                    1f, CoatInto, CoatAdd, 0.15f, 0.35f, false, seed);
                // the dummy lit by the ball as it comes (the clip: from frame 16, bright by 22)
                if (!far) Glow(renderer, kind, now, AtFrame(late, 13f), AtFrame(late, 26.5f), target, right, 2.2f, 2.2f, 2.2f, 1f,
                    1f, ApproachInto, ApproachAdd, 0.40f, 0.05f, false, seed);
            }

            // ---- the puddle round the foot (under everything else): out from frame 34, a bright rim round a dark
            // inside from frame 86, drains from frame 80, dry by frame 160
            {
                float born = AtFrame(late, 34f), gone = AtFrame(late, 160f);
                var r = Rec(now, born, gone - born, AcidBallKind.Puddle, ground, Goo, 1f, ref rng);
                r.Axis = right;
                r.Size = new Vector4(PuddleReach, PuddleReach, 1f, PuddleDepth);
                r.Motion = Slope(ground);
                r.Motion.z = -1f;
                r.Extra.y = 0.40f;
                r.Extra.z = AtFrame(late, 80f) - born;
                r.Erode = new Vector4(AtFrame(late, 50f) - born, 0.6f, 0f, 0f);   // a62: dark inside a broad bright rim from f50 (the clip's f60-90)
                renderer.Spawn(kind, r);
            }

            // ---- the ring that runs out over the ground (frames 33.5-58), breaking into arcs as it goes
            {
                float born = AtFrame(late, RingBorn);
                var r = Rec(now, born, 1.05f, AcidBallKind.Ring, ground + right * 0.2f, Goo, 1f, ref rng);
                r.Axis = right;
                r.Size = new Vector4(RingReach, 1.3f, 0.85f, RingTau);
                r.Motion = Slope(ground);
                r.Motion.z = RingDepth;          // squashed toward the eye, as the clip's (its near edge on f40: y 669, not the frame's foot)
                renderer.Spawn(kind, r);
            }

            // ---- the flat splash the burst throws on the ground (frames 29-36): long yellow streaks out to the sides
            {
                float born = AtFrame(late, 29f), gone = AtFrame(late, 36.5f);
                var r = Rec(now, born, gone - born, AcidBallKind.Puddle, ground, Flash, 1f, ref rng);
                r.Axis = right;
                r.Size = new Vector4(4.6f, 4.6f, 1f, 0.55f);
                r.Motion = Slope(ground);
                r.Extra.y = 0.06f;
                r.Extra.z = AtFrame(late, 33f) - born;
                r.Erode = new Vector4(99f, 1f, 0f, 1f);   // w 1: the splash, not the puddle
                renderer.Spawn(kind, r);
            }

            // ---- the goo on the target (frames 40-108): under the crown at first, standing out of it from frame 46
            if (!far)
            {
                float born = AtFrame(late, 40f), gone = AtFrame(late, 122f);   // a60: still on it at the clip's f104-116
                var r = Rec(now, born, gone - born, AcidBallKind.Coat, ground, Goo, 1f, ref rng);
                r.Axis = right;
                r.Size = new Vector4(0.85f, 2.55f, 0.12f, 0.6f);
                r.Extra.y = 0.25f;
                r.Extra.z = 0.60f;
                renderer.Spawn(kind, r);
            }

            // ---- the ball and its tail (in frame on frame 7, on the dummy at 25.8; the tail goes in the burst)
            {
                var r = Rec(now, delay, Flight + 0.004f, AcidBallKind.Comet, from, Ball, 1f, ref rng);   // gone on the hit: the burst is full size on that frame
                r.Vel = vel;
                r.Motion = new Vector4(Gravity, 0f, 0f, 0f);
                r.Size = new Vector4(BallRadius, TailLength, 1.45f, Flight);
                renderer.Spawn(kind, r);
            }

            // ---- drops shed by the ball as it flies: small beads peeling off its top and its tail
            int shed = far ? 0 : mid ? 4 : 8;
            for (int i = 0; i < shed; i++)
            {
                float when = rng.Range(0.05f, Flight - 0.06f);
                Vector3 at = Along(from, land, when) + Vector3.up * rng.Range(0.1f, 0.6f) + right * rng.Range(-0.5f, 0.2f);
                float rad = rng.Range(0.035f, 0.075f);
                var r = Rec(now, delay + when, rng.Range(0.22f, 0.40f), AcidBallKind.Drop, at, Ball, 1f, ref rng);
                r.Vel = vel * rng.Range(0.15f, 0.45f) + Vector3.up * rng.Range(0.5f, 2.0f);
                r.Motion = new Vector4(6f, 1.0f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.5f, 1f, 0.006f);
                r.Extra.y = 1f;
                r.Extra.z = 1f;
                renderer.Spawn(kind, r);
            }

            // ---- THE BURST (frames 25.8-35): one card, in front of the dummy
            {
                var r = Rec(now, hit - 0.004f, 0.42f, AcidBallKind.Burst, target - Vector3.up * 0.45f, Flash, 1f, ref rng);
                r.Size = new Vector4(9.5f, 8.5f, CoreHalf, BladeReach);
                r.Extra.y = 2.5f;                // toward the eye, so the ground in front of the dummy does not cut it
                renderer.Spawn(kind, r);
            }

            // ---- dark-green splinters thrown out of the burst (frames 27-31): fast thin leaves
            int splinters = far ? 0 : mid ? 8 : 20;
            for (int i = 0; i < splinters; i++)
            {
                float turn = rng.Range(-0.15f, 1.15f) * Mathf.PI;
                Vector3 d = (right * Mathf.Cos(turn) + Vector3.up * Mathf.Sin(turn) * 0.8f + away * rng.Range(-0.3f, 0.3f)).normalized;
                var r = Rec(now, hit + rng.Range(0.02f, 0.12f), rng.Range(0.14f, 0.24f), AcidBallKind.Drop,
                    target - Vector3.up * 0.3f + d * rng.Range(0.8f, 1.6f) - away * 1.2f, Goo, 1f, ref rng);
                r.Vel = d * rng.Range(14f, 24f);
                r.Motion = new Vector4(4f, 2.0f, 0f, 0f);
                r.Size = new Vector4(0.07f, 0.04f, 1f, rng.Range(0.012f, 0.022f));
                r.Extra.y = 2f;
                renderer.Spawn(kind, r);
            }

            // ---- the crown of goo that comes out of the burst (frames 28.5-53.7): a splash of goo streams, thin
            // straight yellow-lime spikes at first, then flame tongues, then strands tied into loops that tear
            if (!far)
            {
                float born = AtFrame(late, 28.5f);
                var r = Rec(now, born, 1.05f, AcidBallKind.Crown, foot, Goo, 1f, ref rng);   // gone by frame 53.7
                r.Tone.x = CelRecord.PackPalette(Goo, Flash);   // the second row: its yellow-lime first frames
                r.Size = new Vector4(5.8f, CrownTall, CrownReach, mid ? 48f : 96f);
                r.Motion = new Vector4(0.10f, 0.40f, 0.8f, 0f);
                renderer.Spawn(kind, r);
            }

            // ---- the splash thrown off the crown (frames 32-56): beads and leaves flung up and out
            int splash = far ? 0 : mid ? 30 : 80;
            for (int i = 0; i < splash; i++)
            {
                Vector2 on = rng.InDisc();
                Vector3 at = ground + right * (on.x * 2.0f) - away * (on.y * 0.8f + 0.6f) + Vector3.up * rng.Range(1.0f, 4.2f);
                Vector3 d = (right * on.x * rng.Range(0.6f, 1.4f) + Vector3.up * rng.Range(0.8f, 1.6f) - away * rng.Range(-0.3f, 0.5f)).normalized;
                float rad = rng.Range(0.05f, 0.12f);
                var r = Rec(now, AtFrame(late, 32f) + rng.Range(0f, 0.55f), rng.Range(0.40f, 0.70f), AcidBallKind.Drop, at, Goo, 1f, ref rng);
                r.Vel = d * rng.Range(2.5f, 6.5f);
                r.Motion = new Vector4(9f, 0.3f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.7f, 1f, rng.Range(0.010f, 0.020f));
                r.Extra.y = i % 3 == 0 ? 2f : 1f;
                r.Extra.z = 1f;
                renderer.Spawn(kind, r);
            }

            // ---- the smoke riding the ring (frames 37-80): pale puffs born where the ring passes, drifting on out
            int puffs = far ? 6 : mid ? 14 : 32;
            for (int i = 0; i < puffs; i++)
            {
                float ang = (i + rng.Range(0f, 0.8f)) / puffs * Mathf.PI * 2f;
                float rad = rng.Range(0.45f, 1.10f) * RingReach;   // a58: on the ring from f37 as the clip's
                float when = -RingTau * Mathf.Log(Mathf.Max(1f - rad / RingReach, 0.05f));
                Vector3 dir = right * Mathf.Cos(ang) - away * (Mathf.Sin(ang) * RingDepth);
                Vector3 at = ground + right * 0.2f + dir * rad;
                at.y = Ground(at) + rng.Range(0.2f, 1.2f);
                float size = rng.Range(1.3f, 2.0f);
                var r = Rec(now, AtFrame(late, RingBorn) + when, rng.Range(1.0f, 2.2f), AcidBallKind.Puff, at, Smoke, 0.80f, ref rng);
                r.Tone.w = 0.6f;                 // soft edged, as the clip's
                r.Vel = dir * rng.Range(0.9f, 1.8f) + Vector3.up * rng.Range(0.1f, 0.4f);
                r.Motion = new Vector4(0f, 1.2f, 0f, 0f);
                r.Size = new Vector4(size * 0.7f, size * 1.3f, 0.8f, 1f);
                r.Extra.y = 0.45f;
                r.Extra.z = 1.2f;
                renderer.Spawn(kind, r);
            }

            // ---- the glowing cloud over the target (frames 46-124): big soft puffs, bright at first, darker as it goes
            int dome = far ? 3 : mid ? 8 : 16;
            for (int i = 0; i < dome; i++)
            {
                // spread evenly over a tall round mass round the target (the clip's frames 56-104)
                float x = ((i % 4) + rng.Range(0.1f, 0.9f)) / 4f * 4.2f - 2.1f;
                float y = ((i / 4) + rng.Range(0.1f, 0.9f)) / 4f * 2.4f;
                Vector3 at = ground + right * x + Vector3.up * y + away * rng.Range(0.7f, 1.5f);   // behind the target: it stands in front of it
                float born = AtFrame(late, rng.Range(46f, 56f)), gone = AtFrame(late, rng.Range(116f, 130f));
                float size = rng.Range(2.3f, 2.9f);
                var r = Rec(now, born, gone - born, AcidBallKind.Puff, at, Dome, rng.Range(0.50f, 0.95f), ref rng);
                r.Tone.w = 0.75f;                // a soft glowing edge, firmer than a blur
                r.Vel = Vector3.up * rng.Range(0.05f, 0.2f) + right * rng.Range(-0.1f, 0.1f);
                r.Motion = new Vector4(0f, 0.5f, 0f, 0f);
                r.Size = new Vector4(size * 0.8f, size * 1.1f, 0.8f, 1f);
                r.Extra.y = 0.62f;
                r.Extra.z = rng.Range(0.20f, 0.95f);   // some stay light, some go deep green early
                renderer.Spawn(kind, r);
            }

            // ---- the cloud low at the target's sides over the puddle (frames 52-110), in front of it but never on it
            int lowSide = far ? 0 : mid ? 2 : 6;
            for (int i = 0; i < lowSide; i++)
            {
                float sgn = (i & 1) == 0 ? -1f : 1f;
                Vector3 at = ground + right * (sgn * rng.Range(0.8f, 2.4f)) + Vector3.up * rng.Range(0.1f, 0.8f) - away * rng.Range(0.2f, 1.0f);
                float born = AtFrame(late, rng.Range(50f, 58f)), gone = AtFrame(late, rng.Range(100f, 112f));
                float size = rng.Range(1.5f, 2.0f);
                var r = Rec(now, born, gone - born, AcidBallKind.Puff, at, Dome, rng.Range(0.30f, 0.50f), ref rng);   // a60: light over the puddle
                r.Tone.w = 0.75f;
                r.Vel = Vector3.up * rng.Range(0.03f, 0.10f) + right * (sgn * rng.Range(0.02f, 0.08f));
                r.Motion = new Vector4(0f, 0.5f, 0f, 0f);
                r.Size = new Vector4(size * 0.8f, size * 1.1f, 0.8f, 1f);
                r.Extra.y = 0.62f;
                r.Extra.z = rng.Range(0.30f, 0.80f);
                renderer.Spawn(kind, r);
            }

            // ---- the cloud's last, deeper green and wider (frames 84-126): four big soft puffs at its sides
            int lateN = far ? 0 : 4;
            for (int i = 0; i < lateN; i++)
            {
                float sgn = (i & 1) == 0 ? -1f : 1f;
                Vector3 at = ground + right * (sgn * rng.Range(1.6f, 2.6f)) + Vector3.up * rng.Range(1.0f, 2.2f) + away * rng.Range(0.7f, 1.5f);
                float born = AtFrame(late, rng.Range(80f, 90f)), gone = AtFrame(late, rng.Range(118f, 128f));
                float size = rng.Range(1.9f, 2.4f);
                var r = Rec(now, born, gone - born, AcidBallKind.Puff, at, Dome, 0.45f, ref rng);
                r.Tone.w = 1f;
                r.Vel = Vector3.up * rng.Range(0.05f, 0.15f) + right * (sgn * rng.Range(0.05f, 0.2f));
                r.Motion = new Vector4(0f, 0.5f, 0f, 0f);
                r.Size = new Vector4(size * 0.9f, size * 1.1f, 0.8f, 1f);
                r.Extra.y = 0.55f;
                r.Extra.z = 0.15f;
                renderer.Spawn(kind, r);
            }

            // ---- bubbles on the puddle (frames 40-120), spawned after the cloud so they draw over it (a60: the clip's are
            // crisp in front of its cloud)
            int bubbles = far ? 0 : mid ? 10 : 30;
            for (int i = 0; i < bubbles; i++)
            {
                float ang = (i + rng.Range(0f, 0.7f)) / bubbles * Mathf.PI * 2f;
                Vector3 dir = right * Mathf.Cos(ang) - away * Mathf.Sin(ang) * 0.7f;
                Vector3 at = ground + dir * rng.Range(1.4f, 3.6f);
                at.y = Ground(at);
                float born = AtFrame(late, rng.Range(38f, 96f));
                var r = Rec(now, born, rng.Range(0.6f, 1.2f), AcidBallKind.Bubble, at, Bubble, 1f, ref rng);
                r.Size = new Vector4(rng.Range(0.18f, 0.62f), 1f, 1f, 1f);
                r.Extra.y = 0.06f;
                renderer.Spawn(kind, r);
            }

            // ---- glowing specks rising out of the cloud (frames 48-110)
            int specks = mid ? 0 : 64;
            for (int i = 0; i < specks; i++)
            {
                float rad = rng.Range(0.016f, 0.032f);
                var r = Rec(now, AtFrame(late, rng.Range(48f, 104f)), rng.Range(0.4f, 0.8f), AcidBallKind.Drop,
                    ground + right * rng.Range(-2.4f, 2.4f) + Vector3.up * rng.Range(0.3f, 2.6f) - away * rng.Range(0.3f, 1.4f), Flash, 1f, ref rng);
                r.Vel = Vector3.up * rng.Range(0.3f, 0.9f) + right * rng.Range(-0.15f, 0.15f);
                r.Motion = new Vector4(-0.4f, 0f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.6f, 1f, 0f);
                r.Extra.y = 1f;
                r.Extra.z = 1f;
                renderer.Spawn(kind, r);
            }
        }

        /// <summary>One light (the shader's glow), as NeonAcidRecipe.Glow.</summary>
        static void Glow(CelFxRenderer renderer, int kind, float now, float born, float gone, Vector3 at, Vector3 axis,
            float along, float up, float behind, float ahead, float strength, Vector3 into, Vector3 add, float rise, float fall, bool level, uint seed)
        {
            var own = new CelRng(seed);
            var r = Rec(now, born, gone - born, AcidBallKind.Glow, at, Goo, strength, ref own);
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
