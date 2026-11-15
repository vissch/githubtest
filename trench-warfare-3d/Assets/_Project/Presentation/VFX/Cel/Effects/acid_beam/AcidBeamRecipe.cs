// Phase: CelFx acid beam (2026-11-15) - a straight beam of acid from the eye's left onto a target: glossy blobs bud off
// its top, sparks and leaves fly where it lands, spiky starbursts and a mass of goo stand over the target, a bank of
// green smoke rises off the beam, its end breaks into a rope of beads, and the goo sinks into a yellow-rimmed puddle
// under a plume of smoke (reference clip acid_beam_cel_00001_, 8 s at 24 fps, drawn on ones).
// Built from NeonAcidRecipe (the light, the puddle decal, the smoke, the mound). Every number below was read off the
// clip: the beam's line and speed from a 3D fit of its head's track to the beach Range's camera (the head enters the
// frame on frame 20 and reaches the chest on frame 31.5: 22 m/s), the times from its frames (the comments name them,
// the effect starting on frame 12), the sizes from the camera's scale at the target (71 px per m), the colours from its
// pixels taken back through the Range's night grade (the neon acid's swatch fit).
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class AcidBeamRecipe
    {
        /// <summary>The beam's speed (m/s), the seconds the nozzle is open (its tail leaves on frame 90.5) and the fall
        /// of the beam (none: the clip's beam is a straight line).</summary>
        public const float Speed = 22f, JetSeconds = 3.095f, Gravity = 0f;
        /// <summary>The beam's half-width (m) at the nozzle and at the target (its lumpy edges sit at 0.68-1 of it: the
        /// clip's beam is 1.3-1.6 m thick where it leaves the frame and 0.9-1.0 m where it lands; b01 filmed 10 % over).</summary>
        public const float HalfNozzle = 0.92f, HalfTarget = 0.70f;
        /// <summary>Seconds of fluid: the head's cone (the first fluid is thin: the clip's beam is 0.36 of its width
        /// 0.02 s behind the point, 0.78 at 0.21 s), the tail's taper, the end that breaks up (the clip: fluid that
        /// left after 2.6 s of the 3.1 is a torn net by frame 88).</summary>
        const float HeadTaper = 0.45f, TailTaper = 0.05f, BreakUp = 0.60f;
        /// <summary>The puddle's half-extent across (m) and its depth over its width (the clip: a slab 3.6 m across
        /// round the target's foot, 260 x 90 px at frame 125; b01 filmed a little big) and how far to the eye's right
        /// of the foot its middle lies (the clip's: 50 px).</summary>
        public const float PuddleReach = 3.6f, PuddleDepth = 0.75f, PuddleRight = 0.7f;
        /// <summary>The mass of goo over the target: the card's width (m) and its height over its width: a see-through
        /// backing (the clip's mass is sheets of goo: the leaves and the core below make it).</summary>
        public const float MoundWidth = 4.6f, MoundTall = 0.75f;

        // A colour as the shader has to write it (the project is gamma, the palette stores .linear). The values are
        // the clip's colours taken back through the Range's night grade; the comments give what they show as.
        static Color S(float r, float g, float b) => new Color(Mathf.LinearToGammaSpace(r), Mathf.LinearToGammaSpace(g), Mathf.LinearToGammaSpace(b), 1f);

        // the beam: rim #48913E, green #5BBE52, lime #9DD957, yellow #DEF14B, hot yellow #F8F26A, pale streaks #E5F794
        static readonly int Jet = CelPalette.Add("acid_beam_jet",
            S(0.227f, 0.448f, 0.227f), S(0.287f, 0.595f, 0.283f), S(0.488f, 0.708f, 0.302f), S(0.628f, 0.844f, 0.286f), S(0.698f, 0.855f, 0.361f), 1.1f, 4, S(0.620f, 0.887f, 0.474f));
        // the splash over the target: #5B9246, #72C560, #98E26C, #BDF27E, pale #EFF798, yellow #DCF564
        static readonly int Splash = CelPalette.Add("acid_beam_splash",
            S(0.284f, 0.450f, 0.247f), S(0.355f, 0.623f, 0.319f), S(0.464f, 0.752f, 0.358f), S(0.532f, 0.848f, 0.411f), S(0.648f, 0.889f, 0.487f), 1.1f, 4, S(0.607f, 0.878f, 0.346f));
        // the puddle: edge #469644, body #51C55A, patches #79D157, #B0DD53, its yellow rim #EBFA56, the line inside it #CEF584
        static readonly int Pool = CelPalette.Add("acid_beam_pool",
            S(0.220f, 0.462f, 0.242f), S(0.259f, 0.626f, 0.304f), S(0.378f, 0.676f, 0.299f), S(0.544f, 0.728f, 0.297f), S(0.635f, 0.908f, 0.317f), 1.1f, 4, S(0.565f, 0.877f, 0.430f));
        // a glossy blob: rim #548D41, shade #51BA58, body #86C35B, light #A7E37C, glint #D9F895
        static readonly int Blob = CelPalette.Add("acid_beam_blob",
            S(0.264f, 0.437f, 0.234f), S(0.257f, 0.584f, 0.299f), S(0.415f, 0.615f, 0.307f), S(0.503f, 0.763f, 0.398f), S(0.584f, 0.893f, 0.480f), 1.1f, 4, S(0.549f, 0.854f, 0.321f));
        // the smoke, seen through at FumeOpacity over the night ground: #5F834C, #6F985A, #7DB15D, #91CD6A, lit #D5F29E
        static readonly int Fume = CelPalette.Add("acid_beam_fume",
            S(0.350f, 0.475f, 0.277f), S(0.402f, 0.547f, 0.322f), S(0.456f, 0.650f, 0.336f), S(0.537f, 0.784f, 0.383f), S(0.711f, 1.000f, 0.576f), 1.1f, 4, S(0.363f, 0.488f, 0.293f));
        const float FumeOpacity = 0.8f;
        // a spark: edge #A06042, #C48456, body #E8C470, #FCE48A, hot #FFF6C0
        static readonly int Spark = CelPalette.Add("acid_beam_spark",
            S(0.517f, 0.308f, 0.236f), S(0.624f, 0.405f, 0.288f), S(0.790f, 0.622f, 0.364f), S(0.761f, 0.772f, 0.442f), S(0.691f, 0.886f, 0.619f), 1.1f, 4, S(0.403f, 0.236f, 0.193f));

        /// <summary>The light the acid throws on the ground and on what stands there (the neon acid's: multiplied into
        /// what it falls on so the ground keeps its pattern, a little added on top).</summary>
        static readonly Vector3 GlowInto = new Vector3(0.87f, 1.136f, 0.18f), GlowAdd = new Vector3(0.117f, 0.234f, 0.045f);
        static readonly Vector3 CoatInto = new Vector3(0.30f, 1f, 0f), CoatAdd = new Vector3(0.10f, 0.33f, 0.05f);
        const float CoatReach = 1.8f;

        // a thin line beside the beam: its place across it (1 is the beam's edge: up the screen, toward the eye), its
        // half-width at the nozzle and at the target (m), its dash duty and dash seconds
        const int LineStride = 6;
        static readonly float[] Lines =
        {
             1.18f,  0.2f, 0.030f, 0.022f, 0.45f, 0.16f,
            -1.22f, -0.1f, 0.034f, 0.024f, 0.50f, 0.13f,
             1.42f, -0.4f, 0.022f, 0.018f, 0.35f, 0.11f,
            -1.50f,  0.3f, 0.024f, 0.018f, 0.40f, 0.10f,
        };

        // starbursts over the target: the clip's frame it opens on, its life (frames), its size at birth and at its
        // end (half-extent, m), its spikes' half-width, and the drawings between new stars
        const int StarStride = 6;
        static readonly float[] Stars =
        {
            37f, 3f, 0.7f, 1.6f, 0.09f, 1f,
            39f, 7f, 2.0f, 3.1f, 0.10f, 2f,
            50f, 4f, 1.8f, 2.4f, 0.08f, 2f,
            61f, 4f, 1.6f, 2.2f, 0.08f, 2f,
            74f, 6f, 2.0f, 2.8f, 0.09f, 2f,
            85f, 4f, 1.6f, 2.2f, 0.08f, 2f,
        };

        /// <summary>The clip's effect starts on its frame 12 and its nozzle opens ClipDelay seconds after that (frame
        /// 16.2). AtFrame gives the seconds after the call at which a clip frame falls, `late` being how much later
        /// than the clip's this play's nozzle opens (delay - ClipDelay).</summary>
        const float FirstFrame = 12f, ClipDelay = 0.176f;
        static float AtFrame(float late, float frame) => (frame - FirstFrame) / 24f - 0.004f + late;

        /// <summary>Seconds from the nozzle to the target.</summary>
        public static float FlightOf(Vector3 nozzle, Vector3 target) => Mathf.Max((target - nozzle).magnitude / Speed, 0.05f);

        /// <summary>The launch velocity that carries the beam from the nozzle to the target.</summary>
        public static Vector3 Launch(Vector3 nozzle, Vector3 target)
        {
            float flight = FlightOf(nozzle, target);
            return (target - nozzle) / flight + Vector3.up * (0.5f * Gravity * flight);
        }

        /// <summary>Where the fluid that left the nozzle s seconds ago is.</summary>
        public static Vector3 Along(Vector3 nozzle, Vector3 target, float s) => nozzle + Launch(nozzle, target) * s + Vector3.down * (0.5f * Gravity * s * s);

        static readonly int LensId = Shader.PropertyToID("_TWAcidBeamLens");

        // the lens handed over is a global of the shader: a session that ends puts it back to none
        static AcidBeamRecipe() { SceneStatics.Register(nameof(AcidBeamRecipe), () => Shader.SetGlobalVector(LensId, Vector4.zero)); }

        /// <summary>
        /// The scene's lens vignette (URP Vignette's intensity and smoothness; 0 for none), for whoever shows the
        /// effect to hand over: the acid is written brighter where the vignette will darken it.
        /// </summary>
        public static void Lens(float intensity, float smoothness) =>
            Shader.SetGlobalVector(LensId, new Vector4(Mathf.Max(0f, intensity) * 3f, Mathf.Max(0.01f, smoothness) * 5f, 0f, 0f));

        /// <summary>
        /// Play the beam: from the nozzle onto the target (its chest), whose foot stands on the ground at `ground`;
        /// `right` is the eye's level right. `delay` holds the nozzle shut for that long after the call. Everything
        /// is spawned now (the light finds what it falls on in the camera's depth texture): nothing is left to do per
        /// frame. No allocation.
        /// </summary>
        public static void Play(Vector3 nozzle, Vector3 target, Vector3 ground, Vector3 right, uint seed, float delay = 0f)
        {
            right.y = 0f;
            right = right.sqrMagnitude > 0.001f ? right.normalized : Vector3.right;
            Vector3 away = Vector3.Cross(right, Vector3.up);   // from the eye
            var renderer = CelFx.Renderer;
            int kind = renderer.KindId(AcidBeamKind.KindName);
            if (kind < 0) throw new System.InvalidOperationException("AcidBeam kind is not registered");
            float lod = CelFx.LodBlend(CelFx.Zoom);
            bool mid = lod >= 0.5f, far = lod >= 1.5f;
            float n = far ? 0.1f : mid ? 0.4f : 1f;              // the share of the loose pieces drawn
            float now = CelFx.Now;
            var rng = new CelRng(seed);
            float flight = FlightOf(nozzle, target);
            Vector3 vel = Launch(nozzle, target);
            Vector3 dir = new Vector3(vel.x, 0f, vel.z).normalized;
            Vector3 across = Vector3.Cross(Vector3.up, dir);
            float late = delay - ClipDelay;      // the clip's frames, moved with this play's nozzle
            ground.y = Ground(ground);
            Vector3 stand = ground - away * 0.3f;   // the goo stands on the target's foot (and is drawn in front of it)
            stand.y = Ground(stand) + 0.03f;

            // ---- the light (under everything). Ahead of the head and round the foot: the ground there is lit before
            // the head lands (frame 24) and dark again as the beam's end goes in (frames 96-100) ...
            {
                Vector3 foot = ground - dir * 1.6f;
                foot.y = Ground(foot) + 1f;
                Glow(renderer, kind, now, AtFrame(late, 24f), AtFrame(late, 100f), foot, across, 6.5f, 6.5f, 6.5f, 0.8f, 0.75f, GlowInto, GlowAdd, 0.25f, 0.20f, true, seed);
                // ... under the near half of the beam while it runs (frames 23-96; b01 lit the near ground too much,
                // too soon) ...
                Vector3 under = Vector3.Lerp(nozzle, target, 0.55f);
                under.y = Ground(under) + 1f;
                Glow(renderer, kind, now, AtFrame(late, 23f), AtFrame(late, 96f), under, across, 6f, 6f, 6f, 1f, far ? 0.25f : mid ? 0.4f : 0.6f, GlowInto, GlowAdd * 1.3f, 0.20f, 0.15f, true, seed);
                // ... the puddle's own glow after it, kept low so it does not light the target's head (frames 98-165) ...
                Vector3 after = ground + right * PuddleRight;
                after.y = Ground(after) + 0.4f;
                Glow(renderer, kind, now, AtFrame(late, 98f), AtFrame(late, 165f), after, across, 3.5f, 0.8f, 3.5f, 1f, 0.5f, GlowInto, GlowAdd, 0.2f, 0.8f, true, seed);
                // ... and the target, green from the moment the beam reaches it until the goo has run off it (frames 33-110)
                if (!far) Glow(renderer, kind, now, AtFrame(late, 33f), AtFrame(late, 110f), target, across, CoatReach, CoatReach, CoatReach, 1f, 1f, CoatInto, CoatAdd, 0.10f, 0.3f, false, seed);
            }

            // ---- the puddle round the target's foot: under the goo from frame 96, full by frame 112, holes open in it
            // from frame 138 and tear it to islands, dry by frame 168
            {
                float born = AtFrame(late, 96f), gone = AtFrame(late, 168f);
                Vector3 at = ground + right * PuddleRight;
                at.y = Ground(at);
                var r = Rec(now, born, gone - born, AcidBeamKind.Puddle, at, Pool, 1f, ref rng);
                r.Axis = right;
                r.Size = new Vector4(PuddleReach, PuddleReach, 1f, PuddleDepth);
                r.Motion = Slope(at);
                r.Motion.z = 1f;
                r.Extra.y = 0.45f;
                r.Extra.z = AtFrame(late, 138f) - born;
                renderer.Spawn(kind, r);
            }

            // ---- the mass of goo over the target while the beam lands (frames 44-100), and the bubbly lump it sinks to
            // after the beam (frames 94-112)
            if (!far)
            {
                float born = AtFrame(late, 44f), gone = AtFrame(late, 100f);
                var r = Rec(now, born, gone - born, AcidBeamKind.Mound, stand, Splash, 0.5f, ref rng);
                r.Axis = dir;
                r.Size = new Vector4(MoundWidth, MoundWidth, 1f, MoundTall);
                r.Motion = new Vector4(0.15f, 0.35f, 0.6f, 0f);
                r.Extra.z = 0.30f;
                renderer.Spawn(kind, r);

                // (the lump: broad and low, wider than it is tall)
                float lb = AtFrame(late, 94f), lg = AtFrame(late, 113f);
                var lump = Rec(now, lb, lg - lb, AcidBeamKind.Mound, stand, Blob, 1f, ref rng);
                lump.Axis = dir;
                lump.Size = new Vector4(4.2f, 4.2f, 1f, 0.60f);
                lump.Motion = new Vector4(0.08f, 0.8f, 0.6f, 0f);
                lump.Extra.z = 0.40f;
                renderer.Spawn(kind, lump);
            }

            // ---- the beam
            {
                var r = Rec(now, delay, JetSeconds + flight + 0.05f, AcidBeamKind.Strand, nozzle, Jet, 1f, ref rng);
                r.Vel = vel;
                r.Motion = new Vector4(Gravity, HeadTaper, TailTaper, BreakUp);
                r.Size = new Vector4(HalfNozzle, HalfTarget, 0f, flight);
                r.Axis = new Vector3(0f, 0f, rng.Range(0f, 6.2832f));
                r.Erode = new Vector4(HalfNozzle, HalfTarget, 1f, 0.1f);
                r.Extra.y = JetSeconds;
                r.Extra.z = 2f;
                renderer.Spawn(kind, r);
            }
            // ---- the thin lines beside it
            if (!far)
                for (int i = 0; i < Lines.Length; i += LineStride)
                {
                    var r = Rec(now, delay + 0.05f, JetSeconds - 0.05f + flight + 0.05f, AcidBeamKind.Strand, nozzle, Jet, 1f, ref rng);
                    r.Vel = vel;
                    r.Motion = new Vector4(Gravity, 0.03f, 0.03f, BreakUp);
                    r.Size = new Vector4(Lines[i + 2], Lines[i + 3], 0f, flight);
                    r.Axis = new Vector3(Lines[i], Lines[i + 1], rng.Range(0f, 6.2832f));
                    r.Erode = new Vector4(HalfNozzle, HalfTarget, Lines[i + 4], Lines[i + 5]);
                    r.Extra.y = JetSeconds - 0.05f;
                    r.Extra.z = 1f;
                    renderer.Spawn(kind, r);
                }

            // ---- glossy blobs that bud off the beam's top near its head and hang back as it goes on (frames 20-48)
            int blobs = Count(16, n);
            for (int i = 0; i < blobs; i++)
            {
                float rad = rng.Range(0.16f, 0.42f);
                var r = Rec(now, delay + rng.Range(0.02f, 0.45f), rng.Range(0.6f, 1.1f), AcidBeamKind.Drop, nozzle + Vector3.up * HalfNozzle * 0.6f, Blob, 1f, ref rng);
                r.Vel = vel * rng.Range(0.75f, 1.0f) + Vector3.up * rng.Range(1.0f, 3.5f) + across * rng.Range(-1.5f, 1.5f);
                r.Motion = new Vector4(3f, 3.2f, 0f, 0f);
                r.Size = new Vector4(rad * 0.7f, rad, 0.6f, 0.012f);
                r.Extra.y = 3f;
                renderer.Spawn(kind, r);
            }

            // ---- beads thrown off it all along while it runs: small round flecks, most of them under it
            int beads = Count(110, n);
            for (int i = 0; i < beads; i++)
            {
                float rad = rng.Range(0.035f, 0.075f);
                var r = Rec(now, delay + rng.Range(0.0f, JetSeconds - 0.1f), rng.Range(0.35f, 0.7f), AcidBeamKind.Drop, nozzle, Jet, 1f, ref rng);
                r.Vel = vel * rng.Range(0.55f, 1.0f) + Vector3.up * rng.Range(-4f, 2.5f) + across * rng.Range(-3f, 3f);
                r.Motion = new Vector4(9f, 1.2f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.8f, 1f, 0.004f);
                r.Tone.w = 2f + (i & 1);                                 // light and yellow (the clip's beads are bright)
                r.Extra.y = 1f;
                renderer.Spawn(kind, r);
            }

            // ---- sparks: yellow-orange dashes thrown out of the hit while the beam lands (frames 37-98), and dashes
            // streaming along the beam
            int sparks = Count(240, n);
            for (int i = 0; i < sparks; i++)
            {
                Vector3 d = rng.OnSphere();
                d.y = Mathf.Abs(d.y) * 0.9f + 0.1f;
                d = (d + dir * 0.45f).normalized;
                float rad = rng.Range(0.02f, 0.04f);
                var r = Rec(now, AtFrame(late, rng.Range(37f, 98f)), rng.Range(0.22f, 0.45f), AcidBeamKind.Drop,
                    target + Vector3.up * rng.Range(-0.4f, 0.6f) - away * 0.5f, Spark, 1f, ref rng);
                r.Vel = d * rng.Range(7f, 17f);
                r.Motion = new Vector4(9f, 1.0f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.7f, 1f, 0.02f);           // short dashes (b01's 0.035 s drew long needles)
                r.Extra.y = 4f;
                renderer.Spawn(kind, r);
            }
            int streams = Count(50, n);
            for (int i = 0; i < streams; i++)
            {
                float rad = rng.Range(0.025f, 0.04f);
                var r = Rec(now, delay + rng.Range(0.3f, JetSeconds - 0.2f), flight * rng.Range(0.6f, 0.95f), AcidBeamKind.Drop,
                    nozzle + Vector3.up * rng.Range(-1.4f, 1.4f) + across * rng.Range(-1.2f, 1.2f), Spark, 1f, ref rng);
                r.Vel = vel * rng.Range(1.0f, 1.25f);
                r.Motion = new Vector4(0f, 0f, 0f, 0f);
                r.Size = new Vector4(rad, rad, 1f, 0.03f);
                r.Extra.y = 4f;
                renderer.Spawn(kind, r);
            }

            // ---- leaves of goo flung out of the hit, up and on past the target (frames 34-98)
            int leaves = Count(110, n);
            for (int i = 0; i < leaves; i++)
            {
                Vector3 d = rng.OnSphere();
                d.y = Mathf.Abs(d.y) * 0.9f + 0.2f;
                d = (d + dir * 0.4f).normalized;
                float rad = rng.Range(0.06f, 0.13f);
                var r = Rec(now, AtFrame(late, rng.Range(34f, 98f)), rng.Range(0.3f, 0.6f), AcidBeamKind.Drop,
                    target + Vector3.up * rng.Range(-0.6f, 0.8f) + across * rng.Range(-0.6f, 0.6f) - away * 0.8f, Splash, 1f, ref rng);
                r.Vel = d * rng.Range(4f, 11f);
                r.Motion = new Vector4(16f, 0.6f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.6f, 1f, rng.Range(0.02f, 0.04f));
                r.Extra.y = i % 4 == 0 ? 1f : 2f;
                renderer.Spawn(kind, r);
            }

            // ---- the splash mass while the beam lands (frames 40-94): sheets of goo thrown out of the hit all round
            // it, on the way the beam goes more than back, each a pointed leaf that lives a few frames, of every tone
            // and seen through a little; together they make the clip's mass of sheets
            int sheets = far ? 0 : mid ? 90 : 260;
            for (int i = 0; i < sheets; i++)
            {
                Vector3 d = rng.OnSphere();
                d = (d + dir * 0.35f + Vector3.up * 0.25f).normalized;
                float rad = rng.Range(0.12f, 0.30f);
                var r = Rec(now, AtFrame(late, rng.Range(40f, 94f)), rng.Range(0.12f, 0.28f), AcidBeamKind.Drop,
                    target + Vector3.up * rng.Range(-0.5f, 0.5f) + across * rng.Range(-0.3f, 0.3f) - away * 0.6f, Splash, 0.85f, ref rng);
                r.Vel = d * rng.Range(2.5f, 7f);
                r.Motion = new Vector4(4f, 3f, 0f, 0f);
                r.Size = new Vector4(rad * 0.6f, rad, 0.5f, rng.Range(0.06f, 0.12f));
                float t = rng.Range(0f, 1f);
                r.Tone.w = t < 0.35f ? 0f : t < 0.60f ? 1f : t < 0.85f ? 2f : 3f;
                r.Extra.y = 2f;
                renderer.Spawn(kind, r);
            }
            // ... round a hot core where the beam goes in: a small pale star, new on every drawing (frames 35-93)
            if (!far)
            {
                float cb = AtFrame(late, 35f), cg = AtFrame(late, 93f);
                var r = Rec(now, cb, cg - cb, AcidBeamKind.Splat, target + Vector3.up * 0.1f, Splash, 1f, ref rng);
                r.Size = new Vector4(0.9f, 1.2f, 1f, 1f);
                r.Axis = new Vector3(1f, 0.75f, 0f);
                r.Extra.y = 1.0f;
                r.Extra.z = 0.14f;
                renderer.Spawn(kind, r);
            }

            // ---- the starbursts
            if (!far)
                for (int i = 0; i < Stars.Length; i += StarStride)
                {
                    var r = Rec(now, AtFrame(late, Stars[i]), Stars[i + 1] / 24f, AcidBeamKind.Splat, target + Vector3.up * 0.2f, Splash, 1f, ref rng);
                    r.Size = new Vector4(Stars[i + 2], Stars[i + 3], 0.6f, 1f);
                    r.Axis = new Vector3(Stars[i + 5], 0.95f, 0f);
                    r.Extra.y = 0.8f;
                    r.Extra.z = Stars[i + 4];
                    renderer.Spawn(kind, r);
                }

            // ---- the beam's end: big glossy blobs it breaks into, falling and carried on (frames 88-112)
            // (of every size: most small, a few very big)
            int ends = Count(26, n);
            for (int i = 0; i < ends; i++)
            {
                float h = rng.Range(0f, 1f);
                float rad = 0.10f + 0.45f * h * h;
                float at = delay + JetSeconds - BreakUp * rng.Range(0.05f, 1.0f);
                var r = Rec(now, at, rng.Range(0.5f, 0.9f), AcidBeamKind.Drop, nozzle + Vector3.up * rng.Range(-0.8f, 0.8f), Blob, 1f, ref rng);
                r.Vel = vel * rng.Range(0.7f, 1.0f) + Vector3.up * rng.Range(-2.5f, 2.5f) + across * rng.Range(-2.5f, 2.5f);
                r.Motion = new Vector4(5f, 1.0f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.9f, 1f, 0.01f);
                r.Extra.y = 3f;
                renderer.Spawn(kind, r);
            }
            // ... with many small bright beads among them
            int endBeads = Count(70, n);
            for (int i = 0; i < endBeads; i++)
            {
                float rad = rng.Range(0.03f, 0.08f);
                var r = Rec(now, delay + JetSeconds - BreakUp * rng.Range(0.0f, 1.0f), rng.Range(0.4f, 0.8f), AcidBeamKind.Drop,
                    nozzle + Vector3.up * rng.Range(-0.8f, 0.8f), Jet, 1f, ref rng);
                r.Vel = vel * rng.Range(0.6f, 1.0f) + Vector3.up * rng.Range(-3f, 3f) + across * rng.Range(-3f, 3f);
                r.Motion = new Vector4(4f, 1.5f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.8f, 1f, 0f);
                r.Tone.w = 2f + (i & 1);
                r.Extra.y = 1f;
                renderer.Spawn(kind, r);
            }
            // ... and the bubbles in the lump of goo the beam leaves on the target (frames 92-110)
            int lumpBlobs = mid ? 0 : 22;
            for (int i = 0; i < lumpBlobs; i++)
            {
                float rad = rng.Range(0.12f, 0.40f);
                Vector3 at = ground + right * rng.Range(-1.3f, 1.3f) + Vector3.up * rng.Range(0.4f, 2.6f) - away * rng.Range(0.6f, 1.2f);
                var r = Rec(now, AtFrame(late, rng.Range(92f, 104f)), rng.Range(0.25f, 0.45f), AcidBeamKind.Drop, at, Blob, 1f, ref rng);
                r.Vel = Vector3.up * rng.Range(-0.3f, 0.5f) + right * rng.Range(-0.3f, 0.3f);
                r.Motion = new Vector4(0f, 0.5f, 0f, 0f);
                r.Size = new Vector4(rad * 0.6f, rad, 0.5f, 0f);
                r.Extra.y = 3f;
                renderer.Spawn(kind, r);
            }

            // ---- the smoke: a bank of it rising off the beam's top all the while it runs (frames 30-110) ...
            int bank = far ? 3 : mid ? 12 : 36;
            for (int i = 0; i < bank; i++)
            {
                float u = rng.Range(0.10f, 0.85f);                       // where along the beam (0 the nozzle side, 1 the target)
                float pass = delay + flight * u;                         // the head goes over it
                float at = rng.Range(pass + 0.05f, delay + JetSeconds - 0.4f);
                // (b01's sat on the beam: the clip's bank is 2-4 m over it by frame 50, and thins away by frame 100)
                Vector3 on = Vector3.Lerp(nozzle, target, u) + Vector3.up * (Mathf.Lerp(HalfNozzle, HalfTarget, u) * 0.7f + 1.0f);
                float size = rng.Range(0.6f, 1.0f);
                var r = Rec(now, at, rng.Range(1.0f, 1.5f), AcidBeamKind.Puff, on, Fume, FumeOpacity * 0.75f, ref rng);
                r.Vel = Vector3.up * rng.Range(1.6f, 2.6f) + dir * rng.Range(0.0f, 0.6f);
                r.Motion = new Vector4(0f, 0.6f, 0f, 0f);
                r.Size = new Vector4(size * 0.5f, size * 1.6f, 0.7f, 1f);
                r.Extra.y = 0.5f;
                r.Extra.z = 0.15f;
                renderer.Spawn(kind, r);
            }
            // ... and the plume off the target after it, leaning to the eye's right (frames 96-150)
            int plume = far ? 2 : mid ? 7 : 16;
            float end = AtFrame(late, 152f);
            for (int i = 0; i < plume; i++)
            {
                float u = i / (float)(plume - 1);
                float at = AtFrame(late, Mathf.Lerp(94f, 128f, u));
                var r = Rec(now, at, Mathf.Min(rng.Range(1.3f, 1.8f), end - at), AcidBeamKind.Puff,
                    ground + Vector3.up * rng.Range(1.8f, 2.6f) + right * rng.Range(-0.2f, 0.3f) - away * 0.4f, Fume, FumeOpacity, ref rng);
                r.Vel = Vector3.up * rng.Range(2.6f, 3.4f) + right * rng.Range(1.8f, 2.6f);
                r.Motion = new Vector4(0f, 0.7f, 0f, 0f);
                r.Size = new Vector4(0.55f, 1.25f * (1f - 0.3f * u), 0.7f, 1f);
                r.Extra.y = 0.45f;
                r.Extra.z = 0.2f;
                renderer.Spawn(kind, r);
            }

            // ---- the fizz: flecks rising round the target as the goo sinks (frames 106-136), and a few glossy bubbles
            // floating up with them
            int fizz = mid ? 0 : 32;
            for (int i = 0; i < fizz; i++)
            {
                float rad = rng.Range(0.03f, 0.06f);
                var r = Rec(now, AtFrame(late, rng.Range(104f, 132f)), rng.Range(0.35f, 0.6f), AcidBeamKind.Drop,
                    ground + right * rng.Range(-1.2f, 1.2f) + Vector3.up * rng.Range(0.3f, 2.4f) - away * rng.Range(0.3f, 1.0f), Splash, 1f, ref rng);
                r.Vel = Vector3.up * rng.Range(0.6f, 1.6f) + right * rng.Range(-0.3f, 0.3f);
                r.Motion = new Vector4(-0.6f, 0f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.5f, 1f, 0f);
                r.Extra.y = 1f;
                renderer.Spawn(kind, r);
            }
            int floats = mid ? 0 : 5;
            for (int i = 0; i < floats; i++)
            {
                float rad = rng.Range(0.12f, 0.22f);
                var r = Rec(now, AtFrame(late, rng.Range(108f, 128f)), rng.Range(0.5f, 0.9f), AcidBeamKind.Drop,
                    ground + right * rng.Range(-1.6f, 1.6f) + Vector3.up * rng.Range(1.0f, 2.6f) - away * rng.Range(0.3f, 1.0f), Blob, 1f, ref rng);
                r.Vel = Vector3.up * rng.Range(0.3f, 0.8f) + right * rng.Range(-0.4f, 0.4f);
                r.Motion = new Vector4(0f, 0.3f, 0f, 0f);
                r.Size = new Vector4(rad, rad, 1f, 0f);
                r.Extra.y = 3f;
                renderer.Spawn(kind, r);
            }
        }

        static int Count(int close, float share) => Mathf.RoundToInt(close * share);

        /// <summary>One light (the shader's glow): alive from `born` to `gone` seconds after the call, coming up over
        /// `rise` seconds and going over the last `fall`. Its reach: `along` its level `axis`, `up` and down,
        /// `behind` square to the axis on the near side, `ahead` of that on the far side. `into` is multiplied into
        /// what it lights, `add` added. A `level` light falls on level ground only.</summary>
        static void Glow(CelFxRenderer renderer, int kind, float now, float born, float gone, Vector3 at, Vector3 axis,
            float along, float up, float behind, float ahead, float strength, Vector3 into, Vector3 add, float rise, float fall, bool level, uint seed)
        {
            var own = new CelRng(seed);   // a light has nothing random in it: the effect's own draws stay as they were
            var r = Rec(now, born, gone - born, AcidBeamKind.Glow, at, Pool, strength, ref own);
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
