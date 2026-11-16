// Phase: CelFx acid beam (2026-11-15) - a straight beam of acid from the eye's left onto a target: glossy blobs bud off
// its top, a crown of blades of goo grows out of where it lands, sheets of goo, lime streaks, droplets and sparks fly
// over the target, a bank of green smoke rises off the beam, the whole beam breaks up at once into glossy blobs and
// droplets that hang where it was, and the goo sinks to a bubbly lump and a yellow-rimmed puddle under a plume of
// smoke (reference clip acid_beam_cel_00001_, 8 s at 24 fps, drawn on ones).
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
        public const float HalfNozzle = 1.20f, HalfTarget = 0.76f;   // (b19: 0.92 filmed a fifth narrow at the frame's edge)
        /// <summary>Seconds of fluid: the head's cone (the first fluid is thin: the clip's beam is 0.36 of its width
        /// 0.02 s behind the point, 0.78 at 0.21 s), the tail's taper; and the seconds the whole beam takes to break
        /// up from frame BreakFrame (the clip's is a lace end to end on frames 85-88 and blobs by 93; b02-b05 broke
        /// its last 0.6 s of fluid up as it flew on into the target).</summary>
        const float HeadTaper = 0.45f, TailTaper = 0.05f, BreakUp = 0.85f, BreakFrame = 81f;   // (b22: 85 -> 84, and it opens fast)   // (b06's 0.33 was gone by f92, b07's 0.45 by f94)
        /// <summary>The puddle's half-extent across (m) and its depth over its width (the clip: a slab 3.6 m across
        /// round the target's foot, 260 x 90 px at frame 125; b01 filmed a little big) and how far to the eye's right
        /// of the foot its middle lies (the clip's: 50 px).</summary>
        public const float PuddleReach = 3.6f, PuddleDepth = 0.75f, PuddleRight = 0.15f;   // (b11: 0.7 lay 40 px right of the clip's)

        // A colour as the shader has to write it (the project is gamma, the palette stores .linear). The values are
        // the clip's colours taken back through the Range's night grade; the comments give what they show as.
        static Color S(float r, float g, float b) => new Color(Mathf.LinearToGammaSpace(r), Mathf.LinearToGammaSpace(g), Mathf.LinearToGammaSpace(b), 1f);

        // the beam: rim #427C38 (b09: the clip's dark-green streaks; b02-b08 #48913E), green #5BBE52, lime #9DD957,
        // yellow #DEF14B, hot yellow #F8F26A, pale streaks #E5F794
        static readonly int Jet = CelPalette.Add("acid_beam_jet",
            S(0.212f, 0.392f, 0.211f), S(0.287f, 0.595f, 0.283f), S(0.488f, 0.708f, 0.302f), S(0.628f, 0.844f, 0.286f), S(0.698f, 0.855f, 0.361f), 1.1f, 4, S(0.620f, 0.887f, 0.474f));
        // the splash over the target, its lime half: #679952, #85C260, #99E46E, #C3F26F, pale #DCF88F, yellow #D4F65C (b05:
        // the clip's mass measured in seven tones, frames 46-90 over the target: 22 % #99E46E, 16 % #C3F26F, 15 % each
        // #679952 and #85C260, 11 % #DCF88F; b04 drew 38 % yellow where the clip has 22 %)
        static readonly int Splash = CelPalette.Add("acid_beam_splash",
            S(0.318f, 0.469f, 0.278f), S(0.412f, 0.611f, 0.319f), S(0.464f, 0.762f, 0.361f), S(0.547f, 0.848f, 0.370f), S(0.593f, 0.893f, 0.460f), 1.1f, 4, S(0.583f, 0.876f, 0.326f));
        // ... and its green half, the flames to the far side and round its rim: line #28512C, shade #43A854, body #50CB67,
        // light #76EA76, pale #A6EB80, #6FDB6D (b17: the clip's f48-56 right of the target, 15-22 % #28512C, 27 % #50CB67,
        // 23 % #76EA76; b04-b16's #3A6232 to #ACEA7C read lime)
        static readonly int Sheet = CelPalette.Add("acid_beam_sheet",
            S(0.123f, 0.273f, 0.176f), S(0.211f, 0.517f, 0.284f), S(0.258f, 0.646f, 0.337f), S(0.360f, 0.793f, 0.382f), S(0.486f, 0.802f, 0.410f), 1.1f, 4, S(0.350f, 0.716f, 0.355f));
        // the lime streaks thrown up out of the splash: edge #A5E146, body #C8F05A, #DCF778, hot #EEFCAF
        static readonly int Streak = CelPalette.Add("acid_beam_streak",
            S(0.372f, 0.596f, 0.232f), S(0.505f, 0.747f, 0.265f), S(0.568f, 0.835f, 0.318f), S(0.599f, 0.885f, 0.397f), S(0.627f, 0.926f, 0.561f), 1.1f, 4, S(0.568f, 0.835f, 0.318f));
        // the puddle: edge #469644, body #51C55A, patches #79D157, #B0DD53, its yellow rim #EBFA56, the line inside it #CEF584
        static readonly int Pool = CelPalette.Add("acid_beam_pool",
            S(0.220f, 0.462f, 0.242f), S(0.259f, 0.626f, 0.304f), S(0.378f, 0.676f, 0.299f), S(0.544f, 0.728f, 0.297f), S(0.635f, 0.908f, 0.317f), 1.1f, 4, S(0.565f, 0.877f, 0.430f));
        // a glossy blob (b06: the clip's big one at the beam's end, f92): its dark line #285C30, inside #3D9749 and
        // #48AA54, its ring #A4ED59 and that ring's shade side #78CD48, the glint #FCFAF0 (b02-b05's were pale balls)
        static readonly int Bubble = CelPalette.Add("acid_beam_bubble",
            S(0.123f, 0.305f, 0.188f), S(0.191f, 0.465f, 0.255f), S(0.226f, 0.523f, 0.284f), S(0.479f, 0.813f, 0.312f), S(0.661f, 0.917f, 0.836f), 1.1f, 4, S(0.374f, 0.655f, 0.263f));
        // the lump (the clip's f102: 7 % #54B454, 7 % #6CCC6C, 6 % #54CC6C, 4 % #84E46C): line #314E3B, shade #54B454,
        // body #66CC6A, light #86E26E, pale #CEFAA0, its yellow flows #CBF748
        static readonly int Lump = CelPalette.Add("acid_beam_lump",
            S(0.160f, 0.263f, 0.216f), S(0.264f, 0.558f, 0.286f), S(0.321f, 0.651f, 0.345f), S(0.413f, 0.751f, 0.360f), S(0.548f, 0.906f, 0.510f), 1.1f, 4, S(0.557f, 0.882f, 0.279f));
        // ... and the bubbles in it and round it: line #315A3B, shade #5AC460, inside #73E67D, lit #A8F078, glint
        // #FBFAF6, the ring's shade side #67D06B
        static readonly int LumpBubble = CelPalette.Add("acid_beam_lump_bubble",
            S(0.159f, 0.299f, 0.217f), S(0.285f, 0.618f, 0.318f), S(0.356f, 0.771f, 0.399f), S(0.481f, 0.832f, 0.391f), S(0.656f, 0.917f, 0.870f), 1.1f, 4, S(0.325f, 0.667f, 0.348f));
        // the smoke (b17: a veil and its ribbons, the clip's f56-104): the veil #5A7E48 with patches of #7CB259 (#3A5036 to
        // #506F44 where one or two lie over the night), its ribbons #96D965, lit by the acid #E0F699, #6FA052 as they age
        // (b02-b16: one see-through tone, #5F834C to #91CD6A; b17 stored a single 0.46 veil's lime, and stacked veils showed it)
        static readonly int Fume = CelPalette.Add("acid_beam_fume",
            S(0.194f, 0.269f, 0.203f), S(0.287f, 0.396f, 0.251f), S(0.415f, 0.611f, 0.314f), S(0.487f, 0.752f, 0.356f), S(0.608f, 0.880f, 0.488f), 1.1f, 4, S(0.341f, 0.491f, 0.278f));   // (b21: dense #86C25E, lit #A0E26C)
        const float FumeOpacity = 1f;   // (b17: the shader sees it through itself - its veil at 0.46, its ribbons at 0.92)
        // a spark: edge #963C28, #CD5F32, body #F09646, #FCC86E, hot #FFEBAA (b05: the clip's few sparks are orange-red
        // lines, b04's were yellow)
        static readonly int Spark = CelPalette.Add("acid_beam_spark",
            S(0.504f, 0.204f, 0.168f), S(0.676f, 0.301f, 0.198f), S(0.836f, 0.462f, 0.254f), S(0.877f, 0.639f, 0.361f), S(0.992f, 0.824f, 0.546f), 1.1f, 4, S(0.566f, 0.233f, 0.182f));

        /// <summary>The light the acid throws on the ground and on what stands there (the neon acid's: multiplied into
        /// what it falls on so the ground keeps its pattern, a little added on top).</summary>
        static readonly Vector3 GlowInto = new Vector3(0.70f, 1.35f, 0.22f), GlowAdd = new Vector3(0.08f, 0.27f, 0.05f);   // (b15: less red - b14's read yellow)   // (b14: greener - the clip's #6EDB5F under the hit, ours #68A151)
        static readonly Vector3 CoatInto = new Vector3(0.45f, 1.3f, 0.05f), CoatAdd = new Vector3(0.16f, 0.45f, 0.06f);
        const float CoatReach = 2.3f;   // (b06: the clip's target is pale lime from its head to its post; b05's head stayed grey)

        // a thin line beside the beam: its place across it (1 is the beam's edge: up the screen, toward the eye), its
        // half-width at the nozzle and at the target (m), its dash duty and dash seconds
        const int LineStride = 6;
        static readonly float[] Lines =
        {
             1.20f,  0.2f, 0.026f, 0.020f, 0.40f, 0.20f,
            -1.12f, -0.1f, 0.075f, 0.040f, 0.72f, 0.26f,   // (b24: strands peeling off under it, the clip's f39-80; b36: one - three read as spears)
        };

        // bursts of blades out of the hit: the clip's frame it starts on, the drawings it throws over, how many blades
        // (close), their speed (m/s) from .. to; the first is the crown (frames 38-43: spikes 100-250 px long that grow
        // out of the hit over three drawings), the rest the clip's later sprays (b04 drew these as pale stars)
        const int BurstStride = 5;
        static readonly float[] Bursts =
        {
            35.5f, 5.5f, 46f, 16f, 30f,
            50f,   3f,   12f, 12f, 22f,
            61f,   3f,   12f, 12f, 22f,
            74f,   4f,   16f, 14f, 26f,
            85f,   3f,   12f, 12f, 22f,
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
                // (b13: from frame 29 and tighter, little on the far side - the clip's ground is dark at f26 and its pool at
                // f42 lies under the hit with a dark patch right of the target; b01-b12 washed the whole ground from f24)
                Glow(renderer, kind, now, AtFrame(late, 28f), AtFrame(late, 100f), foot, across, 6.5f, 6.5f, 6.5f, 1.3f, 0.95f, GlowInto, GlowAdd, 0.15f, 0.20f, true, seed);   // (b15: further onto the far side)   // (b14: b13's 4.8 m left the far side dark)
                // ... under the near half of the beam while it runs (frames 23-96; b01 lit the near ground too much,
                // too soon) ...
                Vector3 under = Vector3.Lerp(nozzle, target, 0.70f);
                under.y = Ground(under) + 1f;
                Glow(renderer, kind, now, AtFrame(late, 25f), AtFrame(late, 96f), under, across, 6f, 6f, 6f, 1f, far ? 0.2f : mid ? 0.25f : 0.32f, GlowInto, GlowAdd * 1.3f, 0.15f, 0.15f, true, seed);
                // ... the puddle's own glow after it, kept low so it does not light the target's head (frames 98-165) ...
                Vector3 after = ground + right * PuddleRight;
                after.y = Ground(after) + 0.4f;
                Glow(renderer, kind, now, AtFrame(late, 98f), AtFrame(late, 165f), after, across, 3.5f, 0.8f, 3.5f, 1f, 0.5f, GlowInto, GlowAdd, 0.2f, 0.8f, true, seed);
                // ... and the target, green from the moment the beam reaches it until the goo has run off it (frames 32-110;
                // the clip's is half up on frame 33)
                if (!far) Glow(renderer, kind, now, AtFrame(late, 32f), AtFrame(late, 110f), target, across, CoatReach, CoatReach, CoatReach, 1f, 1f, CoatInto, CoatAdd, 0.12f, 0.3f, false, seed);
                // ... and its head, which the coat's falloff left an olive ball with dark eyes (b11; the clip's is lime)
                if (!far) Glow(renderer, kind, now, AtFrame(late, 33f), AtFrame(late, 108f), target + Vector3.up * 0.9f, across, 0.9f, 0.9f, 0.9f, 1f, 1f, CoatInto, CoatAdd, 0.12f, 0.3f, false, seed);
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

            // ---- the smoke (b18: drawn first, behind the lump, the beam and the splash - b17's lay over the splash - and
            // from its own draws): a bank of it rising off the beam's top all the while it runs (frames 30-110) ...
            var fume = new CelRng(seed ^ 0x51ED5u);
            int bank = far ? 3 : mid ? 8 : 22;   // (b18: b17's 28 to f125 a wall)                          // (b11: a few big billows, not a bank of cotton)                          // (b02's 36 read as separate puffs, b03's 56 filmed 1.29x the clip's)
            for (int i = 0; i < bank; i++)
            {
                float u = fume.Range(0.28f, 0.90f);                       // where along the beam (0 the nozzle side, 1 the target; b27: b26's near the eye were giant balls)
                float pass = delay + flight * u;                         // the head goes over it
                // (b08: the clip's first smoke is low on the beam's top at frame 40 and small, a soft band on it by f60,
                // big see-through wisps by f84; b02-b07's rose as opaque puffs from the head's pass, 1 m over the beam)
                float lo = Mathf.Max(pass + 0.25f, AtFrame(late, 40f));
                float at = fume.Range(lo, Mathf.Max(lo, AtFrame(late, 70f)));
                Vector3 on = Vector3.Lerp(nozzle, target, u) + Vector3.up * (Mathf.Lerp(HalfNozzle, HalfTarget, u) * 0.6f + 0.35f);
                float size = fume.Range(0.65f, 1.05f) * Mathf.Lerp(0.7f, 1f, u);
                // (b17: wisps to f110-125 and up to the frame's top - the clip's smoke still covers a third of the sky over the
                // beam's end at f80; b16's were gone by f84)
                var r = Rec(now, at, fume.Range(1.3f, 1.8f), AcidBeamKind.Puff, on, Fume, FumeOpacity, ref fume);
                r.Vel = Vector3.up * fume.Range(1.5f, 2.2f) + dir * fume.Range(0.2f, 0.9f);   // (b09: b08's 0.7-1.5 hugged the beam)
                r.Motion = new Vector4(0f, 0.6f, 0f, 0.5f);
                r.Size = new Vector4(size * 0.45f, size * 2.2f, 0.7f, 1f);
                r.Extra.y = 0.30f;
                r.Extra.z = 0.3f;                                         // lit by the beam under it longer (the clip's tops: 169,219,125)
                renderer.Spawn(kind, r);
            }
            // ... and off the splash itself, rising over the target to the frame's top (b17: the clip's f72-96)
            int over = far ? 0 : mid ? 2 : 6;
            for (int i = 0; i < over; i++)
            {
                var r = Rec(now, AtFrame(late, fume.Range(54f, 90f)), fume.Range(1.2f, 1.8f), AcidBeamKind.Puff,
                    target + Vector3.up * fume.Range(0.8f, 2.2f) + right * fume.Range(-1.2f, 1.2f) - away * fume.Range(0.2f, 0.8f), Fume, FumeOpacity, ref fume);
                r.Vel = Vector3.up * fume.Range(1.8f, 2.6f) + right * fume.Range(-0.3f, 0.8f);
                r.Motion = new Vector4(0f, 0.6f, 0f, 0.4f);
                r.Size = new Vector4(0.40f, fume.Range(1.4f, 2.0f), 0.7f, 1f);
                r.Extra.y = 0.30f;
                r.Extra.z = 0.25f;
                renderer.Spawn(kind, r);
            }
            // ... and the plume off the target after it: a tall column off the lump's top right, leaning to the eye's right
            // (b17: the clip's rises to the frame's top f100-130 and is gone by f145; b08-b16's a small wisp)
            int plume = far ? 2 : mid ? 8 : 20;
            float end = AtFrame(late, 150f);
            for (int i = 0; i < plume; i++)
            {
                float u = i / (float)(plume - 1);
                float at = AtFrame(late, Mathf.Lerp(90f, 128f, u));
                var r = Rec(now, at, Mathf.Min(fume.Range(1.2f, 1.7f), end - at), AcidBeamKind.Puff,
                    ground + Vector3.up * fume.Range(1.7f, 2.8f) + right * fume.Range(0.2f, 0.8f) - away * 0.4f, Fume, FumeOpacity, ref fume);   // (b27: off the target's head - b17-b26's hung detached over it)
                r.Vel = Vector3.up * fume.Range(2.4f, 3.2f) + right * fume.Range(0.8f, 1.4f);
                r.Motion = new Vector4(0f, 0.6f, 0f, 0.3f);
                r.Size = new Vector4(0.5f, 1.5f * (1f - 0.3f * u), 0.7f, 1f);
                r.Extra.y = 0.35f;
                r.Extra.z = 0.2f;
                renderer.Spawn(kind, r);
            }

            // ---- the bubbly lump the goo sinks to after the beam (frames 88-113): a round mass 5 m across and 4 m high
            // over the target with bubbles set in it (b06; b02-b05's pillar read as a grass tuft and a tent, a third of
            // the clip's size and left of the target)
            if (!far)
            {
                float lb = AtFrame(late, 88f), lg = AtFrame(late, 119f);   // (b29: the clip's still stands 200 px wide at f110)
                var lump = Rec(now, lb, lg - lb, AcidBeamKind.Mound, stand, Lump, 1f, ref rng);
                lump.Axis = dir;
                lump.Size = new Vector4(4.8f, 4.8f, 1f, 0.90f);   // (b36: smaller under sheets of goo)   // (b07: the lump keeps to 0.86 of its card)
                lump.Motion = new Vector4(0.30f, 0.3f, 0.6f, 0f);
                lump.Tone.y = CelRecord.PackPalette(LumpBubble);
                lump.Extra.z = 0.40f;
                renderer.Spawn(kind, lump);
            }

            // ---- the beam (the whole of it breaks up from frame BreakFrame: its age then)
            float breakAt = AtFrame(late, BreakFrame) - delay;
            {
                var r = Rec(now, delay, JetSeconds + flight + 0.05f, AcidBeamKind.Strand, nozzle, Jet, 1f, ref rng);
                r.Vel = vel;
                r.Motion = new Vector4(Gravity, HeadTaper, TailTaper, BreakUp);
                r.Size = new Vector4(HalfNozzle, HalfTarget, breakAt, flight);
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
                    // (born as the head lands: the clip shows hardly any before the hit)
                    float on = flight;
                    var r = Rec(now, delay + on, JetSeconds - on + flight + 0.05f, AcidBeamKind.Strand, nozzle, Jet, 1f, ref rng);
                    r.Vel = vel;
                    r.Motion = new Vector4(Gravity, 0.03f, 0.03f, BreakUp);
                    r.Size = new Vector4(Lines[i + 2], Lines[i + 3], breakAt - on, flight);
                    r.Axis = new Vector3(Lines[i], Lines[i + 1], rng.Range(0f, 6.2832f));
                    r.Erode = new Vector4(HalfNozzle, HalfTarget, Lines[i + 4], Lines[i + 5]);
                    r.Extra.y = JetSeconds - on;
                    r.Extra.z = 1f;
                    renderer.Spawn(kind, r);
                }

            // ---- glossy blobs that bud off the beam's top near its head and hang back as it goes on (frames 20-48)
            // (b02's trailed at the frame's edge as small balls: the clip's are big and sit on the beam's top)
            // (b10: budding all along its top as the head goes by, light green inside, the big ones near the eye - b02-b09's
            // came out of the nozzle and clumped at the frame's left edge like peas in a pod)
            // (b19: four big ones near the eye and a crest of goo along the rest - b10-b18's 14 read as a row of pearls)
            int blobs = Count(9, n);                                     // (b20: b19's crest read as a hedge; the clip's are a few big blobs)
            for (int i = 0; i < blobs; i++)
            {
                float u = 0.16f + 0.62f * Mathf.Pow(rng.Next(), 1.3f);   // (more near the eye)
                float hb = 0.25f + 0.75f * rng.Next();
                float rad = (0.18f + 0.50f * hb * hb) * Mathf.Lerp(1.5f, 0.7f, u);
                Vector3 at = Along(nozzle, target, flight * u) + Vector3.up * (Mathf.Lerp(HalfNozzle, HalfTarget, u) * 0.45f + rad * 0.2f) + across * rng.Range(-0.3f, 0.3f);
                // (b13: born as the head goes by and sitting on its top, bulging out of it - b10-b12's came late, small,
                // floating over it; the clip's are 100+ px near the eye on f23-27)
                var r = Rec(now, delay + flight * u + rng.Range(0.0f, 0.12f), rng.Range(0.35f, 0.6f), AcidBeamKind.Drop, at, LumpBubble, 1f, ref rng);   // (b28: gone by f40)
                r.Vel = vel * rng.Range(0.04f, 0.10f) + Vector3.up * (hb < 0.7f ? rng.Range(0.1f, 0.7f) : rng.Range(1.2f, 2.2f)) + across * rng.Range(-0.5f, 0.5f);   // (a few lift off it)
                r.Motion = new Vector4(1.5f, 2.0f, 0f, 0f);
                r.Size = new Vector4(rad * 0.6f, rad, 0.6f, 0.05f);
                r.Erode = new Vector4(rng.Next() < 0.4f ? 1f : 0f, 0f, 0f, 0f);
                r.Extra.y = 3f;
                renderer.Spawn(kind, r);
            }
            // ... and a wavy crest of goo along its top as the head goes by: light splats lying along it, rising a little off
            // it (frames 22-46: the clip's beam top is a crest of goo, b19)
            int crest = Count(0, n);                                     // (b20: none - b19's read as a hedge)
            for (int i = 0; i < crest; i++)
            {
                float u = 0.16f + 0.72f * Mathf.Pow(rng.Next(), 1.2f);
                float rad = rng.Range(0.25f, 0.55f) * Mathf.Lerp(1.4f, 0.7f, u);
                Vector3 at = Along(nozzle, target, flight * u) + Vector3.up * (Mathf.Lerp(HalfNozzle, HalfTarget, u) * 0.55f + rad * 0.1f) + across * rng.Range(-0.3f, 0.3f);
                var r = Rec(now, delay + flight * u + rng.Range(0.0f, 0.10f), rng.Range(0.35f, 0.65f), AcidBeamKind.Drop, at, Splash, 1f, ref rng);
                r.Vel = vel * rng.Range(0.05f, 0.10f) + Vector3.up * rng.Range(0.2f, 0.9f) + across * rng.Range(-0.4f, 0.4f);
                float speed = Mathf.Max(r.Vel.magnitude, 0.5f);
                r.Motion = new Vector4(1.5f, 1.2f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.8f, 1f, rng.Range(1.6f, 2.6f) * rad / speed);   // (lying along it)
                float t = rng.Next();
                r.Tone.w = t < 0.25f ? 1f : t < 0.65f ? 2f : t < 0.90f ? 3f : 4f;
                r.Erode = Vector4.zero;
                r.Extra.y = 7f;
                renderer.Spawn(kind, r);
            }
            // ... and goo dripping under it near the eye as the head goes by (frames 24-48; b10: the clip's hang 100-150 px)
            int drips = Count(14, n);
            for (int i = 0; i < drips; i++)
            {
                float u = rng.Range(0.14f, 0.32f);                       // (b24: near the eye only - b23's hung all along it at f40)
                float rad = rng.Range(0.28f, 0.60f) * Mathf.Lerp(1.3f, 0.8f, u);   // (b13: the clip's hang 100-150 px near the eye)
                Vector3 at = Along(nozzle, target, flight * u) - Vector3.up * (Mathf.Lerp(HalfNozzle, HalfTarget, u) * 0.55f) + across * rng.Range(-0.3f, 0.3f);
                var r = Rec(now, delay + flight * u + rng.Range(0.05f, 0.30f), rng.Range(0.30f, 0.45f), AcidBeamKind.Drop, at, Splash, 1f, ref rng);
                r.Vel = vel * rng.Range(0.08f, 0.15f) + Vector3.down * rng.Range(0.3f, 1.2f);
                r.Motion = new Vector4(4f, 1.5f, 0f, 0f);
                r.Size = new Vector4(rad * 0.8f, rad * 0.5f, 0.6f, rng.Range(0.12f, 0.2f));
                float t = rng.Next();
                r.Tone.w = t < 0.4f ? 1f : t < 0.8f ? 2f : 3f;
                r.Erode = new Vector4(rng.Next() < 0.3f ? 1f : 0f, 0f, 0f, 0f);
                r.Extra.y = 6f;
                renderer.Spawn(kind, r);
            }

            // ---- beads thrown off it all along while it runs: small round flecks, most of them under it
            int beads = Count(220, n);                                   // (b21: the clip sprays many round the beam f30-48)
            for (int i = 0; i < beads; i++)
            {
                float hb = rng.Next();
                float rad = 0.04f + 0.11f * hb * hb;                     // (b24: every size to 20 px - the clip's; b23's were dots)
                var r = Rec(now, delay + rng.Range(0.0f, JetSeconds - 0.1f), rng.Range(0.35f, 0.7f), AcidBeamKind.Drop, nozzle, Jet, 1f, ref rng);
                r.Vel = vel * rng.Range(0.55f, 1.0f) + Vector3.up * rng.Range(-4f, 2.5f) + across * rng.Range(-3f, 3f);
                r.Motion = new Vector4(9f, 1.2f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.8f, 1f, 0.004f);
                r.Tone.w = 2f + (i & 1);                                 // light and yellow (the clip's beads are bright)
                r.Extra.y = 1f;
                renderer.Spawn(kind, r);
            }

            // ---- sparks: a few orange-red lines thrown out of the hit while the beam lands (frames 44-96; b04 drew 240
            // short yellow dashes: the clip has about 13 lines a frame round the hit, its longest 36 px)
            int sparks = Count(70, n);
            for (int i = 0; i < sparks; i++)
            {
                Vector3 d = rng.OnSphere();
                d.y = Mathf.Abs(d.y) * 1.1f + 0.15f;
                d = (d + dir * 0.3f).normalized;
                float rad = rng.Range(0.015f, 0.028f);
                var r = Rec(now, AtFrame(late, rng.Range(44f, 96f)), rng.Range(0.18f, 0.35f), AcidBeamKind.Drop,
                    target + Vector3.up * rng.Range(-0.2f, 0.8f) - away * 0.5f, Spark, 1f, ref rng);
                r.Vel = d * rng.Range(10f, 20f);
                r.Motion = new Vector4(9f, 1.0f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.7f, 1f, 0.022f);
                r.Extra.y = 4f;
                renderer.Spawn(kind, r);
            }
            // ... and lime dashes streaming along the beam
            int streams = Count(14, n);                                  // (b24: a few - b23's 50 read as speed lines)
            for (int i = 0; i < streams; i++)
            {
                float rad = rng.Range(0.025f, 0.04f);
                var r = Rec(now, delay + rng.Range(0.3f, JetSeconds - 0.2f), flight * rng.Range(0.6f, 0.95f), AcidBeamKind.Drop,
                    nozzle + Vector3.up * rng.Range(-1.4f, 1.4f) + across * rng.Range(-1.2f, 1.2f), Streak, 1f, ref rng);
                r.Vel = vel * rng.Range(1.0f, 1.25f);
                r.Motion = new Vector4(0f, 0f, 0f, 0f);
                r.Size = new Vector4(rad, rad, 1f, 0.03f);
                r.Extra.y = 4f;
                renderer.Spawn(kind, r);
            }

            // ---- blades of goo thrown out of the hit (frames 38-92): the crown, the later sprays, and loose blades all
            // the while; each grows out of where the beam goes in, then lets go and flies (b04's lens leaves read as a
            // flower: the clip's are pointed flames, 30-250 px long, pale where they leave the hot heart)
            if (!far)
            {
                for (int b = 0; b < Bursts.Length; b += BurstStride)
                {
                    int count = Count((int)Bursts[b + 2], b == 0 ? n : b < 3 * BurstStride ? n * 0.45f : n * 0.15f);   // (b29: the hit's first starburst whole)   // (b27: the clip has a few thin spikes; b07-b26's read as darts)
                    for (int i = 0; i < count; i++)
                        Blade(renderer, kind, now, AtFrame(late, Bursts[b] + rng.Range(0f, Bursts[b + 1])), target, dir, away,
                            rng.Range(Bursts[b + 3], Bursts[b + 4]), b == 0, ref rng);
                }
                int loose = Count(10, n);                                 // (b07: the clip's mass is spikes out of the hit; b19: b07-b18's 220 a spiky ball)
                for (int i = 0; i < loose; i++)
                    Blade(renderer, kind, now, AtFrame(late, rng.Range(44f, 92f)), target, dir, away, rng.Range(8f, 18f), false, ref rng);
            }

            // ---- big green sheets thrown to the far side as the crown stands (frames 39-45: the clip's dark sheets
            // to the right of the target)
            int wings = far ? 0 : mid ? 5 : 14;
            for (int i = 0; i < wings; i++)
            {
                Vector3 d = (dir + Vector3.up * rng.Range(-0.2f, 0.6f) + across * rng.Range(-0.4f, 0.4f)).normalized;
                float rad = rng.Range(0.45f, 0.75f);
                var r = Rec(now, AtFrame(late, rng.Range(39f, 45f)), rng.Range(0.25f, 0.40f), AcidBeamKind.Drop,
                    target + Vector3.up * rng.Range(-0.2f, 1.0f) + dir * rng.Range(0.3f, 0.9f) + away * 0.1f, Sheet, 1f, ref rng);
                r.Vel = d * rng.Range(5f, 10f);
                r.Motion = new Vector4(2f, 3.5f, 0f, 0f);
                r.Size = new Vector4(rad * 0.7f, rad, 0.6f, rng.Range(0.10f, 0.18f));
                r.Tone.w = rng.Next() < 0.5f ? 2f : 1f;
                r.Erode = Vector4.zero;                                   // (no hole: one reads as an eye; no line)
                r.Extra.y = 7f;                                           // (b18: a splat)
                renderer.Spawn(kind, r);
            }

            // ---- the splash mass while the beam lands (frames 40-94): sheets of goo thrown out of the hit, round lobes
            // run into one body with a tail, up and on the way the beam goes, in the clip's seven tones (its lime half
            // and its green half), some with a dark line round them
            // (b03-b04: lens leaves in a compact flower, 0.65 of the clip's cover over the target)
            // (b06: b05's 290 small short-lived sheets slowed to round blobs with night between them, 0.71 of the clip's
            // cover; the clip's mass is 300-450 px across, 3 m left of the target to 3.7 m right of it, and 4 m high)
            // (b07: b06's 430 sheets up to 2.2 m over the target read as a dark bush as wide at its top as at its foot -
            // the clip's mass is a cone, 282/234/130 px wide from its foot up, thrown out from the hit; its darker
            // sheets lie on the far side)
            // (b26) the crown: big sheets of goo growing out of the hit all the while the beam lands, full of holes that open
            // as they go (the clip's mass is one connected mass of such sheets 350-400 px across, green with a yellow heart;
            // b18-b25's was a ball of small chunks)
            int crown = far ? 0 : mid ? 14 : 40;                         // (b27: 26 -> 40, bigger)
            for (int i = 0; i < crown; i++)
            {
                float when = i < 6 ? rng.Range(40.5f, 44f) : rng.Range(44f, 74f);   // (b30: to f74 - the clip's splash is thin splatters after)   // (b29: after the starburst)
                Vector3 d = (Vector3.up * rng.Range(0.15f, 1.0f) + right * rng.Range(-1.0f, 1.0f) - away * rng.Range(-0.2f, 0.6f) + dir * 0.25f).normalized;
                bool green = rng.Next() < 0.7f;
                float rad = green ? rng.Range(1.2f, 2.3f) : rng.Range(0.7f, 1.2f);
                if (!green) d = (d - dir * 0.8f).normalized;             // (the lime ones: its heart, toward the beam)
                float speed = rng.Range(1.6f, 3.6f);
                var r = Rec(now, AtFrame(late, when), when > 62f ? rng.Range(0.40f, 0.60f) : rng.Range(0.50f, 0.85f), AcidBeamKind.Drop, target + Vector3.up * rng.Range(-0.3f, 0.5f) + d * (rad * 0.3f), green ? Sheet : Splash, 1f, ref rng);
                r.Vel = d * speed;
                r.Motion = new Vector4(1.5f, 0.9f, 0f, 0f);
                r.Size = new Vector4(rad * 0.45f, rad * 1.1f, 0.55f, rng.Range(1.7f, 2.6f) * rad / speed);   // (b32: flames of goo along their flight - b26-b31's were round-lobed blobs)
                float t = rng.Next();
                r.Tone.w = green ? (t < 0.60f ? 2f : 3f) : (t < 0.4f ? 3f : 4f);
                r.Erode = new Vector4(1f, 0f, 0f, 0f);
                r.Extra.y = 7f;
                renderer.Spawn(kind, r);
            }
            int sheets = far ? 0 : mid ? 20 : 50;                        // (b08: most of the mass is tongues now, below; b20: 170 splats; b26: 80 - the crown is the mass)
            for (int i = 0; i < sheets; i++)
            {
                float when = rng.Range(40f, 94f);
                Vector3 d = rng.OnSphere();
                d -= away * (Vector3.Dot(d, away) * 0.5f);
                d = (d + dir * 0.7f + Vector3.up * 0.45f).normalized;
                float rad = 0.35f + 0.60f * rng.Next();                  // (b20: bigger)
                Vector3 at = target + Vector3.up * rng.Range(-0.6f, 1.0f) + right * rng.Range(-1.0f, 1.3f) + dir * rng.Range(0f, 0.6f) - away * rng.Range(-0.3f, 0.8f);
                Vector3 o = at - target;
                o -= away * Vector3.Dot(o, away);
                bool green = rng.Next() < (Vector3.Dot(o, right) > 0.3f ? 0.6f : 0.2f);
                var r = Rec(now, AtFrame(late, when), rng.Range(0.22f, 0.45f), AcidBeamKind.Drop, at, green ? Sheet : Splash, 1f, ref rng);
                r.Vel = (o.normalized * 0.65f + d * 0.35f).normalized * rng.Range(2f, 6f);   // out from the hit
                r.Motion = new Vector4(3f, 1.3f, 0f, 0f);
                r.Size = new Vector4(rad * 0.8f, rad, 0.6f, rng.Range(0.18f, 0.30f));
                float t = rng.Next();
                r.Tone.w = green ? (t < 0.45f ? 1f : t < 0.85f ? 2f : 3f)
                                 : (t < 0.25f ? 1f : t < 0.65f ? 2f : t < 0.92f ? 3f : 4f);   // (b20: brighter - the clip's mass is lime-yellow at its heart; b21: a little less)
                r.Erode = Vector4.zero;                                   // (b17: no line round them)
                r.Extra.y = 7f;                                           // (b18: a splat - b05-b17's sheets read as clubs)
                renderer.Spawn(kind, r);
            }
            // ... and tongues of goo thrown out of the hit among them (b08: the clip's mass is flame tongues pointing out of the
            // hit; b06-b07's round-headed sheets read as green clubs)
            int tongues = far ? 0 : mid ? 24 : 54;                      // (b26: 200 -> 120)
            for (int i = 0; i < tongues; i++)
            {
                Vector3 at = target + Vector3.up * rng.Range(-0.5f, 0.9f) + right * rng.Range(-0.6f, 0.9f) + dir * rng.Range(0f, 0.5f) - away * rng.Range(-0.2f, 0.7f);
                bool green = rng.Next() < (Vector3.Dot(at - target, right) > 0.3f ? 0.70f : 0.50f) + 0.25f * Mathf.Clamp01((at - target).magnitude - 0.7f);   // (b15: more green; b17: green round its rim)
                if (i % 3 == 0) Splat(renderer, kind, now, AtFrame(late, rng.Range(40f, 94f)), at, target, away, green, rng.Range(0.25f, 0.50f), ref rng);   // (b18)
                else Tongue(renderer, kind, now, AtFrame(late, rng.Range(40f, 94f)), at, target, away, green, ref rng);
            }
            // ... and late in it, goo thrown low along the ground round the target's foot (frames 72-94: the clip's splash
            // spreads low and wide on the ground before the lump; b19 - b18's stood tall)
            // (b35) ... and late in it, holed sheets of goo thrown low and wide round the target (frames 74-93: the clip's
            // splash is a green web of strands then; b18-b34's was a pile of small pale chunks)
            // (b36) ... as a splatter: thin wavy strands of goo running out low and wide from the target's foot (drop kind 8)
            int splatters = far ? 0 : mid ? 3 : 7;
            for (int i = 0; i < splatters; i++)
            {
                var r = Rec(now, AtFrame(late, 72f + 3f * i + rng.Range(0f, 2f)), rng.Range(0.45f, 0.65f), AcidBeamKind.Drop,
                    ground + Vector3.up * rng.Range(0.5f, 0.9f) + right * rng.Range(-0.3f, 0.3f) - away * 0.3f, i % 3 == 2 ? Sheet : Splash, 1f, ref rng);
                r.Vel = Vector3.zero;
                r.Motion = Vector4.zero;
                float rad = rng.Range(2.8f, 3.6f);
                r.Size = new Vector4(rad, rad, 1f, 0f);
                r.Tone.w = i % 3 == 2 ? 2f : rng.Next() < 0.5f ? 2f : 3f;
                r.Erode = Vector4.zero;
                r.Extra.y = 8f;
                renderer.Spawn(kind, r);
            }
            int web = far ? 0 : mid ? 3 : 8;
            for (int i = 0; i < web; i++)
            {
                Vector3 d = (Vector3.up * rng.Range(0.0f, 0.7f) + right * rng.Range(-1.0f, 1.0f) - away * rng.Range(-0.2f, 0.5f)).normalized;
                float rad = rng.Range(0.8f, 1.5f);
                float speed = rng.Range(1.5f, 3.2f);
                bool green = rng.Next() < 0.75f;
                var r = Rec(now, AtFrame(late, rng.Range(74f, 93f)), rng.Range(0.40f, 0.65f), AcidBeamKind.Drop, target + Vector3.up * rng.Range(-0.8f, 0.3f) + d * (rad * 0.3f), green ? Sheet : Splash, 1f, ref rng);
                r.Vel = d * speed;
                r.Motion = new Vector4(2.5f, 0.9f, 0f, 0f);
                r.Size = new Vector4(rad * 0.45f, rad * 1.1f, 0.55f, rng.Range(1.7f, 2.6f) * rad / speed);
                float t = rng.Next();
                r.Tone.w = green ? (t < 0.60f ? 2f : 3f) : (t < 0.4f ? 3f : 4f);
                r.Erode = new Vector4(1.25f, 0f, 0f, 0f);                 // (holed from the start)
                r.Extra.y = 7f;
                renderer.Spawn(kind, r);
            }
            int low = far ? 0 : mid ? 8 : 20;                           // (b30: the clip's f78-92 splash is these, low and wide)
            for (int i = 0; i < low; i++)
            {
                float side = rng.Next() < 0.5f ? -1f : 1f;
                Vector3 at = ground + Vector3.up * rng.Range(0.15f, 0.7f) + right * (side * rng.Range(0.1f, 0.9f)) - away * rng.Range(-0.2f, 0.6f);
                Splat(renderer, kind, now, AtFrame(late, rng.Range(74f, 93f)), at, ground + Vector3.up * 0.3f, away, rng.Next() < 0.60f, rng.Range(0.22f, 0.45f), ref rng);
            }
            // ... and late in it, tongues shooting up out of the mass (frames 74-96: the clip's splash grows 4-5 m tall; b10)
            int rising = far ? 0 : mid ? 4 : 10;                        // (b12: b10's 70 to f96 read as a porcupine at f84-90)
            for (int i = 0; i < rising; i++)
            {
                Vector3 at = target + Vector3.up * rng.Range(0.4f, 2.4f) + right * rng.Range(-1.0f, 1.2f) - away * rng.Range(-0.2f, 0.6f);
                Tongue(renderer, kind, now, AtFrame(late, rng.Range(70f, 86f)), at, target - Vector3.up * 1.5f, away, rng.Next() < 0.3f, ref rng);
            }
            // ... and tongues of goo running down out of the mass as the beam runs out (frames 84-97: the clip's mass turns
            // to tall streaks of goo running down before it sinks to the lump; b14 - b12-b13's slow sheets read as a pile
            // of round blobs)
            int goo = far ? 0 : mid ? 6 : 16;
            for (int i = 0; i < goo; i++)
            {
                Vector3 at = target + Vector3.up * rng.Range(0.6f, 3.0f) + right * rng.Range(-1.3f, 1.5f) - away * rng.Range(-0.2f, 0.7f);
                Tongue(renderer, kind, now, AtFrame(late, rng.Range(84f, 97f)), at, at + Vector3.up * 3f + right * rng.Range(-0.8f, 0.8f), away, rng.Next() < 0.7f, ref rng);
            }
            // ... round a hot core where the beam goes in: a small pale star, new on every drawing (frames 35-93)
            if (!far)
            {
                float cb = AtFrame(late, 35f), cg = AtFrame(late, 93f);
                var r = Rec(now, cb, cg - cb, AcidBeamKind.Splat, target + Vector3.up * 0.1f, Splash, 1f, ref rng);
                r.Size = new Vector4(0.9f, 1.2f, 1f, 1f);                 // (b11: b08's 1.3-1.6 read as a cartoon star; the pale heart is the beam's fan and the pale blades)
                r.Axis = new Vector3(1f, 0.75f, 0f);
                r.Extra.y = 1.0f;
                r.Extra.z = 0.08f;                                        // (b04's 0.14 drew a star's broad spikes)
                renderer.Spawn(kind, r);
            }
            // ... and a big pale heart where the beam goes in as the crown stands (frames 37-47: the clip's pale-yellow
            // heart is 100-150 px across; b05 had none)
            if (!far)
            {
                float hb = AtFrame(late, 37f), hg = AtFrame(late, 47f);
                var r = Rec(now, hb, hg - hb, AcidBeamKind.Splat, target + Vector3.up * 0.1f, Streak, 1f, ref rng);
                r.Size = new Vector4(0.9f, 1.3f, 1f, 1f);
                r.Axis = new Vector3(1f, 0.70f, 0f);
                r.Extra.y = 1.2f;
                r.Extra.z = 0.16f;
                renderer.Spawn(kind, r);
            }

            // ---- the spray: lime streaks thrown up and out of the splash while the beam lands (frames 38-96), the
            // clip's most numerous piece round the hit: 15-35 px dashes, most of them going up
            int spray = Count(220, n);
            for (int i = 0; i < spray; i++)
            {
                Vector3 d = rng.OnSphere();
                d.y = Mathf.Abs(d.y) * 1.4f + 0.3f;
                d = (d + dir * 0.35f).normalized;
                float rad = rng.Range(0.028f, 0.05f);
                var r = Rec(now, AtFrame(late, rng.Range(38f, 96f)), rng.Range(0.25f, 0.45f), AcidBeamKind.Drop,
                    target + Vector3.up * rng.Range(-0.3f, 1.4f) + across * rng.Range(-0.8f, 0.8f) + dir * rng.Range(0f, 1.2f) - away * rng.Range(0.2f, 0.9f),
                    Streak, 1f, ref rng);
                r.Vel = d * rng.Range(9f, 19f);
                r.Motion = new Vector4(9f, 0.8f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.8f, 1f, 0.034f);        // (b16: long streaks - the clip's shoot 100-200 px up)
                r.Extra.y = 4f;
                renderer.Spawn(kind, r);
            }
            // ---- and round droplets of every size thrown out of the hit (frames 36-100; b06: the clip's spray is about 100
            // small dots a frame round the mass at f76-84, b05 drew 20)
            int drops = Count(420, n);
            for (int i = 0; i < drops; i++)
            {
                Vector3 d = rng.OnSphere();
                d.y = Mathf.Abs(d.y) + 0.1f;
                d = (d + dir * 0.4f).normalized;
                float h = rng.Next();
                float rad = 0.025f + 0.06f * h * h;
                var r = Rec(now, AtFrame(late, rng.Range(36f, 100f)), rng.Range(0.3f, 0.6f), AcidBeamKind.Drop,
                    target + Vector3.up * rng.Range(-0.4f, 1.0f) + across * rng.Range(-0.6f, 0.6f) - away * rng.Range(0.0f, 0.8f), Splash, 1f, ref rng);
                r.Vel = d * rng.Range(4f, 13f);
                r.Motion = new Vector4(9f, 1.0f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.8f, 1f, 0.006f);
                r.Tone.w = i % 3 == 0 ? 2f : i % 3 == 1 ? 0f : 3f;             // light, lime, yellow
                r.Extra.y = 1f;
                renderer.Spawn(kind, r);
            }

            // ---- a dense spray of tiny droplets round the splash late in it (frames 70-90: the clip's are about 150 dots a
            // frame at f76-86; b14) - and all the while the beam lands (b17: the clip's splash is full of them from f44)
            int fine = Count(720, n);                                    // (b28: the clip's f76-90 is full of bright dots)
            for (int i = 0; i < fine; i++)
            {
                Vector3 d = rng.OnSphere();
                d.y = Mathf.Abs(d.y) * 0.8f + 0.05f;
                d = (d + dir * 0.3f).normalized;
                float rad = rng.Range(0.03f, 0.07f);                      // (b15: b14's 0.018-0.04 m were 2-6 px, too small to read)
                // (b22: in the streaks' pale lime-yellow - the clip's sparkles are its brightest dots, b14-b21's lime were dim)
                var r = Rec(now, AtFrame(late, i < fine * 7 / 10 ? rng.Range(72f, 90f) : rng.Range(44f, 72f)), rng.Range(0.25f, 0.5f), AcidBeamKind.Drop,
                    target + Vector3.up * rng.Range(-0.4f, 1.6f) + right * rng.Range(-1.2f, 1.2f) - away * rng.Range(0.0f, 0.8f) - dir * (i % 3 == 0 ? rng.Range(0f, 4f) : 0f), Streak, 1f, ref rng);   // (b30: a third of them over the beam's last metres)
                r.Vel = d * rng.Range(4f, 10f);                           // (b29: out wide - b28's piled up in the heart, a cauliflower)                           // (b23: slower - b14-b22's flew out far and thin)
                r.Motion = new Vector4(7f, 1.2f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.8f, 1f, 0.006f);
                r.Tone.w = i % 3 == 0 ? 2f : i % 3 == 1 ? 3f : 0f;
                r.Extra.y = 1f;
                renderer.Spawn(kind, r);
            }

            // ---- the beam's end (frames 86-108): as the whole beam breaks up it leaves glossy blobs, yellow ropes and
            // droplets hanging where it was, drifting a little on and falling slowly as they go (b06; b02-b05's blobs
            // and beads were thrown from the nozzle and flew on after the beam's end)
            {
                int ends = Count(12, n), ropes = Count(40, n), endDrops = Count(150, n);   // (b08: b07's 40 blobs read as bubble wrap, its ropes as comets; b19-b20: big ones, lime drops)
                for (int i = 0; i < ends + ropes + endDrops; i++)
                {
                    bool blob = i < ends, rope = !blob && i < ends + ropes;
                    float u = 0.18f + 0.79f * Mathf.Pow(rng.Next(), 1.3f);   // where along the beam (0 the nozzle, 1 the target; more near the eye)
                    float hw = Mathf.Lerp(HalfNozzle, HalfTarget, u) * (blob || rope ? 2.2f : 3.0f) * Mathf.Lerp(1.3f, 0.9f, u);   // (b16: broader near the eye)
                    Vector3 at = Along(nozzle, target, flight * u) + Vector3.up * (hw * rng.Range(-0.7f, 1.0f)) + across * (hw * rng.Range(-0.7f, 0.7f));
                    float when = AtFrame(late, blob ? rng.Range(86f, 92f) : rope ? rng.Range(86f, 94f) : rng.Range(87f, 97f));
                    Vector3 drift = vel * rng.Range(0.02f, 0.06f) + rng.OnSphere() * (blob ? 0.6f : 1.2f) + Vector3.up * rng.Range(-0.5f, 0.8f);
                    if (blob)
                    {
                        float h = rng.Next();
                        float rad = 0.20f + 0.70f * h * h;                // (the clip's reach 150 px near the eye; b19: no small ones - they read as eyes)
                        var r = Rec(now, when, rng.Range(0.45f, 0.8f), AcidBeamKind.Drop, at, Bubble, 1f, ref rng);
                        r.Vel = drift;
                        r.Motion = new Vector4(2.5f, 1.2f, 0f, 0f);
                        r.Size = new Vector4(rad, rad * 0.55f, 1f, 0.02f);
                        r.Erode = new Vector4(rng.Next() < 0.3f ? 1f : 0f, rad > 0.55f ? 1f : 0f, 0f, 0f);   // (the biggest open into a C; b07's many read as eyes)
                        r.Extra.y = 3f;
                        renderer.Spawn(kind, r);
                    }
                    else if (rope)
                    {
                        // (b20: round lime drops of every size with pale hearts, the clip's f92-100 between its blobs - b19's
                        // splats read as little stars)
                        float rad = rng.Range(0.08f, 0.24f);
                        var r = Rec(now, when, rng.Range(0.35f, 0.6f), AcidBeamKind.Drop, at, Splash, 1f, ref rng);
                        r.Vel = drift * 0.7f;
                        r.Motion = new Vector4(3f, 1.0f, 0f, 0f);
                        r.Size = new Vector4(rad, rad * 0.8f, 1f, 0.02f);
                        float t = rng.Next();
                        r.Tone.w = t < 0.45f ? 2f : t < 0.80f ? 0f : 3f;     // light, lime, yellow
                        r.Erode = Vector4.zero;
                        r.Extra.y = 1f;
                        renderer.Spawn(kind, r);
                    }
                    else
                    {
                        float h = rng.Next();
                        float rad = 0.03f + 0.12f * h * h;                // (b19: up to 0.15 m - the clip's lime drops with pale hearts)
                        var r = Rec(now, when, rng.Range(0.3f, 0.65f), AcidBeamKind.Drop, at, Splash, 1f, ref rng);
                        r.Vel = drift;
                        r.Motion = new Vector4(5f, 1.0f, 0f, 0f);
                        r.Size = new Vector4(rad, rad * 0.7f, 1f, 0.01f);
                        r.Tone.w = i % 3 == 0 ? 2f : i % 3 == 1 ? 0f : 3f;     // light, lime, yellow
                        r.Extra.y = 1f;
                        renderer.Spawn(kind, r);
                    }
                }
            }
            // ... and round the lump (frames 90-112): glossy bubbles on it and flying off it, droplets hanging round it
            // and thrown off, yellow ribbons torn off its far side as it goes (b06)
            if (!far)
            {
                Vector3 heart = stand + Vector3.up * 1.8f;
                // (b36) sheets of goo sinking over it, holed and lit at their rims (the clip's lump is irregular gappy goo; b06-b35's a neat dome)
                int sinks = Count(8, n);
                for (int i = 0; i < sinks; i++)
                {
                    float a = rng.Range(0f, Mathf.PI);
                    Vector3 o = right * (Mathf.Cos(a) * rng.Range(0.3f, 1.5f)) + Vector3.up * (Mathf.Sin(a) * rng.Range(0.0f, 0.8f) - 0.3f);
                    float rad = rng.Range(0.8f, 1.3f);                    // (b37: lower, not stretched - b36's stood twice the clip's height)
                    var r = Rec(now, AtFrame(late, rng.Range(90f, 108f)), rng.Range(0.55f, 0.85f), AcidBeamKind.Drop, heart + o - away * 0.9f, Sheet, 1f, ref rng);
                    r.Vel = o.normalized * rng.Range(0.3f, 0.9f) + Vector3.down * rng.Range(0.6f, 1.4f);
                    float speed = Mathf.Max(r.Vel.magnitude, 0.5f);
                    r.Motion = new Vector4(2f, 0.6f, 0f, 0f);
                    r.Size = new Vector4(rad * 0.7f, rad * 1.1f, 0.6f, 0f);
                    r.Tone.w = rng.Next() < 0.5f ? 1f : 2f;
                    r.Erode = new Vector4(1.1f, 0f, 0f, 0f);
                    r.Extra.y = 7f;
                    renderer.Spawn(kind, r);
                }
                int bubbles = Count(8, n), hangs = Count(130, n), ribbons = Count(3, n);   // (b29: more drops round it to f118, hardly a dart)
                for (int i = 0; i < bubbles; i++)
                {
                    float rad = rng.Range(0.10f, 0.32f);
                    float a = rng.Range(0f, Mathf.PI);
                    Vector3 at = heart + right * (Mathf.Cos(a) * rng.Range(1.2f, 2.4f)) + Vector3.up * (Mathf.Sin(a) * rng.Range(1.0f, 2.0f) - 0.3f) - away * rng.Range(0.6f, 1.0f);
                    var r = Rec(now, AtFrame(late, rng.Range(92f, 106f)), rng.Range(0.25f, 0.5f), AcidBeamKind.Drop, at, LumpBubble, 1f, ref rng);
                    r.Vel = (at - heart).normalized * rng.Range(0.5f, 1.5f) + Vector3.up * rng.Range(0f, 0.6f);
                    r.Motion = new Vector4(1f, 1.0f, 0f, 0f);
                    r.Size = new Vector4(rad, rad * 0.7f, 1f, 0f);
                    r.Erode = new Vector4(rng.Next() < 0.5f ? 1f : 0f, 0f, 0f, 0f);
                    r.Extra.y = 3f;
                    renderer.Spawn(kind, r);
                }
                for (int i = 0; i < hangs; i++)
                {
                    float a = rng.Range(-0.3f, Mathf.PI + 0.3f);
                    Vector3 o = right * (Mathf.Cos(a) * 2.2f) + Vector3.up * (Mathf.Sin(a) * 1.9f);
                    float rad = rng.Range(0.04f, 0.12f);
                    var r = Rec(now, AtFrame(late, rng.Range(90f, 118f)), rng.Range(0.3f, 0.6f), AcidBeamKind.Drop,
                        heart + o * rng.Range(0.85f, 1.25f) - away * 0.8f, Splash, 1f, ref rng);
                    r.Vel = o.normalized * rng.Range(0.8f, 3f);
                    r.Motion = new Vector4(4f, 1.0f, 0f, 0f);
                    r.Size = new Vector4(rad, rad * 0.7f, 1f, 0.01f);
                    r.Tone.w = i % 3 == 0 ? 2f : i % 3 == 1 ? 0f : 3f;
                    r.Extra.y = 1f;
                    renderer.Spawn(kind, r);
                }
                for (int i = 0; i < ribbons; i++)   // (b09: as tongues - b06-b08's stretched sheets read as comet arrows)
                    Tongue(renderer, kind, now, AtFrame(late, rng.Range(98f, 110f)),
                        heart + right * rng.Range(0.8f, 2.0f) + Vector3.up * rng.Range(-1.2f, 1.2f) - away * 0.8f, heart, away, false, ref rng);
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
                    ground + right * rng.Range(-1.6f, 1.6f) + Vector3.up * rng.Range(1.0f, 2.6f) - away * rng.Range(0.3f, 1.0f), LumpBubble, 1f, ref rng);
                r.Vel = Vector3.up * rng.Range(0.3f, 0.8f) + right * rng.Range(-0.4f, 0.4f);
                r.Motion = new Vector4(0f, 0.3f, 0f, 0f);
                r.Size = new Vector4(rad, rad, 1f, 0f);
                r.Extra.y = 3f;
                renderer.Spawn(kind, r);
            }
        }

        static int Count(int close, float share) => Mathf.RoundToInt(close * share);

        /// <summary>One blade of goo thrown out of the hit `at` seconds after the call at `speed` m/s: mostly across
        /// the view, most of them up, a little on the way the beam goes; a third of them green, the rest lime to pale.
        /// Its root holds at the hit for 0.08-0.14 s (the clip's spikes grow out of it over two or three drawings).</summary>
        static void Blade(CelFxRenderer renderer, int kind, float now, float at, Vector3 target, Vector3 dir, Vector3 away, float speed, bool crown, ref CelRng rng)
        {
            Vector3 d = rng.OnSphere();
            d -= away * (Vector3.Dot(d, away) * 0.7f);
            d.y = Mathf.Abs(d.y) * 1.3f - 0.2f;
            d = (d + dir * 0.35f).normalized;
            float rad = crown ? rng.Range(0.22f, 0.42f) : rng.Range(0.18f, 0.40f);   // (b06: the crown's are broad flames; b30: thinner, yellow-lime)
            bool green = rng.Next() < (crown ? 0.1f : 0.3f);
            var r = Rec(now, at, rng.Range(0.22f, 0.36f), AcidBeamKind.Drop, target + Vector3.up * 0.15f + away * 0.15f, green ? Sheet : Splash, 1f, ref rng);
            r.Vel = d * speed;
            r.Motion = new Vector4(3f, 4.5f, rng.Range(0.08f, 0.14f), 0f);
            r.Size = new Vector4(rad, rad * 0.6f, 1.5f, 0f);
            float t = rng.Next();
            r.Tone.w = green ? (t < 0.5f ? 2f : 3f) : (t < 0.40f ? 2f : t < 0.80f ? 3f : 4f);   // (b15: b11's paler blades made the mass too pale)
            r.Erode = new Vector4(0f, rng.Range(-1f, 1f), green ? 0.3f : 0.85f, 0f);   // (b17: no line round it)
            r.Extra.y = 5f;
            renderer.Spawn(kind, r);
        }

        /// <summary>A tongue of goo thrown out of the splash (b08): a broad blade whose round root holds where it was
        /// thrown from for 0.10-0.16 s while its point flies out from the hit at 6-10 m/s, a flame 2-3 times as long as
        /// it is wide; the green ones darker (the clip's mass is flame tongues pointing out of the hit).</summary>
        static void Tongue(CelFxRenderer renderer, int kind, float now, float at, Vector3 from, Vector3 target, Vector3 away, bool green, ref CelRng rng)
        {
            Vector3 o = from - target;
            o -= away * Vector3.Dot(o, away);
            Vector3 d = (o.normalized * 0.7f + rng.OnSphere() * 0.3f + Vector3.up * 0.25f).normalized;
            float rad = rng.Range(0.22f, 0.42f);                      // (b18: b17's 0.17-0.34 at 8-12 m/s read as thorns)
            var r = Rec(now, at, rng.Range(0.22f, 0.38f), AcidBeamKind.Drop, from, green ? Sheet : Splash, 1f, ref rng);
            r.Vel = d * rng.Range(6f, 10f);
            r.Motion = new Vector4(3f, 1.8f, rng.Range(0.10f, 0.16f), 0f);
            r.Size = new Vector4(rad, rad * 0.6f, 1.5f, 0f);
            float t = rng.Next();
            // (b15: less pale - b14's mass was 34 % pale yellow-lime where the clip's is 16 %; b17: pale where it leaves the
            // middle of the hit, the clip's pale-yellow heart, and its green rim vivid)
            float mid = Mathf.Clamp01(0.9f - o.magnitude / 0.6f);   // (b28: pale at the very heart only)
            r.Tone.w = green ? (t < 0.40f ? 1f : t < 0.80f ? 2f : 3f)
                : t < mid * 0.6f ? (t < mid * 0.3f ? 4f : 5f) : (t < 0.15f ? 1f : t < 0.55f ? 2f : t < 0.88f ? 3f : 4f);
            r.Erode = new Vector4(0f, rng.Range(-1f, 1f), green ? 0.15f : 0.35f + 0.4f * mid, 0f);   // (b17: no line round it - the clip's have none)
            r.Extra.y = 5f;
            renderer.Spawn(kind, r);
        }

        /// <summary>A splat of goo thrown out of the splash (b18): a broad body with fingers out of its front ending in
        /// beads, flying out from the hit at 3-7 m/s and slowing, stretched along its flight 1.1-2 times at first; pale
        /// where it leaves the middle of the hit, its green ones vivid (the clip's mass f44-68 is these).</summary>
        static void Splat(CelFxRenderer renderer, int kind, float now, float at, Vector3 from, Vector3 target, Vector3 away, bool green, float rad, ref CelRng rng)
        {
            Vector3 o = from - target;
            o -= away * Vector3.Dot(o, away);
            Vector3 d = (o.normalized * 0.75f + rng.OnSphere() * 0.25f + Vector3.up * 0.2f).normalized;
            float speed = rng.Range(3f, 7f);
            var r = Rec(now, at, rng.Range(0.20f, 0.34f), AcidBeamKind.Drop, from, green ? Sheet : Splash, 1f, ref rng);
            r.Vel = d * speed;
            r.Motion = new Vector4(3f, 2.2f, 0f, 0f);
            r.Size = new Vector4(rad, rad * 1.15f, 1f, rng.Range(1.1f, 2.0f) * rad / speed);
            float t = rng.Next();
            float mid = Mathf.Clamp01(0.9f - o.magnitude / 0.6f);   // (b28: pale at the very heart only)
            r.Tone.w = green ? (t < 0.40f ? 1f : t < 0.80f ? 2f : 3f)
                : t < mid * 0.6f ? (t < mid * 0.3f ? 4f : 5f) : (t < 0.15f ? 1f : t < 0.55f ? 2f : t < 0.88f ? 3f : 4f);
            r.Erode = Vector4.zero;
            r.Extra.y = 7f;
            renderer.Spawn(kind, r);
        }

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
