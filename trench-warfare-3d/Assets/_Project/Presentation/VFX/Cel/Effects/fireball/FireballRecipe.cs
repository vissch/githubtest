// Phase: CelFx fireball (2026-11-14; a02 the block-in, a04 the mass on the ground, a05 drops, a06 drops at half the size). A comet of fire flies in from the eye's left,
// hits the dummy and bursts into a cloud of round cel lobes that cool to brown smoke and break up, with white
// spark rays, falling embers, a line of embers left along the comet's path and an orange light on the ground.
// Reference: fireball_nolora.mp4, 192 f at 24 fps, drawn on twos. Everything below is read off that clip and
// written out in celfx/spec/eye/fireball.md ("Numbers for the block-in"); nothing here has been fitted to a film
// of this effect yet, so every size, place and colour is a FIRST GUESS to be corrected from the first film.
// Crude on purpose (SKILL.md 16 step 4: right place, size, time and colour, no detail).
//
// It draws through the kind's own shader (CelFireball_URP, copied from neon_acid in a01) and uses five of its
// eight modes: 1 drop (the comet's head, its tail, the tail embers, the spark rays, the falling embers), 2 splat
// (the hit), 5 lobe (one lobe of the burst: a07 rewrote the acid's unused bubble mode into it), 6 puff (the
// smoke the burst cools into) and 7 glow (the ground light). The strand, mound and puddle modes of that shader
// are not used by the fireball and nothing spawns them.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class FireballRecipe
    {
        /// <summary>Seconds from the comet's first light to the hit, measured on the clip: first light frame 12
        /// (= t0), the head is in frame on 14, it reaches the dummy on 26 and the hit is on 28, so 16 frames.</summary>
        public const float Flight = 0.6667f;
        /// <summary>The comet flies nearly level across the frame (clip: the head's centre goes from (230,440) on
        /// frame 16 to (846,395) on frame 26), so it is given a small cartoon droop, not an artillery arc.</summary>
        public const float Gravity = 3.2f;
        /// <summary>Seconds the whole effect is alive: first light frame 12, nothing left by frame 120.</summary>
        public const float Length = 4.5f;

        /// <summary>The clip's effect starts on its frame 12. AtFrame gives the seconds after the call at which one
        /// of the clip's frames falls; `late` is how much later than the clip's this play starts.</summary>
        const float FirstFrame = 12f;
        static float AtFrame(float late, float frame) => (frame - FirstFrame) / 24f - 0.004f + late;
        /// <summary>A time moved onto the drawing it falls in (the effect is drawn on twos), a hair early so the
        /// record is alive on that drawing's first frame.</summary>
        static float OnTwos(float seconds) => Mathf.Round(seconds * 12f) / 12f - 0.004f;

        // ---- sizes, in metres, from the clip at about 0.013 m per px at the dummy's depth (spec/eye/fireball.md)
        /// <summary>The comet's head: 180-200 px across in the clip.</summary>
        public const float HeadRadius = 1.20f;
        /// <summary>The tail: 80-110 px thick and the whole frame long behind the head.</summary>
        const float TailRadius = 0.62f, TailLength = 11.0f;
        /// <summary>The burst at its peak (clip frame 44): 500 x 340 px = 6.5 m across and 4.4 m tall, its foot on
        /// the ground. Its lobes are circles of 60-110 px = 0.8-1.4 m across.</summary>
        public const float BurstRadius = 3.25f, BurstTall = 2.70f, LobeRadius = 0.55f;   // a17: 2.20 -> 2.70, the cloud grows up with the foot a16 gave it (the clip is 1.5 wide to 1 tall)   // a15: 0.45 -> 0.55, so neighbours touch (85-110 px against the clip's 60-110)

        // ---- colours, measured off the clip's pixels (spec/eye/fireball.md). They are written straight, NOT
        // through neon_acid's grade inversion: that inversion was fitted to a film of flat swatches for the acid,
        // and BRIEF.md says to reuse it only once a fireball film confirms it. The first film settles this.
        static readonly int Fire = CelPalette.Add("fb_fire",
            CelPalette.Hex(0xF84C00), CelPalette.Hex(0xF08A10), CelPalette.Hex(0xF8F40C), CelPalette.Hex(0xFCFC80),
            CelPalette.Hex(0xFCFCE4), 0.88f, 4, CelPalette.Hex(0xC83000));
        static readonly int Comet = CelPalette.Add("fb_comet",
            CelPalette.Hex(0xF84C00), CelPalette.Hex(0xF8A000), CelPalette.Hex(0xF8F81C), CelPalette.Hex(0xFCFCA0),
            CelPalette.Hex(0xFCFCFC), 0.80f, 4, CelPalette.Hex(0xC83000));
        // the charred lobes and the flat brown smoke they become (clip frames 72-104)
        static readonly int Smoke = CelPalette.Add("fb_smoke",
            CelPalette.Hex(0x201818), CelPalette.Hex(0x3A2E28), CelPalette.Hex(0x584840), CelPalette.Hex(0x6B5A50),
            CelPalette.Hex(0x6B5A50), 1.1f, 3, CelPalette.Hex(0x140E0C));
        static readonly int Spark = CelPalette.Add("fb_spark",
            CelPalette.Hex(0xF84C00), CelPalette.Hex(0xFFC070), CelPalette.Hex(0xFFE8B0), CelPalette.Hex(0xFFFFFF),
            CelPalette.Hex(0xFFFFFF), 0.5f, 3, CelPalette.Hex(0x401004));
        const float SmokeOpacity = 0.86f;

        /// <summary>The light the fire throws on the ground. The clip's near ground goes from (24,24,40) to
        /// (72,48,40) with tops at (96,64,48): about three times in red, and it keeps the ground's own pattern, so
        /// most of it is multiplied in (GlowInto) and the rest added on top (GlowAdd).</summary>
        static readonly Vector3 GlowInto = new Vector3(1.40f, 0.72f, 0.14f), GlowAdd = new Vector3(0.22f, 0.10f, 0.03f);
        /// <summary>The pool that travels under the comet, and the big one under the burst (m).</summary>
        const float UnderReach = 7.0f, BurstReach = 9.0f;

        static readonly int LensId = Shader.PropertyToID("_TWAcidLens");

        // the lens handed over is a global of the shader: a session that ends puts it back to none
        static FireballRecipe() { SceneStatics.Register(nameof(FireballRecipe), () => Shader.SetGlobalVector(LensId, Vector4.zero)); }

        /// <summary>
        /// The scene's lens vignette (URP Vignette's intensity and smoothness; 0 for none), for whoever shows the
        /// effect to hand over: the fire is written brighter where the vignette will darken it, so it shows the
        /// clip's colours all the way into the corners of the frame (as far as white allows).
        /// </summary>
        public static void Lens(float intensity, float smoothness) =>
            Shader.SetGlobalVector(LensId, new Vector4(Mathf.Max(0f, intensity) * 3f, Mathf.Max(0.01f, smoothness) * 5f, 0f, 0f));

        /// <summary>The launch velocity that carries the comet from where it starts to the target in Flight seconds.</summary>
        public static Vector3 Launch(Vector3 from, Vector3 target) => (target - from) / Flight + Vector3.up * (0.5f * Gravity * Flight);

        /// <summary>Where the comet that left `from` s seconds ago is.</summary>
        public static Vector3 Along(Vector3 from, Vector3 target, float s) => from + Launch(from, target) * s + Vector3.down * (0.5f * Gravity * s * s);

        /// <summary>
        /// Play the fireball: the comet starts at `from` and hits `target` (the dummy's chest), whose foot stands
        /// on the ground at `ground`; `right` is the eye's level right. `delay` holds it for that long after the
        /// call. Everything is spawned now, the light it throws on the ground too (the shader finds what it falls
        /// on in the camera's depth texture): nothing is left to do per frame. No allocation.
        /// </summary>
        public static void Play(Vector3 from, Vector3 target, Vector3 ground, Vector3 right, uint seed, float delay = 0f)
        {
            right.y = 0f;
            right = right.sqrMagnitude > 0.001f ? right.normalized : Vector3.right;
            Vector3 away = Vector3.Cross(right, Vector3.up);   // from the eye
            var renderer = CelFx.Renderer;
            int kind = renderer.KindId(FireballKind.KindName);
            if (kind < 0) throw new System.InvalidOperationException("Fireball kind is not registered");
            float lod = CelFx.LodBlend(CelFx.Zoom);
            bool mid = lod >= 0.5f, far = lod >= 1.5f;
            float now = CelFx.Now;
            var rng = new CelRng(seed);
            Vector3 vel = Launch(from, target);
            Vector3 flat = new Vector3(vel.x, 0f, vel.z);
            Vector3 heading = flat.sqrMagnitude > 0.001f ? flat.normalized : right;
            float late = delay;                  // this play starts on the clip's frame 12
            float hit = delay + Flight;          // the comet lands (clip frame 28)
            ground.y = Ground(ground);

            // ---- the light on the ground, under everything. A pool that travels with the comet (the clip's near
            // ground is orange on frames 11-12, three frames before the comet is in frame), then the big pool under
            // the burst: strongest on frames 32-50, half by 66, gone by 88-92.
            {
                Vector3 under = Vector3.Lerp(from, target, 0.45f);
                under.y = Ground(under) + 1f;
                Glow(renderer, kind, now, AtFrame(late, 11f), AtFrame(late, 30f), under, right, UnderReach, UnderReach,
                    UnderReach, 1f, far ? 0.45f : mid ? 0.7f : 1f, GlowInto, GlowAdd, 0.06f, 0.10f, true, seed);
                Vector3 pool = ground;
                pool.y = Ground(pool) + 1f;
                Glow(renderer, kind, now, AtFrame(late, 28f), AtFrame(late, 92f), pool, right, BurstReach, BurstReach,
                    BurstReach, 1f, 1f, GlowInto, GlowAdd, 0.08f, 1.0f, true, seed);
            }

            // ---- the comet's head: one round ball of fire with a white core, in frame on the clip's frame 14 and
            // on the dummy by 26.
            {
                var r = Rec(now, OnTwos(delay), Flight + 0.04f, FireballKind.Drop, from, Comet, 1f, ref rng);
                r.Vel = vel;
                r.Motion = new Vector4(Gravity, 0f, 0f, 0f);
                r.Size = new Vector4(HeadRadius, HeadRadius * 1.05f, 1f, 0.016f);
                r.Extra.y = 1f;                  // a round fleck
                renderer.Spawn(kind, r);
            }

            // ---- the tail: a ragged banner of flames strung out behind the head, broken into separate lens-shaped
            // leaves along it (the clip: 80-110 px thick, the whole frame long).
            int tail = far ? 4 : mid ? 26 : 54;   // a11: four records this far off, not ten (FireballTests' far bound)
            for (int i = 0; i < tail; i++)
            {
                float back = (i + rng.Range(0f, 0.9f)) / tail;                 // 0 at the head, 1 at the far end
                float rad = Mathf.Lerp(TailRadius, TailRadius * 0.35f, back) * rng.Range(0.75f, 1.25f);
                var r = Rec(now, OnTwos(delay), Flight + 0.04f, FireballKind.Drop,
                    from - heading * (back * TailLength), Comet, 1f, ref rng);
                r.Vel = vel + right * rng.Range(-0.6f, 0.6f) + Vector3.up * rng.Range(-0.5f, 0.8f);
                r.Motion = new Vector4(Gravity, 0f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.55f, 1f, rng.Range(0.020f, 0.045f));
                r.Extra.y = 2f;                  // a leaf: a point at both ends
                renderer.Spawn(kind, r);
            }

            // ---- the embers the tail leaves along the path it flew (clip frames 24-44): round orange dots that
            // die from the far end.
            int embers = far ? 0 : mid ? 26 : 64;   // a29: the clip shows 17-42 dots a frame on the flown path
            // a30: THIS RECIPE HAS ONE SHARED rng AND Rec() DRAWS FROM IT, so raising a count in a block that
            // runs BEFORE another re-rolls that other block. a29 raised this count from 30 to 64 and silently
            // re-rolled the burst's forty lobes (iou .524 -> .466). The first 30 dots therefore keep the shared
            // stream's draws - the per-iteration draw count is unchanged at 5 Range + 1 Next - and only the
            // extra ones take their own stream, the way Glow() already does.
            var own = new CelRng(seed ^ 0x5EEDu);
            for (int i = 0; i < embers; i++)
            {
                ref CelRng use = ref (i < 30 ? ref rng : ref own);
                float back = (i + 0.5f) / Mathf.Max(1, embers);
                // a29: the dots have FALLEN OUT of the tail and lie near the sand. Deposited on the ballistic
                // line itself they sat a metre high - 75 px above the clip's y 430-500 band, out of the crop.
                Vector3 at = Along(from, target, Flight * (1f - back * 0.82f)) + Vector3.up * use.Range(-1.5f, -0.5f)
                             + right * use.Range(-0.4f, 0.4f);
                at.y = Mathf.Max(at.y, Ground(at) + 0.15f);   // and never under the sand
                float rad = use.Range(0.05f, 0.12f);
                // a30: the deposit is spread over fourteen frames, not three (the clip's line still has ten dots
                // at f44), and it is drawn from the Fire row - the clip's dots are ORANGE (hue 29), not the
                // Spark row's pale yellow (hue 59).
                var r = Rec(now, OnTwos(hit - back * 0.42f), use.Range(0.50f, 0.85f) * (1f - back * 0.5f),
                    FireballKind.Drop, at, Fire, 1f, ref use);
                r.Vel = Vector3.up * use.Range(0.2f, 0.9f);
                r.Motion = new Vector4(1.2f, 0.6f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.5f, 1f, 0.004f);
                r.Extra.y = 1f;
                renderer.Spawn(kind, r);
            }

            // ---- the hit: a star of spikes for one drawing (clip frames 28-31)
            if (!far)
            {
                var r = Rec(now, OnTwos(hit), 0.125f, FireballKind.Splat, target + Vector3.up * 0.2f, Fire, 1f, ref rng);
                r.Size = new Vector4(3.0f, 3.0f, 1f, 1f);
                r.Extra.y = 0.8f;
                renderer.Spawn(kind, r);
            }

            // ---- THE BURST (the focus part). A cloud of round lobes standing on the ground over the dummy, wider
            // than it is tall: hot white-yellow from the clip's frame 34 to 48, cooling from 48, charred by 60, the
            // fire out by 76. Each lobe is drawn twice: a fire puff that lives through the hot phase and a brown one
            // in the same place that takes over as it cools, so the cloud goes to smoke without moving off the mark.
            // a15: 40 close and 22 mid, not 24 and 15 - the clip's cloud is 6.5 m across with its lobes
            // overlapping into one silhouette, ours was about 0.6 of that width with ground showing between them.
            // far stays 4 (each spawns a smoke puff too): the far-LOD contract is 8..16 records.
            int lobes = far ? 4 : mid ? 22 : 40;
            Vector3 heart = ground + Vector3.up * (BurstTall * 0.72f);
            for (int i = 0; i < lobes; i++)
            {
                // a04. a03 proved the arrangement matters and that I had the clip's profile BACKWARDS: read from
                // the foot up, the clip's widths at 25/50/75 % of its height are 233/159/163, so its mass is
                // WIDEST AT ITS FOOT, sitting in the dirt, and narrows as it goes up. a03 made ours widest at its
                // top and lifted it off the ground (122/209/228, coverage 0.65 -> 0.43, iou 0.186 -> 0.146): a
                // drop. a03 also fused 13 big lobes into 6 smooth pieces where the clip reads 36.6 pieces inside
                // one silhouette, so the lobes go back to many and smaller.
                Vector3 inside = rng.InSphere();
                // a17: the lobe's HEIGHT comes from its index, not from the sphere. A random point in a sphere
                // crowds the middle by construction, so no spread curve over it can make the foot the widest
                // part; three equal rows, lowest first, put a third of the lobes in the dirt. The sphere still
                // supplies the horizontal jitter.
                // a18: six bands with a jitter that spans a whole band each way, so neighbouring bands interleave
                // and no horizontal seam can form. a17's three hard rows broke the cloud into three clusters with
                // ground between them; the height distribution stays even foot-to-top either way.
                // a19: the band INDEX is weighted toward the FOOT. a18 gave all six bands an equal share
                // (7/7/6/7/7/6 of 40) and the profile came out TOP-heavy, 177/271/276 against the clip's
                // 298/232/240. Width here is lit pixels in a scanline, so a band with a wide spread and the
                // same lobe count simply strews thin while a narrow band packs solid: the top bands at spread
                // 0.50 fill in, the foot bands at 1.35 are spread over 2.7x the room and go to holes. So each
                // band is given lobes in proportion to the horizontal room it is allowed - the inverse CDF of
                // the spread term itself - and the lit area per band comes out even: 9/8/8/6/5/4 of 40 close,
                // 5/5/4/3/3/2 of 22 mid, the far LOD's 4 unchanged. The profile is set by how many lobes are
                // low, not by how wide each band may spread (a16 + a17 + a18 together).
                float f01 = (i + 0.5f) / Mathf.Max(lobes, 1);
                float u01 = (1.35f - Mathf.Sqrt(1.8225f - 1.5725f * f01)) / 0.85f;
                int band = Mathf.Clamp((int)(u01 * 6f), 0, 5);
                float up01 = Mathf.Clamp01((band + rng.Range(-0.6f, 1.6f)) / 6f);   // 0 at the foot, 1 at the top
                // a16: 1.35 at the foot, 0.55 at the top, not 1.00/0.60. The clip's widths at 25/50/75 % of its
                // height are 233/159/163 - widest in the dirt, narrowing as it rises - and a15 measured
                // 112/182/180, bulging at the waist. a03 pushed this knob the WRONG way (widest at the top).
                float spread = Mathf.Lerp(1.35f, 0.50f, up01);
                Vector3 at = heart + right * (inside.x * BurstRadius * spread)
                             + away * (inside.z * BurstRadius * 0.45f * spread)
                             + Vector3.up * ((up01 - 0.5f) * BurstTall * 1.25f);
                if (at.y < Ground(at) + 0.30f) at.y = Ground(at) + 0.30f;
                float size = LobeRadius * rng.Range(0.80f, 1.30f);
                float born = OnTwos(hit + 0.083f + rng.Range(0f, 0.25f));
                // a07. a05 and a06 settled HOW BIG a lobe is and that it wants a hard cel edge, and a06's
                // falsifier fired on the other half: the lobes are one flat yellow at any size. Reading the
                // shader said why, and it is not a knob - the drop mode only ever computes its silhouette and
                // leaves col at the palette's body row, and the puff mode draws no line at all. So a lobe is now
                // drawn by mode 5, written for this effect in a07: a hard-cut round ball banded from a highlight
                // (white core, pale yellow, yellow, orange-red at its far edge) with an ink contour, charring
                // into the smoke row over the last quarter of its life. Place, size, count, birth and drift are
                // a06's, untouched, so this round changes only how one lobe is DRAWN.
                var r = Rec(now, born, rng.Range(1.7f, 2.1f), FireballKind.Lobe, at, Fire, 1f, ref rng);
                r.Vel = (at - heart).normalized * rng.Range(0.25f, 0.65f) + Vector3.up * rng.Range(0.05f, 0.35f);
                r.Motion = new Vector4(0f, 1.6f, 0f, 0f);
                // 0.90: the drop's round fleck filled 0.9 of its radius and the lobe fills all of it, so a06's
                // DRAWN size is kept exactly. w is the lobe's squash now, not the drop's stretch: 1 = round.
                r.Size = new Vector4(size * 0.90f, size * 0.97f, 1f, 1f);
                // the fire row, charring into the smoke row (CelRamp2's second row); 1 the core's multiplier,
                // 1 its opacity, and the ink contour in screen pixels - a08 cuts it from 5.5 to 3.0: the clip has
                // no line round each lobe at all in the hot phase, only a thin rim on the cloud's outer edge
                // a20, the first round of the burst's recorded DIAGNOSIS (a17, a18, a19 all moved the numbers and
                // were dropped on the eye; three arrangements of the same forty drawn balls traded one visible
                // structure for another). The ink goes to 0, from a08's 3.0. a08's own comment beside it has said
                // since then that "the clip has no line round each lobe at all in the hot phase, only a thin rim
                // on the cloud's outer edge", and the line was only narrowed, never removed. Mode 5 closes a
                // complete CelInk contour round every lobe's own circle, so a cloud of forty is drawn as forty
                // closed onions and reads - the independent reader's words - as a bunch of grapes. This tests
                // whether that contour is what stops them fusing, in the hot phase and in the char. The orange-red
                // band the ramp puts at each lobe's outer sliver stays, so the mass keeps a warm edge.
                r.Tone = new Vector4(CelRecord.PackPalette(Fire, Smoke), 1f, 1f, 0f);
                r.Extra.y = 0.11f;               // the lumps on its edge: the clip's lobes are round but not circles
                // a12: the char WINDOW. A lobe born about clip frame f32 with a 1.9 s life begins to char at 0.38
                // of it (about f49, the clip's f48) and is fully charred at 0.72 (about f65; the clip is mostly
                // dark at f60 and its fire is out by f76). a07-a08 started at 0.74 and only finished at the
                // lobe's death, so the cloud was still cream-white at f54-f60 - the defect that dropped a08.
                r.Extra.z = 0.38f;
                // a14: 0.86, not a13's 0.72 - with the cap doing the work the front crossed the whole lobe by
                // about f70 and our burst was wholly dark from f72, where the clip still has red-orange cracks
                // burning in the charcoal until f76.
                r.Erode = new Vector4(0.86f, 0f, 1f, 0f);
                // a22, the THIRD and last budgeted round of the burst's recorded diagnosis: the band field
                // referenced to the CLOUD, not to each lobe. a20 took the per-lobe ink off and the cloud still
                // read as a heap of countable balls; a21 squeezed the per-lobe annulus into a thin rind and it
                // STILL did, so the fencing is not the band's WIDTH but the FRAME it is measured in - mode 5
                // evaluates it in each lobe's own card, from each lobe's own highlight, so two overlapping lobes
                // shade differently in the pixels they share and draw a seam wherever they meet. The clip at f40
                // is one yellow field with large irregular white blobs running ACROSS several lobes and ignoring
                // lobe centres, orange-red only along the mass's outer bottom edge.
                // Mode 5 reads neither Axis nor Erode.yz, so they carry the cloud to the shader at no cost:
                //   Axis    this lobe's offset from the burst's HEART (metres, world; the vertex adds the lobe's
                //           own drift back in, so the shader has p - heart at the drawn age)
                //   Erode.y the cloud's half-extent ACROSS the view, z its half-extent UP (metres), the units the
                //           band field is measured in. 2.60 / 1.55 from this recipe's own geometry: a lobe sits
                //           within BurstRadius 3.25 x spread (1.35 at the foot to 0.50 at the top) across and
                //           within BurstTall 2.70 x 1.25 / 2 up, so u = 1.15 - where the ramp hands over to the
                //           orange-red - falls near the visible outer edge of the mass and nowhere inside it.
                r.Axis = at - heart;
                r.Erode.y = 2.60f;
                r.Erode.z = 1.55f;
                renderer.Spawn(kind, r);

                // the same lobe charred, then flat brown: it comes up as the fire goes and breaks up by frame 112
                var s = Rec(now, OnTwos(hit + 1.25f + rng.Range(0f, 0.3f)), rng.Range(1.6f, 2.3f), FireballKind.Puff,
                    at + Vector3.up * rng.Range(0f, 0.3f), Smoke, SmokeOpacity, ref rng);
                s.Vel = (at - heart).normalized * rng.Range(0.2f, 0.6f) + Vector3.up * rng.Range(0.2f, 0.7f);
                s.Motion = new Vector4(0f, 0.9f, 0f, 0.45f);
                s.Size = new Vector4(size * 1.15f, size * 1.8f, 1.4f, 1f);
                s.Extra.y = 0.55f;
                s.Extra.z = 0.25f;
                renderer.Spawn(kind, s);
            }

            // ---- the white spark rays out of the burst (clip frames 30-52): straight needles thrown out of its top
            // half, no gravity on them, there and gone in two or three drawings.
            int rays = far ? 0 : mid ? 29 : 70;   // a28: the clip has 34-47 separate spark objects a frame, not 8-22 blades
            for (int i = 0; i < rays; i++)
            {
                Vector3 d = rng.OnSphere();
                d.y = Mathf.Abs(d.y) * 1.1f + 0.15f;
                d = d.normalized;
                var r = Rec(now, OnTwos(hit + rng.Range(0.04f, 0.9f)), rng.Range(0.10f, 0.22f), FireballKind.Drop,
                    heart + d * rng.Range(0.5f, 2.2f), Spark, 1f, ref rng);
                r.Vel = d * rng.Range(14f, 30f);
                r.Motion = new Vector4(0f, 0f, 0f, 0f);
                r.Size = new Vector4(0.028f, 0.012f, 1f, rng.Range(0.025f, 0.045f));   // a28: a needle, not a blade - the stretch is SECONDS of its own motion
                r.Extra.y = 2f;
                renderer.Spawn(kind, r);
            }

            // ---- the embers that fall out of it (clip frames 56-92): short bright streaks with a hot head, arcing
            // out and down, the last bright thing in the frame.
            int fall = far ? 0 : mid ? 33 : 80;   // a28: many short embers, not a few fat leaves
            for (int i = 0; i < fall; i++)
            {
                Vector3 d = rng.OnSphere();
                d.y = Mathf.Abs(d.y) * 0.8f + 0.25f;
                d = d.normalized;
                var r = Rec(now, OnTwos(hit + rng.Range(0.5f, 2.4f)), rng.Range(0.8f, 1.6f), FireballKind.Drop,
                    heart + d * rng.Range(0.4f, 2.6f), Spark, 1f, ref rng);
                r.Vel = d * rng.Range(3.5f, 9f) + Vector3.up * rng.Range(0.5f, 3f);
                r.Motion = new Vector4(9.0f, 0.25f, 0f, 0f);
                r.Size = new Vector4(0.026f, 0.014f, 1f, rng.Range(0.05f, 0.11f));   // a28: thinner head; the stretch stays, the clip's falling embers keep a trail
                r.Extra.y = 2f;
                renderer.Spawn(kind, r);
            }
        }

        static void Glow(CelFxRenderer renderer, int kind, float now, float born, float gone, Vector3 at, Vector3 axis,
            float along, float up, float behind, float ahead, float strength, Vector3 into, Vector3 add, float rise, float fall, bool level, uint seed)
        {
            var own = new CelRng(seed);   // a light has nothing random in it: the effect's own draws stay as they were
            var r = Rec(now, born, gone - born, FireballKind.Glow, at, Fire, strength, ref own);
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

        static float Ground(Vector3 p) => RenderGround.Map != null
            ? RenderGround.Sample(RenderGround.Map, p.x, p.z) : p.y;
    }
}
