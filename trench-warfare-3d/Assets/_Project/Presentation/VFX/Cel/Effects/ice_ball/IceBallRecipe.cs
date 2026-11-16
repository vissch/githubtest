// Phase: CelFx ice_ball (2026-11-15, i02: redrawn against the clip, colours through the Range's grade). A ball of ice blades flies in from the eye's near left,
// stops short of the dummy and hangs there spinning with its tail of white streaks behind it, lunges in and bursts
// into a cluster of faceted ice spikes round a white heart, with a swirl of spray thrown out over the ground; the
// crystal stands, cracks and breaks into shards while a pale mist gathers at its foot, glows, and thins away.
// Reference: the first second of ice_ball_nolora_00001_.mp4 (the flight, frames 0-23) and ice_ball_cel_00001_.mp4
// from one second on (frames 24-191), 192 f at 24 fps, drawn on ones; the spliced frames are in
// celfx/runs/ice_ball/_ref/frames. The no-LoRA clip's own burst (its frames 20-23) is NOT built: the ball arrives on
// frame 17 where the cel clip's ball hangs from frame 24, and holds there. Sizes and places are read off the clip
// through the beach Range's camera (a pinhole of 1339 px focal length at 1344 x 768: 70.9 px to the metre at the
// dummy); colours are the clip's pixels, written as the beach Range's night grade needs them written to show
// them (celfx/runs/ice_ball/_ref/grade.py, from a film of a colour chart: the grade lifts the middle tones by a
// quarter, turns anything near white to cream, and shows a written 255 darker than a written 224).
//
// It draws through the kind's own shader (CelIceBall_URP): 0 the comet (ball and tail on one strip), 1 a fleck
// (chips, shards), 2 the crystal, 3 the mist, 4 the swirl on the ground and 7 the light on the ground.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class IceBallRecipe
    {
        /// <summary>Seconds from the launch to where the ball holds: clip frames 6 to 17.</summary>
        public const float Flight = 11f / 24f;
        /// <summary>Seconds the ball hangs short of the target (clip frames 17 to 37.2). 0 for a ball that goes
        /// straight in.</summary>
        public const float Hold = 20.2f / 24f;
        /// <summary>Seconds of its lunge into the target (clip frames 37.2 to 40.4: its nose is a fifth of the way
        /// in on frame 38, two thirds on 39, on the dummy on 40).</summary>
        public const float Lunge = 3.2f / 24f;
        /// <summary>Seconds from the launch to where it is all the way in: clip frame 40.4. It is drawn there on
        /// frame 41 as a soft white star, and bursts half a frame on.</summary>
        public const float Hit = Flight + Hold + Lunge;
        /// <summary>The ball flies nearly straight.</summary>
        public const float Gravity = 1.5f;
        /// <summary>Seconds the whole effect is alive: launch frame 6, nothing left by frame 120.</summary>
        public const float Length = 4.9f;

        /// <summary>The clip's effect starts on its frame 6. AtFrame gives the seconds after the call at which one
        /// of the clip's frames falls; `late` is how much later than the clip's this play starts.</summary>
        const float FirstFrame = 6f;
        static float AtFrame(float late, float frame) => (frame - FirstFrame) / 24f - 0.004f + late;

        // ---- sizes, in metres
        /// <summary>The ball's round head: 88 px of radius where it hangs, 18.9 m from the eye.</summary>
        public const float BallRadius = 1.08f;
        /// <summary>The tail, as long as it shows across the view (it runs away from the eye, so it is longer than this).</summary>
        const float TailLength = 12f;
        /// <summary>The crystal's card: its longest spike is 6.3 m, the white streaks it throws run out to 8.5 m.</summary>
        const float CrystalReach = 8.8f;
        /// <summary>The frames after the burst at which the crystal starts to break up (clip frame 57).</summary>
        const float BreakAt = 15.5f;
        /// <summary>The mist: the half-width and half-height its puffs are laid out in. At its fullest (clip frame
        /// 66) it is 8 m wide and 5 m tall.</summary>
        public const float MistHalfWidth = 3.5f, MistHalfHeight = 2.3f;
        /// <summary>Metres the crystal's and the mist's cards are drawn toward the eye, so the ground in front of the dummy does not cut them.</summary>
        const float Pull = 2.6f;
        /// <summary>The swirl's last reach over the ground.</summary>
        const float SwirlReach = 13f;

        // A colour as the film shows it. The project renders in gamma space and the palette stores .linear, so a
        // colour handed over as picked comes out one gamma too dark: this takes it back first (FireballRecipe.C).
        static Color C(int rgb)
        {
            Color c = CelPalette.Hex(rgb);
            return new Color(Mathf.LinearToGammaSpace(c.r), Mathf.LinearToGammaSpace(c.g), Mathf.LinearToGammaSpace(c.b), 1f);
        }

        // ---- colours, dark to light, then pale and the line. The comment gives what the film SHOWS (the clip's
        // pixels: celfx/runs/ice_ball/_ref/kcol.py), the code what is WRITTEN for the Range's grade to show it
        // (_ref/grade.py; celfx/runs/ice_ball/_grade/palette_i02.json).
        // the ball: (71,102,122) (97,143,167) (148,208,227) (177,235,247), pale (211,249,253), line (28,44,66)
        static readonly int Ball = CelPalette.Add("ib_ball",
            C(0x405461), C(0x507082), C(0x6EA9BB), C(0x78C6EE), C(0x92D0F0), 1.1f, 4, C(0x182839));
        // its tail: (69,87,105) (100,123,146) (129,156,179) (163,191,212), white streaks (212,233,243), line (40,55,78)
        static readonly int Tail = CelPalette.Add("ib_tail",
            C(0x3E4A56), C(0x526272), C(0x667A8A), C(0x7E97AA), C(0x9AC4EA), 1.1f, 4, C(0x233242));
        // the crystal: dark faces (72,91,113), blue-grey (107,140,162), sky (135,215,243), pale sky (178,222,240),
        // the white heart (225,247,251), a navy line (20,36,60)
        static readonly int Crystal = CelPalette.Add("ib_crystal",
            C(0x404C5B), C(0x566E7E), C(0x5EAED2), C(0x82B7D5), C(0xA3CEF0), 1.1f, 4, C(0x0A1E32));
        // the swirl of spray on the ground: greys (66,84,100) (98,118,134) (140,165,182) (176,212,232)
        static readonly int Swirl = CelPalette.Add("ib_swirl",
            C(0x3B4752), C(0x525F6A), C(0x70828C), C(0x84ADC4), C(0x86C0EE), 1.1f, 4, C(0x23303A));
        // the mist while it glows: its grey rim (90,109,128), (118,150,175), (150,200,228), its bright inside (182,232,250)
        static readonly int MistEarly = CelPalette.Add("ib_mist",
            C(0x4C5966), C(0x5E7687), C(0x70A0BA), C(0x7DC0F0), C(0x86C2F0), 1.1f, 4, C(0x303C48));
        // and as it thins: (56,66,80) (72,86,102) (92,110,127) (125,150,170)
        static readonly int MistLate = CelPalette.Add("ib_mist_late",
            C(0x333A44), C(0x404854), C(0x4E5A64), C(0x627684), C(0x7391A4), 1.1f, 4, C(0x212832));
        // the white dots that hang round the tail and glint in the mist: (212,233,243) to white
        static readonly int Chip = CelPalette.Add("ib_chip",
            C(0x667A8A), C(0x7E97AA), C(0x9AC4EA), C(0xB0CDF0), C(0xB4CEF0), 1.1f, 4, C(0x243D58));
        // the shards: sky ice (135,215,243) with a pale face (200,238,249) and a navy line
        static readonly int Shard = CelPalette.Add("ib_shard",
            C(0x404D5A), C(0x596F7C), C(0x83B8D0), C(0x8BC6F0), C(0xA3CEF0), 1.1f, 4, C(0x0C1F33));

        /// <summary>The light the ice throws on the ground and on the target: teal, part of it multiplied into
        /// what it lights (the ground keeps its pattern), part added on top. The clip's ground two metres from the
        /// ball goes from (40,46,59) to (83,135,165).</summary>
        static readonly Vector3 GlowInto = new Vector3(0.5f, 1.0f, 0.9f), GlowAdd = new Vector3(0.07f, 0.15f, 0.18f);

        // the crystal's big spikes, for the shards to come out of: the angle up from the eye's right (degrees),
        // the length (m), the frames after it starts to break at which this one does (CelIceBall_URP.shader's Spikes)
        static readonly Vector3[] BigSpikes =
        {
            new Vector3(123.2f, 5.3f, -1f), new Vector3(138.7f, 4.05f, 0.5f), new Vector3(68.7f, 6.3f, 4f), new Vector3(75.9f, 5f, 4.5f),
            new Vector3(98f, 3.9f, 2.5f), new Vector3(10.1f, 5.95f, 2f), new Vector3(-26f, 4f, 3f), new Vector3(-38.2f, 3.57f, 3f),
            new Vector3(59.5f, 3.7f, 3f), new Vector3(25.4f, 3.8f, 2f), new Vector3(179f, 2.83f, 1f),
        };

        static readonly int LensId = Shader.PropertyToID("_TWAcidLens");

        // the lens handed over is a global of the shader: a session that ends puts it back to none
        static IceBallRecipe() { SceneStatics.Register(nameof(IceBallRecipe), () => Shader.SetGlobalVector(LensId, Vector4.zero)); }

        /// <summary>
        /// The scene's lens vignette (URP Vignette's intensity and smoothness; 0 for none), for whoever shows the
        /// effect to hand over: the ice is written brighter where the vignette will darken it, so it shows the
        /// clip's colours all the way into the corners of the frame (as far as white allows).
        /// </summary>
        public static void Lens(float intensity, float smoothness) =>
            Shader.SetGlobalVector(LensId, new Vector4(Mathf.Max(0f, intensity) * 3f, Mathf.Max(0.01f, smoothness) * 5f, 0f, 0f));

        /// <summary>The launch velocity that carries the ball from where it starts to where it holds in Flight seconds.</summary>
        public static Vector3 Launch(Vector3 from, Vector3 hold) => (hold - from) / Flight + Vector3.up * (0.5f * Gravity * Flight);

        /// <summary>Where the ball that left `from` s seconds ago is (up to Flight: then it holds).</summary>
        public static Vector3 Along(Vector3 from, Vector3 hold, float s) => from + Launch(from, hold) * s + Vector3.down * (0.5f * Gravity * s * s);

        /// <summary>Where a ball flying from `from` at `target` stops and hangs before it goes in: 1.7 m short of it.</summary>
        public static Vector3 HoldSpot(Vector3 from, Vector3 target)
        {
            Vector3 d = target - from;
            d.y = 0f;
            return target - (d.sqrMagnitude > 0.001f ? d.normalized : Vector3.forward) * 1.7f - Vector3.up * 0.15f;
        }

        /// <summary>Play the ice ball at a target: it hangs 1.7 m short of it on its own line of flight.</summary>
        public static void Play(Vector3 from, Vector3 target, Vector3 ground, Vector3 right, uint seed, float delay = 0f) =>
            Play(from, HoldSpot(from, target), target, ground, right, seed, delay);

        /// <summary>
        /// Play the ice ball: it starts at `from`, flies to `hold` and hangs there, then lunges into `target` (the
        /// dummy's chest), whose foot stands on the ground at `ground`; `right` is the eye's level right. `delay`
        /// holds it for that long after the call. Everything is spawned now, the light it throws on the ground and
        /// the swirl too (the shader finds what they fall on in the camera's depth texture): nothing is left to do
        /// per frame. No allocation.
        /// </summary>
        public static void Play(Vector3 from, Vector3 hold, Vector3 target, Vector3 ground, Vector3 right, uint seed, float delay)
        {
            right.y = 0f;
            right = right.sqrMagnitude > 0.001f ? right.normalized : Vector3.right;
            Vector3 away = Vector3.Cross(right, Vector3.up);   // from the eye
            var renderer = CelFx.Renderer;
            int kind = renderer.KindId(IceBallKind.KindName);
            if (kind < 0) throw new System.InvalidOperationException("IceBall kind is not registered");
            float lod = CelFx.LodBlend(CelFx.Zoom);
            bool mid = lod >= 0.5f, far = lod >= 1.5f;
            float now = CelFx.Now;
            var rng = new CelRng(seed);
            float late = delay;                  // this play starts on the clip's frame 6
            ground.y = Ground(ground);
            Vector3 vel = Launch(from, hold);
            Vector3 dir = (hold - from).normalized;
            // the crystal's heart: where its spikes meet, 0.38 m to the eye's left of the chest (clip (806,396));
            // the mist gathers round it
            Vector3 heart = target - right * 0.38f + Vector3.up * 0.03f;
            // the ball goes in at the dummy's chest, in front of it: the dummy does not hide it
            Vector3 hitSpot = target - away * 0.6f;                          // (the shader draws the comet nearer the eye than it stands, so it goes in over the target)

            // ---- the light on the ground, under everything: one pool under the ball's path as it comes in, one
            // under the ball while it hangs (frames 14-42: two metres to the left of the dummy's foot and three
            // nearer, on the dummy too), then the one under the burst (frames 39-56).
            {
                Vector3 under = Vector3.Lerp(from, hold, 0.6f);
                under.y = Ground(under) + 1f;
                Glow(renderer, kind, now, AtFrame(late, 8f), AtFrame(late, 20f), under, right, 5.0f, 5.0f,
                    5.0f, 1f, far ? 0.4f : 0.6f, GlowInto, GlowAdd, 0.12f, 0.20f, true, seed);
                Vector3 pool = ground - right * 2f - away * 3f;
                pool.y = Ground(pool) + 1f;
                Glow(renderer, kind, now, AtFrame(late, 14f), AtFrame(late, 42.5f), pool, right, 4.6f, 4.6f,
                    4.6f, 1f, 1.0f, GlowInto, GlowAdd, 0.25f, 0.08f, false, seed);
                Vector3 burst = ground - away * 1f;
                burst.y = Ground(burst) + 1f;
                Glow(renderer, kind, now, AtFrame(late, 39f), AtFrame(late, 56f), burst, right, 5.5f, 5.5f,
                    5.5f, 1f, 0.8f, GlowInto, GlowAdd, 0.10f, 0.45f, false, seed);
            }

            // ---- the swirl of spray the burst throws over the ground (clip frames 42-53), under the crystal
            {
                var r = Rec(now, AtFrame(late, 41.5f), 14.5f / 24f, IceBallKind.Swirl, ground - right * 0.1f, Swirl, 1f, ref rng);
                r.Axis = right;
                r.Size = new Vector4(SwirlReach, 3.0f, 1f, 1f);
                renderer.Spawn(kind, r);
            }

            // ---- the comet: the ball and its tail, one strip along the path. In frame on the clip's frame 8,
            // hanging from 17, going in on 38-41.
            {
                var r = Rec(now, delay, Hit + 1.1f / 24f - 0.01f, IceBallKind.Comet, from, Ball, 1f, ref rng);   // its last drawing: frame 41
                r.Tone.x = CelRecord.PackPalette(Ball, Tail);
                r.Vel = vel;
                r.Axis = hitSpot - hold;
                r.Motion = new Vector4(Gravity, 0f, 0f, 0f);
                r.Size = new Vector4(BallRadius, TailLength, 2.4f, Flight);
                r.Extra.y = Hold;
                r.Extra.z = Lunge;
                renderer.Spawn(kind, r);
            }

            // ---- what hangs round the tail while the ball holds (clip frames 16-41): white dots over its upper
            // edge and under it, and a few small shards, hardly moving.
            int chips = far ? 0 : mid ? 20 : 50;
            float speed = vel.magnitude;
            for (int i = 0; i < chips; i++)
            {
                float back = rng.Range(0.4f, 6.5f);                           // metres behind the ball along its path
                Vector3 at = Along(from, hold, Flight - back / Mathf.Max(speed, 0.01f))
                    + Vector3.up * rng.Range(-1.3f, 1.6f) + right * rng.Range(-0.5f, 0.5f);
                bool shard = i % 5 == 0;
                float rad = shard ? rng.Range(0.07f, 0.13f) : rng.Range(0.018f, 0.045f);
                float born = AtFrame(late, 15f + rng.Range(0f, 5f));
                var r = Rec(now, born, AtFrame(late, 39f + rng.Range(0f, 2f)) - born, IceBallKind.Drop, at, shard ? Shard : Chip, 1f, ref rng);
                r.Vel = -dir * rng.Range(0.05f, 0.3f) + Vector3.up * rng.Range(-0.08f, 0.12f);
                r.Motion = new Vector4(0f, 0f, rng.Range(-0.5f, 0.5f), 0f);
                r.Size = new Vector4(rad, rad, 1f, 0.004f);
                r.Extra.y = shard ? IceBallKind.Shard : IceBallKind.Round;
                renderer.Spawn(kind, r);
            }

            // ---- the mist that gathers at its heart from frame 47, hides the dummy, parts round it and thins away
            // by 118. Behind the crystal: the crystal gives its heart up to it, its spikes stand out of it.
            {
                var r = Rec(now, AtFrame(late, 46.5f), 76.5f / 24f, IceBallKind.Mist, heart, MistEarly, 1f, ref rng);
                r.Tone.x = CelRecord.PackPalette(MistEarly, MistLate);
                r.Size = new Vector4(MistHalfWidth, MistHalfHeight, Pull, far ? 12f : mid ? 24f : 40f);
                renderer.Spawn(kind, r);
            }

            // ---- THE BURST: the crystal, one card. It shoots out on the clip's frame 42, stands to 57 and breaks
            // up over seven frames a spike, its tall spikes last (gone by 69).
            {
                var r = Rec(now, AtFrame(late, 40.5f), (BreakAt + 13.5f) / 24f, IceBallKind.Crystal, heart, Crystal, 1f, ref rng);
                r.Size = new Vector4(CrystalReach, CrystalReach, Pull, 1f);
                r.Extra.y = BreakAt;
                renderer.Spawn(kind, r);
            }

            // ---- small chips of ice thrown out of the burst (clip frames 42-50)
            int bits = far ? 0 : mid ? 8 : 18;
            for (int i = 0; i < bits; i++)
            {
                float turn = rng.Range(-0.15f, 1.15f) * Mathf.PI;
                Vector3 d = (right * Mathf.Cos(turn) + Vector3.up * Mathf.Sin(turn) + away * rng.Range(-0.3f, 0.3f)).normalized;
                float rad = rng.Range(0.04f, 0.09f);
                var r = Rec(now, AtFrame(late, 42f + rng.Range(0f, 2f)), rng.Range(0.25f, 0.50f), IceBallKind.Drop,
                    heart + d * rng.Range(0.8f, 1.6f) - away * 0.8f, Shard, 1f, ref rng);
                r.Vel = d * rng.Range(5f, 11f);
                r.Motion = new Vector4(6f, 1.2f, rng.Range(-9f, 9f), 0f);
                r.Size = new Vector4(rad, rad * 0.6f, 1f, 0.004f);
                r.Extra.y = IceBallKind.Shard;
                renderer.Spawn(kind, r);
            }

            // ---- the shards the crystal breaks into (clip frames 58-72): small kites of ice that come off where
            // its big spikes stood as each one goes, tumble up and out, and fall.
            int shards = far ? 0 : mid ? 22 : 56;
            for (int i = 0; i < shards; i++)
            {
                Vector3 spike = BigSpikes[i % BigSpikes.Length];
                float turn = (spike.x + rng.Range(-6f, 6f)) * Mathf.Deg2Rad;
                Vector3 d = right * Mathf.Cos(turn) + Vector3.up * Mathf.Sin(turn);
                float rad = i % 4 == 0 ? rng.Range(0.16f, 0.30f) : rng.Range(0.06f, 0.16f);   // one in four a big piece
                var r = Rec(now, AtFrame(late, 41.5f + BreakAt + spike.z + rng.Range(0.5f, 6.5f)), rng.Range(0.45f, 1.1f), IceBallKind.Drop,
                    heart + d * rng.Range(1.4f, spike.y) - away * 0.8f, Shard, 1f, ref rng);
                r.Vel = d * rng.Range(0.5f, 2.5f) + Vector3.up * rng.Range(0.5f, 3f) + right * rng.Range(-1f, 1f);
                r.Motion = new Vector4(6f, 0.6f, rng.Range(-8f, 8f), 0f);
                r.Size = new Vector4(rad, rad * 0.7f, 1f, 0.004f);
                r.Extra.y = IceBallKind.Shard;
                renderer.Spawn(kind, r);
            }

            // ---- the glints that hang in the mist (clip frames 64-104): small chips that catch the light and sink
            int glints = far ? 0 : mid ? 9 : 22;
            for (int i = 0; i < glints; i++)
            {
                Vector3 at = heart + right * rng.Range(-3.2f, 3.2f) + Vector3.up * rng.Range(-1.4f, 2.2f) - away * 0.8f;
                bool shard = i % 2 == 0;
                float rad = shard ? rng.Range(0.04f, 0.08f) : rng.Range(0.02f, 0.04f);
                var r = Rec(now, AtFrame(late, 64f + rng.Range(0f, 30f)), rng.Range(0.30f, 0.80f), IceBallKind.Drop, at, shard ? Shard : Chip, 1f, ref rng);
                r.Vel = Vector3.up * rng.Range(-0.6f, 0.2f) + right * rng.Range(-0.3f, 0.3f);
                r.Motion = new Vector4(1.5f, 0.5f, rng.Range(-5f, 5f), 0f);
                r.Size = new Vector4(rad, rad * 0.5f, 1f, 0.004f);
                r.Extra.y = shard ? IceBallKind.Shard : IceBallKind.Round;
                renderer.Spawn(kind, r);
            }
        }

        /// <summary>Lab only: a card of flat colours at `at`, `halfExtent` metres to a side, for `seconds`: a film
        /// of it tells what the scene's grade makes of a written colour (the shader's mode 5). `palette`: the
        /// effect's own eight palette rows instead of the lattice, to check them against the clip's.</summary>
        public static void Chart(Vector3 at, float halfExtent, float seconds, bool palette = false)
        {
            var renderer = CelFx.Renderer;
            int kind = renderer.KindId(IceBallKind.KindName);
            if (kind < 0) throw new System.InvalidOperationException("IceBall kind is not registered");
            var rng = new CelRng(1u);
            var r = Rec(CelFx.Now, 0f, seconds, IceBallKind.Chart, at, Ball, 1f, ref rng);
            r.Size = new Vector4(halfExtent, halfExtent, 3f, 1f);
            r.Extra.y = palette ? 1f : 0f;
            renderer.Spawn(kind, r);
        }

        static void Glow(CelFxRenderer renderer, int kind, float now, float born, float gone, Vector3 at, Vector3 axis,
            float along, float up, float behind, float ahead, float strength, Vector3 into, Vector3 add, float rise, float fall, bool level, uint seed)
        {
            var own = new CelRng(seed);   // a light has nothing random in it: the effect's own draws stay as they were
            var r = Rec(now, born, gone - born, IceBallKind.Glow, at, Ball, strength, ref own);
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
