// Phase: CelFx fireball (2026-11-14; s01 2026-11-15: rebuilt by eye against the clip). A comet of fire flies in
// from the eye's near left, hits the dummy and bursts into a mound of round cel lobes that cool to brown smoke and
// break up, with white spark rays, a fountain of sparks that fall as embers, a line of embers left along the
// comet's path and an orange light on the ground.
// Reference: fireball_nolora.mp4, 192 f at 24 fps, drawn on twos; the effect starts on its frame 12 and the hit is
// frame 28. Sizes and places are read off the clip through the beach Range's camera (a pinhole of 1339 px focal
// length at 1344 x 768: 70.9 px to the metre at the dummy); colours are the clip's pixels.
//
// It draws through the kind's own shader (CelFireball_URP): 0 the comet (head and tail on one strip), 1 a fleck
// (tail embers, spark rays, falling sparks, ground embers), 2 the hit's star, 3 the cloud (the whole burst, from
// fire to smoke, on one card) and 7 the light on the ground.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class FireballRecipe
    {
        /// <summary>Seconds from the comet's first light to the hit: clip frames 12 to 28.</summary>
        public const float Flight = 0.6667f;
        /// <summary>The comet flies nearly straight; the clip's path is 0.2 m fuller than a line at its middle.</summary>
        public const float Gravity = 3.2f;
        /// <summary>Seconds the whole effect is alive: first light frame 12, nothing left by frame 120.</summary>
        public const float Length = 4.6f;

        /// <summary>The clip's effect starts on its frame 12. AtFrame gives the seconds after the call at which one
        /// of the clip's frames falls; `late` is how much later than the clip's this play starts.</summary>
        const float FirstFrame = 12f;
        static float AtFrame(float late, float frame) => (frame - FirstFrame) / 24f - 0.004f + late;
        /// <summary>A time moved onto the drawing it falls in (the effect is drawn on twos), a hair early so the
        /// record is alive on that drawing's first frame.</summary>
        static float OnTwos(float seconds) => Mathf.Round(seconds * 12f) / 12f - 0.004f;

        // ---- sizes, in metres
        /// <summary>The comet's head: 95 px of radius at 11.4 m from the eye (frame 16), 56 px at the dummy.</summary>
        public const float HeadRadius = 0.79f;
        /// <summary>The tail, as long as it shows across the view (it runs away from the eye, so it is longer than this).</summary>
        const float TailLength = 10.5f;
        /// <summary>The cloud at its peak (clip frame 40): 410 x 350 px, its middle a little right of and above the
        /// dummy's chest.</summary>
        public const float CloudHalfWidth = 2.9f, CloudHalfHeight = 2.45f;
        /// <summary>Metres the cloud's card is drawn toward the eye, so the ground in front of the dummy does not cut its foot.</summary>
        const float CloudPull = 2.6f;

        // A colour as the film shows it. The project renders in gamma space and the palette stores .linear, so a
        // colour handed over as picked comes out one gamma too dark (orange goes red, the smoke goes black):
        // this takes it back first (NeonAcidRecipe.S does the same).
        static Color C(int rgb)
        {
            Color c = CelPalette.Hex(rgb);
            return new Color(Mathf.LinearToGammaSpace(c.r), Mathf.LinearToGammaSpace(c.g), Mathf.LinearToGammaSpace(c.b), 1f);
        }

        // ---- colours. The comments give what the clip shows; the values are what has to be written for the Range's
        // night grade to show that (measured on films s04/s05 with runs/fireball/_s1/clusters.py: the grade turns a
        // written orange (250,110,5) into (254,158,0) and a written smoke (85,66,57) into (103,75,62)).
        // the comet: red-orange rim (250,70,5), orange (240,137,25), yellow (250,247,15), cream, white; ink = the
        // deep red splat on its nose (249,24,0)
        static readonly int Comet = CelPalette.Add("fb_comet",
            C(0xFA2A04), C(0xF55C14), C(0xFAF70F), C(0xFCFAA0), C(0xFDFCF4), 1.1f, 4, C(0xF90A00));
        // the cloud's fire: red-orange (251,60,5), orange (250,105,5), yellow (248,246,10), (cream), white
        // (251,249,231); ink = the red-brown line round a hot lobe (190,85,43)
        static readonly int Fire = CelPalette.Add("fb_fire",
            C(0xFB2404), C(0xFA4605), C(0xF8F60A), C(0xFCFACD), C(0xFAF8F4), 1.1f, 4, C(0xB4401E));
        // what the cloud cools to: char (39,28,23), the crease between lobes (75,61,54), the smoke as it comes
        // (83,67,58) and as it goes (90,73,64)
        static readonly int Smoke = CelPalette.Add("fb_smoke",
            C(0x191411), C(0x463B34), C(0x4F4239), C(0x56483F), C(0x56483F), 1.1f, 4, C(0x191411));
        // the embers the tail leaves: red-orange (250,70,5) with an orange-yellow middle (250,150,20)
        static readonly int Ember = CelPalette.Add("fb_ember",
            C(0xE01802), C(0xE01802), C(0xFA2A04), C(0xFA640F), C(0xFA640F), 1.1f, 4, C(0x401004));
        // the spark rays: white with a pink edge
        static readonly int Ray = CelPalette.Add("fb_ray",
            C(0xFF8C6E), C(0xFF8C6E), C(0xFF7858), C(0xFFF6EB), C(0xFFF6EB), 1.1f, 4, C(0x401004));
        // the sparks that fall: orange (250,100,20) with a pale middle
        static readonly int Fall = CelPalette.Add("fb_fall",
            C(0xF03C08), C(0xF03C08), C(0xFA3E0E), C(0xFFD2A0), C(0xFFD2A0), 1.1f, 4, C(0x401004));

        /// <summary>The light the fire throws on the ground: most of it multiplied into the ground (it keeps its
        /// pattern), the rest added on top.</summary>
        static readonly Vector3 GlowInto = new Vector3(1.40f, 0.52f, 0.08f), GlowAdd = new Vector3(0.22f, 0.075f, 0.015f);
        const float UnderReach = 8.0f, BurstReach = 10.0f;

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
            float late = delay;                  // this play starts on the clip's frame 12
            float hit = delay + Flight;          // the comet lands (clip frame 28)
            ground.y = Ground(ground);
            // the cloud's middle: 0.25 m to the right of the chest and 0.35 m over it
            Vector3 heart = target + right * 0.25f + Vector3.up * 0.35f;

            // ---- the light on the ground, under everything: one pool under the comet's path as it comes in, one
            // where it lands, then the big one under the burst (strongest on frames 30-46, gone by 72).
            {
                Vector3 under = Vector3.Lerp(from, target, 0.28f);
                under.y = Ground(under) + 1f;
                Glow(renderer, kind, now, AtFrame(late, 12f), AtFrame(late, 24f), under, right, UnderReach, UnderReach,
                    UnderReach, 1f, far ? 1.0f : mid ? 1.5f : 2.1f, GlowInto, GlowAdd, 0.16f, 0.25f, true, seed);
                Vector3 near = Vector3.Lerp(from, target, 0.78f);
                near.y = Ground(near) + 1f;
                Glow(renderer, kind, now, AtFrame(late, 17f), AtFrame(late, 31f), near, right, UnderReach, UnderReach,
                    UnderReach, 1f, far ? 1.0f : mid ? 1.5f : 2.1f, GlowInto, GlowAdd, 0.22f, 0.20f, true, seed);
                Vector3 pool = ground;
                pool.y = Ground(pool) + 1f;
                Glow(renderer, kind, now, AtFrame(late, 27f), AtFrame(late, 80f), pool, right, BurstReach, BurstReach,
                    BurstReach, 1f, 2.0f, GlowInto, GlowAdd, 0.12f, 1.5f, true, seed);
            }

            // ---- the comet: its head and its tail, one strip along the path. It is in frame on the clip's frame
            // 14 and on the dummy by 27; after the hit the tail breaks up and its blobs shrink away by frame 40.
            {
                var r = Rec(now, OnTwos(delay), Flight + 0.62f, FireballKind.Comet, from, Comet, 1f, ref rng);
                r.Vel = Launch(from, target - away * 0.9f);   // it lands on the dummy's face, not inside it: the dummy does not hide the head
                r.Motion = new Vector4(Gravity, 0f, 0f, 0f);
                r.Size = new Vector4(HeadRadius, TailLength, 2.0f, Flight);
                renderer.Spawn(kind, r);
            }

            // ---- the embers the tail leaves along the path it flew (clip frames 24-44): round orange dots that
            // hang where the comet passed and go out one by one.
            int embers = far ? 0 : mid ? 12 : 30;
            for (int i = 0; i < embers; i++)
            {
                float back = (i + rng.Range(0f, 1f)) / embers;                // 0 at the hit, 1 far back along the path
                float when = Flight * (1f - back * 0.50f);                    // the head was here then
                Vector3 at = Along(from, target - away * 0.9f, when) + Vector3.up * rng.Range(-0.5f, 0.7f) + right * rng.Range(-0.2f, 0.2f);
                float rad = rng.Range(0.04f, 0.10f);
                var r = Rec(now, OnTwos(hit - 0.10f + rng.Range(0f, 0.40f)), rng.Range(0.30f, 0.80f),
                    FireballKind.Drop, at, Ember, 1f, ref rng);
                r.Vel = Vector3.up * rng.Range(0.2f, 1.4f) - (target - from).normalized * rng.Range(0f, 0.6f);
                r.Motion = new Vector4(0.6f, 0.8f, 0f, 0f);
                r.Size = new Vector4(rad, rad * 0.30f, 1.6f, 0.004f);
                r.Extra.y = 1f;                  // a round fleck
                renderer.Spawn(kind, r);
            }

            // ---- THE BURST: one card. Fire, char, smoke and flecks, on the clip's frame clock from the hit.
            {
                var r = Rec(now, hit - 0.004f, Length - Flight - 0.08f, FireballKind.Cloud, heart, Fire, 1f, ref rng);
                r.Tone = new Vector4(CelRecord.PackPalette(Fire, Smoke), 1f, 1f, 0f);
                r.Size = new Vector4(CloudHalfWidth, CloudHalfHeight, CloudPull, far ? 9f : mid ? 18f : 28f);
                renderer.Spawn(kind, r);
            }

            // ---- the hit: a white star of spikes over the young cloud for two drawings (clip frames 28-32)
            if (!far)
            {
                var r = Rec(now, OnTwos(hit + 0.083f), 0.20f, FireballKind.Splat, heart - Vector3.up * 0.35f, Ray, 1f, ref rng);
                r.Size = new Vector4(1.3f, 1.3f, 1f, 1f);
                r.Extra.y = CloudPull + 0.3f;    // in front of the cloud's card
                renderer.Spawn(kind, r);
            }

            // ---- the white spark rays out of the burst (clip frames 34-50): short needles thrown out all round its
            // top, each two dashes one behind the other, there and gone in two drawings.
            int rays = far ? 0 : mid ? 28 : 56;
            for (int i = 0; i < rays; i++)
            {
                float turn = rng.Range(-0.12f, 1.12f) * Mathf.PI;             // over the top, a little under the sides
                Vector3 d = (right * Mathf.Cos(turn) + Vector3.up * Mathf.Sin(turn) + away * rng.Range(-0.25f, 0.25f)).normalized;
                float born = hit + rng.Range(0.20f, 0.95f);
                float reach = rng.Range(2.0f, 3.4f), speed = rng.Range(4f, 8f);
                for (int j = 0; j < (mid ? 2 : 3); j++)
                {
                    var r = Rec(now, OnTwos(born), 0.24f, FireballKind.Drop, heart + d * (reach + j * rng.Range(0.45f, 0.75f)), Ray, 1f, ref rng);
                    r.Vel = d * speed;
                    r.Size = new Vector4(0.052f, 0.034f, 1f, rng.Range(0.022f, 0.044f));   // the stretch is seconds of its own motion (half its length)
                    r.Extra.y = 2f;              // a leaf
                    renderer.Spawn(kind, r);
                }
            }

            // ---- the fountain of sparks (clip frames 46-90): thrown out of the cloud's rim in arcs, short bright
            // dashes that fall as long thin embers.
            int sparks = far ? 0 : mid ? 50 : 120;
            for (int i = 0; i < sparks; i++)
            {
                float turn = rng.Range(-0.05f, 1.05f) * Mathf.PI;
                Vector3 d = (right * Mathf.Cos(turn) + Vector3.up * Mathf.Sin(turn) + away * rng.Range(-0.5f, 0.5f)).normalized;
                var r = Rec(now, OnTwos(hit + rng.Range(0.70f, 1.45f)), rng.Range(0.7f, 1.5f), FireballKind.Drop,
                    heart + d * rng.Range(1.6f, 2.6f), Fall, 1f, ref rng);
                r.Vel = d * rng.Range(3f, 9f) + Vector3.up * rng.Range(1f, 4f);
                r.Motion = new Vector4(17f, 0.9f, 0f, 0f);
                r.Size = new Vector4(0.034f, 0.022f, 1f, rng.Range(0.024f, 0.052f));
                r.Extra.y = 2f;
                renderer.Spawn(kind, r);
            }

            // ---- the embers left on the ground (clip frames 80-114): small glowing dots that go out one by one
            int coals = far ? 0 : mid ? 7 : 18;
            for (int i = 0; i < coals; i++)
            {
                Vector2 on = rng.InDisc() * 5.5f;
                Vector3 at = ground + right * on.x + away * (on.y * 0.8f);
                at.y = Ground(at) + 0.06f;
                float rad = rng.Range(0.04f, 0.075f);
                var r = Rec(now, OnTwos(hit + rng.Range(2.0f, 2.8f)), rng.Range(0.6f, 1.3f), FireballKind.Drop, at, Fall, 1f, ref rng);
                r.Size = new Vector4(rad, rad * 0.6f, 1f, 0.004f);
                r.Extra.y = 1f;
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
