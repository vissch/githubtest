// Phase: B7 (2026-10-01, first pass) — the game's sound effects, made from noise and sine at load (no audio assets
// exist; Storm's thunder is made the same way). Pure: each sound is a float[] at Rate, so a test and the quality check
// (SfxBench) read exactly what is played. Every sound comes in a near and a far form: far is what reaches you across the
// field, low-passed (the air eats the crack), slower to start and longer to ring. Variants differ by seed, so a volley
// is not one sample repeated.
// Mastering, the same for every sound: a 2 ms fade in and a 10 ms fade out (no click at either end), the DC taken out,
// the peaks rounded off (tanh) and the loudest sample brought to Peak.
namespace TW.Presentation.Audio
{
    public enum Sfx : byte
    {
        Rifle, Mg, BoomSmall, BoomBig, TankGun, Clang, Ricochet, Whistle, Crackle, Hiss, Snap, CookOff,
        Count
    }

    public static class SfxSynth
    {
        public const int Rate = 32000;
        public const float Peak = 0.89f;   // -1 dBFS: headroom for the mixer, never the ceiling
        public const int Variants = 3;

        /// <summary>How long a sound lasts (s), near or far.</summary>
        public static float Seconds(Sfx s, bool far)
        {
            switch (s)
            {
                case Sfx.Rifle: return far ? 1.1f : 0.6f;
                case Sfx.Mg: return far ? 0.8f : 0.35f;
                case Sfx.BoomSmall: return far ? 2.6f : 1.8f;
                case Sfx.BoomBig: return far ? 4.5f : 3.6f;
                case Sfx.TankGun: return far ? 3.2f : 2.4f;
                case Sfx.Clang: return 1.1f;
                case Sfx.Ricochet: return 0.7f;
                case Sfx.Whistle: return 1.5f;
                case Sfx.Crackle: return 1.6f;
                case Sfx.Hiss: return 3.2f;
                case Sfx.Snap: return 0.14f;
                case Sfx.CookOff: return far ? 5.5f : 5f;
                default: return 0.5f;
            }
        }

        /// <summary>The samples of one sound (mono, Rate Hz, mastered).</summary>
        public static float[] Make(Sfx s, int variant, bool far)
        {
            int n = (int)(Rate * Seconds(s, far));
            var d = new float[n];
            var g = new Gen(0x9E3779B9u * (uint)((int)s * 31 + variant * 7 + (far ? 1000 : 0) + 1));
            float pitch = 1f + (variant - 1) * 0.06f;   // the variants a little apart in pitch as well as in noise
            switch (s)
            {
                case Sfx.Rifle: Shot(d, ref g, far, pitch, body: 900f, thump: 95f, tail: 0.22f); break;
                case Sfx.Mg: Shot(d, ref g, far, pitch * 0.9f, body: 700f, thump: 80f, tail: 0.1f); break;
                case Sfx.BoomSmall: Boom(d, ref g, far, pitch * 1.4f, size: 0.55f); break;
                case Sfx.BoomBig: Boom(d, ref g, far, pitch, size: 1f); break;
                case Sfx.TankGun: Gun(d, ref g, far, pitch); break;
                case Sfx.Clang: Clang(d, ref g, pitch); break;
                case Sfx.Ricochet: Ricochet(d, ref g, pitch); break;
                case Sfx.Whistle: Whistle(d, ref g, pitch); break;
                case Sfx.Crackle: Crackle(d, ref g, 1f); break;
                case Sfx.Hiss: Hiss(d, ref g); break;
                case Sfx.Snap: Snap(d, ref g, pitch); break;
                case Sfx.CookOff: Boom(d, ref g, far, pitch * 0.9f, size: 1.1f); Popcorn(d, ref g, far); break;
            }
            Master(d);
            return d;
        }

        // ------------------------------------------------------------------ the recipes
        /// <summary>A gunshot: the crack (bright noise, gone in a few ms), the body (band noise round `body` Hz), a low
        /// thump, and the report rolling off the ground (`tail`). Far: the crack lost, the rest low-passed and smeared.</summary>
        static void Shot(float[] d, ref Gen g, bool far, float pitch, float body, float thump, float tail)
        {
            var lp = new OnePole(); var bp1 = new OnePole(); var bp2 = new OnePole(); var low = new OnePole(); var far1 = new OnePole(); var far2 = new OnePole();
            float aBody = Coef(body * pitch * 1.6f), aBodyLow = Coef(body * pitch * 0.5f), aLow = Coef(160f), aFar = Coef(far ? 650f : 9000f);
            float phase = 0f;
            for (int i = 0; i < d.Length; i++)
            {
                float t = i / (float)Rate;
                float w = g.White();
                float hp = w - lp.Run(w, Coef(2500f));                                     // the crack: noise above ~2.5 kHz
                float band = bp1.Run(w, aBody) - bp2.Run(w, aBodyLow);                     // the body
                float rumble = low.Run(w, aLow);
                phase += 6.2831853f * thump * pitch * (1f - 0.4f * Clamp01(t * 12f)) / Rate;
                float x = 0f;
                if (!far) x += hp * Env(t, 0.0005f, 140f) * 1.1f;
                x += band * Env(t, far ? 0.012f : 0.001f, far ? 14f : 32f) * (far ? 2.2f : 2.8f);
                x += (float)System.Math.Sin(phase) * Env(t, 0.002f, far ? 18f : 28f) * (far ? 0.5f : 0.7f);
                x += rumble * Env(t, far ? 0.03f : 0.01f, far ? 3.5f : 6f) * tail * (far ? 9f : 6f);
                if (far) x = far2.Run(far1.Run(x, aFar), aFar);
                d[i] = x;
            }
        }

        /// <summary>An explosion: the blast (a burst of noise), the low thump that falls in pitch (the pressure wave), the
        /// roar of it rolling across the field, and earth and stones pattering down after. `size` 1 a big shell.</summary>
        static void Boom(float[] d, ref Gen g, bool far, float pitch, float size)
        {
            var lp = new OnePole(); var mid = new OnePole(); var low1 = new OnePole(); var low2 = new OnePole(); var f1 = new OnePole(); var f2 = new OnePole(); var f3 = new OnePole();
            float aMid = Coef(1200f), aLow = Coef(220f * pitch), aLower = Coef(70f * pitch), aFar = Coef(380f);
            float phase = 0f, decay = 1.6f / size;
            for (int i = 0; i < d.Length; i++)
            {
                float t = i / (float)Rate;
                float w = g.White();
                float m = mid.Run(w, aMid);
                float r1 = low1.Run(w, aLow), r2 = low2.Run(r1, aLower);
                float f = (52f + 60f * (float)System.Math.Exp(-t * 9f)) * pitch;          // the thump falls from ~110 to ~50 Hz
                phase += 6.2831853f * f / Rate;
                float x = 0f;
                if (!far) x += (w - lp.Run(w, Coef(3000f))) * Env(t, 0.0005f, 60f) * 0.9f;   // the crack of it
                x += m * Env(t, far ? 0.02f : 0.002f, far ? 7f : 11f) * 3f;
                x += (float)System.Math.Sin(phase) * Env(t, far ? 0.03f : 0.004f, decay * 1.6f) * 1.3f * size;
                x += (r1 * 5f + r2 * 14f) * Env(t, far ? 0.08f : 0.02f, decay) * size;
                // earth falling back: sparse little ticks between 0.25 s and 1.6 s (near only: far off you do not hear it)
                if (!far && t > 0.25f && t < 1.6f && g.Next() < 0.0025f * (1.6f - t) * size) x += (g.White() * 0.5f + 0.5f) * 0.6f * (1.7f - t);
                if (far) x = f3.Run(f2.Run(f1.Run(x, aFar), aFar), aFar) * 1.8f;
                d[i] = x;
            }
        }

        /// <summary>A machine's main gun: a hard crack, a deep boom, and the barrel's ring under it.</summary>
        static void Gun(float[] d, ref Gen g, bool far, float pitch)
        {
            Boom(d, ref g, far, pitch * 0.85f, size: 0.8f);
            var lp = new OnePole(); var f1 = new OnePole(); var f2 = new OnePole();
            float aFar = Coef(500f);
            for (int i = 0; i < d.Length; i++)
            {
                float t = i / (float)Rate;
                float w = g.White();
                float x = far ? 0f : (w - lp.Run(w, Coef(1800f))) * Env(t, 0.0003f, 45f) * 1.6f;
                x += (float)(System.Math.Sin(6.2831853 * 182.0 * pitch * t) + 0.6 * System.Math.Sin(6.2831853 * 497.0 * pitch * t)) * Env(t, 0.004f, 5f) * 0.18f;
                if (far) x = f2.Run(f1.Run(x, aFar), aFar);
                d[i] += x;
            }
        }

        /// <summary>A round striking armour: struck plate, its partials inharmonic (a bell, not a note), each dying at its
        /// own rate, over a hard click.</summary>
        static void Clang(float[] d, ref Gen g, float pitch)
        {
            float[] f = { 412f, 1037f, 1781f, 2643f, 3511f }; float[] a = { 0.9f, 0.7f, 0.5f, 0.35f, 0.2f }; float[] k = { 4.5f, 7f, 10f, 14f, 19f };
            var lp = new OnePole();
            for (int i = 0; i < d.Length; i++)
            {
                float t = i / (float)Rate;
                float x = 0f;
                for (int j = 0; j < f.Length; j++) x += a[j] * (float)System.Math.Sin(6.2831853 * f[j] * pitch * t + j) * (float)System.Math.Exp(-t * k[j]);
                float w = g.White();
                x = x * Clamp01(t / 0.0008f) * 0.6f + (w - lp.Run(w, Coef(2000f))) * Env(t, 0.0002f, 120f) * 1.2f;
                d[i] = x;
            }
        }

        /// <summary>A round glancing off: a click and a falling whine.</summary>
        static void Ricochet(float[] d, ref Gen g, float pitch)
        {
            float phase = 0f;
            for (int i = 0; i < d.Length; i++)
            {
                float t = i / (float)Rate;
                float f = (1400f + 2200f * (float)System.Math.Exp(-t * 4f)) * pitch;
                phase += 6.2831853f * f / Rate;
                d[i] = (float)System.Math.Sin(phase) * Env(t, 0.004f, 5.5f) * 0.6f + g.White() * Env(t, 0.0002f, 200f) * 0.8f;
            }
        }

        /// <summary>A shell coming in: a falling whistle with a breath of air round it, louder as it nears, cut off where
        /// it lands (the burst takes over).</summary>
        static void Whistle(float[] d, ref Gen g, float pitch)
        {
            float phase = 0f; var bp1 = new OnePole(); var bp2 = new OnePole();
            float T = d.Length / (float)Rate;
            for (int i = 0; i < d.Length; i++)
            {
                float t = i / (float)Rate, u = t / T;
                float f = (1650f - 900f * u * u) * pitch * (1f + 0.004f * (float)System.Math.Sin(t * 37f));
                phase += 6.2831853f * f / Rate;
                float w = g.White();
                float air = bp1.Run(w, Coef(f * 1.3f)) - bp2.Run(w, Coef(f * 0.7f));
                float env = u * u * Clamp01((T - t) / 0.03f);
                d[i] = ((float)System.Math.Sin(phase) * 0.5f + air * 1.6f) * env;
            }
        }

        /// <summary>Fire taking hold: a soft whoomph of air and the crackle of it, popping at random.</summary>
        static void Crackle(float[] d, ref Gen g, float size)
        {
            var low = new OnePole(); var lp = new OnePole();
            for (int i = 0; i < d.Length; i++)
            {
                float t = i / (float)Rate;
                float w = g.White();
                float x = low.Run(w, Coef(300f)) * 6f * Env(t, 0.06f, 2.2f) * size;
                if (g.Next() < 0.004f * (1.6f - t)) x += (g.White() > 0f ? 1f : -1f) * (0.4f + 0.5f * g.Next());
                x += (w - lp.Run(w, Coef(4000f))) * 0.08f * Env(t, 0.05f, 1.5f);
                d[i] = x;
            }
        }

        /// <summary>Gas or smoke let go: a long hiss that swells and sinks.</summary>
        static void Hiss(float[] d, ref Gen g)
        {
            var lp = new OnePole(); var lp2 = new OnePole();
            float T = d.Length / (float)Rate;
            for (int i = 0; i < d.Length; i++)
            {
                float t = i / (float)Rate;
                float w = g.White();
                float hiss = lp2.Run(w - lp.Run(w, Coef(1500f)), Coef(7000f));
                d[i] = hiss * Clamp01(t / 0.4f) * Clamp01((T - t) / 1.2f) * 1.4f;
            }
        }

        /// <summary>A round passing close: the supersonic snap, a few milliseconds of bright noise and a tiny tick.</summary>
        static void Snap(float[] d, ref Gen g, float pitch)
        {
            var lp = new OnePole();
            for (int i = 0; i < d.Length; i++)
            {
                float t = i / (float)Rate;
                float w = g.White();
                d[i] = (w - lp.Run(w, Coef(3500f * pitch))) * Env(t, 0.0003f, 70f) * 1.2f;
            }
        }

        /// <summary>A cook-off's ammunition going: pops scattered over three seconds after the blast.</summary>
        static void Popcorn(float[] d, ref Gen g, bool far)
        {
            var low = new OnePole();
            for (int i = 0; i < d.Length; i++)
            {
                float t = i / (float)Rate;
                if (t < 0.5f || t > 3.8f) continue;
                if (g.Next() < (far ? 0.0002f : 0.0004f))
                {
                    // a pop: 40 ms of low noise
                    int len = Rate / 25; float amp = (far ? 0.25f : 0.5f) * (0.5f + g.Next());
                    for (int j = 0; j < len && i + j < d.Length; j++) d[i + j] += low.Run(g.White(), Coef(far ? 300f : 900f)) * amp * 4f * (float)System.Math.Exp(-j / (float)len * 5f);
                }
            }
        }

        // ------------------------------------------------------------------ mastering and the parts
        /// <summary>Fades at both ends, the DC out, the peaks rounded off, the loudest sample brought to Peak.</summary>
        public static void Master(float[] d)
        {
            if (d.Length == 0) return;
            double mean = 0; for (int i = 0; i < d.Length; i++) mean += d[i]; mean /= d.Length;
            int fadeIn = Rate / 500, fadeOut = Rate / 100;
            float peak = 0f;
            for (int i = 0; i < d.Length; i++)
            {
                float x = d[i] - (float)mean;
                if (i < fadeIn) x *= i / (float)fadeIn;
                int left = d.Length - 1 - i; if (left < fadeOut) x *= left / (float)fadeOut;
                x = (float)System.Math.Tanh(x * 1.1f);
                d[i] = x; peak = System.Math.Max(peak, System.Math.Abs(x));
            }
            float gain = peak > 1e-6f ? Peak / peak : 0f;
            for (int i = 0; i < d.Length; i++) d[i] *= gain;
        }

        /// <summary>An attack-then-exponential-decay envelope: a linear rise over `attack` s, then exp(-k t).</summary>
        static float Env(float t, float attack, float k) => Clamp01(t / attack) * (float)System.Math.Exp(-k * t);
        static float Clamp01(float x) => x < 0f ? 0f : x > 1f ? 1f : x;
        /// <summary>A one-pole low-pass's coefficient for a cutoff in Hz.</summary>
        static float Coef(float hz) => 1f - (float)System.Math.Exp(-6.2831853 * hz / Rate);

        struct OnePole { float y; public float Run(float x, float a) { y += (x - y) * a; return y; } }

        /// <summary>A small, seeded noise source (an LCG): the same sound every load.</summary>
        struct Gen
        {
            uint s;
            public Gen(uint seed) { s = seed == 0 ? 1u : seed; }
            public float Next() { s = s * 1664525u + 1013904223u; return (s >> 8) / 16777216f; }
            public float White() => Next() * 2f - 1f;
        }
    }
}
