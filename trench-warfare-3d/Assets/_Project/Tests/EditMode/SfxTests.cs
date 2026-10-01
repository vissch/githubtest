// Phase: B7 (2026-10-01) — the sound effects' quality, checked on the very samples the game plays (SfxSynth, and Storm's
// thunder): nothing silent, nothing clipped or at the ceiling, no click at either end, no DC offset, the far form darker
// than the near one, every variant its own; and the mix (SfxMix): the focus loudest, a blast carrying further than a
// rifle, the far form only past half the carry, the delay of sound, the pan never hard. The owner, 2026-10-01: "also
// check if the sound quality is ok everywhere". ExportForListening (Explicit) writes every sound as a WAV to listen to.
using System;
using System.IO;
using NUnit.Framework;
using TW.Presentation.Audio;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class SfxTests
    {
        static System.Collections.Generic.IEnumerable<(string name, float[] data, int rate)> Every()
        {
            for (int s = 0; s < (int)Sfx.Count; s++)
                for (int v = 0; v < SfxSynth.Variants; v++)
                {
                    yield return ($"{(Sfx)s}_near_{v}", SfxSynth.Make((Sfx)s, v, false), SfxSynth.Rate);
                    yield return ($"{(Sfx)s}_far_{v}", SfxSynth.Make((Sfx)s, v, true), SfxSynth.Rate);
                }
            yield return ("Thunder_near", Storm.ThunderSamples(0, 1f, 5.5f), Storm.ThunderRate);
            yield return ("Thunder_mid", Storm.ThunderSamples(1, .45f, 6.5f), Storm.ThunderRate);
            yield return ("Thunder_far", Storm.ThunderSamples(2, .08f, 7.5f), Storm.ThunderRate);
        }

        static float Rms(float[] d) { double a = 0; foreach (var x in d) a += x * x; return (float)Math.Sqrt(a / Math.Max(1, d.Length)); }
        /// <summary>How bright a sound is: the mean step between samples over the mean level (high for a crack, low for a rumble).</summary>
        static float Brightness(float[] d) { double step = 0, lvl = 0; for (int i = 1; i < d.Length; i++) { step += Math.Abs(d[i] - d[i - 1]); lvl += Math.Abs(d[i]); } return (float)(step / Math.Max(1e-9, lvl)); }

        [Test]
        public void EverySoundIsCleanFromItsFirstSampleToItsLast()
        {
            foreach (var (name, d, rate) in Every())
            {
                Assert.Greater(d.Length, rate / 20, name + ": too short");
                float peak = 0f; double sum = 0; int nearCeiling = 0;
                foreach (var x in d)
                {
                    Assert.IsFalse(float.IsNaN(x) || float.IsInfinity(x), name + ": NaN or infinity");
                    float a = Math.Abs(x); peak = Math.Max(peak, a); sum += x;
                    if (a > 0.985f) nearCeiling++;
                }
                Assert.LessOrEqual(peak, 0.95f, name + ": no headroom (peak " + peak + ")");
                Assert.AreEqual(0, nearCeiling, name + ": samples at the ceiling (clipped)");
                Assert.Greater(Rms(d), 0.02f, name + ": all but silent");
                Assert.Less(Math.Abs(sum / d.Length), 0.01, name + ": a DC offset");
                Assert.Less(Math.Abs(d[0]), 0.02f, name + ": starts with a click");
                Assert.Less(Math.Abs(d[d.Length - 1]), 0.02f, name + ": ends with a click");
            }
        }

        [Test]
        public void AFarSoundIsDarkerThanTheSameSoundNear()
        {
            foreach (var s in new[] { Sfx.Rifle, Sfx.Mg, Sfx.BoomSmall, Sfx.BoomBig, Sfx.TankGun, Sfx.CookOff })
            {
                float near = Brightness(SfxSynth.Make(s, 0, false)), far = Brightness(SfxSynth.Make(s, 0, true));
                Assert.Less(far, near * 0.7f, s + ": the far form is not darker (near " + near + ", far " + far + ")");
            }
        }

        [Test]
        public void EveryVariantIsItsOwn()
        {
            for (int s = 0; s < (int)Sfx.Count; s++)
            {
                var a = SfxSynth.Make((Sfx)s, 0, false); var b = SfxSynth.Make((Sfx)s, 1, false);
                int n = Math.Min(a.Length, b.Length); double diff = 0;
                for (int i = 0; i < n; i++) diff += Math.Abs(a[i] - b[i]);
                Assert.Greater(diff / n, 0.01, (Sfx)s + ": variants 0 and 1 are the same sound");
                CollectionAssert.AreEqual(a, SfxSynth.Make((Sfx)s, 0, false), (Sfx)s + ": not the same sound every load");
            }
        }

        [Test]
        public void TheMixHearsTheFocusLoudestAndABlastFurthest()
        {
            const float h = 40f;
            Assert.AreEqual(1f, SfxMix.Gain(Sfx.Rifle, 0f, h), 1e-4f);
            Assert.Greater(SfxMix.Gain(Sfx.Rifle, 10f, h), SfxMix.Gain(Sfx.Rifle, 30f, h));
            Assert.Greater(SfxMix.Gain(Sfx.BoomBig, 80f, h), SfxMix.Gain(Sfx.Rifle, 80f, h), "a shell carries further than a rifle");
            Assert.AreEqual(0f, SfxMix.Gain(Sfx.Snap, 60f, h), "a round's snap is heard only close by");
            Assert.IsFalse(SfxMix.Far(Sfx.BoomBig, 10f, h)); Assert.IsTrue(SfxMix.Far(Sfx.BoomBig, 200f, h));
            Assert.AreEqual(1f, SfxMix.Delay(Sfx.BoomBig, 343f), 1e-4f);
            Assert.AreEqual(0f, SfxMix.Delay(Sfx.Rifle, 343f), "a volley is not blurred by its travel time");
            Assert.LessOrEqual(Math.Abs(SfxMix.Pan(0f)), 0.85f); Assert.LessOrEqual(Math.Abs(SfxMix.Pan(1f)), 0.85f);
            Assert.Less(SfxMix.Pan(0.2f), 0f); Assert.Greater(SfxMix.Pan(0.8f), 0f);
            Assert.Greater(SfxMix.Hearing(200f), SfxMix.Hearing(30f), "pulled back, the view hears a wider field");
            int total = 0; for (int g = 0; g < (int)SfxGroup.Count; g++) total += SfxMix.Budget((SfxGroup)g);
            Assert.LessOrEqual(total, SfxMix.Voices, "the groups' budgets fit in the pool");
        }

        [Test, Explicit("Writes every sound as a 16-bit WAV to TW_SFX_DIR (default %TEMP%/tw-sfx), to listen to.")]
        public void ExportForListening()
        {
            string dir = Environment.GetEnvironmentVariable("TW_SFX_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-sfx");
            Directory.CreateDirectory(dir);
            int n = 0;
            foreach (var (name, d, rate) in Every()) { Wav(Path.Combine(dir, name + ".wav"), d, rate); n++; }
            TestContext.Out.WriteLine("wrote " + n + " sounds to " + dir);
        }

        static void Wav(string path, float[] d, int rate)
        {
            using var w = new BinaryWriter(File.Create(path));
            int bytes = d.Length * 2;
            w.Write(new[] { (byte)'R', (byte)'I', (byte)'F', (byte)'F' }); w.Write(36 + bytes);
            w.Write(new[] { (byte)'W', (byte)'A', (byte)'V', (byte)'E', (byte)'f', (byte)'m', (byte)'t', (byte)' ' });
            w.Write(16); w.Write((short)1); w.Write((short)1); w.Write(rate); w.Write(rate * 2); w.Write((short)2); w.Write((short)16);
            w.Write(new[] { (byte)'d', (byte)'a', (byte)'t', (byte)'a' }); w.Write(bytes);
            foreach (var x in d) w.Write((short)Math.Round(Math.Max(-1f, Math.Min(1f, x)) * 32767f));
        }
    }
}
