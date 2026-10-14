// Phase: tooling (2026-09-23) — the painted horizon is block compressed, and this proves the compression is safe for
// what its alpha actually carries before anyone trusts it on screen.
//
// The horizon skirt (GreyboxTerrainView.BuildSkirt) was a 846x1404 RGBA32 texture with mips, 12.4 MB, the third
// largest texture in the game (docs/05), for a surface that is mostly drawn beyond the fog. It is now DXT5.
//
// Why not DXT1, which is half the size again. Its alpha is not spare. `Coast` drives it down to 0.45 on wet sand,
// and TW/Toon reads `half gloss = max(_Gloss, 1.0 - base.a)` with `base.a < 0.45` meaning standing water, so alpha
// here is a continuous wetness that crosses a hard threshold. DXT1 carries one bit of alpha and would replace the
// whole beach with a single water/not-water edge.
//
// Why a test rather than an assumption. Block compression declines silently. docs/05 already records one afternoon
// lost to exactly that: a 3072-wide sheet the compressor refused, which came back as 25 MB of uncompressed RGB24 and
// looked identical on screen — only the measurement told them apart. DXT works in 4x4 blocks and will not take a
// texture whose sides are not a multiple of four, which 846 is not. So these tests assert that the compression
// happens, that the dimension rule is what forces it, and that the water threshold survives the round trip.
//
// Every test here runs the production code (review TS15, 2026-10-04): the size comes from
// GreyboxTerrainView.SkirtTexels and the compression from GreyboxTerrainView.CompressHorizon, both extracted out of
// BuildSkirt for exactly this reason. Before that these tests did arithmetic on literals and called Unity's own
// Compress, so dropping Round4 or Compress in BuildSkirt left them green.
using System.Text.RegularExpressions;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class PaintedHorizonCompressionTests
    {
        /// <summary>The alpha below which TW/Toon draws standing water rather than wet ground.</summary>
        const float WaterThreshold = 0.45f;

        static Texture2D Rgba(int w, int h, System.Func<int, int, Color32> paint)
        {
            // mipChain: true because CompressHorizon starts by building the mips, as BuildSkirt's horizon does
            var t = new Texture2D(w, h, TextureFormat.RGBA32, true) { hideFlags = HideFlags.HideAndDontSave };
            var px = new Color32[w * h];
            for (int y = 0; y < h; y++)
                for (int x = 0; x < w; x++)
                    px[y * w + x] = paint(x, y);
            t.SetPixels32(px);
            t.Apply(false, false);
            return t;
        }

        // [TS15]
        [Test]
        public void CompressingAnAlphaTextureGivesDxt5AndNotDxt1()
        {
            var t = Rgba(64, 64, (x, y) => new Color32(120, 110, 90, (byte)(x * 4)));
            try
            {
                GreyboxTerrainView.CompressHorizon(t);
                Assert.AreEqual(TextureFormat.DXT5, t.format,
                    "[TS15] the horizon's alpha carries wetness and the water threshold, so it must land on DXT5. DXT1 has " +
                    "one bit of alpha and would turn the beach into a hard water/not-water edge.");
            }
            finally { Object.DestroyImmediate(t); }
        }

        // [TS15]
        [Test]
        public void TheRoundedGridCompressesAndTheHorizonsRealWidthIsWhyItHasTo()
        {
            // The real sizing call, not arithmetic retyped here: a 282 m span at three texels a metre is 846 texels,
            // which is not a block multiple, so the production rounding must take it to 848.
            Assert.AreEqual(3f, GreyboxTerrainView.SkirtTexelsPerMetre, "[TS15] the horizon's grid is three texels a metre");
            GreyboxTerrainView.SkirtTexels(282f, 282f, out int tw, out int th);
            Assert.AreEqual(848, tw, "[TS15] SkirtTexels must round the horizon's real 846 up to 848; without that " +
                                     "rounding the compression below is declined and the horizon stays RGBA32");
            Assert.AreEqual(848, th, "[TS15] the same rounding applies along Z");
            Assert.AreEqual(0, tw % 4, "[TS15] a DXT block is 4x4, so the size must be a multiple of four");
            Assert.AreNotEqual(0, 846 % 4, "846 is not a block multiple, which is the whole reason the rounding exists");

            // What this actually guarantees: a grid sized the way BuildSkirt sizes it does compress. Whether Unity
            // refuses a non-multiple-of-four outright or pads it is Unity's business and is only reported here — the
            // point is that the rounded size is not left to luck, because a refusal is silent and docs/05 already
            // records an afternoon lost to one.
            var rounded = Rgba(tw, 8, (x, y) => new Color32(150, 140, 120, (byte)(x * 2)));
            try
            {
                GreyboxTerrainView.CompressHorizon(rounded);
                Assert.AreEqual(TextureFormat.DXT5, rounded.format,
                    "[TS15] a grid rounded to a multiple of four did not compress, so the horizon is silently still RGBA32");
            }
            finally { Object.DestroyImmediate(rounded); }

            // And the refusal is real, not folklore. Unity logs an error and leaves the texture uncompressed, which
            // is exactly the silent-in-practice failure Round4 exists to prevent: nothing throws, the code reads as
            // though it worked, and the texture stays at full size. The log is expected, so it is declared.
            LogAssert.Expect(LogType.Error, new Regex("not multiples of 4"));
            var odd = Rgba(6, 8, (x, y) => new Color32(150, 140, 120, (byte)(x * 2)));
            try
            {
                GreyboxTerrainView.CompressHorizon(odd, dropCpuCopy: false);
                Assert.AreEqual(TextureFormat.RGBA32, odd.format,
                    "[TS15] Unity now compresses a texture whose width is not a multiple of four. If that is really true the " +
                    "Round4 call in GreyboxTerrainView.BuildSkirt is no longer load-bearing, but check before removing " +
                    "it: when this refusal happens it costs you the whole saving and says so only in the log.");
            }
            finally { Object.DestroyImmediate(odd); }
        }

        /// <summary>
        /// The claim that matters on screen: after DXT5, wet sand is still wet sand and dry sand is still dry. A beach
        /// runs from dry (alpha 1) to standing water (alpha 0.4) across the strip, exactly as Coast paints it, and
        /// every texel must land on the same side of the threshold as it started — except within one 4x4 block of the
        /// crossing, where a block boundary can legitimately straddle it.
        /// </summary>
        // [TS15]
        [Test]
        public void TheWaterThresholdSurvivesDxt5()
        {
            const int W = 64, H = 16;
            // alpha sweeps 1.0 down to 0.30 across the width, so the 0.45 threshold is crossed once, in the open
            float Alpha(int x) => Mathf.Lerp(1.0f, 0.30f, x / (float)(W - 1));
            var t = Rgba(W, H, (x, y) => new Color(0.42f, 0.39f, 0.32f, Alpha(x)));
            try
            {
                int crossing = 0;
                while (crossing < W && Alpha(crossing) >= WaterThreshold) crossing++;

                // dropCpuCopy false: production drops the CPU copy at the end, which would make the texels this
                // test must read back unreadable. Everything before that is the production path.
                GreyboxTerrainView.CompressHorizon(t, dropCpuCopy: false);
                Assert.AreEqual(TextureFormat.DXT5, t.format, "[TS15] precondition: the strip really did compress");
                var got = t.GetPixels32();

                int flipped = 0, worst = 0;
                float biggest = 0f;
                for (int x = 0; x < W; x++)
                {
                    // one block either side of the crossing is allowed to go either way
                    if (Mathf.Abs(x - crossing) <= 4) continue;
                    for (int y = 0; y < H; y++)
                    {
                        float want = Alpha(x), had = got[y * W + x].a / 255f;
                        biggest = Mathf.Max(biggest, Mathf.Abs(want - had));
                        if ((want < WaterThreshold) != (had < WaterThreshold)) { flipped++; worst = x; }
                    }
                }
                TestContext.WriteLine($"alpha 1.00 -> 0.30 over {W} texels, threshold {WaterThreshold} crossed at " +
                                      $"texel {crossing}; after DXT5 the largest alpha error is {biggest:0.###} and " +
                                      $"{flipped} texel(s) changed side away from the crossing");

                Assert.AreEqual(0, flipped,
                    $"[TS15] DXT5 moved {flipped} texel(s) across the water threshold, the worst at x={worst}, more than a " +
                    "block away from where the beach actually crosses it. That would put patches of standing water " +
                    "up the dry sand of the horizon. Do not compress this texture.");
                Assert.That(biggest, Is.LessThan(0.06f),
                    $"[TS15] DXT5 shifted alpha by up to {biggest:0.###}, which is wetness drifting visibly across the " +
                    "beach rather than block noise. TW/Toon turns alpha straight into gloss.");
            }
            finally { Object.DestroyImmediate(t); }
        }
    }
}
