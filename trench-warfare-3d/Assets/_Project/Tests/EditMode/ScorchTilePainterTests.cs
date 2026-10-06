// Phase: B2 (perf, AOSA C35 2026-09-25) - the crater tile repaint through ScorchTilePainter (marks bucketed per tile,
// a row at a time into a Color32 buffer, one SetPixels32) writes exactly the texels the old per-texel SetPixel loop
// wrote. The old loop is kept below as the oracle, with the ground colour and the clock passed in. A second test pins
// the Color to byte rule against SetPixel itself on every byte's rounding edge.
using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using Unity.Mathematics;
using UnityEngine;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class ScorchTilePainterTests
    {
        const int Tpm = 8;   // GreyboxTerrainView's texels a metre; a tile is 2 m

        /// <summary>A ground colour with a spread in it: runs a little past 0 and 1, lands on byte half-points, and
        /// has alpha below 1 in places.</summary>
        static Color Ground(float wx, float wz)
        {
            float r = wx * .23f - .1f;
            float g = Mathf.PerlinNoise(wx * 1.7f + 3f, wz * 1.3f);
            int k = ((int)(wx * 16f) * 7 + (int)(wz * 16f) * 13) & 255;
            float b = (k + .5f) / 255f;
            float a = wz < 3f ? .4f * wx / 5f : 1f - .1f * wz / 6f;
            return new Color(r, g, b, a);
        }

        /// <summary>The old GreyboxTerrainView.RepaintTile, as it was at 19a88a0, with GroundColor and Time.time
        /// passed in. Do not tidy it: it is the reference.</summary>
        static void OldRepaintTile(Texture2D colorTex, Vector2Int tile, List<TW.Sim.SimEvent> scorchMarks, List<float> scorchBorn, float time)
        {
            const float SnowFillSeconds = GreyboxTerrainView.SnowFillSeconds, ScorchHoldSeconds = GreyboxTerrainView.ScorchHoldSeconds;
            int x1 = Mathf.Min(colorTex.width, (tile.x + 1) * 2 * Tpm), z1 = Mathf.Min(colorTex.height, (tile.y + 1) * 2 * Tpm);
            for (int z = tile.y * 2 * Tpm; z < z1; z++) for (int x = tile.x * 2 * Tpm; x < x1; x++)
            {
                float wx = (x + .5f) / Tpm, wz = (z + .5f) / Tpm;
                Color c = Ground(wx, wz);
                float burn = 0f;
                for (int m = 0; m < scorchMarks.Count; m++)
                {
                    var mark = scorchMarks[m];
                    float radius = mark.Scalar * 1.25f;
                    if (radius <= 0f || Mathf.Abs(wx - mark.Pos.x) > radius || Mathf.Abs(wz - mark.Pos.z) > radius) continue;
                    float distance = Vector2.Distance(new Vector2(wx, wz), new Vector2(mark.Pos.x, mark.Pos.z));
                    float age = m < scorchBorn.Count ? time - scorchBorn[m] : SnowFillSeconds;
                    float fresh = 1f - Mathf.Clamp01((age - ScorchHoldSeconds) / Mathf.Max(1f, SnowFillSeconds - ScorchHoldSeconds));
                    burn = Mathf.Max(burn, ScorchTilePainter.Burn(distance / radius) * fresh * fresh);
                }
                colorTex.SetPixel(x, z, Color.Lerp(c, new Color(.10f, .09f, .08f), burn));
            }
        }

        static TW.Sim.SimEvent Mark(float x, float z, float scalar) =>
            new TW.Sim.SimEvent { Type = TW.Sim.SimEventType.CraterStamp, Pos = new float3(x, 0f, z), Scalar = scalar };

        /// <summary>No texel of Ground has alpha 0, so a texel still holding this was never painted.</summary>
        static readonly Color32 Start = new Color32(0, 255, 0, 0);

        static bool Same(Color32 a, Color32 b) => a.r == b.r && a.g == b.g && a.b == b.b && a.a == b.a;

        /// <summary>The colour texture as GreyboxTerrainView builds it, small, every texel the same start value.</summary>
        static Texture2D NewColorTexture(int w, int h)
        {
            var tex = new Texture2D(w, h, TextureFormat.RGBA32, true) { filterMode = FilterMode.Bilinear, wrapMode = TextureWrapMode.Clamp, anisoLevel = 4 };
            var fill = new Color32[w * h];
            for (int i = 0; i < fill.Length; i++) fill[i] = Start;
            tex.SetPixels32(fill);
            return tex;
        }

        [Test]
        public void Tiles_WithOverlappingScorch_AreTheOldLoopsTexels()
        {
            // 40 x 48 texels = 5 m x 6 m: the third column of tiles is cut to 8 texels, and tile x 3 lies wholly outside
            var marks = new List<TW.Sim.SimEvent>
            {
                Mark(2.0f, 2.5f, 1.6f),           // radius 2, over most of the texture
                Mark(3.1f, 3.0f, 1.0f),           // overlaps the first: the burn is the larger of the two
                Mark(1.03125f, 4.46875f, 2.4f),
                Mark(4.6f, 1.2f, .8f),            // radius 1, reaches into the cut column
                Mark(2.5625f, 1.0625f, .8f),      // on a texel centre: the texels at exactly one radius
                Mark(0f, 0f, 0f),                 // no radius: never burns
                Mark(3f, 3f, -2f),                // a negative radius: never burns
                Mark(-.9f, 2f, 1f),               // outside the texture, reaching in
                Mark(40f, 40f, 3f),               // far away: dropped by the bucket, and burned nothing before either
            };
            const float now = 200f;
            // one fewer than the marks: the last takes the old loop's "no birth time" branch
            var born = new List<float> { 199.5f, 170f, 120f, 199f, 100f, 150f, 190f, 60f };

            var oracle = NewColorTexture(40, 48);
            var fresh = NewColorTexture(40, 48);
            var painter = new ScorchTilePainter();
            try
            {
                for (int tz = 0; tz <= 3; tz++)
                for (int tx = 0; tx <= 3; tx++)
                {
                    var tile = new Vector2Int(tx, tz);
                    OldRepaintTile(oracle, tile, marks, born, now);
                    int x1 = Mathf.Min(fresh.width, (tile.x + 1) * 2 * Tpm), z1 = Mathf.Min(fresh.height, (tile.y + 1) * 2 * Tpm);
                    bool started = painter.Begin(tile.x * 2 * Tpm, tile.y * 2 * Tpm, x1, z1, Tpm, marks, born, now);
                    Assert.AreEqual(tx < 3 && tz < 3, started, $"tile {tile}: an empty tile starts nothing");
                    if (!started) { Assert.IsFalse(painter.Busy); continue; }
                    int rows = 0;
                    while (!painter.PaintRow(Ground)) { rows++; Assert.IsTrue(painter.Busy); }
                    Assert.AreEqual(z1 - tile.y * 2 * Tpm - 1, rows, $"tile {tile}: one row per call");
                    painter.Write(fresh);
                    Assert.IsFalse(painter.Busy);
                }
                AssertSameTexels(oracle, fresh, "tile repaint");
                // the scene has to have burned something, or the comparison proves little
                var px = fresh.GetPixels32(); int changed = 0, burnt = 0;
                var plain = new Color32[px.Length];
                for (int z = 0; z < 48; z++) for (int x = 0; x < 40; x++) plain[z * 40 + x] = ScorchTilePainter.Texel(Ground((x + .5f) / Tpm, (z + .5f) / Tpm));
                for (int i = 0; i < px.Length; i++) { if (!Same(px[i], Start)) changed++; if (!Same(px[i], plain[i])) burnt++; }
                Assert.AreEqual(40 * 48, changed, "every texel was repainted");
                Assert.Greater(burnt, 200, "the marks burned a good share of the texels");
            }
            finally
            {
                UnityEngine.Object.DestroyImmediate(oracle);
                UnityEngine.Object.DestroyImmediate(fresh);
            }
        }

        [Test]
        public void Texel_RoundsAsSetPixelDoes_OnEveryBytesEdge()
        {
            // Row d (0..48) holds, for every byte k, each channel's k + 0.5 half-point (worked out four ways) moved by
            // d - 24 float ulps; the last rows hold values out of range and the edges near 0 and 1.
            const int w = 256, ulps = 24, h = 2 * ulps + 1 + 3;
            var values = new Color[w * h];
            for (int d = -ulps; d <= ulps; d++)
            for (int k = 0; k < w; k++)
                values[(d + ulps) * w + k] = new Color(Nudge((k + .5f) / 255f, d), Nudge((2 * k + 1) / 510f, d), Nudge(k / 255f + .5f / 255f, d), Nudge((k + .5f) * (1f / 255f), d));
            for (int k = 0; k < w; k++)
            {
                values[(h - 3) * w + k] = new Color(-(k + 1) * .01f, 1f + (k + 1) * .01f, k % 2 == 0 ? 1e6f : -1e6f, k / 255f);
                values[(h - 2) * w + k] = new Color(Nudge(.5f / 255f, k - 128), Nudge(254.5f / 255f, k - 128), Nudge(1f, -k), k == 0 ? -0f : Nudge(0f, k));
                values[(h - 1) * w + k] = new Color(Nudge(1.5f / 255f, k - 128), Nudge(127.5f / 255f, k - 128), Nudge(128.5f / 255f, k - 128), Nudge(.5f, k - 128));
            }
            var oracle = new Texture2D(w, h, TextureFormat.RGBA32, true);
            var fresh = new Texture2D(w, h, TextureFormat.RGBA32, true);
            try
            {
                var texels = new Color32[w * h];
                for (int y = 0; y < h; y++)
                for (int x = 0; x < w; x++)
                {
                    oracle.SetPixel(x, y, values[y * w + x]);
                    texels[y * w + x] = ScorchTilePainter.Texel(values[y * w + x]);
                }
                fresh.SetPixels32(0, 0, w, h, texels);
                var want = oracle.GetPixels32(); var got = fresh.GetPixels32();
                var report = new StringBuilder(); int bad = 0;
                for (int i = 0; i < want.Length; i++)
                    for (int ch = 0; ch < 4; ch++)
                    {
                        if (want[i][ch] == got[i][ch]) continue;
                        if (++bad > 12) continue;
                        float f = values[i][ch];
                        report.AppendLine($"texel {i % w},{i / w} ch {ch}: {f:R} (bits {BitConverter.SingleToInt32Bits(f):X8}, x255 = {f * 255f:R}) SetPixel {want[i][ch]}, Unorm8 {got[i][ch]}, Mathf.Round {(byte)Mathf.Round(Mathf.Clamp01(f) * 255f)}");
                    }
                Assert.AreEqual(0, bad, "Unorm8 is not SetPixel's rounding on RGBA32 (if SetPixel matches the Mathf.Round column, it takes a half to even):\n" + report);
            }
            finally
            {
                UnityEngine.Object.DestroyImmediate(oracle);
                UnityEngine.Object.DestroyImmediate(fresh);
            }
        }

        /// <summary>f moved by d float ulps (f and the result both finite and non-negative).</summary>
        static float Nudge(float f, int d) => BitConverter.Int32BitsToSingle(Math.Max(0, BitConverter.SingleToInt32Bits(f) + d));

        static void AssertSameTexels(Texture2D want, Texture2D got, string what)
        {
            Assert.AreEqual(want.width, got.width); Assert.AreEqual(want.height, got.height);
            var a = want.GetPixels32(); var b = got.GetPixels32();
            var report = new StringBuilder(); int bad = 0;
            for (int i = 0; i < a.Length; i++)
            {
                if (Same(a[i], b[i])) continue;
                if (++bad <= 12) report.AppendLine($"texel {i % want.width},{i / want.width}: old {a[i]}, new {b[i]}");
            }
            Assert.AreEqual(0, bad, $"{what}: {bad} texels differ from the old SetPixel loop\n{report}");
        }
    }
}
