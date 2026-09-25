// Phase: B2 (perf, AOSA C35 2026-09-25) - repaints one tile of the ground's colour texture: the ground colour, then
// the scorch of every crater that reaches it, faded by the crater's age.
// This replaced a loop that tested every scorch mark against every texel and wrote each texel with SetPixel. It
// works the same maths out in the same order, so every texel comes out bit for bit as before (ScorchTilePainterTests
// keeps the old loop as its oracle). Three things differ:
// - Only the marks that can reach the tile are tested. A mark more than its radius plus 1 m away from every texel
//   centre in the tile is dropped once per tile; the per-texel test is unchanged. The burn is a max, so the marks
//   dropped could only ever have added zero.
// - A mark's fade depends only on its age, so it is worked out once per tile rather than once per texel. The time is
//   read when the tile starts; the old loop read Time.time per texel, which is the same number within a frame.
// - The texels go into a buffer, a row at a time, and the finished tile is written with one SetPixels32. The caller
//   can stop between rows and carry on next frame, so a frame's time budget binds at a row, not at a whole tile. A
//   tile half done is never written, so the texture never shows a half-painted tile.
// Color to byte is Unorm8 below, which is the rounding SetPixel applies on an RGBA32 texture.
using System;
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Terrain
{
    public sealed class ScorchTilePainter
    {
        /// <summary>The colour a fresh shell hole burns the ground toward.</summary>
        public static readonly Color Burnt = new Color(.10f, .09f, .08f);

        struct Mark { public float X, Z, Radius, Fresh; }

        readonly List<Mark> near = new List<Mark>();
        readonly Dictionary<int, Color32[]> buffers = new Dictionary<int, Color32[]>();
        Color32[] buffer;
        int x0, z0, w, h, tpm, nextRow;

        /// <summary>A tile has been started and not yet written.</summary>
        public bool Busy { get; private set; }

        /// <summary>
        /// Start the texel block [x0, x1) x [z0, z1) at tpm texels a metre. marks and born are the view's scorch marks
        /// and the Time.time each landed; now is the time the whole tile is painted at. False, and nothing started, for
        /// an empty block.
        /// </summary>
        public bool Begin(int x0, int z0, int x1, int z1, int tpm, List<TW.Sim.SimEvent> marks, List<float> born, float now)
        {
            Busy = false;
            if (x1 <= x0 || z1 <= z0) return false;
            this.x0 = x0; this.z0 = z0; w = x1 - x0; h = z1 - z0; this.tpm = tpm; nextRow = 0;
            if (!buffers.TryGetValue(w * h, out buffer)) { buffer = new Color32[w * h]; buffers.Add(w * h, buffer); }   // SetPixels32 wants the exact size
            // the texel centres this tile covers, in metres, as the per-texel loop computes them
            float wxMin = (x0 + .5f) / tpm, wxMax = (x1 - 1 + .5f) / tpm, wzMin = (z0 + .5f) / tpm, wzMax = (z1 - 1 + .5f) / tpm;
            near.Clear();
            for (int m = 0; m < marks.Count; m++)
            {
                var mark = marks[m];
                float radius = mark.Scalar * 1.25f;
                if (radius <= 0f) continue;   // the per-texel test skips it for every texel
                // a metre of slack: this only drops marks that cannot pass the exact test below for any texel
                if (mark.Pos.x + radius < wxMin - 1f || mark.Pos.x - radius > wxMax + 1f || mark.Pos.z + radius < wzMin - 1f || mark.Pos.z - radius > wzMax + 1f) continue;
                // The hole is black for ScorchHoldSeconds and then fills: the ground lightens back toward what it was,
                // and because the snow is keyed off the burn (Toon_URP) the snow comes back with it.
                float age = m < born.Count ? now - born[m] : GreyboxTerrainView.SnowFillSeconds;
                float fresh = 1f - Mathf.Clamp01((age - GreyboxTerrainView.ScorchHoldSeconds) / Mathf.Max(1f, GreyboxTerrainView.SnowFillSeconds - GreyboxTerrainView.ScorchHoldSeconds));
                near.Add(new Mark { X = mark.Pos.x, Z = mark.Pos.z, Radius = radius, Fresh = fresh });
            }
            Busy = true;
            return true;
        }

        /// <summary>Paint the next row of the tile into the buffer. True when that was the last row: call Write.</summary>
        public bool PaintRow(Func<float, float, Color> ground)
        {
            int z = z0 + nextRow, row = nextRow * w;
            float wz = (z + .5f) / tpm;
            for (int i = 0; i < w; i++)
            {
                int x = x0 + i;
                float wx = (x + .5f) / tpm;
                Color c = ground(wx, wz);
                float burn = 0f;
                for (int k = 0; k < near.Count; k++)
                {
                    var mark = near[k];
                    if (Mathf.Abs(wx - mark.X) > mark.Radius || Mathf.Abs(wz - mark.Z) > mark.Radius) continue;
                    float distance = Vector2.Distance(new Vector2(wx, wz), new Vector2(mark.X, mark.Z));
                    burn = Mathf.Max(burn, .45f * (1f - distance / mark.Radius) * mark.Fresh * mark.Fresh);   // this order, as before
                }
                buffer[row + i] = Texel(Color.Lerp(c, Burnt, burn));
            }
            return ++nextRow >= h;
        }

        /// <summary>Write the finished tile to mip 0 of the texture. The caller still owns Apply.</summary>
        public void Write(Texture2D tex)
        {
            tex.SetPixels32(x0, z0, w, h, buffer);
            Busy = false;
        }

        /// <summary>One texel as SetPixel(Color) stores it on an RGBA32 texture.</summary>
        public static Color32 Texel(Color c) => new Color32(Unorm8(c.r), Unorm8(c.g), Unorm8(c.b), Unorm8(c.a));

        /// <summary>
        /// A 0..1 float to a byte the way Unity's native colour conversion does it: clamp, times 255, add a half and
        /// truncate (a half rounds up). Not the managed Color32 cast, which uses Mathf.Round and takes a half to even:
        /// the two differ only where the product lands exactly on k + 0.5. ScorchTilePainterTests checks this against
        /// SetPixel on every byte's rounding edge.
        /// </summary>
        public static byte Unorm8(float f)
        {
            f = f > 0f ? f : 0f;
            f = f < 1f ? f : 1f;
            // Both casts are load-bearing: Mono keeps float arithmetic at double precision unless told to narrow, and
            // then 0.503921568 x 255 is 128.4999... (truncates to 128) where SetPixel's float product is 128.5 (129).
            float scaled = (float)(f * 255f);
            return (byte)(int)(float)(scaled + .5f);
        }
    }
}
