// Phase: VFX pass (owner, 2026-09-28: "improve fire from far away ... it doesn't look as good as the flipbook, we need a
// combination, and optimized") — part of FlipbookFx. Up close a fire is its drawing: the flipbook's cels, licks and
// curls. From far off that drawing is a few busy pixels (bench r9, zoom 240: the drawn pillar a thin flicker beside the
// old procedural column's clean glow) while every lick still costs a card. So past FarFireFrom every Fire book is drawn
// as a combination:
//  - a HALO: the fire cards are gathered on a coarse ground grid (FarGlowCell) and each cell gets one additive Flash card,
//    sized and brightened by how much fire is in it: the procedural read, one card a fire however many licks it has;
//  - THINNING: a stable share of the small cards (narrower than FarThinWidth) is left out, and the ones kept are drawn
//    bigger to cover what the others did: the drawing's shape stays, its card count and overdraw fall.
// Both blend in from FarFireFrom to FarFireFull. Behind fx.recipes (0: nothing here runs, the draw is as it was). No
// random draws: a card's keep is a hash of its birth and life.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public sealed partial class FlipbookFx
    {
        public const float FarFireFrom = 80f, FarFireFull = 200f;   // zoom: the blend's start and end
        public const float FarThinWidth = 3f;                       // m: cards this wide or wider are never thinned (a cook-off, the beam)
        public const float FarKeepMin = 0.4f;                       // the share of small fire cards kept at FarFireFull
        public const float FarGlowCell = 8f;                        // m: the halo's grid
        const float GlowOpacity = 0.75f;                            // bench r11 at zoom 240: 0.55 was barely there
        const int MaxGlowCells = 96;

        readonly bool farFire = CombatFx.ReadRecipes() >= 0.5f;
        int farFrame = -1; float farBlend;
        readonly Vector4[] glowSum = new Vector4[MaxGlowCells];     // x*w, y*w, z*w, w (y: the fire's middle, not its foot)
        readonly float[] glowTall = new float[MaxGlowCells];        // the tallest fire standing in the cell (m)
        readonly long[] glowKey = new long[MaxGlowCells];
        int glowCount;
        readonly List<Matrix4x4> glowCards = new List<Matrix4x4>(MaxGlowCells);

        /// <summary>How far into the far look a zoom is: 0 up to FarFireFrom, 1 from FarFireFull.</summary>
        public static float FarFireBlend(float zoom) => Mathf.Clamp01((zoom - FarFireFrom) / (FarFireFull - FarFireFrom));

        /// <summary>The share of small fire cards kept at a blend.</summary>
        public static float FarKeepShare(float blend) => Mathf.Lerp(1f, FarKeepMin, Mathf.Clamp01(blend));

        /// <summary>Whether a small fire card is kept at a share: by a hash of its birth and life, so a card is kept or
        /// left out for its whole life (no flicker) and the same cards every run.</summary>
        public static bool FarKeep(float born, float life, float share)
        {
            if (share >= 1f) return true;
            uint h = (uint)System.BitConverter.SingleToInt32Bits(born) * 2654435761u ^ (uint)System.BitConverter.SingleToInt32Bits(life) * 40503u;
            h ^= h >> 15; h *= 0x2c1b3c6du; h ^= h >> 12;
            return (h & 0xFFFF) / 65536f < share;
        }

        /// <summary>This frame's blend (read once a frame from the camera's zoom), 0 when fx.recipes is off.</summary>
        float FarBlendNow()
        {
            if (!farFire) return 0f;
            if (farFrame == Time.frameCount) return farBlend;
            farFrame = Time.frameCount;
            var cam = Camera.main;
            farBlend = cam != null && cam.TryGetComponent<IZoomSource>(out var zs) ? FarFireBlend(zs.CurrentZoom) : 0f;
            return farBlend;
        }

        /// <summary>Count a fire card into its halo cell. A card standing on its foot (anchored) counts at its middle, and
        /// its height is kept, so a tall fire (the beam's pillar, a burning tree) gets a tall glow and not a blob at its foot.</summary>
        void Glow(Vector3 at, float width, float height, bool anchored, float alpha)
        {
            float w = Mathf.Max(0f, width) * Mathf.Clamp01(Mathf.Abs(alpha));
            if (w <= 0f) return;
            float tall = anchored ? Mathf.Max(0f, height) : 0f;
            at.y += tall * 0.45f;
            long key = ((long)Mathf.FloorToInt(at.x / FarGlowCell) << 32) ^ (uint)Mathf.FloorToInt(at.z / FarGlowCell);
            for (int i = 0; i < glowCount; i++)
                if (glowKey[i] == key) { glowSum[i] += new Vector4(at.x * w, at.y * w, at.z * w, w); glowTall[i] = Mathf.Max(glowTall[i], tall); return; }
            if (glowCount == MaxGlowCells) return;
            glowKey[glowCount] = key; glowTall[glowCount] = tall; glowSum[glowCount++] = new Vector4(at.x * w, at.y * w, at.z * w, w);
        }

        /// <summary>Draw the halos counted since the last flush: one additive Flash card a cell, over the fire's weighted
        /// middle, growing with the square root of how much fire is in it.</summary>
        void FlushGlow(Bounds bounds)
        {
            if (glowCount == 0) return;
            glowCards.Clear();
            float bright = SceneMood.Night ? 2.2f : 1.4f;
            for (int i = 0; i < glowCount; i++)
            {
                var s = glowSum[i];
                float size = Mathf.Clamp(3f + 2.2f * Mathf.Sqrt(s.w), 3f, 24f);
                // as tall as the tallest fire in it (the Flash drawing is a soft round glow: stretched, a soft column), never
                // narrower than round
                float tall = Mathf.Clamp(glowTall[i] * 1.1f, size, 90f);
                Vector3 at = new Vector3(s.x / s.w, s.y / s.w, s.z / s.w);
                glowCards.Add(Pack(at, size, tall, 0f, 1f, bright, 0f, Kind.None, farBlend * Mathf.Clamp01(s.w / 3f) * GlowOpacity));
            }
            glowCount = 0;
            DrawPacked(Book.Flash, glowCards, bounds);
        }
    }
}
