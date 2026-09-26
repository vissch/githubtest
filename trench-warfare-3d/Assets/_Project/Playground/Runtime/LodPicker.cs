// Phase: Playground (2026-09-26, lane/show/playground) — LOD choice by screen share with hysteresis
// Which LOD to draw, from how tall the thing stands on screen (a share of the screen height), with a margin either
// side of each cut so a thing sitting on a boundary does not flicker between two LODs.
using UnityEngine;

namespace TW.Playground
{
    public sealed class LodPicker
    {
        /// <summary>Screen-height shares at which each LOD gives way to the next, largest first (length = LODs - 1).</summary>
        public float[] Cuts;
        public float Margin = 0.1f;
        public int Current;

        public LodPicker(params float[] cuts) { Cuts = cuts; }

        /// <summary>Share of the screen height a sphere of this radius covers at this distance, for a perspective camera.</summary>
        public static float ScreenShare(Camera cam, Vector3 centre, float radius)
        {
            if (cam == null) return 1f;
            float d = Mathf.Max(0.01f, Vector3.Distance(cam.transform.position, centre));
            float h = 2f * d * Mathf.Tan(cam.fieldOfView * 0.5f * Mathf.Deg2Rad);
            return 2f * radius / h;
        }

        /// <summary>Walk one level at a time from the current LOD: coarser while the share is clearly below the cut ahead,
        /// finer while it is clearly above the cut behind. (Comparing only against the target's cut left a jump of two
        /// levels stuck at the first: a vehicle 240 m out stayed at LOD0.)</summary>
        public int Pick(float share)
        {
            int lod = Mathf.Clamp(Current, 0, Cuts.Length);
            while (lod < Cuts.Length && share < Cuts[lod] * (1f - Margin)) lod++;
            while (lod > 0 && share > Cuts[lod - 1] * (1f + Margin)) lod--;
            return Current = lod;
        }
    }
}
