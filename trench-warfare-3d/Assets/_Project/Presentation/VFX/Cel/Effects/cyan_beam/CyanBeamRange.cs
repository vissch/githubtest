// Phase: CelFx cyan-beam lab effect, calibrated to the unchanged beach Range camera.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    static class CyanBeamRange
    {
        // Capture with StageOpts t0=0.375: the green charge precedes the main shaft by six frames.
        [CelRangeEffect("cyan_beam")]
        static void Fire(RangeShot shot)
        {
            // The beach lens reports Mid even when the close film requests Close.
            CelFx.ZoomOverride = shot.Lod == RangeLod.Close ? 7.5f : shot.Zoom;
            Vector3 from = shot.Muzzle;
            from.y = shot.Ground.y + 1.75f;
            Vector3 to = shot.Target + Vector3.up * 0.65f;
            CyanBeamRecipe.Play(from, to, shot.Ground, (uint)shot.Seed);
        }
    }
}
