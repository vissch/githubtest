// Phase: CelFx orbital laser (2026-11-09) - lab-only Range registration and placement from the reference camera.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    static class OrbitalLaserRange
    {
        [CelRangeEffect("orbital_laser")]
        static void Fire(RangeShot shot)
        {
            CelFx.ZoomOverride = shot.Lod == RangeLod.Close ? CelFx.CloseZoom : shot.Zoom;
            var camera = ViewEye.Main;
            Vector3 forward = camera != null ? camera.transform.forward : shot.Target - shot.Eye;
            forward.y = 0f;
            if (forward.sqrMagnitude < 0.0001f) forward = Vector3.forward;
            forward.Normalize();
            Vector3 right = Vector3.Cross(Vector3.up, forward);
            // Beach reference: arc centre (650, 440), relative to the dummy's foot (828, 479).
            Vector3 centre = shot.Ground - right * 2.55f + forward * 1.8f;
            OrbitalLaserRecipe.Play(centre, right, (uint)shot.Seed);
        }
    }
}
