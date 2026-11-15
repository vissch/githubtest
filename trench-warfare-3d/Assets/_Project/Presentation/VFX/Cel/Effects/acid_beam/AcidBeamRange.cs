// Phase: CelFx acid beam (2026-11-15) - lab-only Range registration: the nozzle the reference clip's beam comes from
// (a 3D fit of its head's track to the beach camera: a straight beam at chest height from the eye's left) and the
// Range's lens vignette handed to the effect.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    static class AcidBeamRange
    {
        const string Id = "acid_beam";
        /// <summary>Filmed with t0 = 0.5 (frame 12); the clip's nozzle opens on frame 16.2 (its head enters the frame
        /// on frame 20 and reaches the target's chest on frame 31.5).</summary>
        const float Delay = 0.176f;
        /// <summary>The beach Range's night vignette as it films at the close shot (the neon acid's fit).</summary>
        const float Vignette = 0.372f, VignetteSmooth = 0.5f;

        [CelRangeEffect(Id)]
        static void Fire(RangeShot shot)
        {
            CelFx.ZoomOverride = shot.Lod == RangeLod.Close ? CelFx.CloseZoom : shot.Zoom;
            Vector3 along = shot.Direction;
            along.y = 0f;
            along = along.sqrMagnitude > 1e-6f ? along.normalized : Vector3.forward;
            Vector3 side = new Vector3(along.z, 0f, -along.x);
            // The fit, held in the shot's own frame so the mid and far stills keep it: 13.6 m back along the flat
            // fire direction, 3.3 m to its left, level with the chest (8 cm under it). Off-screen: the head enters the
            // film's left edge 10.4 m from the target, 0.16 s into its flight.
            Vector3 nozzle = shot.Target - along * 13.597f - side * 3.335f - Vector3.up * 0.084f;
            var camera = ViewEye.Main;
            Vector3 forward = camera != null ? camera.transform.forward : shot.Target - shot.Eye;
            forward.y = 0f;
            if (forward.sqrMagnitude < 0.0001f) forward = Vector3.forward;
            Vector3 right = Vector3.Cross(Vector3.up, forward.normalized);
            AcidBeamRecipe.Lens(Vignette, VignetteSmooth);
            AcidBeamRecipe.Play(nozzle, shot.Target, shot.Ground, right, (uint)shot.Seed, Delay);
        }
    }
}
