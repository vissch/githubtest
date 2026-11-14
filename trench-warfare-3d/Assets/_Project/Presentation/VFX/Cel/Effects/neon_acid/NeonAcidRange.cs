// Phase: CelFx neon acid (2026-11-14) - lab-only Range registration: the nozzle where the reference clip's jet
// comes from (a 3D fit of its arc to the beach camera) and the Range's lens vignette handed to the effect.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    static class NeonAcidRange
    {
        const string Id = "neon_acid";
        /// <summary>Filmed with t0 = 0.5 (frame 12); the clip's jet leaves the nozzle just before frame 13 (its head
        /// has coated the target on frame 22).</summary>
        const float Delay = 0.039f;
        /// <summary>The beach Range's night vignette as it films at the close shot (Atmosphere: the biome's 0.34 and
        /// the close lens's share on top; 0.372 fitted to a film of flat swatches), and its smoothness.</summary>
        const float Vignette = 0.372f, VignetteSmooth = 0.5f;

        [CelRangeEffect(Id)]
        static void Fire(RangeShot shot)
        {
            CelFx.ZoomOverride = shot.Lod == RangeLod.Close ? CelFx.CloseZoom : shot.Zoom;
            Vector3 along = shot.Direction;
            along.y = 0f;
            along = along.sqrMagnitude > 1e-6f ? along.normalized : Vector3.forward;
            Vector3 side = new Vector3(along.z, 0f, -along.x);
            // The fit, held in the shot's own frame so the mid and far stills keep it: 12.6 m back along the flat
            // fire direction, 2.5 m to its left and 4.8 m above the chest. Off-screen: the jet enters the film's
            // upper-left corner 0.07 s into its flight.
            Vector3 nozzle = shot.Target - along * 12.555f - side * 2.474f + Vector3.up * 4.770f;
            var camera = ViewEye.Main;
            Vector3 forward = camera != null ? camera.transform.forward : shot.Target - shot.Eye;
            forward.y = 0f;
            if (forward.sqrMagnitude < 0.0001f) forward = Vector3.forward;
            Vector3 right = Vector3.Cross(Vector3.up, forward.normalized);
            NeonAcidRecipe.Lens(Vignette, VignetteSmooth);
            NeonAcidRecipe.Play(nozzle, shot.Target, shot.Ground, right, (uint)shot.Seed, Delay);
        }
    }
}
