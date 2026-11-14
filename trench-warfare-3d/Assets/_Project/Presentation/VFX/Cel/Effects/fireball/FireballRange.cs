// Phase: CelFx fireball (2026-11-14) - lab-only Range registration: where the reference clip's comet comes from
// (a 3D fit of its path to the beach camera) and the Range's lens vignette handed to the effect.
// a01 is the WIRING film: the geometry below is still neon_acid's fit (an arc from above the left shoulder). The
// clip's fireball is a near-horizontal comet from the LEFT, head in frame at f14 at (230,440) and at the target by
// f26 - spec/eye/fireball.md. The fit is turn 2's step 4; nothing here has been measured against a fireball film.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    static class FireballRange
    {
        const string Id = "fireball";
        /// <summary>Filmed with t0 = 0.5 (frame 12). MEASURED on the clip (tools/t0_expected.json evidence.fireball):
        /// the first thing that happens is the near ground going orange at f11-f12 while the comet is still
        /// off-screen, so the effect starts ON t0 and the delay is zero; the head is in frame at f14 and the hit is
        /// at f28, 16 frames (0.667 s) after t0.</summary>
        const float Delay = 0f;
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
            FireballRecipe.Lens(Vignette, VignetteSmooth);
            FireballRecipe.Play(nozzle, shot.Target, shot.Ground, right, (uint)shot.Seed, Delay);
        }
    }
}
