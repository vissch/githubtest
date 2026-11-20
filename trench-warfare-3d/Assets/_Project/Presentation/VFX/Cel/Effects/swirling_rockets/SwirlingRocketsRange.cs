// Phase: CelFx swirling rockets (2026-11-15) - lab-only Range registration: the reference clip's rockets are laid out
// round the target in the eye's own frame (SwirlingRocketsPaths was read off the clip through this very camera), and
// the Range's lens vignette is handed to the effect.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    static class SwirlingRocketsRange
    {
        const string Id = "swirling_rockets";
        /// <summary>Filmed with t0 = 0.5 (frame 12): the clip's first change is the near ground on frame 12 and the first
        /// rocket is in frame on 16; the recipe keys every path to the clip's frames, so the delay is zero.</summary>
        const float Delay = 0f;
        /// <summary>The beach Range's night vignette as it films at the close shot (as NeonAcidRange / FireballRange).</summary>
        const float Vignette = 0.372f, VignetteSmooth = 0.5f;

        [CelRangeEffect(Id)]
        static void Fire(RangeShot shot)
        {
            CelFx.ZoomOverride = shot.Lod == RangeLod.Close ? CelFx.CloseZoom : shot.Zoom;
            var camera = ViewEye.Main;
            Vector3 forward = camera != null ? camera.transform.forward : shot.Target - shot.Eye;
            forward.y = 0f;
            if (forward.sqrMagnitude < 0.0001f) forward = Vector3.forward;
            forward.Normalize();
            Vector3 right = Vector3.Cross(Vector3.up, forward);
            SwirlingRocketsRecipe.Lens(Vignette, VignetteSmooth);
            SwirlingRocketsRecipe.Play(shot.Target, shot.Ground, right, (uint)shot.Seed, Delay);
        }
    }
}
