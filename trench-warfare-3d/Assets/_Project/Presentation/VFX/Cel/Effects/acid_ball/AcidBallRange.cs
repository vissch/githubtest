// Phase: CelFx acid ball (2026-11-15) - lab-only Range registration: where the reference clip's ball comes from, and
// the Range's lens vignette handed to the effect.
// The clip's ball comes from NEAR THE EYE, to its left, and flies away into the dummy (as the fireball's comet does):
// it is 147 px across on frame 10 and 89 px on frame 23, and its centre runs from (220, 324) to (767, 400). A
// ballistic path fitted through the Range's camera to its centre and size on frames 10-23 (4.4 px rms), pinned to
// land on the dummy's face: a ball of 0.555 m radius at 15.3 m/s that starts 0.80 s before the hit, 6.92 m to the
// eye's left of the target, 9.61 m nearer the eye and 3.53 m higher, and lands on clip frame 25.8.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    static class AcidBallRange
    {
        const string Id = "acid_ball";
        /// <summary>Filmed with t0 = 0.25 (frame 6); the clip's ball leaves 0.025 s after that (frame 6.6), so that it
        /// lands on frame 25.8 and bursts on frame 26.</summary>
        const float Delay = 0.025f;
        /// <summary>The beach Range's night vignette as it films at the close shot (as NeonAcidRange).</summary>
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
            Vector3 from = shot.Target - right * 6.92f - forward * 9.61f + Vector3.up * 3.53f;
            AcidBallRecipe.Lens(Vignette, VignetteSmooth);
            AcidBallRecipe.Play(from, shot.Target, shot.Ground, right, (uint)shot.Seed, Delay);
        }
    }
}
