// Phase: CelFx ice_ball (2026-11-15, i01) - lab-only Range registration: where the reference clip's ball comes from
// and where it hangs, and the Range's lens vignette handed to the effect.
// The clip's ball comes from NEAR THE EYE, to its left, and flies AWAY toward the dummy, shrinking and climbing the
// frame as it goes: its leading edge is in frame at the left edge on frame 8 and it hangs from frame 17 with its
// middle at screen (715,410), 88 px of radius, 1.66 m to the eye's left of the dummy's chest (834,399). Its head on
// frames 9-17, taken through the Range's camera with a radius of 1.24 m, runs nearly straight at 32.5 m/s. i01 is
// the BLOCK-IN fit: measured off the clip (celfx/runs/ice_ball/_ref/fit_head.py) but not yet checked against a film.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    static class IceBallRange
    {
        const string Id = "ice_ball";
        /// <summary>Filmed with t0 = 0.25 (frame 6): the ball is launched off-screen on frame 6 and its leading
        /// edge crosses the frame's left edge on frame 8 (tools/t0_expected.json evidence.ice_ball).</summary>
        const float Delay = 0f;
        /// <summary>The beach Range's night vignette as it films at the close shot (FireballRange: 0.372 fitted to
        /// a film of flat swatches), and its smoothness.</summary>
        const float Vignette = 0.372f, VignetteSmooth = 0.5f;

        [CelRangeEffect(Id)]
        static void Fire(RangeShot shot)
        {
            CelFx.ZoomOverride = shot.Lod == RangeLod.Close ? CelFx.CloseZoom : shot.Zoom;
            var camera = ViewEye.Main;
            Vector3 forward = camera != null ? camera.transform.forward : shot.Target - shot.Eye;
            forward.y = 0f;
            if (forward.sqrMagnitude < 0.0001f) forward = Vector3.forward;
            forward = forward.normalized;
            Vector3 right = Vector3.Cross(Vector3.up, forward);
            // where it hangs: 1.66 m to the eye's left of the chest, a hair nearer and lower
            Vector3 hold = shot.Target - right * 1.66f - forward * 0.06f - Vector3.up * 0.15f;
            // where it starts: 6.27 m further left, 13.33 m nearer the eye and 2.22 m higher (11 frames of flight)
            Vector3 from = hold - right * 6.27f - forward * 13.33f + Vector3.up * 2.22f;
            IceBallRecipe.Lens(Vignette, VignetteSmooth);
            IceBallRecipe.Play(from, hold, shot.Target, shot.Ground, right, (uint)shot.Seed, Delay);
        }

        /// <summary>Lab only: the colour chart in the middle of the frame (2.285 m to the eye's left of the chest),
        /// 84 px to a side, for a film that measures the Range's night grade (celfx/runs/ice_ball/_grade).</summary>
        [CelRangeEffect("ice_ball_swatch")]
        static void Swatch(RangeShot shot)
        {
            CelFx.ZoomOverride = CelFx.CloseZoom;
            var camera = ViewEye.Main;
            Vector3 forward = camera != null ? camera.transform.forward : shot.Target - shot.Eye;
            forward.y = 0f;
            if (forward.sqrMagnitude < 0.0001f) forward = Vector3.forward;
            Vector3 right = Vector3.Cross(Vector3.up, forward.normalized);
            IceBallRecipe.Lens(Vignette, VignetteSmooth);
            IceBallRecipe.Chart(shot.Target - right * 2.285f + Vector3.up * 0.22f, 1.185f, 4f);
        }

        /// <summary>Lab only: the same card showing the effect's own palette (8 rows down: ball, tail, crystal,
        /// swirl, mist, late mist, chip, shard; c0 c1 c2 c3 pale line across), to check what the film shows of
        /// each written colour against the clip's (celfx/runs/ice_ball/_ref/palcheck.py).</summary>
        [CelRangeEffect("ice_ball_palette")]
        static void Palette(RangeShot shot)
        {
            CelFx.ZoomOverride = CelFx.CloseZoom;
            var camera = ViewEye.Main;
            Vector3 forward = camera != null ? camera.transform.forward : shot.Target - shot.Eye;
            forward.y = 0f;
            if (forward.sqrMagnitude < 0.0001f) forward = Vector3.forward;
            Vector3 right = Vector3.Cross(Vector3.up, forward.normalized);
            IceBallRecipe.Lens(Vignette, VignetteSmooth);
            IceBallRecipe.Chart(shot.Target - right * 2.285f + Vector3.up * 0.22f, 1.185f, 4f, true);
        }
    }
}
