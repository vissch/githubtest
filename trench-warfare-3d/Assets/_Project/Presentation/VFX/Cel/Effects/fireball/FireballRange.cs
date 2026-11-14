// Phase: CelFx fireball (2026-11-14, a02) - lab-only Range registration: where the reference clip's comet comes
// from, and the Range's lens vignette handed to the effect.
// The clip's comet flies in from the EYE'S LEFT, nearly level, not down an arc from a shoulder: its head is in
// frame at screen (230,440) on frame 16 and on the dummy on frame 26, and before that (frames 11-12) only the near
// ground is lit by it while it is still off-screen. So it starts 13.6 m to the eye's left of the target, a little
// above its chest. a02 is the BLOCK-IN fit: measured off the clip (spec/eye/fireball.md) but never yet checked
// against a fireball film.
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
            var camera = ViewEye.Main;
            Vector3 forward = camera != null ? camera.transform.forward : shot.Target - shot.Eye;
            forward.y = 0f;
            if (forward.sqrMagnitude < 0.0001f) forward = Vector3.forward;
            Vector3 right = Vector3.Cross(Vector3.up, forward.normalized);
            // 13.6 m to the eye's left of the target and 0.4 m over its chest: the clip's comet crosses the frame
            // from off-screen left in sixteen frames, so it is off-screen for the first two of them.
            Vector3 from = shot.Target - right * 13.6f + Vector3.up * 0.4f;
            FireballRecipe.Lens(Vignette, VignetteSmooth);
            FireballRecipe.Play(from, shot.Target, shot.Ground, right, (uint)shot.Seed, Delay);
        }
    }
}
