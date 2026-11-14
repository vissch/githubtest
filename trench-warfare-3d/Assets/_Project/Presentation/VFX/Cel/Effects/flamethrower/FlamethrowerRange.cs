// Phase: CelFx job, flamethrower (2026-11-09, lane/show/cel-flamethrower) - the flamethrower on the Range: fired from
// the stage's low muzzle (off-screen lower left, where the clip's jet comes in) onto the dummy's chest.
// The Range's film is meant to step 1/24 s a frame; since 2026-11-09 06:15 its clock steps 0.05 s (1.2x, close.json
// "t"), which squeezes the whole effect into fewer frames than the clip's. Until the stage is fixed the hook plays a
// copy stretched by the measured step (and fired as late as the 0.8 s mark lands in the clip's frames), so the film's
// frames line up with the clip's. At a true 1/24 s step it plays the recipe as it is.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    static class FlamethrowerRange
    {
        [CelRangeEffect("flamethrower")]
        static void Fire(RangeShot s)
        {
            // b02: the clip's nozzle is ON screen at the lower left (plate pixel ~(85,705)); the stage's Muzzle is off-screen
            // 8 m from the eye, which blew the jet's near end up into a slab over the whole lower left. The nozzle is put
            // where that plate pixel's ray meets 0.9 m above the dummy's ground (worked out from the beach pose), held
            // in the shot's own frame (10.8 m back along the flat fire direction, 1.07 m to its left, 0.33 m below the
            // chest), so the mid/far stills in the same battle keep it.
            Vector3 fw = s.Direction; fw.y = 0f; fw = fw.sqrMagnitude > 1e-6f ? fw.normalized : Vector3.forward;
            Vector3 right = new Vector3(fw.z, 0f, -fw.x);
            // CaptureRig restores the camera zoom before RangeStage arms a LOD shot. Bind the declared shot zoom
            // at spawn so Far stills also get Far record counts, even when ViewEye still reports the beach's Mid zoom.
            // Close keeps its authored 7.5 pin; CelFx's session reset clears either pin.
            CelFx.ZoomOverride = s.Lod == RangeLod.Close ? 7.5f : s.Zoom;
            // b03: 0.5 m further left and lower, so the jet comes in from the frame's lower-left corner like the clip's (b02's
            // drawn start sat ~65 px right of and above the clip's nozzle at ~134 px/m)
            Vector3 nozzle = s.Target - fw * 10.8f - right * 2.0f - Vector3.up * 0.8f;
            Debug.Log("Range: flamethrower nozzle " + nozzle.ToString("F2") + " (stage muzzle " + s.Muzzle.ToString("F2")
                + ") quality=" + TW.Presentation.FxQuality.Tier + " spawnZoom=" + CelFx.Zoom.ToString("F1"));
            float k = Time.deltaTime * 24f;
            k = (k > 1.05f && k < 1.6f) ? Mathf.Round(k * 100f) / 100f : 1f;
            if (k == 1f) { FlamethrowerRecipe.Play(nozzle, s.Target, (uint)s.Seed); return; }
            var stretched = FlamethrowerRecipe.TimeScaled(FlamethrowerRecipe.Recipe, k, 0.8f * (k - 1f));   // once a shot (a Range lab hook)
            Debug.Log("Range: flamethrower plays " + k + "x stretched (shot clock step " + Time.deltaTime + " s)");
            FlamethrowerRecipe.Play(stretched, nozzle, s.Target, (uint)s.Seed);
        }
    }
}
