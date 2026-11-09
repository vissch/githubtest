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
            float k = Time.deltaTime * 24f;
            k = (k > 1.05f && k < 1.6f) ? Mathf.Round(k * 100f) / 100f : 1f;
            if (k == 1f) { FlamethrowerRecipe.Play(s.Muzzle, s.Target, (uint)s.Seed); return; }
            var stretched = FlamethrowerRecipe.TimeScaled(FlamethrowerRecipe.Recipe, k, 0.8f * (k - 1f));   // once a shot (a Range lab hook)
            Debug.Log("Range: flamethrower plays " + k + "x stretched (shot clock step " + Time.deltaTime + " s)");
            FlamethrowerRecipe.Play(stretched, s.Muzzle, s.Target, (uint)s.Seed);
        }
    }
}
