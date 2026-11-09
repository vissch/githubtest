// Phase: CelFx job, flamethrower (2026-11-09, lane/show/cel-flamethrower) - the flamethrower on the Range: fired from
// the stage's low muzzle (off-screen lower left, where the clip's jet comes in) onto the dummy's chest.
namespace TW.Presentation.VFX.Cel
{
    static class FlamethrowerRange
    {
        [CelRangeEffect("flamethrower")]
        static void Fire(RangeShot s) => FlamethrowerRecipe.Play(s.Muzzle, s.Target, (uint)s.Seed);
    }
}
