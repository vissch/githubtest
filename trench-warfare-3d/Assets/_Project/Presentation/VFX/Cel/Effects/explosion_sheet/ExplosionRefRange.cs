// Phase: CelFx reference sheets (2026-11-19) - explosion_sheet on the Range (effect id "explosion_sheet", lab only). The close shot
// replays the reference clip's own effect (ExplosionRefSheetKind). Mid and Far have nothing yet: a sheet is footage of one
// camera, and the procedural Mid/Far look is not built (celfx tools/refsheet README, scope).
// Procedural layers the sheet leaves out (the lit ground under the effect, sparks) are added in Fire below by the
// refine loop, never inside the sheet.
using UnityEngine;
using TW.Presentation.VFX.Cel;

namespace TW.Presentation.VFX
{
    static class ExplosionRefRange
    {
        static readonly ExplosionRefSheetKind Kind = new ExplosionRefSheetKind();

        [CelRangeEffect("explosion_sheet")]
        static void Fire(RangeShot s)
        {
            RefSheet.PlayAtClose(s, Kind);
        }
    }
}
