// Phase: CelFx reference sheets (2026-11-19) - explosion_sheet: the effect cut out of its approved reference clip
// (see spec/refsheet/explosion_sheet.json) and replayed at the Range's close shot. Sheets and layout are written by celfx/tools/refsheet
// (spec/refsheet/explosion_sheet.json); the shared kind and shader are RefSheetKind / CelRefSheet_URP.
namespace TW.Presentation.VFX.Cel
{
    public sealed class ExplosionRefSheetKind : RefSheetKind
    {
        public const string KindName = "ExplosionRefSheet";
        public override string Name => KindName;
        public override string Prefix => "ExplosionRef";
    }
}
