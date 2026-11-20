// Phase: CelFx flamethrower reference-first rebuild (2026-11-18) - the fire sheet: the jet, the catch on the target,
// the fire at the target and its burn-out loops, drawn from distance fields cut from the reference clip frame by frame
// (Shaders/Cel/Effects/flamethrower/CelFlameSheet_URP.shader documents the record and the sheet). One card a shot, on
// the plane through the nozzle and the aim point, turned about the jet's axis to face the eye.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class FlameSheetKind : CelKind
    {
        /// <summary>Film frames between the shot and the sheet's first cell (the clip's first flame is one frame after the trigger).</summary>
        public const int LeadFrames = 1;
        /// <summary>Cells in the sheet (film frames 8..118 of the reference).</summary>
        public const int Cells = 111;

        public override string Name => "FlameSheet";
        public override Shader FindShader() => Shader.Find("TW/Cel/FlameSheet");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel FlameSheet");
        public override int Capacity => 16;
        public override int Order => 40;

        public override void Configure(Material material)
        {
            material.SetTexture("_SheetA", Resources.Load<Texture2D>("VFX/FlamethrowerSheetA"));
            material.SetTexture("_SheetA2", Resources.Load<Texture2D>("VFX/FlamethrowerSheetA2"));
            material.SetTexture("_SheetB", Resources.Load<Texture2D>("VFX/FlamethrowerSheetB"));
        }

        /// <summary>The card hangs between the emitter's two ends: Pos the nozzle, Axis the aim point, no motion.</summary>
        public override void PrepareRecord(ref CelRecord record, Vector3 origin, Vector3 target, float now)
        {
            record.Pos = origin;
            record.Vel = Vector3.zero;
            record.Axis = target;
            record.Motion = Vector4.zero;
        }

        /// <summary>The cell shown `age` seconds after the shot at `fps` cells a second (-1: none yet, Cells and up: over).
        /// Rounded to the nearest frame so a film stepped at exactly 1/fps never lands on a boundary.</summary>
        public static int CellAt(float age, float fps) => Mathf.FloorToInt(Mathf.Max(age, 0f) * fps + 0.5f) - LeadFrames;
    }
}
