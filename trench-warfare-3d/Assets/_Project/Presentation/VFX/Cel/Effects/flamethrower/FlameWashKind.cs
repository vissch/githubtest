// Phase: CelFx flamethrower reference pass (2026-11-13) - one quad per projected warm tint/emission record, used by
// the sustained ground wash and target afterglow. Its transparent pass skips stencil bit 4 written by visible
// FlameJet and CelLobe body/ink fragments. Shaders/Cel/Effects/flamethrower/CelFlameWash_URP.shader documents the record.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class FlameWashKind : CelKind
    {
        public override string Name => "FlameWash";
        public override Shader FindShader() => Shader.Find("TW/Cel/FlameWash");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel FlameWash");
        public override int Capacity => 16;
        public override int Order => 8;
    }
}
