// Phase: CelFx flamethrower late-ember radius contract (2026-11-17).
using UnityEngine;
namespace TW.Presentation.VFX.Cel
{
    // Isolated late-ember geometry cap; all record fields retain Glow semantics.
    public sealed class FlameEmberKind : CelKind
    {
        public const float CapFraction = 0.422f;
        public override string Name => "FlameEmber";
        public override Shader FindShader() => Shader.Find("TW/Cel/FlameEmber");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel FlameEmber");
        public override int Capacity => 128; // 36 per close emitter; multi-emitter cost/capacity unqualified.
        public override int Order => 10; // Match Glow; inherited PrepareRecord is intentionally no-op.
    }
}
