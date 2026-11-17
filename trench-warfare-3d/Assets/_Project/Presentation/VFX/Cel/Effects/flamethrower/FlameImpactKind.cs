// Phase: CelFx flamethrower refinement (2026-11-17) - authored impact after depth ink.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    /// <summary>Authored target flame drawn after scene ink, with scene depth testing.</summary>
    public sealed class FlameImpactKind : CelKind
    {
        public override string Name => "FlameImpact";
        public override Shader FindShader() => Shader.Find("TW/Cel/FlameImpact");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel FlameImpact");
        public override int Capacity => 64;
        public override int Order => 5;
        public override void Configure(Material material)
        {
            material.SetTexture("_ImpactSheet", Resources.Load<Texture2D>("VFX/FlamethrowerImpact"));
            material.SetTexture("_ImpactTailSheet", Resources.Load<Texture2D>("VFX/FlamethrowerImpactTail"));
        }
    }
}
