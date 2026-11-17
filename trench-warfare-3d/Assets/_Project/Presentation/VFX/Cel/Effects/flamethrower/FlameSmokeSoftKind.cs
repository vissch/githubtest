// Phase: CelFx flamethrower soft extinction (2026-11-17) - effect-local transparent erosion fringe pool.
using UnityEngine;
namespace TW.Presentation.VFX.Cel
{
    public sealed class FlameSmokeSoftKind : CelKind
    {
        public override string Name => "FlameSmokeSoft";
        public override Shader FindShader() => Shader.Find("TW/Cel/FlameSmokeSoft");
        public override Mesh BuildMesh() => CelMeshes.Icosphere(3, "Cel FlameSmokeSoft");
        public override int Capacity => 768;
        public override int Order => 30;
    }
}
