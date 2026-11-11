// Phase: CelFx cyan-beam lab effect, one pooled indirect draw for shafts, impact spikes and ground light.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class CyanBeamKind : CelKind
    {
        public const string KindName = "CyanBeam";
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/CyanBeam");
        public override Mesh BuildMesh() => CelMeshes.Strip(48, "Cel CyanBeam");
        public override int Capacity => 1024;
        public override int Order => 21;
    }
}
