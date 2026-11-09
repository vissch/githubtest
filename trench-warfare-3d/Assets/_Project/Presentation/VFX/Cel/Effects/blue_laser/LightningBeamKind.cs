// Phase: CelFx job, blue_laser (2026-11-09, lane/show/cel-blue-laser) - the LightningBeam kind: a ribbon along a
// straight beam with a pulse window, a zig-zag core, a ragged outline and branch arcs (Shaders/Cel/Effects/blue_laser/
// CelLightningBeam_URP.shader documents the record). One indirect draw for every beam pulse alive.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class LightningBeamKind : CelKind
    {
        public const string KindName = "LightningBeam";
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/LightningBeam");
        public override Mesh BuildMesh() => CelMeshes.Strip(48, "Cel LightningBeam");
        public override int Capacity => 256;
        public override int Order => 21;
    }
}
