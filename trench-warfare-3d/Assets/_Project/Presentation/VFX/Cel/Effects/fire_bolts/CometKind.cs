// Phase: CelFx job (2026-11-09, lane/show/cel-fire-bolts) - the Comet kind: a fire bolt's head, banded flame tail,
// ink edge and wavy smoke ribbon in one quad, or a curling smoke wisp (the ribbon alone). Shader TW/Cel/Comet
// (Shaders/Cel/Effects/fire_bolts/CelComet_URP.shader, its header documents Size.w and Extra). One draw for every
// bolt and wisp on screen.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class CometKind : CelKind
    {
        public const string KindName = "Comet";
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/Comet");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel Comet");
        public override int Capacity => 256;
        public override int Order => 25;
    }
}
