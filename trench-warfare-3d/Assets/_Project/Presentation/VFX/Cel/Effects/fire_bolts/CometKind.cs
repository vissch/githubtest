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
        // d01: a 32-segment strip (the bolt and pool modes read only uv, so they draw the same; the path-ribbon
        // smoke mode bends every segment along its curling path)
        public override Mesh BuildMesh() => CelMeshes.Strip(32, "Cel Comet");
        public override int Capacity => 256;
        public override int Order => 25;
    }
}
