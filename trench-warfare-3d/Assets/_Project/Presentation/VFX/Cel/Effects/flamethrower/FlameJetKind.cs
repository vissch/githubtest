// Phase: CelFx job, flamethrower (2026-11-09, lane/show/cel-flamethrower) - the FlameJet primitive kind: one
// camera-facing ribbon a jet, nozzle to target (Shaders/Cel/Effects/flamethrower/CelFlameJet_URP.shader documents the
// record). A strip of 48 segments so the width can follow the jet's widening and the tongues stay round.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class FlameJetKind : CelKind
    {
        public override string Name => "FlameJet";
        public override Shader FindShader() => Shader.Find("TW/Cel/FlameJet");
        public override Mesh BuildMesh() => CelMeshes.Strip(48, "Cel FlameJet");
        public override int Capacity => 32;
        public override int Order => 5;
    }
}
