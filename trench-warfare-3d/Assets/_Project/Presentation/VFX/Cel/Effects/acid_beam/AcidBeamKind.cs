// Phase: CelFx acid beam (2026-11-15) - one GPU pool for the beam, its lines, blobs, sparks, blades and sheets of goo,
// the mass of goo over the target, the puddle, the smoke and the light.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class AcidBeamKind : CelKind
    {
        public const string KindName = "AcidBeam";
        /// <summary>Extra.x of a record: which of the shader's shapes it is (CelAcidBeam_URP.shader's header).</summary>
        public const float Strand = 0f, Drop = 1f, Splat = 2f, Mound = 3f, Puddle = 4f, Bubble = 5f, Puff = 6f, Glow = 7f;

        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/AcidBeam");
        public override Mesh BuildMesh() => CelMeshes.Strip(48, "Cel Acid Beam");
        public override int Capacity => 4096;   // a close beam queues about 1900 records up front; two must fit
        public override int Order => 18;
    }
}
