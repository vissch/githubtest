// Phase: CelFx neon acid (2026-11-14) - one GPU pool for the jet's strands, drops, splat, mound, puddle, bubbles, smoke and light.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class NeonAcidKind : CelKind
    {
        public const string KindName = "NeonAcid";
        /// <summary>Extra.x of a record: which of the shader's shapes it is (CelNeonAcid_URP.shader's header).</summary>
        public const float Strand = 0f, Drop = 1f, Splat = 2f, Mound = 3f, Puddle = 4f, Bubble = 5f, Puff = 6f, Glow = 7f;

        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/NeonAcid");
        public override Mesh BuildMesh() => CelMeshes.Strip(40, "Cel Neon Acid");
        public override int Capacity => 768;
        public override int Order => 19;
    }
}
