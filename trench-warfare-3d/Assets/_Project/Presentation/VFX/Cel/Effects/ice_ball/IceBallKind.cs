// Phase: CelFx ice_ball (2026-11-15) - one GPU pool for the ball and its tail, the chips and shards, the crystal, the
// mist, the swirl on the ground and the light.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class IceBallKind : CelKind
    {
        public const string KindName = "IceBall";
        /// <summary>Extra.x of a record: which of the shader's shapes it is (CelIceBall_URP.shader's header).</summary>
        public const float Comet = 0f, Drop = 1f, Crystal = 2f, Mist = 3f, Swirl = 4f, Chart = 5f, Glow = 7f;
        /// <summary>Extra.y of a Drop: its outline.</summary>
        public const float Tear = 0f, Round = 1f, Leaf = 2f, Shard = 3f;

        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/IceBall");
        public override Mesh BuildMesh() => CelMeshes.Strip(40, "Cel IceBall");
        public override int Capacity => 768;
        public override int Order => 19;
    }
}
