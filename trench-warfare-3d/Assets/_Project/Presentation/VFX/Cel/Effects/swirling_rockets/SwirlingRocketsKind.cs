// Phase: CelFx swirling rockets (2026-11-15) - one GPU pool for the rockets' trails and bodies, their sparks, the hit's
// star, the fireball, its debris and embers, and the light on the ground.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class SwirlingRocketsKind : CelKind
    {
        public const string KindName = "SwirlingRockets";
        /// <summary>Extra.x of a record: which of the shader's shapes it is (CelSwirlingRockets_URP.shader's header).</summary>
        public const float Trail = 0f, Drop = 1f, Splat = 2f, Cloud = 3f, Body = 4f, Glow = 7f;

        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/SwirlingRockets");
        /// <summary>Sixteen pieces a strip: a stretch of trail is two frames of a looping path, and the loops are tight.</summary>
        public override Mesh BuildMesh() => CelMeshes.Strip(16, "Cel SwirlingRockets");
        /// <summary>One close play spawns ~1300 records (two frames of path = a trail and a body record, five spark dashes
        /// each, and the burst): room for two at once.</summary>
        public override int Capacity => 3072;
        public override int Order => 19;
    }
}
