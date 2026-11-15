// Phase: CelFx fireball (2026-11-14) - one GPU pool for the comet, its embers and sparks, the hit's star, the cloud and the light.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class FireballKind : CelKind
    {
        public const string KindName = "Fireball";
        /// <summary>Extra.x of a record: which of the shader's shapes it is (CelFireball_URP.shader's header).</summary>
        public const float Comet = 0f, Drop = 1f, Splat = 2f, Cloud = 3f, Glow = 7f;

        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/Fireball");
        public override Mesh BuildMesh() => CelMeshes.Strip(40, "Cel Fireball");
        public override int Capacity => 768;
        public override int Order => 18;
    }
}
