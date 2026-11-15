// Phase: CelFx red_lasers (2026-11-15, lane/show/cel-red-lasers) - the RedLasers kind: one GPU pool for the laser bolts
// in flight, the hit's stars and blooms, the smoke puffs and the light thrown on the dummy and the ground
// (Shaders/Cel/Effects/red_lasers/CelRedLasers_URP.shader documents the record). One indirect draw, two passes.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class RedLasersKind : CelKind
    {
        public const string KindName = "RedLasers";
        /// <summary>Extra.x of a dot record (size.w &lt; 0): which of the shader's soft shapes it is.</summary>
        public const float Bloom = 1f, Pool = 2f, Puff = 3f, Rays = 4f, Wash = 5f, Light = 7f;

        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/RedLasers");
        public override Mesh BuildMesh() => CelMeshes.Strip(48, "Cel Red Lasers");
        public override int Capacity => 1536;
        public override int Order => 23;
    }
}
