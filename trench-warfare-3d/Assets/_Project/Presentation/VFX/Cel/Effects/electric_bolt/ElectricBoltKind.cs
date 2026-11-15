// Phase: CelFx electric bolt (2026-11-15, lane/show/cel-electric-bolt) - the ElectricBolt kind: one GPU pool for the
// bolts in flight, the lightning arcs, the hit's blooms and stars and the light thrown on the ground
// (Shaders/Cel/Effects/electric_bolt/CelElectricBolt_URP.shader documents the record). One indirect draw, two passes.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class ElectricBoltKind : CelKind
    {
        public const string KindName = "ElectricBolt";
        /// <summary>Extra.x of a dot record (size.w &lt; 0): which of the shader's soft shapes it is.</summary>
        public const float Bloom = 1f, Pool = 2f, Burst = 3f, Rays = 4f, Wash = 5f, Light = 7f;
        /// <summary>Motion.w of a beam record (size.w &gt;= 0): a comet in flight or a lightning arc.</summary>
        public const float Comet = 0f, Arc = 1f;

        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/ElectricBolt");
        public override Mesh BuildMesh() => CelMeshes.Strip(48, "Cel Electric Bolt");
        public override int Capacity => 1536;
        public override int Order => 22;
    }
}
