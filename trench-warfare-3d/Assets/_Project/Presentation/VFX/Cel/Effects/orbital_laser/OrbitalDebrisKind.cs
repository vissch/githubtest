// Phase: CelFx orbital laser reference match (2026-11-14) - quad records for ballistic fragments and rim stones.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class OrbitalDebrisKind : CelKind
    {
        public const string KindName = "OrbitalDebris";

        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/OrbitalLaser");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel Orbital Debris");
        public override int Capacity => 256;
        public override int Order => 19;
        public override void Configure(Material material) => material.renderQueue = 3017;
    }
}
