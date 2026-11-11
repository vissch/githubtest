// Phase: CelFx orbital laser (2026-11-09) - one GPU pool for the shaft, ribbons, rings, scorch and rocks.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class OrbitalLaserKind : CelKind
    {
        public const string KindName = "OrbitalLaser";
        public const float Beam = 0f, Spiral = 1f, Shock = 2f, Scorch = 3f,
            Impact = 4f, Pool = 5f, SmokeRibbon = 6f, Rock = 7f, Puff = 8f;

        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/OrbitalLaser");
        public override Mesh BuildMesh() => CelMeshes.Strip(64, "Cel Orbital Laser");
        public override int Capacity => 256;
        public override int Order => 18;
    }
}
