// Phase: CelFx orbital laser (2026-11-14) - strip pool for the shaft and rings; ground and debris use their own meshes.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class OrbitalLaserKind : CelKind
    {
        public const string KindName = "OrbitalLaser";
        public const float Beam = 0f, Spiral = 1f, Shock = 2f, Scorch = 3f,
            Impact = 4f, Pool = 5f, SmokeRibbon = 6f, Rock = 7f, Puff = 8f,
            Ejecta = 9f, EndFlash = 10f, OrbitSpark = 11f, Chart = 12f;

        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/OrbitalLaser");
        public override Mesh BuildMesh() => CelMeshes.Strip(64, "Cel Orbital Laser");
        public override int Capacity => 512; // v21: + ~170 orbit sparks, queued up front (low quality halves it)
        public override int Order => 18;
        public override void BeforeSubmit(float now) => OrbitalLaserLighting.Prune(now);
        public override void ResetState() => OrbitalLaserLighting.ResetSession();
    }
}
