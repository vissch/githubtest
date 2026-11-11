// Phase: CelFx artillery - shared GPU pools for shell silhouettes, smoke ribbons and blast light.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class ArtilleryFxKind : CelKind
    {
        public const string KindName = "ArtilleryFx";
        public const float Pool = 0f, Flash = 1f, Arc = 2f, Star = 3f, GroundFan = 4f;
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/ArtilleryFx");
        public override Mesh BuildMesh() => CelMeshes.Strip(16, "Cel Artillery light");
        public override int Capacity => 128;
        public override int Order => 15;
    }

    public sealed class ArtilleryShapeKind : CelKind
    {
        public const string KindName = "ArtilleryShape";
        public const float Shell = 0f, Burst = 1f, Trail = 2f;
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/ArtilleryShape");
        public override Mesh BuildMesh() => CelMeshes.Strip(24, "Cel Artillery shape");
        public override int Capacity => 128;
        public override int Order => 22;
    }
}
