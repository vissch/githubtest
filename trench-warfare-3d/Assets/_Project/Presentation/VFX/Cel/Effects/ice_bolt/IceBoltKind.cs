// Phase: CelFx ice_bolt (2026-11-15, lane/show/cel-ice-bolt) - the IceBolt kind: the darts and mist ribbons along the
// corridor and the bloom, starbursts, chips, shards, frost and specks at the hit (Shaders/Cel/Effects/ice_bolt/
// CelIceBolt_URP.shader documents the record). One indirect draw for every record alive.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class IceBoltKind : CelKind
    {
        public const string KindName = "IceBolt";
        /// <summary>Extra.x of a record: which of the shader's shapes it is.</summary>
        public const float Dart = 0f, Ribbon = 1f, Bloom = 2f, Star = 3f, Fleck = 4f, Shard = 5f, Frost = 6f, Speck = 7f, Puff = 8f;

        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/IceBolt");
        public override Mesh BuildMesh() => CelMeshes.Strip(48, "Cel IceBolt");
        public override int Capacity => 2048;
        public override int Order => 22;
    }
}
