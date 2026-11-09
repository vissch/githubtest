// Phase: CelFx job (2026-11-09, lane/show/cel-explosion) - the explosion's own primitive kind "ExpFx"
// (Shaders/Cel/Effects/explosion/CelExpFx_URP.shader): soft ground pools, soft flashes (domes, the fireball's white
// heart, its bloom halo) and jagged white spark arcs, all in one pool and one draw. The explosion draws every glow with
// it and none with the kit's Glow, so it still costs four draws (Streak, Lobe x2, ExpFx). Kept for a build by
// Resources/ShaderKeep/KeepCelExpFx.mat.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class ExpFxKind : CelKind
    {
        public const string KindName = "ExpFx";
        /// <summary>extra.x of an ExpFx record.</summary>
        public const float Pool = 0f, Flash = 1f, Arc = 2f, Star = 3f;
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/ExpFx");
        public override Mesh BuildMesh() => CelMeshes.Strip(8, "Cel ExpFx");
        public override int Capacity => 512;
        public override int Order => 15;
    }

    /// <summary>d11: dense dark angular debris chunks (Shaders/Cel/Effects/explosion/CelExpChunk_URP.shader): opaque
    /// cut-out polygons, one quad each, one draw. Kept for a build by Resources/ShaderKeep/KeepCelExpChunk.mat.</summary>
    public sealed class ExpChunkKind : CelKind
    {
        public const string KindName = "ExpChunk";
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/ExpChunk");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel ExpChunk");
        public override int Capacity => 1024;
        public override int Order => 12;
    }
}
