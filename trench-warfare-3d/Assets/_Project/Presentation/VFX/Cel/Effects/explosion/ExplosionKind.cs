// Phase: CelFx job (2026-11-09, lane/show/cel-explosion) - the explosion's own primitive kind "ExpFx"
// (Shaders/Cel/Effects/explosion/CelExpFx_URP.shader): soft ground pools, soft flashes (domes, the fireball's white
// heart, its bloom halo), cream pressure jets and curved white spark arcs, all in one pool and one draw. The explosion
// uses this instead of the kit's Glow; the other effect kinds retain their own pooled draws. Kept for a build by
// Resources/ShaderKeep/KeepCelExpFx.mat.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class ExpFxKind : CelKind
    {
        public const string KindName = "ExpFx";
        /// <summary>extra.x of an ExpFx record.</summary>
        public const float Pool = 0f, Flash = 1f, Arc = 2f, Star = 3f, Tracer = 4f, Onset = 5f, Ember = 6f;
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/ExpFx");
        public override Mesh BuildMesh() => CelMeshes.Strip(16, "Cel ExpFx");
        public override int Capacity => 512;
        public override int Order => 15;
    }



    /// <summary>Far-site cooling light, beneath its gray bank and thrown black fragments.</summary>
    public sealed class ExpFarSoftKind : CelKind
    {
        public const string KindName = "ExpFarSoft";
        public const float Cooling = 1.2f;
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/ExpFx");
        public override Mesh BuildMesh() => CelMeshes.Strip(16, "Cel ExpFarSoft");
        public override int Capacity => 32;
        public override int Order => 8;
        public override void Configure(Material material) { material.renderQueue = 3008; }
    }

    /// <summary>One connected Far soot mass, in front of its gray bank and behind thrown fragments.</summary>
    public sealed class ExpFarSootKind : CelKind
    {
        public const string KindName = "ExpFarSoot";
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/ExpPuff");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel ExpFarSoot");
        public override int Capacity => 32;
        public override int Order => 11;
        public override void Configure(Material material) { material.renderQueue = 3011; }
    }

    /// <summary>Only the negative-Star pressure spray, composited before the opaque-looking fire body.
    /// Same shader and strip mesh as ExpFx; other Fx records retain their existing material/order.</summary>
    public sealed class ExpPressureKind : CelKind
    {
        public const string KindName = "ExpPressure";
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/ExpFx");
        public override Mesh BuildMesh() => CelMeshes.Strip(16, "Cel ExpPressure");
        public override int Capacity => 512;
        public override int Order => 12;
        public override void Configure(Material material) { material.renderQueue = 3012; }
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

namespace TW.Presentation.VFX.Cel
{
    /// <summary>d20: a path-ribbon spark streak (Shaders/Cel/Effects/explosion/CelExpRibbon_URP.shader): a thin strip
    /// following the record's own past path plus a seeded screen-plane bend, tapering head to tail, white core with a
    /// faint warm edge, additive, one draw. A drop-in for a Streak layer. Kept for a build by
    /// Resources/ShaderKeep/KeepCelExpRibbon.mat.</summary>
    public sealed class ExpRibbonKind : CelKind
    {
        public const string KindName = "ExpRibbon";
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/ExpRibbon");
        public override Mesh BuildMesh() => CelMeshes.Strip(32, "Cel ExpRibbon");
        public override int Capacity => 1024;
        public override int Order => 16;
    }

    /// <summary>A churning cel smoke puff (Shaders/Cel/Effects/explosion/CelExpPuff_URP.shader): one record is eight
    /// overlapping billows that spread, curl and lift over life. Premultiplied alpha fades the late bank into haze.
    /// One quad per record, one pooled draw; transparent instances are not sorted. A
    /// drop-in for a kit Lobe layer: every record field keeps its Lobe meaning (size.w becomes the sub-ball spread).
    /// Kept for a build by Resources/ShaderKeep/KeepCelExpPuff.mat.</summary>
    public sealed class ExpPuffKind : CelKind
    {
        public const string KindName = "ExpPuff";
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/ExpPuff");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel ExpPuff");
        public override int Capacity => 1024;
        public override int Order => 11;
    }
}

namespace TW.Presentation.VFX.Cel
{
    /// <summary>Only the three Hold wrap layers, drawn over orange fire and below the hot impact.</summary>
    public sealed class ExpFrontPuffKind : CelKind
    {
        public const string KindName = "ExpFrontPuff";
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/ExpPuff");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel ExpFrontPuff");
        public override int Capacity => 1024;
        public override int Order => 16;
        public override void Configure(Material material) { material.renderQueue = 3016; }
    }
}

namespace TW.Presentation.VFX.Cel
{
    // Phase: Explosion reference revision 2026-11-14. Preserve the main fan strip and add four unequal exterior strands.
    // Concrete CelKind discovery registers this automatically; the existing ExpRibbon keeper retains the shader.
    public sealed class ExpFanKind : CelKind
    {
        public const string KindName = "ExpFan";
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/ExpRibbon");
        public override int Capacity => 128;
        public override int Order => 16;

        public override Mesh BuildMesh()
        {
            var vertices = new System.Collections.Generic.List<Vector3>(202);
            var uv = new System.Collections.Generic.List<Vector2>(202);
            var triangles = new System.Collections.Generic.List<int>(576);
            AddFanStrip(32, 0f, vertices, uv, triangles); // Exact original ExpRibbon sampling and winding.
            for (int strand = 0; strand < 4; strand++)
                AddFanStrip(16, 2f + strand * 2f, vertices, uv, triangles);
            var mesh = new Mesh { name = "Cel ExpFan Strands", hideFlags = HideFlags.HideAndDontSave };
            mesh.SetVertices(vertices);
            mesh.SetUVs(0, uv);
            mesh.SetTriangles(triangles, 0);
            mesh.bounds = new Bounds(Vector3.zero, Vector3.one * 2f);
            return mesh;
        }

        static void AddFanStrip(int segments, float uvOffset, System.Collections.Generic.List<Vector3> vertices,
            System.Collections.Generic.List<Vector2> uv, System.Collections.Generic.List<int> triangles)
        {
            int start = vertices.Count;
            for (int i = 0; i <= segments; i++)
            {
                float x = i / (float)segments;
                vertices.Add(new Vector3(x - 0.5f, -0.5f, 0f)); uv.Add(new Vector2(x, uvOffset));
                vertices.Add(new Vector3(x - 0.5f, 0.5f, 0f)); uv.Add(new Vector2(x, uvOffset + 1f));
            }
            for (int i = 0; i < segments; i++)
            {
                int a = start + i * 2;
                triangles.Add(a); triangles.Add(a + 1); triangles.Add(a + 2);
                triangles.Add(a + 2); triangles.Add(a + 1); triangles.Add(a + 3);
            }
        }
    }
}
