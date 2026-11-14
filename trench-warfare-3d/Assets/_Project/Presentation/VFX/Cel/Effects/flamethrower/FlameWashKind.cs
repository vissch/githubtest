// Phase: CelFx flamethrower reference pass (2026-11-14) - four coincident quads per receiver-gain record, used by
// the sustained ground wash and target afterglow. Its transparent pass skips stencil bit 4 written by visible
// FlameJet and CelLobe body/ink fragments. Shaders/Cel/Effects/flamethrower/CelFlameWash_URP.shader documents the record.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class FlameWashKind : CelKind
    {
        public override string Name => "FlameWash";
        public override Shader FindShader() => Shader.Find("TW/Cel/FlameWash");
        public override Mesh BuildMesh()
        {
            // The shader applies one fourth-root receiver gain per coincident
            // quad. Four bounded blends reproduce gains up to 16 on UNORM as
            // well as HDR targets, preserving the dark ground texture.
            // This remains one indirect draw, with four rasterized quads.
            var mesh = CelMeshes.Quad01("Cel FlameWash x4");
            var quad = mesh.triangles;
            var triangles = new int[quad.Length * 4];
            for (int repeat = 0; repeat < 4; repeat++)
                System.Array.Copy(quad, 0, triangles, repeat * quad.Length, quad.Length);
            mesh.SetTriangles(triangles, 0);
            return mesh;
        }
        public override int Capacity => 16;
        public override int Order => 8;
    }
}
