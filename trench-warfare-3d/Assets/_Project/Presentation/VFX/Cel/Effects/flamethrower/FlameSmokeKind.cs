// Phase: CelFx flamethrower smoke staging (2026-11-13). Reuses Lobe's mesh, shader and keeper material.
// Extra.w < 0 opts into emitter-time palette staging; -Extra.w is the time scale (normally 1).
// Incoming Tone.z / layer Opacity = 1 + phase delay in seconds (unused by ordinary cutout shading).
// Preparation stores that absolute phase start and target.y in Extra.z. Negative-w smoke uses fixed -0.8 lighting.
// Direct Spawn requires this preparation; Axis and all motion slots retain their ordinary meanings.
using UnityEngine;
using UnityEngine.Rendering;

namespace TW.Presentation.VFX.Cel
{
    public sealed class FlameSmokeKind : CelKind
    {
        public override string Name => "FlameSmoke";
        public override Shader FindShader() => Shader.Find("TW/Cel/Lobe");
        public override Mesh BuildMesh() => CelMeshes.Icosphere(3, "Cel FlameSmoke");
        public override int Capacity => 768;
        public override int Order => 0;

        public override void Configure(Material material)
        {
            // A material per kind keeps ordinary Lobe opaque. Pass toggles use LightMode tag values.
            material.SetFloat("_SrcBlend", (float)BlendMode.SrcAlpha);
            material.SetFloat("_DstBlend", (float)BlendMode.OneMinusSrcAlpha);
            // Dense lobes occlude one another correctly. Alpha still fades their
            // color late; this transparent pass runs after the global scene ink.
            material.SetFloat("_ZWrite", 1f);
            material.SetOverrideTag("RenderType", "Transparent");
            material.renderQueue = (int)RenderQueue.Transparent;
            material.SetShaderPassEnabled("DepthOnly", false);
            material.SetShaderPassEnabled("SRPDefaultUnlit", false);
        }

        public override void PrepareRecord(ref CelRecord record, Vector3 origin, Vector3 target, float now)
        {
            record.Tone.z = now + Mathf.Max(0f, record.Tone.z - 1f);
            record.Extra.z = target.y;
        }
    }
}
