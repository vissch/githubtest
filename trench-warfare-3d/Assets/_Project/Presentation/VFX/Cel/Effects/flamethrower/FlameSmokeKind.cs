// Phase: CelFx flamethrower smoke staging (2026-11-13). Reuses Lobe's mesh, shader and keeper material.
// Extra.w < 0 opts into emitter-time palette staging; -Extra.w is the time scale (normally 1).
// Incoming Tone.z / layer Opacity = 1 + phase delay in seconds (unused by ordinary cutout shading).
// Preparation replaces it with the absolute emitter start plus delay. Direct Spawn requires this preparation.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class FlameSmokeKind : CelKind
    {
        public override string Name => "FlameSmoke";
        public override Shader FindShader() => Shader.Find("TW/Cel/FlameSmokeOpaque");
        public override Mesh BuildMesh() => CelMeshes.Icosphere(3, "Cel FlameSmoke");
        public override int Capacity => 768;
        public override int Order => 0;

        // The actual owning renderer invokes this AFTER storing the prepared original.
        public override void AfterPreparedSpawn(CelFxRenderer owner, in CelRecord prepared)
        {
            if (prepared.Extra.w >= 0f) return;
            int softKind = owner.KindId("FlameSmokeSoft");
            if (softKind >= 0) owner.Spawn(softKind, prepared);
        }

        public override void PrepareRecord(ref CelRecord record, Vector3 origin, Vector3 target, float now)
        {
            record.Tone.z = now + Mathf.Max(0f, record.Tone.z - 1f);
        }
    }
}
