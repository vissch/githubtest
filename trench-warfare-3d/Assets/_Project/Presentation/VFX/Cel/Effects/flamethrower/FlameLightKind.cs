// Phase: CelFx flamethrower reference-first rebuild (2026-11-18) - the fire's light on the scene: one full-screen
// pass a shot that MULTIPLIES what is already drawn (ground, props, the target) by a gain that falls off with the
// level distance to the lit part of the jet's ground line, so the ground's own texture stays and brightens, as in the
// reference clip (Shaders/Cel/Effects/flamethrower/CelFlameLight_URP.shader documents the record and the table).
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class FlameLightKind : CelKind
    {
        public override string Name => "FlameLight";
        public override Shader FindShader() => Shader.Find("TW/Cel/FlameLight");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel FlameLight");
        public override int Capacity => 16;
        public override int Order => -10;

        /// <summary>The light lies along the emitter: Pos the nozzle, Axis the aim point, no motion.</summary>
        public override void PrepareRecord(ref CelRecord record, Vector3 origin, Vector3 target, float now)
        {
            record.Pos = origin;
            record.Vel = Vector3.zero;
            record.Axis = target;
            record.Motion = Vector4.zero;
        }
    }
}
