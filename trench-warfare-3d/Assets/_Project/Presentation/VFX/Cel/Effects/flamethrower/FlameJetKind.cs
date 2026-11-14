// Phase: CelFx job, flamethrower (2026-11-09, lane/show/cel-flamethrower) - the FlameJet primitive kind: one
// camera-facing ribbon a jet, nozzle to target (Shaders/Cel/Effects/flamethrower/CelFlameJet_URP.shader documents the
// record). A strip of 160 segments follows the widening jet and curved tongue contours.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class FlameJetKind : CelKind
    {
        public override string Name => "FlameJet";
        public override Shader FindShader() => Shader.Find("TW/Cel/FlameJet");
        public override Mesh BuildMesh() => CelMeshes.Strip(160, "Cel FlameJet");
        public override int Capacity => 192;   // Shared by the stream, onset, impact curls and travelling parcels.
        public override int Order => 5;

        public override void PrepareRecord(ref CelRecord record, Vector3 origin, Vector3 target, float now)
        {
            // Target support curls share the smoke field's emitter phase/height,
            // while retaining their own physical motion and random rotation axis.
            if (record.Extra.w < -2.15f && record.Extra.w > -2.25f)
            {
                record.Tone.z = now + Mathf.Max(0f, record.Tone.z - 1f);
                record.Extra.z = target.y;
                return;
            }
            // Ordinary fireballs integrate motion. The travelling front (-3),
            // main jet and ribbons share their emitter's colour and depth frame.
            if (record.Extra.w < -1.5f && record.Extra.w >= -2.5f) return;
            record.Axis = target;
            record.Motion = new Vector4(origin.x, origin.y, origin.z, record.Motion.w);
            record.Tone.z = now;
        }
    }
}
