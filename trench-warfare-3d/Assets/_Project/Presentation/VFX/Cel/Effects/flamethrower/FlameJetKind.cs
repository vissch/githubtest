// Phase: CelFx job, flamethrower (2026-11-09, lane/show/cel-flamethrower) - the FlameJet primitive kind: one
// camera-facing ribbon a jet, nozzle to target (Shaders/Cel/Effects/flamethrower/CelFlameJet_URP.shader documents the
// record). A strip of 48 segments so the width can follow the jet's widening and the tongues stay round.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class FlameJetKind : CelKind
    {
        public override string Name => "FlameJet";
        public override Shader FindShader() => Shader.Find("TW/Cel/FlameJet");
        public override Mesh BuildMesh() => CelMeshes.Strip(160, "Cel FlameJet");
        public override int Capacity => 192;   // d02: at the Close tier the jet + 96 tongues + 14 splash overfill 64
        public override int Order => 5;

        public override void Configure(Material material)
        {
            material.SetTexture("_ImpactSheet", Resources.Load<Texture2D>("VFX/FlamethrowerImpact"));
        }

        public override void PrepareRecord(ref CelRecord record, Vector3 origin, Vector3 target, float now)
        {
            // Ordinary fireballs integrate motion. The travelling front (-3),
            // main jet and ribbons share their emitter's colour and depth frame.
            if (record.Extra.w < -1.5f && record.Extra.w >= -2.5f) return;
            record.Axis = target;
            record.Motion = new Vector4(origin.x, origin.y, origin.z, record.Motion.w);
            record.Tone.z = now;
        }
    }
}
