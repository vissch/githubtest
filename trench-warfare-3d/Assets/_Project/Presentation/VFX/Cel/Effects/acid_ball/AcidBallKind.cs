// Phase: CelFx acid ball (2026-11-15) - one GPU pool for the ball, its drops, the burst, the crown, the ring, the puddle, bubbles, smoke and light.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class AcidBallKind : CelKind
    {
        public const string KindName = "AcidBall";
        /// <summary>Extra.x of a record: which of the shader's shapes it is (CelAcidBall_URP.shader's header).</summary>
        public const float Comet = 0f, Drop = 1f, Burst = 2f, Crown = 3f, Puddle = 4f, Bubble = 5f, Puff = 6f, Glow = 7f, Ring = 8f, Coat = 9f, Book = 10f;

        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/AcidBall");
        public override Mesh BuildMesh() => CelMeshes.Strip(40, "Cel Acid Ball");
        public override int Capacity => 768;
        public override int Order => 20;
        /// <summary>a78: the impact book (Resources/VFX/AcidImpact, Tools/firebooks.py's AcidImpact) on the pool's material.</summary>
        public override void Configure(Material material)
        {
            var tex = Resources.Load<Texture2D>("VFX/AcidImpact");
            if (tex != null) material.SetTexture("_AcidBook", tex);
            var hi = Resources.Load<Texture2D>("VFX/AcidImpactHi");   // a83: the colour book, 24 a second
            if (hi != null) material.SetTexture("_AcidBookHi", hi);
        }
    }
}
