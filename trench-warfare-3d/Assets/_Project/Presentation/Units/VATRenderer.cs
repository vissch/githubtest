// Phase: B3 (stub) — depends on: UnitPose stream (P0), VAT atlases from VATBaker (Editor), Shaders/VAT_URP.shader
// One GraphicsBuffer of per-instance data (matrix, animRow, animT, team tint) filled by a job from
// SimPresenter.Poses, drawn with Graphics.RenderMeshIndirect per (archetype, LOD tier). Tiers:
// hero skeletal pool 0-12 m, VAT lerp 12-45 m, VAT nearest 45-100 m, 8-direction impostor > 100 m.
using UnityEngine;

namespace TW.Presentation.Units
{
    public struct VatInstance { public Matrix4x4 Matrix; public float AnimRow, AnimT, Tint, Lod; }

    public sealed class VATRenderer : MonoBehaviour
    {
        public const int HeroPoolSize = 16;
        void Start() => throw new System.NotImplementedException("Phase B3: VATRenderer");
    }

    public static class LodTiers
    {
        public const float Hero = 12f, Horde = 45f, Distant = 100f;
    }
}
