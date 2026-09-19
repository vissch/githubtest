// Phase: B4 (stub) — depends on: Death events, VATRenderer poses
// 15 pooled PhysX ragdolls aligned from the VAT frame at death; sleep (|v| < 0.05 m/s for 0.5 s) or 2 s lifetime →
// bake final pose into the static instanced corpse batch and return the ragdoll. Ellipsoid clip wounds with embedded gore geometry.
using UnityEngine;

namespace TW.Presentation.Units
{
    public sealed class RagdollPool : MonoBehaviour
    {
        public const int Capacity = 15;
        public const float SleepSpeed = 0.05f, SleepSeconds = 0.5f, MaxLifetime = 2f;
        void Start() => throw new System.NotImplementedException("Phase B4: RagdollPool");
    }

    public sealed class CorpseBaker : MonoBehaviour
    {
        public const int MaxCorpses = 1000;
        void Start() => throw new System.NotImplementedException("Phase B4: CorpseBaker");
    }
}
