// Phase: B5 (stub) — depends on: EventPump (P0), SimEvent types
// Maps SimEvent → pooled VFX: Shot tracers, Hit impacts by material, Explosion/CraterStamp bursts and dust,
// GasCloudSpawned volumetric fog volumes (GasVolumeRenderer), SmokeSpawned screens, AbilityFired bomber flyover.
using UnityEngine;

namespace TW.Presentation.VFX
{
    public sealed class EventVfxRouter : MonoBehaviour
    {
        void Start() => throw new System.NotImplementedException("Phase B5: EventVfxRouter");
    }

    public sealed class GasVolumeRenderer : MonoBehaviour
    {
        void Start() => throw new System.NotImplementedException("Phase B5: GasVolumeRenderer (half-res raymarch over GasSmokeSystem grids)");
    }
}
