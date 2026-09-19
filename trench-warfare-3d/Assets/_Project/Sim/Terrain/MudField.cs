// Phase: A4 (stub) — depends on: MapData, NavLayer.Mud, VehicleKinematics (A1) bog check
using Unity.Mathematics;

namespace TW.Sim.Terrain
{
    public struct MudField
    {
        public float3 Min, Max;
        public float BogFactor; // per-tick bog probability scale for vehicles

        public void Place(MapData map) => throw new System.NotImplementedException("Phase A4: MudField.Place");
    }
}
