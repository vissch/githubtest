// Phase: A4 (implemented) — depends on: MapData, NavLayer.Mud, VehicleKinematics (A1) bog check
// A rectangle of churned ground: half speed for men, bog checks for vehicles. Trench, link, bunker and blocked
// cells keep their layer.
using Unity.Mathematics;

namespace TW.Sim.Terrain
{
    public struct MudField
    {
        public float3 Min, Max;
        public float BogFactor; // per-tick bog probability scale for vehicles

        public void Place(MapData map)
        {
            var a = map.NavCellOf(Min); var b = map.NavCellOf(Max);
            for (int z = a.y; z <= b.y; z++)
            for (int x = a.x; x <= b.x; x++)
            {
                var layer = (NavLayer)map.NavLayers[map.NavIndex(x, z)];
                if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Bunker | NavLayer.Blocked)) != 0) continue;
                map.SetLayer(x, z, layer | NavLayer.Mud);
            }
        }
    }
}
