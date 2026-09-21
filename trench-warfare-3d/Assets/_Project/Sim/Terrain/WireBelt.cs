// Phase: A4 (implemented) — depends on: MapData, NavLayer.Wire, SimEvents.WireBreached
// A wire belt is a rectangle of Wire cells with gaps left by whoever places it. Breach() clears the wire inside a
// radius (a shell landed on it) and reports how many cells opened; DeformationSystem emits WireBreached.
using Unity.Mathematics;

namespace TW.Sim.Terrain
{
    public struct WireBelt
    {
        public float3 Min, Max;

        public void Place(MapData map)
        {
            var a = map.NavCellOf(Min); var b = map.NavCellOf(Max);
            for (int z = a.y; z <= b.y; z++)
            for (int x = a.x; x <= b.x; x++)
            {
                var layer = (NavLayer)map.NavLayers[map.NavIndex(x, z)];
                if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Bunker | NavLayer.Blocked)) != 0) continue;
                map.SetLayer(x, z, layer | NavLayer.Wire);
            }
        }

        public static int Breach(MapData map, float3 at, float radius)
        {
            float n = MapData.NavCellSize;
            int x0 = math.max(0, (int)math.floor((at.x - radius) / n)), x1 = math.min(map.NavWidth - 1, (int)math.floor((at.x + radius) / n));
            int z0 = math.max(0, (int)math.floor((at.z - radius) / n)), z1 = math.min(map.NavLength - 1, (int)math.floor((at.z + radius) / n));
            int opened = 0;
            for (int z = z0; z <= z1; z++)
            for (int x = x0; x <= x1; x++)
            {
                float dx = (x + 0.5f) * n - at.x, dz = (z + 0.5f) * n - at.z;
                if (dx * dx + dz * dz > radius * radius) continue;
                var layer = (NavLayer)map.NavLayers[map.NavIndex(x, z)];
                if ((layer & NavLayer.Wire) == 0) continue;
                map.SetLayer(x, z, layer & ~NavLayer.Wire);
                opened++;
            }
            return opened;
        }
    }
}
