// Phase: A4 (implemented core) — depends on: MapData (P0), NavLayer (P0), SimEvents.CraterStamp
// Carves a cosine bowl into the heightfield and marks open-ground nav cells as Crater (cost 2, speed 0.8, cover
// for whoever lies in it). Trench, link, wire and blocked cells keep their layer: a shell does not fill a trench
// in (CollapsesTrench abilities are A5). The cover comes from the Crater bit, read by DirectFire and Blast.
using Unity.Mathematics;

namespace TW.Sim.Terrain
{
    public struct CraterStamp
    {
        public float3 Center;
        public float Radius;   // metres (HE 3 m, bomber 6 m, field gun 1.5 m)
        public float Depth;    // metres

        /// <summary>Apply to the map. Returns the number of nav cells whose layer changed.</summary>
        public int Apply(MapData map)
        {
            var hf = map.Height;
            float cell = hf.CellSize;
            int minX = (int)math.floor((Center.x - Radius) / cell), maxX = (int)math.ceil((Center.x + Radius) / cell);
            int minZ = (int)math.floor((Center.z - Radius) / cell), maxZ = (int)math.ceil((Center.z + Radius) / cell);
            for (int z = minZ; z <= maxZ; z++)
            for (int x = minX; x <= maxX; x++)
            {
                if (!hf.InBounds(x, z)) continue;
                float dx = (x + 0.5f) * cell - Center.x, dz = (z + 0.5f) * cell - Center.z;
                float d = SimMath.Sqrt(dx * dx + dz * dz);
                if (d >= Radius) continue;
                var nav = map.NavCellOf(new float3((x + 0.5f) * cell, 0f, (z + 0.5f) * cell));
                byte layer = map.NavLayers[map.NavIndex(nav.x, nav.y)];
                if ((layer & (byte)(NavLayer.Trench | NavLayer.Link | NavLayer.Blocked)) != 0) continue;   // trenches keep their floor
                float bowl = 0.5f * (1f + SimMath.Cos(SimMath.Pi * d / Radius));
                hf.Set(x, z, hf.HeightAtCell(x, z) - Depth * bowl);
            }

            int changed = 0;
            float n = MapData.NavCellSize;
            int nMinX = math.max(0, (int)math.floor((Center.x - Radius) / n)), nMaxX = math.min(map.NavWidth - 1, (int)math.floor((Center.x + Radius) / n));
            int nMinZ = math.max(0, (int)math.floor((Center.z - Radius) / n)), nMaxZ = math.min(map.NavLength - 1, (int)math.floor((Center.z + Radius) / n));
            for (int z = nMinZ; z <= nMaxZ; z++)
            for (int x = nMinX; x <= nMaxX; x++)
            {
                float dx = (x + 0.5f) * n - Center.x, dz = (z + 0.5f) * n - Center.z;
                if (dx * dx + dz * dz > Radius * Radius) continue;
                int i = map.NavIndex(x, z);
                var layer = (NavLayer)map.NavLayers[i];
                if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked | NavLayer.Wire | NavLayer.Crater)) != 0) continue;
                map.SetLayer(x, z, layer | NavLayer.Crater);
                changed++;
            }
            changed += map.ApplyWater(nMinX, nMinZ, nMaxX, nMaxZ, allowBlock: false);   // a hole below the water table fills: slow going, never impassable
            map.Touch();
            return changed;
        }
    }
}
