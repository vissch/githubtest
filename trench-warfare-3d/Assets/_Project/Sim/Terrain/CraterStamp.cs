// Phase: A4 (stub) — depends on: MapData (P0), NavLayer (P0), SimEvents.CraterStamp
// Carves a cosine bowl into the heightfield, marks Crater cells, adds a low omnidirectional CoverVolume,
// and marks the touched nav cells dirty so FlowFieldManager recomputes local cost.
using Unity.Mathematics;

namespace TW.Sim.Terrain
{
    public struct CraterStamp
    {
        public float3 Center;
        public float Radius;   // metres (HE 3 m, bomber 6 m, field gun 1.5 m)
        public float Depth;    // metres

        /// <summary>Apply to the map. Returns the number of nav cells whose cost changed.</summary>
        public int Apply(MapData map)
        {
            throw new System.NotImplementedException("Phase A4: CraterStamp.Apply");
        }
    }
}
