// Phase: A4 (stub) — depends on: MapData, NavLayer.Wire, SimEvents.WireBreached
// A wire belt is a rectangle of Wire cells with authored gaps. Breach() clears a width of cells and emits WireBreached.
using Unity.Mathematics;

namespace TW.Sim.Terrain
{
    public struct WireBelt
    {
        public float3 Min, Max;

        public void Place(MapData map) => throw new System.NotImplementedException("Phase A4: WireBelt.Place");
        public static void Breach(MapData map, float3 at, float width) => throw new System.NotImplementedException("Phase A4: WireBelt.Breach");
    }
}
