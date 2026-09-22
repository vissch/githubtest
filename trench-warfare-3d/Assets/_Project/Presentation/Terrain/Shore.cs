// Phase: B2 (implemented) — where the land stops. The map's own heightfield holds the beach (BattlefieldGenerator
// .BeachHeight); this is the rest of it: the sea bed beyond the map edge, and the rule that no land the skirt
// invents may stand up out of the water. Everything that draws past the edge asks here — the skirt, the sea mesh
// and the backdrop props — so the three agree on where the coast is to the centimetre.
using UnityEngine;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public static class Shore
    {
        /// <summary>How fast the bed falls away under the shallows, and then out in the open sea.</summary>
        public const float ShallowSlope = 0.085f, DeepSlope = 0.02f, ShallowFor = 60f, Floor = 14f;

        /// <summary>The bed under open water, <paramref name="z"/> metres along the sea axis.</summary>
        public static float Bed(MapData map, float x, float z)
        {
            float off = Mathf.Max(0f, map.Offshore(z));
            float bed = map.SeaLevel - Mathf.Min(Floor, ShallowSlope * off + DeepSlope * Mathf.Max(0f, off - ShallowFor));
            // bars and runnels, so the bed is not a ramp: strongest in the shallows where the water shows them
            float bars = (Mathf.PerlinNoise(x * .021f + 13.3f, z * .035f + 5.7f) - .5f) * Mathf.Min(1.3f, off * .06f);
            return bed + bars;
        }

        /// <summary>The height the land beyond the map may have at a point: its own, until it reaches the water, where
        /// the bed takes over. Without a sea, the land is left alone.</summary>
        public static float Shape(MapData map, float x, float z, float land)
        {
            if (map == null || !map.HasSea || map.Offshore(z) <= 0f) return land;
            return Mathf.Min(land, Bed(map, x, z));
        }

        /// <summary>True where the sea covers the point (map or skirt), a hand's breadth of water counting as sea.</summary>
        public static bool UnderWater(MapData map, float x, float z, float ground) => map != null && map.HasSea && ground < map.SeaLevel - .04f;
    }
}
