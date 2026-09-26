// Phase: B6 / docs/21 phase 6 (implemented) — which chunks of a Home Front building show at a stage. A building is a
// sliced house (HouseKit): one mesh, a chunk to a bit of the instance's mask, a set bit hiding the chunk. A stage
// shows a fraction of the building's height, and every chunk whose foot stands above that height is masked off, so
// stage 0 is the ground floor and stage 3 the whole building. The ground floor (chunks within GroundedBelow of the
// floor) always shows, whatever the fraction, so a building never vanishes. Pure: a test builds a house by hand.
using UnityEngine;
using TW.Presentation.Terrain;

namespace TW.Presentation.Meta
{
    public static class HomeFrontStages
    {
        /// <summary>A chunk whose foot is this little above the limit still shows (the cuts leave seams).</summary>
        public const float Epsilon = 0.02f;

        /// <summary>The height (in the house's frame) a building showing <paramref name="shown"/> of itself reaches.</summary>
        public static float Limit(HouseKit.House house, float shown)
        {
            float floor = house.Bounds.min.y, top = house.Bounds.max.y;
            return Mathf.Max(floor + HouseKit.GroundedBelow, floor + (top - floor) * Mathf.Clamp01(shown));
        }

        /// <summary>The chunks hidden at that fraction: a set bit hides the chunk (HouseKit.ChunkMask).</summary>
        public static HouseKit.ChunkMask HiddenAt(HouseKit.House house, float shown)
        {
            var mask = default(HouseKit.ChunkMask);
            if (house == null || house.Chunks == null) return mask;
            float limit = Limit(house, shown);
            for (int i = 0; i < house.Chunks.Length; i++)
                if (house.Chunks[i].Local.min.y > limit + Epsilon) mask.Set(i);
            return mask;
        }

        public static int ShownCount(HouseKit.House house, float shown)
        {
            if (house == null || house.Chunks == null) return 0;
            var hidden = HiddenAt(house, shown);
            int n = 0;
            for (int i = 0; i < house.Chunks.Length; i++) if (!hidden.Has(i)) n++;
            return n;
        }
    }
}
