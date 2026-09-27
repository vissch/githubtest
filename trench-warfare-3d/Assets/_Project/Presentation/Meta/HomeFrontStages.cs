// Phase: B6 / docs/21 phase 6 (implemented) — which chunks of a Home Front building show at a stage. A building is a
// sliced house (HouseKit): one mesh, a chunk to a bit of the instance's mask, a set bit hiding the chunk. A stage
// reveals the model's own levels (the distinct heights its chunks stand on), not a share of its height: the real
// models have few levels, and a height share put several stages between the same two, so stage III (the dearest)
// changed nothing on 8 of 10 models (critic r5, 2026-09-27). Stage 0 is the ground floor, the last stage the whole
// building; each paid stage shows at least one more level while the model has them (Reveal).
// The ground floor (chunks within GroundedBelow of the floor) always shows, so a building never vanishes. Pure.
using System.Collections.Generic;
using UnityEngine;
using TW.Presentation.Terrain;

namespace TW.Presentation.Meta
{
    public static class HomeFrontStages
    {
        /// <summary>A chunk whose foot is this little above the limit still shows (the cuts leave seams).</summary>
        public const float Epsilon = 0.02f;

        /// <summary>The stages' ShownHeight values (FactionBuildings.StageHeights; TW.UI cannot be referenced from here, so a
        /// test holds the two equal). Stage 0 is the ground floor, the last the whole building.</summary>
        public static readonly float[] StageFractions = { 0.3f, 0.55f, 0.8f, 1f };
        public static float FirstStage => StageFractions[0];

        /// <summary>The distinct heights (house frame) the chunks above the ground floor stand on, lowest first.</summary>
        public static List<float> Levels(HouseKit.House house)
        {
            var levels = new List<float>();
            if (house == null || house.Chunks == null) return levels;
            float ground = house.Bounds.min.y + HouseKit.GroundedBelow + Epsilon;
            foreach (var c in house.Chunks)
            {
                float y = c.Local.min.y;
                if (y <= ground) continue;
                bool seen = false;
                foreach (var l in levels) if (Mathf.Abs(l - y) <= Epsilon * 2f) { seen = true; break; }
                if (!seen) levels.Add(y);
            }
            levels.Sort();
            return levels;
        }

        /// <summary>How many of a model's levels stage s of the last shows: none at stage 0, all at the last, and between
        /// them at least one more per stage while the model has levels to spare, so a purchase always shows. With fewer
        /// levels than stages the repeat falls on a middle stage, never on stage I or the last (critic r5).</summary>
        public static int Reveal(int s, int last, int levels)
        {
            if (s <= 0) return 0;
            if (s >= last) return levels;
            return Mathf.Min(Mathf.Max(Mathf.RoundToInt(s * levels / (float)last), s), Mathf.Max(0, levels - 1));
        }

        /// <summary>The height (in the house's frame) a building showing <paramref name="shown"/> of itself reaches: at a
        /// stage's fraction exactly its Reveal count of levels, rising smoothly between two stages while it animates.</summary>
        public static float Limit(HouseKit.House house, float shown)
        {
            float floor = house.Bounds.min.y, top = house.Bounds.max.y, ground = floor + HouseKit.GroundedBelow;
            if (shown >= 1f) return top;
            var levels = Levels(house);
            if (levels.Count == 0 || shown <= StageFractions[0]) return levels.Count == 0 ? Mathf.Max(ground, floor + (top - floor) * Mathf.Clamp01(shown)) : ground;
            int last = StageFractions.Length - 1, s = 0;
            while (s < last - 1 && shown >= StageFractions[s + 1]) s++;
            float f = Mathf.Clamp01((shown - StageFractions[s]) / (StageFractions[s + 1] - StageFractions[s]));
            float At(int r) => r <= 0 ? ground : r >= levels.Count ? top : levels[r] - Epsilon * 2f;   // just under the first hidden level
            return Mathf.Lerp(At(Reveal(s, last, levels.Count)), At(Reveal(s + 1, last, levels.Count)), f);
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
