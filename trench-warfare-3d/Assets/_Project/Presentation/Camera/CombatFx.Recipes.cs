// Phase: VFX pass (owner, 2026-09-28; tw3d-board catalogue IN-7, look spec L01) — part of CombatFx (see CombatFx.cs for
// the event dispatch). Which drawn parts a burst gets, by what went off (the sim's Dir.y: 0 shell, 1 falling masonry,
// 2 cook-off), whether it was flying (Dir.xz) and whether it landed in water. Pure, so a test can hold it. Behind the
// knob fx.recipes: 0 (the default until it has been seen in Play at every zoom) returns the old burst exactly.
using UnityEngine;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        /// <summary>The drawn parts of one burst. The old burst is Column + OldSmoke and nothing else.</summary>
        public struct BurstRecipe
        {
            public bool Column;     // the earth (or water) column and its two wings
            public bool OldSmoke;   // the seven Smoke puffs that climb and drift off
            public bool Plume;      // one ShellPlume in their place: the smoke a burst leaves standing
            public bool Mortar;     // a MortarBurst over the column: a round that came down with no lean
            public bool CookOff;    // a FireCookOff fireball in place of the column: a hull going up

            public static BurstRecipe Old => new BurstRecipe { Column = true, OldSmoke = true };
        }

        public const string RecipesKnob = "fx.recipes";
        public const float DefaultRecipes = 0f;   // the old burst until the recipes have been seen in Play (catalogue: proof per zoom)

        /// <summary>fx.recipes, 0 (the old burst) or 1 (the VFX pass's recipes).</summary>
        public static float ReadRecipes() => Mathf.Clamp01(Knobs.Get(RecipesKnob, DefaultRecipes));

        /// <summary>The parts a burst draws. shape: the sim's Dir.y (0 shell, 1 masonry, 2 cook-off); lean: how much of Dir.xz
        /// there was (0 for a round with no flight: the Kettle's, the Salvo's); recipes: fx.recipes.</summary>
        public static BurstRecipe RecipeFor(float shape, float lean, bool wet, float recipes)
        {
            if (recipes < 0.5f || wet) return BurstRecipe.Old;   // water keeps its splash and its drawn smoke as they were
            int kind = Mathf.RoundToInt(shape);
            if (kind == 2) return new BurstRecipe { CookOff = true, Plume = true };   // no earth: the hull is what goes up
            if (kind == 1) return BurstRecipe.Old;                                    // masonry: the house's own look (L24)
            return new BurstRecipe { Column = true, Plume = true, Mortar = lean <= 0f };
        }
    }
}
