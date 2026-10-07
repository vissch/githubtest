// Phase: VFX pass (owner, 2026-09-28; tw3d-board catalogue IN-7, look spec L01) — part of CombatFx (see CombatFx.cs for
// the event dispatch). Which drawn parts a burst gets, by what went off (the sim's Dir.y: 0 shell, 1 falling masonry,
// 2 cook-off), whether it was flying (Dir.xz) and whether it landed in water. Pure, so a test can hold it. Behind the
// knob fx.recipes: 1 by default (owner, 2026-10-07, after the films); 0 returns the old burst exactly.
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
            public bool Ring;       // a GroundRing lying on the ground: how big a big burst was, read from far off
            public bool Lean;       // a ShellLean: the earth thrown the way a flying shell was going (L02)
            public bool Dust;       // two DustPuffs at the foot: a jetpack man coming down (L15)
            public bool Curtain;    // a SmokeBank standing where a creeping barrage's lift landed: the wall that walks (L04)

            public static BurstRecipe Old => new BurstRecipe { Column = true, OldSmoke = true };
        }

        public const string RecipesKnob = "fx.recipes";
        public const float RingRadius = 6f;       // a shell this big (the sim's radius, m) throws a ring along the ground (the barrage's 8 m shells, not its 5 m)
        public const float DefaultRecipes = 1f;   // on since 2026-10-07 (owner, after the films of all eleven: decisions.md); 0 is the old burst

        /// <summary>fx.recipes, 0 (the old burst) or 1 (the VFX pass's recipes).</summary>
        public static float ReadRecipes() => Mathf.Clamp01(Knobs.Get(RecipesKnob, DefaultRecipes));

        /// <summary>The parts a burst draws. shape: the sim's Dir.y (0 shell, 1 masonry, 2 cook-off, 3 incendiary, 5 mine); lean: how much of Dir.xz
        /// there was (0 for a round with no flight: the Kettle's, the Salvo's); recipes: fx.recipes; radius: the sim's blast radius;
        /// source: the Explosion's a (SourceId), which tells a jetpack landing from a shell.</summary>
        public static BurstRecipe RecipeFor(float shape, float lean, bool wet, float recipes, float radius = 0f, int source = 0)
        {
            if (recipes < 0.5f || wet) return BurstRecipe.Old;   // water keeps its splash and its drawn smoke as they were
            if (source == TW.Sim.Combat.LeapSystem.LandingSource) return new BurstRecipe { Ring = true, Dust = true };   // L15: a man lands, no shell: no column, no smoke
            int kind = Mathf.RoundToInt(shape);
            if (kind == 2) return new BurstRecipe { CookOff = true, Plume = true };   // no earth: the hull is what goes up
            if (kind != 0) return BurstRecipe.Old;   // masonry (1), incendiary (3), a mine (5): their own looks come later (L14, L19, L24)
            return new BurstRecipe { Column = true, Plume = true, Mortar = lean <= 0f, Lean = lean > 0f, Ring = radius >= RingRadius,
                                     Curtain = source == (int)TW.Sim.Match.OffMapAbilityId.CreepingBarrage };
        }

        /// <summary>The plume a shell leaves (ShellPlume): its width in blast radii, its seconds, how far it swells and the
        /// opacity it is born with (before fx.smokeWeight); with the absurd deaths on, how high it starts (blast radii)
        /// and how fast it climbs (m/s), so it stands over the crater and not on it.</summary>
        public const float PlumeWidth = 2.2f, PlumeLife = 6f, PlumeGrow = 0.9f, PlumeAlpha = 1f, PlumeLiftBase = 0.5f, PlumeLiftRise = 1.2f;

        public const float BloodSnipeDamage = 60f;   // a hit this hard (the sniper's 95, not a rifle's 22-30) sprays the heavy sheet
        public const float BloodFarZoom = 120f;      // past this (the overview) no blood (decisions.md: far zoom shows none)

        /// <summary>The blood card a man struck gets in place of the dust off his coat (owner, 2026-09-28: "we need blood"),
        /// or null: not with fx.recipes 0, GORE 0, a ricochet (damage <= 0) or from the overview.</summary>
        public static FlipbookFx.Book? BloodFor(float damage, float gore, float zoom, float recipes)
        {
            if (recipes < 0.5f || gore <= 0f || damage <= 0f || zoom >= BloodFarZoom) return null;
            return damage >= BloodSnipeDamage ? FlipbookFx.Book.BloodSnipe : FlipbookFx.Book.BloodSpurt;
        }
    }
}
