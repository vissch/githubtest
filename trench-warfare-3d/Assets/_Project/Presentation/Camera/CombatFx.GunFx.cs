// Phase: gun-fire effects (owner, 2026-10-14, option A: "what a gun does: muzzle flash, muzzle smoke, bullet impact
// puff. 166 of 226 effects fired in four match minutes"). The in-game review measured the faults these answer: at play
// zoom 30 twenty riflemen firing on snow give 0.25 % warm pixels and no frame over 1 %, and the bullet impact puff is
// absent altogether. Here is the variety and the sizing those three cards ask for, as pure statics so tests can read
// them: the flash shows a DIFFERENT cel of its nine-frame sheet per shooter instead of twenty copies of one quad, the
// muzzle leaves a short low cloud, and a round that strikes a man kicks dust at his feet.
//
// Everything is hashed from (tick, unit), never drawn from UnityEngine.Random: the Shot and the Hit paths share one
// Random stream with the tracers, cases and chunks, so one new draw here would move every unrelated effect downstream
// (FxQuality.Hash exists for exactly this reason). The same (tick, unit) therefore gives the same look every run.
using UnityEngine;
using TW.Presentation;   // FxQuality.Hash: the effects' own variety, never the shared Random stream

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        /// <summary>How one shot's muzzle flash differs from its neighbour's: which cel of the sheet it starts on, a little
        /// roll either side of the barrel, its size, and whether the card is mirrored across the barrel.</summary>
        public struct MuzzleVariantLook { public float Start, Roll, Scale; public bool Mirror; }

        /// <summary>The muzzle's own cloud: how wide, how long it lives and how solid.</summary>
        public struct MuzzleSmokeLook { public float Wide, Life, Alpha; }

        /// <summary>The dust a round kicks where it strikes: how wide, how long, its roll, and whether mirrored.</summary>
        public struct ImpactPuffLook { public float Wide, Life, Roll; public bool Mirror; }

        /// <summary>The first cel a flash may start on. The sheet (MuzzleRifle and every class book) is 9 frames at
        /// Fps 12, so a flare of 0.18 s (ArmsLook.Rifle) spends 2.16 frames: from cel 3 it ends at 5.16, well inside
        /// the row's span (Frames * Play - 1.001 = 7.999). Start later and the card would run off the sheet.</summary>
        public const float MuzzleStartCels = 3f;

        static uint GunSeed(uint tick, int unit, uint salt) =>
            tick * 2654435761u + (uint)unit * 40503u + salt * 0x9E3779B1u;

        /// <summary>One shot's flash, hashed from the tick and the shooter: twenty men firing in one frame show different
        /// cels, rolls and sizes, where before every one of them drew cel 0 of the same sheet at the same roll (the
        /// review's "one copied quad twenty times").</summary>
        public static MuzzleVariantLook MuzzleVariant(uint tick, int shooter) => new MuzzleVariantLook
        {
            Start = Mathf.Min(MuzzleStartCels, FxQuality.Hash01(GunSeed(tick, shooter, 0u)) * (MuzzleStartCels + 1f)),
            Roll = (FxQuality.Hash01(GunSeed(tick, shooter, 1u)) - 0.5f) * 0.5f,   // +-0.25 rad about the barrel, never along it
            Scale = 0.85f + FxQuality.Hash01(GunSeed(tick, shooter, 2u)) * 0.4f,
            Mirror = (FxQuality.Hash(GunSeed(tick, shooter, 3u)) & 1u) == 1u,
        };

        /// <summary>The short grey-white cloud the muzzle leaves (the review's words). Small and thin: the Puff row is
        /// retinted per biome by ApplyTints, so it is grey-white on snow and grey at night without touching the row, and
        /// it stays under half alpha so it never becomes the smoke bank a firefight cannot see through. At night it is
        /// thinner still (the owner's rule: night keeps its blue, and black smoke cannot be seen at night).</summary>
        public static MuzzleSmokeLook MuzzleSmoke(bool night, float closeUp) => new MuzzleSmokeLook
        {
            Wide = Mathf.Lerp(0.6f, 1.15f, Mathf.Clamp01(closeUp)),
            Life = Mathf.Lerp(0.45f, 0.6f, Mathf.Clamp01(closeUp)),
            Alpha = night ? 0.22f : 0.38f,
        };

        /// <summary>The ground puff where a round strikes a man: about a quarter second and under a man wide, hashed from
        /// the tick and the target so twenty hits in a frame are twenty different clouds.</summary>
        public static ImpactPuffLook ImpactPuff(uint tick, int target) => new ImpactPuffLook
        {
            Wide = 0.55f + FxQuality.Hash01(GunSeed(tick, target, 11u)) * 0.45f,
            Life = 0.22f + FxQuality.Hash01(GunSeed(tick, target, 13u)) * 0.1f,
            Roll = FxQuality.Hash01(GunSeed(tick, target, 12u)) * 6.2832f,
            Mirror = (FxQuality.Hash(GunSeed(tick, target, 14u)) & 1u) == 1u,
        };
    }
}
