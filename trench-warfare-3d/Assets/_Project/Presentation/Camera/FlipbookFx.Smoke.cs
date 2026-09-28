// Phase: VFX pass (owner, 2026-09-28: "make the smoke in game less heavy" and "small lean smoke plumes") — part of
// FlipbookFx. One place for how much smoke the fight leaves standing, whoever raised it (a shell's puffs and plume, a
// tank's exhaust and its burning deck, a wreck's column, the flamethrower's soot, a smouldering crater): every card of a
// smoke book is born
//  - lighter: its opacity times fx.smokeWeight,
//  - shorter-lived: its life times SmokeLife(weight), so a barrage's smoke clears sooner instead of banking up,
//  - leaner: narrower by fx.smokeLean at the same height, so it rises as a slim column and not a round cloud.
// Knobs read once (the constructor's field initialisers). fx.smokeWeight 1 and fx.smokeLean 1 draw the smoke as it was.
// The gas, the smoke screen and the dust are not smoke here: they are the field's own state or a hit's grit.
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public sealed partial class FlipbookFx
    {
        public const string SmokeWeightKnob = "fx.smokeWeight", SmokeLeanKnob = "fx.smokeLean";
        public const float DefaultSmokeWeight = 0.6f, DefaultSmokeLean = 0.7f;   // owner, 2026-09-28: lighter and leaner (bench r14: near-black round masses over a third of the z30 frame)

        readonly float smokeWeight = Mathf.Clamp(Knobs.Get(SmokeWeightKnob, DefaultSmokeWeight), 0.05f, 1f);
        readonly float smokeLean = Mathf.Clamp(Knobs.Get(SmokeLeanKnob, DefaultSmokeLean), 0.3f, 1f);

        /// <summary>The books that are a fire's or a burst's smoke.</summary>
        public static bool IsSmoke(Book book) => book == Book.Smoke || book == Book.ShellPlume || book == Book.WreckSmoke || book == Book.Smoulder;

        /// <summary>How long a smoke card lives at a weight, as a share of what it was asked for: lighter smoke also clears
        /// sooner, but never in under 60 % of its time (a puff that pops out of existence reads as a glitch).</summary>
        public static float SmokeLife(float weight) => Mathf.Lerp(0.6f, 1f, Mathf.Clamp01(weight));

        /// <summary>A smoke card's width, height, life and opacity as it is born (height 0: the drawing's own aspect, taken
        /// from the width asked for, so the lean narrows it and keeps it as tall).</summary>
        public static void ShapeSmoke(float weight, float lean, float aspect, ref float width, ref float height, ref float life, ref float alpha)
        {
            if (height <= 0f) height = width / Mathf.Max(0.05f, aspect);
            width *= lean;
            life *= SmokeLife(weight);
            alpha *= weight;
        }
    }
}
