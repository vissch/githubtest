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

        /// <summary>The books that are a fire's or a burst's smoke. Burst is the boiling cloud a shell throws up: at the
        /// barrage's 8 m shells it is a 21 m card, and it was the near-black mass over the frame (bench r15: the four
        /// smoke books alone lightened the frame by only 75 k pixels).</summary>
        public static bool IsSmoke(Book book) => book == Book.Smoke || book == Book.ShellPlume || book == Book.WreckSmoke || book == Book.Smoulder || book == Book.Burst;

        /// <summary>How long a smoke card lives at a weight, as a share of what it was asked for: lighter smoke also clears
        /// sooner, but never in under 60 % of its time (a puff that pops out of existence reads as a glitch).</summary>
        /// <summary>The weight the burst's own boiling cloud is born at: most of the way back to full. It lives under two seconds
        /// and is the body the fire burns inside (the owner's snow reference); thinned to the lingering smoke's 0.6 it was a
        /// grey veil the snow showed through (critique s3). The lingering smoke keeps the owner's lighter weight.</summary>
        public static float BurstWeight(float weight) => Mathf.Lerp(Mathf.Clamp01(weight), 1f, 0.7f);

        /// <summary>A cloud born of fire, lit warm from underneath while it is young (Flipbook_URP _Ember): by day, and dimmer at night.</summary>
        public void Ember(Book book, Color warm) { var m = mats[(int)book]; if (m != null) m.SetColor(EmberId, warm); }
        static readonly int EmberId = Shader.PropertyToID("_Ember");

        static readonly Color BurstShade = new Color(0.50f, 0.45f, 0.42f);   // the burst cloud's sooty underside (critique s3)
        // The DAY look (owner's snow reference; critique c1b: "brown cardboard, one value"): the burst cloud from cream-lit tops
        // to soot (tint up, the lit band reached sooner, the shade down), and the thrown earth's shade only half the field's
        // blue. Set by field (CombatFx.ApplyTints, before the night passes): a moonlit field keeps its AOSA-tuned values.
        static readonly Color DayBurstTint = new Color(0.90f, 0.78f, 0.64f), DayBurstShade = new Color(0.30f, 0.25f, 0.22f);
        const float DayBurstHigh = 0.50f, DayEarthMood = 0.45f;

        /// <summary>Put the day look on (on) or back to the sheets' own values (off).</summary>
        public void DayLook(bool on)
        {
            var b = mats[(int)Book.Burst];
            if (b != null)
            {
                var s = Sheets[(int)Book.Burst];
                b.SetColor("_Tint", on ? DayBurstTint : s.Tint);
                b.SetVector("_Levels", new Vector4(s.Low, on ? DayBurstHigh : s.High, 0f, 0f));
                b.SetColor("_Shade", on ? DayBurstShade : BurstShade);
            }
            foreach (var k in new[] { Book.Column, Book.Wings })
            {
                var m = mats[(int)k];
                if (m != null) m.SetFloat("_ShadeMood", on ? DayEarthMood : Sheets[(int)k].Mood > 0f ? Sheets[(int)k].Mood : 1f);
            }
        }

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
