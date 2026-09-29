// Phase: A5d (2026-09-29, the owner's night look: decisions.md) — depends on: Knobs, BiomeProfile.
// The distance lifted into lighter haze, as the owner's colour edit has it. Today the fog, the bank round the field and the
// clear colour all close toward a dark saturated blue (NightMud Haze 0.075, 0.105, 0.17), so the far field is darker than
// the near: the top third of a standard night still reads 0.10 against 0.12 for the bottom third, where the colour edit
// reads 0.25 against 0.14. look.lift (0 today, 1 full) takes the three toward lighter, greyer blues, so the haze lifts the
// distance instead of burying it and the ground's silhouettes stand against it. The blue stays: the edit keeps it.
// Lifted fog over the whole view turns the frame milky (Play, 2026-09-29: the bottom third went 0.12 to 0.16), and the
// edit's foreground stays dark and crisp. The fog begins where it always has, just short of the ground the camera looks
// at, so what lies nearer stays clear; but today it closes some 230 m further on and barely reaches the top of the frame
// (Play: with only the fog lifted the top third moved 0.103 to 0.114). So while it lifts it closes sooner: at full lift,
// look.liftReach times the distance to the ground looked at past its start. The low mist, which lies over the near
// ground too, lifts by only look.liftMist of it; so does the bank's colour (look.liftBank), which the quiet fog
// wears over every stretch of the field without the player's men, near or far (TWAtmosphere.hlsl FieldFogAmount).
// look.wet (0 today, 1 full): the puddles mirror less of the pale sky (big pale patches were where the frame spent its
// highlights: the report's fingerprint), and the flames' pools throw hard glints of their own colour on the wet ground
// (TWLightPools.hlsl, with look.pools on), so the mud sparkles orange by a fire and moon-blue away from it.
// Dark fields only, never lava: its magenta fog is its own look.
using UnityEngine;
using TW.Presentation;

namespace TW.Presentation.Terrain
{
    public sealed partial class Atmosphere
    {
        public const string LiftKnob = "look.lift";
        /// <summary>What the haze (the fog and the clear colour), the bank round the field and the low mist become at
        /// look.lift 1: lighter than the night ground and less saturated than today's, still blue.</summary>
        public static readonly Color LiftHaze = new Color(0.25f, 0.29f, 0.37f), LiftBank = new Color(0.21f, 0.25f, 0.33f), LiftMist = new Color(0.27f, 0.32f, 0.41f);
        public const float LiftReach = 1f, LiftMistShare = 0.3f, LiftBankShare = 0.35f;
        static readonly int WetLookId = Shader.PropertyToID("_TWWetLook");
        float wetLook;
        float lift, liftReach = LiftReach, liftMist = LiftMistShare, liftBank = LiftBankShare; int liftKnobs = -1;

        /// <summary>look.lift now, 0..1, and 0 on a lit field or on lava.</summary>
        float Lift()
        {
            if (liftKnobs != Knobs.Generation)
            {
                liftKnobs = Knobs.Generation;
                lift = Mathf.Clamp01(Knobs.Get(LiftKnob, 0f));
                liftReach = Mathf.Clamp(Knobs.Get("look.liftReach", LiftReach), 0.25f, 20f);
                liftMist = Mathf.Clamp01(Knobs.Get("look.liftMist", LiftMistShare));
                liftBank = Mathf.Clamp01(Knobs.Get("look.liftBank", LiftBankShare));
                wetLook = Mathf.Clamp01(Knobs.Get("look.wet", 0f));
            }
            bool night = Look == Mood.Night && Profile.HeatStrength <= 0f;
            Shader.SetGlobalFloat(WetLookId, night ? wetLook : 0f);
            return night ? lift : 0f;
        }

        /// <summary>Where the fog closes: today's end, drawn in toward start + reach x the distance to the ground looked at
        /// as the lift grows (lift 0: today's end exactly).</summary>
        public static float LiftedFogEnd(float start, float today, float toFocus, float reach, float k) =>
            k <= 0f ? today : Mathf.Lerp(today, start + reach * toFocus, Mathf.Clamp01(k));

        /// <summary>A haze colour taken toward its lifted one by k (k 0: the colour unchanged).</summary>
        public static Color Lifted(Color c, Color to, float k) => k <= 0f ? c : Color.Lerp(c, to, Mathf.Clamp01(k));
    }
}
