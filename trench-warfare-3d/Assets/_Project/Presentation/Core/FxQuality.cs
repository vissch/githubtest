// Phase: VFX pass (owner, 2026-09-28: "we need to go that extra mile and have everything customized, add the extra effort
// zoom and quality level") — the effects' own quality level. Every budget the effects keep (impacts and hits a frame, the
// chunk pools, the close-up reach, the debris and grit counts, how many muzzle cards far from the eye) was one fixed number
// tuned for the desk's machine; this puts them on four tiers. HIGH is those numbers exactly, so a High picture is the one
// every bench and critique measured. EPIC spends more and turns on the close-up pieces each class draws among the men
// (CombatFx.Close.cs); MEDIUM and LOW spend less. The tier comes from, in order: the knob fx.quality (0..3, a bench or a
// capture pins it), the player's EFFECTS DETAIL setting (GameSettings.Video.Effects), or the Unity quality level
// (Very Low and Low: Low, Medium: Medium, High and Very High: High, Ultra: Epic).
using UnityEngine;

namespace TW.Presentation
{
    public enum FxTier : byte { Low = 0, Medium = 1, High = 2, Epic = 3 }

    /// <summary>What one tier spends. High is the numbers the effects had before the tiers, exactly.</summary>
    public readonly struct FxProfile
    {
        public readonly FxTier Tier;
        public readonly float Chunks;                      // the chunk pools' caps, x the old
        public readonly int ImpactsNear, ImpactsFar;       // a round's spurt of dirt: at most this many a frame near the look point / far from it
        public readonly int HitsNear, HitsFar;             // a man struck: the same
        public readonly float Reach;                       // the close-up small things (a case, a thread of smoke): x their old reach
        public readonly int FlareEvery;                    // far from the look point, one muzzle card in this many
        public readonly float Debris, Grit;                // a burst's clods and its grit, x the old
        public readonly int ArcSegments;                   // an indirect round's arc (and a rocket's trail) in this many pieces
        public readonly float ExtraReach;                  // the per-class close-up pieces, drawn this near the eye (m); 0 = none
        public readonly bool Epic;                         // the extra pieces only Epic spends: a drawn smoke puff on every round, belt links

        public FxProfile(FxTier tier, float chunks, int impactsNear, int impactsFar, int hitsNear, int hitsFar, float reach, int flareEvery,
                         float debris, float grit, int arcSegments, float extraReach, bool epic)
        {
            Tier = tier; Chunks = chunks; ImpactsNear = impactsNear; ImpactsFar = impactsFar; HitsNear = hitsNear; HitsFar = hitsFar;
            Reach = reach; FlareEvery = flareEvery; Debris = debris; Grit = grit; ArcSegments = arcSegments; ExtraReach = extraReach; Epic = epic;
        }

        /// <summary>A cap of the old `n`, at this tier.</summary>
        public int Cap(int n) => Mathf.Max(1, Mathf.RoundToInt(n * Chunks));
    }

    public static class FxQuality
    {
        public const string Knob = "fx.quality";   // -1 (unset): the setting, or the Unity quality level; 0..3 pins a tier
        public static readonly string[] Names = { "LOW", "MEDIUM", "HIGH", "EPIC" };
        public const string AutoName = "AUTO";

        public static FxTier Tier { get; private set; } = FxTier.High;
        public static FxProfile Now { get; private set; } = For(FxTier.High);

        /// <summary>The numbers of a tier.</summary>
        public static FxProfile For(FxTier tier)
        {
            switch (tier)
            {
                case FxTier.Low:    return new FxProfile(tier, 0.5f,  8,  3, 16,  4, 0f,   3, 0.5f,  0.4f,  4,  0f, false);
                case FxTier.Medium: return new FxProfile(tier, 0.75f, 16, 5, 28,  7, 0.7f, 2, 0.75f, 0.7f,  6,  0f, false);
                case FxTier.Epic:   return new FxProfile(tier, 1.3f,  32, 12, 56, 14, 1.4f, 1, 1.25f, 1.3f, 12, 55f, true);
                default:            return new FxProfile(FxTier.High, 1f, 24, 8, 40, 10, 1f, 1, 1f,   1f,   8,  30f, false);
            }
        }

        /// <summary>The tier a Unity quality level stands for: the bottom third Low, then Medium, then High, the top level Epic.</summary>
        public static FxTier FromUnity(int level, int count)
        {
            if (count <= 1) return FxTier.High;
            float t = Mathf.Clamp01(level / (float)(count - 1));
            return t < 0.3f ? FxTier.Low : t < 0.5f ? FxTier.Medium : t < 0.95f ? FxTier.High : FxTier.Epic;
        }

        /// <summary>The tier in force: the knob if set, else the player's setting if set, else the Unity quality level's.</summary>
        public static FxTier Resolve(int setting, int unityLevel, int unityCount, int knob)
        {
            if (knob >= 0) return (FxTier)Mathf.Min(knob, 3);
            if (setting >= 0) return (FxTier)Mathf.Min(setting, 3);
            return FromUnity(unityLevel, unityCount);
        }

        /// <summary>Put the player's EFFECTS DETAIL setting (-1 = follow the quality level) into force.</summary>
        public static void Apply(int setting) =>
            Set(Resolve(setting, QualitySettings.GetQualityLevel(), QualitySettings.names.Length, Knobs.Get(Knob, -1)));

        public static void Set(FxTier tier) { Tier = tier; Now = For(tier); }

        /// <summary>Whether this round's muzzle card is drawn: always near the look point (inside `near` m), and far from it
        /// one in FlareEvery by the round (the tick and the shooter, so the same rounds every run). The overview (zoom
        /// past FarZoom) thins High by half again: a flare there is a few pixels.</summary>
        public static bool FlareKept(in FxProfile q, float toLook, float zoom, uint round)
        {
            const float Near = 90f, FarZoom = 150f;
            int every = q.FlareEvery;
            if (zoom >= FarZoom && q.Tier <= FxTier.High) every *= 2;
            if (toLook < Near || every <= 1) return true;
            return Hash(round) % (uint)every == 0u;
        }

        /// <summary>A cheap integer hash for the effects' own variety, so a tier never takes a draw from the shared
        /// UnityEngine.Random stream (an A/B of two tiers keeps the weather's lightning on the same frames).</summary>
        public static uint Hash(uint x)
        {
            x ^= x >> 16; x *= 0x7feb352du; x ^= x >> 15; x *= 0x846ca68bu; x ^= x >> 16;
            return x;
        }

        /// <summary>The hash as a value in [0, 1).</summary>
        public static float Hash01(uint x) => (Hash(x) & 0xFFFFFFu) / 16777216f;
    }
}
