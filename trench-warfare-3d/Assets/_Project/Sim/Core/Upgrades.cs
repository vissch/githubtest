// Phase: P0 (implemented) — the sim's upgrade seam ([Z009.1], docs/21 B1).
// What a side bought on the Home Front and brings to this match. The shop (TW.UI's FactionBuildings) states the
// per-tier sizes; the sim may not reference TW.UI, so it restates them here and this comment says where they come
// from: FactionBuildings.PerTier = 0.05 (a unit track is +5 % a tier), ArmourMmPerTier = 2, AbilityPercentPerTier = 8
// (a cooldown and a cost are 8 % smaller a tier). Keep the two in step by hand; a change to either is a seam change.
// Every size is driven by integers, never by a float literal, so two machines agree bit for bit.
using Unity.Collections;

namespace TW.Sim
{
    /// <summary>The three per-unit-kind tracks the shop sells (UI/Campaign/FactionBuildings.cs: Health, Damage, Accuracy).</summary>
    public enum UpgradeTrack : byte { Health = 0, Damage = 1, Accuracy = 2 }

    /// <summary>The per-tier sizes and the clamps, restated from the shop (see the file header).</summary>
    public static class Upgrades
    {
        /// <summary>A unit track is +5 % a tier (FactionBuildings.PerTier = 0.05f).</summary>
        public const int TrackPercentPerTier = 5;
        /// <summary>Armour plate is +2 mm on every face a tier (FactionBuildings.ArmourMmPerTier).</summary>
        public const int ArmourMmPerTier = 2;
        /// <summary>Signal discipline and requisition are -8 % a tier (FactionBuildings.AbilityPercentPerTier).</summary>
        public const int AbilityPercentPerTier = 8;
        /// <summary>A unit track runs to tier 6 (FactionBuildings.TrackCosts has six prices).</summary>
        public const int MaxTrackTier = 6;
        /// <summary>A global row runs to tier 3 (FactionBuildings.GlobalCosts has three prices).</summary>
        public const int MaxGlobalTier = 3;

        public static byte ClampTrack(int tier) => (byte)(tier < 0 ? 0 : tier > MaxTrackTier ? MaxTrackTier : tier);
        public static byte ClampGlobal(int tier) => (byte)(tier < 0 ? 0 : tier > MaxGlobalTier ? MaxGlobalTier : tier);

        /// <summary>What a unit track multiplies its number by: 1 + 5 % a tier, built from integers (tier 2 = 1.10f exactly).</summary>
        public static float Scale(int tier) => (100 + TrackPercentPerTier * ClampTrack(tier)) / 100f;

        /// <summary>The same, named by its track: the three tracks share one step today (+5 % a tier).</summary>
        public static float Scale(UpgradeTrack track, int tier) => Scale(tier);

        /// <summary>The millimetres of plate a tier adds to every face of every machine.</summary>
        public static float ArmourMm(int tier) => ArmourMmPerTier * ClampGlobal(tier);

        /// <summary>An ability's ticks or silver, 8 % smaller a tier: integer arithmetic, truncating (tier 2: 150 -> 126).</summary>
        public static int Ints(int value, int tier) => value * (100 - AbilityPercentPerTier * ClampGlobal(tier)) / 100;
    }

    /// <summary>
    /// The tiers one side brings to a match. It lives in SimConfig (and so in the lockstep handshake and the replay
    /// header) because both machines must agree on it before the first tick: a side whose shells pierce 2 mm more
    /// plate than its peer thinks is a desync, not a mistake. Every tier zero is the game as it was before the seam.
    /// </summary>
    public struct SideUpgrades
    {
        /// <summary>ARMOUR PLATE: +2 mm on every face of every machine a tier (0..3).</summary>
        public byte Armour;
        /// <summary>SIGNAL DISCIPLINE: an off-map ability is ready 8 % sooner a tier (0..3).</summary>
        public byte AbilityCooldown;
        /// <summary>REQUISITION: an off-map ability costs 8 % less a tier (0..3).</summary>
        public byte AbilityCost;

        /// <summary>One four-byte record a unit kind: archetype, health tier, damage tier, accuracy tier. A kind that
        /// bought nothing is absent, so an untouched side carries an empty list and TierOf early-outs on it. A
        /// FixedList128Bytes holds 124 bytes of payload: 31 kinds a side, more than the ten a roster fields.</summary>
        public FixedList128Bytes<byte> Tracks;

        public const int RecordSize = 4;
        /// <summary>The most unit kinds one side can carry tiers for.</summary>
        public static int MaxKinds => 124 / RecordSize;

        /// <summary>The tier this side bought on one track of one unit kind, 0 when it bought none.</summary>
        public int TierOf(byte archetype, UpgradeTrack track)
        {
            int n = Tracks.Length;
            if (n == 0) return 0;                                     // the common case: nobody upgraded anything
            for (int i = 0; i + RecordSize <= n; i += RecordSize)
                if (Tracks[i] == archetype) return Tracks[i + 1 + (int)track];
            return 0;
        }

        /// <summary>What to multiply one number of one unit kind by (1f when it bought nothing).</summary>
        public float Scale(byte archetype, UpgradeTrack track) => Upgrades.Scale(track, TierOf(archetype, track));

        /// <summary>The plate this side's machines carry on every face, in millimetres.</summary>
        public float ArmourMm => Upgrades.ArmourMm(Armour);

        /// <summary>Set (or replace) one unit kind's three tiers. Returns false when the list is full.</summary>
        public bool SetTrack(byte archetype, int health, int damage, int accuracy)
        {
            byte h = Upgrades.ClampTrack(health), d = Upgrades.ClampTrack(damage), a = Upgrades.ClampTrack(accuracy);
            for (int i = 0; i + RecordSize <= Tracks.Length; i += RecordSize)
                if (Tracks[i] == archetype)
                {
                    Tracks[i + 1] = h; Tracks[i + 2] = d; Tracks[i + 3] = a;
                    return true;
                }
            if (Tracks.Length + RecordSize > Tracks.Capacity) return false;
            Tracks.Add(archetype); Tracks.Add(h); Tracks.Add(d); Tracks.Add(a);
            return true;
        }

        /// <summary>The globals, each clamped to the rows the shop sells.</summary>
        public static SideUpgrades Global(int armour, int abilityCooldown, int abilityCost) => new SideUpgrades
        {
            Armour = Upgrades.ClampGlobal(armour),
            AbilityCooldown = Upgrades.ClampGlobal(abilityCooldown),
            AbilityCost = Upgrades.ClampGlobal(abilityCost),
        };
    }
}
