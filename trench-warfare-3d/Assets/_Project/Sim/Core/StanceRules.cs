// Phase: A1 (implemented; moved here from Units so the Nav jobs can use it) — depends on: Stance (P0)
// Speed, cover and accuracy multipliers per stance, plus the terrain speed multiplier of the occupied nav cell.
// Terrain takes raw NavLayer bits because Core cannot reference Terrain; the constants mirror TW.Sim.Terrain.NavLayer.
namespace TW.Sim
{
    public static class StanceRules
    {
        const byte WireBit = 1 << 4, MudBit = 1 << 5, CraterBit = 1 << 6;   // NavLayer.Wire / Mud / Crater

        public const float ProneSuppression = 60f;    // above this a unit in the open goes prone
        public const float PinnedSuppression = 85f;   // above this it stops moving and firing, and refuses >>

        // Under fire in the open (2026-10-01, the owner: "as realistic as possible"; men ran upright through fire 72-81 %
        // of the time they were being shot at, and went down only past ProneSuppression). A man who has been fired on
        // keeps his head down for a while after the fire stops: SimWorld.Alarm follows his suppression up at once, held to
        // AlarmCap, and comes down AlarmDecay a second where suppression comes down 8. While it is at CarefulAlarm or more
        // he moves in rushes: RushUpTicks bent double at a run (Crouch, RushSpeed), then down on his belly where he is
        // (Prone, still, firing if he has a mark) for the rest of RushTicks, then up again, each man on his own beat so
        // that some of a section are down covering the others while they run; running, he makes for a shell hole a few
        // strides ahead (MovementSystem, CoverReach).
        public const float CarefulAlarm = 8f;     // one rifle round past him is not enough; two are
        public const float AlarmDecay = 2.5f;     // a second: from AlarmCap he is careful another 13 s after the fire stops
        public const float AlarmCap = 40f;
        public const int RushTicks = 160;         // one rush and drop: 8 s
        public const int RushUpTicks = 90;        // 4.5 s of it up and running, 3.5 s down
        public const float RushSpeed = 1.15f;     // bent double: under an upright sprint (1.5), over a walk
        public const float CoverReach = 6f;       // m: a shell hole this near and ahead is where his rush goes
        public const float CrowdedPush = 0.2f;    // m/s: pressed this hard by his mates he keeps his own line, not the hole's
        public const float LyingYield = 0.4f;     // of the push a lying man moves by

        public static float SpeedMultiplier(Stance s)
        {
            switch (s)
            {
                case Stance.Crouch: return 0.8f;
                case Stance.Prone: return 0.4f;
                case Stance.Sprint: return 1.5f;
                case Stance.FireStep: case Stance.Pinned: case Stance.Dead: return 0f;
                default: return 1f;                 // Standing, Vault
            }
        }

        /// <summary>Infantry speed multiplier for the nav cell being crossed: wire 0.2×, mud 0.5×, crater 0.8×.</summary>
        public static float TerrainMultiplier(byte layerBits)
        {
            if ((layerBits & WireBit) != 0) return 0.2f;
            if ((layerBits & MudBit) != 0) return 0.5f;
            if ((layerBits & CraterBit) != 0) return 0.8f;
            return 1f;
        }

        public static float CoverBonusInOpen(Stance s) => s == Stance.Prone || s == Stance.Pinned ? 0.4f : s == Stance.Crouch ? 0.1f : s == Stance.Sprint ? -0.1f : 0f;
        public static float AccuracyMultiplier(Stance s, bool machinegun) => s == Stance.FireStep ? 1.25f : s == Stance.Prone ? (machinegun ? 1.2f : 0.9f) : 1f;
    }
}
