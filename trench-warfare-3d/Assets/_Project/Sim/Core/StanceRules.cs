// Phase: A1 (implemented; moved here from Units so the Nav jobs can use it) — depends on: Stance (P0)
// Speed, cover and accuracy multipliers per stance, plus the terrain speed multiplier of the occupied nav cell.
// Terrain takes raw NavLayer bits because Core cannot reference Terrain; the constants mirror TW.Sim.Terrain.NavLayer.
namespace TW.Sim
{
    public static class StanceRules
    {
        const byte WireBit = 1 << 4, MudBit = 1 << 5, CraterBit = 1 << 6;   // NavLayer.Wire / Mud / Crater

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
