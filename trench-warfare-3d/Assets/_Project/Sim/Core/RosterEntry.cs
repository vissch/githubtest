// Phase: P0 (implemented) — minimal per-slot stats the core needs for deployment and Phase 0 movement.
// Full unit stats (weapons, armor, abilities) live in TW.Sim.Units.UnitStats and are baked by TW.Data (Phase C2).
namespace TW.Sim
{
    public struct RosterEntry
    {
        public byte Archetype;     // index into the faction's UnitStats table (Phase A3); Phase 0 uses 0..4 = slot
        public int Cost;           // silver
        public float Hp;
        public float Speed;        // m/s on flat surface, standing
        public int CooldownTicks;  // per-slot deploy cooldown (specials and vehicles)
        public bool IsVehicle;

        public const int SlotCount = 5;

        /// <summary>Phase 0 placeholder roster: rifleman, assault, machinegunner, special (sniper), vehicle (Mark IV).</summary>
        public static void FillDefault(Unity.Collections.NativeArray<RosterEntry> roster, int playerOffset)
        {
            roster[playerOffset + 0] = new RosterEntry { Archetype = 0, Cost = 25, Hp = 100, Speed = 3.0f };
            roster[playerOffset + 1] = new RosterEntry { Archetype = 1, Cost = 40, Hp = 90, Speed = 4.2f };
            roster[playerOffset + 2] = new RosterEntry { Archetype = 2, Cost = 60, Hp = 110, Speed = 2.2f };
            roster[playerOffset + 3] = new RosterEntry { Archetype = 3, Cost = 90, Hp = 80, Speed = 3.0f, CooldownTicks = 200 };
            roster[playerOffset + 4] = new RosterEntry { Archetype = 4, Cost = 350, Hp = 10000, Speed = 1.6f, CooldownTicks = 600, IsVehicle = true };
        }
    }
}
