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

        /// <summary>Phase 0 placeholder roster: rifleman, assault, machinegunner, special (sniper), vehicle. Player 0's
        /// vehicle is the Maw (heavy, sponson guns, the Mark IV of the roster), player 1's the Tusk (light, turret, the
        /// Renault FT). Vehicle Hp is the hull's structure; armour, modules and crew decide most fights (A5b).</summary>
        public static void FillDefault(Unity.Collections.NativeArray<RosterEntry> roster, int playerOffset)
        {
            roster[playerOffset + 0] = new RosterEntry { Archetype = 0, Cost = 25, Hp = 100, Speed = 3.0f };
            roster[playerOffset + 1] = new RosterEntry { Archetype = 1, Cost = 40, Hp = 90, Speed = 4.2f };
            roster[playerOffset + 2] = new RosterEntry { Archetype = 2, Cost = 60, Hp = 110, Speed = 2.2f };
            roster[playerOffset + 3] = new RosterEntry { Archetype = 3, Cost = 90, Hp = 80, Speed = 3.0f, CooldownTicks = 200 };
            roster[playerOffset + 4] = playerOffset == 0 ? Maw : Tusk;
        }

        public static RosterEntry Maw => new RosterEntry { Archetype = VehicleArchetype.Maw, Cost = 350, Hp = 3600, Speed = 1.6f, CooldownTicks = 600, IsVehicle = true };
        public static RosterEntry Tusk => new RosterEntry { Archetype = VehicleArchetype.Tusk, Cost = 260, Hp = 2000, Speed = 2.4f, CooldownTicks = 450, IsVehicle = true };
    }

    /// <summary>Archetype ids of the two tanks (Resources/Vehicles/Maw and /Tusk).</summary>
    public static class VehicleArchetype
    {
        public const byte Maw = 4;    // heavy: two sponson 6-pdrs, crosses 3.5 m trenches, crushes wire, pushes trees over
        public const byte Tusk = 5;   // light: a turret 37 mm with a coaxial MG, quick, ditches in wide trenches
        public static bool IsTank(byte archetype) => archetype == Maw || archetype == Tusk;
    }
}
