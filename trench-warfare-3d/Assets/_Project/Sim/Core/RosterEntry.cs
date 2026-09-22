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

        public const int SlotCount = 8;

        /// <summary>Phase 0 placeholder roster: rifleman, assault, machinegunner, special (sniper), tank, walker.
        /// Player 0's tank is the Maw (heavy, sponson guns, the Mark IV of the roster), player 1's the Tusk (light,
        /// turret, the Renault FT). Each side then has two walkers: player 0 the Pincer (twin turret guns, six legs,
        /// two heavy claws) and the Pavise (a long gun behind a shield); player 1 the Kettle (a mortar that fires
        /// without seeing) and the Censer (a drum of chlorine it lays as it walks). Vehicle Hp is the hull's
        /// structure; armour, modules, legs and crew decide most fights (A5b).</summary>
        public static void FillDefault(Unity.Collections.NativeArray<RosterEntry> roster, int playerOffset)
        {
            roster[playerOffset + 0] = new RosterEntry { Archetype = 0, Cost = 25, Hp = 100, Speed = 3.0f };
            roster[playerOffset + 1] = new RosterEntry { Archetype = 1, Cost = 40, Hp = 90, Speed = 4.2f };
            roster[playerOffset + 2] = new RosterEntry { Archetype = 2, Cost = 60, Hp = 110, Speed = 2.2f };
            roster[playerOffset + 3] = new RosterEntry { Archetype = 3, Cost = 90, Hp = 80, Speed = 3.0f, CooldownTicks = 200 };
            roster[playerOffset + 4] = playerOffset == 0 ? Maw : Tusk;
            roster[playerOffset + 5] = playerOffset == 0 ? Pincer : Kettle;
            roster[playerOffset + 6] = playerOffset == 0 ? Pavise : Censer;
            roster[playerOffset + 7] = playerOffset == 0 ? Banner : Redoubt;
        }

        public static RosterEntry Maw => new RosterEntry { Archetype = VehicleArchetype.Maw, Cost = 350, Hp = 3600, Speed = 1.6f, CooldownTicks = 600, IsVehicle = true };
        public static RosterEntry Tusk => new RosterEntry { Archetype = VehicleArchetype.Tusk, Cost = 260, Hp = 2000, Speed = 2.4f, CooldownTicks = 450, IsVehicle = true };
        // the walkers scuttle: faster than either tank, and they do not care what the ground is like
        public static RosterEntry Pincer => new RosterEntry { Archetype = VehicleArchetype.Pincer, Cost = 320, Hp = 2300, Speed = 2.9f, CooldownTicks = 520, IsVehicle = true };
        public static RosterEntry Kettle => new RosterEntry { Archetype = VehicleArchetype.Kettle, Cost = 270, Hp = 1600, Speed = 2.3f, CooldownTicks = 560, IsVehicle = true };
        public static RosterEntry Pavise => new RosterEntry { Archetype = VehicleArchetype.Pavise, Cost = 360, Hp = 2100, Speed = 1.9f, CooldownTicks = 620, IsVehicle = true };
        public static RosterEntry Censer => new RosterEntry { Archetype = VehicleArchetype.Censer, Cost = 240, Hp = 1500, Speed = 2.6f, CooldownTicks = 500, IsVehicle = true };
        public static RosterEntry Banner => new RosterEntry { Archetype = VehicleArchetype.Banner, Cost = 400, Hp = 1900, Speed = 2.4f, CooldownTicks = 700, IsVehicle = true };
        public static RosterEntry Redoubt => new RosterEntry { Archetype = VehicleArchetype.Redoubt, Cost = 330, Hp = 3200, Speed = 1.7f, CooldownTicks = 600, IsVehicle = true };
    }

    /// <summary>Archetype ids of the four fighting vehicles (Resources/Vehicles/Maw, /Tusk, /Pincer, /Kettle).</summary>
    public static class VehicleArchetype
    {
        public const byte Maw = 4;    // heavy tank: two sponson 6-pdrs, crosses 3.5 m trenches, crushes wire, pushes trees over
        public const byte Tusk = 5;   // light tank: a turret 37 mm with a coaxial MG, quick, ditches in wide trenches
        // The owner's two crab machines (Tools/crabsplit.py). A walker has no tracks to break and no belly to ditch:
        // it steps over a trench and over wire, climbs what a tank cannot, and is stopped by losing its legs.
        public const byte Pincer = 6; // heavy walker: twin turret guns, two claws that crush what they reach, six legs
        public const byte Kettle = 7; // light walker: one mortar over its back that fires without seeing, four legs
        public const byte Censer = 8; // a drum of chlorine on its back, which it lays as it walks; quick, thin, unarmed
        public const byte Pavise = 9; // a long gun on a pintle behind a shield: it plants itself and reaches furthest
        public const byte Banner = 10; // command walker: a long gun and a standard that steadies the men round it
        public const byte Redoubt = 11;// a blockhouse on six legs: no gun at all, armour and claws, and hard to stop
        public static bool IsTank(byte archetype) => archetype == Maw || archetype == Tusk;
        public static bool IsWalker(byte archetype) => archetype >= Pincer && archetype <= Redoubt;
        /// <summary>Anything with armour, guns and modules: what VehicleModulesSystem and TankGunnerySystem run for.</summary>
        public static bool IsArmoured(byte archetype) => IsTank(archetype) || IsWalker(archetype);
    }
}
