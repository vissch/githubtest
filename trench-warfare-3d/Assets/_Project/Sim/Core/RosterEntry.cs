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

        public const int SlotCount = 10;   // 2026-09-25 (owner decision): every unit in a default roster, hotkeys 1..0

        /// <summary>The roster a player deploys from. Since 2026-09-25 this is the FACTION's table (FactionRoster):
        /// player offset 0 is Iron, today's player-0 side, anything else Brass. Kept as a forwarder so every
        /// caller that built a roster before factions existed still gets the same eight entries.</summary>
        public static void FillDefault(Unity.Collections.NativeArray<RosterEntry> roster, int playerOffset)
            => FactionRoster.Fill(roster, playerOffset, playerOffset == 0 ? FactionId.Iron : FactionId.Brass);

        /// <summary>The entry an archetype is fielded with, whatever slot it sits in (a salvage, a drop, a capture
        /// tool). Default for an id no roster knows.</summary>
        public static RosterEntry ForArchetype(byte archetype)
        {
            switch (archetype)
            {
                case InfantryArchetype.Rifle: return Rifleman;
                case InfantryArchetype.Assault: return Assault;
                case InfantryArchetype.Machinegunner: return Machinegunner;
                case InfantryArchetype.Sniper: return Sniper;
                case InfantryArchetype.Officer: return Officer;
                case InfantryArchetype.Shield: return Shield;
                case InfantryArchetype.Medic: return Medic;
                case InfantryArchetype.Repair: return Repair;
                case InfantryArchetype.Para: return Para;
                case InfantryArchetype.Jetpack: return Jetpack;
                case VehicleArchetype.Maw: return Maw;
                case VehicleArchetype.Tusk: return Tusk;
                case VehicleArchetype.Pincer: return Pincer;
                case VehicleArchetype.Kettle: return Kettle;
                case VehicleArchetype.Censer: return Censer;
                case VehicleArchetype.Pavise: return Pavise;
                case VehicleArchetype.Banner: return Banner;
                case VehicleArchetype.Redoubt: return Redoubt;
                case VehicleArchetype.Breaker: return Breaker;
                default: return default;
            }
        }

        // ---- infantry (the numbers the Phase 0 roster always had, and the 2026-09-25 units; docs/06) -----------
        public static RosterEntry Rifleman => new RosterEntry { Archetype = InfantryArchetype.Rifle, Cost = 25, Hp = 100, Speed = 3.0f };
        public static RosterEntry Assault => new RosterEntry { Archetype = InfantryArchetype.Assault, Cost = 40, Hp = 90, Speed = 4.2f };
        public static RosterEntry Machinegunner => new RosterEntry { Archetype = InfantryArchetype.Machinegunner, Cost = 60, Hp = 110, Speed = 2.2f };
        public static RosterEntry Sniper => new RosterEntry { Archetype = InfantryArchetype.Sniper, Cost = 90, Hp = 80, Speed = 3.0f, CooldownTicks = 200 };
        /// <summary>An aura over the men round him: they hit harder, keep their nerve and never stay pinned.</summary>
        public static RosterEntry Officer => new RosterEntry { Archetype = InfantryArchetype.Officer, Cost = 120, Hp = 100, Speed = 3.0f, CooldownTicks = 400 };
        /// <summary>Walks in front with a plate on his arm and takes the rounds meant for the men behind him.</summary>
        public static RosterEntry Shield => new RosterEntry { Archetype = InfantryArchetype.Shield, Cost = 70, Hp = 140, Speed = 3.2f };
        /// <summary>Patches the living, one at a time. Carries no rifle.</summary>
        public static RosterEntry Medic => new RosterEntry { Archetype = InfantryArchetype.Medic, Cost = 80, Hp = 90, Speed = 3.2f, CooldownTicks = 200 };
        /// <summary>Mends friendly machines he stands beside.</summary>
        public static RosterEntry Repair => new RosterEntry { Archetype = InfantryArchetype.Repair, Cost = 90, Hp = 100, Speed = 2.8f, CooldownTicks = 300 };
        /// <summary>Never a roster slot: eight of them come down where the ParaDrop ability is called.</summary>
        public static RosterEntry Para => new RosterEntry { Archetype = InfantryArchetype.Para, Cost = 0, Hp = 90, Speed = 3.4f };
        /// <summary>Leaps into an enemy trench from thirty metres out.</summary>
        public static RosterEntry Jetpack => new RosterEntry { Archetype = InfantryArchetype.Jetpack, Cost = 110, Hp = 85, Speed = 3.6f, CooldownTicks = 300 };
        /// <summary>The assault tank: halts short of a trench, winds up, charges it, and backs out to do it again.</summary>
        public static RosterEntry Breaker => new RosterEntry { Archetype = VehicleArchetype.Breaker, Cost = 380, Hp = 2800, Speed = 2.2f, CooldownTicks = 650, IsVehicle = true };

        /// <summary>Phase 0 placeholder roster: rifleman, assault, machinegunner, special (sniper), tank, walker.
        /// Player 0's tank is the Maw (heavy, sponson guns, the Mark IV of the roster), player 1's the Tusk (light,
        /// turret, the Renault FT). Each side then has two walkers: player 0 the Pincer (twin turret guns, six legs,
        /// two heavy claws) and the Pavise (a long gun behind a shield); player 1 the Kettle (a mortar that fires
        /// without seeing) and the Censer (a drum of chlorine it lays as it walks). Vehicle Hp is the hull's
        /// structure; armour, modules, legs and crew decide most fights (A5b).</summary>
        /// (That description is now FactionRoster.Slot; the Iron and Brass tables reproduce it.)

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

    /// <summary>Archetype ids of the infantry. An id is a byte the whole sim switches on, so these are the names;
    /// what an id can do is InfantrySpec.For(id) and what it shoots is CombatTables.WeaponFor(id). 4..11 are the
    /// vehicles (VehicleArchetype); the 2026-09-25 units start at 12 because IsWalker is a RANGE check over 6..11.</summary>
    public static class InfantryArchetype
    {
        public const byte Rifle = 0, Assault = 1, Machinegunner = 2, Sniper = 3;
        public const byte Officer = 12, Shield = 13, Medic = 14, Repair = 15, Para = 16, Jetpack = 17;
        /// <summary>TrenchOrders' roster mask is `1 << archetype` in an int: no id may reach 32.</summary>
        public const byte Max = 31;
        public static bool IsInfantry(byte archetype) => archetype <= Sniper || (archetype >= Officer && archetype <= Jetpack);
    }

    /// <summary>Archetype ids of the fighting vehicles (Resources/Vehicles/Maw, /Tusk, /Pincer ... /Breaker).</summary>
    public static class VehicleArchetype
    {
        public const byte Maw = 4;    // heavy tank: two sponson 6-pdrs, crosses 3.5 m trenches, crushes wire, pushes trees over
        public const byte Tusk = 5;   // light tank: a turret 37 mm with a coaxial MG, quick, ditches in wide trenches
        // The owner's two crab machines (Tools/crabsplit.py). A walker has no tracks to break and no belly to ditch:
        // it steps over a trench and over wire, climbs what a tank cannot, and is stopped by losing its legs.
        public const byte Pincer = 6; // heavy walker: two sponson guns, 105 deg either side of a rest yaw of +/-28,
                                      // so ~94 deg dead astern that neither reaches; two crushing claws, six legs
        public const byte Kettle = 7; // light walker: one mortar over its back that fires without seeing, four legs
        public const byte Censer = 8; // a drum of chlorine on its back, which it lays as it walks; quick, thin, unarmed
        public const byte Pavise = 9; // a long gun on a pintle behind a shield: it plants itself and reaches furthest
        public const byte Banner = 10; // command walker: a long gun and a standard that steadies the men round it
        public const byte Redoubt = 11;// a blockhouse on six legs: no gun at all, armour and claws, and hard to stop
        public const byte Breaker = 18;// assault tank: halts, winds up, charges the trench with its claws and hull guns, backs out
                                      // (TankGunnery.WeaponIdBase is 40 + id and SeaLanding.ShipSource is 60: no vehicle id past 19)
        public static bool IsTank(byte archetype) => archetype == Maw || archetype == Tusk || archetype == Breaker;
        /// <summary>A RANGE check over the six crabs: a new walker must be added here explicitly.</summary>
        public static bool IsWalker(byte archetype) => archetype >= Pincer && archetype <= Redoubt;
        /// <summary>Anything with armour, guns and modules: what VehicleModulesSystem and TankGunnerySystem run for.</summary>
        public static bool IsArmoured(byte archetype) => IsTank(archetype) || IsWalker(archetype);
    }
}
