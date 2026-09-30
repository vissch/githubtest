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

        /// <summary>
        /// What it stands on (ChassisKind): a man, tracks, legs or wheels. Read from this table rather than asked of
        /// the id, because the id ranges ran out and a range check called every new machine a tank.
        /// </summary>
        public byte Chassis;

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
        public static RosterEntry Machinegunner => new RosterEntry { Archetype = InfantryArchetype.Machinegunner, Cost = 80, Hp = 110, Speed = 2.2f };
        public static RosterEntry Sniper => new RosterEntry { Archetype = InfantryArchetype.Sniper, Cost = 140, Hp = 80, Speed = 3.0f, CooldownTicks = 200 };
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
        public static RosterEntry Breaker => new RosterEntry { Archetype = VehicleArchetype.Breaker, Cost = 380, Hp = 2800, Speed = 2.2f, CooldownTicks = 650, IsVehicle = true, Chassis = ChassisKind.Tracked };

        /// <summary>Phase 0 placeholder roster: rifleman, assault, machinegunner, special (sniper), tank, walker.
        /// Player 0's tank is the Maw (heavy, sponson guns, the Mark IV of the roster), player 1's the Tusk (light,
        /// turret, the Renault FT). Each side then has two walkers: player 0 the Pincer (twin turret guns, six legs,
        /// two heavy claws) and the Pavise (a long gun behind a shield); player 1 the Kettle (a mortar that fires
        /// without seeing) and the Censer (a drum of chlorine it lays as it walks). Vehicle Hp is the hull's
        /// structure; armour, modules, legs and crew decide most fights (A5b).</summary>
        /// (That description is now FactionRoster.Slot; the Iron and Brass tables reproduce it.)

        public static RosterEntry Maw => new RosterEntry { Archetype = VehicleArchetype.Maw, Cost = 350, Hp = 3600, Speed = 1.6f, CooldownTicks = 600, IsVehicle = true, Chassis = ChassisKind.Tracked };
        public static RosterEntry Tusk => new RosterEntry { Archetype = VehicleArchetype.Tusk, Cost = 260, Hp = 2000, Speed = 2.4f, CooldownTicks = 450, IsVehicle = true, Chassis = ChassisKind.Tracked };
        // the walkers scuttle: faster than either tank, and they do not care what the ground is like
        public static RosterEntry Pincer => new RosterEntry { Archetype = VehicleArchetype.Pincer, Cost = 320, Hp = 2300, Speed = 2.9f, CooldownTicks = 520, IsVehicle = true, Chassis = ChassisKind.Legged };
        public static RosterEntry Kettle => new RosterEntry { Archetype = VehicleArchetype.Kettle, Cost = 270, Hp = 1600, Speed = 2.3f, CooldownTicks = 560, IsVehicle = true, Chassis = ChassisKind.Legged };
        public static RosterEntry Pavise => new RosterEntry { Archetype = VehicleArchetype.Pavise, Cost = 360, Hp = 2100, Speed = 1.9f, CooldownTicks = 620, IsVehicle = true, Chassis = ChassisKind.Legged };
        public static RosterEntry Censer => new RosterEntry { Archetype = VehicleArchetype.Censer, Cost = 240, Hp = 1500, Speed = 2.6f, CooldownTicks = 500, IsVehicle = true, Chassis = ChassisKind.Legged };
        public static RosterEntry Banner => new RosterEntry { Archetype = VehicleArchetype.Banner, Cost = 400, Hp = 1900, Speed = 2.4f, CooldownTicks = 700, IsVehicle = true, Chassis = ChassisKind.Legged };
        public static RosterEntry Redoubt => new RosterEntry { Archetype = VehicleArchetype.Redoubt, Cost = 330, Hp = 3200, Speed = 1.7f, CooldownTicks = 600, IsVehicle = true, Chassis = ChassisKind.Legged };
    }

    /// <summary>Archetype ids of the infantry. An id is a byte the whole sim switches on, so these are the names;
    /// what an id can do is InfantrySpec.For(id) and what it shoots is CombatTables.WeaponFor(id). 4..11 are the
    /// vehicles (VehicleArchetype); the 2026-09-25 units start at 12 for no reason but history, since the walker
    /// range check that used to require it is gone (RosterEntry.Chassis).</summary>
    public static class InfantryArchetype
    {
        public const byte Rifle = 0, Assault = 1, Machinegunner = 2, Sniper = 3;
        public const byte Officer = 12, Shield = 13, Medic = 14, Repair = 15, Para = 16, Jetpack = 17;
        // The Proving Ground's men (2026-09-28): defined in Sim/Match/UnitDefinitions.cs, in no faction's slots or pool.
        public const byte Frog = 25;           // the playground's frog infantryman: a rifleman with his own figure
        public const byte Sentry = 26;         // a machine gunner behind a frontal plate (docs/06)
        public const byte AtRifle = 27;        // an anti-tank rifle: 20 mm at 70 m, hunts light machines
        public const byte DeathBattalion = 28; // the elite rifle: harder, dearer, never pinned
        public const byte Sapper = 35;         // lays mines and tripwires on a UnitAbility (decisions.md 2026-09-26)
        public const byte Flamethrower = 36;   // a cone of fire to 12 m whose hits set men and ground alight
        /// <summary>
        /// The highest id any unit may take. It was 31 because TrenchOrders masked with `1 << archetype`; that order
        /// now carries an OrderGroup mask instead, so what binds is Archetypes.Count — the length of every table an
        /// archetype indexes.
        /// </summary>
        public const byte Max = Archetypes.Max;
        public static bool IsInfantry(byte archetype) => archetype <= Sniper || (archetype >= Officer && archetype <= Jetpack)
            || archetype == Frog || (archetype >= Sentry && archetype <= DeathBattalion) || archetype == Sapper || archetype == Flamethrower;
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
                                      // Ids are no longer squeezed by the explosion source space: SourceId bands it, so a
                                      // machine may take any byte (2026-09-25). What still binds is InfantryArchetype.Max.
        // The two machines of 2026-09-28, defined in Sim/Match/UnitDefinitions.cs (not in the switches above, so
        // ForArchetype does not know them) and in no faction's slots or pool: the Unit Sandbox fields them.
        public const byte Skimmer = 19;// hovercraft: a machine gun in a small turret, a fan astern; skims over mud and trenches
        public const byte Salvo = 20;  // half-track rocket truck: a box of rockets that lands out of sight, long to reload
        // The Proving Ground's machines (2026-09-28), all definitions in Sim/Match/UnitDefinitions.cs and in no faction's
        // slots or pool. The first four are the asset playground's prototypes; the rest are docs/06's historical armour as
        // stand-ins that borrow a shipped model until they have their own.
        public const byte Brute = 21;  // the playground's heavy tank: sponson guns, the Maw's kind
        public const byte Croaker = 22;// a two-legged frog mech: claws and one turret gun
        public const byte Hopper = 23; // a frog gunship: the sim has no flight, so it strides everything and never bogs
        public const byte Mercy = 24;  // a field ambulance: unarmed, heals the men beside it
        public const byte MarkIV = 29; // Mark IV Male: the Maw's numbers and model
        public const byte MarkV = 30;  // Mark V: a Mark IV that handles mud
        public const byte A7V = 31;    // A7V: thick in front, one hull gun, cannot cross a wide trench
        public const byte RenaultFT = 32; // Renault FT: the Tusk's turret, a narrower trench
        public const byte Whippet = 33;// Whippet: quick, machine guns only
        public const byte Austin = 34; // Austin armoured car: wheels, two machine guns, ditches in anything
        public const byte Bullfrog = 37;// the playground's toad mech (2026-09-30): two gatlings over its back; it hops, so it
                                       // clears trenches and wire and has no legs to lose
        /// <summary>
        /// What kind of machine each SHIPPED id is. This is the seed of RosterEntry.Chassis and nothing else: the
        /// three predicates that used to live here (IsTank, IsWalker, IsArmoured, the last a range check over 6..11)
        /// are gone, because an id alone cannot answer the question once units come from definitions. Ask the match
        /// table: w.ChassisOf(archetype), or Roster[Archetype[i]].Chassis inside a job.
        /// </summary>
        public static byte ShippedChassis(byte archetype) => RosterEntry.ForArchetype(archetype).Chassis;
    }
}
