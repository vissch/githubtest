// Phase: A5c (implemented 2026-09-25) — the one place a unit is defined.
//
// A unit used to be five edits in five files: a RosterEntry static and a case in ForArchetype, a case in
// CombatTables.WeaponFor, one in InfantrySpec.For, one in TankSpec.For, one in VehicleProfile.ForArchetype. Each of
// those switches has a silent default — a missing id fell through to a rifleman weapon and a Maw hull — so a unit
// could be half-added and look fine until it drove like the wrong machine. Four historical armies made that untenable.
//
// The tables the sim runs on are now UnitCatalogue (Core), CombatCatalogueSystem (Combat) and
// VehicleKinematicsSystem.Profiles (Nav), all indexed by archetype. This file writes into all three from one place, so
// a new unit is one entry here, and MatchSim applies it after the systems are registered and re-seals the
// fingerprints. It lives in Match because that is the only assembly that can see Core, Combat and Nav at once — the
// same reason TW.Data can bake into these tables later without the sim ever referencing TW.Data.
//
// The units that already shipped keep their numbers where they already are: this applies nothing over them, so no
// balance moves and nothing is transcribed. Everything added from 2026-09-26 on is defined here.
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Combat;
using TW.Sim.Nav;

namespace TW.Sim.Match
{
    /// <summary>Everything about one unit, in one place. Zero fields are simply not used by that kind of unit.</summary>
    public struct UnitDef
    {
        public byte Archetype;
        public RosterEntry Roster;       // cost, hit points, speed, redeploy cooldown, vehicle flag
        public InfantrySpec Infantry;    // what he can do besides shoot, and which order group he answers to
        public WeaponStats Weapon;       // what he shoots with (RangeMax 0 = unarmed)
        public TankSpec Machine;         // hull, guns, claws, cycle — only for a machine
        public VehicleProfile Drive;     // how it drives — only for a machine
    }

    public static class UnitDefinitions
    {
        /// <summary>
        /// The units defined here rather than in the old switches. Applying them changes nothing about the units that
        /// shipped before (UnitDefinitionTests). Since 2026-09-28 it holds the Skimmer and the Salvo, and then the
        /// Playground's five (the Brute, Croaker, Mercy, Hopper and the Frog; owner: "all", spawn-only), which no faction
        /// fields: they are in the match's table, so the Unit Sandbox (Editor/UnitSandbox.cs) can put them on the field,
        /// and in no roster, so the game the owner plays is the one it was.
        /// </summary>
        public static readonly UnitDef[] All = { Skimmer, Salvo, Brute, Croaker, Mercy, Hopper, Frog };

        const float Deg = TankSpec.Deg;

        /// <summary>
        /// The Skimmer (2026-09-28, the owner's hovercraft: Resources/Vehicles/Skimmer, Tools/mechsplit.py TW_KIND=hover).
        /// A machine gun in a small turret and a fan astern: the quickest machine on the field and the thinnest. It rides
        /// its air cushion over mud and across any trench a heavy tank can bridge without ditching, but it cannot push a
        /// tree over and a 37 mm goes through it anywhere. It drives as a tracked machine (ChassisKind.Tracked): its
        /// "tracks" are the skirts the modules system can break, which stops it where it is (decisions.md 2026-09-28).
        /// </summary>
        public static UnitDef Skimmer => new UnitDef
        {
            Archetype = VehicleArchetype.Skimmer,
            Roster = new RosterEntry { Archetype = VehicleArchetype.Skimmer, Cost = 180, Hp = 1300f, Speed = 3.4f, CooldownTicks = 420, IsVehicle = true, Chassis = ChassisKind.Tracked },
            // it hunts light machines with its machine gun (12 mm beats a Salvo's or a Skimmer's side and rear, never a Maw's
            // front) and looks down into a trench it drives up to, as a charging Breaker does (the balance critic, 2026-09-28)
            Infantry = new InfantrySpec { Group = OrderGroup.Line, HuntsArmour = true, LooksDownMetres = CombatTables.ChargeRevealRange },
            // the machine gun in its turret: fired by the small-arms systems (TargetAcquisition reads its range here)
            Weapon = new WeaponStats { Id = VehicleArchetype.Skimmer, Damage = 24f, RangeMax = 130f, RoundsPerSecond = 6f, Accuracy = 0.30f, SuppressionPerShot = 10f, PenetrationMm = 12f },
            Machine = new TankSpec
            {
                Hull = new ArmorProfile { FrontMm = 8f, SideMm = 6f, RearMm = 5f, TopMm = 5f },
                Turret = new ArmorProfile { FrontMm = 10f, SideMm = 8f, RearMm = 6f, TopMm = 5f },
                TurretChance = 0.2f, Crew = 2, GunCount = 0, ShortHalt = false, FuelRisk = 0.40f, AmmoRisk = 0.20f,
                StandOffMetres = 90f, StandOffPatience = 20f,   // shoots from out of grenade range; gives up a mark it is not hurting
            },
            // footprint off the model (7.0 m across the pods at TW_SCALE 7): 6.6 m long, 6.9 m wide
            Drive = new VehicleProfile
            {
                TurnRateRad = 1.1f, TrenchCrossWidth = FlowFieldManager.TrackedCrossWidth, DitchChance = 0f, SlopeLimit = 0.5f,
                BogChance = 0f, HalfLength = 3.3f, HalfWidth = 3.45f, PushesTrees = false,
                Clearance = 1.2f,   // the Maw is drawn 1.2 m wider than its footprint: the Skimmer sat inside its track
            },
        };

        /// <summary>
        /// The Salvo (2026-09-28, the owner's half-track rocket truck: Resources/Vehicles/Salvo, Tools/mechsplit.py
        /// TW_KIND=halftrack). Wheels in front, tracks behind, two clawed legs braced at the tail and a box of rockets on
        /// a turntable. The box is its one gun: indirect, so it needs no line of sight, blind inside 60 m, and it throws
        /// the heaviest burst on the field and then takes sixteen seconds to load again. Thin, slow, and full of rockets
        /// that go up when it is holed.
        /// </summary>
        public static UnitDef Salvo => new UnitDef
        {
            Archetype = VehicleArchetype.Salvo,
            Roster = new RosterEntry { Archetype = VehicleArchetype.Salvo, Cost = 380, Hp = 2000f, Speed = 1.8f, CooldownTicks = 700, IsVehicle = true, Chassis = ChassisKind.Tracked },
            Infantry = new InfantrySpec { Group = OrderGroup.Line },
            // a hull machine gun, the Tusk's: it covers the 60 m the rockets cannot reach (no armour-piercing: men only)
            Weapon = new WeaponStats { Id = VehicleArchetype.Salvo, Damage = 24f, RangeMax = 110f, RoundsPerSecond = 4f, Accuracy = 0.30f, SuppressionPerShot = 9f, PenetrationMm = 9f },
            Machine = new TankSpec
            {
                Hull = new ArmorProfile { FrontMm = 10f, SideMm = 8f, RearMm = 6f, TopMm = 6f },
                Turret = new ArmorProfile { FrontMm = 8f, SideMm = 6f, RearMm = 6f, TopMm = 5f },
                TurretChance = 0.35f, Crew = 3, GunCount = 1, ShortHalt = true, FuelRisk = 0.30f, AmmoRisk = 0.70f,
                // the shell below is the whole rack: a rocket from each of its sixteen tubes, each landing on its own
                // tick (TankGunnerySystem); it holds where it is while it has a target, so it fires from its reach
                Rockets = 16, RocketSpeed = 140f, StandOffMetres = 380f, StandOffPatience = 32f,   // two reloads without a hit: move on
                Gun0 = new TankGun
                {
                    Mount = TankMount.Turret, RestYaw = 0f, ArcHalf = SimMath.Pi, TraverseRate = 20f * Deg,
                    Indirect = true, RangeMin = 60f, RangeMax = 380f, Accuracy = 0.22f, ReloadSeconds = 16f,
                    PenMm = 0f, ApDamage = 0f,
                    HeDamage = 360f, HeRadius = 7.0f, HeSuppression = 90f, HeCrater = 1.6f, Mount3 = new float3(0f, 4.8f, 0.3f),
                },
            },
            // footprint off the model (8.0 m long at TW_SCALE 8): 6.25 m wide
            Drive = new VehicleProfile
            {
                TurnRateRad = 0.5f, TrenchCrossWidth = 2.6f, DitchChance = 0.6f, SlopeLimit = 0.5f, BogChance = 0.035f,
                HalfLength = 4.0f, HalfWidth = 3.1f, PushesTrees = false, Clearance = 1.2f,
            },
        };

        // ---- the Playground's units (2026-09-28). Their models are the Playground's (Playground/Art/Tanks/<Name>,
        // Units/Frog) at the Playground's scale, as the Skimmer's was; footprints off those meshes (Blender bounds of
        // LOD2, width x length). The numbers are the agent's, placed beside the shipped machines they sit between.

        /// <summary>The Brute (Playground/Art/Tanks/Brute, Tools/tank3split.py): a medium tank, 3.9 x 6.3 m, between the
        /// Tusk and the Maw: a 57 mm in its turret, a machine gun in the hull, thicker than the Tusk and quicker than the
        /// Maw, and it pushes a tree over.</summary>
        public static UnitDef Brute => new UnitDef
        {
            Archetype = VehicleArchetype.Brute,
            Roster = new RosterEntry { Archetype = VehicleArchetype.Brute, Cost = 300, Hp = 2600f, Speed = 2.1f, CooldownTicks = 480, IsVehicle = true, Chassis = ChassisKind.Tracked },
            Infantry = new InfantrySpec { Group = OrderGroup.Line },
            Weapon = new WeaponStats { Id = VehicleArchetype.Brute, Damage = 24f, RangeMax = 110f, RoundsPerSecond = 4f, Accuracy = 0.30f, SuppressionPerShot = 9f, PenetrationMm = 9f },
            Machine = new TankSpec
            {
                Hull = new ArmorProfile { FrontMm = 22f, SideMm = 16f, RearMm = 12f, TopMm = 8f },
                Turret = new ArmorProfile { FrontMm = 24f, SideMm = 18f, RearMm = 14f, TopMm = 8f },
                TurretChance = 0.4f, Crew = 4, GunCount = 1, ShortHalt = true, FuelRisk = 0.3f, AmmoRisk = 0.4f,
                Gun0 = new TankGun
                {
                    Mount = TankMount.Turret, RestYaw = 0f, ArcHalf = SimMath.Pi, TraverseRate = 40f * Deg,
                    RangeMax = 230f, Accuracy = 0.50f, ReloadSeconds = 3.2f, PenMm = 38f, ApDamage = 560f,
                    HeDamage = 180f, HeRadius = 3.5f, HeSuppression = 38f, HeCrater = 0.8f, Mount3 = new float3(0f, 3.6f, 1.0f),
                },
            },
            Drive = new VehicleProfile
            {
                TurnRateRad = 0.55f, TrenchCrossWidth = 2.8f, DitchChance = 0.5f, SlopeLimit = 0.55f, BogChance = 0.04f,
                HalfLength = 3.17f, HalfWidth = 1.96f, PushesTrees = true,
            },
        };

        /// <summary>The Croaker (Playground/Art/Tanks/Croaker, Tools/mechsplit.py): a frog mech on two legs, a 37 mm in the
        /// turret on its back and two claws. It walks where a tank would ditch and bog, as the crabs do; two legs, so
        /// losing one stops it. 4.6 m long and 6.5 m across its arms: the footprint is the whole model, arms and all, as the
        /// battle's model test holds every drawn part to the footprint.</summary>
        public static UnitDef Croaker => new UnitDef
        {
            Archetype = VehicleArchetype.Croaker,
            Roster = new RosterEntry { Archetype = VehicleArchetype.Croaker, Cost = 290, Hp = 1900f, Speed = 2.6f, CooldownTicks = 520, IsVehicle = true, Chassis = ChassisKind.Legged },
            Infantry = new InfantrySpec { Group = OrderGroup.Line },
            Machine = new TankSpec
            {
                Hull = new ArmorProfile { FrontMm = 14f, SideMm = 11f, RearMm = 9f, TopMm = 10f },
                Turret = new ArmorProfile { FrontMm = 16f, SideMm = 12f, RearMm = 10f, TopMm = 8f },
                TurretChance = 0.35f, Crew = 2, GunCount = 1, ShortHalt = true, FuelRisk = 0.2f, AmmoRisk = 0.35f,
                ClawReach = 2.4f, ClawDamage = 400f, ClawSeconds = 3.0f,
                Gun0 = new TankGun
                {
                    Mount = TankMount.Turret, RestYaw = 0f, ArcHalf = SimMath.Pi, TraverseRate = 45f * Deg,
                    RangeMax = 210f, Accuracy = 0.50f, ReloadSeconds = 2.4f, PenMm = 28f, ApDamage = 400f,
                    HeDamage = 120f, HeRadius = 2.5f, HeSuppression = 28f, HeCrater = 0f, Mount3 = new float3(0f, 4.6f, 0.4f),
                },
            },
            Drive = new VehicleProfile
            {
                TurnRateRad = 1.2f, TrenchCrossWidth = 3.0f, DitchChance = 0f, SlopeLimit = 0.85f, BogChance = 0.015f,
                HalfLength = 2.3f, HalfWidth = 3.2f, Walker = true, Legs = 2,
            },
        };

        /// <summary>The Mercy (Playground/Art/Tanks/Mercy, Tools/jeepsplit.py): a field ambulance, 3.2 x 4.6 m. Unarmed; it
        /// patches the nearest wounded man of its side within 14 m as a medic does (SupportSystem), quicker than one. It
        /// drives as a tracked machine (ChassisKind.Wheeled has no rules yet, as with the Skimmer): thin, and it ditches.</summary>
        public static UnitDef Mercy => new UnitDef
        {
            Archetype = VehicleArchetype.Mercy,
            Roster = new RosterEntry { Archetype = VehicleArchetype.Mercy, Cost = 150, Hp = 1000f, Speed = 3.0f, CooldownTicks = 400, IsVehicle = true, Chassis = ChassisKind.Tracked },
            Infantry = new InfantrySpec { Group = OrderGroup.Line, HealRadius = 14f, HealPerSecond = 30f },
            Machine = new TankSpec
            {
                Hull = new ArmorProfile { FrontMm = 8f, SideMm = 6f, RearMm = 5f, TopMm = 5f },
                Turret = new ArmorProfile { FrontMm = 8f, SideMm = 6f, RearMm = 5f, TopMm = 5f },
                TurretChance = 0f, Crew = 2, GunCount = 0, ShortHalt = false, FuelRisk = 0.35f, AmmoRisk = 0f,
            },
            Drive = new VehicleProfile
            {
                TurnRateRad = 0.9f, TrenchCrossWidth = 1.8f, DitchChance = 0.8f, SlopeLimit = 0.5f, BogChance = 0.05f,
                HalfLength = 2.3f, HalfWidth = 1.58f, PushesTrees = false,
            },
        };

        /// <summary>The Hopper (Playground/Art/Tanks/Hopper, Tools/mechsplit.py TW_KIND=flyer): a frog gunship on two ducted
        /// engines, an autocannon in its chin turret and a machine gun on each engine. The sim has no flying unit (the
        /// owner's call): it moves as a machine that never ditches or bogs and crosses any trench, and is DRAWN in the air
        /// (presentation only, as the Skimmer is drawn hovering). 8.0 m long, 7.3 m across the wings.</summary>
        public static UnitDef Hopper => new UnitDef
        {
            Archetype = VehicleArchetype.Hopper,
            Roster = new RosterEntry { Archetype = VehicleArchetype.Hopper, Cost = 360, Hp = 1400f, Speed = 3.8f, CooldownTicks = 600, IsVehicle = true, Chassis = ChassisKind.Tracked },
            Infantry = new InfantrySpec { Group = OrderGroup.Line, HuntsArmour = true, LooksDownMetres = CombatTables.ChargeRevealRange },
            Weapon = new WeaponStats { Id = VehicleArchetype.Hopper, Damage = 24f, RangeMax = 140f, RoundsPerSecond = 8f, Accuracy = 0.30f, SuppressionPerShot = 12f, PenetrationMm = 10f },
            Machine = new TankSpec
            {
                Hull = new ArmorProfile { FrontMm = 8f, SideMm = 6f, RearMm = 5f, TopMm = 4f },
                Turret = new ArmorProfile { FrontMm = 8f, SideMm = 6f, RearMm = 5f, TopMm = 4f },
                TurretChance = 0.25f, Crew = 2, GunCount = 1, ShortHalt = false, FuelRisk = 0.45f, AmmoRisk = 0.3f,
                StandOffMetres = 120f, StandOffPatience = 20f,
                Gun0 = new TankGun
                {
                    Mount = TankMount.Turret, RestYaw = 0f, ArcHalf = SimMath.Pi, TraverseRate = 70f * Deg,
                    RangeMax = 200f, Accuracy = 0.45f, ReloadSeconds = 1.2f, PenMm = 20f, ApDamage = 260f,
                    HeDamage = 90f, HeRadius = 2.0f, HeSuppression = 25f, HeCrater = 0f, Mount3 = new float3(0f, 2.0f, 2.5f),
                },
            },
            Drive = new VehicleProfile
            {
                TurnRateRad = 1.3f, TrenchCrossWidth = FlowFieldManager.TrackedCrossWidth, DitchChance = 0f, SlopeLimit = 0.95f,
                BogChance = 0f, HalfLength = 3.97f, HalfWidth = 3.66f, PushesTrees = false, Clearance = 1.2f,
            },
        };

        /// <summary>The Frog (Playground/Art/Units/Frog, Tools/frogrig.py): the Playground's frog rifleman. Its own id so
        /// it is drawn as a frog (a VAT figure of its own) and the riflemen stay men; everything else is a rifleman's.</summary>
        public static UnitDef Frog
        {
            get
            {
                var roster = RosterEntry.Rifleman; roster.Archetype = InfantryArchetype.Frog;
                return new UnitDef
                {
                    Archetype = InfantryArchetype.Frog,
                    Roster = roster,
                    Infantry = InfantrySpec.For(InfantryArchetype.Rifle),
                    Weapon = CombatTables.WeaponFor(InfantryArchetype.Rifle),
                };
            }
        }

        /// <summary>
        /// Write every definition into the tables of a world and re-seal the fingerprints. Called by MatchSim once the
        /// systems are registered, and safe to call twice: it is a write of fixed values, not an accumulation.
        /// </summary>
        public static void Apply(SimWorld world) => Apply(world, All);

        /// <summary>The same, for a set of definitions a test or a bake supplies.</summary>
        public static void Apply(SimWorld world, UnitDef[] defs)
        {
            var combat = world.GetSystem<CombatCatalogueSystem>();
            var kinematics = world.GetSystem<VehicleKinematicsSystem>();
            for (int k = 0; k < defs.Length; k++)
            {
                var d = defs[k];
                if (d.Archetype >= Archetypes.Count) continue;   // a unit past the table cannot be fielded; Archetypes.Count is the gate
                world.Units.Roster[d.Archetype] = d.Roster;
                world.Units.Infantry[d.Archetype] = d.Infantry;
                if (combat != null)
                {
                    combat.Weapon[d.Archetype] = d.Weapon;
                    combat.Tank[d.Archetype] = d.Machine;
                }
                if (kinematics != null && kinematics.Profiles.IsCreated) kinematics.Profiles[d.Archetype] = d.Drive;
            }
            world.Units.Seal();
            combat?.Seal();
            // the slots were filled from the table as it stood when the world was built; a definition may have just
            // changed what one of the chosen archetypes costs or how much it can take
            world.FillRosters();
        }
    }
}
