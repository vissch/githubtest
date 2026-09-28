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
        /// shipped before (UnitDefinitionTests). Since 2026-09-28 it holds the Skimmer and the Salvo, which no faction
        /// fields: they are in the match's table, so the Unit Sandbox (Editor/UnitSandbox.cs) can put them on the field,
        /// and in no roster, so the game the owner plays is the one it was.
        /// </summary>
        public static readonly UnitDef[] All = { Skimmer, Salvo };

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
            Roster = new RosterEntry { Archetype = VehicleArchetype.Skimmer, Cost = 220, Hp = 1300f, Speed = 3.4f, CooldownTicks = 420, IsVehicle = true, Chassis = ChassisKind.Tracked },
            Infantry = new InfantrySpec { Group = OrderGroup.Line },
            // the machine gun in its turret: fired by the small-arms systems (TargetAcquisition reads its range here)
            Weapon = new WeaponStats { Id = VehicleArchetype.Skimmer, Damage = 24f, RangeMax = 130f, RoundsPerSecond = 6f, Accuracy = 0.30f, SuppressionPerShot = 10f, PenetrationMm = 9f },
            Machine = new TankSpec
            {
                Hull = new ArmorProfile { FrontMm = 8f, SideMm = 6f, RearMm = 5f, TopMm = 5f },
                Turret = new ArmorProfile { FrontMm = 10f, SideMm = 8f, RearMm = 6f, TopMm = 5f },
                TurretChance = 0.2f, Crew = 2, GunCount = 0, ShortHalt = false, FuelRisk = 0.40f, AmmoRisk = 0.20f,
            },
            // footprint off the model (7.0 m across the pods at TW_SCALE 7): 6.6 m long, 6.9 m wide
            Drive = new VehicleProfile
            {
                TurnRateRad = 1.1f, TrenchCrossWidth = FlowFieldManager.TrackedCrossWidth, DitchChance = 0f, SlopeLimit = 0.5f,
                BogChance = 0f, HalfLength = 3.3f, HalfWidth = 3.45f, PushesTrees = false,
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
            Weapon = new WeaponStats { Id = VehicleArchetype.Salvo },   // no small arms: RangeMax 0 acquires nothing
            Machine = new TankSpec
            {
                Hull = new ArmorProfile { FrontMm = 10f, SideMm = 8f, RearMm = 6f, TopMm = 6f },
                Turret = new ArmorProfile { FrontMm = 8f, SideMm = 6f, RearMm = 6f, TopMm = 5f },
                TurretChance = 0.35f, Crew = 3, GunCount = 1, ShortHalt = true, FuelRisk = 0.30f, AmmoRisk = 0.70f,
                // the shell below is the whole rack: twelve rockets, each landing on its own tick (TankGunnerySystem)
                Rockets = 12, RocketSpeed = 140f,
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
                HalfLength = 4.0f, HalfWidth = 3.1f, PushesTrees = false,
            },
        };

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
