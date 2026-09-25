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
        /// The units defined here rather than in the old switches. Empty until the historical rosters land, and the
        /// tests hold the property that matters while it is empty: applying it changes nothing.
        /// </summary>
        public static readonly UnitDef[] All = new UnitDef[0];

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
        }
    }
}
