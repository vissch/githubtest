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
        public static readonly UnitDef[] All =
        {
            Skimmer, Salvo,
            // the Proving Ground (2026-09-28): in id order
            Brute, Croaker, Hopper, Mercy, Frog, Sentry, AtRifle, DeathBattalion,
            MarkIV, MarkV, A7V, RenaultFT, Whippet, Austin, Sapper, Flamethrower,
            Bullfrog,   // 2026-09-30: the playground's toad mech
        };

        /// <summary>The ids the Proving Ground added (2026-09-28), for the tests and the tools that list them.</summary>
        public static readonly byte[] ProvingGround =
        {
            VehicleArchetype.Brute, VehicleArchetype.Croaker, VehicleArchetype.Hopper, VehicleArchetype.Mercy,
            InfantryArchetype.Frog, InfantryArchetype.Sentry, InfantryArchetype.AtRifle, InfantryArchetype.DeathBattalion,
            VehicleArchetype.MarkIV, VehicleArchetype.MarkV, VehicleArchetype.A7V, VehicleArchetype.RenaultFT,
            VehicleArchetype.Whippet, VehicleArchetype.Austin, InfantryArchetype.Sapper, InfantryArchetype.Flamethrower,
        };

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
            Weapon = new WeaponStats { Id = VehicleArchetype.Skimmer, Damage = 24f, RangeMax = 130f * CombatTables.RangeScale, RoundsPerSecond = 6f, Accuracy = 0.30f, SuppressionPerShot = 10f, PenetrationMm = 12f },
            Machine = new TankSpec
            {
                Hull = new ArmorProfile { FrontMm = 8f, SideMm = 6f, RearMm = 5f, TopMm = 5f },
                Turret = new ArmorProfile { FrontMm = 10f, SideMm = 8f, RearMm = 6f, TopMm = 5f },
                TurretChance = 0.2f, Crew = 2, GunCount = 0, ShortHalt = false, FuelRisk = 0.40f, AmmoRisk = 0.20f,
                StandOffMetres = 90f * CombatTables.RangeScale, StandOffPatience = 20f,   // shoots from out of grenade range; gives up a mark it is not hurting
            },
            // footprint off the model (7.0 m across the pods at TW_SCALE 7): 6.6 m long, 6.9 m wide
            Drive = new VehicleProfile
            {
                TurnRateRad = 1.1f, Accel = 1.8f, Brake = 1.1f, PivotSpeed = 0.6f, TrenchCrossWidth = FlowFieldManager.TrackedCrossWidth, DitchChance = 0f, SlopeLimit = 0.5f,
                BogChance = 0f, HalfLength = 3.3f, HalfWidth = 3.45f, PushesTrees = false,
                Clearance = 1.2f,   // the Maw is drawn 1.2 m wider than its footprint: the Skimmer sat inside its track
            },
        };

        /// <summary>
        /// The Salvo (2026-09-28, the owner's half-track rocket truck: Resources/Vehicles/Salvo, Tools/mechsplit.py
        /// TW_KIND=halftrack). Wheels in front, tracks behind, two clawed legs braced at the tail and a box of rockets on
        /// a turntable. The box is its one gun: indirect, so it needs no line of sight, blind inside 48 m (60 as designed, x RangeScale), and it throws
        /// the heaviest burst on the field and then takes sixteen seconds to load again. Thin, slow, and full of rockets
        /// that go up when it is holed.
        /// </summary>
        public static UnitDef Salvo => new UnitDef
        {
            Archetype = VehicleArchetype.Salvo,
            Roster = new RosterEntry { Archetype = VehicleArchetype.Salvo, Cost = 380, Hp = 2000f, Speed = 1.8f, CooldownTicks = 700, IsVehicle = true, Chassis = ChassisKind.Tracked },
            Infantry = new InfantrySpec { Group = OrderGroup.Line },
            // a hull machine gun, the Tusk's: it covers the 60 m the rockets cannot reach (no armour-piercing: men only)
            Weapon = new WeaponStats { Id = VehicleArchetype.Salvo, Damage = 24f, RangeMax = 110f * CombatTables.RangeScale, RoundsPerSecond = 4f, Accuracy = 0.30f, SuppressionPerShot = 9f, PenetrationMm = 9f },
            Machine = new TankSpec
            {
                Hull = new ArmorProfile { FrontMm = 10f, SideMm = 8f, RearMm = 6f, TopMm = 6f },
                Turret = new ArmorProfile { FrontMm = 8f, SideMm = 6f, RearMm = 6f, TopMm = 5f },
                TurretChance = 0.35f, Crew = 3, GunCount = 1, ShortHalt = true, FuelRisk = 0.30f, AmmoRisk = 0.70f,
                // the shell below is the whole rack: a rocket from each of its sixteen tubes, each landing on its own
                // tick (TankGunnerySystem); it holds where it is while it has a target, so it fires from its reach
                Rockets = 16, RocketSpeed = 140f, StandOffMetres = 155f, StandOffPatience = 32f,   // two reloads without a hit: move on
                Gun0 = new TankGun
                {
                    Mount = TankMount.Turret, RestYaw = 0f, ArcHalf = SimMath.Pi, TraverseRate = 20f * Deg,
                    Indirect = true, RangeMin = 60f * CombatTables.RangeScale, RangeMax = 155f, Accuracy = 0.22f, ReloadSeconds = 16f,
                    PenMm = 0f, ApDamage = 0f,
                    HeDamage = 360f, HeRadius = 7.0f, HeSuppression = 90f, HeCrater = 1.6f, Mount3 = new float3(0f, 4.8f, 0.3f),
                },
            },
            // footprint off the model (8.0 m long at TW_SCALE 8): 6.25 m wide
            Drive = new VehicleProfile
            {
                TurnRateRad = 0.5f, Accel = 0.7f, Brake = 1.4f, PivotSpeed = 0.06f, TrenchCrossWidth = 2.6f, DitchChance = 0.6f, SlopeLimit = 0.5f, BogChance = 0.035f,
                HalfLength = 4.0f, HalfWidth = 3.1f, PushesTrees = false, Clearance = 1.2f,
            },
        };

        // ==== The Proving Ground's units (2026-09-28, replay v16) ==================================================
        // Every one of these is fielded by no faction: the Proving Ground level (and the Unit Sandbox) puts them on the
        // field, and the game the owner plays is the one it was. Four are the asset playground's prototypes (docs/22),
        // given the sim's nearest shape until their own systems exist; the rest are docs/06's ideas as stand-ins on the
        // specs and weapons the sim already has, borrowing a shipped model until they have their own (the SHOW lane's
        // TankRenderer.Machines names the model). Numbers are docs/06's, scaled the way RosterEntry scaled them (a
        // rifleman 100 hp, the Maw 3,600), and are starting points for the playtests this level exists for.

        static RosterEntry Man(byte id, int cost, float hp, float speed, int cooldown = 0)
            => new RosterEntry { Archetype = id, Cost = cost, Hp = hp, Speed = speed, CooldownTicks = cooldown, Chassis = ChassisKind.Foot };
        static RosterEntry Machine(byte id, int cost, float hp, float speed, int cooldown, byte chassis)
            => new RosterEntry { Archetype = id, Cost = cost, Hp = hp, Speed = speed, CooldownTicks = cooldown, IsVehicle = true, Chassis = chassis };
        /// <summary>A shipped weapon under a new id: the table keys a shooter's rounds by the id it carries.</summary>
        static WeaponStats Borrowed(byte from, byte id) { var w = CombatTables.WeaponFor(from); w.Id = id; return w; }
        /// <summary>A hull machine gun (the Maw's numbers) under a new id; range as designed (RangeScale is applied here).</summary>
        static WeaponStats MachineGun(byte id, float range = 120f, float roundsPerSecond = 5f)
            => new WeaponStats { Id = id, Damage = 24f, RangeMax = range * CombatTables.RangeScale, RoundsPerSecond = roundsPerSecond, Accuracy = 0.30f, SuppressionPerShot = 10f, PenetrationMm = 9f };
        static ArmorProfile Plate(float front, float side, float rear, float top) => new ArmorProfile { FrontMm = front, SideMm = side, RearMm = rear, TopMm = top };

        // ---- the playground's prototypes ----

        /// <summary>Brute (21): the playground's first tank, a heavy of the Maw's weight under thicker plate, and slower. Its
        /// model (Resources/Vehicles/Brute, drawn 1.35 times its sculpt: 8.8 m long, 5.3 m wide) carries ONE gun, in the
        /// front of the hull under a small turret, so it fires the Maw's six-pounder from a hull mount over a narrow arc
        /// and has to turn to bring it on; the footprint is the model's (2026-09-28: it was given the Maw's two sponsons
        /// and a footprint 3.4 m half-wide before the model was in the battle to be measured).</summary>
        public static UnitDef Brute
        {
            get
            {
                var m = TankSpec.Maw; m.Hull = Plate(20f, 14f, 10f, 8f); m.Turret = m.Hull; m.Crew = 5;
                var g = m.Gun0; g.Mount = TankMount.Hull; g.RestYaw = 0f; g.ArcHalf = 25f * Deg; g.Mount3 = new float3(0f, 3.9f, 0.8f);
                m.Gun0 = g; m.Gun1 = default; m.GunCount = 1;
                var d = VehicleProfile.Maw; d.TurnRateRad = 0.40f; d.HalfLength = 4.4f; d.HalfWidth = 2.7f;
                d.Accel = 0.9f; d.Brake = 2.0f; d.PivotSpeed = 0.08f;   // heavier than it is quick: slow to get going, a stop to pivot
                return new UnitDef
                {
                    Archetype = VehicleArchetype.Brute,
                    Roster = Machine(VehicleArchetype.Brute, 420, 4200f, 1.5f, 700, ChassisKind.Tracked),
                    Infantry = new InfantrySpec { Group = OrderGroup.Line },
                    Weapon = MachineGun(VehicleArchetype.Brute),
                    Machine = m, Drive = d,
                };
            }
        }

        /// <summary>Croaker (22): a two-legged frog mech, the playground's walker. One turret gun, two claws, and two legs
        /// to lose: it limps on one and stops on none. The gait is the crabs' with a knee that bends forward (SHOW).</summary>
        public static UnitDef Croaker => new UnitDef
        {
            Archetype = VehicleArchetype.Croaker,
            Roster = Machine(VehicleArchetype.Croaker, 300, 1800f, 3.0f, 520, ChassisKind.Legged),
            Infantry = new InfantrySpec { Group = OrderGroup.Line },
            Weapon = new WeaponStats { Id = VehicleArchetype.Croaker },   // RangeMax 0: every gun it has is TankGunnery's
            Machine = new TankSpec
            {
                Hull = Plate(16f, 12f, 10f, 10f), Turret = Plate(14f, 12f, 10f, 8f),
                TurretChance = 0.25f, Crew = 2, GunCount = 1, ShortHalt = false, FuelRisk = 0.15f, AmmoRisk = 0.40f,
                Unmanned = true, ClawReach = 2.4f, ClawDamage = 400f, ClawSeconds = 3.0f,
                Gun0 = new TankGun
                {
                    Mount = TankMount.Turret, RestYaw = 0f, ArcHalf = 105f * Deg, TraverseRate = 60f * Deg,
                    RangeMax = 110f, Accuracy = 0.5f, ReloadSeconds = 3.0f, PenMm = 34f, ApDamage = 500f,
                    HeDamage = 150f, HeRadius = 3.2f, HeSuppression = 35f, HeCrater = 0.8f, Mount3 = new float3(0f, 4.2f, 0.6f),
                },
            },
            Drive = new VehicleProfile
            {
                TurnRateRad = 1.2f, Accel = 1.9f, Brake = 2.8f, PivotSpeed = 0.5f, TrenchCrossWidth = 3.0f, DitchChance = 0f, SlopeLimit = 0.9f, BogChance = 0.010f,
                HalfLength = 2.3f, HalfWidth = 3.3f, PushesTrees = true, Walker = true, Legs = 2,
            },
        };

        /// <summary>Hopper (23): a frog gunship on two ducted engines. The sim has no flight, so it is the machine the sim
        /// can carry closest to one: a walker profile with no legs to lose, so it strides every trench and wire belt, never
        /// bogs or ditches, and pushes nothing over. Thin, quick, a machine gun and a light HE gun that reaches all round.
        /// Drawn hovering high (SHOW); the sim shoots and is shot at ground level, which is the stand-in's limit.</summary>
        public static UnitDef Hopper => new UnitDef
        {
            Archetype = VehicleArchetype.Hopper,
            Roster = Machine(VehicleArchetype.Hopper, 260, 900f, 4.5f, 450, ChassisKind.Tracked),
            Infantry = new InfantrySpec { Group = OrderGroup.Line },
            Weapon = MachineGun(VehicleArchetype.Hopper, 130f, 6f),
            Machine = new TankSpec
            {
                Hull = Plate(6f, 5f, 4f, 4f), Turret = Plate(6f, 5f, 4f, 4f),
                TurretChance = 0f, Crew = 2, GunCount = 1, ShortHalt = false, FuelRisk = 0.50f, AmmoRisk = 0.30f,
                Gun0 = new TankGun
                {
                    Mount = TankMount.Turret, RestYaw = 0f, ArcHalf = SimMath.Pi, TraverseRate = 90f * Deg,
                    RangeMax = 110f, Accuracy = 0.40f, ReloadSeconds = 1.5f, PenMm = 0f, ApDamage = 0f,
                    HeDamage = 90f, HeRadius = 2.5f, HeSuppression = 25f, HeCrater = 0.3f, Mount3 = new float3(0f, 3.0f, 1.0f),
                },
            },
            Drive = new VehicleProfile
            {
                TurnRateRad = 1.3f, Accel = 2.0f, Brake = 1.4f, PivotSpeed = 0.7f, TrenchCrossWidth = FlowFieldManager.TrackedCrossWidth, DitchChance = 0f, SlopeLimit = 1f,
                BogChance = 0f, HalfLength = 4.0f, HalfWidth = 3.5f, PushesTrees = false, Walker = true, Legs = 0,
            },
        };

        /// <summary>Mercy (24): a field ambulance. Unarmed, on wheels (a tracked chassis with the wheeled bog, since wheels
        /// have no rules of their own), it drives up and patches the men beside it the way the medic does, once SupportSystem
        /// lets a vehicle heal (the next SIM commit). Thin, and full of fuel.</summary>
        public static UnitDef Mercy => new UnitDef
        {
            Archetype = VehicleArchetype.Mercy,
            Roster = Machine(VehicleArchetype.Mercy, 150, 1000f, 3.8f, 300, ChassisKind.Tracked),
            Infantry = new InfantrySpec { Group = OrderGroup.Support, HealRadius = 10f, HealPerSecond = 25f },
            Weapon = new WeaponStats { Id = VehicleArchetype.Mercy },   // RangeMax 0: acquires nothing
            Machine = new TankSpec
            {
                Hull = Plate(5f, 4f, 4f, 3f), Turret = Plate(5f, 4f, 4f, 3f),
                TurretChance = 0f, Crew = 2, GunCount = 0, ShortHalt = false, FuelRisk = 0.40f, AmmoRisk = 0f,
            },
            Drive = new VehicleProfile
            {
                TurnRateRad = 0.9f, Accel = 1.4f, Brake = 2.2f, PivotSpeed = 0.22f, TrenchCrossWidth = 2.0f, DitchChance = 0.9f, SlopeLimit = 0.5f, BogChance = 0.04f,
                HalfLength = 2.3f, HalfWidth = 1.6f, PushesTrees = false, Wheeled = true,
            },
        };

        /// <summary>Frog (25): the playground's frog infantryman, a rifleman with his own figure (a VAT bake through the
        /// playground's retarget, SHOW). The Rifleman's every number.</summary>
        public static UnitDef Frog => new UnitDef
        {
            Archetype = InfantryArchetype.Frog,
            Roster = Man(InfantryArchetype.Frog, 25, 100f, 3.0f),
            Infantry = new InfantrySpec { Group = OrderGroup.Line },
            Weapon = Borrowed(InfantryArchetype.Rifle, InfantryArchetype.Frog),
        };

        // ---- docs/06's ideas, as stand-ins on what the sim already does ----

        /// <summary>Sentry (26): a machine gunner behind a frontal plate. The shield bearer's plate on a machine gunner's
        /// weapon, with no guard radius: the plate is his own (a shooter aiming past him is not redirected).</summary>
        public static UnitDef Sentry => new UnitDef
        {
            Archetype = InfantryArchetype.Sentry,
            Roster = Man(InfantryArchetype.Sentry, 125, 160f, 2.0f, 200),
            Infantry = new InfantrySpec { Group = OrderGroup.Gun, Braced = true, ShieldPlateMm = 8f, ShieldArcHalf = 60f * InfantrySpec.Deg, ShieldGuardRadius = 0f },
            Weapon = Borrowed(InfantryArchetype.Machinegunner, InfantryArchetype.Sentry),
        };

        /// <summary>AT rifle (27): a single-shot anti-tank rifle. 20 mm at 70 m, one round every five seconds, and he hunts
        /// machines the way the Skimmer does (a foot shooter on that path needs the next SIM commit). Prone-only fire has
        /// no spec yet; Braced gives him the machine gunner's prone accuracy.</summary>
        public static UnitDef AtRifle => new UnitDef
        {
            Archetype = InfantryArchetype.AtRifle,
            Roster = Man(InfantryArchetype.AtRifle, 100, 90f, 2.8f, 200),
            Infantry = new InfantrySpec { Group = OrderGroup.Marksman, HuntsArmour = true, Braced = true },
            Weapon = new WeaponStats { Id = InfantryArchetype.AtRifle, Damage = 250f, RangeMax = 70f * CombatTables.RangeScale, RoundsPerSecond = 0.2f, Accuracy = 0.6f, SuppressionPerShot = 15f, PenetrationMm = 20f },
        };

        /// <summary>Death Battalion (28): the elite rifle. Dearer, harder, hits harder, and never stays pinned (the clamp
        /// lands with the next SIM commit).</summary>
        public static UnitDef DeathBattalion
        {
            get
            {
                var w = Borrowed(InfantryArchetype.Rifle, InfantryArchetype.DeathBattalion); w.Damage = 40f;
                return new UnitDef
                {
                    Archetype = InfantryArchetype.DeathBattalion,
                    Roster = Man(InfantryArchetype.DeathBattalion, 45, 120f, 3.0f),
                    Infantry = new InfantrySpec { Group = OrderGroup.Line, NeverPinned = true },
                    Weapon = w,
                };
            }
        }

        /// <summary>Mark IV Male (29): the Maw's numbers and model under its historical name, so the two can be fielded
        /// side by side and told apart on the card.</summary>
        public static UnitDef MarkIV
        {
            get
            {
                var r = RosterEntry.Maw; r.Archetype = VehicleArchetype.MarkIV;
                return new UnitDef
                {
                    Archetype = VehicleArchetype.MarkIV, Roster = r,
                    Infantry = new InfantrySpec { Group = OrderGroup.Line },
                    Weapon = MachineGun(VehicleArchetype.MarkIV), Machine = TankSpec.Maw, Drive = VehicleProfile.Maw,
                };
            }
        }

        /// <summary>Mark V (30): a Mark IV that handles mud, a little thicker and quicker (docs/06). The Maw's model.</summary>
        public static UnitDef MarkV
        {
            get
            {
                var m = TankSpec.Maw; m.Hull = Plate(14f, 12f, 8f, 8f); m.Turret = m.Hull;
                var d = VehicleProfile.Maw; d.BogChance = 0.03f;
                return new UnitDef
                {
                    Archetype = VehicleArchetype.MarkV,
                    Roster = Machine(VehicleArchetype.MarkV, 380, 4000f, 2.0f, 600, ChassisKind.Tracked),
                    Infantry = new InfantrySpec { Group = OrderGroup.Line },
                    Weapon = MachineGun(VehicleArchetype.MarkV), Machine = m, Drive = d,
                };
            }
        }

        /// <summary>A7V (31): thick in front, one gun fixed in the hull's nose, six machine guns (one weapon here), and
        /// a belly that cannot cross a trench wider than two metres and bogs in anything (docs/06). The Brute's model.</summary>
        public static UnitDef A7V => new UnitDef
        {
            Archetype = VehicleArchetype.A7V,
            Roster = Machine(VehicleArchetype.A7V, 400, 4400f, 1.8f, 700, ChassisKind.Tracked),
            Infantry = new InfantrySpec { Group = OrderGroup.Line },
            Weapon = MachineGun(VehicleArchetype.A7V, 120f, 7f),
            Machine = new TankSpec
            {
                Hull = Plate(30f, 20f, 20f, 6f), Turret = Plate(30f, 20f, 20f, 6f),
                TurretChance = 0f, Crew = 6, GunCount = 1, ShortHalt = false, FuelRisk = 0.35f, AmmoRisk = 0.45f,
                Gun0 = new TankGun
                {
                    Mount = TankMount.Hull, RestYaw = 0f, ArcHalf = 25f * Deg, TraverseRate = 20f * Deg,
                    RangeMax = 125f, Accuracy = 0.45f, ReloadSeconds = 4f, PenMm = 40f, ApDamage = 650f,
                    HeDamage = 220f, HeRadius = 4.5f, HeSuppression = 45f, HeCrater = 1.2f, Mount3 = new float3(0f, 2.4f, 3.0f),
                },
            },
            Drive = new VehicleProfile
            {
                TurnRateRad = 0.35f, TrenchCrossWidth = 2.0f, DitchChance = 0.8f, SlopeLimit = 0.4f, BogChance = 0.10f,
                HalfLength = 4.4f, HalfWidth = 2.7f, PushesTrees = true,   // the Brute's model, which it wears (SHOW: TankRenderer.StandIns)
            },
        };

        /// <summary>Renault FT (32): the Tusk's turret and gun with the FT's plate and a narrower trench (docs/06). The
        /// Tusk's model.</summary>
        public static UnitDef RenaultFT
        {
            get
            {
                var m = TankSpec.Tusk; m.Hull = Plate(16f, 16f, 8f, 8f); m.Turret = Plate(16f, 16f, 8f, 8f);
                var d = VehicleProfile.Tusk; d.TrenchCrossWidth = 1.8f; d.DitchChance = 0.85f;
                return new UnitDef
                {
                    Archetype = VehicleArchetype.RenaultFT,
                    Roster = Machine(VehicleArchetype.RenaultFT, 220, 1600f, 3.0f, 450, ChassisKind.Tracked),
                    Infantry = new InfantrySpec { Group = OrderGroup.Line },
                    Weapon = Borrowed(VehicleArchetype.Tusk, VehicleArchetype.RenaultFT), Machine = m, Drive = d,
                };
            }
        }

        /// <summary>Whippet (33): the quick one, machine guns only, no gun for TankGunnery (the Skimmer's shape). The
        /// Tusk's model.</summary>
        public static UnitDef Whippet
        {
            get
            {
                var d = VehicleProfile.Tusk; d.TurnRateRad = 0.9f; d.TrenchCrossWidth = 2.2f; d.DitchChance = 0.8f;
                return new UnitDef
                {
                    Archetype = VehicleArchetype.Whippet,
                    Roster = Machine(VehicleArchetype.Whippet, 200, 1500f, 3.6f, 400, ChassisKind.Tracked),
                    Infantry = new InfantrySpec { Group = OrderGroup.Line },
                    Weapon = MachineGun(VehicleArchetype.Whippet, 120f, 8f),
                    Machine = new TankSpec
                    {
                        Hull = Plate(14f, 12f, 8f, 6f), Turret = Plate(14f, 12f, 8f, 6f),
                        TurretChance = 0.3f, Crew = 3, GunCount = 0, ShortHalt = false, FuelRisk = 0.3f, AmmoRisk = 0.25f,
                    },
                    Drive = d,
                };
            }
        }

        /// <summary>Austin (34): an armoured car. Wheels (the wheeled bog on a tracked chassis), two machine-gun turrets as
        /// one weapon, thin all round, and it ditches in nearly any trench because there is no wheeled path to keep it
        /// off them. The Mercy's model.</summary>
        public static UnitDef Austin => new UnitDef
        {
            Archetype = VehicleArchetype.Austin,
            Roster = Machine(VehicleArchetype.Austin, 220, 1400f, 4.0f, 400, ChassisKind.Tracked),
            Infantry = new InfantrySpec { Group = OrderGroup.Line },
            Weapon = MachineGun(VehicleArchetype.Austin, 120f, 8f),
            Machine = new TankSpec
            {
                Hull = Plate(8f, 8f, 8f, 8f), Turret = Plate(8f, 8f, 8f, 8f),
                TurretChance = 0.4f, Crew = 4, GunCount = 0, ShortHalt = false, FuelRisk = 0.35f, AmmoRisk = 0.2f,
            },
            Drive = new VehicleProfile
            {
                TurnRateRad = 0.8f, TrenchCrossWidth = 2.0f, DitchChance = 0.95f, SlopeLimit = 0.4f, BogChance = 0.10f,
                HalfLength = 2.3f, HalfWidth = 1.6f, PushesTrees = false, Wheeled = true,
            },
        };

        /// <summary>Sapper (35): the officer's carbine and two charges. He lays a mine or a tripwire where a UnitAbility
        /// order sends him (SapperSystem, the SIM commit after next); until then he is a rifleman with a bag.</summary>
        public static UnitDef Sapper => new UnitDef
        {
            Archetype = InfantryArchetype.Sapper,
            Roster = Man(InfantryArchetype.Sapper, 70, 100f, 3.0f, 200),
            Infantry = new InfantrySpec { Group = OrderGroup.Support, MineCharges = 2 },
            Weapon = Borrowed(InfantryArchetype.Officer, InfantryArchetype.Sapper),
        };

        /// <summary>Bullfrog (37): the playground's toad mech (owner, 2026-09-30: "play with all the new models"). Two
        /// gatlings over its back as one weapon, a fast stream of light rounds that pins men and scratches thin plate. It
        /// hops rather than walks (its legs are welded into its body), so the sim carries it as the Hopper is carried: a
        /// walker profile with no legs to lose, clearing trenches and wire, never bogging or ditching, pushing nothing.
        /// Placeholder numbers between the Whippet and the Croaker. Drawn hopping from its own model (SHOW).</summary>
        public static UnitDef Bullfrog => new UnitDef
        {
            Archetype = VehicleArchetype.Bullfrog,
            Roster = Machine(VehicleArchetype.Bullfrog, 280, 1600f, 3.4f, 480, ChassisKind.Tracked),
            Infantry = new InfantrySpec { Group = OrderGroup.Line },
            Weapon = MachineGun(VehicleArchetype.Bullfrog, 140f, 12f),
            Machine = new TankSpec
            {
                Hull = Plate(12f, 10f, 8f, 6f), Turret = Plate(10f, 8f, 8f, 6f),
                TurretChance = 0.3f, Crew = 2, GunCount = 0, ShortHalt = false, FuelRisk = 0.25f, AmmoRisk = 0.45f,
            },
            Drive = new VehicleProfile
            {
                TurnRateRad = 1.4f, TrenchCrossWidth = 4.0f, DitchChance = 0f, SlopeLimit = 0.9f, BogChance = 0f,
                HalfLength = 2.8f, HalfWidth = 3.2f, PushesTrees = false, Walker = true, Legs = 0,   // its model, drawn 1.6 times: 5.6 m long, 6.4 m across the guns
            },
        };

        /// <summary>Flamethrower (36): a cone of fire to 12 m, four bursts a second, and every hit sets the man and the
        /// ground under him alight once DirectFire reads SetsBurning (the last SIM commit of the seam). No penetration:
        /// a plate stops nothing, but the rounds are for men.</summary>
        public static UnitDef Flamethrower => new UnitDef
        {
            Archetype = InfantryArchetype.Flamethrower,
            Roster = Man(InfantryArchetype.Flamethrower, 120, 100f, 3.0f, 300),
            Infantry = new InfantrySpec { Group = OrderGroup.Line },
            Weapon = new WeaponStats { Id = InfantryArchetype.Flamethrower, Mode = FireMode.Cone, Damage = 10f, RangeMax = 12f * CombatTables.RangeScale, RoundsPerSecond = 4f, Accuracy = 0.9f, SuppressionPerShot = 15f, PenetrationMm = 0f, SetsBurning = true },
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
