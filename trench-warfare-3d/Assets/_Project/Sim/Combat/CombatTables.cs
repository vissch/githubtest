// Phase: A2 (implemented as placeholder data; C2 bakes the real tables from TW.Data and replaces WeaponFor)
// One weapon per roster archetype (0 rifleman, 1 assault, 2 machinegunner, 3 sniper, 4/5 tank MGs, 12..18 the
// 2026-09-25 units) and the shared rules of who can see and hit whom. Everything here is pure and Burst-compatible.
// PenetrationMm is what a round does to a shield bearer's plate (DirectFire): a rifle never holes 8 mm, an MG
// usually does, a sniper always.
using Unity.Burst;
using Unity.Mathematics;

namespace TW.Sim.Combat
{
    [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
    public static class CombatTables
    {
        public const float TrenchCover = 0.75f;          // fire-step target shot at from outside its trench
        public const float BelowRimRevealRange = 8f;     // a garrison below the rim can only be engaged from this close
        public const float ChargeRevealRange = 20f;      // a charging Breaker (UnitFlags.Charging) looks down into the trench from this far
        public const float CraterCover = 0.35f;          // added to the stance cover of a man standing in a shell hole
        public const float CloseAssaultRange = 8f;       // infantry this close to a vehicle attack it with bundled grenades
        public const float CloseAssaultDamage = 600f;         // structure, when the charge gets through the plate
        public const float CloseAssaultPenMm = 20f;           // a grenade bundle on the deck: through a top plate, not a front
        public const float CloseAssaultChance = 0.5f;
        public const int CloseAssaultCooldownTicks = 60;
        public const float MovingAccuracy = 0.5f;        // shooter moving faster than MovingSpeed
        public const float MovingSpeed = 0.5f;
        public const float AdvanceFireRange = 60f;       // units under >> only engage this close: they are running
        public const float GarrisonFireSuppressionLimit = 40f;   // above this a garrison stays below the rim

        public static WeaponStats WeaponFor(byte archetype)
        {
            switch (archetype)
            {
                case 1:  // assault: SMG / trench gun
                    return new WeaponStats { Id = 1, Damage = 22f, RangeMax = 45f, RoundsPerSecond = 3f, Accuracy = 0.45f, SuppressionPerShot = 7f, PenetrationMm = 4f };
                case 2:  // machinegunner
                    return new WeaponStats { Id = 2, Damage = 24f, RangeMax = 170f, RoundsPerSecond = 7f, Accuracy = 0.30f, SuppressionPerShot = 9f, PenetrationMm = 9f };
                case 3:  // sniper
                    return new WeaponStats { Id = 3, Damage = 95f, RangeMax = 230f, RoundsPerSecond = 0.3f, Accuracy = 0.9f, SuppressionPerShot = 20f, PenetrationMm = 14f };
                case 4:  // Maw: hull machine guns (its sponson 6-pdrs are TankGunnerySystem's)
                    return new WeaponStats { Id = 4, Damage = 24f, RangeMax = 120f, RoundsPerSecond = 5f, Accuracy = 0.30f, SuppressionPerShot = 10f, PenetrationMm = 9f };
                case 5:  // Tusk: the machine gun beside the 37 mm in its turret
                    return new WeaponStats { Id = 5, Damage = 24f, RangeMax = 110f, RoundsPerSecond = 4f, Accuracy = 0.30f, SuppressionPerShot = 9f, PenetrationMm = 9f };
                // ---- 2026-09-25 ----
                case InfantryArchetype.Officer:   // a carbine: he is there for the men round him, not for his own shooting
                    return new WeaponStats { Id = 12, Damage = 30f, RangeMax = 80f, RoundsPerSecond = 0.8f, Accuracy = 0.6f, SuppressionPerShot = 8f, PenetrationMm = 5f };
                case InfantryArchetype.Shield:    // a pistol in the hand the plate leaves free
                    return new WeaponStats { Id = 13, Damage = 20f, RangeMax = 30f, RoundsPerSecond = 1.5f, Accuracy = 0.45f, SuppressionPerShot = 4f, PenetrationMm = 3f };
                case InfantryArchetype.Medic:     // nothing: RangeMax 0 finds no target and Damage 0 bundles no grenades
                    return new WeaponStats { Id = 14, Damage = 0f, RangeMax = 0f, RoundsPerSecond = 0.5f, Accuracy = 0f, SuppressionPerShot = 0f, PenetrationMm = 0f };
                case InfantryArchetype.Para:      // a rifle, carried down
                    return new WeaponStats { Id = 16, Damage = 30f, RangeMax = 100f, RoundsPerSecond = 0.8f, Accuracy = 0.5f, SuppressionPerShot = 10f, PenetrationMm = 6f };
                case InfantryArchetype.Jetpack:   // a machine pistol for the trench he lands in
                    return new WeaponStats { Id = 17, Damage = 22f, RangeMax = 40f, RoundsPerSecond = 3f, Accuracy = 0.45f, SuppressionPerShot = 7f, PenetrationMm = 4f };
                // The six crabs carry NO small arms: every gun a walker has is TankGunnerySystem's (sponson guns, the
                // Kettle's mortar, the Pavise's and Banner's long guns), and the Redoubt has none at all. Without these
                // cases they fell through to the rifleman below and fired a phantom rifle at 130 m on top of their own
                // guns, because TargetAcquisition reads a shooter's range out of this table for vehicles too.
                case VehicleArchetype.Pincer:
                case VehicleArchetype.Kettle:
                case VehicleArchetype.Censer:
                case VehicleArchetype.Pavise:
                case VehicleArchetype.Banner:
                case VehicleArchetype.Redoubt:
                    return new WeaponStats { Id = archetype };   // RangeMax 0: acquires nothing, fires nothing
                case VehicleArchetype.Breaker:    // hull machine guns either side of the ram
                    return new WeaponStats { Id = 18, Damage = 26f, RangeMax = 110f, RoundsPerSecond = 6f, Accuracy = 0.32f, SuppressionPerShot = 10f, PenetrationMm = 9f };
                default: // rifleman (and the repair engineer, who carries one)
                    return new WeaponStats { Id = 0, Damage = 36f, RangeMax = 130f, RoundsPerSecond = 0.5f, Accuracy = 0.55f, SuppressionPerShot = 12f, PenetrationMm = 6f };
            }
        }

        /// <summary>Ticks between shots for a weapon at the sim's tick rate (at least 1).</summary>
        public static int CooldownTicks(in WeaponStats w, float tickSeconds)
            => math.max(1, (int)math.round(1f / (math.max(0.05f, w.RoundsPerSecond) * tickSeconds)));

        /// <summary>1 inside half range, falling linearly to 0.5 at maximum range.</summary>
        public static float RangeFalloff(float dist, float rangeMax)
        {
            float t = math.saturate((dist / math.max(1f, rangeMax) - 0.5f) * 2f);
            return 1f - 0.5f * t;
        }
    }
}
