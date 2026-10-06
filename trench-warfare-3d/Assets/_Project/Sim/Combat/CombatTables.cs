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
        public const float HullTargetBonus = 1.3f;       // a hull is a big target: the odds of a round hitting one (tank guns, armour-hunting small arms)
        public const float CloseAssaultDamage = 600f;         // structure, when the charge gets through the plate
        public const float CloseAssaultPenMm = 20f;           // a grenade bundle on the deck: through a top plate, not a front
        public const float CloseAssaultChance = 0.5f;
        public const int CloseAssaultCooldownTicks = 60;
        public const float MovingAccuracy = 0.5f;        // shooter moving faster than MovingSpeed
        public const float MovingSpeed = 0.5f;
        public const float AdvanceFireRange = 60f;       // units under >> only engage this close: they are running
        // ---- a running man is a hard mark (2026-09-28): the rifleman has to lead him, and the farther off he is the
        // more of a guess that is; close up it makes no odds. Without it a garrison of ten shot every assault of up to
        // thirty dead at 60-100 m and lost nobody (AssaultLadderTests).
        public const float RunningTargetSpeed = 2f;      // m/s: a man going faster than this is running
        public const float RunningTargetNear = 20f;      // metres: closer than this he is no harder to hit
        public const float RunningTargetFar = 60f;       // metres: from here on the whole penalty
        public const float RunningTargetFloor = 0.35f;   // the share of the chance left at RunningTargetFar and beyond
        public const float GarrisonFireSuppressionLimit = 40f;   // above this a garrison stays below the rim
        public const float DeadGroundMetres = 10f;       // a man this far behind his own front trench is out of the far side's sight (TargetAcquisition)
        // ---- the trench mouth (2026-09-29, v23): closer in than DeadGroundMetres, from DeadGroundLipMetres in front of the
        // trench's line back, a man coming up to his own trench is hidden by its parapet from a shooter on the far side
        // who is more than DeadGroundCloseMetres from him, unless the shooter is a sniper (owner, 2026-09-29). Ten-minute matches lost most of their dead there: men shot
        // at 100-130 m by the far garrison as they stepped down into their own trench (MatchLoopTests, TrenchMouthTests).
        public const float DeadGroundLipMetres = 1.5f;   // the trench's line is its middle: a man dropping in stands up to this far in front of it
        public const float DeadGroundCloseMetres = 40f;  // an attacker this close looks over the parapet at him
        // ---- the bomb (2026-09-28): how a trench was cleared. A man in the open who has come within GrenadeRange of the
        // trench man he is fighting throws one instead of firing: it goes off where it lands (BlastSystem, so the bay
        // still saves a man some of it), and he holds it when a friend stands within GrenadeFriend of the mark. A rifle
        // on the fire step barely finds a running man's shooters; the bomb is how an assault that got there wins.
        public const float GrenadeRange = 22f;           // metres: a strong arm
        public const float GrenadeMin = 5f;              // closer than this he does not throw at his own feet
        public const float GrenadeDamage = 110f;
        public const float GrenadeRadius = 4.5f;
        public const float GrenadeSuppression = 50f;
        public const float GrenadeCooldownSeconds = 3f;  // pin out, throw, get down again
        public const float GrenadeScatter = 0.5f;        // metres off the mark at no range ...
        public const float GrenadeScatterPerMetre = 0.08f; // ... and this much more per metre thrown
        public const float GrenadeFriend = 4f;
        /// <summary>A bomb's time in the air (2026-09-29): this much at any range, and GrenadeFlightPerMetre more a metre:
        /// half a second at 5 m, 1.2 s at 22 m. It went off the tick it was thrown, so no throw could be drawn.</summary>
        public const float GrenadeFlightSeconds = 0.3f, GrenadeFlightPerMetre = 0.04f;
        /// <summary>Ticks a bomb thrown <paramref name="metres"/> is in the air; at least one.</summary>
        public static int GrenadeFlightTicks(float metres, float tickSeconds)
            => math.max(1, (int)math.round((GrenadeFlightSeconds + GrenadeFlightPerMetre * metres) / tickSeconds));

        /// <summary>Bombs a man of this archetype goes into battle with. Riflemen carry two, assault troops (the
        /// bombers) four; a man whose trade is not the assault (the gunner, the sniper, the medic) none.</summary>
        public static byte GrenadesFor(byte archetype)
        {
            switch (archetype)
            {
                case InfantryArchetype.Rifle: case InfantryArchetype.Frog: case InfantryArchetype.DeathBattalion:
                case InfantryArchetype.Para: case InfantryArchetype.Jetpack: case InfantryArchetype.Shield: case InfantryArchetype.Sapper:
                    return 2;
                case InfantryArchetype.Assault: return 4;
                case InfantryArchetype.Officer: return 1;
                default: return 0;
            }
        }
        // ---- smoke (docs/21 phase 5): what a screen does to sight and aim; SmokeLos measures the metres ----
        public const float SmokeBlindMetres = 12.5f;      // this much thick smoke on the line of sight and the target is lost
        public const float SmokeAccuracyPerMetre = 0.08f; // accuracy lost per metre of thick smoke on the line of fire ...
        public const float SmokeAccuracyFloor = 0.2f;     // ... down to this share: a blind burst still finds somebody
        public const float SmokeSuppression = 0.5f;       // suppression gain of a man inside thick smoke: he cannot tell how close it was

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

        /// <summary>The share of a shooter's chance left against a man running at <paramref name="speed"/> m/s
        /// <paramref name="dist"/> metres off: 1 standing, walking or close, falling to RunningTargetFloor by RunningTargetFar.</summary>
        public static float RunningTarget(float dist, float speed)
        {
            if (speed <= RunningTargetSpeed) return 1f;
            float t = math.saturate((dist - RunningTargetNear) / (RunningTargetFar - RunningTargetNear));
            return 1f - (1f - RunningTargetFloor) * t;
        }
    }
}
