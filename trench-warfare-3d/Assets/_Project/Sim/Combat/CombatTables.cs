// Phase: A2 (implemented as placeholder data; C2 bakes the real tables from TW.Data and replaces WeaponFor)
// One weapon per Phase-0 roster archetype (0 rifleman, 1 assault, 2 machinegunner, 3 sniper, 4 tank hull MGs) and
// the shared rules of who can see and hit whom. Everything here is pure and Burst-compatible.
using Unity.Burst;
using Unity.Mathematics;

namespace TW.Sim.Combat
{
    [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
    public static class CombatTables
    {
        public const float TrenchCover = 0.75f;          // fire-step target shot at from outside its trench
        public const float BelowRimRevealRange = 8f;     // a garrison below the rim can only be engaged from this close
        public const float CraterCover = 0.35f;          // added to the stance cover of a man standing in a shell hole
        public const float CloseAssaultRange = 8f;       // infantry this close to a vehicle attack it with bundled grenades
        public const float CloseAssaultDamage = 600f;
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
                    return new WeaponStats { Id = 1, Damage = 22f, RangeMax = 45f, RoundsPerSecond = 3f, Accuracy = 0.45f, SuppressionPerShot = 7f };
                case 2:  // machinegunner
                    return new WeaponStats { Id = 2, Damage = 24f, RangeMax = 170f, RoundsPerSecond = 7f, Accuracy = 0.30f, SuppressionPerShot = 9f };
                case 3:  // sniper
                    return new WeaponStats { Id = 3, Damage = 95f, RangeMax = 230f, RoundsPerSecond = 0.3f, Accuracy = 0.9f, SuppressionPerShot = 20f };
                case 4:  // tank hull machine guns
                    return new WeaponStats { Id = 4, Damage = 24f, RangeMax = 120f, RoundsPerSecond = 5f, Accuracy = 0.30f, SuppressionPerShot = 10f };
                default: // rifleman
                    return new WeaponStats { Id = 0, Damage = 36f, RangeMax = 130f, RoundsPerSecond = 0.5f, Accuracy = 0.55f, SuppressionPerShot = 12f };
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
