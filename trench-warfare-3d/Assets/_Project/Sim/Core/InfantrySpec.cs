// Phase: A3 (implemented 2026-09-25) — the one table of what an infantry archetype can do besides shoot.
// The TankSpec precedent: every field is data, behaviour keys on a non-zero field, and no system switches on the
// archetype id. Lives in Core (not Units) because MoveJob and the combat jobs read it. Numbers are starting points
// (docs/06, the owner's brief of 2026-09-25) and are tuned in play; C2 bakes them from UnitDefinition later.
using Unity.Burst;

namespace TW.Sim
{
    public struct InfantrySpec
    {
        /// <summary>A braced weapon (the machine gun): prone fire is more accurate, not less (DirectFire).</summary>
        public bool Braced;
        /// <summary>Walks straight at what it faces instead of wandering across the line (the shield bearer).</summary>
        public bool NoDrift;

        // ---- officer: an aura over the men round him (AuraSystem) ----
        public float AuraRadius;          // metres; 0 = no aura
        public float AuraDamageMul;       // what his men's rounds do inside it
        public float AuraSuppressionMul;  // what fire does to his men's nerve inside it
        public bool AuraUnpin;            // nobody inside it stays Pinned: they stand up and shoot

        // ---- medic: living men only, one at a time (SupportSystem) ----
        public float HealRadius, HealPerSecond;

        // ---- repair engineer: friendly machines (SupportSystem) ----
        public float RepairRadius, RepairPerSecond, MendEverySeconds, FirePerSecond;

        // ---- shield bearer: a plate on the forearm that takes the rounds meant for the men behind him ----
        public float ShieldPlateMm;       // 0 = no plate
        public float ShieldArcHalf;       // radians either side of his facing the plate covers
        public float ShieldGuardRadius;   // metres: a shooter aiming at a man this close behind him aims at him instead

        // ---- jetpack: a leap into an enemy trench (LeapSystem) ----
        public float JumpRange, JumpSpeed;
        public int JumpCooldownTicks, LandingGraceTicks;
        public float LandingBlastDamage, LandingBlastRadius;

        /// <summary>Delivered by air (the paratrooper): never a roster slot, always an off-map drop.</summary>
        public bool DropCapable;

        public const float Deg = SimMath.Pi / 180f;

        public static InfantrySpec For(byte archetype)
        {
            switch (archetype)
            {
                case InfantryArchetype.Machinegunner: return new InfantrySpec { Braced = true };
                case InfantryArchetype.Officer:
                    return new InfantrySpec { AuraRadius = 15f, AuraDamageMul = 1.2f, AuraSuppressionMul = 0.5f, AuraUnpin = true };
                case InfantryArchetype.Shield:
                    return new InfantrySpec { NoDrift = true, ShieldPlateMm = 8f, ShieldArcHalf = 60f * Deg, ShieldGuardRadius = 4f };
                case InfantryArchetype.Medic:
                    return new InfantrySpec { HealRadius = 8f, HealPerSecond = 25f };
                case InfantryArchetype.Repair:
                    return new InfantrySpec { RepairRadius = 6f, RepairPerSecond = 40f, MendEverySeconds = 8f, FirePerSecond = 0.05f };
                case InfantryArchetype.Para:
                    return new InfantrySpec { DropCapable = true };
                case InfantryArchetype.Jetpack:
                    return new InfantrySpec { JumpRange = 28f, JumpSpeed = 12f, JumpCooldownTicks = 300, LandingGraceTicks = 40, LandingBlastDamage = 80f, LandingBlastRadius = 4f };
                default: return default;
            }
        }
    }
}
