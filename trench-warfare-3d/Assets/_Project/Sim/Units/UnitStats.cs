// Phase: A3 (contract struct, implemented as data) — baked from TW.Data.UnitDefinition (C2)
using TW.Sim.Combat;

namespace TW.Sim.Units
{
    public enum UnitClass : byte
    {
        Rifleman = 0, Assault = 1, Machinegunner = 2,
        Sniper = 10, Sentry = 11, MortarTeam = 12, Arditi = 13, AntiTankRifle = 14, FieldGun = 15, ShieldGrenadier = 16,
        Officer = 17, Flamethrower = 18, Cavalry = 19, EliteRifle = 20,
        Tank = 30, ArmouredCar = 31, Lorry = 32, Motorcycle = 33,
        MgNest = 40, Bunker = 41, FieldGunEmplacement = 42, HqDugout = 43, LivensBattery = 44, Mine = 45,
    }

    public struct UnitStats
    {
        public short Id;
        public UnitClass Class;
        public int Cost;
        public float Hp;
        public float Speed;            // m/s standing on flat ground
        public short PrimaryWeapon;    // WeaponStats id
        public short SecondaryWeapon;  // grenade / MG / -1
        public float GrenadeRange;
        public int DeployCooldownTicks;
        public ArmorProfile Armor;     // zero for unarmoured
        public float AuraRadius;       // officer
        public float ShieldArcHalfWidth; // shield grenadier / sentry frontal plate
        public bool CanEnterTrench;    // cavalry/vehicles false
        public bool IsVehicle;
        public bool IsEmplacement;
        public bool ImmuneToPinned;
        public float TrenchHpBonus;    // sniper +100 %
        public float TrenchAccuracyBonus;
        public float TrenchRangeBonus;
        public byte TargetPriorityProfile; // 0 nearest, 1 sniper weights
    }
}
