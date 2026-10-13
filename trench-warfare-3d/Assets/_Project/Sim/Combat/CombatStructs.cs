// Phase: A2/A5 (contract structs, implemented as data)
namespace TW.Sim.Combat
{
    public enum FireMode : byte { Direct = 0, Indirect = 1, Cone = 2 }

    public struct WeaponStats
    {
        public short Id;
        public FireMode Mode;
        public float Damage;
        public float RangeMin, RangeMax;
        public float RoundsPerSecond;   // the figure the cooldown is derived from, not the pace fired: CombatTables.ShotsPerSecond is one tick more between shots
        public int BurstRounds;        // baked and hashed, no sim reader yet; BalanceSweepTests patches it by name
        public int MagazineRounds;     // 0 = no reload cycle. Baked and hashed, no sim reader yet
        public float ReloadSeconds;
        public float Accuracy;         // base hit chance at optimal range
        public float PenetrationMm;    // 0 for small arms vs armour = auto-fail
        public float BlastRadius;      // 0 = point damage. Baked and hashed, no sim reader yet
        public float SuppressionPerShot;
        public float MuzzleVelocity;   // indirect: fixed v0 for arc solve. Baked and hashed, no sim reader yet
        public byte CraterRadiusDm;    // decimetres, 0 = no crater. Baked and hashed, no sim reader yet
        public bool SetsBurning;
    }

    /// <summary>Armour plate thickness by facing. Penetration = weapon pen × cos(incidence) vs plate.</summary>
    public struct ArmorProfile
    {
        public float FrontMm, SideMm, RearMm, TopMm;
    }

    /// <summary>What a penetrating hit can break (VehicleModulesSystem keeps one health value per module, 1 = whole).
    /// The first five keep their Phase-0 numbers.</summary>
    public enum VehicleModule : byte { None = 0, TrackLeft = 1, Engine = 2, Crew = 3, GunA = 4, TrackRight = 5, GunB = 6, Fuel = 7, Ammo = 8, Count = 9 }
}
