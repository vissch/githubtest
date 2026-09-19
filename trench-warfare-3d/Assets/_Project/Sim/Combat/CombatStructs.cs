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
        public float RoundsPerSecond;
        public int BurstRounds;
        public int MagazineRounds;     // 0 = no reload cycle
        public float ReloadSeconds;
        public float Accuracy;         // base hit chance at optimal range
        public float PenetrationMm;    // 0 for small arms vs armour = auto-fail
        public float BlastRadius;      // 0 = point damage
        public float SuppressionPerShot;
        public float MuzzleVelocity;   // indirect: fixed v0 for arc solve
        public byte CraterRadiusDm;    // decimetres, 0 = no crater
        public bool SetsBurning;
    }

    /// <summary>Armour plate thickness by facing. Penetration = weapon pen × cos(incidence) vs plate.</summary>
    public struct ArmorProfile
    {
        public float FrontMm, SideMm, RearMm, TopMm;
    }

    public enum VehicleModule : byte { None = 0, Track = 1, Engine = 2, Crew = 3, Gun = 4 }
}
