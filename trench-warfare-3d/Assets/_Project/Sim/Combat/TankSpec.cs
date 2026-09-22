// Phase: A5b (implemented as placeholder data; C2 bakes it from VehicleDefinition) — depends on: ArmorProfile, VehicleArchetype
// What each tank carries and how thick it is. Two tanks, from the owner's models (Tools/tanksplit.py):
//  - Maw (archetype 4), the heavy: no turret, a 6-pdr in a sponson on each side (the left one rests 45° off the nose,
//    the right one 24°; each swings 60° either way and never through the hull), six crew, 12/8/6/6 mm (docs/06 Mark IV).
//  - Tusk (archetype 5), the light: a 37 mm in a turret that goes all the way round, two crew, thicker but smaller plates.
// Yaw here is sim yaw: radians, positive to the vehicle's right. Mount3 is hull-local metres (x right, y up, z forward).
using Unity.Mathematics;

namespace TW.Sim.Combat
{
    public enum TankMount : byte { None = 0, Hull = 1, Turret = 2, SponsonLeft = 3, SponsonRight = 4 }

    public struct TankGun
    {
        public TankMount Mount;
        public float RestYaw;          // relative to the hull
        public float ArcHalf;          // how far either side of RestYaw it can swing (≥ π: all round)
        public float TraverseRate;     // rad/s
        public float RangeMax, Accuracy, ReloadSeconds;
        public float PenMm, ApDamage;  // armour-piercing, against vehicles
        public float HeDamage, HeRadius, HeSuppression, HeCrater;   // high explosive, against men
        public float3 Mount3;          // where the gun sits on the hull, for line of sight
        public bool FullCircle => ArcHalf >= SimMath.Pi;
    }

    public struct TankSpec
    {
        public ArmorProfile Hull, Turret;
        public float TurretChance;     // share of direct hits that strike the turret (0: no turret)
        public byte Crew;
        public int GunCount;
        public TankGun Gun0, Gun1;
        public bool ShortHalt;         // stops for a moment to fire its gun (the Tusk) instead of firing on the move (the Maw)
        public float FuelRisk, AmmoRisk;   // chance a penetration into the fuel / the racks starts a fire / sets the rounds off

        public TankGun Gun(int k) => k == 0 ? Gun0 : Gun1;

        public const float Deg = SimMath.Pi / 180f;
        public const int MaxGuns = 2;

        public static TankSpec For(byte archetype) => archetype == VehicleArchetype.Tusk ? Tusk : Maw;

        public static TankSpec Maw => new TankSpec
        {
            Hull = new ArmorProfile { FrontMm = 12f, SideMm = 8f, RearMm = 6f, TopMm = 6f },
            Turret = new ArmorProfile { FrontMm = 12f, SideMm = 8f, RearMm = 6f, TopMm = 6f },
            TurretChance = 0f, Crew = 6, GunCount = 2, ShortHalt = false, FuelRisk = 0.35f, AmmoRisk = 0.45f,
            Gun0 = SixPounder(TankMount.SponsonLeft, -45f * Deg, new float3(-1.65f, 3.0f, -1.6f)),
            Gun1 = SixPounder(TankMount.SponsonRight, 24f * Deg, new float3(2.2f, 3.1f, -1.3f)),
        };

        public static TankSpec Tusk => new TankSpec
        {
            Hull = new ArmorProfile { FrontMm = 16f, SideMm = 12f, RearMm = 10f, TopMm = 6f },
            Turret = new ArmorProfile { FrontMm = 16f, SideMm = 16f, RearMm = 14f, TopMm = 6f },
            TurretChance = 0.4f, Crew = 2, GunCount = 1, ShortHalt = true, FuelRisk = 0.3f, AmmoRisk = 0.35f,
            Gun0 = new TankGun
            {
                Mount = TankMount.Turret, RestYaw = 0f, ArcHalf = SimMath.Pi, TraverseRate = 50f * Deg,
                RangeMax = 220f, Accuracy = 0.55f, ReloadSeconds = 2.2f, PenMm = 30f, ApDamage = 420f,
                HeDamage = 130f, HeRadius = 2.5f, HeSuppression = 30f, HeCrater = 0f, Mount3 = new float3(0f, 2.6f, 0.9f),
            },
        };

        static TankGun SixPounder(TankMount mount, float rest, float3 at) => new TankGun
        {
            Mount = mount, RestYaw = rest, ArcHalf = 60f * Deg, TraverseRate = 35f * Deg,
            RangeMax = 240f, Accuracy = 0.45f, ReloadSeconds = 4f, PenMm = 40f, ApDamage = 650f,
            HeDamage = 220f, HeRadius = 4.5f, HeSuppression = 45f, HeCrater = 1.2f, Mount3 = at,
        };

        /// <summary>Can this gun swing to <paramref name="relYaw"/> (relative to the hull)? The sponsons stop short of the hull.</summary>
        public static bool InArc(in TankGun g, float relYaw) => g.FullCircle || math.abs(SimMath.WrapAngle(relYaw - g.RestYaw)) <= g.ArcHalf;

        /// <summary>The nearest yaw inside the gun's arc.</summary>
        public static float ClampToArc(in TankGun g, float relYaw)
        {
            if (g.FullCircle) return SimMath.WrapAngle(relYaw);
            float off = SimMath.WrapAngle(relYaw - g.RestYaw);
            return SimMath.WrapAngle(g.RestYaw + math.clamp(off, -g.ArcHalf, g.ArcHalf));
        }
    }
}
