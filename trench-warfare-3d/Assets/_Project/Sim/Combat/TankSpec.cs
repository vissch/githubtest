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
        /// <summary>A mortar: it lobs, so it needs no line of sight and cannot reach anything close in (RangeMin).</summary>
        public bool Indirect;
        public float RangeMin;
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
        /// <summary>Nobody gets out: the walkers. They still have a crew (drive cores) that a hit can kill, but when
        /// one is knocked out nothing climbs from the wreck — it just stops where it stands.</summary>
        public bool Unmanned;
        /// <summary>Metres a claw can reach past the hull, and what it does to what it catches (0: no claws).</summary>
        public float ClawReach, ClawDamage;
        public float ClawSeconds;
        /// <summary>A drum of chlorine laid as it walks (Censer): how often a cloud leaves the vent, and how thick.</summary>
        public float GasEverySeconds, GasStrength;
        /// <summary>A standard carried over the machine (Banner): men of its own side within this many metres lose
        /// suppression the faster for it (0: it carries none).</summary>
        public float StandardRadius, StandardSteady;

        public TankGun Gun(int k) => k == 0 ? Gun0 : Gun1;

        public const float Deg = SimMath.Pi / 180f;
        public const int MaxGuns = 2;

        public static TankSpec For(byte archetype)
        {
            switch (archetype)
            {
                case VehicleArchetype.Tusk: return Tusk;
                case VehicleArchetype.Pincer: return Pincer;
                case VehicleArchetype.Kettle: return Kettle;
                case VehicleArchetype.Censer: return Censer;
                case VehicleArchetype.Pavise: return Pavise;
                case VehicleArchetype.Banner: return Banner;
                case VehicleArchetype.Redoubt: return Redoubt;
                default: return Maw;
            }
        }

        // ---- the walkers -------------------------------------------------------------------------------------
        /// <summary>Pincer: two turret guns on the carapace, each with most of a circle to itself, and two claws that
        /// take hold of whatever comes within reach of the front. Porcelain over steel: a domed back that turns a
        /// shell, thin underneath.</summary>
        public static TankSpec Pincer => new TankSpec
        {
            Hull = new ArmorProfile { FrontMm = 20f, SideMm = 14f, RearMm = 11f, TopMm = 16f },
            Turret = new ArmorProfile { FrontMm = 18f, SideMm = 14f, RearMm = 12f, TopMm = 10f },
            TurretChance = 0.22f, Crew = 2, GunCount = 2, ShortHalt = false, FuelRisk = 0.15f, AmmoRisk = 0.40f,
            Unmanned = true, ClawReach = 3.4f, ClawDamage = 900f, ClawSeconds = 2.6f,
            Gun0 = CrabGun(TankMount.SponsonLeft, -28f * Deg, new float3(-0.75f, 2.35f, 0.15f)),
            Gun1 = CrabGun(TankMount.SponsonRight, 28f * Deg, new float3(0.75f, 2.35f, 0.15f)),
        };

        /// <summary>Kettle: one mortar over its back. It drops its shells on men it cannot see, which is the whole
        /// point of it, and it is helpless at close range — hence the two small claws.</summary>
        public static TankSpec Kettle => new TankSpec
        {
            Hull = new ArmorProfile { FrontMm = 14f, SideMm = 11f, RearMm = 9f, TopMm = 10f },
            Turret = new ArmorProfile { FrontMm = 12f, SideMm = 10f, RearMm = 9f, TopMm = 8f },
            TurretChance = 0.30f, Crew = 2, GunCount = 1, ShortHalt = true, FuelRisk = 0.15f, AmmoRisk = 0.55f,
            Unmanned = true, ClawReach = 2.6f, ClawDamage = 420f, ClawSeconds = 3.2f,
            Gun0 = new TankGun
            {
                Mount = TankMount.Turret, RestYaw = 0f, ArcHalf = 24f * Deg, TraverseRate = 24f * Deg,
                RangeMax = 300f, RangeMin = 46f, Indirect = true, Accuracy = 0.30f, ReloadSeconds = 6.5f,
                PenMm = 0f, ApDamage = 0f,
                HeDamage = 320f, HeRadius = 6.0f, HeSuppression = 75f, HeCrater = 2.0f, Mount3 = new float3(0f, 2.2f, -0.2f),
            },
        };

        /// <summary>Censer: no gun at all. It carries a drum of chlorine over its back and lays it where it walks
        /// (VehicleModulesSystem vents it), which is worth more than any gun against a garrison. Thin, quick, and
        /// dangerous to stand near when the drum goes up.</summary>
        public static TankSpec Censer => new TankSpec
        {
            Hull = new ArmorProfile { FrontMm = 11f, SideMm = 9f, RearMm = 8f, TopMm = 8f },
            Turret = new ArmorProfile { FrontMm = 10f, SideMm = 9f, RearMm = 8f, TopMm = 7f },
            TurretChance = 0.35f, Crew = 2, GunCount = 0, ShortHalt = false, FuelRisk = 0.20f, AmmoRisk = 0.85f,
            Unmanned = true, ClawReach = 2.4f, ClawDamage = 380f, ClawSeconds = 3.0f,
            GasEverySeconds = 2.0f, GasStrength = 0.65f,
        };

        /// <summary>Pavise: one long gun on a pintle and a shield on its front flank. It stops to shoot, reaches
        /// furthest of anything on the field, and what comes at it from the front meets the shield.</summary>
        public static TankSpec Pavise => new TankSpec
        {
            Hull = new ArmorProfile { FrontMm = 34f, SideMm = 13f, RearMm = 10f, TopMm = 12f },   // the shield is the front
            Turret = new ArmorProfile { FrontMm = 16f, SideMm = 12f, RearMm = 10f, TopMm = 9f },
            TurretChance = 0.28f, Crew = 2, GunCount = 1, ShortHalt = true, FuelRisk = 0.15f, AmmoRisk = 0.45f,
            Unmanned = true, ClawReach = 2.8f, ClawDamage = 500f, ClawSeconds = 3.4f,
            Gun0 = new TankGun
            {
                Mount = TankMount.Turret, RestYaw = 0f, ArcHalf = 42f * Deg, TraverseRate = 30f * Deg,
                RangeMax = 360f, Accuracy = 0.62f, ReloadSeconds = 5.0f, PenMm = 52f, ApDamage = 780f,
                HeDamage = 190f, HeRadius = 3.8f, HeSuppression = 50f, HeCrater = 1.0f, Mount3 = new float3(0f, 2.45f, 0.1f),
            },
        };

        /// <summary>Banner: a long gun and, over it, a standard. Men fighting within StandardRadius of one keep their
        /// heads: the flag steadies them, which is what a standard was for. It is the dearest thing either side can
        /// put on the field and the easiest to kill, being tall, narrow and thin.</summary>
        public static TankSpec Banner => new TankSpec
        {
            Hull = new ArmorProfile { FrontMm = 15f, SideMm = 11f, RearMm = 9f, TopMm = 9f },
            Turret = new ArmorProfile { FrontMm = 13f, SideMm = 11f, RearMm = 9f, TopMm = 8f },
            TurretChance = 0.26f, Crew = 2, GunCount = 1, ShortHalt = true, FuelRisk = 0.15f, AmmoRisk = 0.40f,
            Unmanned = true, ClawReach = 2.6f, ClawDamage = 430f, ClawSeconds = 3.0f,
            StandardRadius = 26f, StandardSteady = 9f,
            Gun0 = new TankGun
            {
                Mount = TankMount.Turret, RestYaw = 0f, ArcHalf = 38f * Deg, TraverseRate = 34f * Deg,
                RangeMax = 300f, Accuracy = 0.58f, ReloadSeconds = 4.2f, PenMm = 44f, ApDamage = 640f,
                HeDamage = 175f, HeRadius = 3.5f, HeSuppression = 45f, HeCrater = 0.9f, Mount3 = new float3(0f, 2.3f, 0.3f),
            },
        };

        /// <summary>Redoubt: a blockhouse that walks. No gun at all — a vision slit, a hatch and two heavy claws —
        /// but the thickest plate on the field and six legs to carry it. It is sent forward to be shot at.</summary>
        public static TankSpec Redoubt => new TankSpec
        {
            Hull = new ArmorProfile { FrontMm = 38f, SideMm = 26f, RearMm = 18f, TopMm = 20f },
            Turret = new ArmorProfile { FrontMm = 30f, SideMm = 24f, RearMm = 18f, TopMm = 16f },
            TurretChance = 0.18f, Crew = 2, GunCount = 0, ShortHalt = false, FuelRisk = 0.10f, AmmoRisk = 0.20f,
            Unmanned = true, ClawReach = 3.6f, ClawDamage = 1100f, ClawSeconds = 2.4f,
        };

        static TankGun CrabGun(TankMount mount, float rest, float3 at) => new TankGun
        {
            Mount = mount, RestYaw = rest, ArcHalf = 105f * Deg, TraverseRate = 60f * Deg,
            RangeMax = 200f, Accuracy = 0.5f, ReloadSeconds = 3.0f, PenMm = 34f, ApDamage = 500f,
            HeDamage = 150f, HeRadius = 3.2f, HeSuppression = 35f, HeCrater = 0.8f, Mount3 = at,
        };

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
