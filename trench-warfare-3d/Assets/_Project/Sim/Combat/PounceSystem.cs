// Phase: A5b (implemented 2026-09-28, lane/sim/melee) — the owner: "Crabs pounce" (decisions.md 2026-09-28: a crab
// with an enemy man about 10 m in front crouches for 0.6 s, leaps onto him and lands on him, then waits a cooldown).
// A crab is a walker with claws (VehicleProfile.Walker, TankSpec.ClawReach > 0: the six crabs and the Croaker). Its
// claws take what comes within reach of its front (TankGunnerySystem); a man a little further off it goes and gets:
//  - Idle, not knocked out, immobilised, stalled or bogged, its cooldown run out: the nearest enemy man on foot in
//    front of it (within Arc of its nose), beyond its claws and within Range past its hull's front, is the one. It
//    lands where he stands now, if that is ground it can stand on (CanLand: inside the map, no wall, bunker or trench
//    under its footprint, no other machine there, none of its own men under it), checked again as it leaves the ground;
//  - Crouched (CrouchTicks): it turns to him and stays put (VehicleKinematicsSystem.HaltTicks);
//  - In the air (AirTicks): it goes in a straight line to the landing point (drawn as an arc by the picture);
//  - Landed: every enemy man within LandRadius of the point takes LandDamage, less towards the edge (Hit, and Death
//    with the crab as his killer), and it waits CooldownTicks. A man who ran is not there. Once in the air it always
//    comes down where it was going, knocked out or not (stopped half way it hung over walls, critic r1).
// UnitFlags.Pouncing is set while it crouches and flies. Events: PounceCrouched, PounceLanded (SimEvents.cs).
// After Kinematics in the order, so its hold is honoured and its move is the last word; main thread, slot order. State:
// Phase, Ticks, From, To, Target, Cooldown, gen; all hashed.
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Sim.Combat
{
    public sealed class PounceSystem : ISimSystem
    {
        public const float Range = 10f;          // metres past its hull's front (the owner: "about 10 m in front")
        public const float Arc = 0.8f;           // radians either side of its nose
        public const int CrouchTicks = 12;       // 0.6 s at 20 Hz
        public const int AirTicks = 10;          // 0.5 s
        public const int CooldownTicks = 200;    // 10 s
        public const int LookEveryTicks = 5;     // an idle crab looks for a man this often
        public const float LandRadius = 3f;      // metres round the landing point its weight falls on (the owner: "a small radius";
                                                 // its half width + 1 m was 5.75 m on the Pincer, critic r1)
        public const float LandDamage = 400f;    // at the point; half that at the edge (a rifleman has 100)
        public const float LandKnock = 3f;       // m/s: the men it lands among are thrown outward
        public const byte Idle = 0, Crouched = 1, Airborne = 2;

        public int Order => SimSystemOrder.Pounce;

        readonly MapData map;
        CombatCatalogueSystem catalogue;
        VehicleKinematicsSystem kinematics;
        public NativeArray<byte> Phase;
        public NativeArray<short> Ticks, Cooldown;
        public NativeArray<float3> From, To;
        public NativeArray<int> Target;
        NativeArray<ushort> gen;

        public PounceSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            catalogue = world.GetSystem<CombatCatalogueSystem>() ?? throw new System.InvalidOperationException("PounceSystem needs CombatCatalogueSystem registered before it");
            kinematics = world.GetSystem<VehicleKinematicsSystem>() ?? throw new System.InvalidOperationException("PounceSystem needs VehicleKinematicsSystem registered before it");
            int n = world.Config.MaxSlots;
            Phase = new NativeArray<byte>(n, Allocator.Persistent);
            Ticks = new NativeArray<short>(n, Allocator.Persistent);
            Cooldown = new NativeArray<short>(n, Allocator.Persistent);
            From = new NativeArray<float3>(n, Allocator.Persistent);
            To = new NativeArray<float3>(n, Allocator.Persistent);
            Target = new NativeArray<int>(n, Allocator.Persistent);
            gen = new NativeArray<ushort>(n, Allocator.Persistent);
        }

        /// <summary>True for a machine that pounces: a walker with claws.</summary>
        public static bool Pounces(in VehicleProfile drive, in TankSpec spec) => drive.Walker && spec.ClawReach > 0f;

        public void Step(SimWorld w)
        {
            int n = w.HighWater;
            const uint pouncing = (uint)UnitFlags.Pouncing;
            for (int i = 0; i < n; i++)
            {
                uint f = w.Flags[i];
                if ((f & (uint)UnitFlags.Vehicle) == 0) continue;
                if (gen[i] != w.Generation[i]) { gen[i] = w.Generation[i]; Phase[i] = Idle; Ticks[i] = 0; Cooldown[i] = 0; Target[i] = -1; }
                byte arch = w.Archetype[i];
                var drive = kinematics.Profiles[arch];
                var spec = catalogue.Tank[arch];
                bool able = (f & (uint)UnitFlags.Alive) != 0 && (f & (uint)(UnitFlags.KnockedOut | UnitFlags.Immobilised | UnitFlags.Stalled | UnitFlags.Bogged)) == 0 && Pounces(drive, spec);
                if (Phase[i] == Airborne && (f & (uint)UnitFlags.Alive) != 0) able = true;   // in the air it comes down where it was going
                if (!able)
                {
                    if (Phase[i] != Idle) { Phase[i] = Idle; Ticks[i] = 0; }
                    if ((f & pouncing) != 0) w.Flags[i] = f & ~pouncing;
                    continue;
                }
                switch (Phase[i])
                {
                    case Idle:
                        if (Cooldown[i] > 0) { Cooldown[i]--; break; }
                        if ((w.Tick + (uint)i) % LookEveryTicks != 0) break;
                        int victim = Pick(w, i, drive, spec, out float3 land);
                        if (victim < 0) break;
                        Phase[i] = Crouched; Ticks[i] = CrouchTicks; Target[i] = victim;
                        From[i] = w.Position[i]; To[i] = land;
                        float3 d = land - w.Position[i]; d.y = 0f;
                        w.Yaw[i] = SimMath.YawOf(d);
                        w.Flags[i] = f | pouncing;
                        kinematics.HaltTicks[i] = math.max(kinematics.HaltTicks[i], CrouchTicks + AirTicks + 1);
                        w.Events.Add(w.Tick, SimEventType.PounceCrouched, i, victim, land, new float3(CrouchTicks, AirTicks, 0f));
                        break;
                    case Crouched:
                        w.Position[i] = From[i];
                        if (--Ticks[i] > 0) break;
                        // the ground may have filled while it crouched (a machine drove in, its own men walked under)
                        if (!CanLand(w, i, To[i], drive))
                        {
                            Phase[i] = Idle; Ticks[i] = 0; Cooldown[i] = CooldownTicks / 4; Target[i] = -1;
                            w.Flags[i] &= ~pouncing;   // (its hold runs out by itself: zeroing it cut a gun's halt short, critic r2)
                            break;
                        }
                        Phase[i] = Airborne; Ticks[i] = AirTicks;
                        break;
                    case Airborne:
                        int left = --Ticks[i];
                        float k = 1f - (float)left / AirTicks;
                        w.Position[i] = math.lerp(From[i], To[i], k);
                        w.Velocity[i] = left > 0 ? (To[i] - From[i]) / (AirTicks * w.Config.TickSeconds) : float3.zero;
                        if (left <= 0) Land(w, i, drive);
                        break;
                }
            }
        }

        /// <summary>The man a crab leaps at (-1 none), and where it lands: the nearest enemy man on foot in front of it,
        /// past its claws and within Range of its front, standing on ground it can land on.</summary>
        int Pick(SimWorld w, int i, in VehicleProfile drive, in TankSpec spec, out float3 land)
        {
            land = default;
            float3 p = w.Position[i];
            float near = spec.ClawReach + drive.HalfLength, far = drive.HalfLength + Range;
            int best = -1; float bestD = far * far;
            for (int j = 0; j < w.HighWater; j++)
            {
                uint fj = w.Flags[j];
                if (w.Team[j] == w.Team[i] || !MeleeSystem.OnFoot(fj)) continue;
                float3 d = w.Position[j] - p; d.y = 0f;
                float dsq = math.lengthsq(d);
                if (dsq >= bestD || dsq <= near * near) continue;
                if (math.abs(SimMath.WrapAngle(SimMath.YawOf(d) - w.Yaw[i])) > Arc) continue;
                if (!CanLand(w, i, w.Position[j], drive)) continue;
                best = j; bestD = dsq;
            }
            if (best >= 0) land = w.Position[best];
            return best;
        }

        /// <summary>Ground a crab can come down on: inside the map; no wall, bunker or trench under its middle or the
        /// corners of its footprint (a trench wider than it can cross strands it: critic r1); no other machine's hull
        /// within its own; none of its own men under it.</summary>
        bool CanLand(SimWorld w, int i, float3 at, in VehicleProfile drive)
        {
            float edge = drive.Reach;
            if (at.x < edge || at.z < edge || at.x > map.SizeMeters.x - edge || at.z > map.SizeMeters.y - edge) return false;
            const byte closed = (byte)(NavLayer.Blocked | NavLayer.Bunker | NavLayer.Trench | NavLayer.Link);
            // its footprint as it will come down, facing along its leap (square to the map it missed a nose or tail over
            // a trench, critic r2)
            float3 run = at - w.Position[i]; run.y = 0f;
            float yaw = math.lengthsq(run) > 1e-6f ? SimMath.YawOf(run) : w.Yaw[i];
            float sn = SimMath.Sin(yaw), cs = SimMath.Cos(yaw);
            for (int k = 0; k < 9; k++)
            {
                float ox = (k % 3 - 1) * drive.HalfWidth, oz = (k / 3 - 1) * drive.HalfLength;
                float wx = ox * cs + oz * sn, wz = -ox * sn + oz * cs;
                int cx = math.clamp((int)((at.x + wx) / MapData.NavCellSize), 0, map.NavWidth - 1);
                int cz = math.clamp((int)((at.z + wz) / MapData.NavCellSize), 0, map.NavLength - 1);
                if ((map.NavLayers[cz * map.NavWidth + cx] & closed) != 0) return false;
            }
            for (int j = 0; j < w.HighWater; j++)
            {
                if (j == i || (w.Flags[j] & (uint)UnitFlags.Alive) == 0) continue;
                float3 d = w.Position[j] - at; d.y = 0f;
                if ((w.Flags[j] & (uint)UnitFlags.Vehicle) != 0)
                {
                    float room = drive.HalfWidth + kinematics.Profiles[w.Archetype[j]].HalfWidth + 1f;
                    if (math.lengthsq(d) < room * room) return false;
                }
                else if (w.Team[j] == w.Team[i] && math.lengthsq(d) < LandRadius * LandRadius) return false;   // its own man
            }
            return true;
        }

        /// <summary>It comes down: the enemy men under it take its weight.</summary>
        void Land(SimWorld w, int i, in VehicleProfile drive)
        {
            float3 at = To[i];
            float radius = LandRadius;
            w.Position[i] = at;
            w.Flags[i] &= ~(uint)UnitFlags.Pouncing;
            Phase[i] = Idle; Ticks[i] = 0; Cooldown[i] = CooldownTicks; Target[i] = -1;
            w.Events.Add(w.Tick, SimEventType.PounceLanded, i, 0, at, default, radius);
            for (int j = 0; j < w.HighWater; j++)
            {
                if (w.Team[j] == w.Team[i] || !MeleeSystem.OnFoot(w.Flags[j])) continue;
                if ((w.Flags[j] & (uint)UnitFlags.InTrench) != 0 || w.TrenchId[j] >= 0) continue;   // below the parapet, as under a track
                float3 d = w.Position[j] - at; d.y = 0f;
                float dist = SimMath.Length(d);
                if (dist > radius) continue;
                float damage = LandDamage * (1f - 0.5f * dist / radius);
                float3 away = dist > 1e-3f ? d / dist : SimMath.DirFromYaw(w.Yaw[i]);
                w.Hp[j] = w.Hp[j] - damage;
                w.Events.Add(w.Tick, SimEventType.Hit, i, j, w.Position[j], away, damage);
                if (w.Hp[j] <= 0f)
                {
                    var fire = w.GetSystem<DirectFireSystem>();
                    if (fire != null) fire.Kills[w.Team[i] & 1]++;
                    w.Despawn(j, i, away, LandKnock);
                }
            }
        }

        public ulong Hash(ulong h)
        {
            if (!Phase.IsCreated) return h;
            h = SimHash.Array(Phase, h);
            h = SimHash.Array(Ticks, h);
            h = SimHash.Array(Cooldown, h);
            h = SimHash.Array(From, h);
            h = SimHash.Array(To, h);
            h = SimHash.Array(Target, h);
            return SimHash.Array(gen, h);
        }

        public void Dispose()
        {
            if (Phase.IsCreated) Phase.Dispose();
            if (Ticks.IsCreated) Ticks.Dispose();
            if (Cooldown.IsCreated) Cooldown.Dispose();
            if (From.IsCreated) From.Dispose();
            if (To.IsCreated) To.Dispose();
            if (Target.IsCreated) Target.Dispose();
            if (gen.IsCreated) gen.Dispose();
        }
    }
}
