// Phase: A5b (implemented) — depends on: TankSpec, Armor (through VehicleModulesSystem), BlastSystem (HE), HeightfieldRaycast,
// VehicleKinematicsSystem.HaltTicks, SimRandom.SystemId.Armor. The tanks' machine guns stay with DirectFireSystem.
// The tanks' main guns: the Maw's two sponson 6-pdrs, the Tusk's turret 37 mm. One sequential Burst job over the slots:
//  - a gun looks for a target every ScanEveryTicks (staggered by slot and gun) and re-checks its current one every
//    tick: an enemy inside its range and its arc (a sponson never swings through the hull), not a knocked-out hulk,
//    tanks first (a gun with penetration prefers armour at up to 2.5x the distance of a man), men below a parapet
//    last; the nearest three are tried for a line of sight from the gun's mount over the heightfield;
//  - it traverses at its rate toward the target (back to rest when it has none) and fires when it is laid within
//    AimTolerance, loaded and the target still in sight. The Tusk stops for HaltForShot to fire (short halt); the Maw
//    fires on the move at half accuracy. Reload stretches as the crew thins (CrewFactor, VehicleModulesSystem);
//  - against a vehicle it fires armour-piercing: a hit goes to PendingHits for VehicleModulesSystem to resolve against
//    the armour this tick; against men it fires high explosive, which always bursts somewhere: on the target when it
//    hits, scattered round it (wider with range) when it misses, and BlastSystem resolves the burst this tick.
// Every gun's yaw, reload and target, and the crew and gun health the damage system writes, are hashed.
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Sim.Combat
{
    public enum VehicleHitKind : byte { ArmourPiercing = 0, CloseAssault = 1 }

    /// <summary>A round or charge that reached a vehicle: VehicleModulesSystem decides what it did.</summary>
    public struct VehicleHit
    {
        public int Target, Shooter;
        public VehicleHitKind Kind;
        public float PenMm, Damage;
        public float3 Pos, Dir;   // where it struck, which way it was travelling
    }

    public sealed class TankGunnerySystem : ISimSystem
    {
        public const int Guns = TankSpec.MaxGuns;
        public const int ScanEveryTicks = 4;
        public const int HaltForShot = 24;
        public const float ClawArc = 1.0f;            // rad either side of the nose a claw can reach
        public const float ClawPenMm = 26f;           // what a claw is worth against armour: it tears plates apart
        public const float SelfSafeScatter = 2f;   // metres: how far short a miss at close range can fall            // the Tusk stands 1.2 s to lay and fire
        public const float AimTolerance = 0.05f;      // rad (about 3 degrees)
        public const float MovingAccuracy = 0.5f;
        public const float ArmourPreference = 2.5f;   // a gun that can hole armour looks at tanks this much farther out than at men
        public const int WeaponIdBase = 40;           // Explosion.a for a tank shell: 40 + archetype
        public int Order => SimSystemOrder.DirectFire + 5;

        readonly MapData map;
        SimWorld world;
        BlastSystem blast;
        VehicleKinematicsSystem kinematics;

        // ---- per slot × gun, hashed ----
        public NativeArray<float> GunYaw;       // relative to the hull
        public NativeArray<int> Reload;         // ticks until it can fire
        public NativeArray<int> GunTarget;      // slot, -1 none
        public NativeArray<float> GunHealth;    // VehicleModulesSystem: 0 = knocked out
        // ---- per slot, hashed ----
        public NativeArray<ushort> Gen;
        public NativeArray<float> CrewFactor;   // VehicleModulesSystem: the men left to load and lay (0 = cannot fire)
        public NativeArray<int> ClawCooldown;   // ticks until a walker's claws can close again
        /// <summary>This tick's rounds and charges on vehicles (close assaults from DirectFire, then these guns);
        /// VehicleModulesSystem drains it later in the same tick.</summary>
        public NativeList<VehicleHit> PendingHits;

        NativeList<SimEvent> events;
        NativeList<Impact> impacts;
        NativeList<int2> clawed;                // (victim, walker) this tick: men taken in a claw, resolved below
        NativeArray<int> halt;                  // stand-in when no kinematics system is registered

        public TankGunnerySystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld w)
        {
            world = w;
            int n = w.Config.MaxSlots;
            GunYaw = new NativeArray<float>(n * Guns, Allocator.Persistent);
            Reload = new NativeArray<int>(n * Guns, Allocator.Persistent);
            GunTarget = new NativeArray<int>(n * Guns, Allocator.Persistent);
            GunHealth = new NativeArray<float>(n * Guns, Allocator.Persistent);
            Gen = new NativeArray<ushort>(n, Allocator.Persistent);
            CrewFactor = new NativeArray<float>(n, Allocator.Persistent);
            ClawCooldown = new NativeArray<int>(n, Allocator.Persistent);
            clawed = new NativeList<int2>(8, Allocator.Persistent);
            for (int i = 0; i < n * Guns; i++) GunTarget[i] = -1;
            PendingHits = new NativeList<VehicleHit>(32, Allocator.Persistent);
            events = new NativeList<SimEvent>(32, Allocator.Persistent);
            impacts = new NativeList<Impact>(16, Allocator.Persistent);
            halt = new NativeArray<int>(n, Allocator.Persistent);
        }

        public void Step(SimWorld w)
        {
            int n = w.HighWater;
            if (n == 0) return;
            if (blast == null) blast = w.GetSystem<BlastSystem>();
            if (kinematics == null) kinematics = w.GetSystem<VehicleKinematicsSystem>();
            events.Clear(); impacts.Clear(); clawed.Clear();
            new GunneryJob
            {
                Count = n, Tick = w.Tick, Seed = w.Config.Seed, Dt = w.Config.TickSeconds,
                Position = w.Position, Velocity = w.Velocity, Yaw = w.Yaw, Flags = w.Flags, Team = w.Team, Archetype = w.Archetype,
                StanceOf = w.StanceOf, Generation = w.Generation,
                GunYaw = GunYaw, Reload = Reload, GunTarget = GunTarget, GunHealth = GunHealth, Gen = Gen, CrewFactor = CrewFactor,
                ClawCooldown = ClawCooldown, Clawed = clawed,
                HaltTicks = kinematics != null ? kinematics.HaltTicks : halt,
                Height = map.Height, Layers = map.NavLayers, CellCover = map.CellCover, NavWidth = map.NavWidth, NavLength = map.NavLength,
                Hits = PendingHits, Events = events, Impacts = impacts,
            }.Run();
            for (int e = 0; e < events.Length; e++) w.Events.Add(events[e]);
            if (blast != null) for (int k = 0; k < impacts.Length; k++) blast.Queue(impacts[k]);
            // a claw that closed on a man: the job cannot kill him (it never writes Hp), so it is done here, the way
            // VehicleKinematicsSystem finishes off the men it runs over
            for (int c = 0; c < clawed.Length; c++)
            {
                int j = clawed[c].x, i = clawed[c].y;
                if (!w.IsAlive(j)) continue;
                w.Hp[j] = w.Hp[j] - TankSpec.For(w.Archetype[i]).ClawDamage;
                w.Events.Add(w.Tick, SimEventType.VehicleClawed, i, j, w.Position[j]);
                if (w.Hp[j] <= 0f) w.Despawn(j, i, SimMath.DirFromYaw(w.Yaw[i]) * 0.5f);
            }
        }

        /// <summary>World position of a gun's mount on a hull (for line of sight and for drawing).</summary>
        public static float3 MountWorld(in TankGun g, float3 hull, float yaw, in Heightfield height)
        {
            float s = SimMath.Sin(yaw), c = SimMath.Cos(yaw);
            float3 at = hull + new float3(g.Mount3.x * c + g.Mount3.z * s, 0f, -g.Mount3.x * s + g.Mount3.z * c);
            at.y = height.Sample(hull.x, hull.z) + g.Mount3.y;
            return at;
        }

        [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct GunneryJob : IJob
        {
            public int Count, NavWidth, NavLength;
            public uint Tick, Seed;
            public float Dt;
            [ReadOnly] public NativeArray<float3> Position, Velocity;
            [ReadOnly] public NativeArray<float> Yaw;
            [ReadOnly] public NativeArray<uint> Flags;
            [ReadOnly] public NativeArray<byte> Team, Archetype, StanceOf;
            [ReadOnly] public NativeArray<ushort> Generation;
            public NativeArray<float> GunYaw, GunHealth, CrewFactor;
            public NativeArray<int> Reload, GunTarget, HaltTicks, ClawCooldown;
            public NativeList<int2> Clawed;
            public NativeArray<ushort> Gen;
            [ReadOnly] public Heightfield Height;
            [ReadOnly] public NativeArray<byte> Layers, CellCover;
            public NativeList<VehicleHit> Hits;
            public NativeList<SimEvent> Events;
            public NativeList<Impact> Impacts;

            int CellOf(float3 p)
            {
                int cx = math.clamp((int)(p.x / MapData.NavCellSize), 0, NavWidth - 1);
                int cz = math.clamp((int)(p.z / MapData.NavCellSize), 0, NavLength - 1);
                return cz * NavWidth + cx;
            }

            bool Enemy(int i, int j)
            {
                uint fj = Flags[j];
                return j != i && (fj & (uint)UnitFlags.Alive) != 0 && (fj & (uint)UnitFlags.KnockedOut) == 0 && Team[j] != Team[i];
            }

            float3 AimPoint(int j)
            {
                float3 q = Position[j];
                float ground = Height.Sample(q.x, q.z);
                if ((Flags[j] & (uint)UnitFlags.Vehicle) != 0) return new float3(q.x, ground + 1.8f, q.z);
                return new float3(q.x, ground + ((Flags[j] & (uint)UnitFlags.InTrench) != 0 ? 0.3f : HeightfieldRaycast.EyeHeight((Stance)StanceOf[j]) * 0.7f), q.z);
            }

            /// <summary>Distance-like score, lower is better; float.MaxValue = not a target for this gun.</summary>
            float Score(int i, int j, float3 p, float hullYaw, in TankGun g)
            {
                if (!Enemy(i, j)) return float.MaxValue;
                float3 d = Position[j] - p; d.y = 0f;
                float dsq = math.lengthsq(d);
                if (dsq > g.RangeMax * g.RangeMax) return float.MaxValue;
                if (g.RangeMin > 0f && dsq < g.RangeMin * g.RangeMin) return float.MaxValue;   // a mortar cannot drop one on its own feet
                if (!TankSpec.InArc(g, SimMath.WrapAngle(SimMath.YawOf(d) - hullYaw))) return float.MaxValue;
                float dist = SimMath.Sqrt(dsq);
                if ((Flags[j] & (uint)UnitFlags.Vehicle) != 0) return g.PenMm > 0f ? dist / ArmourPreference : float.MaxValue;
                // its own burst could reach its hull (the burst, its half width, and a miss falling short): leave him to the
                // machine guns (VehicleModules bursts reach Radius + HalfWidth from a hull's centre)
                if (dist < g.HeRadius + VehicleProfile.ForArchetype(Archetype[i]).HalfWidth + SelfSafeScatter) return float.MaxValue;
                bool belowRim = (Flags[j] & (uint)UnitFlags.InTrench) != 0 && StanceOf[j] != (byte)Stance.FireStep;
                // a flat-trajectory gun can barely touch a man below the parapet; a mortar is the answer to him
                return belowRim ? dist * (g.Indirect ? 0.55f : 1.8f) : dist;
            }

            bool Sees(int i, int j, in TankGun g)
                => g.Indirect || HeightfieldRaycast.HasLineOfSight(Height, MountWorld(g, Position[i], Yaw[i], Height), AimPoint(j));

            int Pick(int i, float3 p, float hullYaw, in TankGun g)
            {
                int b0 = -1, b1 = -1, b2 = -1;
                float s0 = float.MaxValue, s1 = float.MaxValue, s2 = float.MaxValue;
                for (int j = 0; j < Count; j++)
                {
                    float s = Score(i, j, p, hullYaw, g);
                    if (s == float.MaxValue) continue;
                    if (s < s0) { s2 = s1; b2 = b1; s1 = s0; b1 = b0; s0 = s; b0 = j; }
                    else if (s < s1) { s2 = s1; b2 = b1; s1 = s; b1 = j; }
                    else if (s < s2) { s2 = s; b2 = j; }
                }
                if (b0 >= 0 && Sees(i, b0, g)) return b0;
                if (b1 >= 0 && Sees(i, b1, g)) return b1;
                if (b2 >= 0 && Sees(i, b2, g)) return b2;
                return -1;
            }

            public void Execute()
            {
                for (int i = 0; i < Count; i++)
                {
                    uint f = Flags[i];
                    if ((f & ((uint)UnitFlags.Alive | (uint)UnitFlags.Vehicle)) != ((uint)UnitFlags.Alive | (uint)UnitFlags.Vehicle) || !VehicleArchetype.IsArmoured(Archetype[i])) continue;
                    var spec = TankSpec.For(Archetype[i]);
                    if (Gen[i] != Generation[i])
                    {
                        Gen[i] = Generation[i]; CrewFactor[i] = 1f;
                        for (int k = 0; k < Guns; k++)
                        {
                            int g0 = i * Guns + k;
                            GunYaw[g0] = k < spec.GunCount ? spec.Gun(k).RestYaw : 0f; Reload[g0] = 30 + 10 * k; GunTarget[g0] = -1; GunHealth[g0] = 1f;
                        }
                    }
                    if ((f & (uint)UnitFlags.KnockedOut) != 0) { for (int k = 0; k < Guns; k++) GunTarget[i * Guns + k] = -1; continue; }
                    float3 p = Position[i];
                    float hullYaw = Yaw[i];
                    bool moving = SimMath.Length(Velocity[i]) > 0.3f;
                    for (int k = 0; k < spec.GunCount; k++)
                    {
                        int gi = i * Guns + k;
                        var g = spec.Gun(k);
                        if (Reload[gi] > 0) Reload[gi]--;
                        if (GunHealth[gi] <= 0f || CrewFactor[i] <= 0f) { GunTarget[gi] = -1; continue; }

                        int t = GunTarget[gi];
                        if (t >= 0 && Score(i, t, p, hullYaw, g) == float.MaxValue) t = -1;
                        if (t < 0 || ((uint)(i + k) % ScanEveryTicks) == Tick % ScanEveryTicks) t = Pick(i, p, hullYaw, g);
                        GunTarget[gi] = t;

                        // lay the gun: toward the target, or back to rest
                        float rel = t >= 0 ? SimMath.WrapAngle(SimMath.YawOf(Position[t] - p) - hullYaw) : g.RestYaw;
                        float want = TankSpec.ClampToArc(g, rel);
                        float cur = GunYaw[gi];
                        float step = g.TraverseRate * Dt;
                        cur = TankSpec.ClampToArc(g, cur + math.clamp(SimMath.WrapAngle(want - cur), -step, step));
                        GunYaw[gi] = cur;
                        if (t < 0 || Reload[gi] > 0 || math.abs(SimMath.WrapAngle(rel - cur)) > AimTolerance) continue;
                        if (spec.ShortHalt && moving) { HaltTicks[i] = math.max(HaltTicks[i], HaltForShot); continue; }
                        if (!Sees(i, t, g)) { GunTarget[gi] = -1; continue; }

                        // fire
                        Reload[gi] = (int)math.round(g.ReloadSeconds / Dt / math.max(0.25f, CrewFactor[i]));
                        var rng = SimRandom.For(Seed, Tick, SimRandom.SystemId.Armor, (uint)gi);
                        float3 q = Position[t];
                        float3 d = q - p; d.y = 0f;
                        float dist = SimMath.Length(d);
                        bool armour = (Flags[t] & (uint)UnitFlags.Vehicle) != 0;
                        float chance = g.Accuracy * CombatTables.RangeFalloff(dist, g.RangeMax) * (moving ? MovingAccuracy : 1f) * (0.6f + 0.4f * CrewFactor[i]);
                        if (armour) chance *= 1.3f;   // a tank is a big target
                        else
                        {
                            if ((Flags[t] & (uint)UnitFlags.InTrench) != 0) chance *= StanceOf[t] == (byte)Stance.FireStep ? 0.8f : 0.5f;
                            else if (StanceOf[t] == (byte)Stance.Prone || StanceOf[t] == (byte)Stance.Pinned) chance *= 0.7f;
                            if ((Layers[CellOf(q)] & (byte)NavLayer.Crater) != 0) chance *= 0.75f;
                            chance *= 1f - CellCover[CellOf(q)] * 0.01f;
                        }
                        chance = math.clamp(chance, 0.04f, 0.9f);
                        bool hit = rng.NextFloat() < chance;
                        float3 land = q;
                        if (!hit)
                        {
                            // the round goes wide or long: scatter grows with range, biased along the line of fire
                            float spread = 1.5f + dist * 0.035f;
                            float a = rng.NextFloat(0f, SimMath.TwoPi), r = spread * SimMath.Sqrt(rng.NextFloat(0.15f, 1f));
                            float3 along = dist > 1e-3f ? d / dist : SimMath.DirFromYaw(hullYaw);
                            land = q + new float3(SimMath.Sin(a) * r, 0f, SimMath.Cos(a) * r) + along * (r * 0.6f);
                        }
                        float3 shot = land - p; shot.y = 0f;
                        float sl = SimMath.Length(shot);
                        float3 dir = sl > 1e-3f ? shot / sl : SimMath.DirFromYaw(hullYaw + cur);
                        if (armour && g.PenMm > 0f)
                        {
                            if (hit) Hits.Add(new VehicleHit { Target = t, Shooter = i, Kind = VehicleHitKind.ArmourPiercing, PenMm = g.PenMm * rng.NextFloat(0.9f, 1.1f), Damage = g.ApDamage, Pos = AimPoint(t), Dir = dir });
                            Events.Add(new SimEvent { Tick = Tick, Type = SimEventType.VehicleFired, A = i, B = k, Pos = land, Dir = dir, Scalar = 0f });
                        }
                        else
                        {
                            Impacts.Add(new Impact
                            {
                                Pos = land, Damage = g.HeDamage, Radius = g.HeRadius, Suppression = g.HeSuppression,
                                CraterRadius = g.HeCrater, CraterDepth = g.HeCrater * 0.25f, Source = WeaponIdBase + Archetype[i], Player = Team[i],
                            });
                            Events.Add(new SimEvent { Tick = Tick, Type = SimEventType.VehicleFired, A = i, B = k, Pos = land, Dir = dir, Scalar = 1f });
                        }
                    }

                    // The claws. A walker takes hold of whatever comes within reach of its front and crushes it: a man
                    // dies, a hull takes it as a close assault against its armour. It is what a walker has instead of
                    // the machine guns a tank carries, and the reason infantry cannot simply walk up to one.
                    if (spec.ClawReach > 0f && ClawCooldown[i] <= 0)
                    {
                        float reach = spec.ClawReach + VehicleProfile.ForArchetype(Archetype[i]).HalfLength;
                        int victim = -1; float best = reach * reach;
                        for (int j = 0; j < Count; j++)
                        {
                            if (!Enemy(i, j)) continue;
                            float3 d = Position[j] - p; d.y = 0f;
                            float dsq = math.lengthsq(d);
                            if (dsq >= best) continue;
                            if (math.abs(SimMath.WrapAngle(SimMath.YawOf(d) - hullYaw)) > ClawArc) continue;   // in front of it, not behind
                            best = dsq; victim = j;
                        }
                        if (victim >= 0)
                        {
                            ClawCooldown[i] = (int)math.round(spec.ClawSeconds / Dt);
                            float3 to = Position[victim] - p; to.y = 0f;
                            float len = SimMath.Length(to);
                            float3 dir = len > 1e-3f ? to / len : SimMath.DirFromYaw(hullYaw);
                            if ((Flags[victim] & (uint)UnitFlags.Vehicle) != 0)
                            {
                                Hits.Add(new VehicleHit { Target = victim, Shooter = i, Kind = VehicleHitKind.CloseAssault, PenMm = ClawPenMm, Damage = spec.ClawDamage, Pos = Position[victim], Dir = dir });
                                Events.Add(new SimEvent { Tick = Tick, Type = SimEventType.VehicleClawed, A = i, B = victim, Pos = Position[victim], Dir = dir });
                            }
                            else Clawed.Add(new int2(victim, i));   // the system finishes him: this job never writes Hp
                        }
                    }
                    else if (ClawCooldown[i] > 0) ClawCooldown[i]--;
                }
            }
        }

        public ulong Hash(ulong h)
        {
            int n = world.HighWater;
            h = SimHash.Array(GunYaw, n * Guns, h);
            h = SimHash.Array(Reload, n * Guns, h);
            h = SimHash.Array(GunTarget, n * Guns, h);
            h = SimHash.Array(GunHealth, n * Guns, h);
            h = SimHash.Array(Gen, n, h);
            h = SimHash.Array(CrewFactor, n, h);
            h = SimHash.Array(ClawCooldown, n, h);
            return SimHash.Value(PendingHits.Length, h);   // empty between ticks
        }

        public void Dispose()
        {
            if (GunYaw.IsCreated) GunYaw.Dispose();
            if (Reload.IsCreated) Reload.Dispose();
            if (GunTarget.IsCreated) GunTarget.Dispose();
            if (GunHealth.IsCreated) GunHealth.Dispose();
            if (Gen.IsCreated) Gen.Dispose();
            if (CrewFactor.IsCreated) CrewFactor.Dispose();
            if (ClawCooldown.IsCreated) ClawCooldown.Dispose();
            if (clawed.IsCreated) clawed.Dispose();
            if (PendingHits.IsCreated) PendingHits.Dispose();
            if (events.IsCreated) events.Dispose();
            if (impacts.IsCreated) impacts.Dispose();
            if (halt.IsCreated) halt.Dispose();
        }
    }
}
