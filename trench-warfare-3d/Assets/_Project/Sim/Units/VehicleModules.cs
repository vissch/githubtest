// Phase: A5b (implemented) — depends on: Armor, TankSpec, TankGunnerySystem (PendingHits, CrewFactor, GunHealth),
// BlastSystem (Resolved, Queue), VehicleKinematicsSystem (SpeedFactor), SimRandom.SystemId.Armor
// What happens to a tank when it is hit, and how it dies. Main thread, slot order, after the tick's shots and bursts:
//  1. hits (TankGunnerySystem.PendingHits: grenade bundles from DirectFire, then tank rounds): the round meets the plate
//     it strikes (Armor: facing, incidence; a Tusk's turret takes TurretChance of the rounds, on the turret's plates;
//     a bundle on the deck meets the top plate). Stopped: a little structure, maybe a shaken crew, maybe a track (it
//     sits outside the armour). Holed: structure damage, spalling may kill a man, and one module is struck, rolled by
//     facing: the front holds the crew, the sides the tracks and guns, the rear the engine and fuel (ModuleRoll).
//  2. bursts (BlastSystem.Resolved): a shell on the hull comes through the top plate if it has the weight (damage/10 mm);
//     a near miss shakes the crew and can break the nearer track.
//  3. modules: a broken track immobilises; a broken engine stalls (a hurt one runs at 55 %); a broken gun is silent; a
//     holed fuel tank starts a fire; a holed rack burns, or (AmmoRisk) sets the rounds off: a cook-off in 1..3.5 s.
//  4. fire grows by itself (quicker with the fuel holed) and burns the structure; while it is small the crew fight it
//     (ExtinguishChance a second, fewer men put out fewer fires). Past BailFire they get out. At 1 the racks go up.
//     A hit that takes the structure past -ObliterateShare of its maximum (a heavy shell square on the deck) blows it
//     apart at once: the rounds go up with it, nobody gets out, and the wreck is there the same tick.
//  5. knocked out (crew all dead, structure gone, fire, or the rounds going): the crew that is left bail out as
//     riflemen, the hulk is Immobilised | KnockedOut (no target, fires nothing) and burns until it cooks off, or
//     smoulders BurnOut ticks; then it is despawned (VehicleDestroyed: DeformationSystem leaves a wreck). A cook-off
//     queues an explosion that hurts whoever stands close (it bursts the next tick, BlastSystem having run).
//  6. repairs: a tank not hit for RepairQuietTicks mends its worst broken track, engine or gun to RepairTo every
//     RepairTicks, while it has a crew.
// Crew factor and speed factor go to the gunnery and kinematics systems; the flags carry the rest. All state is hashed.
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Combat;
using TW.Sim.Nav;

namespace TW.Sim.Units
{
    public enum VehicleState : byte { Active = 0, KnockedOut = 1, CookingOff = 2 }

    public sealed class VehicleModulesSystem : ISimSystem
    {
        public const int RepairQuietTicks = 200, RepairTicks = 400;
        public const float RepairTo = 0.6f;
        // A tank's track used to be a switch: whole, or thrown and the tank stuck where it stood. A walker's legs
        // have always been graded (16 % a leg, it limps long before it stops) and the owner asked for the same of
        // armour. Above TrackFullAbove a track is sound; from there down to TrackThrownBelow the tank drags at
        // TrackWorstFactor of its speed; below that the track is OFF and the tank is Immobilised -- which used to
        // take reaching exactly zero. RepairTo 0.6 therefore returns a mended track to 0.78, not to full speed.
        public const float TrackFullAbove = 0.8f, TrackThrownBelow = 0.2f, TrackWorstFactor = 0.35f;
        /// <summary>An uneven pair costs a little more again: a tank with one torn track crabs rather than driving.</summary>
        public const float TrackMismatch = 0.15f;
        public const float EngineFullAbove = 0.7f, StalledBelow = 0.2f, EngineWorstFactor = 0.45f;
        /// <summary>What a walker loses per leg gone, whatever side it was on, and the floor it limps at.</summary>
        public const float LegLoss = 0.16f, LegFloor = 0.25f;
        public const float FireGrowth = 0.035f, FireBurn = 14f, ExtinguishChance = 0.15f, BailFire = 0.6f;
        public const int CookOffMin = 20, CookOffMax = 70, BurnOutMin = 240, BurnOutMax = 480;
        public const float CookOffDamage = 380f, CookOffRadius = 9f, CookOffSuppression = 70f, CookOffCrater = 2f;
        public const int CookOffSource = SourceId.CookOff;   // Explosion.a for a tank blowing up
        public const float BailedHp = 50f, BailedSpeed = 3f;
        public const float ObliterateShare = 0.5f;
        public const uint StandardEvery = 5;   // ticks between two sweeps of a Banner's standard
        const int M = (int)VehicleModule.Count;
        const int Guns = TankGunnerySystem.Guns;

        public int Order => SimSystemOrder.Blast + 10;

        readonly Terrain.MapData map;
        SimWorld world;
        TankGunnerySystem gunnery;
        BlastSystem blast;
        GasSmokeSystem gas;
        VehicleKinematicsSystem kinematics;

        // ---- per slot, hashed ----
        public NativeArray<ushort> Gen;
        public NativeArray<float> Module;        // slot × VehicleModule: 1 whole .. 0 broken
        public NativeArray<byte> Crew, CrewMax;
        public NativeArray<float> Fire;          // 0 none .. 1 the racks go up
        public NativeArray<byte> State;          // VehicleState
        public NativeArray<int> StateTicks;      // cook-off fuse, or how long a hulk smoulders
        public NativeArray<uint> LastHitTick;
        public NativeArray<int> Shaken;          // ticks the crew is rattled (the guns barely work)
        public NativeArray<int> Repair;          // quiet ticks toward the next repair
        public NativeArray<int> LastShooter;     // for the kill
        /// <summary>A walker's legs that are gone: one bit each, the left side's first (VehicleProfile.Legs). A hit
        /// that would break a track takes whole legs off instead, and the side fails when its last one goes.</summary>
        public NativeArray<byte> LegsLost;
        public int Penetrations, Ricochets, KnockOuts, CookOffs, BailedOut;
        ulong checksum = SimHash.Offset;
        int hitSerial;                           // per tick: one random stream per resolved hit

        public VehicleModulesSystem(Terrain.MapData map) { this.map = map; }

        public void Initialize(SimWorld w)
        {
            world = w;
            kinematics = w.GetSystem<VehicleKinematicsSystem>() ?? throw new System.InvalidOperationException("VehicleModulesSystem needs VehicleKinematicsSystem registered before it");
            gunnery = w.GetSystem<TankGunnerySystem>();   // null without combat
            blast = w.GetSystem<BlastSystem>();
            gas = w.GetSystem<GasSmokeSystem>();          // null without combat: the Censer's drum needs it
            int n = w.Config.MaxSlots;
            Gen = new NativeArray<ushort>(n, Allocator.Persistent);
            Module = new NativeArray<float>(n * M, Allocator.Persistent);
            Crew = new NativeArray<byte>(n, Allocator.Persistent);
            CrewMax = new NativeArray<byte>(n, Allocator.Persistent);
            Fire = new NativeArray<float>(n, Allocator.Persistent);
            State = new NativeArray<byte>(n, Allocator.Persistent);
            StateTicks = new NativeArray<int>(n, Allocator.Persistent);
            LastHitTick = new NativeArray<uint>(n, Allocator.Persistent);
            Shaken = new NativeArray<int>(n, Allocator.Persistent);
            Repair = new NativeArray<int>(n, Allocator.Persistent);
            LastShooter = new NativeArray<int>(n, Allocator.Persistent);
            LegsLost = new NativeArray<byte>(n, Allocator.Persistent);
            KillCause = new NativeArray<byte>(n, Allocator.Persistent);
            Fate = new NativeArray<byte>(n, Allocator.Persistent);
            for (int i = 0; i < n; i++) { KillCause[i] = Alive; Fate[i] = Alive; }
        }

        /// <summary>Why each vehicle stopped fighting (VehicleKillCause), or Alive. Kept until the slot is re-used so
        /// the wreck record (DeformationSystem, the same tick as VehicleDestroyed) can read it.</summary>
        public NativeArray<byte> KillCause;

        /// <summary>What became of the hull after the blow that stopped it (VehicleKillCause), or Alive if nothing more:
        /// a shelled hull that then burned through or cooked off is worth what is left of it, not what stopped it. The
        /// record keeps the blow (KillCause); the salvage cut follows the fate.</summary>
        public NativeArray<byte> Fate;
        public const byte Alive = 255;

        /// <summary>What is left of a machine as salvage, 0..1: the mean of its running gear, engine and guns, cut
        /// by how it died (a burnt-out hull is half a hull, a cooked-off one is scrap).</summary>
        public float QualityOf(SimWorld w, int slot)
        {
            var spec = TankSpec.For(w.Archetype[slot]);
            float sum = Module[slot * M + (int)VehicleModule.TrackLeft] + Module[slot * M + (int)VehicleModule.TrackRight] + Module[slot * M + (int)VehicleModule.Engine];
            int n = 3;
            if (spec.GunCount > 0) { sum += Module[slot * M + (int)VehicleModule.GunA]; n++; }
            if (spec.GunCount > 1) { sum += Module[slot * M + (int)VehicleModule.GunB]; n++; }
            byte end = Fate[slot] != Alive ? Fate[slot] : KillCause[slot];
            float cause = end == (byte)VehicleKillCause.Fire ? 0.5f : end == (byte)VehicleKillCause.Ammunition ? 0.15f : 0.85f;
            return math.saturate(sum / n * cause);
        }

        static bool IsTank(SimWorld w, int i)
            => (w.Flags[i] & ((uint)UnitFlags.Alive | (uint)UnitFlags.Vehicle)) == ((uint)UnitFlags.Alive | (uint)UnitFlags.Vehicle) && VehicleArchetype.IsArmoured(w.Archetype[i]);

        /// <summary>How many of a walker's legs on one side are gone.</summary>
        int LegsGone(int slot, bool right, int perSide)
        {
            int n = 0;
            for (int k = 0; k < perSide; k++) if ((LegsLost[slot] & (1 << (right ? perSide + k : k))) != 0) n++;
            return n;
        }

        public float ModuleOf(int slot, VehicleModule m) => Module[slot * M + (int)m];

        public void Step(SimWorld w)
        {
            hitSerial = 0;
            int n = w.HighWater;
            for (int i = 0; i < n; i++) if (IsTank(w, i) && Gen[i] != w.Generation[i]) Init(w, i);
            if (gunnery != null)
            {
                for (int h = 0; h < gunnery.PendingHits.Length; h++) Resolve(w, gunnery.PendingHits[h]);
                gunnery.PendingHits.Clear();
            }
            if (blast != null) for (int k = 0; k < blast.Resolved.Length; k++) Burst(w, blast.Resolved[k], k);
            for (int i = 0; i < n; i++) if (IsTank(w, i)) Tick(w, i);
        }

        void Init(SimWorld w, int i)
        {
            var spec = TankSpec.For(w.Archetype[i]);
            Gen[i] = w.Generation[i];
            for (int m = 0; m < M; m++) Module[i * M + m] = 1f;
            Crew[i] = CrewMax[i] = spec.Crew;
            Fire[i] = 0f; State[i] = (byte)VehicleState.Active; StateTicks[i] = 0; LastHitTick[i] = w.Tick; Shaken[i] = 0; Repair[i] = 0; LastShooter[i] = -1;
            LegsLost[i] = 0; KillCause[i] = Alive; Fate[i] = Alive;
            kinematics.SpeedFactor[i] = 1f;
            if (gunnery != null) { gunnery.CrewFactor[i] = 1f; for (int k = 0; k < Guns; k++) gunnery.GunHealth[i * Guns + k] = 1f; }
        }

        Random Dice(SimWorld w, uint salt) => SimRandom.For(w.Config.Seed, w.Tick, SimRandom.SystemId.Armor, 0x40000000u + salt);

        // ------------------------------------------------------------------ hits
        void Resolve(SimWorld w, VehicleHit hit)
        {
            int t = hit.Target;
            if (!IsTank(w, t)) return;
            var rng = Dice(w, (uint)hitSerial++);
            var spec = TankSpec.For(w.Archetype[t]);
            LastHitTick[t] = w.Tick; Repair[t] = 0;
            if (hit.Shooter >= 0) LastShooter[t] = hit.Shooter;
            bool turret = hit.Kind == VehicleHitKind.ArmourPiercing && spec.TurretChance > 0f && rng.NextFloat() < spec.TurretChance;
            ArmourFacing facing; bool right; bool holed; float plate;
            var hull = spec.Hull;
            if (spec.ChargingTopMm > 0f && (w.Flags[t] & (uint)UnitFlags.Charging) != 0) hull.TopMm = spec.ChargingTopMm;   // hatches down: the deck a bundle lands on
            if (hit.Kind == VehicleHitKind.CloseAssault)
            {
                facing = ArmourFacing.Top; right = rng.NextFloat() < 0.5f;
                holed = Armor.PenetratesTop(hull, hit.PenMm, out plate);
            }
            else
            {
                float yaw = w.Yaw[t] + (turret && gunnery != null ? gunnery.GunYaw[t * Guns] : 0f);
                facing = Armor.FacingOf(hit.Dir, yaw, out float c, out right);
                plate = Armor.PlateFor(turret ? spec.Turret : hull, facing);
                holed = c >= Armor.GlanceCos && hit.PenMm * c >= plate;
            }
            checksum = SimHash.Value(new int4(t, (int)facing, holed ? 1 : 0, turret ? 1 : 0), checksum);
            if (State[t] != (byte)VehicleState.Active)
            {
                // a hulk: every hole feeds the fire
                if (holed && Fire[t] > 0f) Fire[t] = math.min(1f, Fire[t] + 0.2f);
                w.Events.Add(w.Tick, SimEventType.Hit, hit.Shooter, t, hit.Pos, hit.Dir, holed ? 0f : -plate);
                w.Events.Add(w.Tick, SimEventType.VehicleArmourHit, t, hit.Shooter, hit.Pos, hit.Dir, holed ? plate : -plate);
                return;
            }
            if (holed)
            {
                float dmg = hit.Damage * rng.NextFloat(0.8f, 1.2f);
                w.Hp[t] = w.Hp[t] - dmg;
                Penetrations++;
                w.Events.Add(w.Tick, SimEventType.Hit, hit.Shooter, t, hit.Pos, hit.Dir, dmg);
                w.Events.Add(w.Tick, SimEventType.VehicleArmourHit, t, hit.Shooter, hit.Pos, hit.Dir, plate);
                StrikeModule(w, t, spec, ModuleRoll(spec, facing, turret, right, rng.NextFloat()), ref rng);
                if (rng.NextFloat() < (hit.Kind == VehicleHitKind.CloseAssault ? 0.25f : 0.35f)) LoseCrew(w, t, 1);   // splinters inside
                Shaken[t] = math.max(Shaken[t], 40);
            }
            else
            {
                w.Hp[t] = w.Hp[t] - hit.Damage * 0.04f;   // the plate rings, rivet heads fly off inside
                Ricochets++;
                w.Events.Add(w.Tick, SimEventType.Hit, hit.Shooter, t, hit.Pos, hit.Dir, -plate);
                w.Events.Add(w.Tick, SimEventType.VehicleArmourHit, t, hit.Shooter, hit.Pos, hit.Dir, -plate);
                if (rng.NextFloat() < 0.2f) Shaken[t] = math.max(Shaken[t], 30);
                if (facing == ArmourFacing.Side && hit.PenMm >= 15f && rng.NextFloat() < 0.15f)   // the tracks are outside the armour
                    DamageModule(w, t, spec, right ? VehicleModule.TrackRight : VehicleModule.TrackLeft, 0.6f, ref rng);
            }
            if (w.Hp[t] <= -ObliterateShare * w.MaxHp[t]) Obliterate(w, t, ref rng);
            else if (w.Hp[t] <= 0f) KnockOut(w, t, VehicleKillCause.Structure, ref rng);
        }

        /// <summary>Which module a round that came through the struck plate hits (cumulative weights, out of 100).</summary>
        static VehicleModule ModuleRoll(in TankSpec spec, ArmourFacing facing, bool turret, bool right, float u)
        {
            float r = u * 100f;
            var track = right ? VehicleModule.TrackRight : VehicleModule.TrackLeft;
            // a sponson tank's side guns are on the struck side; a turret tank's one gun is in the turret
            var gun = spec.GunCount > 1 ? (right ? VehicleModule.GunB : VehicleModule.GunA) : VehicleModule.GunA;
            if (turret)
                return r < 40f ? VehicleModule.Crew : r < 70f ? VehicleModule.GunA : r < 78f ? VehicleModule.Ammo : VehicleModule.None;
            switch (facing)
            {
                case ArmourFacing.Front:
                    return r < 34f ? VehicleModule.Crew : r < 50f ? track : r < 58f ? VehicleModule.Ammo : r < 62f ? VehicleModule.Fuel
                         : r < 66f ? VehicleModule.Engine : r < 74f && spec.GunCount > 1 ? gun : VehicleModule.None;
                case ArmourFacing.Side:
                    return r < 30f ? track : r < 48f ? VehicleModule.Crew : r < 62f ? VehicleModule.Engine : r < 72f ? VehicleModule.Ammo
                         : r < 82f ? VehicleModule.Fuel : r < 92f && spec.GunCount > 1 ? gun : VehicleModule.None;
                case ArmourFacing.Rear:
                    return r < 46f ? VehicleModule.Engine : r < 70f ? VehicleModule.Fuel : r < 80f ? track : r < 90f ? VehicleModule.Crew
                         : r < 95f ? VehicleModule.Ammo : VehicleModule.None;
                default:   // top: shells and charges on the deck
                    return r < 26f ? VehicleModule.Engine : r < 44f ? VehicleModule.Crew : r < 58f ? VehicleModule.Fuel : r < 68f ? VehicleModule.Ammo
                         : r < 80f ? track : r < 90f ? gun : VehicleModule.None;
            }
        }

        void StrikeModule(SimWorld w, int t, in TankSpec spec, VehicleModule m, ref Random rng)
        {
            if (m == VehicleModule.None) return;
            if (m == VehicleModule.Crew) { LoseCrew(w, t, 1); return; }
            DamageModule(w, t, spec, m, rng.NextFloat(0.55f, 1.1f), ref rng);
        }

        void DamageModule(SimWorld w, int t, in TankSpec spec, VehicleModule m, float amount, ref Random rng)
        {
            int idx = t * M + (int)m;
            float before = Module[idx], after = math.max(0f, before - amount);
            Module[idx] = after;
            w.Events.Add(w.Tick, SimEventType.VehicleModuleHit, t, (int)m, w.Position[t], default, after);
            bool broke = after <= 0f && before > 0f;
            // a track is "thrown" and an engine "stalled" at their thresholds now, not at zero, so the flag and the
            // picture agree: TankRenderer sheds the track off the hull exactly when the sim stops calling it a track
            bool thrownNow = before >= TrackThrownBelow && after < TrackThrownBelow;
            bool stalledNow = before >= StalledBelow && after < StalledBelow;
            switch (m)
            {
                case VehicleModule.TrackLeft:
                case VehicleModule.TrackRight:
                {
                    // A walker has legs on that side, not a track. The module is the share of them still under it, so
                    // a hit takes whole legs off one at a time and the side only fails when the last one has gone.
                    var prof = TW.Sim.Nav.VehicleProfile.ForArchetype(w.Archetype[t]);
                    if (prof.Walker && prof.Legs > 0)
                    {
                        bool onRight = m == VehicleModule.TrackRight;
                        int perSide = math.max(1, prof.Legs / 2);
                        int want = perSide - (int)math.ceil(after * perSide - 1e-4f);
                        for (int k = 0; k < perSide && LegsGone(t, onRight, perSide) < want; k++)
                        {
                            int leg = onRight ? perSide + k : k;
                            if ((LegsLost[t] & (1 << leg)) != 0) continue;
                            LegsLost[t] = (byte)(LegsLost[t] | (1 << leg));
                            w.Events.Add(w.Tick, SimEventType.VehicleLegLost, t, leg, w.Position[t]);
                        }
                    }
                    if (thrownNow) w.Events.Add(w.Tick, SimEventType.VehicleTrackHit, t, m == VehicleModule.TrackRight ? 1 : 0, w.Position[t]);
                    break;
                }
                case VehicleModule.Engine:
                    if (stalledNow) w.Events.Add(w.Tick, SimEventType.VehicleStalled, t, 1, w.Position[t]);
                    if (rng.NextFloat() < 0.3f) StartFire(w, t, 0.25f);
                    break;
                case VehicleModule.GunA:
                case VehicleModule.GunB:
                    if (gunnery != null) gunnery.GunHealth[t * Guns + (m == VehicleModule.GunB ? 1 : 0)] = after;
                    break;
                case VehicleModule.Fuel:
                    if (rng.NextFloat() < 0.5f + spec.FuelRisk) StartFire(w, t, rng.NextFloat(0.3f, 0.55f));
                    break;
                case VehicleModule.Ammo:
                    if (rng.NextFloat() < spec.AmmoRisk) CookOff(w, t, rng.NextInt(CookOffMin, CookOffMax + 1), ref rng);
                    else StartFire(w, t, 0.45f);
                    break;
            }
        }

        void LoseCrew(SimWorld w, int t, int n)
        {
            if (Crew[t] == 0) return;
            Crew[t] = (byte)math.max(0, Crew[t] - n);
            w.Events.Add(w.Tick, SimEventType.VehicleCrewLost, t, Crew[t], w.Position[t]);
            if (Crew[t] == 0) { var rng = Dice(w, 0x100000u + (uint)t); KnockOut(w, t, VehicleKillCause.CrewLost, ref rng); }
        }

        void StartFire(SimWorld w, int t, float intensity)
        {
            if (Fire[t] <= 0f) w.Events.Add(w.Tick, SimEventType.VehicleOnFire, t, 1, w.Position[t], default, intensity);
            Fire[t] = math.max(Fire[t], intensity);
        }

        // ------------------------------------------------------------------ bursts
        void Burst(SimWorld w, Impact im, int k)
        {
            for (int i = 0; i < w.HighWater; i++)
            {
                if (!IsTank(w, i)) continue;
                var prof = VehicleProfile.ForArchetype(w.Archetype[i]);
                float3 d = w.Position[i] - im.Pos; d.y = 0f;
                float dist = SimMath.Length(d), reach = im.Radius + prof.HalfWidth;
                if (dist >= reach) continue;
                var spec = TankSpec.For(w.Archetype[i]);
                var rng = Dice(w, 0x1000000u + (uint)(k & 0xFFF) * 4096u + (uint)i);   // above every other stream here (slots < 4096)
                LastHitTick[i] = w.Tick; Repair[i] = 0;
                bool direct = dist < prof.HalfWidth + 0.6f;
                float3 down = new float3(0f, -1f, 0f);
                if (State[i] != (byte)VehicleState.Active)
                {
                    if (direct && Fire[i] > 0f) Fire[i] = math.min(1f, Fire[i] + 0.25f);
                    continue;
                }
                checksum = SimHash.Value(new int3(i, k, direct ? 1 : 0), checksum);
                if (direct)
                {
                    bool holed = Armor.PenetratesTop(spec.Hull, im.Damage * 0.1f, out float plate);
                    float dmg = holed ? im.Damage * 1.2f : im.Damage * 0.15f;
                    w.Hp[i] = w.Hp[i] - dmg;
                    w.Events.Add(w.Tick, SimEventType.Hit, -1, i, w.Position[i], down, holed ? dmg : -plate);
                    w.Events.Add(w.Tick, SimEventType.VehicleArmourHit, i, -1, w.Position[i], down, holed ? plate : -plate);
                    if (holed) { Penetrations++; StrikeModule(w, i, spec, ModuleRoll(spec, ArmourFacing.Top, false, rng.NextFloat() < 0.5f, rng.NextFloat()), ref rng); }
                    Shaken[i] = math.max(Shaken[i], 50);
                }
                else
                {
                    // outside the plate: the tracks, the vision slits, the men's ears
                    float near = 1f - dist / reach;
                    w.Hp[i] = w.Hp[i] - im.Damage * 0.05f * near;
                    bool right = SimMath.Sin(w.Yaw[i]) * -d.z + SimMath.Cos(w.Yaw[i]) * d.x < 0f;   // the side facing the burst
                    if (dist < im.Radius * 0.6f && rng.NextFloat() < 0.35f * near)
                        DamageModule(w, i, spec, right ? VehicleModule.TrackRight : VehicleModule.TrackLeft, 0.5f + 0.5f * near, ref rng);
                    if (rng.NextFloat() < 0.5f * near) Shaken[i] = math.max(Shaken[i], 30);
                }
                if (w.Hp[i] <= -ObliterateShare * w.MaxHp[i]) Obliterate(w, i, ref rng);
                else if (w.Hp[i] <= 0f) KnockOut(w, i, VehicleKillCause.Structure, ref rng);
            }
        }

        // ------------------------------------------------------------------ per tick
        void Tick(SimWorld w, int i)
        {
            var rng = Dice(w, 0x300000u + (uint)i);
            var spec = TankSpec.For(w.Archetype[i]);
            if (Shaken[i] > 0) Shaken[i]--;
            if (Fire[i] > 0f)
            {
                bool fighting = State[i] == (byte)VehicleState.Active && Crew[i] > 0 && Fire[i] < 0.5f;
                if (fighting && (w.Tick + (uint)i) % 20u == 0u && rng.NextFloat() < ExtinguishChance * Crew[i] / math.max(1f, CrewMax[i]))
                {
                    Fire[i] = 0f;
                    w.Events.Add(w.Tick, SimEventType.VehicleOnFire, i, 0, w.Position[i]);
                }
                else
                {
                    float growth = FireGrowth * w.Config.TickSeconds * (Module[i * M + (int)VehicleModule.Fuel] < 0.5f ? 1.8f : 1f);
                    Fire[i] = math.min(1f, Fire[i] + growth);
                    w.Hp[i] = w.Hp[i] - FireBurn * Fire[i] * w.Config.TickSeconds;
                    if (State[i] == (byte)VehicleState.Active && Fire[i] >= BailFire) KnockOut(w, i, VehicleKillCause.Fire, ref rng);
                    else if (Fire[i] >= BailFire && Fate[i] == Alive) Fate[i] = (byte)VehicleKillCause.Fire;   // a hulk burning through: half a hull
                    if (Fire[i] >= 1f && State[i] != (byte)VehicleState.CookingOff) CookOff(w, i, rng.NextInt(CookOffMin, CookOffMax + 1), ref rng);
                }
            }
            if (State[i] == (byte)VehicleState.Active && w.Hp[i] <= 0f) KnockOut(w, i, VehicleKillCause.Structure, ref rng);

            if (State[i] == (byte)VehicleState.CookingOff && StateTicks[i] <= 0) { Blow(w, i); return; }
            if (State[i] == (byte)VehicleState.KnockedOut)
            {
                w.Hp[i] = math.max(1f, w.Hp[i]);    // a hulk is finished by its own fire, not by more damage
                if (Fire[i] <= 0f && --StateTicks[i] <= 0) { Wreck(w, i); return; }
            }
            else if (State[i] == (byte)VehicleState.CookingOff)
            {
                w.Hp[i] = math.max(1f, w.Hp[i]);
                if (--StateTicks[i] <= 0) { Blow(w, i); return; }
            }
            else if (Crew[i] > 0 && w.Tick - LastHitTick[i] > RepairQuietTicks && ++Repair[i] >= RepairTicks)
            {
                Repair[i] = 0;
                MendWorst(w, i);
            }

            // what the damage means for the rest of the sim
            float trackL = Module[i * M + (int)VehicleModule.TrackLeft], trackR = Module[i * M + (int)VehicleModule.TrackRight];
            float engine = Module[i * M + (int)VehicleModule.Engine];
            var profile = TW.Sim.Nav.VehicleProfile.ForArchetype(w.Archetype[i]);
            uint f = w.Flags[i] & ~((uint)UnitFlags.Immobilised | (uint)UnitFlags.Stalled | (uint)UnitFlags.Burning);
            // a walker's "track" is the share of its legs still under it, and a side fails at its last one; a tank's
            // track is only OFF once it is torn past TrackThrownBelow, and it drags all the way down to there
            bool stuck = profile.Walker
                ? trackL <= 0f || trackR <= 0f
                : trackL < TrackThrownBelow || trackR < TrackThrownBelow;
            if (stuck || State[i] != (byte)VehicleState.Active) f |= (uint)UnitFlags.Immobilised;
            if (engine < StalledBelow) f |= (uint)UnitFlags.Stalled;
            if (Fire[i] > 0f) f |= (uint)UnitFlags.Burning;
            if (State[i] != (byte)VehicleState.Active) f |= (uint)UnitFlags.KnockedOut;
            w.Flags[i] = f;
            float mobility = profile.Walker
                ? (LegsLost[i] != 0 ? math.max(LegFloor, 1f - LegLoss * math.countbits((uint)LegsLost[i])) : 1f)
                : TrackFactor(trackL, trackR);
            kinematics.SpeedFactor[i] = EngineFactor(engine) * (Crew[i] >= 2 ? 1f : 0.6f) * mobility;
            // the standard over a Banner steadies its own side: men fighting near it come out of suppression faster.
            // Checked every StandardEvery ticks over the live slots, which is a few thousand compares for the one or
            // two of these either side can afford.
            if (spec.StandardRadius > 0f && State[i] == (byte)VehicleState.Active && w.Tick % StandardEvery == (uint)i % StandardEvery)
            {
                float r2 = spec.StandardRadius * spec.StandardRadius, steady = spec.StandardSteady * StandardEvery * w.Config.TickSeconds;
                byte team = w.Team[i]; float3 at = w.Position[i];
                for (int j = 0; j < w.HighWater; j++)
                {
                    if (!w.IsAlive(j) || w.Team[j] != team || (w.Flags[j] & (uint)UnitFlags.Vehicle) != 0) continue;
                    if (w.Suppression[j] <= 0f) continue;
                    float3 d = w.Position[j] - at; d.y = 0f;
                    if (math.lengthsq(d) > r2) continue;
                    w.Suppression[j] = math.max(0f, w.Suppression[j] - steady);
                }
            }
            // the Censer lays its chlorine as it walks, out of the drum on its back: hole the drum and it stops (and
            // the drum is its ammunition, so holing it usually ends the machine instead)
            if (spec.GasEverySeconds > 0f && gas != null && State[i] == (byte)VehicleState.Active
                && Module[i * M + (int)VehicleModule.Ammo] > 0f)
            {
                int every = math.max(1, (int)math.round(spec.GasEverySeconds / w.Config.TickSeconds));
                if (w.Tick % (uint)every == (uint)i % (uint)every)
                    gas.AddSource(w.Position[i], spec.GasStrength * Module[i * M + (int)VehicleModule.Ammo], every + 10, w.Team[i]);
            }
            if (gunnery != null)
                gunnery.CrewFactor[i] = State[i] != (byte)VehicleState.Active ? 0f : Shaken[i] > 0 ? 0.2f : Crew[i] / math.max(1f, CrewMax[i]);
        }

        /// <summary>What one track's health leaves of a tank's speed: whole above TrackFullAbove, dragging down to
        /// TrackWorstFactor, and nothing once it is thrown.</summary>
        public static float TrackHealthFactor(float m)
            => m >= TrackFullAbove ? 1f
             : m < TrackThrownBelow ? 0f
             : TrackWorstFactor + (1f - TrackWorstFactor) * (m - TrackThrownBelow) / (TrackFullAbove - TrackThrownBelow);

        /// <summary>Both tracks together: the worse one sets the pace, and a mismatch costs a little more on top.</summary>
        public static float TrackFactor(float left, float right)
        {
            float a = TrackHealthFactor(left), b = TrackHealthFactor(right);
            float worse = math.min(a, b), better = math.max(a, b);
            if (worse <= 0f) return 0f;
            return worse * (1f - TrackMismatch + TrackMismatch * worse / better);
        }

        /// <summary>What the engine leaves of it: whole above EngineFullAbove, down to EngineWorstFactor, then stalled.</summary>
        public static float EngineFactor(float e)
            => e >= EngineFullAbove ? 1f
             : e < StalledBelow ? 0f
             : EngineWorstFactor + (1f - EngineWorstFactor) * (e - StalledBelow) / (EngineFullAbove - StalledBelow);

        /// <summary>The crew's repair, also done by a repair engineer standing by (SupportSystem).</summary>
        internal void MendWorst(SimWorld w, int i)
        {
            var worst = VehicleModule.None; float lowest = RepairTo;
            for (int k = 0; k < 5; k++)
            {
                var m = RepairOrder(k);
                float v = Module[i * M + (int)m];
                if (v < lowest) { lowest = v; worst = m; }
                if (v < TrackThrownBelow) break;   // a failed one wins over a merely hurt one later in the list
            }
            if (worst == VehicleModule.None) return;
            bool wasStalled = worst == VehicleModule.Engine && Module[i * M + (int)worst] < StalledBelow;
            Module[i * M + (int)worst] = RepairTo;
            if (gunnery != null && (worst == VehicleModule.GunA || worst == VehicleModule.GunB)) gunnery.GunHealth[i * Guns + (worst == VehicleModule.GunB ? 1 : 0)] = RepairTo;
            w.Events.Add(w.Tick, SimEventType.VehicleRepaired, i, (int)worst, w.Position[i]);
            if (wasStalled) w.Events.Add(w.Tick, SimEventType.VehicleStalled, i, 0, w.Position[i]);
            checksum = SimHash.Value(new int2(i, (int)worst), checksum);
        }

        /// <summary>Tracks first (a tank that cannot move is only a gun emplacement), then the engine, then the guns.</summary>
        static VehicleModule RepairOrder(int k)
            => k == 0 ? VehicleModule.TrackLeft : k == 1 ? VehicleModule.TrackRight : k == 2 ? VehicleModule.Engine : k == 3 ? VehicleModule.GunA : VehicleModule.GunB;

        // ------------------------------------------------------------------ death
        void KnockOut(SimWorld w, int t, VehicleKillCause cause, ref Random rng)
        {
            if (State[t] != (byte)VehicleState.Active) return;
            State[t] = (byte)VehicleState.KnockedOut;
            KillCause[t] = (byte)cause;
            KnockOuts++;
            w.Flags[t] = w.Flags[t] | (uint)UnitFlags.KnockedOut | (uint)UnitFlags.Immobilised;
            w.TargetSlot[t] = -1;
            w.Events.Add(w.Tick, SimEventType.VehicleKnockedOut, t, (int)cause, w.Position[t], new float3(0f, w.Yaw[t], 0f));
            if (cause == VehicleKillCause.Structure && rng.NextFloat() < 0.5f) StartFire(w, t, 0.4f);
            StateTicks[t] = rng.NextInt(BurnOutMin, BurnOutMax + 1);
            if (!TankSpec.For(w.Archetype[t]).Unmanned) BailOut(w, t);   // nobody gets out of a walker: there is nobody in it
            if (gunnery != null) gunnery.CrewFactor[t] = 0f;
            checksum = SimHash.Value(new int2(t, (int)cause), checksum);
        }

        /// <summary>The rounds go up: an explosion for whoever stands close (it bursts next tick), and the wreck.</summary>
        void Blow(SimWorld w, int i)
        {
            if (blast != null)
                blast.Queue(new Impact
                {
                    Pos = w.Position[i], Damage = CookOffDamage, Radius = CookOffRadius, Suppression = CookOffSuppression,
                    CraterRadius = CookOffCrater, CraterDepth = 0.35f, Source = CookOffSource, Player = -1,
                    Shape = (int)TW.Sim.Combat.BlastShape.CookOff,   // it goes off where it stands: no flight, no lean
                });
            w.Events.Add(w.Tick, SimEventType.VehicleCookOff, i, 0, w.Position[i], new float3(0f, w.Yaw[i], 0f), CookOffRadius);
            CookOffs++;
            checksum = SimHash.Value(new int2(i, (int)w.Tick), checksum);
            Wreck(w, i);
        }

        /// <summary>Blown apart by one hit: nobody gets out, it goes up now.</summary>
        void Obliterate(SimWorld w, int t, ref Random rng)
        {
            Crew[t] = 0;
            KnockOut(w, t, VehicleKillCause.Ammunition, ref rng);
            Fate[t] = (byte)VehicleKillCause.Ammunition;
            State[t] = (byte)VehicleState.CookingOff;
            StateTicks[t] = 0;
            Blow(w, t);
        }

        void CookOff(SimWorld w, int t, int fuse, ref Random rng)
        {
            if (State[t] == (byte)VehicleState.CookingOff) return;
            KnockOut(w, t, VehicleKillCause.Ammunition, ref rng);
            Fate[t] = (byte)VehicleKillCause.Ammunition;   // the rounds went up: scrap, whatever blow had stopped it first
            State[t] = (byte)VehicleState.CookingOff;
            StateTicks[t] = fuse;
            Fire[t] = math.max(Fire[t], 0.8f);
        }

        /// <summary>The men still alive climb out and fall back as riflemen (without the silver a deploy costs).</summary>
        void BailOut(SimWorld w, int t)
        {
            int n = Crew[t];
            Crew[t] = 0;
            if (n == 0) return;
            var prof = VehicleProfile.ForArchetype(w.Archetype[t]);
            float yaw = w.Yaw[t];
            float3 back = -SimMath.DirFromYaw(yaw), side = new float3(-back.z, 0f, back.x);
            int got = 0;
            for (int c = 0; c < n; c++)
            {
                float3 at = w.Position[t] + back * (prof.HalfLength + 1.2f) + side * ((c - (n - 1) * 0.5f) * 1.1f);
                int s = w.Spawn(w.Team[t], 0, OpenGround(w.ClampToMap(at)), BailedHp, BailedSpeed, false);
                if (s >= 0) got++;
            }
            BailedOut += got;
            w.Events.Add(w.Tick, SimEventType.VehicleBailedOut, t, got, w.Position[t]);
        }

        /// <summary>Somewhere a man can stand and walk off from: pos when its nav cell is open surface, else the nearest
        /// point of the nearest open surface cell within three (not a trench, link, bunker or blocked cell: a man there
        /// would have no flow-field direction, or be in a trench without garrisoning it). Ties go to the lower cell.</summary>
        float3 OpenGround(float3 pos) => OpenGround(map, pos);

        /// <summary>The same for anyone putting a man on the ground (a paratrooper, a sea landing): static, so the
        /// off-map abilities can use it without a modules system.</summary>
        public static float3 OpenGround(Terrain.MapData map, float3 pos)
        {
            if (map == null) return pos;
            const Terrain.NavLayer closed = Terrain.NavLayer.Trench | Terrain.NavLayer.Link | Terrain.NavLayer.Bunker | Terrain.NavLayer.Blocked;
            var c = map.NavCellOf(pos);
            var here = (Terrain.NavLayer)map.NavLayers[map.NavIndex(c.x, c.y)];
            if ((here & closed) == 0 && (here & Terrain.NavLayer.Surface) != 0) return pos;
            float best = float.MaxValue; float3 at = pos;
            float inset = Terrain.MapData.NavCellSize * 0.5f - 0.2f;
            for (int dz = -3; dz <= 3; dz++)
                for (int dx = -3; dx <= 3; dx++)
                {
                    int x = c.x + dx, z = c.y + dz;
                    if (x < 0 || z < 0 || x >= map.NavWidth || z >= map.NavLength) continue;
                    int cell = map.NavIndex(x, z);
                    var layer = (Terrain.NavLayer)map.NavLayers[cell];
                    if ((layer & closed) != 0 || (layer & Terrain.NavLayer.Surface) == 0) continue;
                    float3 centre = map.NavCellCenter(cell);
                    float3 q = new float3(math.clamp(pos.x, centre.x - inset, centre.x + inset), pos.y, math.clamp(pos.z, centre.z - inset, centre.z + inset));
                    float d = math.distancesq(q.xz, pos.xz);
                    if (d < best) { best = d; at = q; }
                }
            return at;
        }

        void Wreck(SimWorld w, int t)
        {
            State[t] = (byte)VehicleState.Active;   // the slot's next tenant starts clean (Init runs on the new Generation)
            Fire[t] = 0f;
            w.Despawn(t, LastShooter[t], new float3(0f, 1f, 0f));   // VehicleDestroyed: DeformationSystem leaves the wreck
        }

        public ulong Hash(ulong h)
        {
            int n = world.HighWater;
            h = SimHash.Array(Gen, n, h);
            h = SimHash.Array(Module, n * M, h);
            h = SimHash.Array(Crew, n, h);
            h = SimHash.Array(CrewMax, n, h);
            h = SimHash.Array(Fire, n, h);
            h = SimHash.Array(State, n, h);
            h = SimHash.Array(StateTicks, n, h);
            h = SimHash.Array(LastHitTick, n, h);
            h = SimHash.Array(Shaken, n, h);
            h = SimHash.Array(Repair, n, h);
            h = SimHash.Array(LastShooter, n, h);
            h = SimHash.Array(LegsLost, n, h);
            h = SimHash.Array(KillCause, n, h);
            h = SimHash.Array(Fate, n, h);
            h = SimHash.Value(new int4(Penetrations, Ricochets, KnockOuts, CookOffs), h);
            h = SimHash.Value(BailedOut, h);
            return SimHash.Combine(h, checksum);
        }

        public void Dispose()
        {
            if (Gen.IsCreated) Gen.Dispose();
            if (Module.IsCreated) Module.Dispose();
            if (Crew.IsCreated) Crew.Dispose();
            if (CrewMax.IsCreated) CrewMax.Dispose();
            if (Fire.IsCreated) Fire.Dispose();
            if (State.IsCreated) State.Dispose();
            if (StateTicks.IsCreated) StateTicks.Dispose();
            if (LastHitTick.IsCreated) LastHitTick.Dispose();
            if (Shaken.IsCreated) Shaken.Dispose();
            if (Repair.IsCreated) Repair.Dispose();
            if (LastShooter.IsCreated) LastShooter.Dispose();
            if (LegsLost.IsCreated) LegsLost.Dispose();
            if (KillCause.IsCreated) KillCause.Dispose();
            if (Fate.IsCreated) Fate.Dispose();
        }
    }
}
