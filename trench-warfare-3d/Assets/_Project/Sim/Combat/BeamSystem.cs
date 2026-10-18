// Phase: A5 (implemented; docs/21 phase 5, SIM-C) — the sweeping beam: a line of fire from above that walks up its
// corridor. OffMapAbilitySystem starts one (Beam = 11) after the warm-up: the head moves from Start along Dir over
// Length in SweepTicks, and the TrailMetres behind it are burning. Each tick, in slot order (BeamJob): a man within
// HalfWidth of the trail takes BeamDps a second (TrenchBayFactor of it in a trench: a trench always gives limited
// protection), is suppressed, catches fire (BurningSystem.Ignite, so he runs alight if he lives) and dies of
// DeathCause.Beam when it is too much; a vehicle within HalfWidth of its hull loses BeamVehicleDps a second straight
// off the structure (VehicleModulesSystem knocks it out and emits the wreck the same tick) and a VehicleArmourHit
// (b = -1, scalar > 0) each tick for the sparks. The corridor's half width is the MEN's rule only: a hull burns when
// the trail's segment runs under its footprint (VehicleProfile.Crosses, the match's profile table, grown by
// HullMargin), not when its centre is within HalfWidth of the line - a hull must actually be over the burning line,
// whatever its size (before 2026-10-18 a 3.5 m disc round the centre spared a Maw 4 m off and burned a Mercy 3 m off). Every ScorchEvery ticks the head queues a Beam-shaped Impact that
// BlastSystem resolves without touching men (they are this system's), so Deformation wears the trees and the wire
// under it and the Explosion event (dir.y = BlastShape.Beam) is the picture's scorch cue. MaxPerPlayer is a safety
// cap on the sweeps in flight per player: the cooldown (3600) outlasts a sweep (200), so a second command meets the
// cooldown first and no player reaches the cap. Beams is hashed; the lists the job fills are cleared every tick.
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Sim.Combat
{
    public struct ActiveBeam
    {
        public float3 Start, Dir;         // Dir: unit XZ
        public float Length, HalfWidth;
        public uint StartTick, EndTick;
        public int Player, Source;        // Source: the ability id, for the Explosion events
    }

    public sealed class BeamSystem : ISimSystem
    {
        public const float BeamDps = 300f, BeamVehicleDps = 1200f, BeamSuppressionPerSecond = 120f;
        public const float AlightSeconds = 7f, TrailMetres = 4f;
        /// <summary>How far past the plate the burn still counts, the mine burst's own margin:
        /// VehicleModulesSystem.MineHullMargin.</summary>
        public const float HullMargin = 0.6f;
        public const float ScorchDamage = 60f, ScorchSuppression = 15f, ScorchRadiusFactor = 1.5f;
        public const int ScorchEvery = 4, MaxPerPlayer = 2;
        public int Order => SimSystemOrder.Beam;

        public NativeList<ActiveBeam> Beams;
        /// <summary>Sweeps started since the match began (a stat, not hashed).</summary>
        public int Sweeps { get; private set; }

        readonly MapData map;
        BlastSystem blast;
        BurningSystem burning;
        VehicleKinematicsSystem kinematics;        // registered after this system: resolved on the first step
        NativeArray<VehicleProfile> compiled;      // the compiled defaults, for a world with no kinematics system
        NativeList<int> killed, lit, hulls;
        NativeList<float3> killedDir;

        public BeamSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            blast = world.GetSystem<BlastSystem>() ?? throw new System.InvalidOperationException("BeamSystem needs BlastSystem registered before it");
            burning = world.GetSystem<BurningSystem>();   // without it the beam still kills, nobody runs alight
            Beams = new NativeList<ActiveBeam>(4, Allocator.Persistent);
            killed = new NativeList<int>(64, Allocator.Persistent);
            killedDir = new NativeList<float3>(64, Allocator.Persistent);
            lit = new NativeList<int>(64, Allocator.Persistent);
            hulls = new NativeList<int>(16, Allocator.Persistent);
            compiled = new NativeArray<VehicleProfile>(Archetypes.Count, Allocator.Persistent);
            for (int a = 0; a < Archetypes.Count; a++) compiled[a] = VehicleProfile.ForArchetype((byte)a);
        }

        public int ActiveFor(int player)
        {
            int n = 0;
            for (int i = 0; i < Beams.Length; i++) if (Beams[i].Player == player) n++;
            return n;
        }

        /// <summary>Start a sweep now. False (and nothing started) when the player already runs MaxPerPlayer.</summary>
        public bool Start(SimWorld w, float3 start, float3 dir, float length, float halfWidth, int sweepTicks, int player, int source)
        {
            if (ActiveFor(player) >= MaxPerPlayer) return false;
            float3 d = new float3(dir.x, 0f, dir.z);
            float dl = SimMath.Length(d);
            d = dl > 1e-4f ? d / dl : new float3(0f, 0f, 1f);
            Beams.Add(new ActiveBeam
            {
                Start = new float3(start.x, 0f, start.z), Dir = d, Length = math.max(1f, length), HalfWidth = math.max(0.5f, halfWidth),
                StartTick = w.Tick, EndTick = w.Tick + (uint)math.max(1, sweepTicks), Player = player, Source = source,
            });
            Sweeps++;
            return true;
        }

        /// <summary>Where the head is on a tick: from Start, Length along Dir over the sweep.</summary>
        public static float3 HeadOf(in ActiveBeam b, uint tick)
        {
            float t = b.EndTick > b.StartTick ? math.saturate((float)(tick - b.StartTick) / (b.EndTick - b.StartTick)) : 1f;
            return b.Start + b.Dir * (b.Length * t);
        }

        public void Step(SimWorld w)
        {
            if (Beams.Length == 0) return;
            int keep = 0;
            for (int i = 0; i < Beams.Length; i++)
            {
                var b = Beams[i];
                if (w.Tick > b.EndTick) continue;   // the sweep is over: the last tick still burns
                Beams[keep++] = b;
            }
            Beams.Length = keep;
            if (keep == 0) return;

            killed.Clear(); killedDir.Clear(); lit.Clear(); hulls.Clear();
            if (kinematics == null) kinematics = w.GetSystem<VehicleKinematicsSystem>();
            new BeamJob
            {
                Count = w.HighWater, Beams = Beams.AsArray(), Tick = w.Tick, Dt = w.Config.TickSeconds,
                Position = w.Position, Flags = w.Flags, Hp = w.Hp, Suppression = w.Suppression,
                Archetype = w.Archetype, Yaw = w.Yaw,
                // the match's table, as the mine trigger reads it: the compiled switch knows no unit that is only a
                // definition, and would give every such machine the Maw's footprint
                Profiles = kinematics != null && kinematics.Profiles.IsCreated ? kinematics.Profiles : compiled,
                Killed = killed, KilledDir = killedDir, Lit = lit, Hulls = hulls,
            }.Run();
            for (int k = 0; k < hulls.Length; k += 2)
            {
                int i = hulls[k];
                var b = Beams[hulls[k + 1]];
                w.Events.Add(w.Tick, SimEventType.VehicleArmourHit, i, -1, w.Position[i], b.Dir, 1f);
            }
            if (burning != null)
                for (int k = 0; k < lit.Length; k++) burning.Ignite(w, lit[k], AlightSeconds);
            for (int k = 0; k < killed.Length; k++) w.Despawn(killed[k], (int)DeathCause.Beam, killedDir[k], 4f);
            // the scorch under the head: the trees, the wire and the picture, not the men
            for (int i = 0; i < Beams.Length; i++)
            {
                var b = Beams[i];
                if ((w.Tick - b.StartTick) % (uint)ScorchEvery != 0u) continue;
                blast.Queue(new Impact
                {
                    Pos = w.ClampToMap(HeadOf(b, w.Tick)), Dir = b.Dir, Damage = ScorchDamage, Radius = b.HalfWidth * ScorchRadiusFactor, Suppression = ScorchSuppression,
                    CraterRadius = 0f, Source = b.Source, Player = b.Player, Shape = (int)BlastShape.Beam,
                });
            }
        }

        [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct BeamJob : IJob
        {
            public int Count;
            public uint Tick;
            public float Dt;
            [ReadOnly] public NativeArray<ActiveBeam> Beams;
            [ReadOnly] public NativeArray<float3> Position;
            [ReadOnly] public NativeArray<uint> Flags;
            [ReadOnly] public NativeArray<byte> Archetype;
            [ReadOnly] public NativeArray<float> Yaw;
            [ReadOnly] public NativeArray<VehicleProfile> Profiles;
            public NativeArray<float> Hp, Suppression;
            public NativeList<int> Killed, Lit, Hulls;
            public NativeList<float3> KilledDir;

            static float ToSegment(float3 p, float3 a, float3 b)
            {
                float3 ab = b - a, ap = p - a; ab.y = 0f; ap.y = 0f;
                float l2 = math.dot(ab, ab);
                float t = l2 > 1e-6f ? math.saturate(math.dot(ap, ab) / l2) : 0f;
                float3 q = a + ab * t - p; q.y = 0f;
                return SimMath.Length(q);
            }

            public void Execute()
            {
                for (int k = 0; k < Beams.Length; k++)
                {
                    var b = Beams[k];
                    float3 head = HeadOf(b, Tick);
                    float along = SimMath.Length(head - b.Start);
                    float3 tail = head - b.Dir * math.min(TrailMetres, along);
                    for (int i = 0; i < Count; i++)
                    {
                        uint f = Flags[i];
                        if ((f & (uint)UnitFlags.Alive) == 0 || Hp[i] <= 0f) continue;
                        if ((f & (uint)UnitFlags.Vehicle) != 0)
                        {
                            if ((f & (uint)UnitFlags.KnockedOut) != 0) continue;
                            // the trail under the hull's footprint in its own yaw, not a disc round its centre
                            if (!Profiles[Archetype[i]].Crosses(Yaw[i], Position[i], tail, head, HullMargin)) continue;
                            Hp[i] = Hp[i] - BeamVehicleDps * Dt;   // the structure: VehicleModulesSystem knocks it out this tick
                            Hulls.Add(i); Hulls.Add(k);              // the slot and the beam, in pairs
                            continue;
                        }
                        if (ToSegment(Position[i], tail, head) > b.HalfWidth) continue;   // the men: the corridor's half width
                        float dmg = BeamDps * Dt;
                        if ((f & (uint)UnitFlags.InTrench) != 0) dmg *= BlastRules.TrenchBayFactor;
                        Hp[i] = Hp[i] - dmg;
                        Suppression[i] = math.min(100f, Suppression[i] + BeamSuppressionPerSecond * Dt);
                        if (Hp[i] <= 0f) { Killed.Add(i); KilledDir.Add(b.Dir); }
                        else Lit.Add(i);
                    }
                }
            }
        }

        public ulong Hash(ulong h)
        {
            for (int i = 0; i < Beams.Length; i++) h = SimHash.Value(Beams[i], h);
            return h;
        }

        public void Dispose()
        {
            if (Beams.IsCreated) Beams.Dispose();
            if (killed.IsCreated) killed.Dispose();
            if (killedDir.IsCreated) killedDir.Dispose();
            if (lit.IsCreated) lit.Dispose();
            if (hulls.IsCreated) hulls.Dispose();
            if (compiled.IsCreated) compiled.Dispose();
        }
    }
}
