// Phase: A5 (implemented core: the gas field; smoke shares the grid code and lands with the smoke abilities)
// — depends on: MapData (4 m field grid, wind, Trench / Crater sinks), FlowFieldManager (where a gassed garrison runs to).
// One float concentration grid at 4 m. Per tick: sources hold their cells at strength, the field is advected
// by the map wind (semi-Lagrangian), diffused (5-point) and decayed. Gas is heavier than air: a man standing in a
// Trench or Crater cell breathes twice the cell's concentration. Damage is 6 per second per 10 concentration and
// ignores cover and armour (vehicles are exempt until crews exist, masks take 60 % off); it also suppresses.
// A garrison breathing 8 or more abandons its trench and runs for its own HQ; "fall back" on that trench brings it
// back once the cloud has passed. The field is only simulated and hashed while something is in it.
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Sim.Combat
{
    public struct GasSource
    {
        public int Cell;
        public float Strength;
        public int TicksLeft;
        public int Player;
    }

    public sealed class GasSmokeSystem : ISimSystem
    {
        public const float DamagePerSecondPerTen = 6f;
        public const float FleeConcentration = 8f;
        public const float Diffusion = 0.04f;        // fraction exchanged with each neighbour per tick
        public const float DecayPerTick = 0.004f;    // ~9 s half-life once the source stops
        public const float SuppressionPerSecond = 20f;

        public int Order => SimSystemOrder.GasSmoke;
        public NativeArray<float> Gas;
        public int Width, Length;
        public NativeList<GasSource> Sources;
        /// <summary>True while any cell holds gas; presentation skips drawing and the sim skips the field otherwise.</summary>
        public bool Active { get; private set; }

        readonly MapData map;
        FlowFieldManager fields;
        NativeArray<float> scratch;
        NativeList<int> killed, fled;
        NativeArray<float> peak;   // single element: the job reports the highest concentration left

        public GasSmokeSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            fields = world.GetSystem<FlowFieldManager>() ?? throw new System.InvalidOperationException("GasSmokeSystem needs FlowFieldManager registered before it");
            Width = (int)math.ceil(map.SizeMeters.x / MapData.FieldCellSize);
            Length = (int)math.ceil(map.SizeMeters.y / MapData.FieldCellSize);
            Gas = new NativeArray<float>(Width * Length, Allocator.Persistent);
            scratch = new NativeArray<float>(Width * Length, Allocator.Persistent);
            Sources = new NativeList<GasSource>(8, Allocator.Persistent);
            killed = new NativeList<int>(64, Allocator.Persistent);
            fled = new NativeList<int>(64, Allocator.Persistent);
            peak = new NativeArray<float>(1, Allocator.Persistent);
        }

        public int CellOf(float3 p)
        {
            int cx = math.clamp((int)(p.x / MapData.FieldCellSize), 0, Width - 1);
            int cz = math.clamp((int)(p.z / MapData.FieldCellSize), 0, Length - 1);
            return cz * Width + cx;
        }

        /// <summary>Open a gas cylinder at a world position: the cell is held at <paramref name="strength"/> for the duration.</summary>
        public void AddSource(float3 pos, float strength, int ticks, int player)
        {
            Sources.Add(new GasSource { Cell = CellOf(pos), Strength = strength, TicksLeft = ticks, Player = player });
            Active = true;
        }

        public float ConcentrationAt(float3 p) => Gas.IsCreated ? Gas[CellOf(p)] : 0f;

        public void Step(SimWorld w)
        {
            if (!Active) return;
            new FieldJob
            {
                Gas = Gas, Scratch = scratch, Sources = Sources, Peak = peak, Width = Width, Length = Length,
                Shift = map.Wind * w.Config.TickSeconds / MapData.FieldCellSize,
            }.Run();
            for (int s = Sources.Length - 1; s >= 0; s--)
            {
                var src = Sources[s];
                if (--src.TicksLeft <= 0) Sources.RemoveAt(s); else Sources[s] = src;   // RemoveAt keeps the order
            }
            if (peak[0] < 0.05f && Sources.Length == 0)
            {
                for (int i = 0; i < Gas.Length; i++) Gas[i] = 0f;
                Active = false;
                return;
            }

            killed.Clear(); fled.Clear();
            new BreatheJob
            {
                Count = w.HighWater, Gas = Gas, Width = Width, Length = Length, Dt = w.Config.TickSeconds,
                Position = w.Position, Flags = w.Flags, TrenchId = w.TrenchId, Hp = w.Hp, Suppression = w.Suppression,
                Layers = map.NavLayers, NavWidth = map.NavWidth, NavLength = map.NavLength, Killed = killed, Fled = fled,
            }.Run();
            for (int k = 0; k < fled.Length; k++)
            {
                int i = fled[k];
                short trench = w.TrenchId[i];
                short hq = fields.OwnHq(w.Team[i]);
                w.SourceTrench[i] = trench;
                w.TrenchId[i] = -1;
                w.GoalId[i] = hq >= 0 ? fields.GetGoal(GoalKey.Objective(hq)) : -1;
                w.Flags[i] |= (uint)UnitFlags.Exposed;
                w.Events.Add(w.Tick, SimEventType.UnitLeftTrench, i, trench, w.Position[i]);
            }
            for (int k = 0; k < killed.Length; k++) w.Despawn(killed[k], (int)DeathCause.Gas);
        }

        [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct FieldJob : IJob
        {
            public NativeArray<float> Gas, Scratch, Peak;
            [ReadOnly] public NativeList<GasSource> Sources;
            public int Width, Length;
            public float2 Shift;   // wind per tick, in cells

            float At(int x, int z) => x < 0 || z < 0 || x >= Width || z >= Length ? 0f : Gas[z * Width + x];

            public void Execute()
            {
                for (int s = 0; s < Sources.Length; s++)
                {
                    var src = Sources[s];
                    Gas[src.Cell] = math.max(Gas[src.Cell], src.Strength);
                }
                // advect: sample upwind (bilinear), then diffuse and decay into Scratch
                for (int z = 0; z < Length; z++)
                for (int x = 0; x < Width; x++)
                {
                    float fx = x - Shift.x, fz = z - Shift.y;
                    int x0 = (int)math.floor(fx), z0 = (int)math.floor(fz);
                    float tx = fx - x0, tz = fz - z0;
                    float c = math.lerp(math.lerp(At(x0, z0), At(x0 + 1, z0), tx), math.lerp(At(x0, z0 + 1), At(x0 + 1, z0 + 1), tx), tz);
                    Scratch[z * Width + x] = c;
                }
                float peak = 0f;
                for (int z = 0; z < Length; z++)
                for (int x = 0; x < Width; x++)
                {
                    int i = z * Width + x;
                    float c = Scratch[i];
                    float l = x > 0 ? Scratch[i - 1] : c, r = x < Width - 1 ? Scratch[i + 1] : c;
                    float d = z > 0 ? Scratch[i - Width] : c, u = z < Length - 1 ? Scratch[i + Width] : c;
                    float v = (c + Diffusion * (l + r + d + u - 4f * c)) * (1f - DecayPerTick);
                    if (v < 0.01f) v = 0f;
                    Gas[i] = v;
                    peak = math.max(peak, v);
                }
                Peak[0] = peak;
            }
        }

        [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct BreatheJob : IJob
        {
            public int Count, Width, Length, NavWidth, NavLength;
            public float Dt;
            [ReadOnly] public NativeArray<float> Gas;
            [ReadOnly] public NativeArray<float3> Position;
            [ReadOnly] public NativeArray<uint> Flags;
            [ReadOnly] public NativeArray<short> TrenchId;
            [ReadOnly] public NativeArray<byte> Layers;
            public NativeArray<float> Hp, Suppression;
            public NativeList<int> Killed, Fled;

            public void Execute()
            {
                for (int i = 0; i < Count; i++)
                {
                    uint f = Flags[i];
                    if ((f & (uint)UnitFlags.Alive) == 0 || Hp[i] <= 0f || (f & (uint)UnitFlags.Vehicle) != 0) continue;
                    float3 p = Position[i];
                    int gx = math.clamp((int)(p.x / MapData.FieldCellSize), 0, Width - 1), gz = math.clamp((int)(p.z / MapData.FieldCellSize), 0, Length - 1);
                    float c = Gas[gz * Width + gx];
                    if (c < 0.5f) continue;
                    int nx = math.clamp((int)(p.x / MapData.NavCellSize), 0, NavWidth - 1), nz = math.clamp((int)(p.z / MapData.NavCellSize), 0, NavLength - 1);
                    if ((Layers[nz * NavWidth + nx] & (byte)(NavLayer.Trench | NavLayer.Crater)) != 0) c *= 2f;   // it pools in low ground
                    float dmg = c * 0.1f * DamagePerSecondPerTen * Dt;
                    if ((f & (uint)UnitFlags.Masked) != 0) dmg *= 0.4f;
                    Hp[i] = Hp[i] - dmg;
                    Suppression[i] = math.min(100f, Suppression[i] + SuppressionPerSecond * math.saturate(c * 0.1f) * Dt);
                    if (Hp[i] <= 0f) Killed.Add(i);
                    else if (TrenchId[i] >= 0 && c >= FleeConcentration && (f & (uint)UnitFlags.Masked) == 0) Fled.Add(i);
                }
            }
        }

        public float ConcentrationAlong(float3 a, float3 b, bool smoke) => 0f;   // smoke attenuation lands with the smoke abilities

        public ulong Hash(ulong h)
        {
            if (!Active) return h;
            h = SimHash.Array(Gas, h);
            for (int s = 0; s < Sources.Length; s++) h = SimHash.Value(Sources[s], h);
            return h;
        }

        public void Dispose()
        {
            if (Gas.IsCreated) Gas.Dispose();
            if (scratch.IsCreated) scratch.Dispose();
            if (Sources.IsCreated) Sources.Dispose();
            if (killed.IsCreated) killed.Dispose();
            if (fled.IsCreated) fled.Dispose();
            if (peak.IsCreated) peak.Dispose();
        }
    }
}
