// Phase: A1 (implemented; wire crushing, bog rolls and module damage arrive with A4 / A5b) — depends on: FlowFieldManager, MovementSystem, MapData
// Vehicles do not use separation. Each follows the tracked-mode flow field of its goal with a turn-rate limit,
// slowing while its heading disagrees with the field, and may only enter trench cells the flow field considers
// crossable (FlowFieldManager.TrenchCrossable). Wheeled profiles and slope limits are data for A5b.
using System;
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;
using TW.Sim.Terrain;

namespace TW.Sim.Nav
{
    public struct VehicleProfile
    {
        public float TurnRateRad;      // rad/s
        public float TrenchCrossWidth; // metres; 0 = cannot cross
        public float SlopeLimitRad;    // max climbable slope (A5b)
        public float BogChance;        // per-tick base probability on Mud (A4)
        public bool Wheeled;           // road-only effective speed (A5b)

        /// <summary>Phase 0 roster: archetype 4 is the Mark IV. C2 replaces this with baked UnitStats.</summary>
        public static VehicleProfile ForArchetype(byte archetype)
            => new VehicleProfile { TurnRateRad = 0.5f, TrenchCrossWidth = 3.5f, SlopeLimitRad = 0.35f, BogChance = 0.02f, Wheeled = false };
    }

    public sealed class VehicleKinematicsSystem : ISimSystem
    {
        public int Order => SimSystemOrder.VehicleKinematics;

        readonly MapData map;
        FlowFieldManager fields;
        MovementSystem movement;

        public VehicleKinematicsSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            fields = world.GetSystem<FlowFieldManager>() ?? throw new InvalidOperationException("VehicleKinematicsSystem needs FlowFieldManager registered before it");
            movement = world.GetSystem<MovementSystem>() ?? throw new InvalidOperationException("VehicleKinematicsSystem needs MovementSystem registered before it");
        }

        public void Step(SimWorld w)
        {
            if (movement.Vehicles.Length == 0) return;
            new VehicleJob
            {
                Vehicles = movement.Vehicles.AsArray(),
                Position = w.Position, Velocity = w.Velocity, Yaw = w.Yaw, Layer = w.Layer, StanceOf = w.StanceOf, Flags = w.Flags,
                Speed = w.Speed, Archetype = w.Archetype, GoalId = w.GoalId,
                Directions = fields.Direction, Ready = fields.Ready, TrenchCrossable = fields.TrenchCrossable,
                Layers = map.NavLayers, CellTrenchId = map.CellTrenchId,
                NavWidth = map.NavWidth, NavLength = map.NavLength, CellCount = fields.CellCount, NavCell = MapData.NavCellSize,
                Size = map.SizeMeters, Dt = w.Config.TickSeconds,
            }.Run();
        }

        // Few vehicles, so a single-threaded job over the vehicle list: no parallel-write restrictions to work around.
        [BurstCompile(FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct VehicleJob : IJob
        {
            [ReadOnly] public NativeArray<int> Vehicles;
            public NativeArray<float3> Position, Velocity;
            public NativeArray<float> Yaw;
            public NativeArray<byte> Layer, StanceOf;
            [ReadOnly] public NativeArray<uint> Flags;
            [ReadOnly] public NativeArray<float> Speed;
            [ReadOnly] public NativeArray<byte> Archetype;
            [ReadOnly] public NativeArray<int> GoalId;
            [ReadOnly] public NativeArray<byte> Directions;
            [ReadOnly] public NativeArray<byte> Ready;
            [ReadOnly] public NativeArray<byte> TrenchCrossable;
            [ReadOnly] public NativeArray<byte> Layers;
            [ReadOnly] public NativeArray<short> CellTrenchId;
            public int NavWidth, NavLength, CellCount;
            public float NavCell, Dt;
            public float2 Size;

            int CellOf(float3 p)
            {
                int cx = math.clamp((int)(p.x / NavCell), 0, NavWidth - 1);
                int cz = math.clamp((int)(p.z / NavCell), 0, NavLength - 1);
                return cz * NavWidth + cx;
            }

            static float WrapAngle(float a)
            {
                const float twoPi = 2f * SimMath.Pi;
                while (a > SimMath.Pi) a -= twoPi;
                while (a < -SimMath.Pi) a += twoPi;
                return a;
            }

            public void Execute()
            {
                for (int k = 0; k < Vehicles.Length; k++)
                {
                    int i = Vehicles[k];
                    uint f = Flags[i];
                    Layer[i] = (byte)NavLayer.Surface;          // a vehicle is never "in" a trench for cover purposes
                    StanceOf[i] = (byte)Stance.Standing;
                    if ((f & (uint)(UnitFlags.Immobilised | UnitFlags.Stalled | UnitFlags.Bogged)) != 0) { Velocity[i] = float3.zero; continue; }

                    float3 p = Position[i];
                    int cell = CellOf(p);
                    int goal = GoalId[i];
                    if (goal < 0 || Ready[goal] == 0) { Velocity[i] = float3.zero; continue; }
                    byte d = Directions[goal * CellCount + cell];
                    if (d == FlowField.NoDirection) { Velocity[i] = float3.zero; continue; }

                    // steer: turn toward the field direction at the profile's rate, drive along the current heading
                    var profile = VehicleProfile.ForArchetype(Archetype[i]);
                    float2 want = FlowField.Offset(d);
                    float desiredYaw = SimMath.YawOf(new float3(want.x, 0f, want.y));
                    float yaw = Yaw[i];
                    float delta = WrapAngle(desiredYaw - yaw);
                    float maxTurn = profile.TurnRateRad * Dt;
                    yaw = WrapAngle(yaw + math.clamp(delta, -maxTurn, maxTurn));
                    float remaining = WrapAngle(desiredYaw - yaw);
                    float align = math.max(0.25f, SimMath.Cos(remaining));   // crawl through tight turns rather than stop
                    float3 heading = new float3(SimMath.Sin(yaw), 0f, SimMath.Cos(yaw));
                    byte from = Layers[cell];
                    float terrain = (from & (byte)NavLayer.Trench) != 0 ? 0.5f : (from & (byte)NavLayer.Mud) != 0 ? 0.5f : 1f;
                    float3 v = heading * Speed[i] * align * terrain;
                    float3 np = p + v * Dt;
                    np.x = math.clamp(np.x, 1f, Size.x - 1f);
                    np.z = math.clamp(np.z, 1f, Size.y - 1f);
                    int ncell = CellOf(np);
                    if (!FlowField.CanStep(NavMode.Tracked, from, Layers[ncell], CellTrenchId[ncell], TrenchCrossable)) { np = p; v = float3.zero; }
                    Position[i] = np;
                    Velocity[i] = v;
                    Yaw[i] = yaw;
                }
            }
        }

        public ulong Hash(ulong h) => h;
        public void Dispose() { }
    }
}
