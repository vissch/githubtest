// Phase: A1 (implemented) — depends on: FlowFieldManager, SpatialHash, SeparationJob, MapData, StanceRules
// Infantry movement. Every alive infantry slot follows the flow field of its goal, takes a separation push from its
// neighbours (and keeps clear of vehicles), and moves at base speed × stance × terrain. A unit without a goal gets
// the team's default goal (its front trench). Arriving at the goal trench garrisons the unit: it stops, crouches
// below the rim and stays until TrenchOrdersSystem hands it a new goal; a locked trench passes arrivals straight on
// to the next goal. Layer bookkeeping: on a Surface cell a unit under orders is Exposed and sprints, otherwise it
// walks; the tick it leaves a trench it vaults. Vehicles are moved by VehicleKinematicsSystem, not here.
using System;
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;
using TW.Sim.Terrain;

namespace TW.Sim.Nav
{
    public sealed class MovementSystem : ISimSystem
    {
        public int Order => SimSystemOrder.Movement;

        readonly MapData map;
        FlowFieldManager fields;
        public SpatialHash Spatial;
        public NativeList<int> Vehicles;      // alive vehicle slots this tick, in slot order (transient)
        NativeArray<float3> push;
        NativeArray<short> arrivedLocked;     // transient: trench id a unit reached while it was locked (-1 none)
        NativeArray<short> garrisoned;        // transient: trench id a unit garrisoned this tick (-1 none)

        public MovementSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            fields = world.GetSystem<FlowFieldManager>() ?? throw new InvalidOperationException("MovementSystem needs FlowFieldManager registered before it");
            int n = world.Config.MaxSlots;
            Spatial = new SpatialHash(map.SizeMeters, 1f, n * 2, Allocator.Persistent);
            Vehicles = new NativeList<int>(64, Allocator.Persistent);
            push = new NativeArray<float3>(n, Allocator.Persistent);
            arrivedLocked = new NativeArray<short>(n, Allocator.Persistent);
            garrisoned = new NativeArray<short>(n, Allocator.Persistent);
        }

        public void Step(SimWorld w)
        {
            int n = w.HighWater;
            if (n == 0) return;

            // 1. default goals for fresh units, vehicle list (main thread, slot order)
            Vehicles.Clear();
            for (int i = 0; i < n; i++)
            {
                uint f = w.Flags[i];
                if ((f & (uint)UnitFlags.Alive) == 0) continue;
                bool vehicle = (f & (uint)UnitFlags.Vehicle) != 0;
                if (vehicle) Vehicles.Add(i);
                if (w.GoalId[i] < 0 && w.TrenchId[i] < 0) w.GoalId[i] = fields.DefaultGoal(w.Team[i], vehicle);
            }

            // 2. neighbours
            Spatial.Rebuild(w.Position, w.Flags, n);
            new SeparationJob { Hash = Spatial, Position = w.Position, Flags = w.Flags, Vehicles = Vehicles.AsArray(), Push = push }
                .Schedule(n, 64).Complete();

            // 3. move
            new MoveJob
            {
                Position = w.Position, Velocity = w.Velocity, Yaw = w.Yaw, Layer = w.Layer, StanceOf = w.StanceOf, Flags = w.Flags,
                GoalId = w.GoalId, TrenchId = w.TrenchId, ArrivedLocked = arrivedLocked, Garrisoned = garrisoned,
                Speed = w.Speed, Push = push,
                Directions = fields.Direction, Ready = fields.Ready, Goals = fields.Goals, Trenches = fields.Trenches,
                Layers = map.NavLayers, CellTrenchId = map.CellTrenchId,
                NavWidth = map.NavWidth, NavLength = map.NavLength, CellCount = fields.CellCount, NavCell = MapData.NavCellSize,
                Size = map.SizeMeters, Dt = w.Config.TickSeconds,
            }.Schedule(n, 64).Complete();

            // 4. arrivals (main thread): events for garrisons, re-goal for locked trenches
            for (int i = 0; i < n; i++)
            {
                if (garrisoned[i] >= 0)
                {
                    w.Events.Add(w.Tick, SimEventType.UnitEnteredTrench, i, garrisoned[i], w.Position[i]);
                    garrisoned[i] = -1;
                }
                if (arrivedLocked[i] >= 0)
                {
                    w.GoalId[i] = fields.NextGoalFrom(arrivedLocked[i], w.Team[i]);
                    arrivedLocked[i] = -1;
                }
            }
        }

        [BurstCompile(FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct MoveJob : IJobParallelFor
        {
            public NativeArray<float3> Position, Velocity;
            public NativeArray<float> Yaw;
            public NativeArray<byte> Layer, StanceOf;
            public NativeArray<uint> Flags;
            public NativeArray<int> GoalId;
            public NativeArray<short> TrenchId;
            public NativeArray<short> ArrivedLocked, Garrisoned;
            [ReadOnly] public NativeArray<float> Speed;
            [ReadOnly] public NativeArray<float3> Push;
            [ReadOnly] public NativeArray<byte> Directions;
            [ReadOnly] public NativeArray<byte> Ready;
            [ReadOnly] public NativeArray<GoalKey> Goals;
            [ReadOnly] public NativeArray<TrenchState> Trenches;
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

            public void Execute(int i)
            {
                ArrivedLocked[i] = -1;
                Garrisoned[i] = -1;
                uint f = Flags[i];
                if ((f & (uint)UnitFlags.Alive) == 0 || (f & (uint)UnitFlags.Vehicle) != 0) return;

                float3 p = Position[i];
                int cell = CellOf(p);
                byte from = Layers[cell];
                bool inTrench = (from & (byte)NavLayer.Trench) != 0;
                int goal = GoalId[i];
                short garrison = TrenchId[i];
                bool isGarrisoned = garrison >= 0;

                // steering
                float2 dir = float2.zero;
                if (!isGarrisoned && goal >= 0 && Ready[goal] != 0)
                {
                    byte d = Directions[goal * CellCount + cell];
                    if (d != FlowField.NoDirection) dir = FlowField.Offset(d);
                }

                // stance from situation; A2/A3 layer suppression, fire-step and player overrides on top of this
                Stance stance = isGarrisoned || inTrench ? Stance.Crouch : ((f & (uint)UnitFlags.Exposed) != 0 ? Stance.Sprint : Stance.Standing);
                float speed = Speed[i] * StanceRules.SpeedMultiplier(stance) * StanceRules.TerrainMultiplier(from);
                float3 v = isGarrisoned ? Push[i] : new float3(dir.x, 0f, dir.y) * speed + Push[i];   // a garrison only spreads out
                float3 np = p + v * Dt;
                np.x = math.clamp(np.x, 0.5f, Size.x - 0.5f);
                np.z = math.clamp(np.z, 0.5f, Size.y - 0.5f);

                // block moves into cells that are not steppable from the current one; a garrison never leaves its trench
                int ncell = CellOf(np);
                byte to = Layers[ncell];
                bool ok = isGarrisoned
                    ? CellTrenchId[ncell] == garrison && (to & (byte)NavLayer.Link) == 0   // keep the ladders clear for arrivals
                    : FlowField.CanStepInfantry(from, to);
                if (!ok) { np = p; v = float3.zero; ncell = cell; to = from; }
                Position[i] = np;
                Velocity[i] = v;
                if (SimMath.Length(v) > 0.05f) Yaw[i] = SimMath.YawOf(v);

                // layer bookkeeping
                bool nowTrench = (to & (byte)NavLayer.Trench) != 0;
                Layer[i] = nowTrench ? (byte)NavLayer.Trench : (byte)NavLayer.Surface;
                if (inTrench && !nowTrench) stance = Stance.Vault;

                // arrival: the goal is this trench
                if (!isGarrisoned && goal >= 0 && nowTrench && (to & (byte)NavLayer.Link) == 0)   // off the ladder, in the trench body
                {
                    var g = Goals[goal];
                    short t = CellTrenchId[ncell];
                    if (g.Kind == GoalKind.Trench && t == g.Ref)
                    {
                        if (Trenches[t].Locked != 0) ArrivedLocked[i] = t;
                        else
                        {
                            TrenchId[i] = t; GoalId[i] = -1; Velocity[i] = float3.zero;
                            Garrisoned[i] = t;
                            stance = Stance.Crouch;
                            f &= ~(uint)UnitFlags.Exposed;
                        }
                    }
                }
                if (nowTrench) f |= (uint)UnitFlags.InTrench; else f &= ~(uint)UnitFlags.InTrench;
                StanceOf[i] = (byte)stance;
                Flags[i] = f;
            }
        }

        public ulong Hash(ulong h) => h;   // no state of its own: goals/trenches live in FlowFieldManager, units in SimWorld

        public void Dispose()
        {
            if (Spatial.IsCreated) Spatial.Dispose();
            if (Vehicles.IsCreated) Vehicles.Dispose();
            if (push.IsCreated) push.Dispose();
            if (arrivedLocked.IsCreated) arrivedLocked.Dispose();
            if (garrisoned.IsCreated) garrisoned.Dispose();
        }
    }
}
