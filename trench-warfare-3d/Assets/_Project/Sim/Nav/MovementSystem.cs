// Phase: A1 (initial implementation) — depends on: FlowField, SpatialHash, SeparationJob, MapData
// Moves every alive unit along the flow field of its team's goal, applies separation, updates the unit's
// nav layer from the cell it occupies and flags it Exposed when it vaults onto the surface.
// Still to do in A1: goal groups via FlowFieldManager, trench garrison stop, stance/terrain speed
// multipliers (mud, wire, sprint), vehicle kinematics.
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
        public FlowField FieldTeam0, FieldTeam1;   // goal = enemy HQ objective cells
        public SpatialHash Spatial;
        NativeArray<float3> push;

        public MovementSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            FieldTeam0 = new FlowField(map.NavWidth, map.NavLength, Allocator.Persistent);
            FieldTeam1 = new FlowField(map.NavWidth, map.NavLength, Allocator.Persistent);
            BuildTeamField(ref FieldTeam0, 0);
            BuildTeamField(ref FieldTeam1, 1);
            Spatial = new SpatialHash(map.SizeMeters, 1f, world.Config.MaxSlots * 2, Allocator.Persistent);
            push = new NativeArray<float3>(world.Config.MaxSlots, Allocator.Persistent);
        }

        void BuildTeamField(ref FlowField field, byte team)
        {
            // Goal: the enemy HQ objective; falls back to the enemy spawn cell when the map has no HQ.
            var goals = new NativeList<int>(256, Allocator.Temp);
            for (int i = 0; i < map.Objectives.Length; i++)
            {
                var o = map.Objectives[i];
                if (o.Kind == ObjectiveKind.HQ && o.SideTeam != team)
                    for (int c = 0; c < o.CellCount; c++) goals.Add(map.ObjectiveCells[o.CellStart + c]);
            }
            if (goals.Length == 0)
            {
                for (int i = 0; i < map.Spawns.Length; i++)
                    if (map.Spawns[i].Team != team) { var c = map.NavCellOf(map.Spawns[i].Pos); goals.Add(map.NavIndex(c.x, c.y)); }
            }
            field.Build(map, goals.AsArray());
            goals.Dispose();
        }

        public void Step(SimWorld w)
        {
            int n = w.HighWater;
            if (n == 0) return;
            Spatial.Rebuild(w.Position, w.Flags, n);
            new SeparationJob { Hash = Spatial, Position = w.Position, Flags = w.Flags, Push = push }.Schedule(n, 64).Complete();
            new MoveJob
            {
                Position = w.Position, Velocity = w.Velocity, Yaw = w.Yaw, Layer = w.Layer, StanceOf = w.StanceOf, Flags = w.Flags,
                Speed = w.Speed, Team = w.Team, Push = push,
                Dir0 = FieldTeam0.Direction, Dir1 = FieldTeam1.Direction, Layers = map.NavLayers,
                NavWidth = map.NavWidth, NavLength = map.NavLength, NavCell = MapData.NavCellSize,
                Size = map.SizeMeters, Dt = w.Config.TickSeconds,
            }.Schedule(n, 64).Complete();
        }

        [BurstCompile(FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct MoveJob : IJobParallelFor
        {
            public NativeArray<float3> Position, Velocity;
            public NativeArray<float> Yaw;
            public NativeArray<byte> Layer, StanceOf;
            public NativeArray<uint> Flags;
            [ReadOnly] public NativeArray<float> Speed;
            [ReadOnly] public NativeArray<byte> Team;
            [ReadOnly] public NativeArray<float3> Push;
            [ReadOnly] public NativeArray<byte> Dir0, Dir1, Layers;
            public int NavWidth, NavLength;
            public float NavCell, Dt;
            public float2 Size;

            public void Execute(int i)
            {
                uint f = Flags[i];
                if ((f & (uint)UnitFlags.Alive) == 0) return;
                float3 p = Position[i];
                int cx = math.clamp((int)(p.x / NavCell), 0, NavWidth - 1);
                int cz = math.clamp((int)(p.z / NavCell), 0, NavLength - 1);
                int cell = cz * NavWidth + cx;
                byte d = Team[i] == 0 ? Dir0[cell] : Dir1[cell];
                float2 dir = d == FlowField.NoDirection ? float2.zero : FlowField.Offset(d);
                float3 v = new float3(dir.x, 0f, dir.y) * Speed[i] + Push[i];
                float3 np = p + v * Dt;
                np.x = math.clamp(np.x, 0.5f, Size.x - 0.5f);
                np.z = math.clamp(np.z, 0.5f, Size.y - 0.5f);
                // block moves into cells that are not steppable from the current one
                int nx = math.clamp((int)(np.x / NavCell), 0, NavWidth - 1);
                int nz = math.clamp((int)(np.z / NavCell), 0, NavLength - 1);
                int ncell = nz * NavWidth + nx;
                byte from = Layers[cell], to = Layers[ncell];
                bool ok = (to & (byte)NavLayer.Blocked) == 0 &&
                          (((from | to) & (byte)NavLayer.Link) != 0 || (from & to & (byte)(NavLayer.Surface | NavLayer.Trench)) != 0);
                if (!ok) { np = p; v = float3.zero; ncell = cell; to = from; }
                Position[i] = np;
                Velocity[i] = v;
                if (SimMath.Length(v) > 0.05f) Yaw[i] = SimMath.YawOf(v);
                // layer bookkeeping: trench vs surface, exposed when on the surface heading for the enemy
                byte layer = (to & (byte)NavLayer.Trench) != 0 ? (byte)NavLayer.Trench : (byte)NavLayer.Surface;
                bool wasTrench = Layer[i] == (byte)NavLayer.Trench;
                Layer[i] = layer;
                if (layer == (byte)NavLayer.Surface) { f |= (uint)UnitFlags.Exposed; f &= ~(uint)UnitFlags.InTrench; StanceOf[i] = (byte)(wasTrench ? Stance.Vault : Stance.Sprint); }
                else { f &= ~(uint)UnitFlags.Exposed; f |= (uint)UnitFlags.InTrench; StanceOf[i] = (byte)Stance.Crouch; }
                Flags[i] = f;
            }
        }

        public ulong Hash(ulong h) { h = FieldTeam0.Hash(h); return FieldTeam1.Hash(h); }

        public void Dispose()
        {
            FieldTeam0.Dispose(); FieldTeam1.Dispose(); Spatial.Dispose();
            if (push.IsCreated) push.Dispose();
        }
    }
}
