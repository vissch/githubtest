// Phase: A1 (implemented) — depends on: FlowFieldManager, SpatialHash, SeparationJob, MapData, StanceRules
// Infantry movement. Every alive infantry slot follows the flow field of its goal, takes a separation push from its
// neighbours (and keeps clear of vehicles), and moves at base speed × stance × terrain. A unit without a goal gets
// the team's default goal (its front trench). Arriving at the goal trench garrisons the unit: it stops, crouches
// below the rim and stays until TrenchOrdersSystem hands it a new goal; a locked trench passes arrivals straight on
// to the next goal. Layer bookkeeping: on a Surface cell a unit under orders is Exposed and sprints, otherwise it
// walks; the tick it leaves a trench it vaults. Vehicles are moved by VehicleKinematicsSystem, not here.
// Spread: the flow direction is blended between the four cells round the man (no eight-way zigzag lines), and on
// open ground each man carries a slow, hashed lateral drift, so a company fans out across the field instead of
// filing along one line; SeparationJob adds a soft comfortable spacing on the surface. All of it is deterministic.
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
            new SeparationJob { Hash = Spatial, Position = w.Position, Flags = w.Flags, TrenchId = w.TrenchId, Vehicles = Vehicles.AsArray(), Push = push }
                .Schedule(n, 64).Complete();

            // 3. move
            new MoveJob
            {
                Position = w.Position, Velocity = w.Velocity, Yaw = w.Yaw, Layer = w.Layer, StanceOf = w.StanceOf, Flags = w.Flags,
                PostCell = w.PostCell, PostKind = w.PostKind, TrenchDefs = map.Trenches.AsArray(), Team = w.Team,
                GoalId = w.GoalId, Cooldown = w.Cooldown, Knock = w.Knock, TrenchId = w.TrenchId, ArrivedLocked = arrivedLocked, Garrisoned = garrisoned,
                Speed = w.Speed, Push = push, Suppression = w.Suppression, TargetSlot = w.TargetSlot, Generation = w.Generation, Tick = w.Tick,
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

        [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct MoveJob : IJobParallelFor
        {
            public NativeArray<float3> Position, Velocity;
            public NativeArray<float> Yaw;
            public NativeArray<byte> Layer, StanceOf;
            public NativeArray<uint> Flags;
            public NativeArray<int> GoalId, Cooldown;
            public NativeArray<float3> Knock;
            public NativeArray<short> TrenchId;
            public NativeArray<short> ArrivedLocked, Garrisoned;
            [ReadOnly] public NativeArray<float> Speed, Suppression;
            [ReadOnly] public NativeArray<int> TargetSlot;
            [ReadOnly] public NativeArray<ushort> Generation;
            [ReadOnly] public NativeArray<float3> Push;
            public uint Tick;
            public const float DriftAmount = 0.38f;     // lateral drift as a fraction of the forward speed
            public const float DriftPeriodTicks = 320f; // one wander cycle: 16 s
            public const float KnockDecay = 0.78f;      // per tick: a throw of 9 m/s carries him about 2 m over a second
            public const int VaultTicks = 16;          // a man takes 0.8 s to get over the parapet (Cooldown counts it; the climb is drawn up the wall)
            [ReadOnly] public NativeArray<byte> Directions;
            [ReadOnly] public NativeArray<byte> Ready;
            [ReadOnly] public NativeArray<GoalKey> Goals;
            [ReadOnly] public NativeArray<TrenchState> Trenches;
            [ReadOnly] public NativeArray<byte> Layers;
            public const byte PostReserve = 2;
            public const float PostReached = 0.22f;   // m: near enough to his post to be standing at it. Posts are a nav
                                                      // cell apart (2 m), so a loose tolerance would let two men at
                                                      // neighbouring posts stand within a metre of each other

            [ReadOnly] public NativeArray<short> CellTrenchId;
            [ReadOnly] public NativeArray<int> PostCell;          // A3: his post in the trench (TrenchGarrisonSystem), -1 none
            [ReadOnly] public NativeArray<byte> PostKind;         // 1 firing (at the parapet), 2 reserve
            [ReadOnly] public NativeArray<TW.Sim.Terrain.TrenchDef> TrenchDefs;
            [ReadOnly] public NativeArray<byte> Team;
            public int NavWidth, NavLength, CellCount;
            public float NavCell, Dt;
            public float2 Size;

            /// <summary>The middle of a nav cell, where a post stands.</summary>
            float3 CellCentre(int cell) => new float3((cell % NavWidth + 0.5f) * NavCell, 0f, (cell / NavWidth + 0.5f) * NavCell);
            /// <summary>His own spot in his post cell. Cell centres put a garrison on a visible 2 m lattice.</summary>
            float3 PostPoint(int cell) => CellCentre(cell) + TrenchPost.Offset(cell, NavWidth);

            int CellOf(float3 p)
            {
                int cx = math.clamp((int)(p.x / NavCell), 0, NavWidth - 1);
                int cz = math.clamp((int)(p.z / NavCell), 0, NavLength - 1);
                return cz * NavWidth + cx;
            }

            /// <summary>The flow direction blended between the four cells round a point; cells without a direction are left out.</summary>
            float2 Flow(int goal, float3 p)
            {
                float fx = p.x / NavCell - 0.5f, fz = p.z / NavCell - 0.5f;
                int x0 = (int)math.floor(fx), z0 = (int)math.floor(fz);
                float tx = fx - x0, tz = fz - z0;
                float2 sum = float2.zero; float total = 0f;
                for (int k = 0; k < 4; k++)
                {
                    int x = math.clamp(x0 + (k & 1), 0, NavWidth - 1), z = math.clamp(z0 + (k >> 1), 0, NavLength - 1);
                    float wgt = ((k & 1) == 0 ? 1f - tx : tx) * ((k >> 1) == 0 ? 1f - tz : tz);
                    byte d = Directions[goal * CellCount + z * NavWidth + x];
                    if (d == FlowField.NoDirection || wgt <= 0f) continue;
                    sum += FlowField.Offset(d) * wgt; total += wgt;
                }
                if (total <= 0f) return float2.zero;
                float len = SimMath.Length(new float3(sum.x, 0f, sum.y));
                return len > 1e-4f ? sum / len : float2.zero;
            }

            bool CanEnter(bool isGarrisoned, short garrison, bool onLadder, bool toPost, float pushX, byte from, int ncell)
            {
                byte to = Layers[ncell];
                // a garrison keeps the ladders clear for arrivals, but a man walking to his post may cross one: without
                // that the ladders cut the trench into segments and half the garrison can never reach the post it was
                // given, so a company bunches against the nearest ladder instead of spreading along the line
                return isGarrisoned
                    ? CellTrenchId[ncell] == garrison && ((to & (byte)NavLayer.Link) == 0 || onLadder || toPost)
                    : FlowField.CanStepInfantry(from, to);
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
                    dir = Flow(goal, p);
                    if (SimMath.Length(new float3(dir.x, 0f, dir.y)) < 0.5f)
                    {
                        byte d = Directions[goal * CellCount + cell];
                        dir = d != FlowField.NoDirection ? FlowField.Offset(d) : float2.zero;
                    }
                    else if (!inTrench)
                    {
                        // open ground: a slow wander to one side and back, different for every man, so the company spreads
                        uint seed = (uint)i * 2654435761u ^ (uint)Generation[i] * 40503u;
                        float phase = (seed & 0xFFFF) / 65536f * 6.2831853f;
                        float lat = DriftAmount * SimMath.Sin(Tick * (6.2831853f / DriftPeriodTicks) + phase);
                        float2 side = new float2(-dir.y, dir.x);
                        float2 mixed = dir + side * lat;
                        float ml = SimMath.Length(new float3(mixed.x, 0f, mixed.y));
                        if (ml > 1e-4f) dir = mixed / ml;
                    }
                }

                // stance from situation: a garrison mans the fire-step while it has a target, suppression forces prone /
                // pinned in the open; A3 adds player overrides on top of this.
                // Only a man at a FIRING post goes up on the step: the rest of the garrison keeps its head down at the
                // rear wall, so a trench reads as a firing line with men in support rather than one long queue.
                float supp = Suppression[i];
                Stance stance;
                bool ladderPost = PostCell[i] >= 0 && (Layers[PostCell[i]] & (byte)NavLayer.Link) != 0;
                bool atPost = PostCell[i] < 0 || ladderPost || PostCell[i] == cell || SimMath.Length(PostPoint(PostCell[i]) - p) < 0.9f;
                bool toPost = isGarrisoned && PostCell[i] >= 0 && !atPost && !ladderPost;   // walking to his post: he may cross a ladder, and keeps going across it
                if (isGarrisoned) stance = TargetSlot[i] >= 0 && PostKind[i] != PostReserve && atPost ? Stance.FireStep : Stance.Crouch;
                else if (supp >= StanceRules.PinnedSuppression) stance = Stance.Pinned;
                else if (inTrench) stance = Stance.Crouch;
                else if (supp >= StanceRules.ProneSuppression) stance = Stance.Prone;
                else stance = (f & (uint)UnitFlags.Exposed) != 0 ? Stance.Sprint : Stance.Standing;
                float speed = Speed[i] * StanceRules.SpeedMultiplier(stance) * StanceRules.TerrainMultiplier(from);
                float3 v = isGarrisoned ? Push[i] : new float3(dir.x, 0f, dir.y) * speed + Push[i];   // a garrison only spreads out
                if (isGarrisoned)
                {
                    // he walks to the post he was given and holds it (the parapet, a junction, a dugout mouth), instead of
                    // relaxing onto the duckboard centreline under separation alone
                    // a post on a ladder cell is not a place to stand: the ladders stay clear for men coming over the top,
                    // so he ignores it and the safety net below steps him off if he is on one
                    int post = PostCell[i];
                    if (post >= 0 && (Layers[post] & (byte)NavLayer.Link) != 0) post = -1;
                    if (post >= 0 && post < CellCount)
                    {
                        float3 want = PostPoint(post);
                        float2 toPostDir = new float2(want.x - p.x, want.z - p.z);
                        float d = SimMath.Length(new float3(toPostDir.x, 0f, toPostDir.y));
                        // eased, so he settles onto his post instead of overshooting it and jostling the man at the next one
                        if (d > PostReached)
                            v += new float3(toPostDir.x, 0f, toPostDir.y) / d * (Speed[i] * StanceRules.SpeedMultiplier(Stance.Crouch) * math.saturate((d - PostReached) / 1.2f));
                    }
                    if (stance == Stance.FireStep)
                    {
                        // up at the parapet he faces the field, not the exact bearing of his target: a line of men at the
                        // top of the trench, which is what a fire step is for
                        short t = TrenchId[i];
                        if (t >= 0 && t < TrenchDefs.Length)
                        {
                            var def = TrenchDefs[t];
                            Yaw[i] = SimMath.WrapAngle(def.OwnerTeam == Team[i] ? def.FacingYaw : def.FacingYaw + SimMath.Pi);
                        }
                    }
                }
                // thrown by a shell: the throw replaces his own steering until it is spent
                float3 knock = Knock[i];
                if (math.lengthsq(knock) > 0.09f && !isGarrisoned) { v = knock; Knock[i] = knock * KnockDecay; }
                else if (math.lengthsq(knock) > 0f) Knock[i] = float3.zero;
                bool onLadder = isGarrisoned && (from & (byte)NavLayer.Link) != 0;
                if (onLadder)
                {
                    // a garrison never stands on a ladder. A man crossing one on his way to his post is already being
                    // carried the right way, and nudging him to the nearer side would cancel that and strand him on the
                    // rungs; everyone else steps off to the nearer side.
                    if (toPost) v.x += math.sign(v.x) * 1.5f;
                    else
                    {
                        float centre = ((int)(p.x / NavCell) + 0.5f) * NavCell;
                        v.x += (p.x >= centre ? 1f : -1f) * 2.5f;
                    }
                }
                float3 np = p + v * Dt;
                np.x = math.clamp(np.x, 0.5f, Size.x - 0.5f);
                np.z = math.clamp(np.z, 0.5f, Size.y - 0.5f);

                // block moves into cells that are not steppable from the current one; a garrison never leaves its trench.
                // A blocked step slides along the obstacle (X only, then Z only) instead of stopping dead: a diagonal
                // path that clips a trench wall next to a ladder would otherwise pin the unit there for good.
                float pushX = Push[i].x;
                int ncell = CellOf(np);
                if (!CanEnter(isGarrisoned, garrison, onLadder, toPost, pushX, from, ncell))
                {
                    // the blended flow at the corner of a trench cell beside a ladder points through the wall: the cell's
                    // own direction never does, so a blocked step takes that first (it used to oscillate against the wall
                    // beside the ladder for good), then slides along the obstacle
                    byte d = !isGarrisoned && goal >= 0 && Ready[goal] != 0 ? Directions[goal * CellCount + cell] : FlowField.NoDirection;
                    float2 o = d != FlowField.NoDirection ? FlowField.Offset(d) : float2.zero;
                    float3 v2 = new float3(o.x, 0f, o.y) * speed; float3 np2 = p + v2 * Dt; int cell2 = CellOf(np2);
                    float3 slideX = new float3(np.x, np.y, p.z), slideZ = new float3(p.x, np.y, np.z);
                    int cellX = CellOf(slideX), cellZ = CellOf(slideZ);
                    if (d != FlowField.NoDirection && CanEnter(isGarrisoned, garrison, onLadder, toPost, pushX, from, cell2)) { np = np2; v = v2; ncell = cell2; }
                    else if (math.abs(v.x) > 1e-4f && CanEnter(isGarrisoned, garrison, onLadder, toPost, pushX, from, cellX)) { np = slideX; v.z = 0f; ncell = cellX; }
                    else if (math.abs(v.z) > 1e-4f && CanEnter(isGarrisoned, garrison, onLadder, toPost, pushX, from, cellZ)) { np = slideZ; v.x = 0f; ncell = cellZ; }
                    else { np = p; v = float3.zero; ncell = cell; }
                }
                byte to = Layers[ncell];
                // the parapet: the step that would take him out of the trench holds him at the edge for VaultTicks first
                // (the climb has to be seen), then lets him over
                bool crossing = inTrench && (to & (byte)NavLayer.Trench) == 0;
                if (crossing && Cooldown[i] < VaultTicks) { Cooldown[i]++; np = p; v = float3.zero; ncell = cell; to = from; stance = Stance.Vault; }
                else if (!crossing && Cooldown[i] > 0 && Cooldown[i] < VaultTicks && (to & (byte)NavLayer.Trench) != 0 && !isGarrisoned) { Cooldown[i]++; np = p; v = float3.zero; ncell = cell; to = from; stance = Stance.Vault; }   // shoved sideways mid-climb: he keeps climbing
                else if (!crossing) Cooldown[i] = 0;
                Position[i] = np;
                Velocity[i] = v;
                if (SimMath.Length(v) > 0.05f && !(isGarrisoned && stance == Stance.FireStep)) Yaw[i] = SimMath.YawOf(v);   // on the step he keeps facing over the parapet

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
