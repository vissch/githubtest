// Phase: A5b (implemented) — depends on: FlowFieldManager, MovementSystem (vehicle list, spatial hash), MapData, WireBelt,
// PropDef, SimRandom.SystemId.Bog. Damage (engine, tracks, crew) reaches here through the unit flags and SpeedFactor,
// which VehicleModulesSystem writes; TankGunnerySystem asks for a halt to lay a gun through HaltTicks.
// How a tank gets across the battlefield. Each follows the tracked-mode flow field of its goal:
//  - steering: it turns toward the field at its profile's rate; a turn sharper than PivotAngle is made on the spot
//    (one track forward, one back), anything gentler is driven round while the heading closes;
//  - trenches: a trench no wider than its TrenchCrossWidth is bridged at CrossSpeed. A wider one, up to the
//    tracked flow field's limit (FlowFieldManager.TrackedCrossWidth, one field for every tank), is tried anyway and may
//    ditch it, the chance rising from 0 at TrenchCrossWidth to DitchChance at the limit: nose down in the trench for
//    DitchMin..DitchMax ticks before it claws out and goes on (one roll per trench). Wider still, the flow field routes
//    it round (TrenchCrossable). The Maw bridges everything the field allows; the Tusk risks the wider ones;
//  - ground: mud halves its speed and may bog it (BogChance per second moving, BogMin..BogMax ticks stuck), a shell
//    hole slows it, wire slows it a little and is crushed flat under it (WireBreached), a slope steeper than half its
//    SlopeLimit slows it to a crawl at the limit (trench walls do not count: it bridges them);
//  - obstacles: the flow field routes round what blocks a cell, but a hull is wider than its path: a tree under a
//    moving hull goes over into a log (a heavy tank, PushesTrees, flattens standing trees, any tank what is left of a
//    broken one; a tree it drives straight into goes the same way); wrecks stop anything;
//  - men: an enemy man in front of the hull at speed is run down (CrushDamage); friends are pushed aside by separation;
//  - other tanks: two hulls never overlap (pushed apart along the line between them, in slot order). A push obeys the
//    tracked rules (never into a blocked cell or a trench it is not already in) and never moves a tank that is not
//    under its own power (knocked out, stalled, immobilised, ditched, bogged);
// A knocked-out, stalled or immobilised vehicle does not move. All state is per slot and hashed; Gen notices a
// slot that was reused by a new vehicle.
using System;
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;
using TW.Sim.Terrain;

namespace TW.Sim.Nav
{
    /// <summary>How much larger than the sculpt a machine is built.
    ///
    /// Tools/crabsplit.py bakes each walker to about 3.3 m across and 2.6 m tall, and Tools/tanksplit.py the
    /// tanks to about 5 m by 4.5 m. Against a soldier drawn 2 m tall that made a walker man-height: the
    /// machines were never large, whatever the roster called them. These carry them to something that towers
    /// - a Pincer 9.5 m across with its belly above a man's head - and they are the only place the size is
    /// written, so presentation and simulation cannot drift apart. A walker grows more than a tank because a
    /// tank was already the size of a tank.
    ///
    /// A rebake must NOT fold these into crabsplit's own scale as well, or the machines come out squared.</summary>
    public static class VehicleSize
    {
        public const float Walker = 2.5f, Tank = 1.7f;
    }

    public struct VehicleProfile
    {
        public float TurnRateRad;      // rad/s
        public float TrenchCrossWidth; // metres bridged cleanly
        public float DitchChance;      // chance of ditching in a trench as wide as FlowFieldManager.TrackedCrossWidth (0 at TrenchCrossWidth)
        public float SlopeLimit;       // rise over run it can just climb
        public float BogChance;        // per second while moving on mud
        public float HalfLength, HalfWidth;   // footprint, metres (the tracks)
        public bool PushesTrees;
        public bool Wheeled;           // road-only effective speed (A5b data, no wheeled vehicle yet)
        public bool Walker;            // legs, not tracks: steps over trenches and wire, climbs, cannot ditch
        public byte Legs;              // how many it has to lose (VehicleModulesSystem)

        /// <summary>Is a world point under this hull's footprint: the rectangle HalfLength x HalfWidth in the hull's yaw
        /// (forward = (sin yaw, 0, cos yaw)). The one test for "under the hull": MineSystem's trigger and a mine's burst on
        /// the hull (VehicleModulesSystem) agree because both ask this.</summary>
        public bool Covers(float yaw, float3 centre, float3 point)
        {
            float3 q = point - centre;
            float s = SimMath.Sin(yaw), c = SimMath.Cos(yaw);
            float lx = q.x * c - q.z * s, lz = q.x * s + q.z * c;
            return math.abs(lx) <= HalfWidth && math.abs(lz) <= HalfLength;
        }

        /// <summary>The farthest the footprint reaches from the centre: a corner.</summary>
        public float Reach => SimMath.Length(new float3(HalfLength, 0f, HalfWidth));

        /// <summary>Two tanks and two walkers (VehicleArchetype). C2 replaces this with baked VehicleDefinition data.</summary>
        public static VehicleProfile ForArchetype(byte archetype)
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

        public static VehicleProfile Maw => new VehicleProfile
        { TurnRateRad = 0.42f, TrenchCrossWidth = FlowFieldManager.TrackedCrossWidth, DitchChance = 0f, SlopeLimit = 0.55f, BogChance = 0.05f, HalfLength = 2.55f * VehicleSize.Tank, HalfWidth = 2.25f * VehicleSize.Tank, PushesTrees = true };

        public static VehicleProfile Tusk => new VehicleProfile
        { TurnRateRad = 0.75f, TrenchCrossWidth = 2.4f, DitchChance = 0.75f, SlopeLimit = 0.6f, BogChance = 0.025f, HalfLength = 1.85f * VehicleSize.Tank, HalfWidth = 2.25f * VehicleSize.Tank };

        // The crabs, measured off Tools/crabsplit.py's crabs.json: Pincer 3.80 x 3.36 m, Kettle 3.20 x 2.65 m. A leg
        // finds its own footing, so mud barely holds them and a slope a tank would slide off is nothing; what stops a
        // walker is losing legs.
        public static VehicleProfile Pincer => new VehicleProfile
        { TurnRateRad = 1.15f, TrenchCrossWidth = 3.6f, DitchChance = 0f, SlopeLimit = 0.95f, BogChance = 0.008f, HalfLength = 1.70f * VehicleSize.Walker, HalfWidth = 1.90f * VehicleSize.Walker, PushesTrees = true, Walker = true, Legs = 6 };

        public static VehicleProfile Kettle => new VehicleProfile
        { TurnRateRad = 1.35f, TrenchCrossWidth = 3.0f, DitchChance = 0f, SlopeLimit = 0.90f, BogChance = 0.012f, HalfLength = 1.35f * VehicleSize.Walker, HalfWidth = 1.60f * VehicleSize.Walker, Walker = true, Legs = 4 };

        // Censer 3.30 x 2.95 m, Pavise 3.60 x 3.55 m. The gas crab is the quickest thing on the field on its feet;
        // the shielded one is the slowest, and plants itself to shoot.
        public static VehicleProfile Censer => new VehicleProfile
        { TurnRateRad = 1.45f, TrenchCrossWidth = 3.1f, DitchChance = 0f, SlopeLimit = 0.92f, BogChance = 0.010f, HalfLength = 1.50f * VehicleSize.Walker, HalfWidth = 1.65f * VehicleSize.Walker, Walker = true, Legs = 4 };

        public static VehicleProfile Pavise => new VehicleProfile
        { TurnRateRad = 0.95f, TrenchCrossWidth = 3.4f, DitchChance = 0f, SlopeLimit = 0.88f, BogChance = 0.014f, HalfLength = 1.78f * VehicleSize.Walker, HalfWidth = 1.80f * VehicleSize.Walker, PushesTrees = true, Walker = true, Legs = 4 };

        // Banner 2.03 x 3.40 m on four tall legs, Redoubt 2.97 x 3.60 m on six. The blockhouse is the heaviest thing
        // that walks and the slowest; the command walker is tall and narrow and steps over anything.
        public static VehicleProfile Banner => new VehicleProfile
        { TurnRateRad = 1.05f, TrenchCrossWidth = 3.5f, DitchChance = 0f, SlopeLimit = 0.94f, BogChance = 0.010f, HalfLength = 1.70f * VehicleSize.Walker, HalfWidth = 1.05f * VehicleSize.Walker, Walker = true, Legs = 4 };

        public static VehicleProfile Redoubt => new VehicleProfile
        { TurnRateRad = 0.80f, TrenchCrossWidth = 3.3f, DitchChance = 0f, SlopeLimit = 0.86f, BogChance = 0.018f, HalfLength = 1.80f * VehicleSize.Walker, HalfWidth = 1.50f * VehicleSize.Walker, PushesTrees = true, Walker = true, Legs = 6 };

        /// <summary>A round radius for keeping two hulls apart: the longer half plus a hand, so the corners of two
        /// hulls side by side or nose to flank do not pass through each other (the mean of length and width let the
        /// Maw's sponson into a Tusk's side).</summary>
        public float Radius => math.max(HalfLength, HalfWidth) + 0.25f;
    }

    public sealed class VehicleKinematicsSystem : ISimSystem
    {
        public const float CrossSpeed = 0.45f, MudSpeed = 0.5f, CraterSpeed = 0.7f, WireSpeed = 0.8f, PivotAngle = 1.1f, PivotSpeed = 0.06f;
        /// <summary>The same three for a walker, which is slowed far less by all of them (and not at all by wire).</summary>
        public const float StepOverSpeed = 0.72f, WadeSpeed = 0.78f, PickSpeed = 0.88f;
        public const int DitchMin = 240, DitchMax = 600, BogMin = 80, BogMax = 260;
        public const float CrushDamage = 200f, CrushSpeed = 0.4f;
        public int Order => SimSystemOrder.VehicleKinematics;

        readonly MapData map;
        SimWorld world;
        FlowFieldManager fields;
        MovementSystem movement;

        // ---- per slot, hashed ----
        public NativeArray<ushort> Gen;          // Generation this slot's state belongs to
        public NativeArray<float> SpeedFactor;   // VehicleModulesSystem: a damaged engine, a lone driver (1 = sound)
        public NativeArray<int> HaltTicks;       // TankGunnerySystem: stand still while a gun is laid
        public NativeArray<short> CrossTrench;   // trench it is crossing (-1 none): one ditching roll per trench
        public NativeArray<int> DitchTicks;      // > 0: nosed into a trench too wide for it
        public NativeArray<int> BogTicks;        // > 0: stuck in mud
        public int WireCrushed, TreesPushed, MenCrushed;
        ulong checksum = SimHash.Offset;

        NativeArray<float> trenchWidth;
        NativeList<SimEvent> events;
        NativeList<int2> blocked;                // (slot, nav cell) that stopped a vehicle this tick (transient)
        NativeList<int2> crushed;                // (victim, vehicle) this tick (transient)

        public VehicleKinematicsSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            this.world = world;
            fields = world.GetSystem<FlowFieldManager>() ?? throw new InvalidOperationException("VehicleKinematicsSystem needs FlowFieldManager registered before it");
            movement = world.GetSystem<MovementSystem>() ?? throw new InvalidOperationException("VehicleKinematicsSystem needs MovementSystem registered before it");
            int n = world.Config.MaxSlots;
            Gen = new NativeArray<ushort>(n, Allocator.Persistent);
            SpeedFactor = new NativeArray<float>(n, Allocator.Persistent);
            HaltTicks = new NativeArray<int>(n, Allocator.Persistent);
            CrossTrench = new NativeArray<short>(n, Allocator.Persistent);
            DitchTicks = new NativeArray<int>(n, Allocator.Persistent);
            BogTicks = new NativeArray<int>(n, Allocator.Persistent);
            for (int i = 0; i < n; i++) { SpeedFactor[i] = 1f; CrossTrench[i] = -1; }
            int trenches = map.Trenches.Length;
            trenchWidth = new NativeArray<float>(math.max(1, trenches), Allocator.Persistent);
            for (int t = 0; t < trenches; t++) trenchWidth[t] = map.Trenches[t].WidthMeters;
            events = new NativeList<SimEvent>(16, Allocator.Persistent);
            blocked = new NativeList<int2>(16, Allocator.Persistent);
            crushed = new NativeList<int2>(16, Allocator.Persistent);
        }

        public void Step(SimWorld w)
        {
            if (movement.Vehicles.Length == 0) return;
            events.Clear(); blocked.Clear();
            new VehicleJob
            {
                Vehicles = movement.Vehicles.AsArray(),
                Position = w.Position, Velocity = w.Velocity, Yaw = w.Yaw, Layer = w.Layer, StanceOf = w.StanceOf, Flags = w.Flags,
                Speed = w.Speed, Archetype = w.Archetype, GoalId = w.GoalId, Generation = w.Generation,
                Gen = Gen, SpeedFactor = SpeedFactor, HaltTicks = HaltTicks, CrossTrench = CrossTrench, DitchTicks = DitchTicks, BogTicks = BogTicks,
                Directions = fields.Direction, Ready = fields.Ready, TrenchCrossable = fields.TrenchCrossable, TrenchWidth = trenchWidth,
                Layers = map.NavLayers, CellTrenchId = map.CellTrenchId, Height = map.Height,
                NavWidth = map.NavWidth, NavLength = map.NavLength, CellCount = fields.CellCount, NavCell = MapData.NavCellSize,
                Size = map.SizeMeters, Dt = w.Config.TickSeconds, Seed = w.Config.Seed, Tick = w.Tick,
                Events = events, Blocked = blocked,
            }.Run();
            for (int e = 0; e < events.Length; e++) w.Events.Add(events[e]);
            CrushUnder(w);
        }

        /// <summary>What the tracks do to what is under and in front of them: wire, trees, men. Main thread (it changes
        /// the map), in vehicle slot order.</summary>
        void CrushUnder(SimWorld w)
        {
            bool navChanged = false;
            crushed.Clear();
            var list = movement.Vehicles;
            for (int k = 0; k < list.Length; k++)
            {
                int i = list[k];
                uint f = w.Flags[i];
                if ((f & (uint)UnitFlags.Alive) == 0 || (f & (uint)UnitFlags.KnockedOut) != 0) continue;
                var prof = VehicleProfile.ForArchetype(w.Archetype[i]);
                float3 p = w.Position[i], v = w.Velocity[i];
                float speed = SimMath.Length(v);
                if (speed > 0.2f && !prof.Walker)   // a walker steps over wire: it neither slows nor breaks it
                {
                    int opened = WireBelt.Breach(map, p, prof.HalfWidth);
                    if (opened > 0)
                    {
                        WireCrushed += opened; navChanged = true;
                        checksum = SimHash.Value(new int2(i, opened), checksum);
                        w.Events.Add(w.Tick, SimEventType.WireBreached, 0, 0, p, default, prof.HalfWidth * 2f);
                        w.Events.Add(w.Tick, SimEventType.VehicleCrushed, i, 0, p);
                    }
                }
                if (speed > CrushSpeed) FindMenUnder(w, i, p, prof);
                if (speed > 0.2f) navChanged |= FellUnder(w, i, p, prof);
            }
            // trees a tank ran into this tick go over (the cell opens: the next tick it drives on)
            for (int b = 0; b < blocked.Length; b++)
            {
                int i = blocked[b].x, cell = blocked[b].y;
                if ((w.Flags[i] & (uint)UnitFlags.KnockedOut) != 0) continue;
                bool heavy = VehicleProfile.ForArchetype(w.Archetype[i]).PushesTrees;
                for (int p = 0; p < map.Props.Length; p++)
                    if (map.Props[p].Cell == cell && Fells(map.Props[p].Kind, heavy)) { navChanged |= Fell(w, i, p); break; }
            }
            // men run down, in victim order so the result does not depend on the hash's buckets
            if (crushed.Length > 1) crushed.Sort(new ByVictim());
            for (int c = 0; c < crushed.Length; c++)
            {
                int j = crushed[c].x, i = crushed[c].y;
                if (!w.IsAlive(j) || (c > 0 && crushed[c - 1].x == j)) continue;
                w.Hp[j] = w.Hp[j] - CrushDamage;
                MenCrushed++;
                checksum = SimHash.Value(new int2(i, j), checksum);
                w.Events.Add(w.Tick, SimEventType.VehicleCrushed, i, 2, w.Position[j]);
                if (w.Hp[j] <= 0f) w.Despawn(j, i, SimMath.DirFromYaw(w.Yaw[i]));
            }
            if (navChanged) fields.MarkCostDirty(0);
        }

        static bool Fells(PropKind kind, bool heavy) => kind == PropKind.BrokenTree || (heavy && kind == PropKind.Tree);

        bool Fell(SimWorld w, int i, int p)
        {
            var prop = map.Props[p];
            bool nav = map.SetPropKind(p, PropKind.Log);
            TreesPushed++;
            checksum = SimHash.Value(new int2(i, p), checksum);
            w.Events.Add(w.Tick, SimEventType.PropChanged, p, (int)PropKind.Log, prop.Pos);
            w.Events.Add(w.Tick, SimEventType.VehicleCrushed, i, 1, prop.Pos);
            return nav;
        }

        /// <summary>Trees standing under the moving hull (its footprint and a hand's breadth more) go over.</summary>
        bool FellUnder(SimWorld w, int i, float3 p, VehicleProfile prof)
        {
            float yaw = w.Yaw[i];
            float2 fwd = new float2(SimMath.Sin(yaw), SimMath.Cos(yaw)), right = new float2(fwd.y, -fwd.x);
            float reach = prof.HalfLength + prof.HalfWidth;
            bool nav = false;
            for (int k = 0; k < map.Props.Length; k++)
            {
                var prop = map.Props[k];
                if (!Fells(prop.Kind, prof.PushesTrees)) continue;
                float2 d = prop.Pos.xz - p.xz;
                if (math.abs(d.x) > reach || math.abs(d.y) > reach) continue;
                if (math.abs(math.dot(d, fwd)) < prof.HalfLength + 0.3f && math.abs(math.dot(d, right)) < prof.HalfWidth + 0.3f) nav |= Fell(w, i, k);
            }
            return nav;
        }

        struct ByVictim : System.Collections.Generic.IComparer<int2>
        {
            public int Compare(int2 a, int2 b) => a.x != b.x ? a.x.CompareTo(b.x) : a.y.CompareTo(b.y);
        }

        void FindMenUnder(SimWorld w, int i, float3 p, VehicleProfile prof)
        {
            var hash = movement.Spatial;
            float yaw = w.Yaw[i];
            float2 fwd = new float2(SimMath.Sin(yaw), SimMath.Cos(yaw)), right = new float2(fwd.y, -fwd.x);
            float reach = prof.HalfLength + 1f;
            int x0 = math.max(0, (int)((p.x - reach) / hash.CellSize)), x1 = math.min(hash.Width - 1, (int)((p.x + reach) / hash.CellSize));
            int z0 = math.max(0, (int)((p.z - reach) / hash.CellSize)), z1 = math.min(hash.Length - 1, (int)((p.z + reach) / hash.CellSize));
            byte team = w.Team[i];
            for (int z = z0; z <= z1; z++)
            for (int x = x0; x <= x1; x++)
            {
                if (!hash.Map.TryGetFirstValue(hash.KeyXZ(x, z), out int j, out var it)) continue;
                do
                {
                    uint fj = w.Flags[j];
                    if ((fj & (uint)UnitFlags.Alive) == 0 || (fj & ((uint)UnitFlags.Vehicle | (uint)UnitFlags.InTrench)) != 0 || w.Team[j] == team) continue;
                    float2 d = w.Position[j].xz - p.xz;
                    float ahead = math.dot(d, fwd), side = math.dot(d, right);
                    if (ahead > 0f && ahead < prof.HalfLength + 0.4f && math.abs(side) < prof.HalfWidth) crushed.Add(new int2(j, i));
                } while (hash.Map.TryGetNextValue(out j, ref it));
            }
        }

        // Few vehicles, so a single-threaded job over the vehicle list: no parallel-write restrictions to work around.
        [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct VehicleJob : IJob
        {
            [ReadOnly] public NativeArray<int> Vehicles;
            public NativeArray<float3> Position, Velocity;
            public NativeArray<float> Yaw;
            public NativeArray<byte> Layer, StanceOf;
            public NativeArray<uint> Flags;
            [ReadOnly] public NativeArray<float> Speed;
            [ReadOnly] public NativeArray<byte> Archetype;
            [ReadOnly] public NativeArray<int> GoalId;
            [ReadOnly] public NativeArray<ushort> Generation;
            public NativeArray<ushort> Gen;
            public NativeArray<float> SpeedFactor;
            public NativeArray<int> HaltTicks, DitchTicks, BogTicks;
            public NativeArray<short> CrossTrench;
            [ReadOnly] public NativeArray<byte> Directions;
            [ReadOnly] public NativeArray<byte> Ready;
            [ReadOnly] public NativeArray<byte> TrenchCrossable;
            [ReadOnly] public NativeArray<float> TrenchWidth;
            [ReadOnly] public NativeArray<byte> Layers;
            [ReadOnly] public NativeArray<short> CellTrenchId;
            [ReadOnly] public Heightfield Height;
            public int NavWidth, NavLength, CellCount;
            public float NavCell, Dt;
            public uint Seed, Tick;
            public float2 Size;
            public NativeList<SimEvent> Events;
            public NativeList<int2> Blocked;

            int CellOf(float3 p)
            {
                int cx = math.clamp((int)(p.x / NavCell), 0, NavWidth - 1);
                int cz = math.clamp((int)(p.z / NavCell), 0, NavLength - 1);
                return cz * NavWidth + cx;
            }

            void Emit(SimEventType type, int slot, int b, float3 pos) => Events.Add(new SimEvent { Tick = Tick, Type = type, A = slot, B = b, Pos = pos });

            /// <summary>1 on the level, a crawl at the slope limit uphill, a little quicker downhill. Trench walls are
            /// bridged, not climbed, so a footprint end over a trench cell does not count.</summary>
            float SlopeFactor(float3 p, float3 heading, in VehicleProfile prof)
            {
                float reach = prof.HalfLength * 0.8f;
                float3 nose = p + heading * reach, tail = p - heading * reach;
                if (((Layers[CellOf(nose)] | Layers[CellOf(tail)]) & (byte)NavLayer.Trench) != 0) return 1f;
                float rise = (Height.Sample(nose.x, nose.z) - Height.Sample(tail.x, tail.z)) / (2f * reach);
                float half = prof.SlopeLimit * 0.5f;
                if (rise > 0f) return math.clamp(1f - math.max(0f, rise - half) / half, 0.2f, 1f);
                return math.min(1.15f, 1f - rise * 0.5f);
            }

            public void Execute()
            {
                for (int k = 0; k < Vehicles.Length; k++)
                {
                    int i = Vehicles[k];
                    if (Gen[i] != Generation[i])
                    {
                        Gen[i] = Generation[i]; SpeedFactor[i] = 1f; HaltTicks[i] = 0; CrossTrench[i] = -1; DitchTicks[i] = 0; BogTicks[i] = 0;
                    }
                    bool halted = HaltTicks[i] > 0;              // a gun is being laid: the time runs out whether or not it could drive
                    if (halted) HaltTicks[i]--;
                    uint f = Flags[i];
                    Layer[i] = (byte)NavLayer.Surface;          // a vehicle is never "in" a trench for cover purposes
                    StanceOf[i] = (byte)Stance.Standing;
                    if ((f & (uint)(UnitFlags.Immobilised | UnitFlags.Stalled | UnitFlags.KnockedOut)) != 0) { Velocity[i] = float3.zero; continue; }
                    float3 p = Position[i];
                    if (DitchTicks[i] > 0)
                    {
                        Velocity[i] = float3.zero;
                        if (--DitchTicks[i] == 0) Emit(SimEventType.VehicleDitched, i, -1, p);   // clawed its way out: it goes on across
                        continue;
                    }
                    if (BogTicks[i] > 0)
                    {
                        Velocity[i] = float3.zero;
                        if (--BogTicks[i] == 0) { Flags[i] = f & ~(uint)UnitFlags.Bogged; Emit(SimEventType.VehicleBogged, i, 0, p); }
                        continue;
                    }
                    if (halted) { Velocity[i] = float3.zero; continue; }

                    int cell = CellOf(p);
                    int goal = GoalId[i];
                    if (goal < 0 || Ready[goal] == 0) { Velocity[i] = float3.zero; continue; }
                    byte d = Directions[goal * CellCount + cell];
                    if (d == FlowField.NoDirection) { Velocity[i] = float3.zero; continue; }

                    // steer: turn toward the field direction at the profile's rate; pivot on the spot for a sharp turn
                    var prof = VehicleProfile.ForArchetype(Archetype[i]);
                    float2 want = FlowField.Offset(d);
                    float desiredYaw = SimMath.YawOf(new float3(want.x, 0f, want.y));
                    float yaw = Yaw[i];
                    float maxTurn = prof.TurnRateRad * Dt * math.max(0.6f, SpeedFactor[i]);
                    yaw = SimMath.WrapAngle(yaw + math.clamp(SimMath.WrapAngle(desiredYaw - yaw), -maxTurn, maxTurn));
                    float remaining = SimMath.WrapAngle(desiredYaw - yaw);
                    float align = math.abs(remaining) > PivotAngle ? PivotSpeed : math.max(0.3f, SimMath.Cos(remaining));
                    float3 heading = SimMath.DirFromYaw(yaw);
                    byte from = Layers[cell];
                    // A walker picks its way over what a tank has to drive through: it strides a trench instead of
                    // bellying across it, finds footing in mud and shell holes, and lifts its legs over wire.
                    float terrain = prof.Walker
                        ? ((from & (byte)NavLayer.Trench) != 0 ? StepOverSpeed : (from & (byte)NavLayer.Mud) != 0 ? WadeSpeed : (from & (byte)NavLayer.Crater) != 0 ? PickSpeed : 1f)
                        : ((from & (byte)NavLayer.Trench) != 0 ? CrossSpeed : (from & (byte)NavLayer.Mud) != 0 ? MudSpeed : (from & (byte)NavLayer.Crater) != 0 ? CraterSpeed : 1f);
                    if ((from & (byte)NavLayer.Wire) != 0 && !prof.Walker) terrain *= WireSpeed;
                    float3 v = heading * (Speed[i] * align * terrain * SlopeFactor(p, heading, prof) * SpeedFactor[i]);
                    float3 np = p + v * Dt;
                    np.x = math.clamp(np.x, 1f, Size.x - 1f);
                    np.z = math.clamp(np.z, 1f, Size.y - 1f);
                    int ncell = CellOf(np);
                    byte to = Layers[ncell];
                    if (!FlowField.CanStep(NavMode.Tracked, from, to, CellTrenchId[ncell], TrenchCrossable))
                    {
                        Blocked.Add(new int2(i, ncell));
                        np = p; v = float3.zero;
                    }
                    else if ((to & (byte)NavLayer.Trench) != 0)
                    {
                        short t = CellTrenchId[ncell];
                        if (t >= 0 && t != CrossTrench[i])
                        {
                            CrossTrench[i] = t;
                            float width = TrenchWidth[t];
                            if (width > prof.TrenchCrossWidth)
                            {
                                // too wide to bridge: it tries anyway, and may go nose first into it
                                var rng = SimRandom.For(Seed, Tick, SimRandom.SystemId.Bog, (uint)i);
                                float chance = prof.DitchChance * math.saturate((width - prof.TrenchCrossWidth) / math.max(0.1f, FlowFieldManager.TrackedCrossWidth - prof.TrenchCrossWidth));
                                if (rng.NextFloat() < chance)
                                {
                                    DitchTicks[i] = rng.NextInt(DitchMin, DitchMax + 1);
                                    Position[i] = np; Velocity[i] = float3.zero; Yaw[i] = yaw;
                                    Emit(SimEventType.VehicleDitched, i, t, np);
                                    continue;
                                }
                            }
                        }
                    }
                    else CrossTrench[i] = -1;
                    if ((to & (byte)NavLayer.Mud) != 0 && SimMath.Length(v) > 0.1f)
                    {
                        var rng = SimRandom.For(Seed, Tick, SimRandom.SystemId.Bog, (uint)i | 0x80000000u);
                        if (rng.NextFloat() < prof.BogChance * Dt * (prof.Wheeled ? 3f : 1f))
                        {
                            BogTicks[i] = rng.NextInt(BogMin, BogMax + 1);
                            Flags[i] = Flags[i] | (uint)UnitFlags.Bogged;
                            Emit(SimEventType.VehicleBogged, i, 1, np);
                        }
                    }
                    Position[i] = np;
                    Velocity[i] = v;
                    Yaw[i] = yaw;
                }
                // two hulls never overlap: push the pair apart along the line between them (slot order, so deterministic)
                for (int a = 0; a < Vehicles.Length; a++)
                for (int b = a + 1; b < Vehicles.Length; b++)
                {
                    int i = Vehicles[a], j = Vehicles[b];
                    float2 d = Position[j].xz - Position[i].xz;
                    float dist = SimMath.Length(d);
                    float min = VehicleProfile.ForArchetype(Archetype[i]).Radius + VehicleProfile.ForArchetype(Archetype[j]).Radius;
                    if (dist >= min) continue;
                    float2 dir = dist > 1e-3f ? d / dist : new float2(1f, 0f);
                    float push = math.min(0.15f, (min - dist) * 0.5f);
                    TryShift(i, -dir * push);
                    TryShift(j, dir * push);
                }
            }

            void TryShift(int i, float2 by)
            {
                // a tank not under its own power does not give way (a hulk, a stalled or broken-tracked one, one nosed
                // into a trench or stuck in mud: moving it would pull it out without its timer knowing)
                if ((Flags[i] & (uint)(UnitFlags.KnockedOut | UnitFlags.Stalled | UnitFlags.Immobilised | UnitFlags.Bogged)) != 0 || DitchTicks[i] > 0 || BogTicks[i] > 0) return;
                float3 p = Position[i], q = p + new float3(by.x, 0f, by.y);
                q.x = math.clamp(q.x, 1f, Size.x - 1f); q.z = math.clamp(q.z, 1f, Size.y - 1f);
                int from = CellOf(p), to = CellOf(q);
                if (to != from)
                {
                    // the tracked rules, and never into a trench it is not already crossing (that skips the ditching roll,
                    // and a wide one would leave it where its flow field has no direction)
                    if (!FlowField.CanStep(NavMode.Tracked, Layers[from], Layers[to], CellTrenchId[to], TrenchCrossable)) return;
                    if ((Layers[to] & (byte)NavLayer.Trench) != 0 && CellTrenchId[to] != CellTrenchId[from]) return;
                }
                Position[i] = q;
            }
        }

        public ulong Hash(ulong h)
        {
            int n = math.min(Gen.Length, world.HighWater);
            h = SimHash.Array(Gen, n, h);
            h = SimHash.Array(SpeedFactor, n, h);
            h = SimHash.Array(HaltTicks, n, h);
            h = SimHash.Array(CrossTrench, n, h);
            h = SimHash.Array(DitchTicks, n, h);
            h = SimHash.Array(BogTicks, n, h);
            h = SimHash.Value(new int3(WireCrushed, TreesPushed, MenCrushed), h);
            return SimHash.Combine(h, checksum);
        }

        public void Dispose()
        {
            if (Gen.IsCreated) Gen.Dispose();
            if (SpeedFactor.IsCreated) SpeedFactor.Dispose();
            if (HaltTicks.IsCreated) HaltTicks.Dispose();
            if (CrossTrench.IsCreated) CrossTrench.Dispose();
            if (DitchTicks.IsCreated) DitchTicks.Dispose();
            if (BogTicks.IsCreated) BogTicks.Dispose();
            if (trenchWidth.IsCreated) trenchWidth.Dispose();
            if (events.IsCreated) events.Dispose();
            if (blocked.IsCreated) blocked.Dispose();
            if (crushed.IsCreated) crushed.Dispose();
        }
    }
}
