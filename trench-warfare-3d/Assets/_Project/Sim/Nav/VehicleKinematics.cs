// Phase: A5b (implemented) — depends on: FlowFieldManager, MovementSystem (vehicle list, spatial hash), MapData, WireBelt,
// PropDef, SimRandom.SystemId.Bog. Damage (engine, tracks, crew) reaches here through the unit flags and SpeedFactor,
// which VehicleModulesSystem writes; TankGunnerySystem asks for a halt to lay a gun through HaltTicks.
// How a tank gets across the battlefield. Each follows the tracked-mode flow field of its goal:
//  - steering: it turns toward the field at its profile's rate; a turn sharper than PivotAngle is made at the
//    profile's PivotSpeed (a heavy tank on the spot, one track forward and one back; a walker or a skimmer keeps
//    going round), anything gentler is driven round while the heading closes. It steers at where the field's steps
//    lead a hull length on (Steer), so it drives the line a staircase of 45-degree steps stands for and begins a
//    turn before the corner, and slides along an edge that line would cut (the cell's own step);
//  - momentum (2026-09-28): speed is not set, it is driven toward the wanted speed at the profile's Accel and
//    shed at its Brake, so a machine gathers way and runs on to a stop (a halt to lay a gun, the end of a drive,
//    no field). Velocity carries it from tick to tick; being stuck (ditched, bogged, stalled) or a charge's strike
//    still stops it dead;
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
        /// <summary>Metres added to Radius (2026-09-28): room a machine keeps round it beyond its footprint, for one
        /// whose neighbours are drawn wider than theirs (the Maw's sponsons reach 5.8 m out on a 3.8 m half width).</summary>
        public float Clearance;
        /// <summary>How it gathers and sheds way (2026-09-28), m/s per second: a landship takes seconds to get going
        /// and a skimmer glides on to a stop. 0 takes DefaultAccel / DefaultBrake.</summary>
        public float Accel, Brake;
        /// <summary>The share of its speed it keeps through a turn sharper than PivotAngle: a heavy tank stops to
        /// pivot (0.04), a walker steps round (0.5). 0 takes DefaultPivot.</summary>
        public float PivotSpeed;
        public const float DefaultAccel = 1.2f, DefaultBrake = 2.4f, DefaultPivot = 0.06f;
        public float AccelOr => Accel > 0f ? Accel : DefaultAccel;
        public float BrakeOr => Brake > 0f ? Brake : DefaultBrake;
        public float PivotOr => PivotSpeed > 0f ? PivotSpeed : DefaultPivot;

        /// <summary>Is a world point under this hull's footprint: the rectangle HalfLength x HalfWidth in the hull's yaw
        /// (forward = (sin yaw, 0, cos yaw)), grown by <paramref name="margin"/> on every side. The one test for "under
        /// the hull": MineSystem's trigger asks it with no margin, a mine's burst on the hull (VehicleModulesSystem) with
        /// the burst's 0.6 m, and nothing else decides.</summary>
        public bool Covers(float yaw, float3 centre, float3 point, float margin = 0f)
        {
            float3 q = point - centre;
            float s = SimMath.Sin(yaw), c = SimMath.Cos(yaw);
            float lx = q.x * c - q.z * s, lz = q.x * s + q.z * c;
            return math.abs(lx) <= HalfWidth + margin && math.abs(lz) <= HalfLength + margin;
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
                case VehicleArchetype.Breaker: return Breaker;
                default: return Maw;
            }
        }

        /// <summary>The Breaker (2026-09-25): a squat assault tank, shorter than the Maw, that bridges a full-width
        /// trench and never ditches (it is built to go in and come out).</summary>
        public static VehicleProfile Breaker => new VehicleProfile
        { TurnRateRad = 0.6f, Accel = 2.2f, Brake = 3.0f, PivotSpeed = 0.08f, TrenchCrossWidth = FlowFieldManager.TrackedCrossWidth, DitchChance = 0f, SlopeLimit = 0.55f, BogChance = 0.04f, HalfLength = 2.2f, HalfWidth = 1.9f, PushesTrees = true };   // grows with the tanks when VehicleSize lands

        public static VehicleProfile Maw => new VehicleProfile
        { TurnRateRad = 0.42f, Accel = 0.5f, Brake = 1.2f, PivotSpeed = 0.04f, TrenchCrossWidth = FlowFieldManager.TrackedCrossWidth, DitchChance = 0f, SlopeLimit = 0.55f, BogChance = 0.05f, HalfLength = 2.55f * VehicleSize.Tank, HalfWidth = 2.25f * VehicleSize.Tank, PushesTrees = true };

        public static VehicleProfile Tusk => new VehicleProfile
        { TurnRateRad = 0.75f, Accel = 1.6f, Brake = 2.6f, PivotSpeed = 0.18f, TrenchCrossWidth = 2.4f, DitchChance = 0.75f, SlopeLimit = 0.6f, BogChance = 0.025f, HalfLength = 1.85f * VehicleSize.Tank, HalfWidth = 2.25f * VehicleSize.Tank };

        // The crabs, measured off Tools/crabsplit.py's crabs.json: Pincer 3.80 x 3.36 m, Kettle 3.20 x 2.65 m. A leg
        // finds its own footing, so mud barely holds them and a slope a tank would slide off is nothing; what stops a
        // walker is losing legs.
        public static VehicleProfile Pincer => new VehicleProfile
        { TurnRateRad = 1.15f, Accel = 2.2f, Brake = 3.5f, PivotSpeed = 0.5f, TrenchCrossWidth = 3.6f, DitchChance = 0f, SlopeLimit = 0.95f, BogChance = 0.008f, HalfLength = 1.70f * VehicleSize.Walker, HalfWidth = 1.90f * VehicleSize.Walker, PushesTrees = true, Walker = true, Legs = 6 };

        public static VehicleProfile Kettle => new VehicleProfile
        { TurnRateRad = 1.35f, Accel = 1.6f, Brake = 3.0f, PivotSpeed = 0.45f, TrenchCrossWidth = 3.0f, DitchChance = 0f, SlopeLimit = 0.90f, BogChance = 0.012f, HalfLength = 1.35f * VehicleSize.Walker, HalfWidth = 1.60f * VehicleSize.Walker, Walker = true, Legs = 4 };

        // Censer 3.30 x 2.95 m, Pavise 3.60 x 3.55 m. The gas crab is the quickest thing on the field on its feet;
        // the shielded one is the slowest, and plants itself to shoot.
        public static VehicleProfile Censer => new VehicleProfile
        { TurnRateRad = 1.45f, Accel = 2.4f, Brake = 3.5f, PivotSpeed = 0.55f, TrenchCrossWidth = 3.1f, DitchChance = 0f, SlopeLimit = 0.92f, BogChance = 0.010f, HalfLength = 1.50f * VehicleSize.Walker, HalfWidth = 1.65f * VehicleSize.Walker, Walker = true, Legs = 4 };

        public static VehicleProfile Pavise => new VehicleProfile
        { TurnRateRad = 0.95f, Accel = 0.9f, Brake = 3.2f, PivotSpeed = 0.3f, TrenchCrossWidth = 3.4f, DitchChance = 0f, SlopeLimit = 0.88f, BogChance = 0.014f, HalfLength = 1.78f * VehicleSize.Walker, HalfWidth = 1.80f * VehicleSize.Walker, PushesTrees = true, Walker = true, Legs = 4 };

        // Banner 2.03 x 3.40 m on four tall legs, Redoubt 2.97 x 3.60 m on six. The blockhouse is the heaviest thing
        // that walks and the slowest; the command walker is tall and narrow and steps over anything.
        public static VehicleProfile Banner => new VehicleProfile
        { TurnRateRad = 1.05f, Accel = 1.2f, Brake = 2.0f, PivotSpeed = 0.4f, TrenchCrossWidth = 3.5f, DitchChance = 0f, SlopeLimit = 0.94f, BogChance = 0.010f, HalfLength = 1.70f * VehicleSize.Walker, HalfWidth = 1.05f * VehicleSize.Walker, Walker = true, Legs = 4 };

        public static VehicleProfile Redoubt => new VehicleProfile
        { TurnRateRad = 0.80f, Accel = 0.6f, Brake = 1.8f, PivotSpeed = 0.3f, TrenchCrossWidth = 3.3f, DitchChance = 0f, SlopeLimit = 0.86f, BogChance = 0.018f, HalfLength = 1.80f * VehicleSize.Walker, HalfWidth = 1.50f * VehicleSize.Walker, PushesTrees = true, Walker = true, Legs = 6 };

        /// <summary>A round radius for keeping two hulls apart: the longer half plus a hand, so the corners of two
        /// hulls side by side or nose to flank do not pass through each other (the mean of length and width let the
        /// Maw's sponson into a Tusk's side).</summary>
        public float Radius => math.max(HalfLength, HalfWidth) + 0.25f + Clearance;
    }

    public sealed class VehicleKinematicsSystem : ISimSystem
    {
        public const float CrossSpeed = 0.45f, MudSpeed = 0.5f, CraterSpeed = 0.7f, WireSpeed = 0.8f, PivotAngle = 1.1f;
        /// <summary>The same three for a walker, which is slowed far less by all of them (and not at all by wire).</summary>
        public const float StepOverSpeed = 0.72f, WadeSpeed = 0.78f, PickSpeed = 0.88f;
        public const int DitchMin = 240, DitchMax = 600, BogMin = 80, BogMax = 260;
        public const float CrushDamage = 200f, CrushSpeed = 0.4f;
        public int Order => SimSystemOrder.VehicleKinematics;

        readonly MapData map;
        SimWorld world;
        FlowFieldManager fields;
        MovementSystem movement;

        // ---- per archetype, hashed: the machines of this match ----
        /// <summary>How each archetype drives: turn rate, trench crossing, slope, bog, footprint. A table rather than a
        /// switch compiled into the job, so the bake can fill it. Read here, by VehicleModules, by the gunnery and by
        /// the presentation picker.</summary>
        public NativeArray<VehicleProfile> Profiles;

        // ---- per slot, hashed ----
        public NativeArray<ushort> Gen;          // Generation this slot's state belongs to
        public NativeArray<float> SpeedFactor;   // VehicleModulesSystem: a damaged engine, a lone driver (1 = sound)
        public NativeArray<int> HaltTicks;       // TankGunnerySystem: stand still while a gun is laid
        public NativeArray<short> CrossTrench;   // trench it is crossing (-1 none): one ditching roll per trench
        public NativeArray<int> DitchTicks;      // > 0: nosed into a trench too wide for it
        public NativeArray<int> BogTicks;        // > 0: stuck in mud
        // ---- driven by a system rather than the flow field (2026-09-25: the Breaker's charge and withdrawal) ----
        /// <summary>DriveFlow: the goal's flow field, as ever. DriveStraight: turn toward DriveTarget and drive at it.
        /// DriveReverse: back toward DriveTarget along the nose axis without turning (the tracks run the other way).
        /// Either stops within DriveArrive of the point. HaltTicks, ditching and bogging still win.</summary>
        public NativeArray<byte> Drive;
        public NativeArray<float3> DriveTarget;
        public NativeArray<float> DriveSpeedMul;  // 1 normally; the Breaker charges at more and backs out at less
        public const byte DriveFlow = 0, DriveStraight = 1, DriveReverse = 2;
        public const float DriveArrive = 1.0f;
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
            // How each machine drives, as a table indexed by archetype rather than a switch compiled into the job.
            // Filled from the ForArchetype defaults today and from the bake later; hashed, because a machine that turns
            // faster on one player's copy is a different battle and the tick hash should say so at once.
            Profiles = new NativeArray<VehicleProfile>(Archetypes.Count, Allocator.Persistent);
            for (int a = 0; a < Archetypes.Count; a++) Profiles[a] = VehicleProfile.ForArchetype((byte)a);
            int n = world.Config.MaxSlots;
            Gen = new NativeArray<ushort>(n, Allocator.Persistent);
            SpeedFactor = new NativeArray<float>(n, Allocator.Persistent);
            HaltTicks = new NativeArray<int>(n, Allocator.Persistent);
            CrossTrench = new NativeArray<short>(n, Allocator.Persistent);
            DitchTicks = new NativeArray<int>(n, Allocator.Persistent);
            BogTicks = new NativeArray<int>(n, Allocator.Persistent);
            Drive = new NativeArray<byte>(n, Allocator.Persistent);
            DriveTarget = new NativeArray<float3>(n, Allocator.Persistent);
            DriveSpeedMul = new NativeArray<float>(n, Allocator.Persistent);
            for (int i = 0; i < n; i++) { SpeedFactor[i] = 1f; CrossTrench[i] = -1; DriveSpeedMul[i] = 1f; }
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
                Vehicles = movement.Vehicles.AsArray(), Profiles = Profiles,
                Position = w.Position, Velocity = w.Velocity, Yaw = w.Yaw, Layer = w.Layer, StanceOf = w.StanceOf, Flags = w.Flags,
                Speed = w.Speed, Archetype = w.Archetype, GoalId = w.GoalId, Generation = w.Generation,
                Gen = Gen, SpeedFactor = SpeedFactor, HaltTicks = HaltTicks, CrossTrench = CrossTrench, DitchTicks = DitchTicks, BogTicks = BogTicks,
                Drive = Drive, DriveTarget = DriveTarget, DriveSpeedMul = DriveSpeedMul,
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
                var prof = Profiles[w.Archetype[i]];
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
                bool heavy = Profiles[w.Archetype[i]].PushesTrees;
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
            [ReadOnly] public NativeArray<VehicleProfile> Profiles;   // the match table, by archetype
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
            public NativeArray<byte> Drive;
            public NativeArray<float> DriveSpeedMul;
            [ReadOnly] public NativeArray<float3> DriveTarget;
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

            float3 Clamp(float3 q)
            {
                q.x = math.clamp(q.x, 1f, Size.x - 1f);
                q.z = math.clamp(q.z, 1f, Size.y - 1f);
                return q;
            }

            /// <summary>Where to steer on the field: follow its steps from this cell for about a hull length (3 to 6
            /// cells) and aim from this cell's centre at where they lead. A straight path stays exactly straight; one
            /// the 8-way field draws as a staircase (north, north, north-east, ...) is driven as the line it stands
            /// for; a turn the path is about to make is begun before the corner. (Blending the neighbouring cells'
            /// directions instead, tried first, put a hull on a cell edge 20 degrees off a straight course: a
            /// neighbour's field breaks its tie to the goal with a diagonal.) Falls back to the cell's own step where
            /// the walk goes nowhere.</summary>
            float2 Steer(int goal, int cell, in VehicleProfile prof, float2 own)
            {
                int steps = math.clamp((int)math.round(2f * prof.HalfLength / NavCell), 3, 6);
                int cx = cell % NavWidth, cz = cell / NavWidth, x = cx, z = cz;
                for (int k = 0; k < steps; k++)
                {
                    byte d = Directions[goal * CellCount + z * NavWidth + x];
                    if (d == FlowField.NoDirection) break;
                    float2 o = FlowField.Offset(d);
                    int nx = x + (o.x > 0.1f ? 1 : o.x < -0.1f ? -1 : 0), nz = z + (o.y > 0.1f ? 1 : o.y < -0.1f ? -1 : 0);
                    if (nx < 0 || nz < 0 || nx >= NavWidth || nz >= NavLength) break;
                    x = nx; z = nz;
                }
                float2 to = new float2(x - cx, z - cz);
                float len = SimMath.Length(to);
                return len < 0.5f ? own : to / len;
            }

            public void Execute()
            {
                for (int k = 0; k < Vehicles.Length; k++)
                {
                    int i = Vehicles[k];
                    if (Gen[i] != Generation[i])
                    {
                        Gen[i] = Generation[i]; SpeedFactor[i] = 1f; HaltTicks[i] = 0; CrossTrench[i] = -1; DitchTicks[i] = 0; BogTicks[i] = 0;
                        Drive[i] = DriveFlow; DriveSpeedMul[i] = 1f;
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
                    int cell = CellOf(p);
                    var prof = Profiles[Archetype[i]];
                    byte drive = Drive[i];
                    float yaw = Yaw[i];
                    // the way it has on it: Velocity along the nose, negative when it is backing
                    float3 was = Velocity[i];
                    float cur = SimMath.Length(was);
                    if (math.dot(was, SimMath.DirFromYaw(yaw)) < -0.5f * cur) cur = -cur;   // (a slide along a wall is not backing)
                    // stopping: a gun is being laid, no field, or the drive's point is reached. It runs on to a stop.
                    // a charge ends in the trench it hit: a halted Breaker still charging stops dead (the strike)
                    if (halted && (f & (uint)UnitFlags.Charging) != 0) { Velocity[i] = float3.zero; continue; }
                    bool stopping = halted;
                    float2 want = float2.zero, step = float2.zero;
                    float ramp = float.MaxValue;              // the speed it can still shed before a drive's point
                    if (!stopping && drive == DriveFlow)
                    {
                        int goal = GoalId[i];
                        byte d = goal < 0 || Ready[goal] == 0 ? FlowField.NoDirection : Directions[goal * CellCount + cell];
                        if (d == FlowField.NoDirection) stopping = true;
                        else
                        {
                            step = FlowField.Offset(d);
                            want = Steer(goal, cell, prof, step);
                        }
                    }
                    else if (!stopping)
                    {
                        // driven at a point by a system (the Breaker's charge and withdrawal): a straight line, no field
                        float3 toward = DriveTarget[i] - p; toward.y = 0f;
                        float len = SimMath.Length(toward);
                        if (len < DriveArrive) stopping = true;
                        else
                        {
                            want = new float2(toward.x, toward.z) / len;
                            // ease in to arrive, unless it is charging: a charge slams in and its strike stops it dead
                            if ((f & (uint)UnitFlags.Charging) == 0) ramp = SimMath.Sqrt(2f * prof.BrakeOr * (len - DriveArrive * 0.5f));
                        }
                    }
                    if (stopping && math.abs(cur) < 0.02f) { Velocity[i] = float3.zero; continue; }   // standing, and staying so
                    float align = 1f;
                    if (!stopping && drive != DriveReverse)
                    {
                        // steer: turn toward the wanted direction at the profile's rate; a sharp turn is made at its pivot share
                        float desiredYaw = SimMath.YawOf(new float3(want.x, 0f, want.y));
                        float maxTurn = prof.TurnRateRad * Dt * math.max(0.6f, SpeedFactor[i]);
                        yaw = SimMath.WrapAngle(yaw + math.clamp(SimMath.WrapAngle(desiredYaw - yaw), -maxTurn, maxTurn));
                        float remaining = SimMath.WrapAngle(desiredYaw - yaw);
                        align = math.abs(remaining) > PivotAngle ? prof.PivotOr : math.max(0.3f, SimMath.Cos(remaining));
                    }
                    float3 heading = SimMath.DirFromYaw(yaw);
                    // backing up: the tracks run the other way, the nose stays where it points (and one running on to a
                    // stop keeps the way it had)
                    float sense = stopping ? (cur < 0f ? -1f : 1f) : drive == DriveReverse ? -1f : 1f;
                    byte from = Layers[cell];
                    // A walker picks its way over what a tank has to drive through: it strides a trench instead of
                    // bellying across it, finds footing in mud and shell holes, and lifts its legs over wire.
                    float terrain = prof.Walker
                        ? ((from & (byte)NavLayer.Trench) != 0 ? StepOverSpeed : (from & (byte)NavLayer.Mud) != 0 ? WadeSpeed : (from & (byte)NavLayer.Crater) != 0 ? PickSpeed : 1f)
                        : ((from & (byte)NavLayer.Trench) != 0 ? CrossSpeed : (from & (byte)NavLayer.Mud) != 0 ? MudSpeed : (from & (byte)NavLayer.Crater) != 0 ? CraterSpeed : 1f);
                    if ((from & (byte)NavLayer.Wire) != 0 && !prof.Walker) terrain *= WireSpeed;
                    float top = stopping ? 0f : math.min(ramp, Speed[i] * align * terrain * SlopeFactor(p, heading * sense, prof) * SpeedFactor[i] * DriveSpeedMul[i]);
                    // gather way at Accel (less with a hurt engine, less in what slows it), shed it at Brake
                    float target = sense * top;
                    bool gaining = math.abs(target) > math.abs(cur) && target * cur >= 0f;
                    // (driven past its speed, the Breaker's charge, it lunges: it gathers way that much faster too)
                    float rate = gaining ? prof.AccelOr * math.max(0.4f, SpeedFactor[i]) * math.max(0.5f, terrain) * math.max(1f, DriveSpeedMul[i]) : prof.BrakeOr;
                    float s = cur + math.clamp(target - cur, -rate * Dt, rate * Dt);
                    float3 v = heading * s;
                    float3 np = Clamp(p + v * Dt);
                    int ncell = CellOf(np);
                    byte to = Layers[ncell];
                    if (!FlowField.CanStep(NavMode.Tracked, from, to, CellTrenchId[ncell], TrenchCrossable))
                    {
                        Blocked.Add(new int2(i, ncell));
                        // the steered line cut an edge the field goes round: slide along the cell's own step instead,
                        // at the way it has (a hull scrapes along a wall, it does not stop dead against it)
                        float3 slide = Clamp(p + new float3(step.x, 0f, step.y) * (math.abs(s) * Dt));
                        int scell = CellOf(slide);
                        if (!stopping && drive == DriveFlow && math.any(step != 0f) && scell != ncell
                            && FlowField.CanStep(NavMode.Tracked, from, Layers[scell], CellTrenchId[scell], TrenchCrossable)
                            && (Layers[scell] & (byte)NavLayer.Trench) == 0)
                        {
                            np = slide; ncell = scell; to = Layers[scell]; v = new float3(step.x, 0f, step.y) * math.abs(s);
                        }
                        else { np = p; v = float3.zero; }
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
                    float min = Profiles[Archetype[i]].Radius + Profiles[Archetype[j]].Radius;
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
                if ((Flags[i] & (uint)(UnitFlags.KnockedOut | UnitFlags.Stalled | UnitFlags.Immobilised | UnitFlags.Bogged | UnitFlags.Charging)) != 0 || DitchTicks[i] > 0 || BogTicks[i] > 0) return;   // a charging Breaker is not shoved off its line
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
            h = SimHash.Array(Profiles, h);   // the machines of this match: a different table is a different battle
            int n = math.min(Gen.Length, world.HighWater);
            h = SimHash.Array(Gen, n, h);
            h = SimHash.Array(SpeedFactor, n, h);
            h = SimHash.Array(HaltTicks, n, h);
            h = SimHash.Array(CrossTrench, n, h);
            h = SimHash.Array(DitchTicks, n, h);
            h = SimHash.Array(BogTicks, n, h);
            h = SimHash.Value(new int3(WireCrushed, TreesPushed, MenCrushed), h);
            h = SimHash.Array(Drive, n, h);
            h = SimHash.Array(DriveTarget, n, h);
            h = SimHash.Array(DriveSpeedMul, n, h);
            return SimHash.Combine(h, checksum);
        }

        public void Dispose()
        {
            if (Profiles.IsCreated) Profiles.Dispose();
            if (Gen.IsCreated) Gen.Dispose();
            if (SpeedFactor.IsCreated) SpeedFactor.Dispose();
            if (HaltTicks.IsCreated) HaltTicks.Dispose();
            if (CrossTrench.IsCreated) CrossTrench.Dispose();
            if (DitchTicks.IsCreated) DitchTicks.Dispose();
            if (BogTicks.IsCreated) BogTicks.Dispose();
            if (Drive.IsCreated) Drive.Dispose();
            if (DriveTarget.IsCreated) DriveTarget.Dispose();
            if (DriveSpeedMul.IsCreated) DriveSpeedMul.Dispose();
            if (trenchWidth.IsCreated) trenchWidth.Dispose();
            if (events.IsCreated) events.Dispose();
            if (blocked.IsCreated) blocked.Dispose();
            if (crushed.IsCreated) crushed.Dispose();
        }
    }
}
