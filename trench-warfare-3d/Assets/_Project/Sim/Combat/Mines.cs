// Phase: A5 / docs/21 SIM-D (implemented 2026-09-26: the mine half; the sapper that lays them waits for units-meta) —
// mines and tripwires on the ground.
// Depends on: BlastSystem (Queue: a mine's burst; Resolved: a shell that digs sets off the mines in its crater),
// MapData (where a mine may lie), VehicleProfile (how wide a hull is).
//
// A MINE lies at a point; a TRIPWIRE is a line up to MaxTripwireMetres, open ground along its whole length. Place()
// is a system call today (the sapper's UnitAbility will call it, docs/21 SIM-D): the mine arms in ArmTicks and then
// waits. An enemy man within TriggerRadius of a mine, or within TripwireReach of a tripwire's line, or an enemy hull
// whose rectangle (half length by half width, in its yaw) covers a mine, sets it off (slot order, the first wins): a
// MineTriggered event (dir = the victim's velocity direction, zero if he stood still) and an Impact of BlastShape.Mine that BlastSystem
// resolves next tick, which men take as any burst and a hull takes on the nearer track (VehicleModulesSystem). The
// layer's own side never triggers it; the burst spares nobody (friendly fire is on, as for every burst). A burst that
// digs a crater cooks off the mines within ClearReachFactor x its crater radius: MineCleared now, their own bursts a
// few ticks apart. Steps after VehicleKinematics (it reads the tick's final positions); its bursts resolve in the
// next tick's Blast.
//
// Mines is authoritative and hashed; a spent mine keeps its index (the picture holds markers by index) and Place
// reuses the first spent slot. Nothing here is random: the cook-off stagger is by order.
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Sim.Combat
{
    public enum MineKind : int { Mine = 0, Tripwire = 1 }
    public enum MineState : int { Arming = 0, Armed = 1, Spent = 2, Cooking = 3 }

    public struct Mine
    {
        public float3 Pos, Dir;      // Dir: unit XZ heading of a tripwire, zero for a mine
        public float Length;         // a tripwire's, metres; 0 for a mine
        public uint ArmTick;         // Arming: when it is armed; Cooking: when it goes off
        public int Player, Kind, State;
    }

    public sealed class MineSystem : ISimSystem
    {
        public int Order => SimSystemOrder.VehicleKinematics + 10;

        /// <summary>A placed mine is live after this many ticks (1 s at 20 Hz).</summary>
        public const int ArmTicks = 20;
        /// <summary>An enemy man this close to a mine sets it off.</summary>
        public const float TriggerRadius = 1.0f;
        /// <summary>An enemy man this close to a tripwire's line sets it off.</summary>
        public const float TripwireReach = 0.6f;
        public const float MaxTripwireMetres = 12f, MinTripwireMetres = 1f;
        public const float MineDamage = 260f, MineRadius = 4f, MineSuppression = 50f, MineCrater = 1.5f;
        /// <summary>The hole's depth as a share of its radius: a cook-off's (VehicleModules), a small, real bowl.</summary>
        public const float MineCraterDepthShare = 0.35f;
        public const float TripwireDamage = 90f, TripwireRadius = 4.5f, TripwireSuppression = 40f;
        /// <summary>A burst that digs a crater cooks off the mines within this many crater radii.</summary>
        public const float ClearReachFactor = 1.5f;
        /// <summary>Cooked mines go off CookMinTicks after the shell, the next ones CookStepTicks apart (up to five steps).</summary>
        public const int CookMinTicks = 4, CookStepTicks = 4, CookSteps = 5;
        public const int MaxMines = 256;
        /// <summary>Impact.Source of a mine's burst: SourceBase + the kind.</summary>
        public const int SourceBase = 70;

        readonly MapData map;
        BlastSystem blast;
        /// <summary>Every mine laid this match, spent ones included (their index is the picture's handle). Authoritative, hashed.</summary>
        public NativeList<Mine> Mines;
        NativeList<int> triggered;   // transient: (mine, victim) pairs the job found this tick

        /// <summary>Stats, not hashed.</summary>
        public int Placed { get; private set; }
        public int Triggered { get; private set; }
        public int Cleared { get; private set; }

        public MineSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            blast = world.GetSystem<BlastSystem>() ?? throw new System.InvalidOperationException("MineSystem needs BlastSystem registered before it");
            Mines = new NativeList<Mine>(64, Allocator.Persistent);
            triggered = new NativeList<int>(64, Allocator.Persistent);
        }

        /// <summary>How many mines are lying live or arming.</summary>
        public int Live
        {
            get { int n = 0; for (int m = 0; m < Mines.Length; m++) if (Mines[m].State == (int)MineState.Armed || Mines[m].State == (int)MineState.Arming) n++; return n; }
        }

        /// <summary>May a mine lie here: on the map, on open ground (not in a trench, a link, a bunker or anything blocked).</summary>
        public bool Lies(float3 p)
        {
            if (p.x < 0f || p.z < 0f || p.x >= map.NavWidth * MapData.NavCellSize || p.z >= map.NavLength * MapData.NavCellSize) return false;
            int cx = (int)(p.x / MapData.NavCellSize), cz = (int)(p.z / MapData.NavCellSize);
            byte layers = map.NavLayers[cz * map.NavWidth + cx];
            return (layers & (byte)(NavLayer.Trench | NavLayer.Link | NavLayer.Blocked | NavLayer.Bunker)) == 0;
        }

        /// <summary>Every nav cell a tripwire crosses must be open ground: it may not run across a trench (half a cell a step).</summary>
        public bool LiesAlong(float3 from, float3 dir, float length)
        {
            int steps = math.max(1, (int)math.ceil(length / (MapData.NavCellSize * 0.5f)));
            for (int s = 0; s <= steps; s++) if (!Lies(from + dir * (length * s / steps))) return false;
            return true;
        }

        /// <summary>Lay a mine at <paramref name="pos"/>, or a tripwire from it along <paramref name="dir"/> for
        /// <paramref name="length"/> metres (clamped to MinTripwireMetres..MaxTripwireMetres). The index, or -1 when it
        /// may not lie there or the field is full. Arms in ArmTicks. Emits MinePlaced (a = index, b = player, pos, dir =
        /// heading x length, scalar = kind).</summary>
        public int Place(SimWorld w, float3 pos, float3 dir, float length, int player, MineKind kind)
        {
            pos = w.ClampToMap(pos); pos.y = 0f;
            float3 d = new float3(dir.x, 0f, dir.z);
            if (kind == MineKind.Tripwire)
            {
                float dl = SimMath.Length(d);
                if (dl < 1e-3f) return -1;
                d /= dl; length = math.clamp(length, MinTripwireMetres, MaxTripwireMetres);
                if (!LiesAlong(pos, d, length)) return -1;
            }
            else { d = float3.zero; length = 0f; if (!Lies(pos)) return -1; }

            int index = -1;
            for (int m = 0; m < Mines.Length; m++) if (Mines[m].State == (int)MineState.Spent) { index = m; break; }
            var mine = new Mine { Pos = pos, Dir = d, Length = length, ArmTick = w.Tick + ArmTicks, Player = player, Kind = (int)kind, State = (int)MineState.Arming };
            if (index < 0)
            {
                if (Mines.Length >= MaxMines) return -1;
                Mines.Add(mine); index = Mines.Length - 1;
            }
            else Mines[index] = mine;
            Placed++;
            w.Events.Add(w.Tick, SimEventType.MinePlaced, index, player, pos, d * length, (float)kind);
            return index;
        }

        public void Step(SimWorld w)
        {
            if (Mines.Length == 0) return;

            // ---- what the tick's bursts dug up: every crater cooks off the mines in it ------------------------------
            var resolved = blast.Resolved;
            for (int k = 0; k < resolved.Length; k++)
            {
                var im = resolved[k];
                if (im.CraterRadius <= 0f) continue;
                float reach = im.CraterRadius * ClearReachFactor;
                int marked = 0;
                for (int m = 0; m < Mines.Length; m++)
                {
                    var mine = Mines[m];
                    if (mine.State != (int)MineState.Armed && mine.State != (int)MineState.Arming) continue;
                    float3 q = Nearest(mine, im.Pos) - im.Pos; q.y = 0f;
                    if (SimMath.Length(q) > reach) continue;
                    mine.State = (int)MineState.Cooking;
                    mine.ArmTick = w.Tick + (uint)(CookMinTicks + (marked % CookSteps) * CookStepTicks);
                    marked++;
                    Mines[m] = mine;
                    Cleared++;
                    w.Events.Add(w.Tick, SimEventType.MineCleared, m, im.Player, mine.Pos, mine.Dir * mine.Length, mine.Kind);
                }
            }

            // ---- arming, and the cooked ones going off ----------------------------------------------------------
            for (int m = 0; m < Mines.Length; m++)
            {
                var mine = Mines[m];
                if (mine.State == (int)MineState.Arming && mine.ArmTick <= w.Tick) { mine.State = (int)MineState.Armed; Mines[m] = mine; }
                else if (mine.State == (int)MineState.Cooking && mine.ArmTick <= w.Tick)
                {
                    mine.State = (int)MineState.Spent; Mines[m] = mine;
                    Burst(w, mine, mine.Pos, float3.zero);
                }
            }

            // ---- who stepped on what ---------------------------------------------------------------------------
            triggered.Clear();
            new TriggerJob
            {
                Count = w.HighWater, Mines = Mines.AsArray(),
                Position = w.Position, Flags = w.Flags, Hp = w.Hp, Team = w.Team, Archetype = w.Archetype, Yaw = w.Yaw,
                Triggered = triggered,
            }.Run();
            for (int k = 0; k < triggered.Length; k += 2)
            {
                int m = triggered[k], victim = triggered[k + 1];
                var mine = Mines[m];
                float3 at = mine.Kind == (int)MineKind.Tripwire ? Nearest(mine, w.Position[victim]) : mine.Pos;
                float3 vel = w.Velocity[victim]; vel.y = 0f;
                float vl = SimMath.Length(vel);
                float3 dir = vl > 1e-3f ? vel / vl : float3.zero;
                Triggered++;
                w.Events.Add(w.Tick, SimEventType.MineTriggered, m, victim, at, dir, mine.Kind);
                Burst(w, mine, at, dir);
            }
        }

        /// <summary>The nearest point of a mine (its position, or a point of a tripwire's line) to <paramref name="p"/>.</summary>
        public static float3 Nearest(in Mine mine, float3 p)
        {
            if (mine.Kind != (int)MineKind.Tripwire || mine.Length <= 0f) return mine.Pos;
            float3 ap = p - mine.Pos; ap.y = 0f;
            float t = math.clamp(math.dot(ap, mine.Dir), 0f, mine.Length);
            return mine.Pos + mine.Dir * t;
        }

        void Burst(SimWorld w, in Mine mine, float3 at, float3 dir)
        {
            bool trip = mine.Kind == (int)MineKind.Tripwire;
            blast.Queue(new Impact
            {
                Pos = at, Dir = dir,
                Damage = trip ? TripwireDamage : MineDamage, Radius = trip ? TripwireRadius : MineRadius,
                Suppression = trip ? TripwireSuppression : MineSuppression, CraterRadius = trip ? 0f : MineCrater, CraterDepth = trip ? 0f : MineCrater * MineCraterDepthShare,
                Source = SourceBase + mine.Kind, Player = mine.Player, Shape = (int)BlastShape.Mine,
            });
        }

        [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct TriggerJob : IJob
        {
            public int Count;
            public NativeArray<Mine> Mines;
            [ReadOnly] public NativeArray<float3> Position;
            [ReadOnly] public NativeArray<uint> Flags;
            [ReadOnly] public NativeArray<float> Hp, Yaw;
            [ReadOnly] public NativeArray<byte> Team, Archetype;
            public NativeList<int> Triggered;   // (mine, victim) pairs

            /// <summary>Past any hull's corner (VehicleProfile.Reach: a Pincer's is 6.4 m) and a tripwire's reach beyond it,
            /// so the box test throws a slot out only when no test after it could hit.</summary>
            const float WidestHull = 7f;

            public void Execute()
            {
                for (int m = 0; m < Mines.Length; m++)
                {
                    var mine = Mines[m];
                    if (mine.State != (int)MineState.Armed) continue;
                    bool trip = mine.Kind == (int)MineKind.Tripwire;
                    float3 end = mine.Pos + mine.Dir * mine.Length;
                    float minX = math.min(mine.Pos.x, end.x) - WidestHull, maxX = math.max(mine.Pos.x, end.x) + WidestHull;
                    float minZ = math.min(mine.Pos.z, end.z) - WidestHull, maxZ = math.max(mine.Pos.z, end.z) + WidestHull;
                    for (int i = 0; i < Count; i++)
                    {
                        uint f = Flags[i];
                        if ((f & (uint)UnitFlags.Alive) == 0 || Hp[i] <= 0f || Team[i] == mine.Player) continue;
                        float3 p = Position[i];
                        if (p.x < minX || p.x > maxX || p.z < minZ || p.z > maxZ) continue;   // the cheap box first: most men are nowhere near
                        bool vehicle = (f & (uint)UnitFlags.Vehicle) != 0;
                        if (vehicle && (f & (uint)UnitFlags.KnockedOut) != 0) continue;
                        bool hit;
                        if (vehicle && !trip)
                        {
                            // the hull's footprint in its own yaw, not a disc round its centre: a mine under the bow goes off
                            // before the hull has driven over it, one beside the hull never does (VehicleProfile.Covers, the
                            // same test VehicleModules asks when the burst reaches the hull)
                            hit = VehicleProfile.ForArchetype(Archetype[i]).Covers(Yaw[i], p, mine.Pos);
                        }
                        else if (trip)
                        {
                            float hull = vehicle ? VehicleProfile.ForArchetype(Archetype[i]).HalfWidth : 0f;
                            hit = ToSegment(p, mine.Pos, end) <= TripwireReach + hull;
                        }
                        else
                        {
                            float3 q = p - mine.Pos; q.y = 0f;
                            hit = math.abs(q.x) <= TriggerRadius && math.abs(q.z) <= TriggerRadius && SimMath.Length(q) <= TriggerRadius;
                        }
                        if (!hit) continue;
                        mine.State = (int)MineState.Spent; Mines[m] = mine;
                        Triggered.Add(m); Triggered.Add(i);
                        break;   // one mine, one victim: the first in slot order
                    }
                }
            }

            static float ToSegment(float3 p, float3 a, float3 b)
            {
                float3 ab = b - a, ap = p - a; ab.y = 0f; ap.y = 0f;
                float l2 = math.dot(ab, ab);
                float t = l2 > 1e-6f ? math.saturate(math.dot(ap, ab) / l2) : 0f;
                float3 q = a + ab * t - p; q.y = 0f;
                return SimMath.Length(q);
            }
        }

        public ulong Hash(ulong h)
        {
            for (int m = 0; m < Mines.Length; m++) h = SimHash.Value(Mines[m], h);
            return h;
        }

        public void Dispose()
        {
            if (Mines.IsCreated) Mines.Dispose();
            if (triggered.IsCreated) triggered.Dispose();
        }
    }
}
