// Phase: A1 (implemented) — depends on: FlowField, MapData, ObjectiveDef, TrenchDef
// Owns one flow field per goal. A goal is (kind, ref, nav mode): the cells of a trench, of an objective, a player's
// rally point, or a single cell. Goals are created lazily by the systems that hand them to units (TrenchOrders,
// Movement) and live for the match. Fields are rebuilt only when their goal cells or the map cost change, at most
// MaxRebuildsPerTick per tick in goal-id order, so a burst of dirty fields never stalls a tick; a unit whose field
// is not ready yet simply waits one tick. The manager also owns the per-trench match state (owner, locked,
// hold-fire, garrison count) that goal selection needs. The goal table and trench state are authoritative and
// hashed; the field arrays are a deterministic cache of (map version, goal cells, rebuild schedule) and are not.
using System;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Terrain;

namespace TW.Sim.Nav
{
    public enum GoalKind : byte { Trench = 0, Objective = 1, Rally = 2, Cell = 3 }

    public struct GoalKey : IEquatable<GoalKey>
    {
        public GoalKind Kind;
        public NavMode Mode;
        public int Ref;   // trench id, objective id, player index or nav cell index depending on Kind

        public static GoalKey Trench(short trenchId, NavMode mode = NavMode.Infantry) => new GoalKey { Kind = GoalKind.Trench, Mode = mode, Ref = trenchId };
        public static GoalKey Objective(short objectiveId, NavMode mode = NavMode.Infantry) => new GoalKey { Kind = GoalKind.Objective, Mode = mode, Ref = objectiveId };
        public static GoalKey Rally(int player, NavMode mode = NavMode.Infantry) => new GoalKey { Kind = GoalKind.Rally, Mode = mode, Ref = player };
        public static GoalKey Cell(int navCell, NavMode mode = NavMode.Infantry) => new GoalKey { Kind = GoalKind.Cell, Mode = mode, Ref = navCell };

        public bool Equals(GoalKey o) => Kind == o.Kind && Mode == o.Mode && Ref == o.Ref;
        public override string ToString() => $"{Kind}({Ref}) {Mode}";
    }

    /// <summary>Per-trench match state. OwnerTeam flips in A3 (sector control); Locked / HoldFire come from trench commands.</summary>
    public struct TrenchState
    {
        public byte OwnerTeam;
        public byte Locked;        // arrivals pass through to the next goal instead of garrisoning
        public byte HoldFire;      // garrison stays below the rim (A2 reads it)
        public int GarrisonCount;  // refreshed each tick by TrenchOrdersSystem
    }

    public sealed class FlowFieldManager : ISimSystem
    {
        public const int MaxGoals = 16;
        public const int MaxRebuildsPerTick = 2;
        public const float TrackedCrossWidth = 3.5f;   // Mark IV / A7V class; wheeled vehicles never cross (A5b)

        public int Order => SimSystemOrder.FlowField;

        readonly MapData map;
        public int CellCount { get; private set; }
        public int GoalCount { get; private set; }

        // ---- goal table (authoritative, hashed) ----
        public NativeArray<GoalKey> Goals;
        public NativeArray<byte> Dirty;        // 1 = needs a rebuild
        public NativeArray<byte> Ready;        // 1 = built at least once; units ignore fields that are not ready
        public NativeArray<int> RallyCell;     // Rally goals: the cell the field was built for, to detect SetRally
        public NativeArray<TrenchState> Trenches;
        public NativeArray<byte> TrenchCrossable;   // per TrenchDef: 1 if a tracked vehicle may cross it

        // ---- field storage (cache): goal g occupies [g * CellCount, (g + 1) * CellCount) ----
        public NativeArray<int> Integration;
        public NativeArray<byte> Direction;

        public FlowFieldManager(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            CellCount = map.NavWidth * map.NavLength;
            Goals = new NativeArray<GoalKey>(MaxGoals, Allocator.Persistent);
            Dirty = new NativeArray<byte>(MaxGoals, Allocator.Persistent);
            Ready = new NativeArray<byte>(MaxGoals, Allocator.Persistent);
            RallyCell = new NativeArray<int>(MaxGoals, Allocator.Persistent);
            for (int g = 0; g < MaxGoals; g++) RallyCell[g] = -1;
            Integration = new NativeArray<int>(MaxGoals * CellCount, Allocator.Persistent);
            Direction = new NativeArray<byte>(MaxGoals * CellCount, Allocator.Persistent);
            for (int i = 0; i < Direction.Length; i++) Direction[i] = FlowField.NoDirection;

            int trenchCount = map.Trenches.Length;
            Trenches = new NativeArray<TrenchState>(trenchCount, Allocator.Persistent);
            TrenchCrossable = new NativeArray<byte>(math.max(1, trenchCount), Allocator.Persistent);
            for (int t = 0; t < trenchCount; t++)
            {
                var def = map.Trenches[t];
                Trenches[t] = new TrenchState { OwnerTeam = def.OwnerTeam };
                TrenchCrossable[t] = (byte)(def.WidthMeters > 0f && def.WidthMeters <= TrackedCrossWidth ? 1 : 0);
            }
        }

        // ------------------------------------------------------------------ goals
        /// <summary>Id of the goal for <paramref name="key"/>, creating (dirty, not ready) it on first request. Main thread only.</summary>
        public int GetGoal(GoalKey key)
        {
            for (int g = 0; g < GoalCount; g++) if (Goals[g].Equals(key)) return g;
            if (GoalCount >= MaxGoals) throw new InvalidOperationException($"FlowFieldManager: more than {MaxGoals} goals requested ({key})");
            int id = GoalCount++;
            Goals[id] = key; Dirty[id] = 1; Ready[id] = 0; RallyCell[id] = -1;
            return id;
        }

        public bool IsReady(int goalId) => goalId >= 0 && goalId < GoalCount && Ready[goalId] != 0;

        /// <summary>View over one goal's field (no ownership).</summary>
        public FlowField Field(int goalId)
            => new FlowField(map.NavWidth, map.NavLength, Integration.GetSubArray(goalId * CellCount, CellCount), Direction.GetSubArray(goalId * CellCount, CellCount));

        /// <summary>The map's cost field changed at <paramref name="navCell"/> (deformation, wire breach): every field is stale.</summary>
        public void MarkCostDirty(int navCell)
        {
            // A4 may narrow this to fields whose reachable set contains the cell; a full mark is correct and cheap at ≤ 2 rebuilds/tick.
            for (int g = 0; g < GoalCount; g++) Dirty[g] = 1;
        }

        // ------------------------------------------------------------------ goal selection helpers
        /// <summary>The fire trench this team owns that lies closest to the enemy (team 0 attacks toward +Z), or -1.</summary>
        public short FrontTrench(byte team)
        {
            short best = -1;
            float bestZ = team == 0 ? float.MinValue : float.MaxValue;
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                if (Trenches[t].OwnerTeam != team) continue;
                var def = map.Trenches[t];
                if (def.Kind != 0 || def.CellCount == 0) continue;
                float z = map.NavCellCenter(map.TrenchCells[def.CellStart]).z;
                if (team == 0 ? z > bestZ : z < bestZ) { bestZ = z; best = (short)t; }
            }
            return best;
        }

        /// <summary>Objective id of the enemy HQ for <paramref name="team"/>, or -1.</summary>
        public short EnemyHq(byte team)
        {
            for (int i = 0; i < map.Objectives.Length; i++)
            {
                var o = map.Objectives[i];
                if (o.Kind == ObjectiveKind.HQ && o.SideTeam != team) return o.Id;
            }
            return -1;
        }

        /// <summary>The goal a ">>" from <paramref name="trenchId"/> sends <paramref name="team"/> to: the next trench in the chain, or the enemy HQ when the chain ends.</summary>
        public int NextGoalFrom(short trenchId, byte team, NavMode mode = NavMode.Infantry)
        {
            var def = map.Trenches[trenchId];
            short next = team == 0 ? def.NextTrenchForTeam0 : def.NextTrenchForTeam1;
            if (next >= 0 && next < map.Trenches.Length) return GetGoal(GoalKey.Trench(next, mode));
            short hq = EnemyHq(team);
            return hq >= 0 ? GetGoal(GoalKey.Objective(hq, mode)) : -1;
        }

        /// <summary>Goal for a freshly deployed unit: infantry walk to their team's front trench, vehicles drive at the enemy HQ.</summary>
        public int DefaultGoal(byte team, bool vehicle)
        {
            if (vehicle)
            {
                short hq = EnemyHq(team);
                return hq >= 0 ? GetGoal(GoalKey.Objective(hq, NavMode.Tracked)) : -1;
            }
            short front = FrontTrench(team);
            if (front >= 0) return GetGoal(GoalKey.Trench(front));
            short hq2 = EnemyHq(team);
            return hq2 >= 0 ? GetGoal(GoalKey.Objective(hq2)) : -1;
        }

        // ------------------------------------------------------------------ tick
        public void Step(SimWorld world)
        {
            // Rally goals follow the player's rally point.
            for (int g = 0; g < GoalCount; g++)
            {
                if (Goals[g].Kind != GoalKind.Rally) continue;
                var c = map.NavCellOf(world.Rally[Goals[g].Ref]);
                int cell = map.NavIndex(c.x, c.y);
                if (cell != RallyCell[g]) { RallyCell[g] = cell; Dirty[g] = 1; }
            }
            int rebuilt = 0;
            for (int g = 0; g < GoalCount && rebuilt < MaxRebuildsPerTick; g++)
            {
                if (Dirty[g] == 0) continue;
                Rebuild(g);
                Dirty[g] = 0; Ready[g] = 1;
                rebuilt++;
            }
        }

        void Rebuild(int g)
        {
            var key = Goals[g];
            var goals = new NativeList<int>(256, Allocator.Temp);
            switch (key.Kind)
            {
                case GoalKind.Trench:
                    if (key.Ref >= 0 && key.Ref < map.Trenches.Length)
                    {
                        // Link cells are excluded so arrivals step off the ladder into the trench body before they garrison;
                        // otherwise the first units in would sit on the entrance and block everyone behind them.
                        var def = map.Trenches[key.Ref];
                        for (int c = 0; c < def.CellCount; c++)
                        {
                            int cell = map.TrenchCells[def.CellStart + c];
                            if ((map.NavLayers[cell] & (byte)NavLayer.Link) == 0) goals.Add(cell);
                        }
                        if (goals.Length == 0) for (int c = 0; c < def.CellCount; c++) goals.Add(map.TrenchCells[def.CellStart + c]);
                    }
                    break;
                case GoalKind.Objective:
                    for (int i = 0; i < map.Objectives.Length; i++)
                    {
                        var o = map.Objectives[i];
                        if (o.Id != key.Ref) continue;
                        for (int c = 0; c < o.CellCount; c++) goals.Add(map.ObjectiveCells[o.CellStart + c]);
                    }
                    break;
                case GoalKind.Rally:
                    if (RallyCell[g] >= 0) goals.Add(RallyCell[g]);
                    break;
                case GoalKind.Cell:
                    goals.Add(key.Ref);
                    break;
            }
            Field(g).Build(map, goals.AsArray(), key.Mode, TrenchCrossable);
            goals.Dispose();
        }

        public ulong Hash(ulong h)
        {
            h = SimHash.Value(GoalCount, h);
            h = SimHash.Array(Goals, GoalCount, h);
            h = SimHash.Array(Dirty, GoalCount, h);
            h = SimHash.Array(Ready, GoalCount, h);
            h = SimHash.Array(RallyCell, GoalCount, h);
            h = SimHash.Array(Trenches, h);
            h = SimHash.Value(map.Version, h);   // the fields derive from the map; hashing its version covers them
            return h;
        }

        public void Dispose()
        {
            if (Goals.IsCreated) Goals.Dispose();
            if (Dirty.IsCreated) Dirty.Dispose();
            if (Ready.IsCreated) Ready.Dispose();
            if (RallyCell.IsCreated) RallyCell.Dispose();
            if (Integration.IsCreated) Integration.Dispose();
            if (Direction.IsCreated) Direction.Dispose();
            if (Trenches.IsCreated) Trenches.Dispose();
            if (TrenchCrossable.IsCreated) TrenchCrossable.Dispose();
        }
    }
}
