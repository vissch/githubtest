// Phase: A5 (implemented 2026-09-26, docs/21 phase 4) — fire on men and on the ground.
// Depends on: BlastSystem (Resolved: an Incendiary burst is what lights things), MapData (nav cells), FlowFieldManager
// (where a burning garrison runs to).
//
// Two things burn. A MAN alight (UnitFlags.Burning, AlightUntil[slot] > tick) loses BurnDps a second, is suppressed,
// and if he was standing in a trench he leaves it and runs for his own HQ the way a gassed man does: the picture's
// "running in flames" is a man who is really running. When the fire goes out (or he dies) the flag clears; a man
// who burns to death dies of DeathCause.Burning. GROUND alight (a BurningCell, a nav cell with a tick it stops)
// sets any man who stands on it alight. Ignition comes from an Incendiary Impact resolved by BlastSystem this tick
// (every man inside its radius, every nav cell inside it), from the beam (docs/21 phase 5) and from Ignite() /
// IgniteCell() called by another sim system; presentation never calls into here.
//
// The flag and the timer are set together and only together (Ignite, the catch in the job). A dead man's timer goes
// out the tick the job sees him dead, with one UnitAlight (b = 0) whatever killed him. A man who dies AFTER this
// system stepped (gas at 900, a track at 1120) and whose slot Spawn hands on before it steps again arrives with a
// timer and no flag (Spawn clears the flags): that timer is a dead man's and goes out silently, so a recruit never
// inherits the fire.
//
// Nothing here is random. Both arrays are authoritative and hashed; the per-cell lookup the job uses is rebuilt from
// Cells every tick and is not.
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Sim.Combat
{
    public struct BurningCell
    {
        public int Cell;      // nav cell index
        public uint Until;    // the tick it stops burning
        public int Player;    // who lit it (-1 nobody)
    }

    public sealed class BurningSystem : ISimSystem
    {
        public int Order => SimSystemOrder.Blast + 5;

        /// <summary>What a man alight loses per second. Balance: a rifleman (100 hp) burning the full MaxBurnSeconds dies.</summary>
        public const float BurnDps = 12f;
        /// <summary>The longest one ignition burns a man; re-ignition extends to at most this from now.</summary>
        public const float MaxBurnSeconds = 10f;
        /// <summary>How long an incendiary burst sets a man alight.</summary>
        public const float BurstManSeconds = 7f;
        /// <summary>How long an incendiary burst keeps the ground alight (docs/07: burning cells 5 s).</summary>
        public const float BurstCellSeconds = 5f;
        /// <summary>How long a man who walks onto burning ground burns.</summary>
        public const float GroundCatchSeconds = 4f;
        public const float SuppressionPerSecond = 40f;
        /// <summary>Cells lit by one burst, at most: a bound on the per-burst cost and on the Cells list.</summary>
        public const int MaxCellsPerBurst = 96;

        readonly MapData map;
        BlastSystem blast;
        FlowFieldManager fields;
        /// <summary>Per slot: the tick the fire goes out; 0 = not alight. Authoritative, hashed to HighWater.</summary>
        public NativeArray<uint> AlightUntil;
        /// <summary>Ground alight. Authoritative, hashed.</summary>
        public NativeList<BurningCell> Cells;
        NativeArray<byte> cellFire;          // transient: 1 where a cell burns this tick, rebuilt from Cells
        NativeList<int> killed, caught, expired, fled;
        int highWater;                       // what the last Step saw, so Hash() knows how far AlightUntil is live

        public BurningSystem(MapData map) { this.map = map; }

        /// <summary>True while anything burns: the presentation may skip its fire pass otherwise.</summary>
        public bool Active { get; private set; }
        /// <summary>Men set alight over the match, for the tests and the debrief.</summary>
        public int MenIgnited { get; private set; }

        public void Initialize(SimWorld world)
        {
            blast = world.GetSystem<BlastSystem>() ?? throw new System.InvalidOperationException("BurningSystem needs BlastSystem registered before it");
            fields = world.GetSystem<FlowFieldManager>();   // null in a bare world: burning garrisons then simply stay put
            AlightUntil = new NativeArray<uint>(world.Config.MaxSlots, Allocator.Persistent);
            Cells = new NativeList<BurningCell>(64, Allocator.Persistent);
            cellFire = new NativeArray<byte>(map.NavWidth * map.NavLength, Allocator.Persistent);
            killed = new NativeList<int>(64, Allocator.Persistent);
            caught = new NativeList<int>(64, Allocator.Persistent);
            expired = new NativeList<int>(64, Allocator.Persistent);
            fled = new NativeList<int>(64, Allocator.Persistent);
        }

        public bool IsAlight(SimWorld w, int slot) => w.IsAlive(slot) && AlightUntil[slot] > w.Tick && (w.Flags[slot] & (uint)UnitFlags.Burning) != 0;

        /// <summary>Set a man alight for <paramref name="seconds"/> (capped at MaxBurnSeconds from now; a man already
        /// burning keeps the longer of the two). Vehicles have their own fire (VehicleModulesSystem) and are ignored.</summary>
        public void Ignite(SimWorld w, int slot, float seconds)
        {
            if (!w.IsAlive(slot) || (w.Flags[slot] & (uint)UnitFlags.Vehicle) != 0) return;
            uint until = w.Tick + (uint)math.max(1, (int)math.ceil(math.min(seconds, MaxBurnSeconds) / w.Config.TickSeconds));
            // no fire, or a dead man's timer the job has not cleared yet (he died after it stepped and Spawn handed
            // his slot on with clean flags): either way this man was not alight, and the timer is his own now
            bool fresh = AlightUntil[slot] <= w.Tick || (w.Flags[slot] & (uint)UnitFlags.Burning) == 0;
            if (fresh || until > AlightUntil[slot]) AlightUntil[slot] = until;
            w.Flags[slot] |= (uint)UnitFlags.Burning;
            Active = true;
            if (fresh)
            {
                MenIgnited++;
                w.Events.Add(w.Tick, SimEventType.UnitAlight, slot, 1, w.Position[slot], default, (AlightUntil[slot] - w.Tick) * w.Config.TickSeconds);
            }
        }

        /// <summary>Set the ground alight at a point for <paramref name="seconds"/>. A cell already burning keeps the later end.</summary>
        public void IgniteCell(SimWorld w, float3 pos, float seconds, int player)
        {
            int cell = CellOf(pos);
            if ((map.NavLayers[cell] & (byte)NavLayer.Blocked) != 0) return;
            uint until = w.Tick + (uint)math.max(1, (int)math.ceil(seconds / w.Config.TickSeconds));
            for (int k = 0; k < Cells.Length; k++)
            {
                if (Cells[k].Cell != cell) continue;
                var c = Cells[k];
                if (until > c.Until) { c.Until = until; Cells[k] = c; }
                return;
            }
            Cells.Add(new BurningCell { Cell = cell, Until = until, Player = player });
            Active = true;
            w.Events.Add(w.Tick, SimEventType.CellBurning, cell, player, CellCentre(cell), default, seconds);
        }

        public int CellOf(float3 p)
        {
            int cx = math.clamp((int)(p.x / MapData.NavCellSize), 0, map.NavWidth - 1);
            int cz = math.clamp((int)(p.z / MapData.NavCellSize), 0, map.NavLength - 1);
            return cz * map.NavWidth + cx;
        }

        float3 CellCentre(int cell) => new float3((cell % map.NavWidth + 0.5f) * MapData.NavCellSize, 0f, (cell / map.NavWidth + 0.5f) * MapData.NavCellSize);

        public void Step(SimWorld w)
        {
            highWater = w.HighWater;

            // ---- what the tick's bursts lit: every incendiary that BlastSystem resolved this tick --------------------
            var resolved = blast.Resolved;
            for (int k = 0; k < resolved.Length; k++)
            {
                var im = resolved[k];
                if (im.Shape != (int)BlastShape.Incendiary) continue;
                for (int i = 0; i < w.HighWater; i++)
                {
                    if (!w.IsAlive(i) || (w.Flags[i] & (uint)UnitFlags.Vehicle) != 0) continue;
                    float3 d = w.Position[i] - im.Pos; d.y = 0f;
                    if (SimMath.Length(d) < im.Radius) Ignite(w, i, BurstManSeconds);
                }
                LightGround(w, im.Pos, im.Radius, im.Player);
            }

            if (!Active) return;

            // ---- drop the cells that went out; mark the ones that burn -------------------------------------------
            for (int k = Cells.Length - 1; k >= 0; k--) if (Cells[k].Until <= w.Tick) Cells.RemoveAt(k);   // RemoveAt keeps the order
            for (int i = 0; i < cellFire.Length; i++) cellFire[i] = 0;
            for (int k = 0; k < Cells.Length; k++) cellFire[Cells[k].Cell] = 1;

            killed.Clear(); caught.Clear(); expired.Clear(); fled.Clear();
            new BurnJob
            {
                Count = w.HighWater, Tick = w.Tick, Dt = w.Config.TickSeconds, NavWidth = map.NavWidth, NavLength = map.NavLength,
                CatchTicks = (uint)math.max(1, (int)math.ceil(GroundCatchSeconds / w.Config.TickSeconds)),
                Position = w.Position, Flags = w.Flags, TrenchId = w.TrenchId, Hp = w.Hp, Suppression = w.Suppression,
                AlightUntil = AlightUntil, CellFire = cellFire,
                Killed = killed, Caught = caught, Expired = expired, Fled = fled,
            }.Run();

            for (int k = 0; k < caught.Length; k++)
            {
                int i = caught[k];
                MenIgnited++;
                w.Events.Add(w.Tick, SimEventType.UnitAlight, i, 1, w.Position[i], default, (AlightUntil[i] - w.Tick) * w.Config.TickSeconds);
            }
            for (int k = 0; k < expired.Length; k++)
            {
                int i = expired[k];
                w.Events.Add(w.Tick, SimEventType.UnitAlight, i, 0, w.Position[i]);
            }
            // a burning garrison runs: out of the trench and for its own HQ, exactly as a gassed one does
            for (int k = 0; k < fled.Length; k++)
            {
                int i = fled[k];
                short trench = w.TrenchId[i];
                w.SourceTrench[i] = trench;
                w.TrenchId[i] = -1;
                short hq = fields != null ? fields.OwnHq(w.Team[i]) : (short)-1;
                w.GoalId[i] = hq >= 0 ? fields.GetGoal(GoalKey.Objective(hq)) : -1;
                w.Flags[i] |= (uint)UnitFlags.Exposed;
                w.Events.Add(w.Tick, SimEventType.UnitLeftTrench, i, trench, w.Position[i]);
            }
            for (int k = 0; k < killed.Length; k++)
            {
                int i = killed[k];
                float3 way = w.Velocity[i]; way.y = 0f;
                float len = SimMath.Length(way);
                AlightUntil[i] = 0;
                w.Despawn(i, (int)DeathCause.Burning, len > 1e-3f ? way / len : default, 0f);
            }

            Active = Cells.Length > 0 || AnyAlight(w);
        }

        bool AnyAlight(SimWorld w)
        {
            for (int i = 0; i < w.HighWater; i++) if (AlightUntil[i] > w.Tick && w.IsAlive(i) && (w.Flags[i] & (uint)UnitFlags.Burning) != 0) return true;
            return false;
        }

        void LightGround(SimWorld w, float3 at, float radius, int player)
        {
            int r = (int)math.ceil(radius / MapData.NavCellSize);
            int cx = (int)(at.x / MapData.NavCellSize), cz = (int)(at.z / MapData.NavCellSize);
            int lit = 0;
            for (int z = cz - r; z <= cz + r && lit < MaxCellsPerBurst; z++)
            for (int x = cx - r; x <= cx + r && lit < MaxCellsPerBurst; x++)
            {
                if (x < 0 || z < 0 || x >= map.NavWidth || z >= map.NavLength) continue;
                float3 c = new float3((x + 0.5f) * MapData.NavCellSize, 0f, (z + 0.5f) * MapData.NavCellSize);
                float3 d = c - at; d.y = 0f;
                if (SimMath.Length(d) > radius) continue;
                IgniteCell(w, c, BurstCellSeconds, player);
                lit++;
            }
        }

        [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct BurnJob : IJob
        {
            public int Count, NavWidth, NavLength;
            public uint Tick, CatchTicks;
            public float Dt;
            [ReadOnly] public NativeArray<float3> Position;
            [ReadOnly] public NativeArray<short> TrenchId;
            [ReadOnly] public NativeArray<byte> CellFire;
            public NativeArray<uint> Flags;
            public NativeArray<uint> AlightUntil;
            public NativeArray<float> Hp, Suppression;
            public NativeList<int> Killed, Caught, Expired, Fled;

            public void Execute()
            {
                for (int i = 0; i < Count; i++)
                {
                    uint f = Flags[i];
                    bool alive = (f & (uint)UnitFlags.Alive) != 0 && Hp[i] > 0f;
                    if (!alive || (f & (uint)UnitFlags.Vehicle) != 0)
                    {
                        // a dead man's fire is out, so a reused slot never inherits it; said once (UnitAlight b = 0)
                        // whatever killed him, so the picture douses his slot
                        if (AlightUntil[i] != 0)
                        {
                            AlightUntil[i] = 0;
                            if ((f & (uint)UnitFlags.Vehicle) == 0) Expired.Add(i);   // a vehicle in a dead torch's slot only drops the timer: its fire is its own
                        }
                        continue;
                    }
                    bool alight = AlightUntil[i] > Tick;
                    if (!alight)
                    {
                        float3 p = Position[i];
                        int nx = math.clamp((int)(p.x / MapData.NavCellSize), 0, NavWidth - 1), nz = math.clamp((int)(p.z / MapData.NavCellSize), 0, NavLength - 1);
                        if (CellFire[nz * NavWidth + nx] == 0)
                        {
                            if ((f & (uint)UnitFlags.Burning) != 0) Flags[i] = f & ~(uint)UnitFlags.Burning;
                            continue;
                        }
                        AlightUntil[i] = Tick + CatchTicks;
                        Flags[i] = f | (uint)UnitFlags.Burning;
                        Caught.Add(i);
                        alight = true;
                    }
                    else if ((f & (uint)UnitFlags.Burning) == 0)
                    {
                        // a timer without the flag is a dead man's: he died after this system stepped (gas, a track)
                        // and Spawn handed his slot on before it stepped again, clearing his flags. Only Ignite and
                        // the catch above set the two together, so this man was never alight: out, silently
                        AlightUntil[i] = 0;
                        continue;
                    }

                    Hp[i] = Hp[i] - BurnDps * Dt;
                    Suppression[i] = math.min(100f, Suppression[i] + SuppressionPerSecond * Dt);
                    if (Hp[i] <= 0f) { Killed.Add(i); continue; }
                    if (TrenchId[i] >= 0) Fled.Add(i);
                    if (AlightUntil[i] <= Tick + 1)
                    {
                        // this was the last tick of it
                        AlightUntil[i] = 0;
                        Flags[i] = Flags[i] & ~(uint)UnitFlags.Burning;
                        Expired.Add(i);
                    }
                }
            }
        }

        public ulong Hash(ulong h)
        {
            h = SimHash.Array(AlightUntil, highWater, h);
            for (int k = 0; k < Cells.Length; k++) h = SimHash.Value(Cells[k], h);
            return h;
        }

        public void Dispose()
        {
            if (AlightUntil.IsCreated) AlightUntil.Dispose();
            if (Cells.IsCreated) Cells.Dispose();
            if (cellFire.IsCreated) cellFire.Dispose();
            if (killed.IsCreated) killed.Dispose();
            if (caught.IsCreated) caught.Dispose();
            if (expired.IsCreated) expired.Dispose();
            if (fled.IsCreated) fled.Dispose();
        }
    }
}
