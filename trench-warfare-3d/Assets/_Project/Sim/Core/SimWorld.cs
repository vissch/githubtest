// Phase: P0 (implemented)
// Authoritative simulation state as Structure-of-Arrays keyed by stable slot index, stepped at a fixed tick.
// Rules: docs/03-determinism-rules.md. Every array below is hashed in Hash() unless marked transient.
using System;
using System.Collections.Generic;
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;

namespace TW.Sim
{
    public sealed class SimWorld : IDisposable
    {
        public SimConfig Config;
        public SimConfig.WorldInit Init;
        public uint Tick;
        public int WinnerTeam = -1;

        // ---- per-slot state (index = slot) ----
        public NativeArray<float3> Position;
        public NativeArray<float3> Velocity;
        public NativeArray<float> Yaw;
        public NativeArray<float> Hp;
        public NativeArray<float> MaxHp;
        public NativeArray<float> Suppression;
        public NativeArray<float> Speed;
        public NativeArray<byte> StanceOf;
        public NativeArray<byte> Team;
        public NativeArray<byte> Archetype;
        public NativeArray<byte> Layer;        // NavLayer bits of the cell the unit occupies (Surface/Trench)
        public NativeArray<short> TrenchId;    // -1 when not garrisoned
        public NativeArray<short> SourceTrench; // trench a unit last left under orders (fallback target); -1 none
        public NativeArray<int> TargetSlot;    // -1 when none
        public NativeArray<int> GoalId;        // flow-field goal group (A1); Phase 0: unused
        public NativeArray<uint> Flags;        // UnitFlags
        public NativeArray<ushort> Generation; // bumps on every spawn so stale slot references can be detected
        public NativeArray<int> Cooldown;      // generic per-unit cooldown ticks (grenade, ability)
        public NativeArray<int> FireCooldown;  // ticks until the primary weapon may fire again (A2)

        // ---- per-player state ----
        public NativeArray<int> Silver;
        public NativeArray<float> SilverFraction;
        public NativeArray<float3> Rally;
        public NativeArray<RosterEntry> Roster;     // MaxPlayers * RosterEntry.SlotCount
        public NativeArray<int> SlotCooldown;       // MaxPlayers * RosterEntry.SlotCount
        public NativeArray<byte> SlotUnlocked;      // MaxPlayers * RosterEntry.SlotCount (missions lock slots)

        // ---- bookkeeping ----
        NativeList<int> freeSlots;      // LIFO; deterministic
        public int HighWater;           // highest slot index ever used + 1
        public int AliveCount;
        public SimEventBuffer Events;   // transient (not hashed)
        public NativeList<SimCommand> TickCommands; // sorted commands for the current tick (transient)
        public ulong LastHash;
        public bool UsePhase0Movement = true;   // replaced by TW.Sim.Nav.MovementSystem in A1

        readonly List<ISimSystem> systems = new List<ISimSystem>();
        NativeList<SimCommand> sortScratch;

        public SimWorld(SimConfig config, SimConfig.WorldInit init)
        {
            Config = config;
            Init = init;
            int n = config.MaxSlots;
            Position = new NativeArray<float3>(n, Allocator.Persistent);
            Velocity = new NativeArray<float3>(n, Allocator.Persistent);
            Yaw = new NativeArray<float>(n, Allocator.Persistent);
            Hp = new NativeArray<float>(n, Allocator.Persistent);
            MaxHp = new NativeArray<float>(n, Allocator.Persistent);
            Suppression = new NativeArray<float>(n, Allocator.Persistent);
            Speed = new NativeArray<float>(n, Allocator.Persistent);
            StanceOf = new NativeArray<byte>(n, Allocator.Persistent);
            Team = new NativeArray<byte>(n, Allocator.Persistent);
            Archetype = new NativeArray<byte>(n, Allocator.Persistent);
            Layer = new NativeArray<byte>(n, Allocator.Persistent);
            TrenchId = new NativeArray<short>(n, Allocator.Persistent);
            SourceTrench = new NativeArray<short>(n, Allocator.Persistent);
            TargetSlot = new NativeArray<int>(n, Allocator.Persistent);
            GoalId = new NativeArray<int>(n, Allocator.Persistent);
            Flags = new NativeArray<uint>(n, Allocator.Persistent);
            Generation = new NativeArray<ushort>(n, Allocator.Persistent);
            Cooldown = new NativeArray<int>(n, Allocator.Persistent);
            FireCooldown = new NativeArray<int>(n, Allocator.Persistent);
            for (int i = 0; i < n; i++) { TrenchId[i] = -1; SourceTrench[i] = -1; TargetSlot[i] = -1; GoalId[i] = -1; }

            int p = SimConfig.MaxPlayers;
            Silver = new NativeArray<int>(p, Allocator.Persistent);
            SilverFraction = new NativeArray<float>(p, Allocator.Persistent);
            Rally = new NativeArray<float3>(p, Allocator.Persistent);
            Roster = new NativeArray<RosterEntry>(p * RosterEntry.SlotCount, Allocator.Persistent);
            SlotCooldown = new NativeArray<int>(p * RosterEntry.SlotCount, Allocator.Persistent);
            SlotUnlocked = new NativeArray<byte>(p * RosterEntry.SlotCount, Allocator.Persistent);
            for (int i = 0; i < p; i++)
            {
                Silver[i] = config.StartingSilver;
                Rally[i] = i == 0 ? init.SpawnA : init.SpawnB;
                RosterEntry.FillDefault(Roster, i * RosterEntry.SlotCount);
                for (int s = 0; s < RosterEntry.SlotCount; s++) SlotUnlocked[i * RosterEntry.SlotCount + s] = 1;
            }

            freeSlots = new NativeList<int>(n, Allocator.Persistent);
            Events = new SimEventBuffer(config.EventCapacity, Allocator.Persistent);
            TickCommands = new NativeList<SimCommand>(256, Allocator.Persistent);
            sortScratch = new NativeList<SimCommand>(256, Allocator.Persistent);
        }

        // ------------------------------------------------------------------ systems
        public void AddSystem(ISimSystem system)
        {
            systems.Add(system);
            systems.Sort((a, b) => a.Order.CompareTo(b.Order));
            system.Initialize(this);
            if (system.Order == SimSystemOrder.Movement) UsePhase0Movement = false;
        }

        public T GetSystem<T>() where T : class, ISimSystem
        {
            foreach (var s in systems) if (s is T t) return t;
            return null;
        }

        // ------------------------------------------------------------------ slots
        public bool IsAlive(int slot) => slot >= 0 && slot < HighWater && (Flags[slot] & (uint)UnitFlags.Alive) != 0;

        public int Spawn(byte team, byte archetype, float3 pos, float hp, float speed, bool vehicle)
        {
            int slot;
            if (freeSlots.Length > 0) { slot = freeSlots[freeSlots.Length - 1]; freeSlots.RemoveAt(freeSlots.Length - 1); }
            else { if (HighWater >= Config.MaxSlots) return -1; slot = HighWater++; }

            Position[slot] = pos; Velocity[slot] = float3.zero; Yaw[slot] = team == 0 ? 0f : SimMath.Pi;
            Hp[slot] = hp; MaxHp[slot] = hp; Suppression[slot] = 0f; Speed[slot] = speed;
            StanceOf[slot] = (byte)Stance.Standing; Team[slot] = team; Archetype[slot] = archetype; Layer[slot] = 1;
            TrenchId[slot] = -1; SourceTrench[slot] = -1; TargetSlot[slot] = -1; GoalId[slot] = -1; Cooldown[slot] = 0; FireCooldown[slot] = 0;
            Flags[slot] = (uint)UnitFlags.Alive | (vehicle ? (uint)UnitFlags.Vehicle : 0u);
            Generation[slot] = (ushort)(Generation[slot] + 1);
            AliveCount++;
            Events.Add(Tick, SimEventType.UnitSpawned, slot, archetype, pos);
            return slot;
        }

        public void Despawn(int slot, int killer = -1, float3 impulse = default)
        {
            if (!IsAlive(slot)) return;
            bool vehicle = (Flags[slot] & (uint)UnitFlags.Vehicle) != 0;
            Flags[slot] = 0u; StanceOf[slot] = (byte)Stance.Dead; Hp[slot] = 0f; TrenchId[slot] = -1; TargetSlot[slot] = -1;
            freeSlots.Add(slot);
            AliveCount--;
            Events.Add(Tick, SimEventType.Death, slot, killer, Position[slot], impulse);
            if (vehicle) Events.Add(Tick, SimEventType.VehicleDestroyed, slot, killer, Position[slot], new float3(0f, Yaw[slot], 0f));   // dir.y = hull yaw, for the wreck
        }

        // ------------------------------------------------------------------ tick
        /// <summary>Advance one tick. <paramref name="commands"/> holds every peer's commands scheduled for this tick.</summary>
        public void Step(NativeArray<SimCommand> commands)
        {
            Events.Clear();
            SortCommands(commands);
            ApplyCoreCommands();
            StepEconomy();
            foreach (var s in systems) s.Step(this);
            if (UsePhase0Movement) StepPhase0Movement();
            Tick++;
            LastHash = Hash();
        }

        void SortCommands(NativeArray<SimCommand> commands)
        {
            // Stable sort by player; arrival order within a player is preserved (peers send frames in issue order).
            TickCommands.Clear();
            for (int p = 0; p < SimConfig.MaxPlayers; p++)
                for (int i = 0; i < commands.Length; i++)
                    if (commands[i].Player == p) TickCommands.Add(commands[i]);
        }

        void ApplyCoreCommands()
        {
            for (int i = 0; i < TickCommands.Length; i++)
            {
                var c = TickCommands[i];
                if (c.Player >= SimConfig.MaxPlayers) { Reject(c); continue; }
                switch (c.Type)
                {
                    case CommandType.DeployUnit: Deploy(c); break;
                    case CommandType.SetRally:
                        Rally[c.Player] = ClampToMap(c.Pos);
                        break;
                    case CommandType.Surrender:
                        if (WinnerTeam < 0) { WinnerTeam = 1 - c.Player; Events.Add(Tick, SimEventType.MatchEnded, WinnerTeam); }
                        break;
                    // Trench orders, stances and abilities are consumed by their systems (A3/A5) from TickCommands.
                }
            }
        }

        // transient, derived from the tick's commands: not hashed. Per player, so the stream does not depend on how the
        // two players' commands interleave within a tick.
        uint deployTick; readonly int[] deploysThisTick = new int[SimConfig.MaxPlayers];

        void Deploy(SimCommand c)
        {
            if (c.A < 0 || c.A >= RosterEntry.SlotCount) { Reject(c); return; }
            int ri = c.Player * RosterEntry.SlotCount + c.A;
            var entry = Roster[ri];
            if (SlotUnlocked[ri] == 0 || SlotCooldown[ri] > 0 || Silver[c.Player] < entry.Cost) { Reject(c); return; }
            // one stream per deploy: several deploys by one player in one tick must not share a spawn point
            if (deployTick != Tick) { deployTick = Tick; System.Array.Clear(deploysThisTick, 0, deploysThisTick.Length); }
            var rng = SimRandom.For(Config.Seed, Tick, SimRandom.SystemId.Deployment, (uint)c.Player + 16u * (uint)deploysThisTick[c.Player]++);
            float3 spawn = c.Player == 0 ? Init.SpawnA : Init.SpawnB;
            spawn.x += rng.NextFloat(-30f, 30f);   // reinforcements come up on a front, so they use several ladders
            spawn.z += rng.NextFloat(-2f, 2f);
            int slot = Spawn(c.Player, entry.Archetype, ClampToMap(spawn), entry.Hp, entry.Speed, entry.IsVehicle);
            if (slot < 0) { Reject(c); return; }
            Silver[c.Player] -= entry.Cost;
            SlotCooldown[ri] = entry.CooldownTicks;
        }

        /// <summary>Drop a command deterministically and report it. Systems call this for commands they validate.</summary>
        public void Reject(SimCommand c) => Events.Add(Tick, SimEventType.CommandRejected, (int)c.Type, c.Player);

        void StepEconomy()
        {
            for (int p = 0; p < SimConfig.MaxPlayers; p++)
            {
                float f = SilverFraction[p] + Config.SilverPerSecond * Config.TickSeconds;
                int whole = (int)f;
                Silver[p] += whole;
                SilverFraction[p] = f - whole;
                for (int s = 0; s < RosterEntry.SlotCount; s++)
                {
                    int ri = p * RosterEntry.SlotCount + s;
                    if (SlotCooldown[ri] > 0) SlotCooldown[ri]--;
                }
            }
        }

        public float3 ClampToMap(float3 p)
        {
            p.x = math.clamp(p.x, 0f, Init.SizeMeters.x);
            p.z = math.clamp(p.z, 0f, Init.SizeMeters.y);
            return p;
        }

        // ------------------------------------------------------------------ Phase 0 movement (replaced in A1)
        [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct Phase0MoveJob : IJobParallelFor
        {
            public NativeArray<float3> Position;
            public NativeArray<float3> Velocity;
            public NativeArray<float> Yaw;
            [ReadOnly] public NativeArray<float> Speed;
            [ReadOnly] public NativeArray<byte> Team;
            [ReadOnly] public NativeArray<uint> Flags;
            public float GoalZA, GoalZB, Dt;

            public void Execute(int i)
            {
                if ((Flags[i] & (uint)UnitFlags.Alive) == 0) return;
                float goalZ = Team[i] == 0 ? GoalZA : GoalZB;
                float3 p = Position[i];
                float dz = goalZ - p.z;
                float step = Speed[i] * Dt;
                float3 v = float3.zero;
                if (math.abs(dz) > step)
                {
                    v = new float3(0f, 0f, dz > 0f ? Speed[i] : -Speed[i]);
                    p.z += dz > 0f ? step : -step;
                    Yaw[i] = SimMath.YawOf(v);
                }
                else p.z = goalZ;
                Position[i] = p;
                Velocity[i] = v;
            }
        }

        void StepPhase0Movement()
        {
            var job = new Phase0MoveJob
            {
                Position = Position, Velocity = Velocity, Yaw = Yaw, Speed = Speed, Team = Team, Flags = Flags,
                GoalZA = Init.GoalZA, GoalZB = Init.GoalZB, Dt = Config.TickSeconds,
            };
            job.Schedule(HighWater, 128).Complete();
        }

        // ------------------------------------------------------------------ hash
        public ulong Hash()
        {
            ulong h = SimHash.Offset;
            h = SimHash.Value(Tick, h);
            h = SimHash.Value(WinnerTeam, h);
            h = SimHash.Value(HighWater, h);
            h = SimHash.Value(AliveCount, h);
            h = SimHash.Array(Silver, h);
            h = SimHash.Array(SilverFraction, h);
            h = SimHash.Array(Rally, h);
            h = SimHash.Array(SlotCooldown, h);
            h = SimHash.Array(SlotUnlocked, h);
            int n = HighWater;
            h = SimHash.Array(Position, n, h);
            h = SimHash.Array(Velocity, n, h);
            h = SimHash.Array(Yaw, n, h);
            h = SimHash.Array(Hp, n, h);
            h = SimHash.Array(Suppression, n, h);
            h = SimHash.Array(Speed, n, h);
            h = SimHash.Array(StanceOf, n, h);
            h = SimHash.Array(Team, n, h);
            h = SimHash.Array(Archetype, n, h);
            h = SimHash.Array(Layer, n, h);
            h = SimHash.Array(TrenchId, n, h);
            h = SimHash.Array(SourceTrench, n, h);
            h = SimHash.Array(TargetSlot, n, h);
            h = SimHash.Array(GoalId, n, h);
            h = SimHash.Array(Flags, n, h);
            h = SimHash.Array(Generation, n, h);
            h = SimHash.Array(Cooldown, n, h);
            h = SimHash.Array(FireCooldown, n, h);
            foreach (var s in systems) h = s.Hash(h);
            return h;
        }

        public void Dispose()
        {
            foreach (var s in systems) s.Dispose();
            systems.Clear();
            Position.Dispose(); Velocity.Dispose(); Yaw.Dispose(); Hp.Dispose(); MaxHp.Dispose(); Suppression.Dispose();
            Speed.Dispose(); StanceOf.Dispose(); Team.Dispose(); Archetype.Dispose(); Layer.Dispose(); TrenchId.Dispose(); SourceTrench.Dispose();
            TargetSlot.Dispose(); GoalId.Dispose(); Flags.Dispose(); Generation.Dispose(); Cooldown.Dispose(); FireCooldown.Dispose();
            Silver.Dispose(); SilverFraction.Dispose(); Rally.Dispose(); Roster.Dispose(); SlotCooldown.Dispose(); SlotUnlocked.Dispose();
            freeSlots.Dispose(); Events.Dispose(); TickCommands.Dispose(); sortScratch.Dispose();
        }
    }
}
