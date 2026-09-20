// Phase: P0 (implemented)
// Fixed simulation configuration. Everything here is part of the lockstep handshake and the replay header.
using Unity.Mathematics;

namespace TW.Sim
{
    public struct SimConfig
    {
        public const int MaxPlayers = 2;

        /// <summary>Simulation ticks per second. 20 Hz = 50 ms per tick.</summary>
        public int TickRate;
        /// <summary>Ticks between a command being issued locally and executed on every peer.</summary>
        public int InputDelayTicks;
        /// <summary>Maximum live unit slots (infantry + vehicles + emplacements).</summary>
        public int MaxSlots;
        /// <summary>Match seed. Every random draw in the sim derives from this, the tick and a system id.</summary>
        public uint Seed;
        /// <summary>Silver income per second per player (mission-tunable).</summary>
        public float SilverPerSecond;
        /// <summary>Starting silver per player.</summary>
        public int StartingSilver;
        /// <summary>Capacity of the per-tick event ring buffer.</summary>
        public int EventCapacity;

        public float TickSeconds => 1f / TickRate;

        public static SimConfig Default => new SimConfig
        {
            TickRate = 20,
            InputDelayTicks = 3,
            MaxSlots = 3584,   // owner decision 2026-09-20: 3,000 units is the ceiling; the rest is head room for vehicles and emplacements
            Seed = 0xC0FFEEu,
            SilverPerSecond = 1f,
            StartingSilver = 120,
            EventCapacity = 8192,
        };

        /// <summary>Corridor and spawn description handed to the sim by the map (see TW.Sim.Terrain.MapData).</summary>
        public struct WorldInit
        {
            public float2 SizeMeters;      // X = lateral width, Y = longitudinal length (Z axis in world)
            public float3 SpawnA;          // player 0 deployment point
            public float3 SpawnB;          // player 1 deployment point
            public float GoalZA;           // Z that player 0 units advance toward in Phase 0 movement
            public float GoalZB;           // Z that player 1 units advance toward in Phase 0 movement
        }
    }
}
