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
        // ---- 2026-09-25 (replay v5): factions and the hero pity each side brings to the match ----
        /// <summary>FactionId of player 0 and player 1: their rosters and the support they may call (FactionRoster).</summary>
        public byte FactionA, FactionB;
        /// <summary>Hero pity each side starts the match with, 0..1 (HeroSystem; the profile's residue from the last one).</summary>
        public float HeroPity0, HeroPity1;
        // ---- 2026-09-25 (replay v6): the ten each side chose at the briefing (owner decision) ----
        /// <summary>
        /// The archetypes filling a side's ten slots this battle, in slot order. EMPTY means the faction's default ten
        /// (FactionRoster), which is what every mission and every test that does not choose gets. Fewer than ten falls
        /// back to the faction's slot for the rest, so a loadout may name only what it changes.
        ///
        /// It lives in the config because it must be part of what both machines agree on before the first tick: a side
        /// deploying a different unit from slot 3 than its peer thinks it deployed is a desync, not a mistake.
        /// </summary>
        public Unity.Collections.FixedList32Bytes<byte> LoadoutA, LoadoutB;

        public float TickSeconds => 1f / TickRate;
        public FactionId FactionOf(int player) => Factions.Of(player == 0 ? FactionA : FactionB);
        public Unity.Collections.FixedList32Bytes<byte> LoadoutOf(int player) => player == 0 ? LoadoutA : LoadoutB;

        public static SimConfig Default => new SimConfig
        {
            TickRate = 20,
            InputDelayTicks = 3,
            MaxSlots = 3584,   // owner decision 2026-09-20: 3,000 units is the ceiling; the rest is head room for vehicles and emplacements
            Seed = 0xC0FFEEu,
            SilverPerSecond = 1f,
            StartingSilver = 120,
            EventCapacity = 8192,
            FactionA = (byte)FactionId.Iron,
            FactionB = (byte)FactionId.Brass,
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
