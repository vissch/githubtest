// Phase: P0 (implemented) — Event stream contract, see docs/02-contracts.md
// Presentation reads events exactly once per frame; they are never read back by the sim.
using Unity.Collections;
using Unity.Mathematics;

namespace TW.Sim
{
    public enum SimEventType : byte
    {
        None = 0,
        Shot,               // a = shooter slot, b = target slot, dir = direction, scalar = 0 direct / 1 arc
        Hit,                // a = shooter, b = target, scalar = damage (negative = ricochet)
        NearMiss,           // a = target slot, scalar = suppression added
        Explosion,          // a = ability/weapon id, pos, scalar = radius
        CraterStamp,        // pos, scalar = radius, dir.y = depth
        Death,              // a = slot, b = killer slot, dir = impulse
        StanceChanged,      // a = slot, b = new stance
        Suppressed,         // a = slot, scalar = meter value
        Pinned,             // a = slot
        UnitSpawned,        // a = slot, b = archetype
        UnitEnteredTrench,  // a = slot, b = trench id
        UnitLeftTrench,     // a = slot, b = trench id
        TrenchCaptured,     // a = trench id, b = team
        ObjectiveCaptured,  // a = objective id, b = team
        GasCloudSpawned,    // a = agent id, pos, dir = wind, scalar = concentration
        SmokeSpawned,       // pos, dir = line direction, scalar = length
        VehicleTrackHit,    // a = slot
        VehicleStalled,     // a = slot
        VehicleDestroyed,   // a = slot
        WireBreached,       // pos, scalar = width
        AbilityFired,       // a = ability id, b = player, pos
        WaveStarted,        // a = wave index, b = unit count
        MissionTriggerFired,// a = trigger id
        CellBurning,        // pos, scalar = seconds
        MatchEnded,         // a = winning team (-1 draw)
        CommandRejected,    // a = command type, b = player (debug only; not hashed)
    }

    public struct SimEvent
    {
        public uint Tick;
        public SimEventType Type;
        public int A;
        public int B;
        public float3 Pos;
        public float3 Dir;
        public float Scalar;
    }

    /// <summary>Per-tick append-only buffer. Cleared by SimWorld at the start of every Step().</summary>
    public struct SimEventBuffer
    {
        public NativeList<SimEvent> Events;
        /// <summary>Number of events dropped because the buffer was full. Presentation must rebuild from state if > 0.</summary>
        public int Overrun;
        int capacity;

        public SimEventBuffer(int capacity, Allocator allocator)
        {
            Events = new NativeList<SimEvent>(capacity, allocator);
            Overrun = 0;
            this.capacity = capacity;
        }

        public void Clear() { Events.Clear(); Overrun = 0; }

        public void Add(SimEvent e)
        {
            if (Events.Length >= capacity) { Overrun++; return; }
            Events.Add(e);
        }

        public void Add(uint tick, SimEventType type, int a = 0, int b = 0, float3 pos = default, float3 dir = default, float scalar = 0f)
            => Add(new SimEvent { Tick = tick, Type = type, A = a, B = b, Pos = pos, Dir = dir, Scalar = scalar });

        public bool IsCreated => Events.IsCreated;
        public void Dispose() { if (Events.IsCreated) Events.Dispose(); }
    }
}
