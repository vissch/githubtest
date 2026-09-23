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
        VehicleTrackHit,    // a = slot, b = side (0 left, 1 right)
        VehicleStalled,     // a = slot, b = 1 the engine died / 0 it runs again
        VehicleDestroyed,   // a = slot, b = killer, dir.y = hull yaw (SimWorld.Despawn)
        WireBreached,       // pos, scalar = width
        AbilityFired,       // a = ability id, b = player, pos
        WaveStarted,        // a = wave index, b = unit count
        MissionTriggerFired,// a = trigger id
        CellBurning,        // pos, scalar = seconds
        MatchEnded,         // a = winning team (-1 draw)
        CommandRejected,    // a = command type, b = player (debug only; not hashed)
        PropChanged,        // a = prop index, b = new PropKind, pos (a tree broke, a wreck appeared; for a wreck dir.x = the dead slot + 1)
        // ---- armour (A5b): TankGunnerySystem, VehicleModulesSystem, VehicleKinematicsSystem ----
        VehicleFired,       // a = slot, b = gun index, pos = where the round went, dir = muzzle direction, scalar = 0 AP / 1 HE
        VehicleArmourHit,   // a = target, b = shooter (-1 blast), pos, dir = the round's direction, scalar = plate mm: + holed, - stopped
        VehicleModuleHit,   // a = slot, b = VehicleModule, scalar = what is left of it (0 = destroyed)
        VehicleOnFire,      // a = slot, b = 1 caught / 0 put out, scalar = intensity 0..1
        VehicleCrewLost,    // a = slot, b = crew left
        VehicleBailedOut,   // a = slot, b = men who got out (they are riflemen now)
        VehicleKnockedOut,  // a = slot, b = VehicleKillCause, dir.y = hull yaw
        VehicleCookOff,     // a = slot, pos, scalar = blast radius (its Explosion comes from BlastSystem the next tick)
        VehicleBogged,      // a = slot, b = 1 stuck / 0 free
        VehicleDitched,     // a = slot, b = trench id it nosed into (-1 = climbed out)
        VehicleCrushed,     // a = slot, b = 0 wire / 1 tree / 2 man, pos
        VehicleRepaired,    // a = slot, b = VehicleModule
        VehicleLegLost,     // a = slot, b = which leg (left legs first), pos = where it stood: a walker is down a leg
        VehicleClawed,      // a = slot, b = victim, pos = where it caught him: a claw closed on a man or a hull
        // ---- landings (A3): SeaLandingSystem ----
        CraftInbound,       // a = craft index, b = team, pos = where it appeared out to sea
        CraftBeached,       // a = craft index, b = team, pos = where it grounded
        ShipFired,          // a = ship index, b = team, pos = where the shell is going, dir = from the ship to it
        // ---- orders (A3): TrenchOrderSystem ----
        OrderFoundNoOne,    // a = command type, b = player: the order was VALID and moved nobody (an empty trench).
                            // Deliberately not CommandRejected, which means the command itself was bad - a UI that
                            // beeps at a rejection should not beep at a player who ordered an empty line forward.
    }

    /// <summary>Why a vehicle stopped fighting (VehicleKnockedOut.b).</summary>
    public enum VehicleKillCause : byte { Structure = 0, CrewLost = 1, Fire = 2, Ammunition = 3 }

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
