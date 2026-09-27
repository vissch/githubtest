// Phase: N2 (stub) — depends on: com.unity.transport 2.x, ILockstepTransport
// Reliable-sequenced pipeline for command frames, lobby handshake (seed, map id, factions, difficulty, roster
// choices), clock sync, and stall handling (peer missing for > 40 ticks → stall UI, > 600 → drop).
using System.Collections.Generic;

namespace TW.Net
{
    public sealed class UtpTransport : ILockstepTransport
    {
        public int LocalPlayer => throw new System.NotImplementedException("Phase N2: UtpTransport");
        public int PeerCount => throw new System.NotImplementedException("Phase N2: UtpTransport");
        public void Send(in CommandFrame frame) => throw new System.NotImplementedException("Phase N2: UtpTransport.Send");
        public void Receive(List<CommandFrame> into) => throw new System.NotImplementedException("Phase N2: UtpTransport.Receive");
        public void Pump(int simTick) => throw new System.NotImplementedException("Phase N2: UtpTransport.Pump");
    }

    public struct LobbyHandshake
    {
        public uint Seed; public int MapId; public byte FactionA, FactionB; public byte Difficulty; public int TickRate; public int InputDelay;
        /// <summary>The ten archetypes each side chose (SimConfig.Loadout*), empty for the faction's default ten. Both
        /// machines must build the same rosters from this before the first tick or slot 3 means two different units.</summary>
        public Unity.Collections.FixedList32Bytes<byte> LoadoutA, LoadoutB;
    }
}
