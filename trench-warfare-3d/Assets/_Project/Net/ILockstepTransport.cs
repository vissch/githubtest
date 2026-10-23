// Phase: N1 (implemented interface)
// A transport moves command frames between peers. Frames are (tick, player, commands[]). The driver never
// steps a tick until it holds a frame from every peer for that tick.
using System.Collections.Generic;
using TW.Sim;

namespace TW.Net
{
    public struct CommandFrame
    {
        public const uint NoHash = uint.MaxValue;   // HashTick when the sender had no hash for the tick it just stepped

        public uint Tick;
        public byte Player;
        public SimCommand[] Commands;   // may be empty; an empty frame still counts as "peer reported for tick"
        public ulong Hash;              // the sender's sim hash when it sent this frame, for desync detection (N3).
                                        // Meaningful only when HasHash. 0 is a real hash value, not a marker: a world
                                        // that skipped the hash (SimWorld.HashInterval) and a seat that owns no world
                                        // send Hash 0 together with HashTick = NoHash.
        public uint HashTick;           // S9: the tick Hash is the hash of. It is NOT frame.Tick - 1: a frame is sent
                                        // InputDelay ticks ahead, so HashTick is the last tick the sender had stepped.
                                        // NoHash when the sender had no hash. Do not read this field unless HasHash.
        public bool HasHash => HashTick != NoHash;
    }

    public interface ILockstepTransport
    {
        int LocalPlayer { get; }
        int PeerCount { get; }
        void Send(in CommandFrame frame);
        /// <summary>Drain frames that have arrived since the last call. Called once per driver update.</summary>
        void Receive(List<CommandFrame> into);
        /// <summary>Advance any internal clock (loopback latency simulation). Real transports pump the socket here.</summary>
        void Pump(int simTick);
    }
}
