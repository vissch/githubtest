// Phase: N1 (implemented interface)
// A transport moves command frames between peers. Frames are (tick, player, commands[]). The driver never
// steps a tick until it holds a frame from every peer for that tick.
using System.Collections.Generic;
using TW.Sim;

namespace TW.Net
{
    public struct CommandFrame
    {
        public uint Tick;
        public byte Player;
        public SimCommand[] Commands;   // may be empty; an empty frame still counts as "peer reported for tick"
        public ulong Hash;              // the sender's sim hash when it sent this frame, for desync detection (N3).
                                        // Hash == 0 means "this frame carries no hash": the sender had not stepped a
                                        // tick yet (the primed frames before InputDelay), or its world skipped the
                                        // hash this tick (SimWorld.HashInterval != 1, the live single-player path).
                                        // A reader must skip a frame with Hash == 0 and must not read HashTick on it:
                                        // HashTick is meaningless when there is no hash to go with it.
        public uint HashTick;           // S9: the tick Hash is the hash of. It is NOT frame.Tick - 1: a frame is sent
                                        // InputDelay ticks ahead, so HashTick is the last tick the sender had stepped.
                                        // Ignore this field when Hash == 0.
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
