// Phase: N1 (implemented)
// Deterministic lockstep: local commands issued at tick T are scheduled for T + InputDelay and sent as a frame.
// The driver steps tick T only once it holds a frame from every peer for T. Every peer therefore executes an
// identical command set per tick; hashes are recorded per tick for desync detection (compared in N3).
using System.Collections.Generic;
using Unity.Collections;
using TW.Sim;

namespace TW.Net
{
    public sealed class LockstepDriver
    {
        public readonly SimWorld World;
        public readonly ILockstepTransport Transport;
        public readonly int InputDelay;
        public readonly ReplayRecorder Recorder;   // optional

        readonly List<SimCommand> pendingLocal = new List<SimCommand>();
        readonly Dictionary<uint, Dictionary<byte, CommandFrame>> frames = new Dictionary<uint, Dictionary<byte, CommandFrame>>();
        readonly List<CommandFrame> scratch = new List<CommandFrame>();
        readonly List<SimCommand> tickScratch = new List<SimCommand>();
        uint nextSendTick;
        public int StallTicks;         // ticks we could not step because a peer frame was missing (UI shows a stall after N)
        public uint LastSteppedTick => World.Tick;

        public LockstepDriver(SimWorld world, ILockstepTransport transport, ReplayRecorder recorder = null)
        {
            World = world; Transport = transport; Recorder = recorder;
            InputDelay = world.Config.InputDelayTicks;
            nextSendTick = 0;
            // Prime the first InputDelay ticks with empty frames so the match can start.
            for (uint t = 0; t < (uint)InputDelay; t++) SendFrame(t);
            nextSendTick = (uint)InputDelay;
        }

        /// <summary>Queue a local command. It executes on every peer at (current tick + InputDelay).</summary>
        public void Issue(SimCommand command)
        {
            command.Player = (byte)Transport.LocalPlayer;
            pendingLocal.Add(command);
        }

        /// <summary>Try to advance one sim tick. Returns true if a tick was stepped.</summary>
        public bool TryStep()
        {
            // 1. send this tick's frame (commands scheduled InputDelay ahead)
            if (nextSendTick <= World.Tick + (uint)InputDelay) SendFrame(nextSendTick++);
            // 2. receive
            Transport.Pump((int)World.Tick);
            scratch.Clear();
            Transport.Receive(scratch);
            foreach (var f in scratch)
            {
                if (!frames.TryGetValue(f.Tick, out var byPlayer)) { byPlayer = new Dictionary<byte, CommandFrame>(); frames[f.Tick] = byPlayer; }
                byPlayer[f.Player] = f;
            }
            // 3. step if complete
            uint tick = World.Tick;
            if (!frames.TryGetValue(tick, out var set) || set.Count < Transport.PeerCount) { StallTicks++; return false; }
            StallTicks = 0;
            tickScratch.Clear();
            for (byte p = 0; p < Transport.PeerCount; p++)
                if (set.TryGetValue(p, out var fr) && fr.Commands != null) tickScratch.AddRange(fr.Commands);
            using (var arr = new NativeArray<SimCommand>(tickScratch.ToArray(), Allocator.Temp))
            {
                World.Step(arr);
                Recorder?.Record(arr, World.LastHash);
            }
            frames.Remove(tick);
            return true;
        }

        void SendFrame(uint tick)
        {
            var cmds = pendingLocal.ToArray();
            for (int i = 0; i < cmds.Length; i++) cmds[i].Tick = tick;
            pendingLocal.Clear();
            Transport.Send(new CommandFrame { Tick = tick, Player = (byte)Transport.LocalPlayer, Commands = cmds, HashOfPreviousTick = World.LastHash });
        }
    }
}
