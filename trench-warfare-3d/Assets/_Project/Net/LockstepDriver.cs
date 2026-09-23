// Phase: N1 (implemented)
// Deterministic lockstep: local commands issued at tick T are scheduled for T + InputDelay and sent as a frame.
// The driver steps tick T only once it holds a frame from every peer for T. Every peer therefore executes an
// identical command set per tick; hashes are recorded per tick for desync detection (compared in N3).
// Allocation (perf pass, 2026-09-23): a tick used to allocate a Dictionary for each new tick's frames and two arrays
// (ToArray of the tick's commands, ToArray of the pending ones even when there were none). The per-tick
// dictionaries are pooled, the tick's commands go straight into the Temp NativeArray the sim steps on, and an
// empty frame shares Array.Empty. Same commands, same order, same hashes.
using System;
using System.Collections.Generic;
using Unity.Collections;
using TW.Sim;

namespace TW.Net
{
    public sealed class LockstepDriver : ICommandSink
    {
        public readonly SimWorld World;
        public readonly ILockstepTransport Transport;
        public readonly int InputDelay;
        public readonly ReplayRecorder Recorder;   // optional

        readonly List<SimCommand> pendingLocal = new List<SimCommand>();
        readonly Dictionary<uint, Dictionary<byte, CommandFrame>> frames = new Dictionary<uint, Dictionary<byte, CommandFrame>>();
        readonly Stack<Dictionary<byte, CommandFrame>> spare = new Stack<Dictionary<byte, CommandFrame>>();
        readonly List<CommandFrame> scratch = new List<CommandFrame>();
        uint nextSendTick;
        public int StallTicks;         // ticks we could not step because a peer frame was missing (UI shows a stall after N)
        public uint LastSteppedTick => World.Tick;
        public int Player => Transport.LocalPlayer;

        public LockstepDriver(SimWorld world, ILockstepTransport transport, ReplayRecorder recorder = null)
        {
            if (recorder != null && world.HashInterval != 1)
                throw new ArgumentException("a replay records every tick's hash: its world must hash every tick (SimWorld.HashInterval = 1)");
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
            for (int i = 0; i < scratch.Count; i++)
            {
                var f = scratch[i];
                if (!frames.TryGetValue(f.Tick, out var byPlayer))
                {
                    byPlayer = spare.Count > 0 ? spare.Pop() : new Dictionary<byte, CommandFrame>();
                    frames[f.Tick] = byPlayer;
                }
                byPlayer[f.Player] = f;
            }
            // 3. step if complete
            uint tick = World.Tick;
            if (!frames.TryGetValue(tick, out var set) || set.Count < Transport.PeerCount) { StallTicks++; return false; }
            StallTicks = 0;
            int n = 0;
            for (byte p = 0; p < Transport.PeerCount; p++)
                if (set.TryGetValue(p, out var fr) && fr.Commands != null) n += fr.Commands.Length;
            var arr = new NativeArray<SimCommand>(n, Allocator.Temp, NativeArrayOptions.UninitializedMemory);
            try
            {
                int k = 0;
                for (byte p = 0; p < Transport.PeerCount; p++)
                    if (set.TryGetValue(p, out var fr) && fr.Commands != null)
                        for (int c = 0; c < fr.Commands.Length; c++) arr[k++] = fr.Commands[c];
                World.Step(arr);
                Recorder?.Record(arr, World.LastHash);
            }
            finally { arr.Dispose(); }
            frames.Remove(tick);
            set.Clear();
            spare.Push(set);
            return true;
        }

        void SendFrame(uint tick)
        {
            var cmds = pendingLocal.Count == 0 ? Array.Empty<SimCommand>() : pendingLocal.ToArray();
            for (int i = 0; i < cmds.Length; i++) cmds[i].Tick = tick;
            pendingLocal.Clear();
            Transport.Send(new CommandFrame { Tick = tick, Player = (byte)Transport.LocalPlayer, Commands = cmds, HashOfPreviousTick = World.LastHash });
        }
    }
}
