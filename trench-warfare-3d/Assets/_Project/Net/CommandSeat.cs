// Phase: tooling (perf pass, 2026-09-23) — a lockstep player that sends commands but steps no world of its own.
// Single player used to run the whole match twice: the player's world, and a second full world for the scripted
// enemy's seat, hashed and compared every tick. That doubled the sim (measured: TW.Host.StepLocal 11.0 + StepPeer
// 10.3 ms a tick at 1,500 a side) to check something the gate checks anyway. A seat is what is left of the peer when
// its world is taken away: it sends player 1's frames on the transport, on the same ticks a peer driver would, so the
// player's world receives exactly the command stream it did before and steps identically. The enemy's script reads
// the player's own world, which is the same state at the same tick. The two-world check survives as SimHost's
// determinism canary; online play later puts a real peer on the other end of a real transport and needs no seat.
using System.Collections.Generic;
using TW.Sim;

namespace TW.Net
{
    public sealed class CommandSeat : ICommandSink
    {
        public readonly ILockstepTransport Transport;
        public readonly int InputDelay;
        readonly List<SimCommand> pending = new List<SimCommand>();
        readonly List<CommandFrame> drain = new List<CommandFrame>();
        uint nextSendTick;

        public int Player => Transport.LocalPlayer;

        public CommandSeat(ILockstepTransport transport, int inputDelay)
        {
            Transport = transport; InputDelay = inputDelay;
            // like LockstepDriver: the first InputDelay ticks get empty frames so the match can start
            for (uint t = 0; t < (uint)inputDelay; t++) Send(t);
            nextSendTick = (uint)inputDelay;
        }

        /// <summary>Queue a command. It executes at (the world's current tick + InputDelay), as a driver's would.</summary>
        public void Issue(SimCommand command)
        {
            command.Player = (byte)Transport.LocalPlayer;
            pending.Add(command);
        }

        /// <summary>Call once per host pass, BEFORE the world's driver tries to step `worldTick`: sends this seat's frame
        /// for worldTick + InputDelay (what a peer driver's TryStep sends at that tick) and drains the frames the
        /// transport hands the seat, which it has no world to apply to.</summary>
        public void Pump(uint worldTick)
        {
            while (nextSendTick <= worldTick + (uint)InputDelay) Send(nextSendTick++);
            Transport.Pump((int)worldTick);
            drain.Clear();
            Transport.Receive(drain);
            drain.Clear();
        }

        void Send(uint tick)
        {
            var cmds = pending.Count == 0 ? System.Array.Empty<SimCommand>() : pending.ToArray();
            for (int i = 0; i < cmds.Length; i++) cmds[i].Tick = tick;
            pending.Clear();
            Transport.Send(new CommandFrame { Tick = tick, Player = (byte)Transport.LocalPlayer, Commands = cmds, HashOfPreviousTick = 0 });
        }
    }
}
