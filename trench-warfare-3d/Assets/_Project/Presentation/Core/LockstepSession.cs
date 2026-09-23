// Phase: tooling (perf pass, 2026-09-23) — the match's worlds, seats and tick loop, as a plain class a test can drive.
// Two shapes (owner decision, 2026-09-23: "one world in single player, the two-world check kept as a dev canary"):
//   single player (default)  one MatchSim; the player's LockstepDriver and the enemy's CommandSeat on a zero-lag
//                            loopback; the world hashes nothing, because nobody reads a hash (SimWorld.HashInterval 0).
//   determinism canary       two MatchSims over a loopback with simulated latency, jitter and loss, every tick hashed
//                            and compared: what every single-player session paid for until now, at twice the sim cost.
// Both play the same match: the enemy's orders reach the player's world on the same ticks either way
// (SinglePlayerEquivalenceTests holds all three shapes hash-identical tick for tick). Online play later is the
// canary's shape with a real transport and a real peer, no seat and no script.
using System;
using UnityEngine;
using TW.Net;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Presentation
{
    public sealed class LockstepSession : IDisposable
    {
        public readonly MatchSim Local;
        /// <summary>The canary's second world; null in single player.</summary>
        public readonly MatchSim Peer;
        public readonly LockstepDriver LocalDriver;
        /// <summary>The canary's peer driver; null in single player.</summary>
        public readonly LockstepDriver PeerDriver;
        /// <summary>Single player: the enemy's seat, which sends orders and owns no world. Null in the canary.</summary>
        public readonly CommandSeat EnemySeat;
        public readonly LoopbackNetwork Net;

        public bool Canary => Peer != null;
        /// <summary>The world the enemy reads: its own in the canary, the player's (same state, same tick) otherwise.</summary>
        public MatchSim EnemyView => Peer ?? Local;
        public ICommandSink Enemy => PeerDriver != null ? PeerDriver : (ICommandSink)EnemySeat;
        public bool Desync { get; private set; }
        public uint DesyncTick { get; private set; }

        // every tick's hash from each world, compared whenever the other has the same tick: the old check compared only
        // when both worlds happened to sit on the same tick at the end of a pass, so under latency it skipped ticks
        const int Ring = 128;
        readonly ulong[] localHash = new ulong[Ring], peerHash = new ulong[Ring];
        readonly uint[] localAt = new uint[Ring], peerAt = new uint[Ring];

        public LockstepSession(Func<MatchSim> newMatch, bool canary, int latencyTicks, int jitterTicks, float lossChance, uint seed)
        {
            Local = newMatch();
            for (int k = 0; k < Ring; k++) localAt[k] = peerAt[k] = uint.MaxValue;
            if (canary)
            {
                Peer = newMatch();
                Net = new LoopbackNetwork(latencyTicks, jitterTicks, lossChance, seed);
                LocalDriver = new LockstepDriver(Local.World, Net.A);
                PeerDriver = new LockstepDriver(Peer.World, Net.B);
            }
            else
            {
                Net = new LoopbackNetwork(0, 0, 0f, seed);
                LocalDriver = new LockstepDriver(Local.World, Net.A);
                EnemySeat = new CommandSeat(Net.B, Local.World.Config.InputDelayTicks);
                Local.World.HashInterval = 0;
            }
        }

        /// <summary>One pass of the host's tick loop: the enemy thinks, its seat sends, each world tries to step.
        /// True when the player's world stepped a tick.</summary>
        public bool StepOnce(ScriptedEnemy ai)
        {
            PerfMarkers.HostEnemyAi.Begin();
            ai?.Think(EnemyView, Local, Enemy, LocalDriver);
            PerfMarkers.HostEnemyAi.End();
            EnemySeat?.Pump(Local.World.Tick);
            PerfMarkers.HostStepLocal.Begin(); bool a = LocalDriver.TryStep(); PerfMarkers.HostStepLocal.End();
            if (PeerDriver == null) return a;
            PerfMarkers.HostStepPeer.Begin(); bool b = PeerDriver.TryStep(); PerfMarkers.HostStepPeer.End();
            if (a) Compare(true);
            if (b) Compare(false);
            return a;
        }

        /// <summary>Tooling: in the canary the two worlds can be a tick apart between passes, so a change written to both
        /// would land on different ticks and desync them. Steps the one behind until they match; false while it waits
        /// on the network. `onLocalStep` runs after each step of the player's world (the host's presentation). Single
        /// player has one world and is always aligned.</summary>
        public bool AlignWorlds(Action onLocalStep)
        {
            if (Peer == null) return true;
            for (int k = 0; k < 8 && Local.World.Tick != Peer.World.Tick; k++)
            {
                if (Local.World.Tick < Peer.World.Tick)
                {
                    if (!LocalDriver.TryStep()) return false;
                    Compare(true);
                    onLocalStep?.Invoke();
                }
                else
                {
                    if (!PeerDriver.TryStep()) return false;
                    Compare(false);
                }
            }
            return Local.World.Tick == Peer.World.Tick;
        }

        void Compare(bool local)
        {
            var w = local ? Local.World : Peer.World;
            uint tick = w.Tick;   // the tick just completed: LastHash is the state after it
            int k = (int)(tick % Ring);
            var mine = local ? localHash : peerHash; var mineAt = local ? localAt : peerAt;
            var theirs = local ? peerHash : localHash; var theirsAt = local ? peerAt : localAt;
            mine[k] = w.LastHash; mineAt[k] = tick;
            if (theirsAt[k] != tick || theirs[k] == w.LastHash || Desync) return;
            Desync = true; DesyncTick = tick;
            ulong l = local ? w.LastHash : theirs[k], p = local ? theirs[k] : w.LastHash;
            Debug.LogError($"DESYNC at tick {tick}: local {l:X16} peer {p:X16}");
        }

        public void Dispose()
        {
            Local?.Dispose();
            Peer?.Dispose();
        }
    }
}
