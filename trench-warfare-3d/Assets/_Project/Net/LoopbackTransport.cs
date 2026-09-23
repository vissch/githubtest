// Phase: N1 (implemented)
// Two in-process peers sharing a pair of queues, with deterministic simulated latency, jitter and loss so the
// lockstep driver can be tested without a network. Loss only delays (re-sends on the next pump) because lockstep
// cannot tolerate a lost frame; a real transport (N2) uses reliable delivery for frames.
using System.Collections.Generic;
using Unity.Mathematics;

namespace TW.Net
{
    public sealed class LoopbackNetwork
    {
        public readonly LoopbackTransport A, B;
        public int LatencyTicks;
        public int JitterTicks;
        public float LossChance;
        Random rng;

        public LoopbackNetwork(int latencyTicks = 2, int jitterTicks = 0, float lossChance = 0f, uint seed = 1)
        {
            LatencyTicks = latencyTicks; JitterTicks = jitterTicks; LossChance = lossChance;
            rng = new Random(seed == 0 ? 1u : seed);
            A = new LoopbackTransport(this, 0);
            B = new LoopbackTransport(this, 1);
        }

        internal int DeliveryDelay()
        {
            int d = LatencyTicks;
            if (JitterTicks > 0) d += rng.NextInt(0, JitterTicks + 1);
            if (LossChance > 0f && rng.NextFloat() < LossChance) d += LatencyTicks + 1; // "lost" = retransmitted later
            return math.max(0, d);
        }
    }

    public sealed class LoopbackTransport : ILockstepTransport
    {
        struct InFlight { public int DeliverAtTick; public CommandFrame Frame; }

        readonly LoopbackNetwork net;
        readonly int player;
        readonly List<InFlight> inbox = new List<InFlight>();
        int now;

        internal LoopbackTransport(LoopbackNetwork net, int player) { this.net = net; this.player = player; }

        public int LocalPlayer => player;
        public int PeerCount => 2;

        public void Send(in CommandFrame frame)
        {
            var other = player == 0 ? net.B : net.A;
            // the local copy is delivered instantly; the remote copy after the simulated delay
            inbox.Add(new InFlight { DeliverAtTick = now, Frame = frame });
            other.inbox.Add(new InFlight { DeliverAtTick = other.now + net.DeliveryDelay(), Frame = frame });
        }

        public void Receive(List<CommandFrame> into)
        {
            for (int i = inbox.Count - 1; i >= 0; i--)
            {
                if (inbox[i].DeliverAtTick <= now) { into.Add(inbox[i].Frame); inbox.RemoveAt(i); }
            }
            // keep arrival order stable for the driver (it sorts by tick/player anyway). A static comparer, and no sort
            // for one frame: List.Sort(Comparison) wrapped the lambda in a new comparer on every call.
            if (into.Count > 1) into.Sort(ByTickThenPlayer.Instance);
        }

        sealed class ByTickThenPlayer : IComparer<CommandFrame>
        {
            public static readonly ByTickThenPlayer Instance = new ByTickThenPlayer();
            public int Compare(CommandFrame x, CommandFrame y) => x.Tick != y.Tick ? x.Tick.CompareTo(y.Tick) : x.Player.CompareTo(y.Player);
        }

        /// <summary>Delivery delay is counted in pump calls (driver updates), not sim ticks, so a stalled peer still receives frames.</summary>
        public void Pump(int simTick) { now++; }
    }
}
