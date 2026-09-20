// Phase: P0 (implemented)
// Runs the match: two MatchSims (local player + a peer) stepped through LockstepDriver over LoopbackTransport
// so every play session exercises the lockstep path. Cross-checks the two sims' hashes each tick.
// N2 swaps the peer for a UtpTransport; A6 swaps the scripted peer for WaveAiSystem.
using UnityEngine;
using TW.Net;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Presentation
{
    [DefaultExecutionOrder(-100)]
    public sealed class SimHost : MonoBehaviour
    {
        [Header("Loopback network simulation")]
        public int LatencyTicks = 2;
        public int JitterTicks = 1;
        [Range(0f, 0.5f)] public float LossChance = 0.05f;

        [Header("Match")]
        public uint Seed = 0xC0FFEE;
        public int StartingSilver = 120;
        public float SilverPerSecond = 1f;
        public bool ScriptedPeer = true;
        public int PeerDeployEveryTicks = 40;
        [Tooltip("Stress preset: both players start with enough silver to field this many riflemen each, deployed at 4 per tick.")]
        public int StressUnits = 0;
        [Tooltip("Stress preset: send both garrisons over the top this many ticks after the last deployment.")]
        public int StressAdvanceDelayTicks = 300;
        int stressDeployed;
        uint stressAdvanceTick;
        bool stressAdvanced;

        public MatchSim Local { get; private set; }
        public MatchSim Peer { get; private set; }
        public LockstepDriver LocalDriver { get; private set; }
        public LockstepDriver PeerDriver { get; private set; }
        public SimPresenter Presenter { get; private set; }
        public EventPump Events { get; } = new EventPump();
        public bool Desync { get; private set; }
        public float Alpha { get; private set; }

        LoopbackNetwork net;
        float accumulator;
        float animTime;

        void Awake()
        {
            Application.runInBackground = true;   // lockstep must keep ticking when the window loses focus (editor included)
            var cfg = SimConfig.Default;
            cfg.Seed = Seed;
            cfg.StartingSilver = StressUnits > 0 ? StressUnits * 25 : StartingSilver;
            cfg.SilverPerSecond = SilverPerSecond;
            Local = MatchSim.CreateGreybox(cfg);
            Peer = MatchSim.CreateGreybox(cfg);
            net = new LoopbackNetwork(LatencyTicks, JitterTicks, LossChance, Seed);
            LocalDriver = new LockstepDriver(Local.World, net.A);
            PeerDriver = new LockstepDriver(Peer.World, net.B);
            Presenter = new SimPresenter(cfg.MaxSlots);
            Presenter.Capture(Local.World);
        }

        void Update()
        {
            float tick = Local.World.Config.TickSeconds;
            accumulator += Time.deltaTime;
            int guard = 8;
            while (accumulator >= tick && guard-- > 0)
            {
                IssuePeerCommands();
                bool a = LocalDriver.TryStep();
                bool b = PeerDriver.TryStep();
                if (a) { Presenter.Capture(Local.World); Events.Collect(Local.World); }
                if (a && b && Local.World.Tick == Peer.World.Tick && Local.World.LastHash != Peer.World.LastHash && !Desync)
                {
                    Desync = true;
                    Debug.LogError($"DESYNC at tick {Local.World.Tick}: local {Local.World.LastHash:X16} peer {Peer.World.LastHash:X16}");
                }
                if (a) accumulator -= tick; else break; // stalled: wait for frames without burning time
            }
            Alpha = Mathf.Clamp01(accumulator / tick);
            animTime += Time.deltaTime;
            Presenter.Interpolate(Alpha, animTime);
            Events.Dispatch();
        }

        void IssuePeerCommands()
        {
            if (!ScriptedPeer) return;
            uint t = Peer.World.Tick;
            if (t % (uint)PeerDeployEveryTicks == 0) PeerDriver.Issue(SimCommand.Deploy(t, 1, (int)(t / (uint)PeerDeployEveryTicks) % 3));
            if (StressUnits > 0)
            {
                if (stressDeployed < StressUnits)
                {
                    for (int k = 0; k < 4 && stressDeployed < StressUnits; k++, stressDeployed++)
                    {
                        PeerDriver.Issue(SimCommand.Deploy(t, 1, 0));
                        LocalDriver.Issue(SimCommand.Deploy(t, 0, 0));
                    }
                    stressAdvanceTick = t + (uint)StressAdvanceDelayTicks;
                }
                else if (!stressAdvanced && t >= stressAdvanceTick)
                {
                    stressAdvanced = true;
                    short front0 = Local.Fields.FrontTrench(0), front1 = Peer.Fields.FrontTrench(1);
                    if (front0 >= 0) LocalDriver.Issue(new SimCommand { Type = CommandType.TrenchAdvance, A = front0 });
                    if (front1 >= 0) PeerDriver.Issue(new SimCommand { Type = CommandType.TrenchAdvance, A = front1 });
                }
            }
        }

        /// <summary>Entry point for UI and debug input: queue a command for the local player.</summary>
        public void Issue(SimCommand c) => LocalDriver.Issue(c);

        void OnDestroy()
        {
            Presenter?.Dispose();
            Local?.Dispose();
            Peer?.Dispose();
        }
    }
}
