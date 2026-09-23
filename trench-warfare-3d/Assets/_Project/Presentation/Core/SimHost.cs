// Phase: P0 (implemented)
// Runs the match through the lockstep path (LockstepSession): the player's world stepped by a LockstepDriver, the
// scripted enemy's orders sent by a seat on the other end of a loopback transport. Single player runs ONE world
// (owner, 2026-09-23); DeterminismCanary (or -twCanary, or CanaryOverride in tests) runs the old shape instead, a
// second world for the enemy seat over a lossy loopback with every tick's hash compared, at twice the sim cost.
// N2 swaps the loopback for a UtpTransport and a real peer; A6 swaps the scripted enemy for WaveAiSystem.
using UnityEngine;
using TW.Net;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Presentation
{
    [DefaultExecutionOrder(-100)]
    public sealed class SimHost : MonoBehaviour
    {
        [Header("Determinism canary (dev): a second world for the enemy seat, every tick's hash compared")]
        [Tooltip("Run the enemy seat on a world of its own and compare hashes every tick. Twice the sim cost; the gate's PlayMode run turns it on.")]
        public bool DeterminismCanary = false;
        /// <summary>Tests (PlayMode gate): forces the canary on or off for every SimHost, whatever the scene says.</summary>
        public static bool? CanaryOverride;
        [Tooltip("Canary only: the loopback's simulated latency, jitter and loss between the two worlds.")]
        public int LatencyTicks = 2;
        public int JitterTicks = 1;
        [Range(0f, 0.5f)] public float LossChance = 0.05f;

        [Header("Match")]
        public uint Seed = 0xC0FFEE;
        public int StartingSilver = 300;
        public float SilverPerSecond = 2f;
        [Tooltip("Sim time multiplier for testing: 0 pauses, 1 is real time. Both sims step together, so it never affects determinism.")]
        public float TimeScale = 1f;
        [Tooltip("Two trench lines a side and 200 m of no man's land. Off = the 800 m M1 corridor.")]
        public bool PlaytestMap = true;
        [Tooltip("Play on a generated battlefield (shelled wood, river, mud) instead of the flat playtest map.")]
        public bool GeneratedBattlefield = true;
        public uint BattlefieldSeed = 1917;
        [Tooltip("Shells per minute that fall on no man's land all match long. 0 = quiet.")]
        public float BombardmentPerMinute = 8f;
        /// <summary>Set by the debug panel before a restart; survives the scene reload. Negative = use the field above.</summary>
        public static float BombardmentOverride = -1f;
        /// <summary>Tooling (TW.Perf.PerfBench): riflemen a side for the stress preset, set before the scene loads so a
        /// player build can run it; negative = the field below. Read once, in Awake.</summary>
        public static int StressOverride = -1;
        public float BombardmentNow => Local != null && Local.Bombardment != null ? Local.Bombardment.ShellsPerMinute : 0f;
        public bool ScriptedPeer = true;
        public int PeerDeployEveryTicks = 40;
        [Tooltip("Scripted peer sends its front trench over the top once the garrison reaches PeerAttackGarrison.")]
        public bool PeerAttacks = true;
        [Tooltip("The scripted enemy sends its tank (roster slot 4) whenever the slot is ready and it has the silver.")]
        public bool PeerDeploysTanks = false;
        public int PeerAttackGarrison = 8;
        [Tooltip("Scripted peer shells or gasses your front trench when it can afford it and you have men there.")]
        public bool PeerUsesSupport = true;
        public int PeerSupportReserve = 180;
        [Tooltip("Stress preset: both players start with enough silver to field this many riflemen each, deployed at 4 per tick.")]
        public int StressUnits = 0;
        [Tooltip("Stress preset: send both garrisons over the top this many ticks after the last deployment.")]
        public int StressAdvanceDelayTicks = 300;

        public MatchSim Local { get; private set; }
        /// <summary>The canary's second world. NULL in single player: tooling that wrote "both worlds" uses WriteWorlds.</summary>
        public MatchSim Peer { get; private set; }
        public LockstepDriver LocalDriver { get; private set; }
        /// <summary>The canary's peer driver; null in single player.</summary>
        public LockstepDriver PeerDriver { get; private set; }
        /// <summary>The world the enemy seat reads: its own in the canary, the player's otherwise (same state, same tick).</summary>
        public MatchSim EnemyView => session?.EnemyView;
        public bool CanaryActive => session != null && session.Canary;
        public SimPresenter Presenter { get; private set; }
        /// <summary>The character controller's decision layer (docs/15): what each man's body is doing, from the sim's state and events.</summary>
        public AnimationController Animation { get; private set; }
        [Tooltip("Drive the men's animation rows from the character controller instead of the stance switch.")]
        public bool UseAnimationController = true;
        public EventPump Events { get; } = new EventPump();
        public bool Desync => session != null && session.Desync;
        public float Alpha { get; private set; }

        LockstepSession session;
        readonly ScriptedEnemy enemy = new ScriptedEnemy();
        float accumulator;
        float animTime;

        MatchSim NewMatch(SimConfig cfg)
        {
            if (GeneratedBattlefield)
            {
                var field = TW.Sim.Terrain.BattlefieldParams.ShelledForest(BattlefieldSeed);
                field.Bombardment = BombardmentOverride >= 0f ? BombardmentOverride : BombardmentPerMinute;
                return MatchSim.CreateBattlefield(cfg, field);
            }
            return PlaytestMap ? MatchSim.CreatePlaytest(cfg) : MatchSim.CreateGreybox(cfg);
        }

        void Awake()
        {
            MatchLaunch.Apply(this);   // a mission chosen in the shell writes its knobs into the fields below before the match is built
            if (StressOverride >= 0) StressUnits = StressOverride;
            Application.runInBackground = true;   // lockstep must keep ticking when the window loses focus (editor included)
            var cfg = SimConfig.Default;
            cfg.Seed = Seed;
            cfg.StartingSilver = StressUnits > 0 ? StressUnits * 25 : StartingSilver;
            cfg.SilverPerSecond = SilverPerSecond;
            bool canary = CanaryOverride ?? (DeterminismCanary || System.Array.IndexOf(System.Environment.GetCommandLineArgs(), "-twCanary") >= 0);
            session = new LockstepSession(() => NewMatch(cfg), canary, LatencyTicks, JitterTicks, LossChance, Seed);
            Local = session.Local; Peer = session.Peer; LocalDriver = session.LocalDriver; PeerDriver = session.PeerDriver;
            Presenter = new SimPresenter(cfg.MaxSlots);
            Animation = new AnimationController(cfg.MaxSlots, Local.Map, Local.Gas, Presenter.RowIn, Presenter.PhaseIn, Presenter.YawIn);
            Presenter.Capture(Local.World);
        }

        void Update()
        {
            using var update = PerfMarkers.HostUpdate.Auto();
            float tick = Local.World.Config.TickSeconds;
            accumulator += Time.deltaTime * Mathf.Max(0f, TimeScale);
            int guard = Mathf.Max(8, Mathf.CeilToInt(TimeScale * 2f));
            while (accumulator >= tick && guard-- > 0)
            {
                SyncEnemy();
                bool a = session.StepOnce(enemy);
                if (a) LocalStepped();
                if (a) accumulator -= tick; else break; // stalled: wait for frames without burning time
            }
            Alpha = Mathf.Clamp01(accumulator / tick);
            animTime += Time.deltaTime;
            PerfMarkers.AnimAdvance.Begin(); Animation.Advance(Time.deltaTime * Mathf.Max(0f, TimeScale)); PerfMarkers.AnimAdvance.End();
            Presenter.UseController = UseAnimationController;
            PerfMarkers.PresentInterpolate.Begin(); Presenter.Interpolate(Alpha, animTime); PerfMarkers.PresentInterpolate.End();
            Events.Dispatch();
        }

        /// <summary>Tooling (PerfBench): forget sim time the host still owes. After a fast-forward the accumulator can
        /// hold seconds of backlog, and a measurement taken while it drains is a measurement of catch-up frames.</summary>
        public void DropBacklog() => accumulator = 0f;

        /// <summary>Tooling (TankCapture): between frames the two lockstep worlds can be a tick apart, so a change made
        /// to both at once would land on different ticks and desync them. This steps the one behind until they match;
        /// false when it is waiting on the network.</summary>
        public bool AlignWorlds() => session.AlignWorlds(LocalStepped);

        /// <summary>Tooling: write the same change into every world the match has (one in single player, two in the
        /// canary), aligned to the same tick first. False, with nothing written, while the canary waits on the network.
        /// This replaces writing `h.Local.World...` and `h.Peer.World...` by hand, which throws when there is no peer.</summary>
        public bool WriteWorlds(System.Action<MatchSim> write)
        {
            if (!AlignWorlds()) return false;
            write(Local);
            if (Peer != null) write(Peer);
            return true;
        }

        /// <summary>What the presentation does with every tick the player's world steps.</summary>
        void LocalStepped()
        {
            PerfMarkers.PresentCapture.Begin(); Presenter.Capture(Local.World); PerfMarkers.PresentCapture.End();
            PerfMarkers.AnimTick.Begin(); Animation.Tick(Local.World); PerfMarkers.AnimTick.End();
            PerfMarkers.EventsCollect.Begin(); Events.Collect(Local.World); PerfMarkers.EventsCollect.End();
        }

        /// <summary>The scripted enemy's knobs are the serialized fields above (the test panel flips them at runtime).</summary>
        void SyncEnemy()
        {
            enemy.Enabled = ScriptedPeer; enemy.DeployEveryTicks = PeerDeployEveryTicks; enemy.Attacks = PeerAttacks;
            enemy.DeploysTanks = PeerDeploysTanks; enemy.AttackGarrison = PeerAttackGarrison; enemy.UsesSupport = PeerUsesSupport;
            enemy.SupportReserve = PeerSupportReserve; enemy.StressUnits = StressUnits; enemy.StressAdvanceDelayTicks = StressAdvanceDelayTicks;
        }

        /// <summary>Entry point for UI and debug input: queue a command for the local player.</summary>
        public void Issue(SimCommand c) => LocalDriver.Issue(c);

        /// <summary>Test panel only: queue a command as the enemy seat (player 1), so the greybox can stage an enemy assault.</summary>
        public void IssuePeer(SimCommand c) => session.Enemy.Issue(c);

        /// <summary>Reload the active scene: fresh sims, same seed.</summary>
        public void Restart() => UnityEngine.SceneManagement.SceneManager.LoadScene(gameObject.scene.buildIndex >= 0 ? gameObject.scene.buildIndex : 0);

        void OnDestroy()
        {
            Presenter?.Dispose();
            Animation?.Dispose();
            session?.Dispose();
        }
    }
}
