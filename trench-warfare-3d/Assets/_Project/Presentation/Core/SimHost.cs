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
        public float BombardmentNow => Local != null && Local.Bombardment != null ? Local.Bombardment.ShellsPerMinute : 0f;
        public bool ScriptedPeer = true;
        public int PeerDeployEveryTicks = 40;
        [Tooltip("Scripted peer sends its front trench over the top once the garrison reaches PeerAttackGarrison.")]
        public bool PeerAttacks = true;
        public int PeerAttackGarrison = 8;
        [Tooltip("Scripted peer shells or gasses your front trench when it can afford it and you have men there.")]
        public bool PeerUsesSupport = true;
        public int PeerSupportReserve = 180;
        int peerSupportCount;
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
            Application.runInBackground = true;   // lockstep must keep ticking when the window loses focus (editor included)
            var cfg = SimConfig.Default;
            cfg.Seed = Seed;
            cfg.StartingSilver = StressUnits > 0 ? StressUnits * 25 : StartingSilver;
            cfg.SilverPerSecond = SilverPerSecond;
            Local = NewMatch(cfg);
            Peer = NewMatch(cfg);
            net = new LoopbackNetwork(LatencyTicks, JitterTicks, LossChance, Seed);
            LocalDriver = new LockstepDriver(Local.World, net.A);
            PeerDriver = new LockstepDriver(Peer.World, net.B);
            Presenter = new SimPresenter(cfg.MaxSlots);
            Presenter.Capture(Local.World);
        }

        void Update()
        {
            float tick = Local.World.Config.TickSeconds;
            accumulator += Time.deltaTime * Mathf.Max(0f, TimeScale);
            int guard = Mathf.Max(8, Mathf.CeilToInt(TimeScale * 2f));
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
            if (t % (uint)PeerDeployEveryTicks == 0)
            {
                // keep a reserve for support fire once the first squad is out; silver is the only brake on the script
                int slot = (int)(t / (uint)PeerDeployEveryTicks) % 3;
                int cost = Peer.World.Roster[RosterEntry.SlotCount + slot].Cost;
                int reserve = PeerUsesSupport && Peer.World.AliveCount > 0 && t > 600 ? PeerSupportReserve : 0;
                if (Peer.World.Silver[1] >= cost + reserve) PeerDriver.Issue(SimCommand.Deploy(t, 1, slot));
            }
            if (t % 100 == 20)
            {
                // every trench behind the front is locked so reinforcements walk through to the front line
                short front = Peer.Fields.FrontTrench(1);
                for (int k = 0; k < Peer.Fields.Trenches.Length; k++)
                {
                    var ts = Peer.Fields.Trenches[k];
                    if (ts.OwnerTeam != 1) continue;
                    byte want = (byte)(k != front ? 1 : 0);
                    if (ts.Locked != want) PeerDriver.Issue(new SimCommand { Tick = t, Player = 1, Type = CommandType.TrenchLock, A = k, B = want });
                    if (k != front && ts.GarrisonCount > 0) PeerDriver.Issue(new SimCommand { Tick = t, Player = 1, Type = CommandType.TrenchAdvance, A = k });
                }
            }
            if (PeerAttacks && t % 100 == 50)
            {
                short front = Peer.Fields.FrontTrench(1);
                if (front >= 0 && Peer.Fields.Trenches[front].GarrisonCount >= PeerAttackGarrison)
                    PeerDriver.Issue(new SimCommand { Tick = t, Player = 1, Type = CommandType.TrenchAdvance, A = front });
            }
            if (PeerUsesSupport && t % 200 == 150 && Peer.Abilities != null)
            {
                short mine = Peer.Fields.FrontTrench(0);
                var ability = (peerSupportCount & 1) == 0 ? OffMapAbilityId.HeBarrage : OffMapAbilityId.ChlorineGas;
                if (mine >= 0 && Peer.Fields.Trenches[mine].GarrisonCount >= 6 && Peer.Abilities.CooldownOf(1, ability) == 0
                    && OffMapAbilitySystem.TryGetStats((int)ability, out var stats) && Peer.World.Silver[1] >= stats.Cost + 20)
                {
                    var pw = Peer.World;
                    Vector3 sum = Vector3.zero; int n = 0;
                    for (int i = 0; i < pw.HighWater; i++)
                        if (pw.IsAlive(i) && pw.TrenchId[i] == mine) { sum += (Vector3)pw.Position[i]; n++; }
                    if (n > 0)
                    {
                        sum /= n;
                        // gas is released upwind (the map wind blows toward -Z) so the cloud rolls over the trench
                        float dz = ability == OffMapAbilityId.ChlorineGas ? 12f : 0f;
                        PeerDriver.Issue(new SimCommand { Tick = t, Player = 1, Type = CommandType.SupportFire, A = (int)ability, Pos = new Unity.Mathematics.float3(sum.x, 0f, sum.z + dz) });
                        peerSupportCount++;
                    }
                }
            }
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

        /// <summary>Test panel only: queue a command as the peer (player 1), so the greybox can stage an enemy assault.</summary>
        public void IssuePeer(SimCommand c) => PeerDriver.Issue(c);

        /// <summary>Reload the active scene: fresh sims, same seed.</summary>
        public void Restart() => UnityEngine.SceneManagement.SceneManager.LoadScene(gameObject.scene.buildIndex >= 0 ? gameObject.scene.buildIndex : 0);

        void OnDestroy()
        {
            Presenter?.Dispose();
            Local?.Dispose();
            Peer?.Dispose();
        }
    }
}
