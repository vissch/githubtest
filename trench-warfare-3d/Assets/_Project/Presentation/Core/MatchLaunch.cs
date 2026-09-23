// Phase: B6 (implemented) — what the mission select decided, carried across the scene load into SimHost.
// A scene load destroys every object but not a static, so the request waits here and SimHost.Awake applies it to
// its own public fields before it builds the match (MatchLaunch.Apply is the first line of Awake). With no request
// (pressing Play in GreyboxCorridor) nothing happens and the scene's serialized values stand. The request stays
// armed across SimHost.Restart() so a restart replays the same mission; Quit to menu clears it.
using UnityEngine;

namespace TW.Presentation
{
    public static class MatchLaunch
    {
        [System.Serializable]
        public sealed class Request
        {
            public string MissionId = "";
            public string Title = "";
            public string Difficulty = "";
            public uint MatchSeed = 0xC0FFEE;
            public int StartingSilver = 300;
            public float SilverPerSecond = 2f;
            public bool GeneratedBattlefield = true;
            public bool PlaytestMap = true;
            public uint BattlefieldSeed = 1917;
            public float Bombardment = 8f;
            public bool ScriptedPeer = true;
            public int PeerDeployEveryTicks = 40;
            public bool PeerAttacks = true;
            public int PeerAttackGarrison = 8;
            public bool PeerDeploysTanks = false;
            public bool PeerUsesSupport = true;
            public int PeerSupportReserve = 180;

            /// <summary>The scene's SimHost as it is, so a request can start from what the scene already says.</summary>
            public static Request From(SimHost h) => new Request
            {
                MatchSeed = h.Seed, StartingSilver = h.StartingSilver, SilverPerSecond = h.SilverPerSecond,
                GeneratedBattlefield = h.GeneratedBattlefield, PlaytestMap = h.PlaytestMap, BattlefieldSeed = h.BattlefieldSeed,
                Bombardment = h.BombardmentPerMinute, ScriptedPeer = h.ScriptedPeer, PeerDeployEveryTicks = h.PeerDeployEveryTicks,
                PeerAttacks = h.PeerAttacks, PeerAttackGarrison = h.PeerAttackGarrison, PeerDeploysTanks = h.PeerDeploysTanks,
                PeerUsesSupport = h.PeerUsesSupport, PeerSupportReserve = h.PeerSupportReserve,
            };
        }

        /// <summary>The request the next SimHost.Awake applies, or null for "whatever the scene says".</summary>
        public static Request Current;

        /// <summary>The request the running match was started from (kept for the debrief and for Restart).</summary>
        public static Request Running { get; private set; }

        public const string BattleScene = "GreyboxCorridor";
        public const string MenuScene = "MainMenu";

        /// <summary>Called first thing in SimHost.Awake. Writes the request's values into the host's public fields.</summary>
        public static void Apply(SimHost h)
        {
            var r = Current;
            if (r == null || h == null) { Running = null; return; }
            h.Seed = r.MatchSeed; h.StartingSilver = r.StartingSilver; h.SilverPerSecond = r.SilverPerSecond;
            h.GeneratedBattlefield = r.GeneratedBattlefield; h.PlaytestMap = r.PlaytestMap; h.BattlefieldSeed = r.BattlefieldSeed;
            h.BombardmentPerMinute = r.Bombardment;
            h.ScriptedPeer = r.ScriptedPeer; h.PeerDeployEveryTicks = Mathf.Max(1, r.PeerDeployEveryTicks);   // SimHost divides by it
            h.PeerAttacks = r.PeerAttacks; h.PeerAttackGarrison = r.PeerAttackGarrison; h.PeerDeploysTanks = r.PeerDeploysTanks;
            h.PeerUsesSupport = r.PeerUsesSupport; h.PeerSupportReserve = r.PeerSupportReserve;
            SimHost.BombardmentOverride = -1f;   // the debug presets never outrank a mission
            Running = r;
        }

        /// <summary>Arm a request and load the battle scene.</summary>
        public static void Start(Request r)
        {
            Current = r;
            UnityEngine.SceneManagement.SceneManager.LoadScene(BattleScene);
        }

        /// <summary>Drop the request and go back to the menu.</summary>
        public static void QuitToMenu()
        {
            Current = null; Running = null;
            SceneStatics.Reset();
            UnityEngine.SceneManagement.SceneManager.LoadScene(MenuScene);
        }
    }
}
