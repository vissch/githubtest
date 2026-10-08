// Phase: B6 (implemented) — what the mission select decided, carried across the scene load into SimHost.
// A scene load destroys every object but not a static, so the request waits here and SimHost.Awake applies it to
// its own public fields before it builds the match (MatchLaunch.Apply is the first line of Awake). With no request
// (pressing Play in GreyboxCorridor) nothing happens and the scene's serialized values stand. The request stays
// armed across SimHost.Restart() so a restart replays the same mission; Quit to menu clears it.
using UnityEngine;

namespace TW.Presentation
{
    /// <summary>
    /// Which battlefield a mission is fought on. ShelledForest is deliberately the ZERO value: every Request and
    /// every MissionCard asset serialized before this existed reads back as the map it was already using.
    ///
    /// This names the GROUND only. Which look goes with it is BiomeProfile.ForGround, over in Terrain, because
    /// TW.Sim cannot see Biome without closing a reference cycle (BattlefieldGenerator.cs:76 records why).
    /// </summary>
    public enum Ground
    {
        ShelledForest = 0,
        WinterLine = 1,
        Landing = 2,
        /// <summary>The Narrows: as wide as the wood, half as long, so the front trenches stand 50 m apart instead
        /// of 100. Appended, never inserted: the value is serialized in mission cards.</summary>
        Narrows = 3,
    }

    public static class MatchLaunch
    {
        /// <summary>The one place a Ground becomes a battlefield. A switch rather than a table so that adding a
        /// preset without choosing its ground is a compile error rather than a silent fall back to the wood.</summary>
        public static TW.Sim.Terrain.BattlefieldParams Field(Ground ground, uint seed)
        {
            switch (ground)
            {
                case Ground.WinterLine: return TW.Sim.Terrain.BattlefieldParams.WinterLine(seed);
                case Ground.Landing: return TW.Sim.Terrain.BattlefieldParams.Landing(seed);
                // The Narrows is built inline rather than as a fourth preset: a preset lives in TW.Sim, and this
                // field needs no sim change at all (docs/design/idea-the-narrows-a-field-half-as-deep.md, 5).
                // Half the wood's length at the same width, so only depth changes and the gap halves to 50 m.
                // No river (its half width never scales under 6 m: up to 17 m of water in a 50 m gap) and no sea.
                case Ground.Narrows: return new TW.Sim.Terrain.BattlefieldParams
                {
                    Seed = seed, Width = 90f, Length = 120f, Forest = 0.35f, Shelling = 0.7f, Mud = 0.5f,
                    WaterLevel = 0.15f, River = false, Wrecks = 1, Bombardment = 8f, Sea = false,
                };
                default: return TW.Sim.Terrain.BattlefieldParams.ShelledForest(seed);
            }
        }

        [System.Serializable]
        public sealed class Request
        {
            public string MissionId = "";
            public string Title = "";
            public string Difficulty = "";
            public uint MatchSeed = 0xC0FFEE;
            // ---- 2026-09-25: who is fighting, and with which ten (the briefing writes these; phase 2) ----
            public byte FactionA = (byte)TW.Sim.FactionId.Iron, FactionB = (byte)TW.Sim.FactionId.Brass;
            /// <summary>The ten each side brings, or null/empty for its faction's default ten.</summary>
            public byte[] LoadoutA, LoadoutB;
            public int StartingSilver = 300;
            public float SilverPerSecond = 2f;
            public bool GeneratedBattlefield = true;
            public bool PlaytestMap = true;
            public uint BattlefieldSeed = 1917;
            /// <summary>Which ground. Zero is the shelled wood, so an old saved request is unchanged.</summary>
            public Ground Ground = TW.Presentation.Ground.ShelledForest;
            public float Bombardment;   // carried, not applied: no ambient bombardment (SimHost.BombardmentPerMinute)
            public bool ScriptedPeer = true;
            public int PeerDeployEveryTicks = 40;
            public bool PeerAttacks = true;
            public int PeerAttackGarrison = 8;
            public bool PeerDeploysTanks = false;
            public bool PeerUsesSupport = true;
            public int PeerSupportReserve = 180;
            public bool PeerDefends;
            /// <summary>Campaign (docs/21 phase 6): which support abilities each side may fire (a bit per OffMapAbilityId;
            /// 0 = the faction's default). The sides' factions are FactionA/FactionB above, the same ids (FactionId: Iron 0,
            /// Brass 1) the campaign's FactionBuildings uses. The sim's upgrade seam reads the masks when it lands (B1);
            /// until then Apply leaves them for the HUD.</summary>
            /// <summary>Seat A is the local player by fiat: the HUD's cards, the incoming markers and the mine marks all read
            /// seat 0; B is the scripted peer today, so AbilityMaskB is built and unread until a second human sits in B.</summary>
            public uint AbilityMaskA, AbilityMaskB;
            /// <summary>The Proving Ground (2026-09-28): the test level. The shell opens its panel over the match (units,
            /// waves, match) and the ten of each side may name any unit the match's table defines.</summary>
            public bool ProvingGround;
            /// <summary>SimConfig.Endless: holding every objective names no winner, so a tester's waves never end the match.</summary>
            public bool Endless;
            /// <summary>Does a mask field an ability: bit (int)id, or everything when the mask is 0 (a skirmish, a test). The one
            /// copy: the HUD's cards and keys, the debug panel and (B1, later) the sim ask this.</summary>
            public static bool Offered(uint mask, int ability) => mask == 0 || (mask & (1u << ability)) != 0;

            /// <summary>The scene's SimHost as it is, so a request can start from what the scene already says.</summary>
            public static Request From(SimHost h) => new Request
            {
                MatchSeed = h.Seed, StartingSilver = h.StartingSilver, SilverPerSecond = h.SilverPerSecond,
                FactionA = h.FactionA, FactionB = h.FactionB, LoadoutA = h.LoadoutA, LoadoutB = h.LoadoutB,
                GeneratedBattlefield = h.GeneratedBattlefield, PlaytestMap = h.PlaytestMap, BattlefieldSeed = h.BattlefieldSeed,
                Ground = h.Ground,
                Bombardment = h.BombardmentPerMinute, ScriptedPeer = h.ScriptedPeer, PeerDeployEveryTicks = h.PeerDeployEveryTicks,
                PeerAttacks = h.PeerAttacks, PeerAttackGarrison = h.PeerAttackGarrison, PeerDeploysTanks = h.PeerDeploysTanks,
                PeerUsesSupport = h.PeerUsesSupport, PeerSupportReserve = h.PeerSupportReserve, PeerDefends = h.PeerDefends,
                Endless = h.Endless,
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
            h.FactionA = r.FactionA; h.FactionB = r.FactionB; h.LoadoutA = r.LoadoutA; h.LoadoutB = r.LoadoutB;
            h.GeneratedBattlefield = r.GeneratedBattlefield; h.PlaytestMap = r.PlaytestMap; h.BattlefieldSeed = r.BattlefieldSeed;
            h.Ground = r.Ground;
            h.BombardmentPerMinute = 0f;   // owner 2026-09-28: no constant bombardment; a mission's rate is not applied
            h.ScriptedPeer = r.ScriptedPeer; h.PeerDeployEveryTicks = Mathf.Max(1, r.PeerDeployEveryTicks);   // SimHost divides by it
            h.PeerAttacks = r.PeerAttacks; h.PeerAttackGarrison = r.PeerAttackGarrison; h.PeerDeploysTanks = r.PeerDeploysTanks;
            h.PeerUsesSupport = r.PeerUsesSupport; h.PeerSupportReserve = r.PeerSupportReserve; h.PeerDefends = r.PeerDefends;
            h.Endless = r.Endless;
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
            Current = null; Running = null; CampaignSession.Clear();
            SceneStatics.Reset();
            UnityEngine.SceneManagement.SceneManager.LoadScene(MenuScene);
        }
    }
}
