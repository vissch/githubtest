// Phase: B6 / docs/21 phase 6 (implemented) — the campaign mission in flight: which node and which of its missions the
// staging screen launched, and whether the debrief has paid for it. Carried across the scene load like
// MatchLaunch.Current (not reset by SceneStatics: the router resets those on every scene load, which is exactly when
// this must survive); cleared when the player leaves the match (MatchLaunch.QuitToMenu) or by Clear.
namespace TW.Presentation
{
    public static class CampaignSession
    {
        public static string NodeId = "";
        public static int MissionIndex = -1;
        public static byte Faction;
        /// <summary>The debrief has recorded the win and paid its gold; a restart or a re-bind does not pay again.</summary>
        public static bool Awarded;
        /// <summary>What the debrief paid, so a re-bind shows the same figure.</summary>
        public static int LastAward;
        /// <summary>The debrief's CONTINUE asked for the map: the main menu pushes it when it binds after the scene
        /// load. Survives Clear (which runs on the way to the menu) and is consumed by the menu.</summary>
        public static bool ResumeMap;

        public static bool Active => !string.IsNullOrEmpty(NodeId) && MissionIndex >= 0;

        public static void Begin(string nodeId, int missionIndex, byte faction)
        {
            NodeId = nodeId ?? ""; MissionIndex = missionIndex; Faction = faction; Awarded = false; LastAward = 0;
        }

        public static void Clear() { NodeId = ""; MissionIndex = -1; Faction = 0; Awarded = false; LastAward = 0; }
    }
}
