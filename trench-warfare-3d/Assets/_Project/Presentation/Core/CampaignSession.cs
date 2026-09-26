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

        public static bool Active => !string.IsNullOrEmpty(NodeId) && MissionIndex >= 0;

        public static void Begin(string nodeId, int missionIndex, byte faction)
        {
            NodeId = nodeId ?? ""; MissionIndex = missionIndex; Faction = faction; Awarded = false;
        }

        public static void Clear() { NodeId = ""; MissionIndex = -1; Faction = 0; Awarded = false; }
    }
}
