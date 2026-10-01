// Phase: tooling (2026-09-27) — one centre banner at a time, and which event may take it over.
// Two HUDs draw the banner: the Toolkit HUD's ObjectiveTracker, and CombatFx's IMGUI line while the old HUD is on
// (HudBridge.UseToolkitHud false). Both ask this rule, so a stream of ability banners cannot wipe "Trench 2 lost" or the
// match's end off the screen: a banner replaces the one showing only when it ranks at least as high, or that one is done.
using TW.Sim;

namespace TW.Presentation
{
    public static class BannerRules
    {
        public const int None = 0, OwnAbility = 1, EnemyAbility = 2, Trench = 3, MatchEnd = 4;

        /// <summary>How much a sim event's banner matters: the match's end over a trench changing hands over the enemy's
        /// support coming in over your own; 0 for an event that raises no banner.</summary>
        public static int Rank(SimEventType type, bool mine)
        {
            switch (type)
            {
                case SimEventType.MatchEnded: return MatchEnd;
                case SimEventType.TrenchCaptured: return Trench;
                case SimEventType.AbilityFired: return mine ? OwnAbility : EnemyAbility;
                case SimEventType.CommandRejected: return mine ? OwnAbility : None;   // your own refused order; the enemy's is none of your business
                default: return None;
            }
        }

        /// <summary>Whether a new banner of a rank takes the plate from the one showing (its rank, and whether it is still up).</summary>
        public static bool Replaces(int newRank, int showingRank, bool showingUp) => newRank > None && (!showingUp || newRank >= showingRank);
    }
}
