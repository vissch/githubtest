// Phase: C2 (implemented 2026-09-25, widened A5c) — which sim faction a campaign nation fields.
// The campaign data names thirteen nations (Faction); the sim knows six rosters (FactionId). Four nations now have a
// roster of their own; the rest field the nearest ally until they get one, which is the same rule as before with more
// places to land. Iron and Brass stay the greybox factions and are nobody nation.
using TW.Sim;

namespace TW.Data
{
    public static class FactionMap
    {
        public static FactionId ToSim(Faction nation)
        {
            switch (nation)
            {
                case Faction.British: return FactionId.British;
                case Faction.German: return FactionId.German;
                case Faction.French: return FactionId.French;
                case Faction.AustroHungarian: return FactionId.AustroHungarian;
                // the other Central Powers fight as the German army until their own rosters land
                case Faction.Ottoman:
                case Faction.Bulgarian: return FactionId.German;
                // and the rest of the Entente as the British
                default: return FactionId.British;
            }
        }
    }
}
