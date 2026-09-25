// Phase: C2 (implemented 2026-09-25) — which of the sim's two factions a campaign nation fields.
// The campaign data names thirteen nations (Faction); the sim knows two rosters (FactionId). Until the nations get
// rosters of their own, the Entente fields Iron and the Central Powers field Brass.
using TW.Sim;

namespace TW.Data
{
    public static class FactionMap
    {
        public static FactionId ToSim(Faction nation)
        {
            switch (nation)
            {
                case Faction.German:
                case Faction.AustroHungarian:
                case Faction.Ottoman:
                case Faction.Bulgarian:
                    return FactionId.Brass;
                default:
                    return FactionId.Iron;
            }
        }
    }
}
