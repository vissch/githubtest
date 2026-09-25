// Phase: A3 (implemented 2026-09-25, widened 2026-09-25 A5c) — the sim's own faction id.
// A faction is a roster and the off-map abilities it may call (FactionRoster). SimConfig.FactionA/B pick one per
// player, so a faction is part of the lockstep handshake and of the replay header — both already store it as a byte,
// which is why adding factions needs no format change.
//
// Two are the fiction the game shipped with: Iron (the Maw, the Pincer, the Banner) and Brass (the Tusk, the Kettle,
// the Redoubt). The four that follow are the historical armies the owner asked for; they stand beside the fiction
// rather than replacing it, which is why Iron and Brass keep ids 0 and 1 and every existing replay still reads.
// Named FactionId rather than Faction: TW.Data.Faction (the thirteen nations of the campaign data) already exists and
// TW.Editor references both assemblies. TW.Data.FactionMap folds a nation onto one of these.
namespace TW.Sim
{
    public enum FactionId : byte
    {
        Iron = 0, Brass = 1,
        British = 2, German = 3, French = 4, AustroHungarian = 5,
    }

    public static class Factions
    {
        public const int Count = 6;

        /// <summary>
        /// A byte off the wire or out of a replay header. Anything out of range is Iron rather than silently becoming
        /// the last faction in the enum: it used to be `id == 0 ? Iron : Brass`, which turned every unknown byte into
        /// Brass and would have made a corrupt handshake look like a legal one.
        /// </summary>
        public static FactionId Of(byte id) => id < Count ? (FactionId)id : FactionId.Iron;

        /// <summary>For logs, debriefs and the briefing screen; not a localised string.</summary>
        public static string Name(FactionId f)
        {
            switch (f)
            {
                case FactionId.Iron: return "Iron";
                case FactionId.Brass: return "Brass";
                case FactionId.British: return "British Empire";
                case FactionId.German: return "German Empire";
                case FactionId.French: return "French Republic";
                case FactionId.AustroHungarian: return "Austria-Hungary";
                default: return "Unknown";
            }
        }

        /// <summary>Whose side a faction is on, for mission authoring: Iron, Britain and France against the rest.</summary>
        public static bool IsEntente(FactionId f) => f == FactionId.Iron || f == FactionId.British || f == FactionId.French;
    }
}
