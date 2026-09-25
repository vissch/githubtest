// Phase: A3 (implemented 2026-09-25) — the sim's own faction id.
// A faction is the side's roster and the off-map abilities it may call (FactionRoster). Two exist: Iron, today's
// player-0 side (the Maw, the Pincer, the Banner), and Brass, today's player-1 side (the Tusk, the Kettle, the
// Redoubt). SimConfig.FactionA/B pick one per player, so it is part of the lockstep handshake and the replay header.
// Named FactionId rather than Faction: TW.Data.Faction (the thirteen nations of the campaign data) already exists
// and TW.Editor references both assemblies. TW.Data.FactionMap folds a nation onto one of these two.
namespace TW.Sim
{
    public enum FactionId : byte { Iron = 0, Brass = 1 }

    public static class Factions
    {
        public const int Count = 2;
        public static FactionId Of(byte id) => id == 0 ? FactionId.Iron : FactionId.Brass;
    }
}
