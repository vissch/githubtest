// Phase: A3 (implemented) — the sea half of LogisticsSystem: reinforcements arrive instead of appearing.
// SimWorld.Deploy offers every paid-for unit to the lift first; the lift takes it aboard a craft standing off the
// beach and puts it ashore itself a few seconds later. A lift that refuses (no berth, no sea on that team's side)
// leaves the old instant spawn in place, so a player is never blocked by the boats.
namespace TW.Sim
{
    public interface ISeaLift
    {
        /// <summary>Takes a deployed unit aboard an inbound craft. False: it spawns at the player's spawn point as before.</summary>
        bool Embark(SimWorld world, byte player, RosterEntry entry);
    }
}
