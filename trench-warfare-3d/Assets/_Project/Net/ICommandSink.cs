// Phase: tooling (perf pass, 2026-09-23) — something a player's commands can be issued to: a LockstepDriver (a seat with
// a world) or a CommandSeat (a seat without one). The scripted enemy issues to one without knowing which.
using TW.Sim;

namespace TW.Net
{
    public interface ICommandSink
    {
        int Player { get; }
        void Issue(SimCommand command);
    }
}
