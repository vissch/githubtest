// Phase: N3 (stub) — depends on: CommandFrame.Hash / CommandFrame.HashTick, ReplayRecorder
// Compares each peer's reported hash for its HashTick against the local hash for that tick. On mismatch: freeze, dump both
// replays plus a SimWorld snapshot to disk, raise DesyncDetected for the UI.
// Two cases the comparison must handle, decided here (the comparison itself is still unwritten):
//  - frame.Hash == 0 means the frame carries no hash (see CommandFrame.Hash): skip it, there is nothing to compare
//    and it is never a desync.
//  - a peer whose world has SimWorld.HashInterval != 1 sends Hash 0 on every tick it skipped hashing, so the stream
//    of hashes from that peer is sparse. Keying the comparison on HashTick alone, without the Hash == 0 skip, would
//    compare a 0 against a real local hash and read as a desync on the very first skipped tick.
namespace TW.Net
{
    public sealed class HashExchange
    {
        public bool DesyncDetected;
        public uint DesyncTick;
        public void Observe(in CommandFrame frame, ulong localHashForThatTick)
        {
            if (frame.Hash == 0) return;   // no hash to compare; see CommandFrame.Hash
            throw new System.NotImplementedException("Phase N3: HashExchange.Observe");
        }
    }
}
