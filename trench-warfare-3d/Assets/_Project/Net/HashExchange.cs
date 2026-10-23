// Phase: N3 (stub) — depends on: CommandFrame.Hash / CommandFrame.HashTick, ReplayRecorder
// Compares each peer's reported hash for its HashTick against the local hash for that tick. On mismatch: freeze, dump both
// replays plus a SimWorld snapshot to disk, raise DesyncDetected for the UI.
// Two cases the comparison must handle, decided here (the comparison itself is still unwritten):
//  - !frame.HasHash (HashTick == CommandFrame.NoHash) means the frame carries no hash: skip it. Hash == 0 is not
//    that signal. A hashed tick may hash to 0, and skipping every 0 would hide a real mismatch.
//  - a peer whose world has SimWorld.HashInterval != 1 sends NoHash on every tick it skipped hashing, so the stream
//    of hashes from that peer is sparse. Keying the comparison on HashTick alone, without the HasHash skip, would
//    compare a missing hash against a real local hash and read as a desync on the very first skipped tick.
namespace TW.Net
{
    public sealed class HashExchange
    {
        public bool DesyncDetected;
        public uint DesyncTick;
        public void Observe(in CommandFrame frame, ulong localHashForThatTick)
        {
            if (!frame.HasHash) return;   // no hash to compare; Hash == 0 can be a real hash
            throw new System.NotImplementedException("Phase N3: HashExchange.Observe");
        }
    }
}
