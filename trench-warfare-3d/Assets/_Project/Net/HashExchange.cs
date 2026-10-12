// Phase: N3 (stub) — depends on: CommandFrame.Hash / CommandFrame.HashTick, ReplayRecorder
// Compares each peer's reported hash for its HashTick against the local hash for that tick. On mismatch: freeze, dump both
// replays plus a SimWorld snapshot to disk, raise DesyncDetected for the UI.
namespace TW.Net
{
    public sealed class HashExchange
    {
        public bool DesyncDetected;
        public uint DesyncTick;
        public void Observe(in CommandFrame frame, ulong localHashForThatTick) => throw new System.NotImplementedException("Phase N3: HashExchange.Observe");
    }
}
