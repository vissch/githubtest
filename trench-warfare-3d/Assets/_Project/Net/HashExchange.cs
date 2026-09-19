// Phase: N3 (stub) — depends on: CommandFrame.HashOfPreviousTick, ReplayRecorder
// Compares each peer's reported hash for tick T-1 against the local hash. On mismatch: freeze, dump both
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
