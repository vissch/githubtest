// Phase: N3 (stub) — full SimWorld + system state serialization for late join / reconnect / desync dumps.
using System.IO;
using TW.Sim;

namespace TW.Net
{
    public static class Snapshot
    {
        public const uint Magic = 0x31535754; // "TWS1"
        public static void Write(SimWorld world, BinaryWriter w) => throw new System.NotImplementedException("Phase N3: Snapshot.Write");
        public static void Read(SimWorld world, BinaryReader r) => throw new System.NotImplementedException("Phase N3: Snapshot.Read");
    }
}
