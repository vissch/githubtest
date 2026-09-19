// Phase: A4 (stub) — depends on: CraterStamp, WireBelt, MudField, FlowFieldManager.MarkCostDirty
// Applies queued CraterStamps (≤ 4 per tick), updates NavCost locally, emits CraterStamp events for the terrain
// renderer, and folds MapData's mutable arrays into the hash.
using Unity.Collections;
using TW.Sim.Terrain;

namespace TW.Sim.Match
{
    public sealed class DeformationSystem : ISimSystem
    {
        public int Order => SimSystemOrder.Deformation;
        readonly MapData map;
        public NativeList<CraterStamp> Queue;
        public DeformationSystem(MapData map) { this.map = map; }
        public void Initialize(SimWorld world) { Queue = new NativeList<CraterStamp>(64, Allocator.Persistent); }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A4: DeformationSystem.Step");
        public ulong Hash(ulong h) => map.Hash(h);
        public void Dispose() { if (Queue.IsCreated) Queue.Dispose(); }
    }
}
