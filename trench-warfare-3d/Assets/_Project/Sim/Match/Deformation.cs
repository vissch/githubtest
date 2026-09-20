// Phase: A4 (implemented core) — depends on: CraterStamp, BlastSystem.Craters, FlowFieldManager.MarkCostDirty
// Collects the craters the tick produced, applies at most 4 per tick (the rest wait their turn), emits CraterStamp
// events for the terrain renderer and marks the flow fields stale when a nav cell changed layer. The map's mutable
// arrays are covered by MapData.Version, which FlowFieldManager hashes; this system hashes its queue and a running
// checksum of what it has applied. Wire belts and mud fields are later A4 work.
using Unity.Collections;
using TW.Sim.Combat;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Sim.Match
{
    public sealed class DeformationSystem : ISimSystem
    {
        public const int MaxStampsPerTick = 4;
        public int Order => SimSystemOrder.Deformation;
        readonly MapData map;
        FlowFieldManager fields;
        BlastSystem blast;
        public NativeList<CraterStamp> Queue;
        public int Applied;
        ulong checksum = SimHash.Offset;

        public DeformationSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            fields = world.GetSystem<FlowFieldManager>() ?? throw new System.InvalidOperationException("DeformationSystem needs FlowFieldManager registered before it");
            blast = world.GetSystem<BlastSystem>();
            Queue = new NativeList<CraterStamp>(64, Allocator.Persistent);
        }

        public void Step(SimWorld w)
        {
            if (blast != null && blast.Craters.Length > 0)
            {
                for (int i = 0; i < blast.Craters.Length; i++) Queue.Add(blast.Craters[i]);
                blast.Craters.Clear();
            }
            int n = Queue.Length < MaxStampsPerTick ? Queue.Length : MaxStampsPerTick;
            if (n == 0) return;
            bool navChanged = false;
            for (int i = 0; i < n; i++)
            {
                var stamp = Queue[i];
                if (stamp.Apply(map) > 0) navChanged = true;
                checksum = SimHash.Value(stamp, checksum);
                Applied++;
                w.Events.Add(w.Tick, SimEventType.CraterStamp, 0, 0, stamp.Center, new Unity.Mathematics.float3(0f, stamp.Depth, 0f), stamp.Radius);
            }
            for (int i = n; i < Queue.Length; i++) Queue[i - n] = Queue[i];   // keep the order of what still waits
            Queue.Length = Queue.Length - n;
            if (navChanged) fields.MarkCostDirty(0);
        }

        public ulong Hash(ulong h)
        {
            h = SimHash.Value(Applied, h);
            h = SimHash.Combine(h, checksum);
            for (int i = 0; i < Queue.Length; i++) h = SimHash.Value(Queue[i], h);
            return h;
        }

        public void Dispose() { if (Queue.IsCreated) Queue.Dispose(); }
    }
}
