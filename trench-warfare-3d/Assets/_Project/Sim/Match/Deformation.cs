// Phase: A4 (implemented) — depends on: CraterStamp, PropDef, WireBelt, BlastSystem.Craters/Resolved, FlowFieldManager.MarkCostDirty
// The one place that changes the battlefield during a match:
// - craters: at most 4 stamps a tick (the rest wait), each carves the heightfield, marks Crater cells and turns wet
//   where it went under the water table; emits CraterStamp for the terrain renderer;
// - props: every blast wears down the trees inside its radius (tree -> broken tree -> stump, PropChanged), and a
//   destroyed vehicle leaves a wreck that blocks its cell and gives cover (a wreck cannot stand in a trench, link,
//   bunker or blocked cell, so a tank that died ditched, astride a trench or against a tree leaves it in the nearest
//   cell within two that can hold it, or none);
// - wire: a cratering blast clears the wire inside its crater (WireBreached).
// Flow fields are marked stale whenever a nav cell changed. The map's mutable arrays are covered by MapData.Version,
// which FlowFieldManager hashes; this system hashes its queue and a running checksum of what it has applied.
using Unity.Collections;
using Unity.Mathematics;
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
        public int Applied, PropsChanged, WireOpened;
        ulong checksum = SimHash.Offset;

        public DeformationSystem(MapData map) { this.map = map; }

        /// <summary>The centre of the nearest nav cell within two of <paramref name="pos"/> that can hold a prop (not a
        /// trench, link, bunker or blocked cell); ties go to the lower cell index.</summary>
        bool NearestFreeCell(float3 pos, out float3 at)
        {
            var c = map.NavCellOf(pos);
            float best = float.MaxValue; at = pos;
            for (int dz = -2; dz <= 2; dz++)
                for (int dx = -2; dx <= 2; dx++)
                {
                    int x = c.x + dx, z = c.y + dz;
                    if (x < 0 || z < 0 || x >= map.NavWidth || z >= map.NavLength) continue;
                    int cell = map.NavIndex(x, z);
                    if (((NavLayer)map.NavLayers[cell] & (NavLayer.Trench | NavLayer.Link | NavLayer.Bunker | NavLayer.Blocked)) != 0) continue;
                    var centre = map.NavCellCenter(cell);
                    float d = math.distancesq(centre.xz, pos.xz);
                    if (d < best) { best = d; at = centre; }
                }
            return best < float.MaxValue;
        }

        public void Initialize(SimWorld world)
        {
            fields = world.GetSystem<FlowFieldManager>() ?? throw new System.InvalidOperationException("DeformationSystem needs FlowFieldManager registered before it");
            blast = world.GetSystem<BlastSystem>();
            Queue = new NativeList<CraterStamp>(64, Allocator.Persistent);
        }

        public void Step(SimWorld w)
        {
            bool navChanged = false;
            if (blast != null)
            {
                for (int i = 0; i < blast.Craters.Length; i++) Queue.Add(blast.Craters[i]);
                blast.Craters.Clear();
                for (int i = 0; i < blast.Resolved.Length; i++) navChanged |= Shake(w, blast.Resolved[i]);   // read, not drained: BlastSystem clears its own list at its next Step
            }
            // wrecks: this tick's vehicle deaths (events are appended in sim order, so this is deterministic). The
            // PropChanged carries the dead slot + 1 in dir.x, so the view can tie the hull it draws to this prop.
            var events = w.Events.Events;
            for (int i = 0, n0 = events.Length; i < n0; i++)
            {
                var e = events[i];
                if (e.Type != SimEventType.VehicleDestroyed) continue;
                float3 at = e.Pos;
                int index = map.AddProp(new PropDef { Pos = at, Yaw = e.Dir.y, Kind = PropKind.Wreck });
                if (index < 0 && NearestFreeCell(e.Pos, out at)) index = map.AddProp(new PropDef { Pos = at, Yaw = e.Dir.y, Kind = PropKind.Wreck });
                if (index < 0) continue;
                checksum = SimHash.Value(at, checksum);
                PropsChanged++; navChanged = true;
                w.Events.Add(w.Tick, SimEventType.PropChanged, index, (int)PropKind.Wreck, at, new float3(e.A + 1, 0f, 0f));
            }

            int n = Queue.Length < MaxStampsPerTick ? Queue.Length : MaxStampsPerTick;
            for (int i = 0; i < n; i++)
            {
                var stamp = Queue[i];
                if (stamp.ApplyDynamic(map) > 0) navChanged = true;   // merges into the hole it lands in, throws spoil up, spares the trenches
                checksum = SimHash.Value(stamp, checksum);
                Applied++;
                // a mound (a building's rubble) is the same event with a NEGATIVE depth: the terrain view rebuilds
                // its chunks either way, and anything that wants to tell a heap from a hole reads the sign
                float signed = stamp.Kind == (int)CraterKind.Mound ? -stamp.Depth : stamp.Depth;
                w.Events.Add(w.Tick, SimEventType.CraterStamp, 0, 0, stamp.Center, new float3(0f, signed, 0f), stamp.Radius);
            }
            if (n > 0)
            {
                for (int i = n; i < Queue.Length; i++) Queue[i - n] = Queue[i];   // keep the order of what still waits
                Queue.Length = Queue.Length - n;
            }
            if (navChanged) fields.MarkCostDirty(0);
        }

        /// <summary>What one blast does to props and wire. Returns true when a nav cell changed.</summary>
        bool Shake(SimWorld w, Impact im)
        {
            bool nav = false;
            for (int p = 0; p < map.Props.Length; p++)
            {
                var prop = map.Props[p];
                if (prop.Hp <= 0f) continue;   // stumps, logs, wrecks, bridges: nothing left to break
                float3 d = prop.Pos - im.Pos; d.y = 0f;
                float dist = SimMath.Length(d);
                if (dist >= im.Radius) continue;
                prop.Hp -= im.Damage * (1f - 0.75f * (dist / im.Radius));
                if (prop.Hp > 0f) { map.Props[p] = prop; map.Touch(); continue; }
                var next = PropRules.Next(prop.Kind);
                if (map.SetPropKind(p, next)) nav = true;
                checksum = SimHash.Value(p * 8 + (int)next, checksum);
                PropsChanged++;
                w.Events.Add(w.Tick, SimEventType.PropChanged, p, (int)next, prop.Pos);
            }
            if (im.CraterRadius > 0f)
            {
                int opened = WireBelt.Breach(map, im.Pos, im.CraterRadius);
                if (opened > 0)
                {
                    WireOpened += opened; nav = true;
                    checksum = SimHash.Value(opened, checksum);
                    w.Events.Add(w.Tick, SimEventType.WireBreached, 0, 0, im.Pos, default, im.CraterRadius * 2f);
                }
            }
            return nav;
        }

        public ulong Hash(ulong h)
        {
            h = SimHash.Value(Applied, h);
            h = SimHash.Value(PropsChanged, h);
            h = SimHash.Value(WireOpened, h);
            h = SimHash.Combine(h, checksum);
            for (int i = 0; i < Queue.Length; i++) h = SimHash.Value(Queue[i], h);
            return h;
        }

        public void Dispose() { if (Queue.IsCreated) Queue.Dispose(); }
    }
}
