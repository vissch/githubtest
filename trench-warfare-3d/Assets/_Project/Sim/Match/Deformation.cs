// Phase: A4 (implemented) — depends on: CraterStamp, PropDef, WireBelt, BlastSystem.Craters/Resolved, FlowFieldManager.MarkCostDirty
// The one place that changes the battlefield during a match:
// - craters: at most 4 stamps a tick (the rest wait), each carves the heightfield, marks Crater cells and turns wet
//   where it went under the water table; emits CraterStamp for the terrain renderer;
// - props: every blast wears down the trees inside its radius (tree -> broken tree -> stump, PropChanged), and a
//   destroyed vehicle leaves a wreck that blocks its cell and gives cover (a wreck cannot stand in a trench, link,
//   bunker or blocked cell, so a tank that died ditched, astride a trench or against a tree leaves it in the nearest
//   cell within two that can hold it, or none); a wreck is sized to its machine and blasts wear it down a stage at a
//   time (wreck -> broken wreck -> scrap -> cleared, owner 2026-09-28), PropWorn for a hit it stands;
// - wire: a cratering blast clears the wire inside its crater (WireBreached).
// Flow fields are marked stale whenever a nav cell changed. The map's mutable arrays are covered by MapData.Version,
// which FlowFieldManager hashes; this system hashes its queue and a running checksum of what it has applied.
// Wreck provenance (2026-09-25, for the salvage report and, later, the auctions): every wreck it leaves is also a
// WreckRecord — whose machine it was, which side's, how it died, what is left of it (VehicleModulesSystem.QualityOf,
// read here in the same tick as VehicleDestroyed while the slot's modules are still true) and which prop it became.
// The list is hashed; the debrief reads it at the end and asks SectorControlSystem.ObjectiveAt who holds the ground.
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Combat;
using TW.Sim.Nav;
using TW.Sim.Terrain;
using TW.Sim.Units;

namespace TW.Sim.Match
{
    /// <summary>A vehicle that became a wreck. Every field is 4 bytes wide, so it hashes without padding.</summary>
    public struct WreckRecord
    {
        public int Slot;          // the slot it died in (re-used since: Generation tells the lives apart)
        public int Generation;
        public int Archetype, Team, Cause, Killer, PropIndex;
        public float Quality;     // 0..1, VehicleModulesSystem.QualityOf at the moment it died
        public float3 Pos;
        public uint Tick;
    }

    public sealed class DeformationSystem : ISimSystem
    {
        public const int MaxStampsPerTick = 4;
        public int Order => SimSystemOrder.Deformation;
        readonly MapData map;
        FlowFieldManager fields;
        BlastSystem blast;
        public NativeList<CraterStamp> Queue;
        public NativeList<WreckRecord> Wrecks;
        VehicleModulesSystem modules;
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
            Wrecks = new NativeList<WreckRecord>(16, Allocator.Persistent);
            Queue = new NativeList<CraterStamp>(64, Allocator.Persistent);
        }

        public void Step(SimWorld w)
        {
            bool navChanged = false;
            if (modules == null) modules = w.GetSystem<VehicleModulesSystem>();   // registered after this system: resolve on the first tick
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
                // its size class (PropDef.Scale) from the machine's hull in the match's unit table, not the slot's MaxHp: a
                // tougher machine leaves a tougher wreck, and every stage keeps the size (MapData.SetPropKind)
                byte kindOf = w.Archetype[e.A];
                float size = PropRules.WreckSize(kindOf < Archetypes.Count ? w.Units.Roster[kindOf].Hp : PropRules.WreckSizeHp);
                int index = map.AddProp(new PropDef { Pos = at, Yaw = e.Dir.y, Kind = PropKind.Wreck, Scale = size });
                if (index < 0 && NearestFreeCell(e.Pos, out at)) index = map.AddProp(new PropDef { Pos = at, Yaw = e.Dir.y, Kind = PropKind.Wreck, Scale = size });
                if (index < 0) continue;
                checksum = SimHash.Value(at, checksum);
                PropsChanged++; navChanged = true;
                w.Events.Add(w.Tick, SimEventType.PropChanged, index, (int)PropKind.Wreck, at, new float3(e.A + 1, 0f, 0f));
                if (Wrecks.IsCreated)
                {
                    int slot = e.A;
                    byte cause = modules != null && modules.KillCause.IsCreated && slot < modules.KillCause.Length ? modules.KillCause[slot] : (byte)VehicleKillCause.Structure;
                    float quality = modules != null ? modules.QualityOf(w, slot) : 0.5f;
                    var rec = new WreckRecord
                    {
                        Slot = slot, Generation = w.Generation[slot], Archetype = w.Archetype[slot], Team = w.Team[slot],
                        Cause = cause == VehicleModulesSystem.Alive ? (int)VehicleKillCause.Structure : cause, Killer = e.B, PropIndex = index,
                        Quality = quality, Pos = at, Tick = w.Tick,
                    };
                    Wrecks.Add(rec);
                    w.Events.Add(w.Tick, SimEventType.WreckRecorded, Wrecks.Length - 1, index, at, new float3(rec.Archetype, rec.Team, rec.Cause), quality);
                }
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
                if (prop.Hp <= 0f) continue;   // stumps, logs, bridges, a cleared wreck: nothing left to break
                bool wreck = PropRules.IsWreckage(prop.Kind);
                float3 d = prop.Pos - im.Pos; d.y = 0f;
                float dist = SimMath.Length(d);
                // a burst on a wreck's deck is on the wreck, not WreckBlastReach metres off its middle
                if (wreck) dist = math.max(0f, dist - PropRules.WreckBlastReach * (prop.Scale > 0f ? prop.Scale : 1f));
                if (dist >= im.Radius) continue;
                if (wreck && im.Shape == (int)BlastShape.CookOff && MadeLastTick(w, p)) continue;   // a cook-off does not wear the wreck it made
                prop.Hp -= im.Damage * (1f - 0.75f * (dist / im.Radius));
                if (prop.Hp > 0f)
                {
                    map.Props[p] = prop; map.Touch();
                    if (wreck)
                    {
                        float3 way = math.normalizesafe(d);
                        w.Events.Add(w.Tick, SimEventType.PropWorn, p, 0, prop.Pos, way, prop.Hp / PropRules.StartHp(prop.Kind, prop.Scale));
                    }
                    continue;
                }
                var next = PropRules.Next(prop.Kind);   // one stage a hit, however hard: a wreck goes wreck, broken, scrap, gone
                if (map.SetPropKind(p, next)) nav = true;
                checksum = SimHash.Value(new int2(p, (int)next), checksum);
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

        /// <summary>Prop p is the wreck of a machine that died last tick. A cook-off queues its burst as the machine dies,
        /// and BlastSystem resolves it the tick after, where the wreck already stands: this spares the wreck its own
        /// ammunition (and, the same tick, a newborn wreck beside another cook-off: derived from hashed state, no new state).</summary>
        bool MadeLastTick(SimWorld w, int p)
        {
            if (!Wrecks.IsCreated) return false;
            for (int k = Wrecks.Length - 1; k >= 0 && Wrecks[k].Tick + 1u >= w.Tick; k--)
                if (Wrecks[k].PropIndex == p && Wrecks[k].Tick + 1u == w.Tick) return true;
            return false;
        }

        public ulong Hash(ulong h)
        {
            if (Wrecks.IsCreated) { h = SimHash.Value(Wrecks.Length, h); for (int i = 0; i < Wrecks.Length; i++) h = SimHash.Value(Wrecks[i], h); }
            h = SimHash.Value(Applied, h);
            h = SimHash.Value(PropsChanged, h);
            h = SimHash.Value(WireOpened, h);
            h = SimHash.Combine(h, checksum);
            for (int i = 0; i < Queue.Length; i++) h = SimHash.Value(Queue[i], h);
            return h;
        }

        public void Dispose() { if (Queue.IsCreated) Queue.Dispose(); if (Wrecks.IsCreated) Wrecks.Dispose(); }
    }
}
