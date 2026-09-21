// Phase: A5 (implemented core) — depends on: MapData (trench / crater cells), Suppression. Armour top plates,
// wire breaching and light-vehicle flips are later A5 work.
// Anything that explodes queues an Impact; this system resolves the tick's impacts in one Burst job: damage falls
// from 100 % at the centre to 25 % at the edge of the radius, a man in a trench takes 35 % unless the shell landed
// in that trench, a man in a shell hole 60 %, prone or pinned 70 %, a vehicle 10 % (A5 replaces that with armour).
// Suppression is added on the same falloff. Craters are handed to DeformationSystem through Craters.
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;
using TW.Sim.Terrain;

namespace TW.Sim.Combat
{
    public struct Impact
    {
        public float3 Pos;
        public float Damage, Radius, Suppression, CraterRadius, CraterDepth;
        public int Source;   // ability or weapon id, for the Explosion event
        public int Player;   // who fired it (-1 none); friendly fire is on
    }

    public sealed class BlastSystem : ISimSystem
    {
        public int Order => SimSystemOrder.Blast;

        readonly MapData map;
        public NativeList<Impact> Pending;         // filled by abilities / indirect fire earlier in the same tick
        public NativeList<CraterStamp> Craters;    // drained by DeformationSystem later in the same tick
        public NativeList<Impact> Resolved;        // this tick's impacts, for what they do to props and wire; drained the same way
        NativeList<int> killed;

        public BlastSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            Pending = new NativeList<Impact>(32, Allocator.Persistent);
            Craters = new NativeList<CraterStamp>(32, Allocator.Persistent);
            Resolved = new NativeList<Impact>(32, Allocator.Persistent);
            killed = new NativeList<int>(64, Allocator.Persistent);
        }

        public void Queue(Impact impact) => Pending.Add(impact);

        public void Step(SimWorld w)
        {
            if (Pending.Length == 0) return;
            killed.Clear();
            new BlastJob
            {
                Count = w.HighWater, Impacts = Pending.AsArray(), Killed = killed,
                Position = w.Position, Flags = w.Flags, StanceOf = w.StanceOf, Hp = w.Hp, Suppression = w.Suppression,
                Layers = map.NavLayers, CellTrenchId = map.CellTrenchId, NavWidth = map.NavWidth, NavLength = map.NavLength,
            }.Run();
            for (int k = 0; k < Pending.Length; k++)
            {
                var im = Pending[k];
                w.Events.Add(w.Tick, SimEventType.Explosion, im.Source, im.Player, im.Pos, default, im.Radius);
                Resolved.Add(im);
                if (im.CraterRadius > 0f) Craters.Add(new CraterStamp { Center = im.Pos, Radius = im.CraterRadius, Depth = im.CraterDepth });
            }
            for (int k = 0; k < killed.Length; k++) w.Despawn(killed[k], -1, new float3(0f, 1f, 0f));
            Pending.Clear();
        }

        [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct BlastJob : IJob
        {
            public int Count, NavWidth, NavLength;
            [ReadOnly] public NativeArray<Impact> Impacts;
            [ReadOnly] public NativeArray<float3> Position;
            [ReadOnly] public NativeArray<uint> Flags;
            [ReadOnly] public NativeArray<byte> StanceOf, Layers;
            [ReadOnly] public NativeArray<short> CellTrenchId;
            public NativeArray<float> Hp, Suppression;
            public NativeList<int> Killed;

            int CellOf(float3 p)
            {
                int cx = math.clamp((int)(p.x / MapData.NavCellSize), 0, NavWidth - 1);
                int cz = math.clamp((int)(p.z / MapData.NavCellSize), 0, NavLength - 1);
                return cz * NavWidth + cx;
            }

            public void Execute()
            {
                for (int k = 0; k < Impacts.Length; k++)
                {
                    var im = Impacts[k];
                    short hitTrench = CellTrenchId[CellOf(im.Pos)];
                    for (int i = 0; i < Count; i++)
                    {
                        uint f = Flags[i];
                        if ((f & (uint)UnitFlags.Alive) == 0 || Hp[i] <= 0f) continue;
                        float3 d = Position[i] - im.Pos; d.y = 0f;
                        float dist = SimMath.Length(d);
                        if (dist >= im.Radius) continue;
                        float falloff = 1f - 0.75f * (dist / im.Radius);
                        int cell = CellOf(Position[i]);
                        float protection = 1f;
                        if ((f & (uint)UnitFlags.Vehicle) != 0) protection = 0.1f;
                        else if ((f & (uint)UnitFlags.InTrench) != 0) protection = hitTrench >= 0 && CellTrenchId[cell] == hitTrench ? 1f : 0.35f;
                        else if ((Layers[cell] & (byte)NavLayer.Crater) != 0) protection = 0.6f;
                        else if (StanceOf[i] == (byte)Stance.Prone || StanceOf[i] == (byte)Stance.Pinned) protection = 0.7f;
                        Hp[i] = Hp[i] - im.Damage * falloff * protection;
                        Suppression[i] = math.min(100f, Suppression[i] + im.Suppression * falloff);
                        if (Hp[i] <= 0f) Killed.Add(i);
                    }
                }
            }
        }

        public ulong Hash(ulong h) => h;   // Pending and Craters are empty between ticks; Hp and Suppression live in SimWorld

        public void Dispose()
        {
            if (Pending.IsCreated) Pending.Dispose();
            if (Craters.IsCreated) Craters.Dispose();
            if (Resolved.IsCreated) Resolved.Dispose();
            if (killed.IsCreated) killed.Dispose();
        }
    }
}
