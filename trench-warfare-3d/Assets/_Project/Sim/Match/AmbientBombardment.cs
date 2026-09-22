// Phase: A5 (implemented) — depends on: BlastSystem.Queue, SimRandom, MapData trench lines
// The front is never quiet: shells that belong to nobody keep falling between the two front lines, at a rate set
// for the match (BattlefieldParams.Bombardment, shells per minute; 0 = off). One in seven drops short or long and
// can land on a trench line. Everything a shell does goes through BlastSystem and DeformationSystem like any other
// shell: men die, craters open and fill, trees break, wire is cut. Deterministic: position, size and the gap to the
// next shell come from SimRandom on (seed, tick).
using Unity.Mathematics;
using TW.Sim.Combat;
using TW.Sim.Terrain;

namespace TW.Sim.Match
{
    public sealed class AmbientBombardmentSystem : ISimSystem
    {
        public int Order => SimSystemOrder.AmbientBombardment;
        public float ShellsPerMinute;
        public int Fired;
        readonly MapData map;
        BlastSystem blast;
        uint nextTick;
        float bandMin, bandMax, reachMin, reachMax;

        public AmbientBombardmentSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            blast = world.GetSystem<BlastSystem>() ?? throw new System.InvalidOperationException("AmbientBombardmentSystem needs BlastSystem registered before it");
            // no man's land lies between the forward-most trench of each side; stray shells reach the rear-most
            float front0 = 0f, front1 = map.SizeMeters.y, rear0 = map.SizeMeters.y, rear1 = 0f;
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                var def = map.Trenches[t];
                if (def.CellCount == 0) continue;
                float z = map.NavCellCenter(map.TrenchCells[def.CellStart]).z;
                if (def.OwnerTeam == 0) { front0 = math.max(front0, z); rear0 = math.min(rear0, z); }
                else { front1 = math.min(front1, z); rear1 = math.max(rear1, z); }
            }
            bandMin = front0 + 14f; bandMax = front1 - 14f;
            reachMin = math.min(rear0, bandMin); reachMax = math.max(rear1, bandMax);
            nextTick = (uint)(world.Config.TickRate * 8);   // the first shell after eight seconds
        }

        public void Step(SimWorld w)
        {
            if (ShellsPerMinute <= 0f || bandMax <= bandMin || w.Tick < nextTick || w.WinnerTeam >= 0) return;
            blast.Queue(Roll(w, w.Tick, out float gapTicks));
            Fired++;
            nextTick = w.Tick + (uint)math.max(1f, gapTicks);
        }

        /// <summary>The shell Step fires at <paramref name="tick"/>: its impact, and the gap to the one after.</summary>
        Impact Roll(SimWorld w, uint tick, out float gapTicks)
        {
            var rng = SimRandom.For(w.Config.Seed, tick, SimRandom.SystemId.IndirectFire, 7777u);
            bool stray = rng.NextFloat() < 1f / 7f;
            float z = stray ? rng.NextFloat(reachMin, reachMax) : rng.NextFloat(bandMin, bandMax);
            float x = rng.NextFloat(4f, map.SizeMeters.x - 4f);
            float crater = rng.NextFloat(2f, 4.4f);
            gapTicks = w.Config.TickRate * 60f / ShellsPerMinute * rng.NextFloat(0.35f, 1.65f);
            return new Impact
            {
                Pos = new float3(x, 0f, z), Damage = 150f, Radius = 6f + crater, Suppression = 50f,
                CraterRadius = crater, CraterDepth = crater * 0.36f, Source = (int)OffMapAbilityId.HeBarrage, Player = -1,
            };
        }

        /// <summary>
        /// The next stray shell, read ahead without changing anything (the presentation lets the men hear it coming):
        /// the tick it lands, where, and its radius. False when none is due.
        /// </summary>
        public bool Upcoming(SimWorld w, out uint tick, out float3 pos, out float radius)
        {
            tick = nextTick; pos = default; radius = 0f;
            if (ShellsPerMinute <= 0f || bandMax <= bandMin || w.WinnerTeam >= 0) return false;
            if (tick < w.Tick) tick = w.Tick;   // due now (Step fires it on the next tick it runs)
            var im = Roll(w, tick, out _);
            pos = im.Pos; radius = im.Radius;
            return true;
        }

        public ulong Hash(ulong h)
        {
            h = SimHash.Value(nextTick, h);
            return SimHash.Value(Fired, h);
        }

        public void Dispose() { }
    }
}
