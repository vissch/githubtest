// Phase: wrecks (2026-09-28, implemented) — part of DirectFireSystem — depends on: PropHarm, PropRules, MapData.CellCover
// Sustained fire wears a wreck (owner, 2026-09-28): a machine gun's round that a man's cover stopped (FireJob: the same
// roll, a miss that would have hit with no cover) is recorded with its target's cell and the prop's share of the cover.
// After the job, in the order they were fired, each goes to the wreck that gives that cell its cover (the wreckage
// within one cell of it with the most cover; ties to the lower index) through PropHarm: PropWorn b = 1 while the stage
// stands, the next stage when its hit points run out. A cell whose cover is a tree's wears nothing (trees do not wear
// under gunfire, plan default). Rifles do not wear wrecks (CombatTables.WearsWrecks: a machine gun's rate of fire).
// The props' hit points are in MapData's hash, so this system still hashes nothing of its own.
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Sim.Combat
{
    /// <summary>A round a prop's cover stopped: the target's nav cell, what it would have done to the wreck, which way it went.</summary>
    public struct WreckRound
    {
        public int Cell;
        public float Damage;
        public float3 Way;
    }

    public sealed partial class DirectFireSystem
    {
        NativeList<WreckRound> stopped;
        FlowFieldManager fields;
        ulong wearChecksum = SimHash.Offset;   // PropHarm folds stage changes here; the map's hash already has them
        /// <summary>Rounds that wore a wreck since the match started. Derived from hashed state, not hashed itself.</summary>
        public int WreckRounds;

        /// <summary>This tick's stopped rounds, each on the wreck giving its cell cover, in the order they were fired.</summary>
        void WearWrecks(SimWorld w)
        {
            if (stopped.Length == 0) return;
            bool nav = false;
            for (int k = 0; k < stopped.Length; k++)
            {
                var round = stopped[k];
                int p = WreckCovering(round.Cell);
                if (p < 0) continue;
                var harm = PropHarm.Harm(w, map, p, round.Damage, round.Way, 1, ref wearChecksum, out bool changed);
                if (harm == PropHarm.Outcome.None) continue;
                WreckRounds++;
                nav |= changed;
            }
            if (nav)
            {
                if (fields == null) fields = w.GetSystem<FlowFieldManager>();
                fields?.MarkCostDirty(0);
            }
        }

        /// <summary>The wreckage prop that gives nav cell `cell` its cover: within one cell of it (MapData.StampCover's
        /// reach), the most cover, the lower index on a tie; -1 when the cover there is a tree's or nothing's.</summary>
        int WreckCovering(int cell)
        {
            int cx = cell % map.NavWidth, cz = cell / map.NavWidth;
            int best = -1; byte bestCover = 0, anyCover = 0;
            for (int i = 0; i < map.Props.Length; i++)
            {
                var prop = map.Props[i];
                int px = prop.Cell % map.NavWidth, pz = prop.Cell / map.NavWidth;
                if (math.abs(px - cx) > 1 || math.abs(pz - cz) > 1) continue;
                byte c = PropRules.CoverPercent(prop.Kind);
                if (c > anyCover) anyCover = c;
                if (!PropRules.IsWreckage(prop.Kind) || prop.Hp <= 0f || c <= bestCover) continue;
                best = i; bestCover = c;
            }
            return best >= 0 && bestCover >= anyCover ? best : -1;   // a tree's more cover there took it
        }
    }
}
