// Phase: B2 (implemented) — what lies outside the fought-over ground (owner, 2026-09-22: "i want more decoration on
// the edges of the map, i want the trenches to continue past the playble scene as well as the environment, the main
// clusters of decorations should be on the edges of the map to gate off the zone, these should be placed
// prgramaticlly").
//
// Three jobs, none of which touches the simulation — every piece here is drawn and nothing else:
//   1. The lines run on. Each fire trench is continued off both flanks: parapet, revetment and spoil marching away
//      with the same wander the generator gave the trench, thinning with distance until it is a line of dots in the
//      haze. A front does not stop at the edge of a level.
//   2. The edges are closed. A belt of wire, knife rests and hedgehogs runs the length of every land edge a few
//      metres outside it, thickened every twenty to forty metres into a knot of wreckage — a limber on its side,
//      a barricade and gabions, a gun pit, the corner of a building. The eye stops at the belt, which is the point:
//      it says "not that way" without a wall and without an invisible one.
//   3. The coast is dressed. Stakes and hedgehogs stand in the surf where no craft grounds, wire lies along the dry
//      sand, and driftwood works up the tide line.
//
// Everything is hashed off its own position and the decoration seed, so it is the same battlefield every run and
// costs nothing to rebuild after a shell lands. It all goes through the composer's emit, which means it is batched
// and culled with the rest of the props (BattlefieldProps) rather than being a second renderer.
using System;
using UnityEngine;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public sealed class BattlefieldBackdrop
    {
        readonly BattlefieldKit kit;
        readonly int seed;
        Action<BattlefieldKit.Module, Matrix4x4> emit;
        MapData map;
        BattlefieldSurface surface;

        /// <summary>How far the trench lines and the country outside run before the haze takes them.</summary>
        public const float TrenchReach = 240f, BeltOut = 3.5f, BeltDepth = 9f;
        public int Pieces { get; private set; }

        public BattlefieldBackdrop(BattlefieldKit kit, int seed) { this.kit = kit; this.seed = seed; }

        public void Build(MapData map, BattlefieldSurface surface, Action<BattlefieldKit.Module, Matrix4x4> emit)
        {
            this.map = map; this.surface = surface; this.emit = emit;
            Pieces = 0;
            LinesRunOn();
            EdgeBelts();
            Coast();
        }

        // ---- helpers ------------------------------------------------------------------------------------------
        static float Rand(int i, int salt)
        {
            uint h = (uint)i * 0x9E3779B1u ^ (uint)salt * 0x85EBCA77u;
            h ^= h >> 15; h *= 0x2C1B3C6Du; h ^= h >> 12;
            return (h & 0xFFFF) / 65535f;
        }
        int Key(float x, float z, int salt) => Mathf.RoundToInt(x * 7.3f) * 92821 + Mathf.RoundToInt(z * 7.3f) * 53987 + salt * 7919 + seed;

        float Ground(float x, float z)
            => x >= 0f && z >= 0f && x <= map.SizeMeters.x && z <= map.SizeMeters.y
                ? surface.VisualHeight(x, z) : GreyboxTerrainView.SkirtHeight(map, x, z);

        /// <summary>Nothing is stood in the sea: it would be a tree growing out of the water.</summary>
        bool Dry(float x, float z, out float ground)
        {
            ground = Ground(x, z);
            return !Shore.UnderWater(map, x, z, ground);
        }

        void Put(BattlefieldKit.Module module, float x, float z, float yaw, float size, float sink = .06f, float lean = 0f)
        {
            if (!Dry(x, z, out float ground)) return;
            emit(module, Matrix4x4.TRS(new Vector3(x, ground - sink, z), Quaternion.Euler(lean, yaw, 0f), Vector3.one * size));
            Pieces++;
        }

        // ---- 1. the lines run on ------------------------------------------------------------------------------
        void LinesRunOn()
        {
            float w = map.SizeMeters.x;
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                var trench = map.Trenches[t];
                if (trench.Kind != 0 || trench.CellCount < 4) continue;
                // the two ends of the dug line, and which way each of them was heading
                for (int end = 0; end < 2; end++)
                {
                    int last = end == 0 ? trench.CellStart : trench.CellStart + trench.CellCount - 1;
                    int before = end == 0 ? trench.CellStart + 2 : trench.CellStart + trench.CellCount - 3;
                    var tip = map.NavCellCenter(map.TrenchCells[last]);
                    var inner = map.NavCellCenter(map.TrenchCells[before]);
                    float away = Mathf.Sign(tip.x - inner.x);
                    if (away == 0f) away = end == 0 ? -1f : 1f;
                    if ((away < 0f && tip.x > w * .25f) || (away > 0f && tip.x < w * .75f)) continue;   // that end does not reach a flank
                    RunOn(t, trench, new Vector2(tip.x, tip.z), away);
                }
            }
        }

        /// <summary>One trench continued off a flank: its parapet, its revetment and its wire, wandering the way the
        /// generator wanders a trench, coarser the further out it goes.</summary>
        void RunOn(int id, TrenchDef trench, Vector2 tip, float away)
        {
            var facing = new Vector3(Mathf.Sin(trench.FacingYaw), 0f, Mathf.Cos(trench.FacingYaw));
            float yaw = trench.FacingYaw * Mathf.Rad2Deg;
            for (float d = 1.5f; d < TrenchReach; )
            {
                float step = d < 70f ? 2f : d < 150f ? 4f : 8f;
                float x = tip.x + away * d;
                // the line goes on wandering, and drifts a little as it leaves: no ruled edge to the level
                float z = tip.y + 3.4f * Mathf.Sin(d * .052f + id * 2.1f) + 1.5f * Mathf.Sin(d * .173f + id) + Mathf.Sin(d * .011f + id * .7f) * d * .06f;
                int key = Key(x, z, 610 + id);
                float bank = .82f + Rand(key, 1) * .26f;
                // the parapet: bags on the enemy's side of the cut, laid in courses the length of the step
                if (Dry(x, z, out float ground))
                {
                    var lip = new Vector3(x, ground + .02f, z) + facing * .55f;
                    emit(kit.TrenchBags[(int)(Rand(key, 2) * 3f) % 3], Matrix4x4.TRS(lip,
                        Quaternion.LookRotation(facing) * Quaternion.Euler((Rand(key, 3) - .5f) * 5f, (Rand(key, 4) - .5f) * 7f, (Rand(key, 5) - .5f) * 4f),
                        new Vector3(step / 2f, bank, 1.05f)));
                    Pieces++;
                    // the back of the cut, while it is still close enough to see into
                    if (d < 90f && Rand(key, 6) < .8f)
                        emit(kit.TrenchWalls[(int)(Rand(key, 7) * 3f) % 3], Matrix4x4.TRS(new Vector3(x, ground - .55f, z) - facing * .55f,
                            Quaternion.LookRotation(facing) * Quaternion.Euler(-4f, 0f, 0f), new Vector3(step / 2f, .9f, 1f)));
                    if (d < 60f && Rand(key, 8) < .22f) Put(kit.ladder, x + Rand(key, 9) * 1.2f - .6f, z - facing.z * .9f, yaw, 1f, 0f);
                    if (Rand(key, 10) < .30f) Put(kit.sandbags, x + (Rand(key, 11) - .5f) * 1.6f, z - facing.z * (1.4f + Rand(key, 12)), yaw + (Rand(key, 13) - .5f) * 40f, .9f + Rand(key, 14) * .3f);
                    if (Rand(key, 15) < .12f) Put(kit.stones, x + (Rand(key, 16) - .5f) * 3f, z - facing.z * (2f + Rand(key, 17) * 2f), Rand(key, 18) * 360f, .9f);
                }
                // its wire, out in front, in broken runs like the generator's
                if (Rand(key, 20) < .62f)
                {
                    float out1 = 9f + 3f * Mathf.Sin(d * .09f + id);
                    var at = new Vector3(x, 0f, z) + facing * out1;
                    if (Dry(at.x, at.z, out float wireGround))
                    {
                        float size = .95f + Rand(key, 21) * .25f;
                        var turn = Quaternion.Euler(0f, yaw + 90f + (Rand(key, 22) - .5f) * 16f, 0f);
                        emit(Rand(key, 23) < .55f ? kit.knifeRest : kit.wire, Matrix4x4.TRS(new Vector3(at.x, wireGround - .05f, at.z), turn, Vector3.one * size));
                        Pieces++;
                    }
                }
                d += step;
            }
        }

        // ---- 2. the edges are closed --------------------------------------------------------------------------
        /// <summary>The land edges: both flanks, and the rear behind the near army. The coast closes the fourth by
        /// itself, which is the whole reason a sea is worth having.</summary>
        void EdgeBelts()
        {
            float w = map.SizeMeters.x, l = map.SizeMeters.y;
            // side 0: x below 0, side 1: x above w, side 2: z below 0, side 3: z above l (only without a coast there)
            for (int side = 0; side < 4; side++)
            {
                if (side == 3 && map.HasSea && map.SeaSide == 1) continue;
                if (side == 2 && map.HasSea && map.SeaSide == 0) continue;
                bool alongZ = side < 2;
                float span = alongZ ? l : w;
                float from = alongZ ? -90f : -90f, to = span + 90f;
                for (float t = from; t < to; t += 2.4f)
                {
                    if (CutByTrench(side, t)) continue;   // a trench runs out of the map here: wire is not strung across its mouth
                    for (int row = 0; row < 3; row++)
                    {
                        float outAt = BeltOut + row * (BeltDepth / 3f) + Mathf.Sin(t * .07f + row * 2.3f) * 1.8f;
                        float x = alongZ ? (side == 0 ? -outAt : w + outAt) : t;
                        float z = alongZ ? t : (side == 2 ? -outAt : l + outAt);
                        int key = Key(x, z, 700 + side * 3 + row);
                        if (Rand(key, 1) < (row == 1 ? .42f : .30f)) continue;   // broken runs, thickest in the middle row
                        float yaw = (alongZ ? 0f : 90f) + (Rand(key, 2) - .5f) * 22f;
                        float size = .95f + Rand(key, 3) * .35f;
                        float pick = Rand(key, 4);
                        var piece = pick < .34f ? kit.knifeRest : pick < .56f ? kit.wire : pick < .74f ? kit.wireFence : pick < .88f ? kit.hedgehog : kit.stakes;
                        Put(piece, x, z, yaw, size, .05f);
                        if (piece == kit.wire && Rand(key, 5) < .5f)
                            Put(kit.wirePost, x + (alongZ ? 0f : .95f * size), z + (alongZ ? .95f * size : 0f), yaw, size, .08f);
                    }
                    // the knots: a wreck, a strongpoint, the corner of a building, every twenty to forty metres
                    int clusterKey = Key(alongZ ? side * 1000f : t, alongZ ? t : side * 1000f, 760 + side);
                    if (Rand(clusterKey, 1) < .07f) Cluster(side, t, clusterKey);
                }
            }
        }

        /// <summary>Does a trench leave the map at this point on this edge? Its continuation (LinesRunOn) runs through
        /// here, and a belt laid over it would wire a trench shut.</summary>
        bool CutByTrench(int side, float t)
        {
            if (side > 1) return false;                       // only the flanks: no trench reaches the rear edges
            float x = side == 0 ? .5f : map.SizeMeters.x - .5f;
            for (float d = -7f; d <= 7f; d += 2f)
            {
                float z = t + d;
                if (z < 0f || z > map.SizeMeters.y) continue;
                var cell = map.NavCellOf(new Unity.Mathematics.float3(x, 0f, z));
                var layer = (NavLayer)map.NavLayers[map.NavIndex(cell.x, cell.y)];
                if ((layer & (NavLayer.Trench | NavLayer.Link)) != 0) return true;
            }
            return false;
        }

        /// <summary>One knot of wreckage outside an edge. Six kinds, so a long edge does not repeat itself.</summary>
        void Cluster(int side, float t, int key)
        {
            float w = map.SizeMeters.x, l = map.SizeMeters.y;
            bool alongZ = side < 2;
            float outAt = BeltOut + BeltDepth + 3f + Rand(key, 2) * 14f;
            float cx = alongZ ? (side == 0 ? -outAt : w + outAt) : t;
            float cz = alongZ ? t : (side == 2 ? -outAt : l + outAt);
            float into = alongZ ? (side == 0 ? 1f : -1f) : 0f;       // back towards the field
            float along = alongZ ? 0f : (side == 2 ? 1f : -1f);
            float face = alongZ ? (side == 0 ? 90f : 270f) : (side == 2 ? 0f : 180f);
            int kind = (int)(Rand(key, 3) * 6f) % 6;
            void At(BattlefieldKit.Module module, float lateral, float depth, float yaw, float size, float sink = .06f)
                => Put(module, cx + (alongZ ? depth * into : lateral), cz + (alongZ ? lateral : depth * along), face + yaw, size, sink);

            switch (kind)
            {
                case 0:   // a limber on its side where it was caught, its load thrown out
                    At(kit.limber, 0f, 0f, Rand(key, 4) * 360f, 1.15f);
                    At(kit.shellStack, 2.6f, 1.4f, Rand(key, 5) * 60f, 1f);
                    At(kit.dudShell, -1.9f, 2.2f, Rand(key, 6) * 360f, .9f);
                    At(kit.branches, 3.4f, -1.8f, Rand(key, 7) * 360f, 1.2f);
                    break;
                case 1:   // a strongpoint: barricade, gabions and a course of bags
                    At(kit.barricade, 0f, 0f, (Rand(key, 4) - .5f) * 20f, 1.2f);
                    At(kit.gabion, -2.2f, .6f, Rand(key, 5) * 360f, .95f);
                    At(kit.gabion, -3.3f, -.4f, Rand(key, 6) * 360f, .85f);
                    At(kit.sandbags, 2.4f, .2f, (Rand(key, 7) - .5f) * 30f, 1.1f);
                    At(kit.sandbags, 3.6f, -.6f, (Rand(key, 8) - .5f) * 30f, 1f);
                    break;
                case 2:   // the corner of a building, and what fell off it
                    At(kit.wallStub, 0f, 0f, (Rand(key, 4) - .5f) * 40f, 1.3f);
                    At(kit.rebarSlab, 3.1f, 1.9f, Rand(key, 5) * 360f, 1f);
                    At(kit.boulder, -2.6f, 1.2f, Rand(key, 6) * 360f, 1.1f);
                    At(kit.corrugated, 1.4f, -2.4f, Rand(key, 7) * 360f, 1f);
                    break;
                case 3:   // a gun pit that has been given up
                    At(kit.fieldGun, 0f, 0f, 160f + Rand(key, 4) * 40f, 1f);
                    At(kit.sandbags, -2.8f, -1.2f, 80f + Rand(key, 5) * 30f, 1.1f);
                    At(kit.sandbags, 2.9f, -1.4f, -80f - Rand(key, 6) * 30f, 1.05f);
                    At(kit.shellStack, 1.8f, 2.6f, Rand(key, 7) * 90f, .95f);
                    break;
                case 4:   // a shelter dug into the bank, with its kit outside the door
                    At(kit.sodShelter, 0f, 0f, (Rand(key, 4) - .5f) * 30f, 1.25f);
                    At(kit.supplies, 2.9f, 1.6f, Rand(key, 5) * 360f, 1f);
                    At(kit.bucket, -2.1f, 1.9f, Rand(key, 6) * 360f, 1f);
                    At(kit.leanRifle, 1.2f, 1.1f, Rand(key, 7) * 360f, 1f);
                    break;
                default:  // what is left of a copse, cut down where it stood
                    At(kit.trunk, 0f, 0f, Rand(key, 4) * 360f, 1.3f);
                    At(kit.snag, -3.4f, 1.8f, Rand(key, 5) * 360f, 1.1f);
                    At(kit.fallenLog, 2.8f, 2.4f, Rand(key, 6) * 360f, 1.2f);
                    At(kit.stumpSplit, 4.2f, -1.6f, Rand(key, 7) * 360f, 1f);
                    At(kit.bush, -1.8f, -2.8f, Rand(key, 8) * 360f, 1f);
                    break;
            }
        }

        // ---- 3. the coast -------------------------------------------------------------------------------------
        /// <summary>The beach: obstacles standing in the surf off both flanks of it, where no craft grounds, wire and
        /// driftwood along the dry sand, and a scatter of boards and wreckage up the tide line.</summary>
        void Coast()
        {
            if (!map.HasSea) return;
            float w = map.SizeMeters.x;
            // obstacles stand out in the water only off the ends of the beach: a craft grounds anywhere between
            // them, and a hedgehog under a hull would read as a mistake rather than as an obstacle
            for (float x = -140f; x < w + 140f; x += 2.2f)
            {
                bool lane = x > w * .10f && x < w * .90f;
                for (int row = 0; row < 4; row++)
                {
                    float off = 1.5f + row * 3.4f + Mathf.Sin(x * .11f + row) * 1.4f;   // metres out from the waterline
                    float z = map.ShoreZ + map.SeaAway * off;
                    int key = Key(x, z, 820 + row);
                    if (Rand(key, 1) < (lane ? .93f : .52f)) continue;
                    float ground = Ground(x, z);
                    float depth = map.SeaLevel - ground;
                    if (depth > 1.5f) continue;                                          // deeper than that and it would be swimming
                    float size = 1f + Rand(key, 2) * .4f;
                    var piece = Rand(key, 3) < .5f ? kit.hedgehog : Rand(key, 3) < .8f ? kit.stakes : kit.knifeRest;
                    // they stand on the bed, leaning the way the sea has worked them
                    emit(piece, Matrix4x4.TRS(new Vector3(x, ground - .12f, z),
                        Quaternion.Euler((Rand(key, 4) - .5f) * 16f, Rand(key, 5) * 360f, (Rand(key, 6) - .5f) * 14f), Vector3.one * size));
                    Pieces++;
                }
                // the dry sand above the tide line: wire, and what the sea has pushed up
                for (int row = 0; row < 3; row++)
                {
                    float up = 3f + row * 3.2f + Mathf.Sin(x * .07f + row * 1.7f) * 1.6f;
                    float z = map.ShoreZ - map.SeaAway * up;
                    int key = Key(x, z, 860 + row);
                    if (Rand(key, 1) < .74f) continue;
                    float pick = Rand(key, 2);
                    var piece = pick < .30f ? kit.wire : pick < .50f ? kit.knifeRest : pick < .66f ? kit.branches
                              : pick < .80f ? kit.looseBoards : pick < .90f ? kit.stakes : kit.bracedPlank;
                    Put(piece, x, z, (Rand(key, 3) - .5f) * 30f + (pick < .5f ? 0f : Rand(key, 4) * 180f), .9f + Rand(key, 5) * .4f, .04f);
                }
            }
        }
    }
}
