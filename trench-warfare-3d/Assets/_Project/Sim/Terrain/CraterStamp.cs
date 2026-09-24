// Phase: A4 (implemented core; growth, rims and the trench guard band 2026-09-24)
// Carves a cosine bowl into the heightfield and marks open-ground nav cells as Crater (cost 2, speed 0.8, cover
// for whoever lies in it). Trench, link, wire and blocked cells keep their layer: a shell does not fill a trench in.
//
// Owner, 2026-09-24: the environment must be very malleable under high impacts, and "the more explosions on one
// place, the bigger the hole should become" -- but this is a trench game, so the ground NEAR a trench takes very
// little, the trench always stands, and it always gives limited protection. Three rules follow.
//
//   GROWTH. A runtime stamp (ApplyDynamic) landing on an existing hole WIDENS it instead of only deepening it:
//   R' = min(MaxRadius, sqrt(R^2 + GrowShare r^2)), and it carries only DepthShare of its own depth into the
//   deeper middle. Twenty shells on one spot make one big hole, not twenty pits inside each other. MapData.Holes
//   is the record; the heightfield already holds the old bowl, so a merge keeps the first centre.
//
//   RIMS. Spoil is thrown up in a ring outside the hole. It is bounded PER HOLE (HoleRecord.RimUp against MaxRim),
//   because a rim added afresh on every shell would build a mountain over a three-minute barrage: each stamp adds
//   only the difference between what the hole has already thrown up and what its depth now deserves.
//
//   THE GUARD BAND. Every carve and every rim is scaled by Hard(): zero within TrenchKeep of a trench or a ladder,
//   ramping to full over the next TrenchGuard metres. The parapet and the metre behind it never drop, the next four
//   metres dig progressively less, and only past five metres is the field soft. A hole may still grow its RADIUS
//   into the band -- its bowl is simply flat there -- so a shell hole never joins a trench and never undercuts one.
//   Nav cells are marked Crater only where the ground actually moved, so cover never lies about a hole that the
//   band refused to dig. There is no trench cave-in: a direct hit is thrown sandbags and planks, which is
//   presentation's business (PropDestruction), and BlastSystem's TrenchBayFactor is what makes it hurt.
//
// The generator's own pre-shelled field calls Apply (no growth, no rim) BEFORE MapData.BuildTrenchDistance has run,
// so Hard() reads 1 everywhere and the authored map is bit-for-bit what it always was.
using Unity.Mathematics;

namespace TW.Sim.Terrain
{
    public enum CraterKind : int
    {
        /// <summary>A shell hole: the ground goes down, and a second hit widens it.</summary>
        Bowl = 0,
        /// <summary>A heap: the ground goes UP (a building's rubble). Never merges, never records a hole.</summary>
        Mound = 1,
    }

    /// <summary>One hole in the field, kept so that later shells make it bigger rather than deeper only.
    /// 28 bytes, no padding: MapData.Hash folds it in field by field all the same.</summary>
    public struct HoleRecord
    {
        public float3 Center;
        public float Radius;    // as last carved
        public float Depth;     // cumulative depth carved at the middle
        public float RimUp;     // spoil already thrown up for this hole
        public int Hits;
    }

    public struct CraterStamp
    {
        public float3 Center;
        public float Radius;   // metres (HE 3 m, bomber 6 m, field gun 1.5 m)
        public float Depth;    // metres
        public int Kind;       // CraterKind; 0 = Bowl, so every existing call site is unchanged

        /// <summary>The widest a hole ever gets, however long it is shelled.</summary>
        public const float MaxRadius = 12f;
        /// <summary>A stamp merges into a hole whose centre is within this share of the larger of the two radii.</summary>
        public const float MergeShare = 0.75f;
        /// <summary>How much of a merging shell's area is added to the hole, and how much of its depth.</summary>
        public const float GrowShare = 0.6f, DepthShare = 0.6f;
        /// <summary>Spoil: RimShare of the hole's depth, never more than MaxRim, over a ring RimWidth of R wide.</summary>
        public const float RimShare = 0.25f, MaxRim = 0.5f, RimWidth = 0.3f;
        /// <summary>No ground within TrenchKeep metres of a trench or a ladder ever moves; over the next
        /// TrenchGuard metres it moves progressively more.</summary>
        public const float TrenchKeep = 1f, TrenchGuard = 4f;
        /// <summary>How far under the water table a shell may ever dig (ApplyWater then fills it: slow, never blocked).</summary>
        public const float MaxUnderWater = 0.9f;
        /// <summary>Holes remembered for merging. Past this the oldest is forgotten; its ground stays dug.</summary>
        public const int MaxHoles = 512;

        /// <summary>Apply as authored: no growth, no rim, no hole record. The generator's pre-shelled field.</summary>
        public int Apply(MapData map) => Stamp(map, record: false);

        /// <summary>Apply as a shell in the match: merges into the hole it lands in, throws spoil up, obeys the
        /// trench guard band. Returns the number of nav cells whose layer changed.</summary>
        public int ApplyDynamic(MapData map) => Stamp(map, record: true);

        /// <summary>How much of a carve this height cell takes: 0 beside a trench, 1 well clear of one.</summary>
        static float Hard(MapData map, int x, int z)
        {
            if (!map.CellTrenchDist.IsCreated) return 1f;
            float d = map.CellTrenchDist[map.Height.Index(x, z)] * 0.1f;
            return math.saturate((d - TrenchKeep) / TrenchGuard);
        }

        int Stamp(MapData map, bool record)
        {
            bool mound = Kind == (int)CraterKind.Mound;
            float3 c = Center;
            float r = Radius, depth = Depth, rim = 0f;

            if (record && !mound && map.Holes.IsCreated) Merge(map, ref c, ref r, ref depth, ref rim);

            var hf = map.Height;
            float cell = hf.CellSize;
            float outer = !mound && rim > RimEpsilon ? r * (1f + RimWidth) : r;
            float floorY = map.Bedrock;
            if (map.WaterLevel > MapData.NoWater) floorY = math.max(floorY, map.WaterLevel - MaxUnderWater);

            int minX = (int)math.floor((c.x - outer) / cell), maxX = (int)math.ceil((c.x + outer) / cell);
            int minZ = (int)math.floor((c.z - outer) / cell), maxZ = (int)math.ceil((c.z + outer) / cell);
            for (int z = minZ; z <= maxZ; z++)
            for (int x = minX; x <= maxX; x++)
            {
                if (!hf.InBounds(x, z)) continue;
                float px = (x + 0.5f) * cell, pz = (z + 0.5f) * cell;
                float dx = px - c.x, dz = pz - c.z;
                float d = SimMath.Sqrt(dx * dx + dz * dz);
                if (d >= outer) continue;
                byte layer = map.NavLayers[NavIndexAt(map, px, pz)];
                if ((layer & (byte)(NavLayer.Trench | NavLayer.Link | NavLayer.Blocked)) != 0) continue;   // trenches keep their floor
                float hard = Hard(map, x, z);
                if (hard <= 0f) continue;
                float h0 = hf.HeightAtCell(x, z);
                if (mound)
                {
                    if ((layer & (byte)NavLayer.Bunker) != 0) continue;
                    float dome = 0.5f * (1f + SimMath.Cos(SimMath.Pi * d / r));
                    hf.Set(x, z, h0 + Depth * dome * hard);
                }
                else if (d < r)
                {
                    float bowl = 0.5f * (1f + SimMath.Cos(SimMath.Pi * d / r));
                    hf.Set(x, z, math.max(floorY, h0 - depth * bowl * hard));
                }
                else if (rim > RimEpsilon && (layer & (byte)NavLayer.Bunker) == 0)
                {
                    float t = (d - r) / (RimWidth * r);
                    hf.Set(x, z, h0 + rim * SimMath.Sin(SimMath.Pi * t) * hard);
                }
            }

            int changed = 0;
            float n = MapData.NavCellSize;
            int nMinX = math.max(0, (int)math.floor((c.x - r) / n)), nMaxX = math.min(map.NavWidth - 1, (int)math.floor((c.x + r) / n));
            int nMinZ = math.max(0, (int)math.floor((c.z - r) / n)), nMaxZ = math.min(map.NavLength - 1, (int)math.floor((c.z + r) / n));
            for (int z = nMinZ; z <= nMaxZ; z++)
            for (int x = nMinX; x <= nMaxX; x++)
            {
                float px = (x + 0.5f) * n, pz = (z + 0.5f) * n;
                float dx = px - c.x, dz = pz - c.z;
                if (dx * dx + dz * dz > r * r) continue;
                int i = map.NavIndex(x, z);
                var layer = (NavLayer)map.NavLayers[i];
                if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked | NavLayer.Wire | NavLayer.Crater)) != 0) continue;
                // the band left this ground standing, so it is not a shell hole and must not give a shell hole's cover
                if (Hard(map, HeightCellOf(map, px), HeightCellOf(map, pz)) <= 0f) continue;
                map.SetLayer(x, z, layer | NavLayer.Crater);
                changed++;
            }
            changed += map.ApplyWater(nMinX, nMinZ, nMaxX, nMaxZ, allowBlock: false);   // a hole below the water table fills: slow going, never impassable
            map.Touch();
            return changed;
        }

        const float RimEpsilon = 0.005f;

        static int HeightCellOf(MapData map, float world) => (int)math.floor(world / map.Height.CellSize);
        static int NavIndexAt(MapData map, float px, float pz)
        {
            var nav = map.NavCellOf(new float3(px, 0f, pz));
            return map.NavIndex(nav.x, nav.y);
        }

        /// <summary>Find the hole this shell lands in and widen it; otherwise record a new one.</summary>
        void Merge(MapData map, ref float3 c, ref float r, ref float depth, ref float rim)
        {
            int hit = -1;
            for (int i = 0; i < map.Holes.Length; i++)
            {
                var q = map.Holes[i];
                float lim = MergeShare * math.max(q.Radius, Radius);
                float dx = q.Center.x - Center.x, dz = q.Center.z - Center.z;
                if (dx * dx + dz * dz <= lim * lim) { hit = i; break; }
            }
            if (hit >= 0)
            {
                var q = map.Holes[hit];
                r = math.min(MaxRadius, SimMath.Sqrt(q.Radius * q.Radius + GrowShare * Radius * Radius));
                depth = Depth * DepthShare;
                c = q.Center;                       // the ground already holds the old bowl: keep its middle
                q.Radius = r; q.Depth += depth; q.Hits++;
                float want = math.min(MaxRim, RimShare * q.Depth);
                rim = math.max(0f, want - q.RimUp);
                q.RimUp = want;
                map.Holes[hit] = q;
                return;
            }
            rim = math.min(MaxRim, RimShare * Depth);
            var fresh = new HoleRecord { Center = Center, Radius = r, Depth = Depth, RimUp = rim, Hits = 1 };
            if (map.Holes.Length >= MaxHoles)
            {
                for (int i = 1; i < map.Holes.Length; i++) map.Holes[i - 1] = map.Holes[i];
                map.Holes[map.Holes.Length - 1] = fresh;
            }
            else map.Holes.Add(fresh);
        }
    }
}
