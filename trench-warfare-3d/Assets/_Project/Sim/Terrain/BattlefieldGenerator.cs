// Phase: A4 / C1 (implemented) — depends on: MapData, CraterStamp, MudField, WireBelt, PropDef, GreyboxMapGenerator helpers
// A whole battlefield from a seed and a handful of dials: rolling ground, a river across no man's land with fords
// and a plank bridge, low ground under the water table, shell holes, mud, wire with gaps, a shelled wood (some
// trees standing, most broken or stumps, fallen logs) and wrecks. Everything here is deterministic across machines:
// integer-hash value noise, Unity.Mathematics.Random and SimMath only (validate.py checks the rest). The same params give
// the same MapData.Hash, so a mission is a BattlefieldParams value, not a saved map.
// Trench layout and objectives are the playtest map's (reserve and front line per side).
using System.IO;
using Unity.Collections;
using Unity.Mathematics;

namespace TW.Sim.Terrain
{
    public struct BattlefieldParams
    {
        public uint Seed;
        public float Width, Length;
        public float Forest;      // 0..1 how much of the ground is wooded
        public float Shelling;    // 0..1 crater count, and how much of the wood is already broken
        public float Mud;         // 0..1 share of open ground that is churned to half speed
        public float WaterLevel;  // metres; MapData.NoWater for a dry field
        public bool River;
        public int Wrecks;
        public float Bombardment; // ambient shells per minute on no man's land during the match; 0 = a quiet sector

        public static BattlefieldParams ShelledForest(uint seed) => new BattlefieldParams
        { Seed = seed, Width = 90f, Length = 240f, Forest = 0.55f, Shelling = 0.7f, Mud = 0.5f, WaterLevel = 0.15f, River = true, Wrecks = 3, Bombardment = 8f };

        public byte[] Serialize()
        {
            using var ms = new MemoryStream();
            using var w = new BinaryWriter(ms);
            w.Write(Seed); w.Write(Width); w.Write(Length); w.Write(Forest); w.Write(Shelling); w.Write(Mud); w.Write(WaterLevel); w.Write(River); w.Write(Wrecks); w.Write(Bombardment);
            return ms.ToArray();
        }

        public static BattlefieldParams Deserialize(byte[] bytes)
        {
            using var r = new BinaryReader(new MemoryStream(bytes));
            return new BattlefieldParams
            {
                Seed = r.ReadUInt32(), Width = r.ReadSingle(), Length = r.ReadSingle(), Forest = r.ReadSingle(), Shelling = r.ReadSingle(),
                Mud = r.ReadSingle(), WaterLevel = r.ReadSingle(), River = r.ReadBoolean(), Wrecks = r.ReadInt32(), Bombardment = r.ReadSingle(),
            };
        }
    }

    public static class BattlefieldGenerator
    {
        public const int MapId = 3;
        // The layout was drawn for a 180 x 480 m field and scales with the params: along the front axis by Length / 480
        // (trench lines, HQ depth, the river's wander), across it by Width / 180 (crossings, wire runs).
        const float ReserveAt = 60f, FrontAt = 140f, HqDepthAt = 34f, BaseHeight = 1.2f;
        const float DugInHeight = 2.3f;   // trench lines and HQs sit on the higher ground, so a 1.8 m trench floor stays above the water table
        const float RiverHalfWidthAt = 10f, RiverBedDepth = 1.7f;

        public static MapData Create(BattlefieldParams p, Allocator allocator)
        {
            var map = new MapData(MapId, new float2(p.Width, p.Length), allocator);
            map.WaterLevel = p.WaterLevel;
            var rng = new Random(math.max(1u, p.Seed * 747796405u + 2891336453u));
            float L = p.Length, W = p.Width, sz = L / 480f, sx = W / 180f;
            float ReserveZ = ReserveAt * sz, FrontZ = FrontAt * sz, HqDepth = HqDepthAt * sz, RiverHalfWidth = RiverHalf(p);
            float[] trenchZ = { ReserveZ, FrontZ, L - FrontZ, L - ReserveZ };
            // Trenches are not ruled lines (owner, 2026-09-21): each 20 m fire bay sits up to 4 m forward or back of its
            // neighbour and a traverse joins them. Bays change on columns x % 10 == 0, the ladders stand on x % 10 == 5,
            // so a ladder always climbs out of the middle of a bay onto open ground.
            var bends = new int[4][];
            for (int t = 0; t < 4; t++)
            {
                var bendRng = new Random(math.max(1u, p.Seed * 2246822519u + (uint)t * 3266489917u));
                bends[t] = new int[map.NavWidth];
                int bay = bendRng.NextInt(-1, 2);
                for (int x = 0; x < map.NavWidth; x++)
                {
                    if (x > 0 && x % 10 == 0)
                    {
                        int step = bendRng.NextInt(1, 3) * (bendRng.NextBool() ? 1 : -1);
                        int next = math.clamp(bay + step, -2, 2);
                        bay = next != bay ? next : math.clamp(bay - step, -2, 2);   // at the limit, bend the other way: never a straight joint
                    }
                    bends[t][x] = bay;
                }
            }

            // ---- 1. rolling ground, levelled where men dig in ---------------------------------------------------
            var hf = map.Height;
            for (int z = 0; z < hf.Length; z++)
            for (int x = 0; x < hf.Width; x++)
            {
                float wx = x + 0.5f, wz = z + 0.5f;
                float h = BaseHeight + 2.6f * (Noise(p.Seed, wx, wz, 70f) - 0.5f) + 0.7f * (Noise(p.Seed + 1u, wx, wz, 18f) - 0.5f);
                float keep = math.min(Ramp(wz - HqDepth, 0f, 16f * sz), Ramp(L - HqDepth - wz, 0f, 16f * sz));   // HQ areas are level
                for (int t = 0; t < trenchZ.Length; t++) keep = math.min(keep, Ramp(math.abs(wz - (trenchZ[t] + 2f)) - 9f, 0f, 14f * sz));   // level under every bay, 4 m either way
                hf.Set(x, z, math.lerp(DugInHeight, h, keep));
            }

            // ---- 2. the river: a channel across the width, two fords and a plank bridge -------------------------
            bool river = p.River && p.WaterLevel > MapData.NoWater;
            float[] crossingX = { W * 0.2f + rng.NextFloat(-15f, 15f) * sx, W * 0.5f + rng.NextFloat(-20f, 20f) * sx, W * 0.8f + rng.NextFloat(-15f, 15f) * sx };
            int bridge = 1;
            float riverShift = rng.NextFloat(-25f, 25f) * sz;
            if (river)
            {
                float bed = p.WaterLevel - RiverBedDepth;
                for (int z = 0; z < hf.Length; z++)
                for (int x = 0; x < hf.Width; x++)
                {
                    float wx = x + 0.5f, wz = z + 0.5f;
                    float d = math.abs(wz - RiverZ(p, riverShift, wx));
                    if (d >= RiverHalfWidth) continue;
                    float h = math.lerp(hf.HeightAtCell(x, z), bed, Ramp(RiverHalfWidth - d, 0f, 6f));
                    for (int c = 0; c < crossingX.Length; c++)
                    {
                        float half = c == bridge ? 3f : 7f;
                        float top = c == bridge ? p.WaterLevel + 0.3f : p.WaterLevel - 0.4f;   // planks stay dry, a ford is knee deep
                        float on = 1f - Ramp(math.abs(wx - crossingX[c]) - half, 0f, 3f);
                        h = math.max(h, math.lerp(h, top, on));
                    }
                    hf.Set(x, z, math.min(hf.HeightAtCell(x, z), h));
                }
            }

            // ---- 3. trenches, objectives, spawns (the playtest layout) ------------------------------------------
            GreyboxMapGenerator.AddFireTrench(map, 0, 0, ReserveZ, 0f, next0: 1, next1: -1, offsets: bends[0]);
            GreyboxMapGenerator.AddFireTrench(map, 0, 1, FrontZ, 0f, next0: 2, next1: 0, offsets: bends[1]);
            GreyboxMapGenerator.AddFireTrench(map, 1, 2, L - FrontZ, SimMath.Pi, next0: 3, next1: 1, offsets: bends[2]);
            GreyboxMapGenerator.AddFireTrench(map, 1, 3, L - ReserveZ, SimMath.Pi, next0: -1, next1: 2, offsets: bends[3]);
            GreyboxMapGenerator.AddLineObjective(map, 0, ObjectiveKind.MainLine, 0, 0, FrontZ, 1, bends[1]);
            GreyboxMapGenerator.AddLineObjective(map, 1, ObjectiveKind.ReserveLine, 0, 0, ReserveZ, 2, bends[0]);
            GreyboxMapGenerator.AddLineObjective(map, 2, ObjectiveKind.HQ, 0, 0, 20f * sz, 3);
            GreyboxMapGenerator.AddLineObjective(map, 3, ObjectiveKind.MainLine, 1, 1, L - FrontZ, 1, bends[2]);
            GreyboxMapGenerator.AddLineObjective(map, 4, ObjectiveKind.ReserveLine, 1, 1, L - ReserveZ, 2, bends[3]);
            GreyboxMapGenerator.AddLineObjective(map, 5, ObjectiveKind.HQ, 1, 1, L - 20f * sz, 3);
            map.Spawns.Add(new SpawnPoint { Team = 0, Pos = new float3(W * 0.5f, 0f, 6f), Kind = 0 });
            map.Spawns.Add(new SpawnPoint { Team = 1, Pos = new float3(W * 0.5f, 0f, L - 6f), Kind = 0 });
            map.SupplyRoad.Add(new float3(W * 0.5f, 0f, 0f)); map.SupplyRoad.Add(new float3(W * 0.5f, 0f, 40f * sz));
            map.SupplyRoad.Add(new float3(W * 0.5f, 0f, L)); map.SupplyRoad.Add(new float3(W * 0.5f, 0f, L - 40f * sz));
            map.Wind = new float2(0f, -1f);

            // ---- 4. standing water: the river and the lowest hollows. Only here may water close a cell. ---------
            map.ApplyWater(0, 0, map.NavWidth - 1, map.NavLength - 1, allowBlock: true);

            // ---- 5. years of shelling (craters fill where they go under the water table, never block) ----------
            int shells = (int)(p.Shelling * 0.0017f * W * L);
            for (int k = 0; k < shells; k++)
            {
                float z = L * 0.5f + (rng.NextFloat() + rng.NextFloat() - 1f) * L * 0.42f;   // thickest in no man's land
                float x = rng.NextFloat(4f, W - 4f);
                float radius = rng.NextFloat(2f, 4.6f);
                if (z < HqDepth || z > L - HqDepth) continue;
                new CraterStamp { Center = new float3(x, 0f, z), Radius = radius, Depth = radius * 0.38f }.Apply(map);
            }

            // ---- 6. mud, by patches ------------------------------------------------------------------------------
            for (int z = 0; z < map.NavLength; z++)
            for (int x = 0; x < map.NavWidth; x++)
            {
                float wx = (x + 0.5f) * MapData.NavCellSize, wz = (z + 0.5f) * MapData.NavCellSize;
                if (wz < HqDepth || wz > L - HqDepth) continue;
                if (Noise(p.Seed + 7u, wx, wz, 34f) >= p.Mud * 0.42f) continue;
                var layer = (NavLayer)map.NavLayers[map.NavIndex(x, z)];
                if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Bunker | NavLayer.Blocked)) != 0) continue;
                map.SetLayer(x, z, layer | NavLayer.Mud);
            }

            // ---- 7. wire in front of both front lines, with gaps ------------------------------------------------
            for (int side = 0; side < 2; side++)
            {
                float z0 = side == 0 ? FrontZ + 16f * sz + 6f : L - FrontZ - 16f * sz - 4f - 6f;   // clear of the most forward bay
                float x = 0f;
                while (x < W)
                {
                    float run = rng.NextFloat(38f, 64f) * sx;
                    new WireBelt { Min = new float3(x, 0f, z0), Max = new float3(math.min(W - 1f, x + run), 0f, z0 + 3.9f) }.Place(map);
                    x += run + rng.NextFloat(8f, 12f);   // the gap
                }
            }

            // ---- 8. what is left of the wood, and the wrecks ----------------------------------------------------
            const float grid = 6f;
            for (float gz = HqDepth; gz < L - HqDepth; gz += grid)
            for (float gx = 3f; gx < W - 3f; gx += grid)
            {
                float x = gx + rng.NextFloat(0f, grid - 2f), z = gz + rng.NextFloat(0f, grid - 2f);
                float roll = rng.NextFloat();   // drawn for every grid point so the stream does not depend on what is skipped
                if (Noise(p.Seed + 11u, x, z, 46f) < 1f - p.Forest * 0.62f) continue;
                if (!ClearForProp(map, p, riverShift, river, trenchZ, x, z)) continue;
                PropKind kind = roll < p.Shelling * 0.45f ? PropKind.Stump : roll < p.Shelling * 0.8f ? PropKind.BrokenTree
                    : roll < p.Shelling * 0.92f ? PropKind.Log : PropKind.Tree;
                map.AddProp(new PropDef { Pos = new float3(x, 0f, z), Yaw = roll * 6.2831853f, Kind = kind });
            }
            for (int k = 0; k < p.Wrecks; k++)
            {
                float x = rng.NextFloat(20f, W - 20f), z = rng.NextFloat(FrontZ + 30f * sz, L - FrontZ - 30f * sz), yaw = rng.NextFloat(0f, 6.2831853f);
                if (ClearForProp(map, p, riverShift, river, trenchZ, x, z)) map.AddProp(new PropDef { Pos = new float3(x, 0f, z), Yaw = yaw, Kind = PropKind.Wreck });
            }
            if (river) map.AddProp(new PropDef { Pos = new float3(crossingX[bridge], 0f, RiverZ(p, riverShift, crossingX[bridge])), Yaw = 0f, Kind = PropKind.Bridge });

            map.RebuildCost();
            map.RebuildCover();
            return map;
        }

        /// <summary>Solid props stay off trench approaches (ladders must not be sealed), the river and its crossings,
        /// wire, water and the supply road.</summary>
        static bool ClearForProp(MapData map, BattlefieldParams p, float riverShift, bool river, float[] trenchZ, float x, float z)
        {
            for (int t = 0; t < trenchZ.Length; t++) if (math.abs(z - (trenchZ[t] + 2f)) < 14f) return false;
            if (river && math.abs(z - RiverZ(p, riverShift, x)) < RiverHalf(p) + 6f) return false;
            if (math.abs(x - p.Width * 0.5f) < 5f && (z < ReserveAt * p.Length / 480f || z > p.Length - ReserveAt * p.Length / 480f)) return false;
            var layer = map.LayerAt(new float3(x, 0f, z));
            return (layer & (NavLayer.Wire | NavLayer.Blocked | NavLayer.Trench | NavLayer.Link)) == 0 && map.WaterDepthAtCell(map.NavCellOf(new float3(x, 0f, z)).x, map.NavCellOf(new float3(x, 0f, z)).y) < MapData.WetDepth;
        }

        static float RiverHalf(BattlefieldParams p) => RiverHalfWidthAt * math.max(0.6f, p.Length / 480f);

        static float RiverZ(BattlefieldParams p, float shift, float x)
            => p.Length * 0.5f + shift + 44f * (p.Length / 480f) * (Noise(p.Seed + 3u, x, 0f, 90f) - 0.5f);

        /// <summary>True when infantry can walk from one spawn to the other (trenches through their ladders).</summary>
        public static bool Connected(MapData map)
        {
            int n = map.NavWidth * map.NavLength;
            var seen = new NativeArray<byte>(n, Allocator.Temp);
            var open = new NativeList<int>(1024, Allocator.Temp);
            int start = -1, goal = -1;
            for (int i = 0; i < map.Spawns.Length; i++)
            {
                var c = map.NavCellOf(map.Spawns[i].Pos);
                if (map.Spawns[i].Team == 0) start = map.NavIndex(c.x, c.y); else goal = map.NavIndex(c.x, c.y);
            }
            bool found = false;
            if (start >= 0 && goal >= 0)
            {
                open.Add(start); seen[start] = 1;
                while (open.Length > 0 && !found)
                {
                    int cell = open[open.Length - 1]; open.Length = open.Length - 1;
                    if (cell == goal) { found = true; break; }
                    int cx = cell % map.NavWidth, cz = cell / map.NavWidth;
                    for (int d = 0; d < 4; d++)
                    {
                        int nx = cx + (d == 0 ? 1 : d == 1 ? -1 : 0), nz = cz + (d == 2 ? 1 : d == 3 ? -1 : 0);
                        if (nx < 0 || nz < 0 || nx >= map.NavWidth || nz >= map.NavLength) continue;
                        int ni = map.NavIndex(nx, nz);
                        if (seen[ni] != 0) continue;
                        byte from = map.NavLayers[cell], to = map.NavLayers[ni];
                        if ((to & (byte)NavLayer.Blocked) != 0) continue;
                        const byte traversal = (byte)(NavLayer.Surface | NavLayer.Trench);
                        bool link = ((from | to) & (byte)NavLayer.Link) != 0;
                        if (!link && (from & to & traversal) == 0) continue;   // FlowField.CanStepInfantry, which lives above this assembly
                        seen[ni] = 1; open.Add(ni);
                    }
                }
            }
            seen.Dispose(); open.Dispose();
            return found;
        }

        // ---- deterministic value noise ---------------------------------------------------------------------------
        static float Lattice(uint seed, int x, int z)
        {
            uint h = seed * 0x9E3779B1u ^ (uint)x * 0x85EBCA77u ^ (uint)z * 0xC2B2AE3Du;
            h ^= h >> 15; h *= 0x2C1B3C6Du; h ^= h >> 12; h *= 0x297A2D39u; h ^= h >> 15;
            return (h & 0xFFFFFFu) * (1f / 16777216f);
        }

        /// <summary>Smooth value noise in 0..1 with features about <paramref name="cell"/> metres across.</summary>
        public static float Noise(uint seed, float x, float z, float cell)
        {
            float fx = x / cell, fz = z / cell;
            int x0 = (int)SimMath.Floor(fx), z0 = (int)SimMath.Floor(fz);
            float tx = fx - x0, tz = fz - z0;
            tx = tx * tx * (3f - 2f * tx); tz = tz * tz * (3f - 2f * tz);
            float a = math.lerp(Lattice(seed, x0, z0), Lattice(seed, x0 + 1, z0), tx);
            float b = math.lerp(Lattice(seed, x0, z0 + 1), Lattice(seed, x0 + 1, z0 + 1), tx);
            return math.lerp(a, b, tz);
        }

        /// <summary>0 at or below <paramref name="from"/>, 1 at or above from + width, smooth in between.</summary>
        static float Ramp(float v, float from, float width)
        {
            float t = math.saturate((v - from) / width);
            return t * t * (3f - 2f * t);
        }
    }
}
