// Phase: B7 (docs/21 phase 2) — the fields the scatter rules read, on the nav grid (2 m cells), plain C# over arrays.
// ScatterInput is what the map, the surface and the composer's layout know (cells, banks, wetness, mounds, holes, the
// things that stand up, the footprints that are taken, the links, the roads, the season, the coast, the rear bands);
// a test fills one by hand. ScatterField turns it into five numbers a cell:
//   Traffic  1 in a trench or on a ladder, falling away from the ladders, 0.7 along the corridors the men cross no
//            man's land by (link to link), 0.8 on the supply road and at the trench foot, 0.5 in the cart-rut band;
//   Vertical how much stands up within 3 m (a tree, a post, a wall): grass gathers against it;
//   Patch    where grass grows at all: slow noise over the field, salted by the decoration seed, thicker on a rise;
//   Wet      the ground's wetness (grass thins in the mud);
//   Open     0 where nothing may be placed (a trench, a ladder, a blocked or wire cell, a shell hole, the bank, the
//            water, the edge, a footprint), else 1.
// Every hash here is salted by the seed, so the same field grows the same grass on every machine and another seed
// grows another field. Nothing here reads the sim's flow fields: the corridors are the proxy for where men walk.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public sealed class ScatterInput
    {
        public struct Obstruction { public float X, Z, Radius, Height; }
        /// <summary>A rectangle of taken ground (a house, a shelter), turned by Yaw degrees. Dugout: a shelter whose
        /// inside gets a few crates and tins.</summary>
        public struct Footprint
        {
            public float X, Z, HalfX, HalfZ, Yaw; public bool Dugout;
            public bool Contains(float x, float z, float margin = 0f)
            {
                // world = R(yaw) local, with x' = x c - z s and z' = x s + z c; so local = R(-yaw) world
                float a = Yaw * Mathf.Deg2Rad, s = Mathf.Sin(a), c = Mathf.Cos(a);
                float dx = x - X, dz = z - Z;
                float lx = dx * c + dz * s, lz = -dx * s + dz * c;
                return Mathf.Abs(lx) <= HalfX + margin && Mathf.Abs(lz) <= HalfZ + margin;
            }
        }
        public struct Segment { public float X0, Z0, X1, Z1; }

        public int W, L;
        public const float Cell = MapData.NavCellSize;
        public uint Seed;
        public byte[] Nav;               // NavLayer bits per cell
        public float[] BankDistance, Wetness, Mound, TrenchDist;
        public bool[] Hollow;
        public readonly List<Obstruction> Obstructions = new List<Obstruction>(256);
        public readonly List<Footprint> Occupied = new List<Footprint>(32);
        public readonly List<Segment> Roads = new List<Segment>(4);
        /// <summary>The ladder cells of each side's front trench (cell x, z): the corridors run between them.</summary>
        public readonly List<Vector2Int> LinksA = new List<Vector2Int>(32), LinksB = new List<Vector2Int>(32);
        /// <summary>Where a lantern stands by a rear building.</summary>
        public readonly List<Vector2> RearLanterns = new List<Vector2>(8);
        public bool Frozen, Coast;
        /// <summary>The coast's sand band in z (grass thins on it, no flowers) and where the water begins (nothing past it).</summary>
        public float SandFromZ = float.PositiveInfinity, SandToZ = float.PositiveInfinity, WaterFromZ = float.PositiveInfinity, WaterToZ = float.NegativeInfinity;
        /// <summary>z below RearA is team 0's rear band, z above RearB team 1's: crates and shell stacks go there.</summary>
        public float RearA = float.NegativeInfinity, RearB = float.PositiveInfinity;

        public int Index(int x, int z) => z * W + x;
        public bool Inside(int x, int z) => x >= 0 && z >= 0 && x < W && z < L;
        public bool Is(int cell, NavLayer bits) => (Nav[cell] & (byte)bits) != 0;
        public static float CentreX(int x) => (x + 0.5f) * Cell;
        public static float CentreZ(int z) => (z + 0.5f) * Cell;
        public bool InSand(float z) => Coast && z >= SandFromZ && z <= SandToZ;
        public bool InWater(float z) => Coast && z >= WaterFromZ && z <= WaterToZ;
        public bool InRear(float z) => z < RearA || z > RearB;

        public bool Taken(float x, float z, out bool dugout)
        {
            dugout = false;
            for (int i = 0; i < Occupied.Count; i++)
                if (Occupied[i].Contains(x, z)) { dugout = Occupied[i].Dugout; return true; }
            return false;
        }

        /// <summary>An open field: surface everywhere, dry, level-ish rises (Mound 0.5), far from any bank. Tests build on it.</summary>
        public static ScatterInput Blank(int w, int l, uint seed)
        {
            var input = new ScatterInput { W = w, L = l, Seed = seed, Nav = new byte[w * l], BankDistance = new float[w * l], Wetness = new float[w * l], Mound = new float[w * l], TrenchDist = new float[w * l], Hollow = new bool[w * l] };
            for (int i = 0; i < w * l; i++) { input.Nav[i] = (byte)NavLayer.Surface; input.BankDistance[i] = 99f; input.Mound[i] = 0.5f; input.TrenchDist[i] = 255f; }
            return input;
        }

        /// <summary>Mark a row of cells as a trench (with ladders where <paramref name="links"/> says), for a test.</summary>
        public void TrenchRow(int z, int[] links, bool sideA)
        {
            for (int x = 0; x < W; x++) { Nav[Index(x, z)] = (byte)NavLayer.Trench; TrenchDist[Index(x, z)] = 0f; }
            foreach (int x in links)
            {
                Nav[Index(x, z)] = (byte)NavLayer.Link;
                (sideA ? LinksA : LinksB).Add(new Vector2Int(x, z));
            }
            for (int x = 0; x < W; x++)
                for (int dz = -4; dz <= 4; dz++)
                {
                    int zz = z + dz;
                    if (zz < 0 || zz >= L || dz == 0) continue;
                    int i = Index(x, zz);
                    float d = Mathf.Abs(dz) * Cell;
                    if (d < TrenchDist[i]) TrenchDist[i] = d;
                    if (d - Cell * 0.5f < BankDistance[i]) BankDistance[i] = Mathf.Max(0f, d - Cell * 0.5f);
                }
        }
    }

    public sealed class ScatterField
    {
        public readonly int W, L;
        public readonly float[] Traffic, Vertical, Patch, Wet, Open;
        public const float LadderReach = 8f, CorridorHalf = 3f, CorridorTraffic = 0.7f, RoadHalf = 2.5f, RoadTraffic = 0.8f;
        public const float TrenchFoot = 2f, TrenchFootTraffic = 0.8f, RutFrom = 3.9f, RutTo = 7.6f, RutTraffic = 0.5f;
        public const float CorridorMaxAcross = 40f, VerticalReach = 3f, VerticalFullHeight = 1.5f;
        public const float BankClear = 1.3f, EdgeClear = 1f;
        public const uint PatchSalt = 0x5CA77E12u;

        ScatterField(int w, int l)
        {
            W = w; L = l;
            Traffic = new float[w * l]; Vertical = new float[w * l]; Patch = new float[w * l]; Wet = new float[w * l]; Open = new float[w * l];
        }

        public static ScatterField Build(ScatterInput input)
        {
            var f = new ScatterField(input.W, input.L);
            f.BuildTraffic(input);
            f.BuildVertical(input);
            f.BuildPatch(input);
            for (int i = 0; i < f.Wet.Length; i++) f.Wet[i] = Mathf.Clamp01(input.Wetness[i]);
            f.BuildOpen(input);
            return f;
        }

        void BuildTraffic(ScatterInput input)
        {
            int w = W, l = L;
            // 1 in a trench or on a ladder; then a chamfer distance from the ladders, falling away over LadderReach
            var dist = new float[w * l];
            for (int i = 0; i < dist.Length; i++) dist[i] = float.MaxValue;
            for (int z = 0; z < l; z++)
            for (int x = 0; x < w; x++)
            {
                int i = input.Index(x, z);
                if (input.Is(i, NavLayer.Trench | NavLayer.Link)) Traffic[i] = 1f;
                if (input.Is(i, NavLayer.Link)) dist[i] = 0f;
            }
            const float straight = ScatterInput.Cell, diagonal = ScatterInput.Cell * 1.4142f;
            for (int z = 0; z < l; z++)
            for (int x = 0; x < w; x++)
            {
                int i = input.Index(x, z); float d = dist[i];
                if (x > 0) d = Mathf.Min(d, dist[i - 1] + straight);
                if (z > 0) d = Mathf.Min(d, dist[i - w] + straight);
                if (x > 0 && z > 0) d = Mathf.Min(d, dist[i - w - 1] + diagonal);
                if (x < w - 1 && z > 0) d = Mathf.Min(d, dist[i - w + 1] + diagonal);
                dist[i] = d;
            }
            for (int z = l - 1; z >= 0; z--)
            for (int x = w - 1; x >= 0; x--)
            {
                int i = input.Index(x, z); float d = dist[i];
                if (x < w - 1) d = Mathf.Min(d, dist[i + 1] + straight);
                if (z < l - 1) d = Mathf.Min(d, dist[i + w] + straight);
                if (x < w - 1 && z < l - 1) d = Mathf.Min(d, dist[i + w + 1] + diagonal);
                if (x > 0 && z < l - 1) d = Mathf.Min(d, dist[i + w - 1] + diagonal);
                dist[i] = d;
            }
            for (int i = 0; i < dist.Length; i++)
                if (dist[i] < LadderReach) Traffic[i] = Mathf.Max(Traffic[i], 1f - dist[i] / LadderReach);
            // the corridors: from each ladder of one side's front to the nearest ladder of the other's, straight
            foreach (var a in input.LinksA)
            {
                float bestD = float.MaxValue; Vector2Int best = default; bool found = false;
                foreach (var b in input.LinksB)
                {
                    float dx = Mathf.Abs(a.x - b.x) * ScatterInput.Cell;
                    if (dx > CorridorMaxAcross) continue;
                    float d = (a - b).sqrMagnitude;
                    if (d < bestD) { bestD = d; best = b; found = true; }
                }
                if (!found) continue;
                Band(input, ScatterInput.CentreX(a.x), ScatterInput.CentreZ(a.y), ScatterInput.CentreX(best.x), ScatterInput.CentreZ(best.y), CorridorHalf, CorridorTraffic, true);
            }
            foreach (var r in input.Roads) Band(input, r.X0, r.Z0, r.X1, r.Z1, RoadHalf, RoadTraffic, false);
            // the trench foot and the cart-rut band beside the bank
            for (int i = 0; i < Traffic.Length; i++)
            {
                if (input.TrenchDist[i] <= TrenchFoot) Traffic[i] = Mathf.Max(Traffic[i], TrenchFootTraffic);
                float bank = input.BankDistance[i];
                if (bank >= RutFrom && bank <= RutTo) Traffic[i] = Mathf.Max(Traffic[i], RutTraffic);
            }
        }

        /// <summary>Traffic along a segment, full at its middle line and (with <paramref name="fade"/>) falling to 0 at its half width.</summary>
        void Band(ScatterInput input, float x0, float z0, float x1, float z1, float half, float traffic, bool fade)
        {
            float minX = Mathf.Min(x0, x1) - half, maxX = Mathf.Max(x0, x1) + half, minZ = Mathf.Min(z0, z1) - half, maxZ = Mathf.Max(z0, z1) + half;
            int cx0 = Mathf.Max(0, (int)(minX / ScatterInput.Cell)), cx1 = Mathf.Min(W - 1, (int)(maxX / ScatterInput.Cell));
            int cz0 = Mathf.Max(0, (int)(minZ / ScatterInput.Cell)), cz1 = Mathf.Min(L - 1, (int)(maxZ / ScatterInput.Cell));
            float dx = x1 - x0, dz = z1 - z0, len2 = dx * dx + dz * dz;
            for (int z = cz0; z <= cz1; z++)
            for (int x = cx0; x <= cx1; x++)
            {
                float px = ScatterInput.CentreX(x), pz = ScatterInput.CentreZ(z);
                float t = len2 > 1e-4f ? Mathf.Clamp01(((px - x0) * dx + (pz - z0) * dz) / len2) : 0f;
                float ax = px - (x0 + dx * t), az = pz - (z0 + dz * t);
                float across = Mathf.Sqrt(ax * ax + az * az);
                if (across > half) continue;
                float v = fade ? traffic * (1f - across / half) : traffic;
                int i = input.Index(x, z);
                Traffic[i] = Mathf.Max(Traffic[i], v);
            }
        }

        void BuildVertical(ScatterInput input)
        {
            foreach (var o in input.Obstructions)
            {
                float reach = VerticalReach + o.Radius;
                int cx0 = Mathf.Max(0, (int)((o.X - reach) / ScatterInput.Cell)), cx1 = Mathf.Min(W - 1, (int)((o.X + reach) / ScatterInput.Cell));
                int cz0 = Mathf.Max(0, (int)((o.Z - reach) / ScatterInput.Cell)), cz1 = Mathf.Min(L - 1, (int)((o.Z + reach) / ScatterInput.Cell));
                float tall = Mathf.Min(1f, o.Height / VerticalFullHeight);
                for (int z = cz0; z <= cz1; z++)
                for (int x = cx0; x <= cx1; x++)
                {
                    float dx = ScatterInput.CentreX(x) - o.X, dz = ScatterInput.CentreZ(z) - o.Z;
                    float d = Mathf.Max(0f, Mathf.Sqrt(dx * dx + dz * dz) - o.Radius);
                    if (d >= VerticalReach) continue;
                    float t = 1f - d / VerticalReach;
                    Vertical[input.Index(x, z)] += t * t * tall;
                }
            }
        }

        void BuildPatch(ScatterInput input)
        {
            uint seed = input.Seed ^ PatchSalt;
            for (int z = 0; z < L; z++)
            for (int x = 0; x < W; x++)
            {
                float px = ScatterInput.CentreX(x), pz = ScatterInput.CentreZ(z);
                float n = BattlefieldGenerator.Noise(seed, px, pz, 18f) * 0.5f + BattlefieldGenerator.Noise(seed + 1u, px, pz, 7f) * 0.3f + BattlefieldGenerator.Noise(seed + 2u, px, pz, 3f) * 0.2f;
                float t = Mathf.Clamp01((n - 0.48f) / 0.14f); t = t * t * (3f - 2f * t);
                int i = input.Index(x, z);
                Patch[i] = t * (0.6f + 0.4f * Mathf.Clamp01(input.Mound[i] / 0.3f));
            }
        }

        void BuildOpen(ScatterInput input)
        {
            float edgeX = W * ScatterInput.Cell, edgeZ = L * ScatterInput.Cell;
            for (int z = 0; z < L; z++)
            for (int x = 0; x < W; x++)
            {
                int i = input.Index(x, z);
                float px = ScatterInput.CentreX(x), pz = ScatterInput.CentreZ(z);
                bool shut = input.Is(i, NavLayer.Trench | NavLayer.Link | NavLayer.Blocked | NavLayer.Wire) || input.Hollow[i] || input.BankDistance[i] <= BankClear
                            || px < EdgeClear || pz < EdgeClear || px > edgeX - EdgeClear || pz > edgeZ - EdgeClear || input.InWater(pz) || input.Taken(px, pz, out _);
                Open[i] = shut ? 0f : 1f;
            }
        }
    }
}
