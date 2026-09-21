// Phase: B2 (presentation-only semantic surface and trench-boundary adapter)
using System.Collections.Generic;
using UnityEngine;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    /// <summary>Read-only interpretation of MapData shared by pigment and dressing. Never writes simulation data.</summary>
    public sealed class BattlefieldSurface
    {
        public struct Edge
        {
            public Vector3 Center, Outward;
            public Vector3 DressStart, DressEnd, DressCenter, DressOutward, DressTangent;
            public float DressLength;
            public int Cell, Trench, Key;
            public bool Link;
            public Quaternion Rotation => Quaternion.LookRotation(Outward);
        }
        public struct Hollow { public Vector2 Center; public float Radius, Depth, Level; }   // Level: water surface, or very low when dry
        public struct Sample
        {
            public float Height, Slope, Concavity, BankDistance, Wetness;
            public bool Trench;
            public int Hollow;
        }
        public readonly List<Edge> Edges = new List<Edge>();
        public readonly List<Hollow> Hollows = new List<Hollow>();
        readonly MapData map;
        readonly int[] nearEdge, nearHollow;
        readonly int width, length;

        /// <summary>Share of shell holes that stand full of water, 0..1 (the night look floods most of them).</summary>
        public readonly float Flooding;

        public BattlefieldSurface(MapData map, float flooding = 0f)
        {
            Flooding = flooding;
            this.map = map; width = map.Height.Width; length = map.Height.Length;
            nearEdge = new int[width * length]; nearHollow = new int[width * length];
            for (int i = 0; i < nearEdge.Length; i++) nearEdge[i] = nearHollow[i] = -1;
            var distances = new float[nearEdge.Length];
            for (int i = 0; i < distances.Length; i++) distances[i] = 64f;
            Vector3[] directions = { Vector3.forward, Vector3.back, Vector3.right, Vector3.left };
            float n = MapData.NavCellSize;
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                var trench = map.Trenches[t];
                for (int k = 0; k < trench.CellCount; k++)
                {
                    int cell = map.TrenchCells[trench.CellStart + k], cx = cell % map.NavWidth, cz = cell / map.NavWidth;
                    foreach (var outward in directions)
                    {
                        int nx = cx + Mathf.RoundToInt(outward.x), nz = cz + Mathf.RoundToInt(outward.z);
                        if (nx < 0 || nz < 0 || nx >= map.NavWidth || nz >= map.NavLength) continue;
                        if (((NavLayer)map.NavLayers[map.NavIndex(nx, nz)] & NavLayer.Trench) != 0) continue;
                        var edge = new Edge { Center = new Vector3((cx + .5f) * n, 0f, (cz + .5f) * n) + outward * (n * .5f),
                            Outward = outward, Cell = cell, Trench = t, Key = cell * 7 + Edges.Count,
                            Link = ((NavLayer)map.NavLayers[cell] & NavLayer.Link) != 0 };
                        int index = Edges.Count; Edges.Add(edge);
                        Vector3 tangent = Vector3.Cross(Vector3.up, outward);
                        for (int z = Mathf.Max(0, (int)edge.Center.z - 7); z <= Mathf.Min(length - 1, (int)edge.Center.z + 7); z++)
                        for (int x = Mathf.Max(0, (int)edge.Center.x - 7); x <= Mathf.Min(width - 1, (int)edge.Center.x + 7); x++)
                        {
                            Vector3 delta = new Vector3(x + .5f, 0f, z + .5f) - edge.Center;
                            float along = Mathf.Max(0f, Mathf.Abs(Vector3.Dot(delta, tangent)) - n * .5f);
                            float across = Vector3.Dot(delta, outward), d = along * along + across * across;
                            int i = z * width + x;
                            if (d < distances[i]) { distances[i] = d; nearEdge[i] = index; }
                        }
                    }
                }
            }
            BuildDressingContours();
            RefreshHollows();
        }

        void BuildDressingContours()
        {
            // Shared vertices join neighbouring modules. Broad sway, bay-scale shoulders and small wear are separate.
            var joins = new Dictionary<Vector3, List<int>>();
            for (int i = 0; i < Edges.Count; i++)
            {
                var e = Edges[i]; var tangent = Vector3.Cross(Vector3.up, e.Outward);
                foreach (var p in new[] { e.Center - tangent, e.Center + tangent })
                { if (!joins.TryGetValue(p, out var list)) joins[p] = list = new List<int>(2); list.Add(i); }
            }
            var points = new Dictionary<Vector3, Vector3>();
            foreach (var join in joins)
            {
                var p = join.Key; var normal = Vector3.zero; var toward = Vector3.zero;
                foreach (int i in join.Value) { normal += Edges[i].Outward; toward += Edges[i].Center - p; }
                normal.Normalize();
                float clearance = 100f;
                foreach (var edge in Edges) if (edge.Link) clearance = Mathf.Min(clearance, Vector3.Distance(p, edge.Center));
                float taper = Mathf.SmoothStep(0f, 1f, (clearance - 2f) / 4f);
                int trench = Edges[join.Value[0]].Trench;
                var sway = new Vector3(Mathf.Sin(p.z * .14f + 8f) * .20f, 0f, Mathf.Sin(p.x * .14f + trench * 1.7f) * .52f);
                float medium = (Mathf.PerlinNoise(p.x * .15f + 17f, p.z * .15f) - .5f) * .28f;
                // Positive shoulder allowance keeps the widening outside the walking corridor.
                bool corner = Vector3.Dot(normal, Edges[join.Value[0]].Outward) < .95f;
                float outset = Mathf.Max(corner ? .48f : .12f, .68f + Vector3.Dot(sway, normal) + medium);
                points[p] = p + (normal * outset + toward * .30f) * taper;
            }
            // Tight one-cell notches cannot accept the full broad offset. Relax shared endpoints together,
            // retaining a useful panel length without opening a seam in either neighbouring segment.
            for (int pass = 0; pass < 8; pass++)
            {
                bool changed = false;
                foreach (var edge in Edges)
                {
                    var tangent = Vector3.Cross(Vector3.up, edge.Outward);
                    var a = edge.Center - tangent; var b = edge.Center + tangent;
                    var segment = points[b] - points[a];
                    if (Vector3.Dot(segment, tangent) >= .85f && Vector3.Dot(segment.normalized, tangent) >= .65f) continue;
                    points[a] = Vector3.Lerp(a, points[a], .5f); points[b] = Vector3.Lerp(b, points[b], .5f); changed = true;
                }
                if (!changed) break;
            }
            for (int i = 0; i < Edges.Count; i++)
            {
                var edge = Edges[i]; var tangent = Vector3.Cross(Vector3.up, edge.Outward);
                edge.DressStart = points[edge.Center - tangent]; edge.DressEnd = points[edge.Center + tangent];
                edge.DressCenter = (edge.DressStart + edge.DressEnd) * .5f;
                edge.DressLength = Vector3.Distance(edge.DressStart, edge.DressEnd);
                edge.DressTangent = (edge.DressEnd - edge.DressStart) / edge.DressLength;
                edge.DressOutward = Vector3.Cross(edge.DressTangent, Vector3.up);
                Edges[i] = edge;
            }
        }

        int Index(float x, float z) => Mathf.Clamp(Mathf.FloorToInt(z), 0, length - 1) * width + Mathf.Clamp(Mathf.FloorToInt(x), 0, width - 1);
        public float BankDistance(float x, float z)
        {
            int i = nearEdge[Index(x, z)];
            if (i < 0) return 8f;
            var e = Edges[i]; var delta = new Vector3(x, 0f, z) - e.DressCenter;
            float along = Mathf.Max(0f, Mathf.Abs(Vector3.Dot(delta, e.DressTangent)) - e.DressLength * .5f);
            float across = Vector3.Dot(delta, e.DressOutward);
            return Mathf.Sqrt(along * along + across * across);
        }

        public float BankRise(float x, float z)
        {
            if (x <= .5f || z <= .5f || x >= width - .5f || z >= length - .5f) return 0f;
            int cell = map.NavIndex(Mathf.Clamp((int)(x / MapData.NavCellSize), 0, map.NavWidth - 1), Mathf.Clamp((int)(z / MapData.NavCellSize), 0, map.NavLength - 1));
            if (((NavLayer)map.NavLayers[cell] & (NavLayer.Trench | NavLayer.Link)) != 0) return 0f;
            int index = nearEdge[Index(x, z)];
            if (index < 0 || Edges[index].Link) return 0f;
            var edge = Edges[index];
            if (Vector3.Dot(new Vector3(x, 0f, z) - edge.DressCenter, edge.DressOutward) < 0f) return 0f;
            float distance = BankDistance(x, z);
            // World-coordinate profile: neighbouring terrain chunks and boundary segments agree exactly.
            float widthScale = 2.4f + Mathf.PerlinNoise(x * .10f + 41f, z * .10f) * 2.6f;
            float crest = .40f + Mathf.PerlinNoise(x * .13f, z * .13f + 17f) * .65f;
            return crest * Mathf.SmoothStep(0f, 1f, distance / .65f) * (1f - Mathf.SmoothStep(0f, 1f, (distance - .8f) / widthScale));
        }

        /// <summary>Rolling mounds and soil berms, in metres around zero. A function of the world position only, so the
        /// ground painter reads the same shapes the mesh shows.</summary>
        public static float Mound(float x, float z)
            => (Mathf.PerlinNoise(x * .085f + 13f, z * .085f + 57f) - .5f) * 1.0f
             + (Mathf.PerlinNoise(x * .23f + 71f, z * .23f + 5f) - .5f) * .42f
             + (Mathf.PerlinNoise(x * .61f + 29f, z * .61f + 83f) - .5f) * .14f;

        /// <summary>How much of the mounds this point may show: none at the map edge (the skirt meets the sim height),
        /// next to a trench (the bank owns that ground) or at the water's edge (the water sheet is flat).</summary>
        float MoundWeight(float x, float z, float height)
        {
            float edge = Mathf.Min(Mathf.Min(x, width - x), Mathf.Min(z, length - z));
            float weight = Mathf.SmoothStep(0f, 1f, (edge - .5f) / 7f) * Mathf.SmoothStep(0f, 1f, (BankDistance(x, z) - 1.5f) / 4f);
            if (map.WaterLevel > MapData.NoWater) weight *= Mathf.SmoothStep(0f, 1f, (height - map.WaterLevel - .15f) / .7f);
            return weight;
        }

        /// <summary>The drawn surface: the bed, or the flat water standing in a flooded shell hole.</summary>
        public float VisualHeight(float x, float z)
        {
            float bed = Bed(x, z);
            return bed + Mathf.Max(0f, PoolDepth(x, z, bed));
        }

        /// <summary>Depth of standing water in a flooded shell hole at this point, 0 or less when there is none.</summary>
        public float PoolDepth(float x, float z) => PoolDepth(x, z, Bed(x, z));

        float PoolDepth(float x, float z, float bed)
        {
            if (Flooding <= 0f || x <= .5f || z <= .5f || x >= width - .5f || z >= length - .5f) return 0f;
            int index = nearHollow[Index(x, z)];
            if (index < 0) return 0f;
            var hollow = Hollows[index];
            if ((new Vector2(x, z) - hollow.Center).magnitude > hollow.Radius * 1.02f) return 0f;   // the pool stays inside its own rim
            return hollow.Level - bed;
        }

        /// <summary>The ground itself, under any standing water: men wade on it, debris lies on it.</summary>
        public float Bed(float x, float z)
        {
            float height = map.Height.Sample(x, z);
            if (x <= .5f || z <= .5f || x >= width - .5f || z >= length - .5f) return height;
            int cell = map.NavIndex((int)(x / MapData.NavCellSize), (int)(z / MapData.NavCellSize));
            if (((NavLayer)map.NavLayers[cell] & (NavLayer.Trench | NavLayer.Link)) != 0) return height;
            int boundary = nearEdge[Index(x, z)];
            if (boundary >= 0)
            {
                var edge = Edges[boundary]; var p = new Vector3(x, 0f, z);
                float across = Vector3.Dot(p - edge.DressCenter, edge.DressOutward);
                if (!edge.Link && across < .15f && Vector3.Dot(p - edge.Center, edge.Outward) >= 0f)
                {
                    var inside = edge.Center - edge.Outward * .95f;
                    float floor = map.Height.Sample(inside.x, inside.z);
                    height = Mathf.Lerp(floor, height, Mathf.SmoothStep(0f, 1f, (across + .20f) / .35f));
                }
            }
            height += Mound(x, z) * MoundWeight(x, z, height);
            float rise = BankRise(x, z);
            int index = nearHollow[Index(x, z)];
            if (index >= 0)
            {
                var hollow = Hollows[index];
                var delta = new Vector2(x, z) - hollow.Center;
                float angle = Mathf.Atan2(delta.y, delta.x);
                float radius = hollow.Radius * (1f + Mathf.Sin(angle * 7f + hollow.Center.x) * .04f);
                // ejecta lip: thrown earth stands in clods around the hole, higher for a deeper hole, steep inside and
                // trailing off outside so neighbouring lips run into each other
                float across = delta.magnitude - radius;
                float rim = across < 0f ? Mathf.Clamp01(1f + across / .7f) : Mathf.Clamp01(1f - across / 1.6f);
                float clods = .72f + .28f * Mathf.Sin(angle * 5f + hollow.Center.y) * Mathf.Sin(angle * 11f + hollow.Center.x * 3f);
                rise = Mathf.Max(rise, rim * rim * clods * Mathf.Clamp(hollow.Depth * .42f, .22f, .6f));
            }
            return height + rise;
        }

        public Sample At(float x, float z)
        {
            var hf = map.Height; float h = hf.Sample(x, z);
            float dx = hf.Sample(x + 1f, z) - hf.Sample(x - 1f, z), dz = hf.Sample(x, z + 1f) - hf.Sample(x, z - 1f);
            float bx = (hf.Sample(x - 3f, z) + hf.Sample(x + 3f, z)) * .5f - h;
            float bz = (hf.Sample(x, z - 3f) + hf.Sample(x, z + 3f)) * .5f - h;
            int nx = Mathf.Clamp((int)(x / MapData.NavCellSize), 0, map.NavWidth - 1), nz = Mathf.Clamp((int)(z / MapData.NavCellSize), 0, map.NavLength - 1);
            return new Sample { Height = h, Slope = Mathf.Sqrt(dx * dx + dz * dz) * .5f, Concavity = Mathf.Min(bx, bz),
                BankDistance = BankDistance(x, z), Hollow = nearHollow[Index(x, z)],
                Trench = ((NavLayer)map.NavLayers[map.NavIndex(nx, nz)] & NavLayer.Trench) != 0,
                Wetness = map.WaterLevel > MapData.NoWater ? Mathf.Clamp01(1f - (h - map.WaterLevel) / .6f) : 0f };
        }

        public void RefreshHollows()
        {
            Hollows.Clear(); for (int i = 0; i < nearHollow.Length; i++) nearHollow[i] = -1;
            var hf = map.Height;
            for (int z = 3; z < length - 3; z++)
            for (int x = 3; x < width - 3; x++)
            {
                float wx = x + .5f, wz = z + .5f, h = hf.HeightAtCell(x, z);
                if (BankDistance(wx, wz) < 4f) continue;
                bool minimum = true;
                for (int dz = -1; dz <= 1; dz++) for (int dx = -1; dx <= 1; dx++)
                    if ((dx != 0 || dz != 0) && hf.HeightAtCell(x + dx, z + dz) < h + .005f) minimum = false;
                if (!minimum) continue;
                float ring = (hf.Sample(wx - 5f, wz) + hf.Sample(wx + 5f, wz) + hf.Sample(wx, wz - 5f) + hf.Sample(wx, wz + 5f)) * .25f;
                float depth = ring - h; if (depth < .42f) continue;
                bool overlap = false;
                foreach (var other in Hollows) if (Vector2.Distance(other.Center, new Vector2(wx, wz)) < 3f) overlap = true;
                if (overlap) continue;
                float radius = 1.5f;
                for (; radius < 5.5f; radius += .25f)
                {
                    float shoulder = (hf.Sample(wx - radius, wz) + hf.Sample(wx + radius, wz) + hf.Sample(wx, wz - radius) + hf.Sample(wx, wz + radius)) * .25f;
                    if (shoulder - h > depth * .86f) break;
                }
                int mark = Hollows.Count;
                // most holes hold water, each to its own level; hashed on the cell so a hole keeps its water when others land
                uint hash = (uint)x * 0x9E3779B1u ^ (uint)z * 0x85EBCA77u; hash ^= hash >> 15; hash *= 0x2C1B3C6Du; hash ^= hash >> 12;
                float roll = (hash & 0xFFFF) / 65535f, fill = .40f + .28f * ((hash >> 16) & 0xFF) / 255f;
                Hollows.Add(new Hollow { Center = new Vector2(wx, wz), Radius = radius, Depth = depth, Level = roll < Flooding ? h + depth * fill : -1000f });
                for (int zz = Mathf.Max(0, z - 8); zz <= Mathf.Min(length - 1, z + 8); zz++)
                for (int xx = Mathf.Max(0, x - 8); xx <= Mathf.Min(width - 1, x + 8); xx++)
                {
                    int i = zz * width + xx;
                    float distance = Vector2.Distance(new Vector2(xx + .5f, zz + .5f), new Vector2(wx, wz));
                    if (distance > radius + 1.5f) continue;
                    if (nearHollow[i] < 0 || distance < Vector2.Distance(new Vector2(xx + .5f, zz + .5f), Hollows[nearHollow[i]].Center)) nearHollow[i] = mark;
                }
            }
        }
    }
}
