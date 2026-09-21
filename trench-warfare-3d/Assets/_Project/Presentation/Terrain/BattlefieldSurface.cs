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
            public int Cell, Trench, Key;
            public bool Link;
            public Quaternion Rotation => Quaternion.LookRotation(Outward);
        }
        public struct Hollow { public Vector2 Center; public float Radius, Depth; }
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

        public BattlefieldSurface(MapData map)
        {
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
            RefreshHollows();
        }

        int Index(float x, float z) => Mathf.Clamp(Mathf.FloorToInt(z), 0, length - 1) * width + Mathf.Clamp(Mathf.FloorToInt(x), 0, width - 1);
        public float BankDistance(float x, float z)
        {
            int i = nearEdge[Index(x, z)];
            if (i < 0) return 8f;
            var e = Edges[i]; var delta = new Vector3(x, 0f, z) - e.Center;
            float along = Mathf.Max(0f, Mathf.Abs(Vector3.Dot(delta, Vector3.Cross(Vector3.up, e.Outward))) - MapData.NavCellSize * .5f);
            float across = Vector3.Dot(delta, e.Outward);
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
            if (Vector3.Dot(new Vector3(x, 0f, z) - edge.Center, edge.Outward) < 0f) return 0f;
            float distance = BankDistance(x, z);
            // World-coordinate profile: neighbouring terrain chunks and boundary segments agree exactly.
            float widthScale = 3.2f + Mathf.PerlinNoise(x * .19f + 41f, z * .19f) * 1.3f;
            float crest = .54f + Mathf.PerlinNoise(x * .38f, z * .38f + 17f) * .36f;
            return crest * Mathf.SmoothStep(0f, 1f, distance / .65f) * (1f - Mathf.SmoothStep(0f, 1f, (distance - .8f) / widthScale));
        }

        public float VisualHeight(float x, float z)
        {
            float height = map.Height.Sample(x, z);
            if (x <= .5f || z <= .5f || x >= width - .5f || z >= length - .5f) return height;
            int cell = map.NavIndex((int)(x / MapData.NavCellSize), (int)(z / MapData.NavCellSize));
            if (((NavLayer)map.NavLayers[cell] & (NavLayer.Trench | NavLayer.Link)) != 0) return height;
            float rise = BankRise(x, z);
            int index = nearHollow[Index(x, z)];
            if (index >= 0)
            {
                var hollow = Hollows[index];
                var delta = new Vector2(x, z) - hollow.Center;
                float angle = Mathf.Atan2(delta.y, delta.x);
                float radius = hollow.Radius * (1f + Mathf.Sin(angle * 7f + hollow.Center.x) * .04f);
                float rim = Mathf.Clamp01(1f - Mathf.Abs(delta.magnitude - radius) / .65f);
                rise = Mathf.Max(rise, rim * rim * .18f);
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
                Hollows.Add(new Hollow { Center = new Vector2(wx, wz), Radius = radius, Depth = depth });
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
