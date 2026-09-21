// Phase: B2 (seeded read-only placement; no rendering, mesh construction or simulation writes)
using System;
using System.Collections.Generic;
using UnityEngine;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public sealed class BattlefieldComposer
    {
        public readonly struct Site
        {
            public readonly BattlefieldBlueprint Blueprint;
            public readonly Vector3 Position;
            public readonly Quaternion Rotation;
            public readonly int Trench;
            public readonly Vector3 ApproachEnd;
            public Site(BattlefieldBlueprint blueprint, Vector3 position, Quaternion rotation, int trench, Vector3 approachEnd)
            { Blueprint = blueprint; Position = position; Rotation = rotation; Trench = trench; ApproachEnd = approachEnd; }
        }
        readonly BattlefieldKit kit;
        readonly BattlefieldBlueprint[] blueprints;
        readonly int seed;
        readonly Vector3 preferredFront;
        readonly List<Site> sites = new List<Site>();
        readonly Dictionary<string, int> rejections = new Dictionary<string, int>();
        public string PlacementReport => string.Join(", ", rejections);
        public IReadOnlyList<Site> Sites => sites;
        Action<BattlefieldKit.Module, Matrix4x4> emit;
        MapData layoutMap;
        public BattlefieldComposer(BattlefieldKit kit, int seed = 1917, BattlefieldBlueprint[] blueprints = null, Vector3? preferredFront = null)
        { this.kit = kit; this.seed = seed; this.blueprints = blueprints ?? BattlefieldBlueprint.Stock(kit); this.preferredFront = preferredFront ?? Vector3.right; }

        public void Build(MapData map, BattlefieldSurface surface, Action<BattlefieldKit.Module, Matrix4x4> emit, bool regenerateLayout = false)
        {
            this.emit = emit;
            MapProps(map);
            TrenchKit(map, surface);
            if (!ReferenceEquals(layoutMap, map) || regenerateLayout)
            { sites.Clear(); rejections.Clear(); PlaceSites(map, surface); layoutMap = map; }
            else
            {
                // Combat updates validate existing sites; never run the global candidate search or relocate them.
                for (int i = sites.Count - 1; i >= 0; i--)
                {
                    var site = sites[i]; var at = site.Position; at.y = map.Height.Sample(at.x, at.z) - .10f;
                    if (!Fits(map, surface, site.Blueprint, at, site.Rotation)) { sites.RemoveAt(i); continue; }
                    sites[i] = new Site(site.Blueprint, at, site.Rotation, site.Trench, site.ApproachEnd);
                }
                foreach (var site in sites) EmitSite(map, surface, site);
            }
        }

        void TrenchKit(MapData map, BattlefieldSurface surface)
        {
            var floors = new HashSet<int>();
            foreach (var edge in surface.Edges)
            {
                var center = edge.Center; var outward = edge.Outward; var rotation = edge.Rotation;
                var inside = center - outward * .95f;
                float floor = map.Height.Sample(inside.x, inside.z);
                if (floors.Add(edge.Cell)) emit(kit.duckboards, Matrix4x4.TRS(new Vector3(inside.x, floor + .035f, inside.z), rotation, Vector3.one));
                if (edge.Link)
                {
                    emit(kit.ladder, Matrix4x4.TRS(new Vector3(center.x, floor, center.z) - outward * .35f, rotation, Vector3.one));
                    continue;
                }
                float waviness = (Mathf.PerlinNoise(center.x * .23f + 8f, center.z * .23f) - .5f) * .65f;
                var wall = center - outward * (.36f + waviness * .25f);
                var lip = center + outward * (.65f + waviness);
                float upper = surface.VisualHeight(lip.x, lip.z);
                float height = Mathf.Clamp((upper - floor) / 2f, .55f, 1.3f);
                emit(kit.planks, Matrix4x4.TRS(new Vector3(wall.x, floor, wall.z), rotation * Quaternion.Euler(-5f, 0f, Rand(edge.Key, 73) * 4f - 2f), new Vector3(.99f, height, 1.5f)));
                if ((edge.Cell / 3) % 9 != 4)
                    emit(kit.sandbags, Matrix4x4.TRS(new Vector3(lip.x, upper + .02f, lip.z), rotation * Quaternion.Euler(0f, Rand(edge.Key, 75) * 10f - 5f, 0f), new Vector3(1.04f, .95f, 1.2f)));
            }
        }

        void PlaceSites(MapData map, BattlefieldSurface surface)
        {
            if (blueprints.Length == 0) return;
            var candidates = new List<BattlefieldSurface.Edge>(surface.Edges);
            candidates.Sort((a, b) => { int order = Rand(a.Key, seed).CompareTo(Rand(b.Key, seed)); return order != 0 ? order : a.Key.CompareTo(b.Key); });
            var count = new int[map.Trenches.Length];
            foreach (var edge in candidates)
            {
                if (edge.Link || count[edge.Trench] >= 2) continue;
                var blueprint = blueprints[(edge.Trench + count[edge.Trench]) % blueprints.Length];
                // Splayed entrances still face the trench, with the readable side chosen by the art direction.
                var left = edge.Rotation * Quaternion.Euler(0f, -60f, 0f);
                var right = edge.Rotation * Quaternion.Euler(0f, 60f, 0f);
                var rotation = Vector3.Dot(left * Vector3.back, preferredFront) > Vector3.Dot(right * Vector3.back, preferredFront) ? left : right;
                rotation *= Quaternion.Euler(0f, (Rand(edge.Key, seed + 1) - .5f) * 12f, 0f);
                float nearest = float.MaxValue;
                for (int corner = 0; corner < 4; corner++)
                {
                    var b = blueprint.Footprint;
                    var p = new Vector3((corner & 1) == 0 ? b.min.x : b.max.x, 0f, (corner & 2) == 0 ? b.min.z : b.max.z);
                    nearest = Mathf.Min(nearest, Vector3.Dot(rotation * p, edge.Outward));
                }
                var at = Vector3.zero; bool found = false; string reason = "spacing";
                for (int setback = 0; setback < 3; setback++)
                {
                    at = edge.Center + edge.Outward * (2.1f + setback * 2f - nearest);
                    at.y = map.Height.Sample(at.x, at.z) - .10f;
                    bool crowded = false;
                    foreach (var other in sites) if (Vector2.Distance(new Vector2(at.x, at.z), new Vector2(other.Position.x, other.Position.z)) < 18f) crowded = true;
                    if (crowded) continue;
                    if (Fits(map, surface, blueprint, at, rotation, out reason)) { found = true; break; }
                }
                if (!found) { rejections.TryGetValue(reason, out int rejected); rejections[reason] = rejected + 1; continue; }
                var site = new Site(blueprint, at, rotation, edge.Trench, edge.Center + edge.Outward * 1.8f);
                sites.Add(site); count[edge.Trench]++; EmitSite(map, surface, site);
            }
        }

        void EmitSite(MapData map, BattlefieldSurface surface, Site site)
        {
            var matrix = Matrix4x4.TRS(site.Position, site.Rotation, Vector3.one);
            foreach (var socket in site.Blueprint.Sockets)
            {
                var part = matrix * socket.Local;
                if (socket.Grounded) part.m13 = surface.VisualHeight(part.m03, part.m23) + .025f;
                emit(socket.Module, part);
            }
            Approach(map, surface, matrix.MultiplyPoint3x4(site.Blueprint.Entrance), site.ApproachEnd);
        }

        public static bool Fits(MapData map, BattlefieldSurface surface, BattlefieldBlueprint blueprint, Vector3 at, Quaternion rotation)
            => Fits(map, surface, blueprint, at, rotation, out _);

        static bool Fits(MapData map, BattlefieldSurface surface, BattlefieldBlueprint blueprint, Vector3 at, Quaternion rotation, out string reason)
        {
            reason = "bounds/navigation";
            var b = blueprint.Footprint; float min = float.MaxValue, max = float.MinValue;
            float foundationMin = float.MaxValue, foundationMax = float.MinValue;
            // Bounds derive from actual module geometry, including every composition socket.
            int nx = Mathf.CeilToInt((b.size.x + .6f) / .5f), nz = Mathf.CeilToInt((b.size.z + .6f) / .5f);
            for (int z = 0; z <= nz; z++) for (int x = 0; x <= nx; x++)
            {
                var local = new Vector3(Mathf.Lerp(b.min.x - .3f, b.max.x + .3f, x / (float)nx), 0f, Mathf.Lerp(b.min.z - .3f, b.max.z + .3f, z / (float)nz));
                var p = at + rotation * local;
                if (!Clear(map, p.x, p.z)) return false;
                float h = surface.VisualHeight(p.x, p.z);
                if (h < map.WaterLevel + .15f) { reason = "water"; return false; }
                min = Mathf.Min(min, h); max = Mathf.Max(max, h);
                var f = blueprint.Foundation;
                if (local.x >= f.min.x && local.x <= f.max.x && local.z >= f.min.z && local.z <= f.max.z)
                { foundationMin = Mathf.Min(foundationMin, h); foundationMax = Mathf.Max(foundationMax, h); }
            }
            if (max - min > 1.45f || foundationMax - foundationMin > .65f) { reason = "slope"; return false; }
            var inverse = Quaternion.Inverse(rotation);
            foreach (var prop in map.Props)
            {
                var p = inverse * (new Vector3(prop.Pos.x, at.y, prop.Pos.z) - at);
                if (p.x > b.min.x - 1f && p.x < b.max.x + 1f && p.z > b.min.z - 1f && p.z < b.max.z + 1f) { reason = "existing prop"; return false; }
            }
            return true;
        }
        static bool Clear(MapData map, float x, float z)
        {
            if (x < 0f || z < 0f || x >= map.SizeMeters.x || z >= map.SizeMeters.y) return false;
            return ((NavLayer)map.NavLayers[map.NavIndex((int)(x / MapData.NavCellSize), (int)(z / MapData.NavCellSize))] & (NavLayer.Trench | NavLayer.Link | NavLayer.Wire | NavLayer.Blocked)) == 0;
        }
        void Approach(MapData map, BattlefieldSurface surface, Vector3 from, Vector3 to)
        {
            var delta = to - from; delta.y = 0f;
            float length = delta.magnitude; if (length < .5f) return;
            var direction = delta / length; var rotation = Quaternion.LookRotation(direction);
            int steps = Mathf.Min(8, Mathf.CeilToInt(length / 1.4f));
            for (int k = 0; k < steps; k++)
            {
                var p = Vector3.Lerp(from, to, (k + .5f) / steps);
                bool clear = true;
                for (int side = -1; side <= 1; side += 2)
                {
                    var corner = p + rotation * new Vector3(side * .85f, 0f, 0f);
                    if (!Clear(map, corner.x, corner.z)) clear = false;
                }
                if (!clear) continue;
                p.y = surface.VisualHeight(p.x, p.z) + .05f;
                var front = p + direction * .5f; var back = p - direction * .5f;
                float rise = surface.VisualHeight(front.x, front.z) - surface.VisualHeight(back.x, back.z);
                emit(kit.duckboards, Matrix4x4.TRS(p, rotation * Quaternion.Euler(-Mathf.Atan(rise) * Mathf.Rad2Deg, 0f, 0f), new Vector3(.8f, 1f, length / steps / 1.7f)));
            }
        }

        static float Rand(int i, int salt)
        {
            uint h = (uint)i * 0x9E3779B1u ^ (uint)salt * 0x85EBCA77u;
            h ^= h >> 15; h *= 0x2C1B3C6Du; h ^= h >> 12;
            return (h & 0xFFFF) / 65535f;
        }
        void MapProps(MapData map)
        {
            var hf = map.Height;
            for (int i = 0; i < map.Props.Length; i++)
            {
                var p = map.Props[i];
                float s = 0.85f + 0.3f * ((i * 37) % 100) / 100f;   // no two trees the same height
                var m = Matrix4x4.TRS(new Vector3(p.Pos.x, hf.Sample(p.Pos.x, p.Pos.z) - 0.05f, p.Pos.z), Quaternion.Euler(0f, p.Yaw * Mathf.Rad2Deg, 0f), new Vector3(s, s, s));
                switch (p.Kind)
                {
                    case PropKind.Tree: emit(i % 3 == 0 ? kit.fork : kit.trunk, m); break;
                    case PropKind.BrokenTree: emit(kit.snag, m); emit(kit.fallen, m); break;
                    case PropKind.Stump: emit(kit.stump, m); break;
                    case PropKind.Log: emit(kit.log, m); break;
                    case PropKind.Wreck: emit(kit.wreck, Matrix4x4.TRS(new Vector3(p.Pos.x, hf.Sample(p.Pos.x, p.Pos.z) - 0.25f, p.Pos.z), Quaternion.Euler(0f, p.Yaw * Mathf.Rad2Deg, 0f), Vector3.one)); break;
                    case PropKind.Bridge: emit(kit.bridge, Matrix4x4.TRS(new Vector3(p.Pos.x, map.WaterLevel + 0.3f, p.Pos.z), Quaternion.identity, Vector3.one)); break;
                }
            }

            float n = MapData.NavCellSize;
            for (int z = 0; z < map.NavLength; z++)
            for (int x = 0; x < map.NavWidth; x++)
            {
                if ((map.NavLayers[map.NavIndex(x, z)] & (byte)NavLayer.Wire) == 0) continue;
                float wx = (x + 0.5f) * n, wz = (z + 0.5f) * n;
                var at = new Vector3(wx, hf.Sample(wx, wz), wz);
                var turn = Quaternion.Euler(0f, (x * 13 + z * 7) % 16 - 8f, 0f);
                emit(kit.knifeRest, Matrix4x4.TRS(at, turn, Vector3.one));
                emit(kit.wire, Matrix4x4.TRS(at, turn, Vector3.one));
            }

            Horizon(map);

        }

        void Horizon(MapData map)
        {
            float w = map.SizeMeters.x, l = map.SizeMeters.y;
            for (int i = 0; i < 520; i++)
            {
                float x = -260f + Rand(i, 1) * (w + 520f), z = -200f + Rand(i, 2) * (l + 400f);
                float outside = Mathf.Max(Mathf.Max(-x, x - w), Mathf.Max(-z, z - l));
                if (outside < 5f) continue;
                float level = GreyboxTerrainView.SkirtHeight(map, x, z) - 0.05f;
                float s = 0.9f + 0.7f * Rand(i, 3);
                var m = Matrix4x4.TRS(new Vector3(x, level, z), Quaternion.Euler(0f, Rand(i, 4) * 360f, 0f), new Vector3(s, s, s));
                if (Rand(i, 5) < 0.26f) emit(kit.fork, m); else if (Rand(i, 5) < 0.62f) emit(kit.trunk, m); else if (Rand(i, 5) < 0.85f) emit(kit.snag, m); else emit(kit.stump, m);
            }
            for (int i = 0; i < 9; i++)
            {
                bool farSide = i < 6;   // the standard view looks along -X, so most of them stand there
                float x = farSide ? -45f - Rand(i, 6) * 120f : w + 45f + Rand(i, 6) * 90f, z = Rand(i, 7) * l;
                float s = 1.2f + Rand(i, 8) * 1.0f;
                emit(kit.ruin, Matrix4x4.TRS(new Vector3(x, GreyboxTerrainView.SkirtLevel - 0.25f, z), Quaternion.Euler(0f, 80f + Rand(i, 9) * 40f, 0f), new Vector3(s, s, s)));
            }
        }


    }
}
