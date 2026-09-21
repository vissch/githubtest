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
            Debris(map, surface);
            Clumps(map, surface);
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
                int variant = Mathf.Min(2, (int)(Rand(edge.Key, 802) * 3f));
                if (floors.Add(edge.Cell)) emit(kit.TrenchFloors[variant], Matrix4x4.TRS(new Vector3(inside.x, floor + .035f, inside.z), rotation * Quaternion.Euler(0f, edge.Link ? 0f : (Rand(edge.Key, 803) - .5f) * 7f, 0f), Vector3.one));
                if (edge.Link)
                {
                    emit(kit.ladder, Matrix4x4.TRS(new Vector3(center.x, floor, center.z) - outward * .35f, rotation, Vector3.one));
                    continue;
                }
                center = edge.DressCenter; outward = edge.DressOutward; rotation = Quaternion.LookRotation(outward);
                var wall = center - outward * .08f;
                var lip = center + outward * (.60f + (Rand(edge.Key, 804) - .5f) * .12f);
                float upper = surface.VisualHeight(lip.x, lip.z);
                float height = Mathf.Clamp((upper - floor) / 2f, .55f, 1.3f);
                emit(kit.TrenchWalls[variant], Matrix4x4.TRS(new Vector3(wall.x, floor, wall.z), rotation * Quaternion.Euler(-4f - Rand(edge.Key, 73) * 3f, 0f, 0f), new Vector3(edge.DressLength / 2f, height, 1f)));
                float frontage = Mathf.PerlinNoise(center.x * .09f + 19f, center.z * .09f + 7f);
                if (frontage > .26f)
                {
                    int course = frontage > .62f ? 0 : frontage > .43f ? 1 : 2;
                    emit(kit.TrenchBags[course], Matrix4x4.TRS(new Vector3(lip.x, upper + .025f, lip.z), rotation * Quaternion.Euler((Rand(edge.Key, 805) - .5f) * 4f, Rand(edge.Key, 75) * 6f - 3f, (Rand(edge.Key, 806) - .5f) * 3f), new Vector3(edge.DressLength / 2f, .88f + Rand(edge.Key, 807) * .20f, 1.05f)));
                }
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
        /// <summary>Loose litter on open ground: branches and boards everywhere, spent cases near the trenches. One
        /// candidate per 7 m square, placed by hash so it never moves between rebuilds.</summary>
        void Debris(MapData map, BattlefieldSurface surface)
        {
            const float grid = 4.5f;
            int k = 0;
            for (float gz = 4f; gz < map.SizeMeters.y - 4f; gz += grid)
            for (float gx = 3f; gx < map.SizeMeters.x - 3f; gx += grid, k++)
            {
                float x = gx + Rand(k, 62) * (grid - 1f), z = gz + Rand(k, 63) * (grid - 1f);
                // Litter gathers: thick in some stretches and absent in others (a slow noise), thicker again round shell
                // holes and along the trench, where things get thrown, dropped and blown.
                float gather = Mathf.PerlinNoise(x * .045f + 17f, z * .045f + 3f);
                var near = surface.At(x, z);
                float chance = gather * gather * 1.15f + (near.Hollow >= 0 ? .45f : 0f) + (near.BankDistance < 7f ? .30f : 0f);
                if (Rand(k, 61) > chance) continue;
                var layer = (NavLayer)map.NavLayers[map.NavIndex(Mathf.Clamp((int)(x / MapData.NavCellSize), 0, map.NavWidth - 1), Mathf.Clamp((int)(z / MapData.NavCellSize), 0, map.NavLength - 1))];
                if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked | NavLayer.Wire)) != 0) continue;
                var at = surface.At(x, z);
                if (at.Wetness > .35f || at.BankDistance < 1.2f) continue;
                float pick = Rand(k, 64);
                var module = at.BankDistance < 7f && pick < .45f ? kit.shellCases : pick < .72f ? kit.branches : kit.looseBoards;
                emit(module, Matrix4x4.TRS(new Vector3(x, surface.VisualHeight(x, z) + .01f, z), Quaternion.Euler(0f, Rand(k, 65) * 360f, 0f), Vector3.one * (.85f + Rand(k, 66) * .5f)));
            }
        }

        bool Open(MapData map, BattlefieldSurface surface, float x, float z)
        {
            if (x < 1f || z < 1f || x > map.SizeMeters.x - 1f || z > map.SizeMeters.y - 1f) return false;
            var layer = (NavLayer)map.NavLayers[map.NavIndex(Mathf.Clamp((int)(x / MapData.NavCellSize), 0, map.NavWidth - 1), Mathf.Clamp((int)(z / MapData.NavCellSize), 0, map.NavLength - 1))];
            if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked)) != 0) return false;
            var at = surface.At(x, z);
            return at.Wetness < .3f && at.BankDistance > 1.3f && at.Hollow < 0;
        }

        /// <summary>One gathering of small and medium shapes round a big one: scrub close in, grass and stones thinning
        /// outward (distance grows with the square root of a uniform roll, so the middle is densest).</summary>
        void Gather(MapData map, BattlefieldSurface surface, Vector3 heart, int key, float reach, int scrub, int small)
        {
            for (int i = 0; i < scrub + small; i++)
            {
                int k = key * 32 + i;
                bool medium = i < scrub;
                float far = medium ? .9f + Rand(k, 91) * reach * .55f : (.4f + Mathf.Sqrt(Rand(k, 91)) * reach), angle = Rand(k, 92) * Mathf.PI * 2f;
                float x = heart.x + Mathf.Cos(angle) * far, z = heart.z + Mathf.Sin(angle) * far;
                if (!Open(map, surface, x, z)) continue;
                var module = medium ? kit.bush : Rand(k, 93) < .78f ? kit.tuft : kit.stones;
                float size = medium ? .8f + Rand(k, 94) * .7f : .7f + Rand(k, 94) * .9f;
                emit(module, Matrix4x4.TRS(new Vector3(x, surface.VisualHeight(x, z) - .02f, z), Quaternion.Euler(0f, Rand(k, 95) * 360f, 0f), Vector3.one * size));
            }
        }

        /// <summary>Big, medium, small (owner, 2026-09-21): every big shape on the field gathers smaller ones round it,
        /// and the dry rises grow their own patches. Nothing here is on a grid: patch centres are dart-thrown with a
        /// minimum spacing, members fall off from the middle.</summary>
        void Clumps(MapData map, BattlefieldSurface surface)
        {
            for (int i = 0; i < map.Props.Length; i++)
            {
                var p = map.Props[i];
                if (p.Kind == PropKind.Bridge) continue;
                bool big = p.Kind == PropKind.Wreck || p.Scale >= 1.1f;
                if (big) Gather(map, surface, p.Pos, i + 1, p.Kind == PropKind.Wreck ? 5.5f : 4.2f, 1 + (int)(Rand(i, 96) * 3f), 6 + (int)(Rand(i, 97) * 7f));
                else if (p.Scale >= .75f && Rand(i, 98) < .5f) Gather(map, surface, p.Pos, i + 1, 2.2f, 0, 2 + (int)(Rand(i, 97) * 4f));
            }
            for (int i = 0; i < surface.Hollows.Count; i++)
            {
                if (Rand(i, 99) > .55f) continue;   // fresh holes are bare; old ones have grown a fringe
                var h = surface.Hollows[i];
                float angle = Rand(i, 100) * Mathf.PI * 2f;   // on one side of the rim, not a wreath
                var heart = new Vector3(h.Center.x + Mathf.Cos(angle) * (h.Radius + 1.2f), 0f, h.Center.y + Mathf.Sin(angle) * (h.Radius + 1.2f));
                Gather(map, surface, heart, 5000 + i, 2.6f, Rand(i, 101) < .4f ? 1 : 0, 3 + (int)(Rand(i, 102) * 5f));
            }
            // free patches on the dry rises: dart-throwing with a 9 m minimum spacing
            var patches = new List<Vector2>();
            int darts = (int)(map.SizeMeters.x * map.SizeMeters.y / 40f);
            for (int d = 0; d < darts; d++)
            {
                var c = new Vector2(2f + Rand(d, 103) * (map.SizeMeters.x - 4f), 2f + Rand(d, 104) * (map.SizeMeters.y - 4f));
                if (BattlefieldSurface.Mound(c.x, c.y) < .08f || !Open(map, surface, c.x, c.y)) continue;
                bool crowded = false;
                foreach (var other in patches) if ((other - c).sqrMagnitude < 81f) { crowded = true; break; }
                if (crowded) continue;
                patches.Add(c);
                Gather(map, surface, new Vector3(c.x, 0f, c.y), 9000 + d, 3.4f, Rand(d, 105) < .6f ? 1 : 2, 5 + (int)(Rand(d, 106) * 8f));
            }
        }

        void MapProps(MapData map)
        {
            var hf = map.Height;
            for (int i = 0; i < map.Props.Length; i++)
            {
                var p = map.Props[i];
                float s = p.Scale > 0f ? p.Scale : 0.85f + 0.3f * ((i * 37) % 100) / 100f;   // the generator sizes clump members: big, medium, small
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
                int key = z * map.NavWidth + x;
                if (Rand(key, 31) < .07f) continue;   // a frame carried off or blown to bits
                // stand each frame along the belt's own direction (where is the wire in the next column?), then
                // shove, turn and size it a little: set out by tired men at night, not by a surveyor
                float lean = 0f;
                for (int dz = -2; dz <= 2; dz++)
                {
                    int zz = z + dz;
                    if (x + 1 < map.NavWidth && zz >= 0 && zz < map.NavLength && (map.NavLayers[map.NavIndex(x + 1, zz)] & (byte)NavLayer.Wire) != 0) { lean = dz; if (dz == 0) break; }
                }
                float wx = (x + 0.5f) * n + (Rand(key, 32) - .5f) * 1.1f, wz = (z + 0.5f) * n + (Rand(key, 33) - .5f) * 1.3f;
                float yaw = -Mathf.Atan2(lean * n, n) * Mathf.Rad2Deg * .6f + (Rand(key, 34) - .5f) * 30f;
                bool down = Rand(key, 35) < .13f;
                var turn = Quaternion.Euler(down ? 62f + Rand(key, 36) * 25f : (Rand(key, 36) - .5f) * 14f, yaw, (Rand(key, 37) - .5f) * 10f);
                float size = .85f + Rand(key, 38) * .35f;
                var at = new Vector3(wx, RenderGround.Sample(map, wx, wz) + (down ? .15f : 0f), wz);
                emit(kit.knifeRest, Matrix4x4.TRS(at, turn, Vector3.one * size));
                if (!down) emit(kit.wire, Matrix4x4.TRS(at, turn, Vector3.one * size));
            }

            Horizon(map);

        }

        void Horizon(MapData map)
        {
            float w = map.SizeMeters.x, l = map.SizeMeters.y;
            // clumps, as on the field: a big tree, a couple of medium ones close by, small stuff round them
            for (int c = 0; c < 110; c++)
            {
                float cx = -260f + Rand(c, 1) * (w + 520f), cz = -200f + Rand(c, 2) * (l + 400f);
                int members = 3 + (int)(Rand(c, 3) * 6f);
                for (int i = 0; i < members; i++)
                {
                    int key = c * 16 + i;
                    float reach = i == 0 ? 0f : i < 3 ? 3f + Rand(key, 4) * 4f : 5f + Rand(key, 4) * 9f, angle = Rand(key, 5) * Mathf.PI * 2f;
                    float x = cx + Mathf.Cos(angle) * reach, z = cz + Mathf.Sin(angle) * reach;
                    float outside = Mathf.Max(Mathf.Max(-x, x - w), Mathf.Max(-z, z - l));
                    if (outside < 5f) continue;
                    float level = GreyboxTerrainView.SkirtHeight(map, x, z) - 0.05f;
                    float s = i == 0 ? 1.3f + Rand(key, 6) * .5f : i < 3 ? .85f + Rand(key, 6) * .3f : .5f + Rand(key, 6) * .3f;
                    var m = Matrix4x4.TRS(new Vector3(x, level, z), Quaternion.Euler(0f, Rand(key, 7) * 360f, 0f), new Vector3(s, s, s));
                    if (i == 0) emit(Rand(key, 8) < .4f ? kit.fork : kit.trunk, m);
                    else if (i < 3) emit(Rand(key, 8) < .5f ? kit.trunk : kit.snag, m);
                    else emit(Rand(key, 8) < .45f ? kit.stump : Rand(key, 8) < .75f ? kit.bush : kit.snag, m);
                }
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
