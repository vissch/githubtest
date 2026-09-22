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
            MapProps(map, surface);
            TrenchKit(map, surface);
            Debris(map, surface);
            Litter(map, surface);
            Clumps(map, surface);
            Margins(map, surface);
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
            Landmarks(map, surface);
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
                    if (Rand(edge.Key, 815) < .35f)
                    {
                        // a board by the way out says where it leads
                        var post = center + outward * 1.1f + (rotation * Vector3.right) * (Rand(edge.Key, 816) < .5f ? 1.0f : -1.0f);
                        emit(kit.signBoard, Matrix4x4.TRS(new Vector3(post.x, surface.VisualHeight(post.x, post.z) - .05f, post.z), rotation * Quaternion.Euler(0f, 180f + (Rand(edge.Key, 817) - .5f) * 30f, 0f), Vector3.one));
                    }
                    continue;
                }
                center = edge.DressCenter; outward = edge.DressOutward; rotation = Quaternion.LookRotation(outward);
                var wall = center - outward * .08f;
                var lip = center + outward * (.60f + (Rand(edge.Key, 804) - .5f) * .12f);
                float upper = surface.VisualHeight(lip.x, lip.z);
                float height = Mathf.Clamp((upper - floor) / 2f, .55f, 1.3f);
                emit(kit.TrenchWalls[variant], Matrix4x4.TRS(new Vector3(wall.x, floor, wall.z), rotation * Quaternion.Euler(-4f - Rand(edge.Key, 73) * 3f, 0f, 0f), new Vector3(edge.DressLength / 2f, height, 1f)));
                // the trench is lived in (small kit, close camera only): rifles stood against the wall, tins on a nail, a
                // bucket on the boards, and the telephone wire stapled along the revetment in sagging lengths
                var along = rotation * Vector3.right;
                float depth = Mathf.Max(.8f, upper - floor), life = Rand(edge.Key, 811);
                if (life < .10f) emit(kit.leanRifle, Matrix4x4.TRS(new Vector3(wall.x, floor + .04f, wall.z) - outward * .30f + along * ((Rand(edge.Key, 812) - .5f) * edge.DressLength * .5f), rotation * Quaternion.Euler(13f, 0f, 0f), Vector3.one));
                else if (life < .16f) emit(kit.hangingTins, Matrix4x4.TRS(new Vector3(wall.x, floor + depth * .72f, wall.z) - outward * .16f + along * ((Rand(edge.Key, 812) - .5f) * edge.DressLength * .5f), rotation, Vector3.one));
                else if (life < .21f) emit(kit.bucket, Matrix4x4.TRS(new Vector3(wall.x, floor + .05f, wall.z) - outward * .45f + along * ((Rand(edge.Key, 812) - .5f) * edge.DressLength * .5f), Quaternion.Euler(0f, Rand(edge.Key, 813) * 360f, 0f), Vector3.one));
                if (Rand(edge.Key, 814) < .62f) emit(kit.phoneWire, Matrix4x4.TRS(new Vector3(wall.x, floor + depth * .84f, wall.z) - outward * .13f, Quaternion.LookRotation(outward) , new Vector3(edge.DressLength / 2f, 1f, 1f)));
                float frontage = Mathf.PerlinNoise(center.x * .09f + 19f, center.z * .09f + 7f);
                if (frontage > .26f)
                {
                    int course = frontage > .62f ? 0 : frontage > .43f ? 1 : 2;
                    if (Rand(edge.Key, 808) < .12f)
                        // a length held up with gabions instead: two wicker baskets of earth on the lip
                        for (int g = -1; g <= 1; g += 2)
                        {
                            var basket = lip + along * (g * edge.DressLength * .26f) + outward * .08f;
                            emit(kit.gabion, Matrix4x4.TRS(new Vector3(basket.x, surface.VisualHeight(basket.x, basket.z) - .12f, basket.z),
                                Quaternion.Euler((Rand(edge.Key + g, 809) - .5f) * 6f, Rand(edge.Key + g, 810) * 360f, 0f), Vector3.one * (.76f + Rand(edge.Key + g, 818) * .12f)));
                        }
                    else emit(kit.TrenchBags[course], Matrix4x4.TRS(new Vector3(lip.x, upper + .025f, lip.z), rotation * Quaternion.Euler((Rand(edge.Key, 805) - .5f) * 4f, Rand(edge.Key, 75) * 6f - 3f, (Rand(edge.Key, 806) - .5f) * 3f), new Vector3(edge.DressLength / 2f, .88f + Rand(edge.Key, 807) * .20f, 1.05f)));
                }
            }
        }

        void PlaceSites(MapData map, BattlefieldSurface surface)
        {
            if (blueprints.Length == 0) return;
            var candidates = new List<BattlefieldSurface.Edge>(surface.Edges);
            candidates.Sort((a, b) => { int order = Rand(a.Key, seed).CompareTo(Rand(b.Key, seed)); return order != 0 ? order : a.Key.CompareTo(b.Key); });
            var count = new int[map.Trenches.Length];
            var bunkers = new List<BattlefieldBlueprint>(); var others = new List<BattlefieldBlueprint>();
            foreach (var blueprint in blueprints) (IsBunker(blueprint) ? bunkers : others).Add(blueprint);

            // the bunkers first (owner, 2026-09-22): each side's shelter and pillbox behind its line on the stretch nearest
            // the far edge, where the fog lies, the opening turned towards the fog and the back to the field
            var farFirst = new List<BattlefieldSurface.Edge>(candidates);
            farFirst.Sort((a, b) => Vector3.Dot(a.Center, preferredFront).CompareTo(Vector3.Dot(b.Center, preferredFront)));
            for (int side = 0; side < 2; side++)
                foreach (var blueprint in bunkers)
                    foreach (var edge in farFirst)
                    {
                        var trench = map.Trenches[edge.Trench];
                        var facing = new Vector3(Mathf.Sin(trench.FacingYaw), 0f, Mathf.Cos(trench.FacingYaw));
                        if (edge.Link || count[edge.Trench] >= 2 || (facing.z > 0f ? 0 : 1) != side || Vector3.Dot(edge.Outward, facing) > -.5f) continue;
                        if (TrySite(map, surface, edge, blueprint, true, count)) break;
                    }

            if (others.Count == 0) return;
            foreach (var edge in candidates)
            {
                if (edge.Link || count[edge.Trench] >= 2) continue;
                TrySite(map, surface, edge, others[(edge.Trench + count[edge.Trench]) % others.Count], false, count);
            }
        }

        bool IsBunker(BattlefieldBlueprint blueprint)
        {
            foreach (var socket in blueprint.Sockets) if (socket.Name == "shell" && (socket.Module == kit.sodShelter || socket.Module == kit.pillbox)) return true;
            return false;
        }

        bool TrySite(MapData map, BattlefieldSurface surface, BattlefieldSurface.Edge edge, BattlefieldBlueprint blueprint, bool towardsFog, int[] count)
        {
            // Splayed entrances still face the trench, with the readable side chosen by the art direction; a bunker turns
            // the other way, its opening towards the fog.
            var left = edge.Rotation * Quaternion.Euler(0f, -60f, 0f);
            var right = edge.Rotation * Quaternion.Euler(0f, 60f, 0f);
            bool leftReads = Vector3.Dot(left * Vector3.back, preferredFront) > Vector3.Dot(right * Vector3.back, preferredFront);
            var rotation = leftReads != towardsFog ? left : right;
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
            if (!found) { rejections.TryGetValue(reason, out int rejected); rejections[reason] = rejected + 1; return false; }
            var site = new Site(blueprint, at, rotation, edge.Trench, edge.Center + edge.Outward * 1.8f);
            sites.Add(site); count[edge.Trench]++; EmitSite(map, surface, site);
            return true;
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
        /// <summary>Loose litter on open ground: branches, boards and braced planks everywhere, now and then a door or a
        /// crossed pair of boards laid flat; spent cases, fallen sacks and sheets of corrugated iron near the trenches. One
        /// candidate per 4.5 m square, placed by hash so it never moves between rebuilds.</summary>
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
                float pick = Rand(k, 64), ground = surface.VisualHeight(x, z);
                var yaw = Quaternion.Euler(0f, Rand(k, 65) * 360f, 0f);
                var tilt = Quaternion.Euler((Rand(k, 67) - .5f) * 8f, 0f, (Rand(k, 68) - .5f) * 8f);   // bedded unevenly in the mud
                float size = .85f + Rand(k, 66) * .5f;
                if (at.BankDistance < 7f)
                {
                    // by the trench: spent cases, sacks fallen off the parapet, sheets of iron, boards
                    if (pick < .30f) emit(kit.shellCases, Matrix4x4.TRS(new Vector3(x, ground + .01f, z), yaw, Vector3.one * size));
                    else if (pick < .44f) emit(kit.sandbag, Matrix4x4.TRS(new Vector3(x, ground - .04f, z), yaw * tilt, Vector3.one * (.9f + Rand(k, 66) * .25f)));
                    else if (pick < .53f) Sheet(x, z, ground, yaw, k);
                    else if (pick < .70f) emit(kit.branches, Matrix4x4.TRS(new Vector3(x, ground + .01f, z), yaw, Vector3.one * size));
                    else if (pick < .85f) emit(kit.looseBoards, Matrix4x4.TRS(new Vector3(x, ground + .01f, z), yaw, Vector3.one * size));
                    else emit(kit.bracedPlank, Matrix4x4.TRS(new Vector3(x, ground - .10f, z), yaw * tilt, Vector3.one * (.8f + Rand(k, 66) * .3f)));
                }
                else
                {
                    // planks everywhere: broken branches and boards, braced planks, a door off some farm, a crossed pair of boards
                    if (pick < .40f) emit(kit.branches, Matrix4x4.TRS(new Vector3(x, ground + .01f, z), yaw, Vector3.one * size));
                    else if (pick < .62f) emit(kit.looseBoards, Matrix4x4.TRS(new Vector3(x, ground + .01f, z), yaw, Vector3.one * size));
                    else if (pick < .84f) emit(kit.bracedPlank, Matrix4x4.TRS(new Vector3(x, ground - .10f, z), yaw * tilt, Vector3.one * (.8f + Rand(k, 66) * .3f)));
                    else if (pick < .93f) Flat(kit.plankDoor, x, z, ground, yaw * tilt, .78f + Rand(k, 66) * .14f, .25f);
                    else Flat(kit.crossedBoards, x, z, ground, yaw * tilt, .75f + Rand(k, 66) * .2f, .4f);
                }
            }
        }

        /// <summary>An upright imported piece laid on its back (its front face up), thinned to thickness of its depth.</summary>
        void Flat(BattlefieldKit.Module module, float x, float z, float ground, Quaternion rotation, float size, float thickness)
            => emit(module, Matrix4x4.TRS(new Vector3(x, ground + .05f, z), rotation * Quaternion.Euler(-90f, 0f, 0f), new Vector3(size, size, size * thickness)));

        /// <summary>A curved sheet of corrugated iron thrown down arch-up, half sunk in the mud.</summary>
        void Sheet(float x, float z, float ground, Quaternion yaw, int k)
        {
            float size = .6f + Rand(k, 69) * .18f;
            emit(kit.corrugated, Matrix4x4.TRS(new Vector3(x, ground + 1.35f * size * .45f, z), yaw * Quaternion.Euler((Rand(k, 70) - .5f) * 12f, 0f, 180f), Vector3.one * size));
        }

        /// <summary>
        /// What men drop and leave (small kit, close camera only): helmets, boots, mess tins, tools and opened ammunition
        /// tins, thick along the trenches and round the shell holes, thin in between; now and then a rifle stood in the
        /// ground with a helmet on it. One candidate per 3.4 m square, placed by hash so it never moves between rebuilds.
        /// </summary>
        void Litter(MapData map, BattlefieldSurface surface)
        {
            const float grid = 3.4f;
            int k = 0;
            for (float gz = 4f; gz < map.SizeMeters.y - 4f; gz += grid)
            for (float gx = 3f; gx < map.SizeMeters.x - 3f; gx += grid, k++)
            {
                float x = gx + Rand(k, 132) * (grid - .6f), z = gz + Rand(k, 133) * (grid - .6f);
                var at = surface.At(x, z);
                bool byTrench = at.BankDistance < 6f;
                float gather = Mathf.PerlinNoise(x * .06f + 41f, z * .06f + 9f);
                float chance = gather * gather * .34f + (at.Hollow >= 0 ? .22f : 0f) + (byTrench ? .34f : 0f);
                if (Rand(k, 131) > chance) continue;
                var layer = (NavLayer)map.NavLayers[map.NavIndex(Mathf.Clamp((int)(x / MapData.NavCellSize), 0, map.NavWidth - 1), Mathf.Clamp((int)(z / MapData.NavCellSize), 0, map.NavLength - 1))];
                if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked)) != 0) continue;
                if (at.Wetness > .35f || at.BankDistance < 1.1f) continue;
                float pick = Rand(k, 134);
                BattlefieldKit.Module module;
                if (byTrench) module = pick < .28f ? kit.ammoTin : pick < .50f ? kit.messKit : pick < .68f ? kit.spade : pick < .84f ? kit.helmet : pick < .93f ? kit.boots : kit.hatchLid;
                else module = pick < .07f && at.Hollow < 0 ? kit.graveMarker : pick < .50f ? kit.helmet : pick < .66f ? kit.boots : pick < .82f ? kit.spade : kit.messKit;
                float sink = module == kit.graveMarker || module == kit.spade ? .06f : .015f;
                emit(module, Matrix4x4.TRS(new Vector3(x, surface.VisualHeight(x, z) - sink, z), Quaternion.Euler(0f, Rand(k, 135) * 360f, 0f), Vector3.one * (.92f + Rand(k, 136) * .2f)));
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
                // medium: scrub, a rock, a mossy stump; small: the old dry tufts and stones with the painted grass among
                // them everywhere, and here and there poppies
                BattlefieldKit.Module module; float size;
                float pick = Rand(k, 93);
                if (medium)
                {
                    module = pick < .62f ? kit.bush : pick < .86f ? kit.boulder : kit.stumpMoss;
                    size = module == kit.bush ? .8f + Rand(k, 94) * .7f : module == kit.boulder ? .38f + Rand(k, 94) * .3f : .55f + Rand(k, 94) * .3f;
                }
                else
                {
                    module = pick < .50f ? kit.tuft : pick < .74f ? kit.grass : pick < .95f ? kit.stones : kit.poppies;
                    size = module == kit.grass ? .45f + Rand(k, 94) * .5f : module == kit.poppies ? .7f + Rand(k, 94) * .4f : .7f + Rand(k, 94) * .9f;
                }
                emit(module, Matrix4x4.TRS(new Vector3(x, surface.VisualHeight(x, z) - (module == kit.boulder ? .10f : .02f), z), Quaternion.Euler(0f, Rand(k, 95) * 360f, 0f), Vector3.one * size));
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

        /// <summary>
        /// A spawn rule, the way a world generator scatters a biome: reeds grow where the ground is only just above
        /// standing water (the river's margin, the rim of a flooded shell hole, the wet end of a drainage line) and
        /// nowhere else. Stands are dart-thrown 3.5 m apart and each is a big / medium / small family of stems.
        /// </summary>
        void Margins(MapData map, BattlefieldSurface surface)
        {
            var stands = new List<Vector2>();
            int darts = (int)(map.SizeMeters.x * map.SizeMeters.y / 9f), planted = 0;
            bool river = map.WaterLevel > MapData.NoWater;
            for (int d = 0; d < darts && planted < 320; d++)
            {
                var c = new Vector2(2f + Rand(d, 121) * (map.SizeMeters.x - 4f), 2f + Rand(d, 122) * (map.SizeMeters.y - 4f));
                var layer = (NavLayer)map.NavLayers[map.NavIndex(Mathf.Clamp((int)(c.x / MapData.NavCellSize), 0, map.NavWidth - 1), Mathf.Clamp((int)(c.y / MapData.NavCellSize), 0, map.NavLength - 1))];
                if ((layer & (NavLayer.Trench | NavLayer.Link)) != 0) continue;
                var at = surface.At(c.x, c.y);
                if (at.BankDistance < 2.5f) continue;
                float bed = surface.Bed(c.x, c.y);
                bool shore = river && bed > map.WaterLevel + .03f && bed < map.WaterLevel + .40f;
                bool rim = false;
                if (!shore && at.Hollow >= 0)
                {
                    var h = surface.Hollows[at.Hollow];
                    float r = (c - h.Center).magnitude / h.Radius;
                    rim = h.Level > -100f && r > .80f && r < 1.12f && bed > h.Level - .05f;
                }
                bool seep = !shore && !rim && at.Hollow < 0 && surface.Rill(c.x, c.y) > .7f && Rand(d, 123) < .35f;
                if (!shore && !rim && !seep) continue;
                bool crowded = false;
                foreach (var other in stands) if ((other - c).sqrMagnitude < 12.25f) { crowded = true; break; }
                if (crowded) continue;
                stands.Add(c);
                int stems = 3 + (int)(Rand(d, 124) * 6f);
                for (int k = 0; k < stems; k++)
                {
                    float far = Mathf.Sqrt(Rand(d * 16 + k, 125)) * 1.3f, angle = Rand(d * 16 + k, 126) * Mathf.PI * 2f;
                    float x = c.x + Mathf.Cos(angle) * far, z = c.y + Mathf.Sin(angle) * far;
                    if (x < 1f || z < 1f || x > map.SizeMeters.x - 1f || z > map.SizeMeters.y - 1f) continue;
                    float size = k == 0 ? 1.25f + Rand(d, 127) * .35f : k < 3 ? .85f + Rand(d * 16 + k, 128) * .3f : .5f + Rand(d * 16 + k, 128) * .3f;
                    // most stands grow round a clump of cattails; the old reeds make up the rest of the family
                    if (k == 0 && Rand(d, 130) < .65f) emit(kit.cattails, Matrix4x4.TRS(new Vector3(x, surface.Bed(x, z) - .04f, z), Quaternion.Euler(0f, Rand(d * 16 + k, 129) * 360f, 0f), Vector3.one * (.75f + Rand(d, 127) * .35f)));
                    else emit(kit.reeds, Matrix4x4.TRS(new Vector3(x, surface.Bed(x, z) - .04f, z), Quaternion.Euler(0f, Rand(d * 16 + k, 129) * 360f, 0f), Vector3.one * size));
                    planted++;
                }
            }
        }

        void MapProps(MapData map, BattlefieldSurface surface)
        {
            var hf = map.Height;
            for (int i = 0; i < map.Props.Length; i++)
            {
                var p = map.Props[i];
                float s = p.Scale > 0f ? p.Scale : 0.85f + 0.3f * ((i * 37) % 100) / 100f;   // the generator sizes clump members: big, medium, small
                var at = new Vector3(p.Pos.x, hf.Sample(p.Pos.x, p.Pos.z) - 0.05f, p.Pos.z);
                var yaw = Quaternion.Euler(0f, p.Yaw * Mathf.Rad2Deg, 0f);
                var m = Matrix4x4.TRS(at, yaw, new Vector3(s, s, s));
                switch (p.Kind)
                {
                    case PropKind.Tree: emit(i % 3 == 0 ? kit.fork : kit.trunk, m); break;
                    case PropKind.BrokenTree: emit(kit.snag, m); emit(kit.fallen, m); break;
                    case PropKind.Stump:
                    {
                        // the old sawn stump among the painted ones: mossed over, splintered, and now and then a tall shard
                        float pick = Rand(i, 140);
                        if (pick < .30f) emit(kit.stump, m);
                        else if (pick < .58f) emit(kit.stumpMoss, Matrix4x4.TRS(at, yaw, Vector3.one * s * .95f));
                        else if (pick < .86f) emit(kit.stumpSplit, Matrix4x4.TRS(at, yaw, Vector3.one * s * .85f));
                        else emit(kit.stumpTall, Matrix4x4.TRS(at, yaw, Vector3.one * s * .75f));
                        break;
                    }
                    case PropKind.Log: emit(kit.fallenLog, Matrix4x4.TRS(at - new Vector3(0f, .06f, 0f), yaw, Vector3.one * s * .9f)); break;
                    case PropKind.Wreck:
                        if (SceneHooks.DrawnWreck != null && SceneHooks.DrawnWreck(p.Pos.x, p.Pos.z)) break;   // TankRenderer draws the tank that died here
                        emit(kit.wreck, Matrix4x4.TRS(new Vector3(p.Pos.x, hf.Sample(p.Pos.x, p.Pos.z) - 0.25f, p.Pos.z), yaw, Vector3.one));
                        if (Rand(i, 141) < .6f)
                        {
                            // its turret, blown off and lying canted a few metres to one side of the hull
                            float side = (p.Yaw + (Rand(i, 142) < .5f ? 1f : -1f) * (1.25f + Rand(i, 143) * .5f));
                            var lying = at + new Vector3(Mathf.Sin(side), 0f, Mathf.Cos(side)) * (3.1f + Rand(i, 144) * 1.2f);
                            if (Open(map, surface, lying.x, lying.z))
                                emit(kit.tankTurret, Matrix4x4.TRS(new Vector3(lying.x, surface.VisualHeight(lying.x, lying.z) - .14f, lying.z),
                                    Quaternion.Euler((Rand(i, 145) - .5f) * 16f, Rand(i, 146) * 360f, (Rand(i, 147) - .5f) * 22f), Vector3.one * (.9f + Rand(i, 148) * .2f)));
                        }
                        break;
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
                // half the belt is still the old knife rests; the rest is the imported obstacles, most with the same
                // four strands run through them (a fence section carries its own)
                float kind = Rand(key, 44);
                bool strung = !down;
                if (down || kind < .50f) emit(kit.knifeRest, Matrix4x4.TRS(at, turn, Vector3.one * size));
                else if (kind < .72f) { emit(kit.wireFence, Matrix4x4.TRS(at, turn, Vector3.one * size * 1.05f)); strung = false; }
                else if (kind < .86f) emit(kit.hedgehog, Matrix4x4.TRS(at, turn * Quaternion.Euler(0f, Rand(key, 45) * 90f, 0f), Vector3.one * size));
                else if (kind < .94f) emit(kit.stakes, Matrix4x4.TRS(at, turn * Quaternion.Euler(0f, (Rand(key, 45) - .5f) * 60f, 0f), Vector3.one * size));
                else
                    for (int end = -1; end <= 1; end += 2)
                        emit(kit.wirePost, Matrix4x4.TRS(at + turn * new Vector3(end * .95f * size, -.08f, 0f), turn * Quaternion.Euler(0f, Rand(key + end, 45) * 90f, 0f), Vector3.one * size));
                if (strung) emit(kit.wire, Matrix4x4.TRS(at, turn, Vector3.one * size));
                if (strung && Rand(key, 39) < .20f)
                {
                    // something has caught on the top strand: a strip of cloth, or the tins hung there to rattle (small kit)
                    var hang = at + turn * (new Vector3((Rand(key, 40) - .5f) * 1.6f, 1.05f, Rand(key, 41) < .5f ? .30f : -.30f) * size);
                    emit(Rand(key, 42) < .6f ? kit.rag : kit.wireTins, Matrix4x4.TRS(hang, Quaternion.Euler(0f, yaw + (Rand(key, 43) - .5f) * 40f, 0f), Vector3.one));
                }
            }

            Horizon(map);

        }

        /// <summary>
        /// The imported landmarks, sparingly (owner, 2026-09-22: "some can be used sparingly like the turrets"): one MG nest
        /// on the enemy-facing parapet of each fire trench, two field guns and an observation stand in each side's rear
        /// corners, one well behind a line, a crashed biplane just beyond the far edge of no man's land, and duds left in
        /// a few dry shell holes. Decoration only, like the site blueprints: none of it is cover or an obstacle, so the
        /// big pieces keep to ground the men do not cross (the parapet between ladders, the rear corners off the road,
        /// outside the map) and every one is dropped rather than moved when the ground under it is no longer fit.
        /// </summary>
        void Landmarks(MapData map, BattlefieldSurface surface)
        {
            float W = map.SizeMeters.x, L = map.SizeMeters.y;
            var taken = new List<Vector3>();
            foreach (var site in sites) taken.Add(site.Position);
            bool Free(Vector3 p, float spacing) { foreach (var t in taken) if ((new Vector2(t.x - p.x, t.z - p.z)).sqrMagnitude < spacing * spacing) return false; return true; }
            void Put(BattlefieldKit.Module module, Vector3 p, Quaternion rotation, float size, float sink)
                => emit(module, Matrix4x4.TRS(new Vector3(p.x, Ground(map, surface, p.x, p.z) - sink, p.z), rotation, Vector3.one * size));
            // half the drawn footprint of a kind: its look's size (Module.Size) spaces what stands round it
            Vector3 Half(BattlefieldKit.Module module) => Vector3.Scale(module.Mesh.bounds.extents, module.Size);

            // MG nests: on the lip of the parapet that faces the enemy, the gun out over no man's land, never by a ladder
            var ladders = new List<Vector3>();
            foreach (var edge in surface.Edges) if (edge.Link) ladders.Add(edge.Center);
            var candidates = new List<BattlefieldSurface.Edge>(surface.Edges);
            candidates.Sort((a, b) => { int order = Rand(a.Key, 850).CompareTo(Rand(b.Key, 850)); return order != 0 ? order : a.Key.CompareTo(b.Key); });
            var nests = new int[map.Trenches.Length];
            var nest = kit.mgNest.Size;
            foreach (var edge in candidates)
            {
                var trench = map.Trenches[edge.Trench];
                if (edge.Link || trench.Kind != 0 || nests[edge.Trench] > 0) continue;
                var facing = new Vector3(Mathf.Sin(trench.FacingYaw), 0f, Mathf.Cos(trench.FacingYaw));
                if (Vector3.Dot(edge.DressOutward, facing) < .92f || edge.DressCenter.x < 10f || edge.DressCenter.x > W - 10f) continue;
                bool byLadder = false;
                foreach (var ladder in ladders) if ((ladder - edge.Center).sqrMagnitude < 36f) { byLadder = true; break; }
                if (byLadder) continue;
                var at = edge.DressCenter + edge.DressOutward * (.66f + 1.29f * nest.z);   // its back on the lip, however deep its look draws it
                var rotation = Quaternion.LookRotation(edge.DressOutward) * Quaternion.Euler(0f, (Rand(edge.Key, 851) - .5f) * 10f, 0f);
                if (!Free(at, Mathf.Max(9f, 3.5f * nest.z)) || !Room(map, surface, at, rotation, .95f * nest.x, 1.3f * nest.z, false, 1.0f * nest.z)) continue;   // it hangs over the parapet's fall: a metre of fall for each time its own size it is drawn
                nests[edge.Trench]++; taken.Add(at);
                Put(kit.mgNest, at, rotation, 1f, .06f);
                Put(kit.sandbag, at + rotation * Vector3.Scale(new Vector3(-1.05f, 0f, 1.25f), nest), rotation * Quaternion.Euler(0f, 70f + Rand(edge.Key, 852) * 30f, 0f), 1f, .04f);
                Put(kit.sandbag, at + rotation * Vector3.Scale(new Vector3(1.1f, 0f, 1.1f), nest), rotation * Quaternion.Euler(0f, -80f - Rand(edge.Key, 853) * 30f, 0f), .95f, .04f);
            }

            // each side's rear, the owner's way (hand placement, 2026-09-22): a field gun on either flank pulled back to the rear
            // edge (its trail may run off the map) and laid toward the enemy, its shells and a sack beside it and one limber
            // behind; and four observation stands, two along the rear edge, half off the map, and two out past the far edge
            // where the fog lies (the side the standard view looks toward). None stands out in the open between the lines.
            // Behind one side, the well of a farm that is no longer there.
            var gun = Half(kit.fieldGun); var stand = Half(kit.armouredStand);
            for (int side = 0; side < 2; side++)
            {
                float ahead = side == 0 ? 1f : -1f, rear = side == 0 ? 0f : L;
                var toward = Quaternion.LookRotation(new Vector3(0f, 0f, ahead));
                for (int flank = 0; flank < 2; flank++)
                    for (int attempt = 0; attempt < 10; attempt++)
                    {
                        int key = side * 64 + flank * 16 + attempt;
                        var at = new Vector3(W * (flank == 0 ? .06f : .84f) + Rand(key, 860) * W * .10f, 0f, rear + ahead * (2.5f + Rand(key, 861) * 6.5f));
                        var rotation = toward * Quaternion.Euler(0f, (Rand(key, 862) - .5f) * 24f, 0f);
                        if (!Free(at, Mathf.Max(7f, gun.z)) || !Room(map, surface, at, rotation, gun.x, gun.z, true)) continue;
                        taken.Add(at);
                        float outer = flank == 0 ? 1f : -1f;
                        Put(kit.fieldGun, at, rotation, 1f, .05f);
                        Put(kit.shellStack, at + rotation * new Vector3(outer * (gun.x + .9f), 0f, -.35f * gun.z), rotation * Quaternion.Euler(0f, 90f + (Rand(key, 863) - .5f) * 30f, 0f), .9f, .04f);
                        Put(kit.sandbag, at + rotation * new Vector3(-outer * (gun.x + .5f), 0f, .3f * gun.z), rotation * Quaternion.Euler(0f, Rand(key, 864) * 180f, 0f), 1f, .04f);
                        if (flank == side) Put(kit.limber, at + rotation * new Vector3(-outer * (gun.x * .5f + .4f), 0f, -(gun.z + 1.8f)), Quaternion.Euler(0f, Rand(key, 865) * 360f, (Rand(key, 866) - .5f) * 10f), .95f, .08f);
                        break;
                    }
                for (int attempt = 0, stands = 0; attempt < 24 && stands < 4; attempt++)
                {
                    int key = side * 64 + 40 + attempt;
                    var at = stands < 2
                        ? new Vector3(W * (.22f + Rand(key, 867) * .72f), 0f, rear + ahead * (-3f + Rand(key, 868) * 7f))
                        : new Vector3(-3f - Rand(key, 867) * 7f, 0f, rear + ahead * (4f + Rand(key, 868) * L * .28f));
                    var rotation = toward * Quaternion.Euler(0f, 45f + (Rand(key, 869) - .5f) * 20f, 0f);
                    if (!Free(at, 14f) || !Room(map, surface, at, rotation, stand.x, stand.z, true)) continue;
                    taken.Add(at); Put(kit.armouredStand, at, rotation, 1f, .05f); stands++;
                }
                if (side == (int)(Rand(seed, 870) * 2f))
                    for (int attempt = 0; attempt < 6; attempt++)
                    {
                        int key = side * 64 + 50 + attempt;
                        var at = new Vector3(W * (.62f + Rand(key, 871) * .18f), 0f, rear + ahead * (4.5f + Rand(key, 872) * 4f));
                        var rotation = Quaternion.Euler(0f, Rand(key, 873) * 360f, 0f);
                        if (!Free(at, 7f) || !Room(map, surface, at, rotation, 1.1f, 1.1f)) continue;
                        taken.Add(at); Put(kit.well, at, rotation, 1f, .08f); break;
                    }
            }

            // a biplane that came down nose-first just beyond the far edge of no man's land (the side the standard view
            // looks toward), where it is seen behind the fighting and never stood in
            {
                float x = -6.5f - Rand(seed, 874) * 5f, z = L * .5f + (Rand(seed, 875) - .5f) * L * .22f;
                var rotation = Quaternion.Euler(0f, 90f + (Rand(seed, 876) - .5f) * 70f, 0f) * Quaternion.Euler(20f + Rand(seed, 877) * 12f, 0f, (Rand(seed, 878) - .5f) * 30f);
                emit(kit.biplane, Matrix4x4.TRS(new Vector3(x, GreyboxTerrainView.SkirtHeight(map, x, z) - .55f, z), rotation, Vector3.one));
            }

            // duds: a shell that did not go off, nose down in the bottom of a dry hole, or in the shallows at the edge of
            // a flooded one (the night look floods most of them), its fins out of the water
            for (int i = 0; i < surface.Hollows.Count; i++)
            {
                var h = surface.Hollows[i];
                bool flooded = h.Level > -100f;
                if (Rand(i, 880) > (flooded ? .14f : .30f) || h.Radius < 1.2f) continue;
                float angle = Rand(i, 881) * Mathf.PI * 2f, off = h.Radius * (flooded ? .72f + .14f * Rand(i, 882) : .3f * Rand(i, 882));
                float x = h.Center.x + Mathf.Cos(angle) * off, z = h.Center.y + Mathf.Sin(angle) * off;
                if (!Clear(map, x, z) || surface.At(x, z).Hollow < 0) continue;
                emit(kit.dudShell, Matrix4x4.TRS(new Vector3(x, surface.Bed(x, z) - .22f, z), Quaternion.Euler((Rand(i, 883) - .5f) * 30f, Rand(i, 884) * 360f, (Rand(i, 885) - .5f) * 30f), Vector3.one * (.6f + Rand(i, 886) * .2f)));
            }
        }

        /// <summary>A rectangle (half sizes in metres, turned by rotation) of dry, fairly level, open ground in the map that no
        /// trench, ladder, wire or blocked cell touches, and not in a shell hole at its middle. With offMap, the part beyond the
        /// edge stands on the skirt and needs nothing else. rise overrides how much the ground may climb under it.</summary>
        bool Room(MapData map, BattlefieldSurface surface, Vector3 centre, Quaternion rotation, float halfX, float halfZ, bool offMap = false, float rise = 0f)
        {
            bool Inside(float x, float z) => x >= 0f && z >= 0f && x < map.SizeMeters.x && z < map.SizeMeters.y;
            if (Inside(centre.x, centre.z) && surface.At(centre.x, centre.z).Hollow >= 0) return false;
            int nx = Mathf.CeilToInt(halfX * 4f), nz = Mathf.CeilToInt(halfZ * 4f);
            float lo = float.MaxValue, hi = float.MinValue;
            for (int iz = 0; iz <= nz; iz++)
            for (int ix = 0; ix <= nx; ix++)
            {
                var p = centre + rotation * new Vector3(Mathf.Lerp(-halfX, halfX, ix / (float)nx), 0f, Mathf.Lerp(-halfZ, halfZ, iz / (float)nz));
                float h;
                if (offMap && !Inside(p.x, p.z)) h = GreyboxTerrainView.SkirtHeight(map, p.x, p.z);   // beyond the edge: the skirt, nothing to keep clear
                else
                {
                    if (!Clear(map, p.x, p.z)) return false;
                    h = surface.VisualHeight(p.x, p.z);
                    if (h < map.WaterLevel + .15f || surface.At(p.x, p.z).Wetness > .45f) return false;
                }
                lo = Mathf.Min(lo, h); hi = Mathf.Max(hi, h);
            }
            return hi - lo < (rise > 0f ? rise : .9f + .12f * Mathf.Max(0f, Mathf.Max(halfX, halfZ) - 1.3f));   // a big prop spans more ground
        }

        /// <summary>The drawn ground on the map or on the land beyond it.</summary>
        static float Ground(MapData map, BattlefieldSurface surface, float x, float z)
            => x >= 0f && z >= 0f && x <= map.SizeMeters.x && z <= map.SizeMeters.y ? surface.VisualHeight(x, z) : GreyboxTerrainView.SkirtHeight(map, x, z);

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
                var turn = Quaternion.Euler(0f, 80f + Rand(i, 9) * 40f, 0f);
                var at = new Vector3(x, GreyboxTerrainView.SkirtLevel - 0.25f, z);
                emit(kit.ruin, Matrix4x4.TRS(at, turn, new Vector3(s, s, s)));
                // what else is left of the place: wall stubs either side, a slab and a low wall in the rubble
                (Vector3 local, BattlefieldKit.Module module, float size)[] rubble =
                {
                    (new Vector3(-10.5f, 0f, 1.5f), kit.wallStub, 1.1f), (new Vector3(7.8f, 0f, -1.8f), kit.wallStub, .9f),
                    (new Vector3(2.4f, 0f, 3.2f), kit.rebarSlab, 1f), (new Vector3(-4.2f, 0f, -3.4f), kit.barricade, 1.1f),
                };
                for (int r = 0; r < rubble.Length; r++)
                {
                    if (Rand(i * 8 + r, 10) < .25f) continue;
                    emit(rubble[r].module, Matrix4x4.TRS(at + turn * (rubble[r].local * s) + new Vector3(0f, .1f, 0f), turn * Quaternion.Euler(0f, (Rand(i * 8 + r, 11) - .5f) * 50f, 0f), Vector3.one * rubble[r].size * s));
                }
            }
        }


    }
}
