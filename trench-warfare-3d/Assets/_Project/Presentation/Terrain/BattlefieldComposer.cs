// Phase: B2 (seeded read-only placement; no rendering, mesh construction or simulation writes)
// One class in six files (docs/21 phase 2): this one holds the steps, the sites and the map's own props; .Trench.cs
// the lining; .Ground.cs debris, litter, gatherings, margins and the winter ground; .Landmarks.cs the landmarks and
// the horizon; .Buildings.cs the village and the rear; .Scatter.cs the rule-based grass, flowers and camp scatter.
using System;
using System.Collections.Generic;
using UnityEngine;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public sealed partial class BattlefieldComposer
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
        readonly BattlefieldBackdrop backdrop;
        readonly BattlefieldBlueprint[] blueprints;
        readonly int seed;
        readonly Vector3 preferredFront;
        readonly List<Site> sites = new List<Site>();
        /// <summary>A village house as placed: its matrix (origin at the middle of its base) and the footprint it keeps clear.</summary>
        public readonly struct Hamlet
        {
            public readonly int House; public readonly Matrix4x4 Matrix; public readonly Vector3 Centre; public readonly float Radius;
            public Hamlet(int house, Matrix4x4 matrix, Vector3 centre, float radius) { House = house; Matrix = matrix; Centre = centre; Radius = radius; }
        }
        readonly List<Hamlet> hamlets = new List<Hamlet>();
        public IReadOnlyList<Hamlet> Hamlets => hamlets;
        /// <summary>How far a house keeps from the line of the bridge (the road over it), beyond its own half size.</summary>
        public const float RoadClear = 4.5f;
        readonly Dictionary<string, int> rejections = new Dictionary<string, int>();
        public string PlacementReport => string.Join(", ", rejections);
        public IReadOnlyList<Site> Sites => sites;
        Action<BattlefieldKit.Module, Matrix4x4> emit;
        MapData layoutMap;
        public BattlefieldComposer(BattlefieldKit kit, int seed = 1917, BattlefieldBlueprint[] blueprints = null, Vector3? preferredFront = null)
        { this.kit = kit; this.seed = seed; this.blueprints = blueprints ?? BattlefieldBlueprint.Stock(kit); this.preferredFront = preferredFront ?? Vector3.right; backdrop = new BattlefieldBackdrop(kit, seed); }

        public void Build(MapData map, BattlefieldSurface surface, Action<BattlefieldKit.Module, Matrix4x4> emit, bool regenerateLayout = false)
        {
            foreach (var _ in BuildSteps(map, surface, emit, regenerateLayout)) { }
        }

        /// <summary>
        /// Build, a generator at a time: each step runs one of them and yields, so a recomposition after a crater can be
        /// spread over frames (BattlefieldProps). The whole of it was measured at 21.5 ms in the editor on 2026-09-23,
        /// the largest single step (Landmarks) at 6.1. Run to the end, the steps are Build exactly: the same generators in
        /// the same order with the same state. The first layout (hamlets, rear, sites) is placed once, as in Build.
        /// </summary>
        public IEnumerable<bool> BuildSteps(MapData map, BattlefieldSurface surface, Action<BattlefieldKit.Module, Matrix4x4> emit, bool regenerateLayout = false)
        {
            this.emit = emit;
            MapProps(map, surface); yield return true;
            TrenchKit(map, surface); yield return true;
            Debris(map, surface); yield return true;
            Litter(map, surface); yield return true;
            Clumps(map, surface); yield return true;
            Margins(map, surface); yield return true;
            if (!ReferenceEquals(layoutMap, map) || regenerateLayout)
            { sites.Clear(); rejections.Clear(); PlaceHamlets(map, surface); PlaceRear(map, surface); PlaceSites(map, surface); layoutMap = map; }
            else
            {
                // Combat updates never run the global candidate search or relocate a site, and never take one away: a shelter
                // stands through a barrage (owner, 2026-09-23: "the shelter should have the sandbags blown off, but otherwise
                // stay fine"; PropDestruction blows the bags off). A site whose ground still fits settles onto it; one whose
                // ground a crater has torn up keeps the height it was built at rather than vanishing, as it used to.
                for (int i = sites.Count - 1; i >= 0; i--)
                {
                    var site = sites[i]; var at = site.Position; at.y = map.Height.Sample(at.x, at.z) - .10f;
                    if (!Fits(map, surface, site.Blueprint, at, site.Rotation)) continue;
                    sites[i] = new Site(site.Blueprint, at, site.Rotation, site.Trench, site.ApproachEnd);
                }
                foreach (var site in sites) EmitSite(map, surface, site);
            }
            // the houses stand where they were first put, at the height they were built at: a crater under one does not
            // move it, and PropDestruction takes its chunks out one by one
            foreach (var h in hamlets)
            {
                var house = kit.Houses[h.House];
                if (house.Whole != null) emit(house.Whole, h.Matrix);   // what is drawn: the whole house, its fallen chunks masked off
                foreach (var chunk in house.Chunks) emit(chunk.Module, HouseKit.Place(h.Matrix, chunk));   // what is hit, hidden and remembered
                if (SceneTints.Now.Frozen && kit.icicles != null) Icicles(h);
            }
            yield return true;
            Landmarks(map, surface); yield return true;
            Scatter(map, surface); yield return true;   // after the footprints that mask it (docs/21 phase 2)
            if (SceneTints.Now.Frozen) { SnowGround(map, surface); yield return true; }
            backdrop.Build(map, surface, emit);   // the lines run on past the flanks, and the edges are closed off
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
                foreach (var h in hamlets) if (Vector2.Distance(new Vector2(at.x, at.z), new Vector2(h.Centre.x, h.Centre.z)) < h.Radius + 6f) crowded = true;
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

    }
}
