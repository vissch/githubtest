// Phase: Playground (2026-09-26, lane/show/playground) — a house from the game's kit, brought down chunk by chunk
// A building from the game's house kit (Resources/Env/<Set>, Tools/housesplit.py, HouseKit), standing in the playground
// as one GameObject a chunk so it can be shelled and watched coming down. What rests on what is HouseKit.Solve's answer,
// the same one the battle uses; the look is the battle's (TW/Toon on the env atlas cell, the kit's paint, outline 1.3).
// A shell knocks out every chunk it hurts past its strength (stone takes more than timber) and throws it; then anything
// whose supports have all gone comes down, a storey at a time (Settle), dropping rather than thrown. Loose chunks fly on
// the same Tumble as a vehicle's parts, in the building's own frame.
using System.Collections.Generic;
using TW.Presentation.Tactical;
using TW.Presentation.Terrain;
using UnityEngine;

namespace TW.Playground
{
    public sealed class BuildingRig : MonoBehaviour
    {
        public sealed class Piece
        {
            public bool Anchored;   // a corner stub: falls only to its own damage, never for want of support
            public HouseKit.Chunk Chunk; public Transform T; public MeshRenderer R; public Bounds Box;
            public float Hp, MaxHp; public bool Loose, Falling; public float FallAt; public Tumble Fly;
            public float Dust;
        }

        public const float Step = 1f / 120f;
        public const float StoreyDelay = 0.45f;   // PropDestruction's pace: a storey comes down this long after the one under it
        public string Set, House;
        public PlaygroundFx Fx;
        public readonly List<Piece> Pieces = new List<Piece>();
        public int Seed = 1;
        public string LastEvent = "";
        public float Radius { get; private set; } = 6f;
        System.Random rng;
        float acc;
        static readonly Dictionary<string, HouseKit.House[]> cache = new Dictionary<string, HouseKit.House[]>();
        static Material[] shared = new Material[0];

        /// <summary>Every building in a kit set (Houses, Military, Ruins, ...), loaded once.</summary>
        public static HouseKit.House[] Kit(string set)
        {
            if (cache.TryGetValue(set, out var h)) return h;
            h = HouseKit.Load(set, chunk => new BattlefieldKit.Module { Mesh = Resources.Load<Mesh>("Env/" + set + "/" + chunk), Name = chunk, Drawn = false }, 0);
            cache[set] = h;
            return h;
        }

        static Material MaterialFor(string set)
        {
            int cell = System.Array.IndexOf(BattlefieldKit.EnvSets, set);
            foreach (var m in shared) if (m != null && m.name == "PG_" + set) return m;
            var mat = new Material(Shader.Find("TW/Toon (URP)")) { name = "PG_" + set, enableInstancing = true };
            mat.SetColor("_BaseColor", new Color(.84f, .83f, .80f));   // BattlefieldKit's "paint": the sets brought into the field's range
            mat.SetFloat("_OutlineWidth", 1.3f);
            var atlas = Resources.Load<Texture2D>("Env/EnvAtlas");
            if (atlas != null && cell >= 0)
            {
                mat.SetTexture("_BaseMap", atlas);
                mat.SetTextureScale("_BaseMap", new Vector2(1f / BattlefieldKit.EnvCols, 1f / BattlefieldKit.EnvRows));
                mat.SetTextureOffset("_BaseMap", BattlefieldKit.EnvOffset(cell));
            }
            var list = new List<Material>(shared) { mat }; shared = list.ToArray();
            return mat;
        }

        public static BuildingRig Build(string set, string house, PlaygroundFx fx, Transform parent, Vector3 at, float yaw, int seed)
        {
            var kit = Kit(set);
            var h = System.Array.Find(kit, x => x.Name == house) ?? (kit.Length > 0 ? kit[0] : null);
            if (h == null) { Debug.LogError("BuildingRig: no buildings in set " + set); return null; }
            var root = new GameObject(set + "/" + h.Name);
            root.transform.SetParent(parent, false);
            root.transform.SetPositionAndRotation(at, Quaternion.Euler(0f, yaw, 0f));
            var rig = root.AddComponent<BuildingRig>();
            rig.Set = set; rig.House = h.Name; rig.Fx = fx; rig.Seed = seed; rig.rng = new System.Random(seed);
            var mat = MaterialFor(set);
            foreach (var c in h.Chunks)
            {
                var go = new GameObject(c.Module.Name);
                go.transform.SetParent(root.transform, false);
                go.transform.localPosition = c.Offset;
                var mesh = c.Module.Mesh;
                go.AddComponent<MeshFilter>().sharedMesh = mesh;
                var r = go.AddComponent<MeshRenderer>(); r.sharedMaterial = mat;
                // the ground floor carries the rest and is built heaviest: three times as strong, and only a burst close to it
                // breaks it (critic r2: eight shells razed a 12 m tower to nothing, no ruin left standing)
                float hp = (c.Timber ? 30f : 60f) * (c.Grounded ? 3f : 1f);
                rig.Pieces.Add(new Piece { Chunk = c, T = go.transform, R = r, Box = mesh != null ? mesh.bounds : new Bounds(Vector3.zero, Vector3.one), Hp = hp, MaxHp = hp });
            }
            rig.Radius = h.Bounds.extents.magnitude;
            rig.AnchorCorners();
            return rig;
        }

        float R(float a, float b) => a + (b - a) * (float)rng.NextDouble();

        /// <summary>The two ground-floor chunks farthest apart, and the chunk stacked on each, become corner stubs: a shelled
        /// building should leave a ruin standing, not a mound.</summary>
        void AnchorCorners()
        {
            var ground = Pieces.FindAll(p => p.Chunk.Grounded);
            if (ground.Count < 2) return;
            Piece a = null, b = null; float best = -1f;
            foreach (var x in ground) foreach (var y in ground)
            {
                float d = (x.Chunk.Local.center - y.Chunk.Local.center).sqrMagnitude;
                if (d > best) { best = d; a = x; b = y; }
            }
            foreach (var corner in new[] { a, b })
            {
                corner.Anchored = true; corner.Hp = corner.MaxHp *= 1.5f;
                foreach (var p in Pieces)
                    if (System.Array.IndexOf(p.Chunk.RestsOn, corner.Chunk.Index) >= 0) { p.Anchored = true; p.Hp = p.MaxHp *= 2f; break; }
            }
        }

        public Vector3 Centre => transform.TransformPoint(new Vector3(0f, Radius * 0.35f, 0f));
        public int Standing { get { int n = 0; foreach (var p in Pieces) if (!p.Loose && !p.Falling) n++; return n; } }

        /// <summary>A shell bursting at a point in the building's frame: every chunk within reach is hurt, those past their
        /// strength are thrown out from the burst, and whatever stood on them comes down after.</summary>
        public void ShellLocal(Vector3 at, float damage, float reach = 4.5f)
        {
            var world = transform.TransformPoint(at);
            Fx?.Burst(world, 1.6f);
            int broke = 0;
            foreach (var p in Pieces)
            {
                if (p.Loose || p.Falling) continue;
                var c = p.Chunk.Local.ClosestPoint(at);
                float d = Vector3.Distance(c, at);
                if (d > reach || (p.Chunk.Grounded && d > 1.5f)) continue;
                p.Hp -= damage * (1f - d / reach);
                if (p.Hp > 0f) continue;
                var centre = p.Chunk.Local.center;
                var away = centre - at; if (away.sqrMagnitude < 1e-4f) away = Vector3.up;
                var v = away.normalized * R(4f, 8f) * (1f - d / reach) + Vector3.up * R(2f, 5f);
                Detach(p, v, new Vector3(R(-1f, 1f), R(-1f, 1f), R(-1f, 1f)) * R(2f, 6f));
                broke++;
            }
            LastEvent = $"shell {damage:0}: {broke} chunks out";
            Settle();
        }

        /// <summary>Anything standing whose supports have all gone falls, one storey after the next.</summary>
        void Settle()
        {
            bool changed = true; int storey = 0;
            var falling = new HashSet<Piece>();
            while (changed)
            {
                changed = false; storey++;
                foreach (var p in Pieces)
                {
                    if (p.Loose || p.Falling || p.Anchored || falling.Contains(p) || p.Chunk.Grounded || p.Chunk.RestsOn.Length == 0) continue;
                    bool held = false;
                    foreach (int j in p.Chunk.RestsOn) { var s = Pieces[j]; if (!s.Loose && !(s.Falling || falling.Contains(s))) { held = true; break; } }
                    if (held) continue;
                    p.Falling = true; p.FallAt = Time.time + storey * StoreyDelay + R(0f, 0.12f);
                    falling.Add(p); changed = true;
                }
            }
        }

        void Detach(Piece p, Vector3 velocity, Vector3 spin)
        {
            if (p.Loose) return;
            p.Loose = true; p.Falling = false;
            p.Fly = new Tumble { Pos = p.T.localPosition, Rot = p.T.localRotation, Vel = velocity, Spin = spin, Nudge = new Vector3(R(-1f, 1f), 0f, R(-1f, 1f)) };
            p.Dust = 1f;
            if (Fx != null && Fx.Debris != null)
                Fx.Debris.Burst(p.Chunk.Timber ? DebrisRenderer.Piece.Plank : DebrisRenderer.Piece.Rubble, p.T.TransformPoint(p.Box.center), 3, 5f, 0.3f,
                                p.Chunk.Timber ? new Color(0.24f, 0.19f, 0.14f) : new Color(0.28f, 0.26f, 0.24f), 20f, 0f, 1.4f, default, (uint)(Seed * 97 + p.Chunk.Index));
        }

        /// <summary>The top of the rubble under a falling chunk: the highest chunk already come to rest whose footprint it
        /// overlaps (and the ground). Rubble heaps up instead of every chunk passing through the others to the ground.</summary>
        float FloorUnder(Piece p)
        {
            var me = new Bounds(p.Fly.Pos + p.Fly.Rot * p.Box.center, p.Box.size);   // near enough: the chunk's box, upright
            float floor = 0f;
            foreach (var q in Pieces)
            {
                if (q == p || !q.Loose || !q.Fly.Resting) continue;
                var b = new Bounds(q.Fly.Pos + q.Fly.Rot * q.Box.center, q.Box.size * 0.8f);
                if (b.max.x < me.min.x || b.min.x > me.max.x || b.max.z < me.min.z || b.min.z > me.max.z) continue;
                if (b.max.y <= me.center.y) floor = Mathf.Max(floor, b.max.y);
            }
            return floor;
        }

        public void Rebuild()
        {
            foreach (var p in Pieces)
            {
                p.Loose = p.Falling = false; p.Hp = p.MaxHp; p.Fly = default;
                p.T.localPosition = p.Chunk.Offset; p.T.localRotation = Quaternion.identity;
            }
            rng = new System.Random(Seed); LastEvent = "rebuilt";
        }

        void Update() => Advance(Time.deltaTime);

        public void Advance(float dt)
        {
            if (dt <= 0f) return;
            foreach (var p in Pieces)
                if (p.Falling && Time.time >= p.FallAt)
                {
                    Detach(p, new Vector3(R(-0.6f, 0.6f), -0.5f, R(-0.6f, 0.6f)), new Vector3(R(-1f, 1f), 0f, R(-1f, 1f)) * R(0.5f, 2f));
                    if (Fx != null) Fx.Smoke(p.T.TransformPoint(p.Box.center), 2.2f, 0.8f, 0.55f, 5f);
                }
            acc += Mathf.Min(dt, 0.1f);
            while (acc >= Step)
            {
                foreach (var p in Pieces)
                {
                    if (!p.Loose || p.Fly.Resting) continue;
                    float impact = p.Fly.Step(p.Box, 1f, Step, FloorUnder(p));
                    p.T.localPosition = p.Fly.Pos; p.T.localRotation = p.Fly.Rot;
                    if (impact > 3f && Fx != null)
                    {
                        var w = p.T.TransformPoint(p.Box.center); w.y = transform.position.y;
                        Fx.Smoke(w, 1.8f + p.Box.extents.magnitude, 0.5f, 0.45f, 4f);
                    }
                }
                acc -= Step;
            }
        }
    }
}
