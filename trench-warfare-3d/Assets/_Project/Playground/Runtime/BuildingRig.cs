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
            public int[] Holds = System.Array.Empty<int>();   // the supports that count (see Supports)
            public int[] Leans = System.Array.Empty<int>();   // a thin grounded piece (a downpipe): the chunks it is fixed to
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
                var mesh = CutFaces(c.Module.Mesh);
                go.AddComponent<MeshFilter>().sharedMesh = mesh;
                var r = go.AddComponent<MeshRenderer>(); r.sharedMaterial = mat;
                // the ground floor carries the rest and is built heaviest: three times as strong, and only a burst close to it
                // breaks it (critic r2: eight shells razed a 12 m tower to nothing, no ruin left standing)
                float hp = (c.Timber ? 30f : 60f) * (c.Grounded ? 3f : 1f);
                rig.Pieces.Add(new Piece { Chunk = c, T = go.transform, R = r, Box = mesh != null ? mesh.bounds : new Bounds(Vector3.zero, Vector3.one), Hp = hp, MaxHp = hp });
            }
            rig.Radius = h.Bounds.extents.magnitude;
            rig.Supports();
            rig.AnchorCorners();
            return rig;
        }

        float R(float a, float b) => a + (b - a) * (float)rng.NextDouble();

        static readonly Dictionary<Mesh, Mesh> recut = new Dictionary<Mesh, Mesh>();
        public static int CutFacesFound { get; private set; }

        /// <summary>A prototype of the fix Tools/housesplit.py needs (its fix_fill_uvs gives each corner of a cut face the UV of
        /// whichever wall loop it finds first, so a cut face stretches one strip of the atlas across itself and reads as flat
        /// brown card, 6-20x flatter than the walls: critic r5/r6). A cut face lies flat on the chunk's bounding box (the
        /// cuts are axis-aligned). Each is given its own corners, projected flat onto that plane at the chunk's own texel
        /// density, placed on the chunk's largest wall face's UVs (the building's masonry), and darkened: broken stone.</summary>
        static Mesh CutFaces(Mesh src)
        {
            if (src == null || !src.isReadable) return src;
            if (recut.TryGetValue(src, out var done)) return done;
            var v = new List<Vector3>(src.vertices); var n = new List<Vector3>(src.normals); var uv = new List<Vector2>(src.uv);
            var col = new List<Color>(src.colors.Length == v.Count ? src.colors : new Color[v.Count]);
            if (src.colors.Length != v.Count) for (int i = 0; i < col.Count; i++) col[i] = Color.white;
            var tris = new List<int>(src.triangles); var b = src.bounds; float tol = 0.002f + 0.002f * b.size.magnitude;
            // the chunk's texel density and its biggest wall face (the masonry the cut goes through)
            double uvArea = 0, wArea = 0; int wall = -1; float wallArea = 0f;
            bool OnBox(int i, int axis, out float side)
            {
                side = 0f; float lo = b.min[axis], hi = b.max[axis];
                bool atLo = Mathf.Abs(v[tris[i]][axis] - lo) < tol && Mathf.Abs(v[tris[i + 1]][axis] - lo) < tol && Mathf.Abs(v[tris[i + 2]][axis] - lo) < tol;
                bool atHi = Mathf.Abs(v[tris[i]][axis] - hi) < tol && Mathf.Abs(v[tris[i + 1]][axis] - hi) < tol && Mathf.Abs(v[tris[i + 2]][axis] - hi) < tol;
                side = atLo ? -1f : 1f; return atLo || atHi;
            }
            var cutAxis = new int[tris.Count / 3];
            for (int t = 0; t < tris.Count; t += 3)
            {
                cutAxis[t / 3] = -1;
                for (int ax = 0; ax < 3; ax++) if (OnBox(t, ax, out _)) { cutAxis[t / 3] = ax; break; }
                var p0 = v[tris[t]]; var p1 = v[tris[t + 1]]; var p2 = v[tris[t + 2]];
                float wa = Vector3.Cross(p1 - p0, p2 - p0).magnitude * 0.5f;
                if (cutAxis[t / 3] >= 0) continue;
                var e1 = uv[tris[t + 1]] - uv[tris[t]]; var e2 = uv[tris[t + 2]] - uv[tris[t]];
                uvArea += Mathf.Abs(e1.x * e2.y - e1.y * e2.x) * 0.5f; wArea += wa;
                if (wa > wallArea) { wallArea = wa; wall = t; }
            }
            int found = 0; foreach (int ax in cutAxis) if (ax >= 0) found++;
            if (found == 0 || wall < 0 || wArea <= 0) { recut[src] = src; return src; }
            float uvPerMetre = Mathf.Sqrt((float)(uvArea / wArea));
            var anchor = (uv[tris[wall]] + uv[tris[wall + 1]] + uv[tris[wall + 2]]) / 3f;
            for (int t = 0; t < tris.Count; t += 3)
            {
                int ax = cutAxis[t / 3]; if (ax < 0) continue;
                int a1 = (ax + 1) % 3, a2 = (ax + 2) % 3;
                for (int k = 0; k < 3; k++)
                {
                    int i = tris[t + k]; var p = v[i];
                    v.Add(p); n.Add(n[i]); col.Add(new Color(0.62f, 0.60f, 0.58f, 1f));
                    uv.Add(anchor + new Vector2(p[a1] - b.center[a1], p[a2] - b.center[a2]) * uvPerMetre * 0.5f);
                    tris[t + k] = v.Count - 1;
                }
            }
            var m = new Mesh { name = src.name + " (cut faces)", indexFormat = v.Count > 65000 ? UnityEngine.Rendering.IndexFormat.UInt32 : UnityEngine.Rendering.IndexFormat.UInt16 };
            m.SetVertices(v); m.SetNormals(n); m.SetUVs(0, uv); m.SetColors(col); m.SetTriangles(tris, 0); m.RecalculateBounds();
            recut[src] = m; CutFacesFound += found;
            return m;
        }

        /// <summary>Which of the chunks a chunk rests on (HouseKit.Solve) really carry it: not a detail (under 0.2 m3: a pipe, a
        /// rail, a bit of scaffold) and one whose top covers at least a quarter of its footprint. A roof stood on a
        /// drainpipe after the walls under it went (critic r4). A chunk with no such support keeps what Solve gave it,
        /// so the building still stands at the start.</summary>
        void Supports()
        {
            foreach (var p in Pieces)
            {
                var a = p.Chunk.Local; float foot = Mathf.Max(1e-4f, a.size.x * a.size.z);
                var keep = new List<int>();
                foreach (int j in p.Chunk.RestsOn)
                {
                    var b = Pieces[j].Chunk.Local;
                    float vol = b.size.x * b.size.y * b.size.z;
                    float ox = Mathf.Max(0f, Mathf.Min(a.max.x, b.max.x) - Mathf.Max(a.min.x, b.min.x));
                    float oz = Mathf.Max(0f, Mathf.Min(a.max.z, b.max.z) - Mathf.Max(a.min.z, b.min.z));
                    if (vol >= 0.2f && ox * oz >= 0.25f * foot) keep.Add(j);
                }
                p.Holds = keep.Count > 0 ? keep.ToArray() : p.Chunk.RestsOn;
                // a thin piece standing on the ground beside the wall it is fixed to: it goes when that wall goes (a
                // downpipe stood alone with its arm in the air over the rubble, critic r5/r6)
                // and a piece HouseKit calls grounded only because nothing lies under it (Solve's fallback: an elbow pipe
                // 2.4 m up the wall was "grounded") hangs on its neighbours the same way
                if (p.Chunk.Grounded && ((Detail(p) && a.size.y > 1.2f) || !OnGround(p)))
                {
                    var near = new List<int>(); var grown = a; grown.Expand(0.3f);
                    for (int j = 0; j < Pieces.Count; j++)
                        if (j != Pieces.IndexOf(p) && !Detail(Pieces[j]) && grown.Intersects(Pieces[j].Chunk.Local)) near.Add(j);
                    p.Leans = near.ToArray();
                }
            }
        }

        static float Volume(Piece p) { var b = p.Chunk.Local.size; return b.x * b.y * b.z; }
        float floorY = float.MaxValue;
        bool OnGround(Piece p)
        {
            if (floorY == float.MaxValue) foreach (var q in Pieces) floorY = Mathf.Min(floorY, q.Chunk.Local.min.y);
            return p.Chunk.Local.min.y - floorY < HouseKit.GroundedBelow;
        }
        /// <summary>A detail: little (under 0.2 m3 of box) or thin (under 0.35 m across its narrowest way): a pipe, a rail. An
        /// L-shaped downpipe has 0.8 m3 of box and passed as structure, and stood with its arm in the air (critic r6).</summary>
        static bool Detail(Piece p) { var b = p.Chunk.Local.size; return b.x * b.y * b.z < 0.2f || Mathf.Min(b.x, Mathf.Min(b.y, b.z)) < 0.35f; }

        /// <summary>Standing chunks off the ground with nothing standing under them that counts: 0 in a sound ruin.</summary>
        public int Floating
        {
            get
            {
                int n = 0;
                foreach (var p in Pieces)
                {
                    if (p.Loose || p.Falling || p.Chunk.Grounded || p.Anchored) continue;
                    bool held = false;
                    foreach (int j in p.Holds) if (!Pieces[j].Loose && !Pieces[j].Falling) { held = true; break; }
                    if (!held) n++;
                }
                return n;
            }
        }

        /// <summary>The two ground-floor chunks farthest apart, and the chunk stacked on each, become corner stubs: a shelled
        /// building should leave a ruin standing, not a mound.</summary>
        void AnchorCorners()
        {
            // corners are real walls on the real ground: not a detail, not a piece Solve grounded for want of anything under it
            var ground = Pieces.FindAll(p => p.Chunk.Grounded && OnGround(p) && !Detail(p));
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
                var below = corner;
                for (int level = 0; level < 2 && below != null; level++)
                {
                    Piece above = null;
                    foreach (var p in Pieces)
                        if (!p.Anchored && !Detail(p) && System.Array.IndexOf(p.Chunk.RestsOn, below.Chunk.Index) >= 0) { above = p; break; }
                    if (above != null) { above.Anchored = true; above.Hp = above.MaxHp *= 2f; }
                    below = above;
                }
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
                    if (p.Loose || p.Falling || p.Anchored || falling.Contains(p)) continue;
                    if (p.Chunk.Grounded)
                    {
                        if (p.Leans.Length == 0) continue;
                        bool fixedTo = false;
                        foreach (int j in p.Leans) { var s = Pieces[j]; if (!s.Loose && !(s.Falling || falling.Contains(s))) { fixedTo = true; break; } }
                        if (fixedTo) continue;
                        p.Falling = true; p.FallAt = Time.time + storey * StoreyDelay + R(0f, 0.12f); falling.Add(p); changed = true;
                        continue;
                    }
                    if (p.Chunk.RestsOn.Length == 0) continue;
                    bool held = false, detail = Detail(p);
                    if (detail)
                    {
                        held = p.Chunk.RestsOn.Length > 0;
                        foreach (int j in p.Chunk.RestsOn) { var s = Pieces[j]; if (s.Loose || s.Falling || falling.Contains(s)) { held = false; break; } }
                    }
                    else foreach (int j in p.Holds) { var s = Pieces[j]; if (!s.Loose && !(s.Falling || falling.Contains(s))) { held = true; break; } }
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
