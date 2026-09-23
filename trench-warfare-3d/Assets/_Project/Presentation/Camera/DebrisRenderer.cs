// Phase: B5 (implemented) — what a blast breaks off, drawn for nothing. Every fragment on the field (clods, splinters,
// planks, rubble, sandbags, armour plates, a tree's crown, a man's limbs and kit) is one 96-byte record written ONCE
// when it is thrown; TW/Debris integrates the flight itself from the record and the clock, so a thousand pieces in the
// air cost the CPU nothing a frame and the GPU a few instructions a vertex. No GameObjects, no PhysX, no per-frame
// SetData beyond the records born this frame. One indirect draw per fragment mesh (about a dozen), one material.
//
// A record is an arc: where it started, how fast, when it lands (solved here against the drawn ground at the place it
// will come down, two Newton steps), the height it rests at, its tumble (an axis and a rate), its birth pose, a tint
// (rgb; a = how much it burns) and how long it lies before the mud takes it. The shader bounces it once, settles it
// flat, and sinks it out of sight after its life. Mode 1 is a hinge instead of an arc: a tree top or a wall slab
// falling over about its foot.
//
// Budgets: fixed pools per mesh (below), a ring each — past the cap the oldest piece is overwritten, so a barrage never
// allocates and never grows. Spend follows the look point (CameraShake.DistanceToLook): a burst under the eye throws
// everything, one on the far side of the field a quarter of it.
//
// The sim never sees any of this. What breaks is decided there (BlastSystem, DeformationSystem, VehicleModulesSystem)
// and told through events; how the pieces fly is presentation, seeded from the event's position so a replay and a
// capture look the same.
using System.Collections.Generic;
using System.Runtime.InteropServices;
using Unity.Collections;
using UnityEngine;
using UnityEngine.Rendering;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    /// <summary>The pure arithmetic of a thrown piece, kept apart from the renderer so it can be tested without a GPU.</summary>
    public static class DebrisMath
    {
        public const float Gravity = 9.8f;
        /// <summary>After landing a piece keeps this much of its sideways speed and this much of its fall, upwards, for one bounce.</summary>
        public const float BounceKeep = 0.45f, BounceUp = 0.30f;
        /// <summary>A piece past its life sinks this many metres (times its scale) over SinkSeconds and is then gone.</summary>
        public const float SinkSeconds = 3f, SinkDepth = 1.2f;

        /// <summary>
        /// When a piece thrown from p0 at v0 first reaches the height restY: the positive root of the fall, 0 when it
        /// starts at or below it and is not going up.
        /// </summary>
        public static float TimeToHeight(float y0, float vy, float restY)
        {
            float drop = y0 - restY;                      // how far above the rest height it starts
            float disc = vy * vy + 2f * Gravity * drop;
            if (disc <= 0f) return 0f;
            float t = (vy + Mathf.Sqrt(disc)) / Gravity;
            return t > 0f ? t : 0f;
        }

        /// <summary>
        /// Where and when a piece lands on ground that is not flat: the flight time is solved for the ground under the
        /// start, the landing point that gives is looked up, and the time is solved again for the ground there. Two
        /// steps are enough for a battlefield; the rest is the shader's clamp. lift is the height the piece's centre
        /// rests above the ground (half its thickness).
        /// </summary>
        public static void Landing(Vector3 p0, Vector3 v0, float lift, System.Func<float, float, float> ground, out float landT, out float landY)
        {
            landY = ground(p0.x, p0.z) + lift;
            landT = TimeToHeight(p0.y, v0.y, landY);
            for (int step = 0; step < 2; step++)
            {
                float x = p0.x + v0.x * landT, z = p0.z + v0.z * landT;
                float y = ground(x, z) + lift;
                float t = TimeToHeight(p0.y, v0.y, y);
                landY = y; landT = t;
            }
        }

        /// <summary>How long the bounce off the ground lasts for a piece that landed with this downward speed.</summary>
        public static float BounceSeconds(float vyAtLanding) => Mathf.Max(0f, 2f * (-vyAtLanding * BounceUp) / Gravity);

        /// <summary>The piece's position at time t after its throw, as the shader computes it (mirrored here for the tests).</summary>
        public static Vector3 PositionAt(Vector3 p0, Vector3 v0, float landT, float landY, float t)
        {
            float t1 = Mathf.Min(Mathf.Max(t, 0f), landT);
            Vector3 p = p0 + v0 * t1 + Vector3.down * (0.5f * Gravity * t1 * t1);
            if (t <= landT) return p;
            float vy1 = v0.y - Gravity * landT;
            Vector3 v2 = new Vector3(v0.x * BounceKeep, -vy1 * BounceUp, v0.z * BounceKeep);
            float t2 = Mathf.Min(t - landT, BounceSeconds(vy1));
            p += v2 * t2 + Vector3.down * (0.5f * Gravity * t2 * t2);
            p.y = Mathf.Max(p.y, landY);
            return p;
        }

        /// <summary>The share of a burst's pieces worth throwing this far from the middle of the picture: all of them under
        /// the eye, a quarter across the field. Counts are scaled by it so the pools are spent where they are seen.</summary>
        public static float Share(float distanceToLook) => distanceToLook < 55f ? 1f : distanceToLook < 120f ? 0.5f : 0.25f;
    }

    /// <summary>A small deterministic generator seeded from a place, so a burst's pieces fly the same way in a replay.</summary>
    public struct DebrisRng
    {
        uint s;
        public DebrisRng(Vector3 at, uint salt)
        {
            s = (uint)Mathf.FloorToInt(at.x * 37f) * 0x9E3779B1u ^ (uint)Mathf.FloorToInt(at.z * 53f) * 0x85EBCA77u ^ (uint)Mathf.FloorToInt(at.y * 11f) * 0xC2B2AE3Du ^ salt * 0x27D4EB2Fu;
            if (s == 0) s = 0x1234567u;
        }
        public float Next() { s ^= s << 13; s ^= s >> 17; s ^= s << 5; return (s & 0xFFFFFF) / (float)0x1000000; }
        public float Range(float a, float b) => a + (b - a) * Next();
        public Vector3 OnSphere()
        {
            float z = Range(-1f, 1f), a = Range(0f, Mathf.PI * 2f), r = Mathf.Sqrt(Mathf.Max(0f, 1f - z * z));
            return new Vector3(Mathf.Cos(a) * r, z, Mathf.Sin(a) * r);
        }
        public Quaternion Rotation() => Quaternion.AngleAxis(Range(0f, 360f), OnSphere());
    }

    [DefaultExecutionOrder(500)]
    public sealed class DebrisRenderer : MonoBehaviour
    {
        /// <summary>The kinds of piece there are: one mesh, one pool and one indirect draw each.</summary>
        public enum Piece : byte
        {
            Clod,     // a lump of earth (also the dark lumps a blast makes of a man)
            Shard,    // a splinter of wood: a tree's, a plank's
            Plank,    // a board from a revetment, a roof, a crate
            Rubble,   // a block of concrete or stone
            Sandbag,  // a sack, burst off a parapet
            Plate,    // a sheet of armour or corrugated iron
            Crown,    // the top of a tree, hinged at the break
            Limb,     // an arm or a leg
            Helmet,
            Rifle,
            Count
        }

        /// <summary>The record TW/Debris reads: see the shader's header. 96 bytes.</summary>
        [StructLayout(LayoutKind.Sequential)]
        public struct Record
        {
            public Vector3 P0; public float Born;
            public Vector3 V0; public float LandT;
            public Vector3 Axis; public float Spin;
            public Quaternion Rot0;
            public Vector4 Tint;
            public float Scale, Life, Mode, LandY;
        }
        public const int RecordBytes = 96;

        sealed class Pool
        {
            public Mesh Mesh; public int Start, Capacity, Head, Count; public bool Shadows; public float Lift;
            public int DirtyLo = int.MaxValue, DirtyHi = -1, Wrapped;   // Wrapped: records written past the end this frame (they start at 0)
            public MaterialPropertyBlock Props;
        }

        public static DebrisRenderer Instance { get; private set; }
        public SimHost Host;
        /// <summary>0 turns the dark lumps and the limbs off (a player setting), 1 as designed.</summary>
        public static float Gore = 1f;
        /// <summary>
        /// The battlefield's say over every piece: rgb multiplies each piece's own tint (white = as thrown; grey-white
        /// for rock under snow, near-black for basalt), a is a floor under the ember glow (0 = only burning pieces
        /// glow; about 0.4 = everything smoulders, for lava). A biome sets it once; it is pushed to the shader each frame.
        /// </summary>
        public static Color Biome = new Color(1f, 1f, 1f, 0f);
        /// <summary>A puff of dust at a place, this many metres wide (CombatFx lends its flipbooks; null when it is not there).</summary>
        public System.Action<Vector3, float> Dust;
        public bool Ready { get; private set; }
        public int Alive { get; private set; }
        public int DrawCalls { get; private set; }

        static readonly int[] Capacity = { 1024, 512, 384, 512, 256, 256, 64, 256, 128, 128 };
        static readonly bool[] CastsShadow = { false, false, true, true, true, true, true, false, false, false };
        /// <summary>The pool for a kind of piece: past it the oldest is overwritten. Sums to about 3,500 records (330 KB) for the field.</summary>
        public static int CapacityOf(Piece piece) => Capacity[(int)piece];
        readonly Pool[] pools = new Pool[(int)Piece.Count];
        NativeArray<Record> records;
        GraphicsBuffer buffer, args;
        GraphicsBuffer.IndirectDrawIndexedArgs[] argsData;
        Material material;
        readonly List<Mesh> owned = new List<Mesh>();
        static readonly int NowId = Shader.PropertyToID("_DebrisNow"), BiomeId = Shader.PropertyToID("_DebrisBiome"), RecordsId = Shader.PropertyToID("_Records"), LiftId = Shader.PropertyToID("_Lift");
        static readonly Bounds Everywhere = new Bounds(Vector3.zero, Vector3.one * 5000f);
        /// <summary>How much a mesh's bounds are padded (total, both sides), so the rest height is read from the true extents.</summary>
        const float BoundsPad = 0.5f;

        void Awake() { Instance = this; }

        void Start()
        {
            if (Host == null) Host = FindFirstObjectByType<SimHost>();
            var shader = Shader.Find("TW/Debris (URP)");
            if (shader == null) { Debug.LogWarning("DebrisRenderer: TW/Debris is missing; nothing breaks off."); enabled = false; return; }
            material = new Material(shader) { hideFlags = HideFlags.HideAndDontSave, name = "Debris" };
            int total = 0;
            for (int k = 0; k < (int)Piece.Count; k++) total += Capacity[k];
            records = new NativeArray<Record>(total, Allocator.Persistent);
            buffer = new GraphicsBuffer(GraphicsBuffer.Target.Structured, total, RecordBytes);
            args = new GraphicsBuffer(GraphicsBuffer.Target.IndirectArguments, (int)Piece.Count, GraphicsBuffer.IndirectDrawIndexedArgs.size);
            argsData = new GraphicsBuffer.IndirectDrawIndexedArgs[(int)Piece.Count];
            int start = 0;
            for (int k = 0; k < (int)Piece.Count; k++)
            {
                var mesh = Build((Piece)k);
                pools[k] = new Pool { Mesh = mesh, Start = start, Capacity = Capacity[k], Shadows = CastsShadow[k], Lift = (Piece)k == Piece.Crown ? 0f : mesh.bounds.extents.y - BoundsPad * 0.5f, Props = new MaterialPropertyBlock() };
                pools[k].Props.SetBuffer(RecordsId, buffer);
                pools[k].Props.SetFloat(LiftId, pools[k].Lift);
                start += Capacity[k];
            }
            material.SetBuffer(RecordsId, buffer);
            Ready = true;
        }

        void OnDestroy()
        {
            if (Instance == this) Instance = null;
            if (records.IsCreated) records.Dispose();
            buffer?.Dispose(); args?.Dispose();
            if (material != null) Destroy(material);
            foreach (var m in owned) if (m != null) Destroy(m);
        }

        float Ground(float x, float z) => Host != null && Host.Local != null ? RenderGround.Sample(Host.Local.Map, x, z) : 0f;

        // ------------------------------------------------------------------ throwing
        /// <summary>
        /// One piece thrown from a place at a speed: its arc is solved against the drawn ground now and never touched
        /// again. scale is metres for a unit mesh (a clod of 0.3 is a fist-sized lump). life is how long it lies after
        /// it lands before it sinks away. burn (0..1) makes it glow and smoulder as it cools.
        /// </summary>
        public void Throw(Piece piece, Vector3 at, Vector3 velocity, float scale, Color tint, ref DebrisRng rng, float life = 20f, float burn = 0f, Quaternion? pose = null)
        {
            if (!Ready) return;
            var pool = pools[(int)piece];
            DebrisMath.Landing(at, velocity, pool.Lift * scale, Ground, out float landT, out float landY);
            float speed = velocity.magnitude;
            var r = new Record
            {
                P0 = at, Born = Time.time, V0 = velocity, LandT = landT, LandY = landY,
                Axis = rng.OnSphere(), Spin = rng.Range(0.6f, 1.4f) * Mathf.Clamp(speed * 1.1f, 2f, 14f),
                Rot0 = pose ?? rng.Rotation(), Tint = new Vector4(tint.r, tint.g, tint.b, burn), Scale = scale, Life = life, Mode = 0f
            };
            Put(pool, r);
        }

        /// <summary>
        /// A burst of pieces from a place: a cone that leans up (dir.y is stretched, so the pieces come down again in
        /// the picture rather than skittering off), each at 0.5-1.2 of the speed and 0.6-1.5 of the size. The count is
        /// scaled by how near the burst is to the middle of the picture.
        /// </summary>
        public void Burst(Piece piece, Vector3 at, int count, float speed, float scale, Color tint, float life = 20f, float burn = 0f, float up = 1.6f, Vector3 lean = default, uint salt = 0)
        {
            if (!Ready || count <= 0) return;
            count = Mathf.CeilToInt(count * DebrisMath.Share(CameraShake.DistanceToLook(at)));
            var rng = new DebrisRng(at, salt + (uint)piece * 17u);
            for (int k = 0; k < count; k++)
            {
                Vector3 dir = rng.OnSphere(); dir.y = Mathf.Abs(dir.y) * up + 0.35f;
                dir += lean;
                Vector3 vel = dir.normalized * (speed * rng.Range(0.5f, 1.2f));
                Throw(piece, at, vel, scale * rng.Range(0.6f, 1.5f), tint, ref rng, life * rng.Range(0.7f, 1.3f), burn);
            }
        }

        /// <summary>
        /// A piece that falls over rather than flying: hinged at pivot (the mesh's foot), standing in pose, going over
        /// about the horizontal axis to the side of fallDirection until it lies at angle radians (about 1.5 for the
        /// ground), taking seconds to do it. It lies life seconds, then sinks. A tree's crown, a slab of wall.
        /// </summary>
        public void Topple(Piece piece, Vector3 pivot, Quaternion pose, Vector3 fallDirection, float seconds, float scale, Color tint, float life = 6f, float angle = 1.5f)
        {
            if (!Ready) return;
            fallDirection.y = 0f;
            if (fallDirection.sqrMagnitude < 1e-4f) fallDirection = Vector3.forward;
            Vector3 axis = Vector3.Cross(Vector3.up, fallDirection.normalized);   // the hinge lies across the fall
            var r = new Record
            {
                P0 = pivot, Born = Time.time, V0 = Vector3.zero, LandT = Mathf.Max(0.2f, seconds), LandY = Mathf.Min(pivot.y, Ground(pivot.x, pivot.z) + 0.25f * scale),
                Axis = axis, Spin = angle, Rot0 = pose, Tint = new Vector4(tint.r, tint.g, tint.b, 0f), Scale = scale, Life = life, Mode = 1f
            };
            Put(pools[(int)piece], r);
        }

        void Put(Pool pool, in Record r)
        {
            int i = pool.Head;
            records[pool.Start + i] = r;
            if (i < pool.DirtyLo) pool.DirtyLo = i;
            if (i > pool.DirtyHi) pool.DirtyHi = i;
            pool.Head = (i + 1) % pool.Capacity;
            if (pool.Count < pool.Capacity) pool.Count++;
        }

        // ------------------------------------------------------------------ frame
        void LateUpdate()
        {
            if (!Ready) return;
            Shader.SetGlobalFloat(NowId, Time.time);
            Shader.SetGlobalVector(BiomeId, Biome);
            DrawCalls = 0; Alive = 0;
            for (int k = 0; k < pools.Length; k++)
            {
                var pool = pools[k];
                if (pool.DirtyHi >= pool.DirtyLo)
                {
                    int n = pool.DirtyHi - pool.DirtyLo + 1;
                    buffer.SetData(records, pool.Start + pool.DirtyLo, pool.Start + pool.DirtyLo, n);
                    pool.DirtyLo = int.MaxValue; pool.DirtyHi = -1;
                }
                Alive += pool.Count;
                argsData[k] = new GraphicsBuffer.IndirectDrawIndexedArgs
                {
                    indexCountPerInstance = pool.Mesh.GetIndexCount(0), instanceCount = (uint)pool.Count,
                    startIndex = 0, baseVertexIndex = 0, startInstance = (uint)pool.Start
                };
            }
            args.SetData(argsData);
            for (int k = 0; k < pools.Length; k++)
            {
                var pool = pools[k];
                if (pool.Count == 0) continue;
                var rp = new RenderParams(material) { worldBounds = Everywhere, shadowCastingMode = pool.Shadows ? ShadowCastingMode.On : ShadowCastingMode.Off, receiveShadows = true, matProps = pool.Props };
                Graphics.RenderMeshIndirect(rp, pool.Mesh, args, 1, k);
                DrawCalls++;
            }
        }

        // ------------------------------------------------------------------ the pieces themselves
        // Unit meshes, a few dozen vertices each, centred on their middle (the shader lifts them by half their height to
        // rest on the ground) except the crown, which stands on its foot. A smoothed normal per vertex goes in UV3 for
        // the ink outline, as BattlefieldKit.Combine does, so a box's outline does not split at the corners.
        Mesh Build(Piece piece)
        {
            var v = new List<Vector3>(); var t = new List<int>(); var c = new List<Color>();
            var rng = new DebrisRng(new Vector3((int)piece * 3.7f, 1f, 2.3f), 99u);
            switch (piece)
            {
                case Piece.Clod: Lump(v, t, c, 6, 4, 0.5f, 0.22f, ref rng, new Color(0.9f, 0.9f, 0.9f), new Color(1.05f, 1.05f, 1.05f)); break;
                case Piece.Shard:
                    Box(v, t, c, new Vector3(0.10f, 0.08f, 1.0f), 0.55f, ref rng, new Color(0.92f, 0.92f, 0.92f), new Color(1.06f, 1.06f, 1.06f));   // tapered along its length: a splinter
                    break;
                case Piece.Plank: Box(v, t, c, new Vector3(0.22f, 0.05f, 1.0f), 0.98f, ref rng, new Color(0.9f, 0.9f, 0.9f), new Color(1.08f, 1.08f, 1.08f)); break;
                case Piece.Rubble: Box(v, t, c, new Vector3(1.0f, 0.75f, 0.85f), 0.85f, ref rng, new Color(0.86f, 0.86f, 0.86f), new Color(1.06f, 1.06f, 1.06f), 0.08f); break;
                case Piece.Sandbag: Lump(v, t, c, 8, 4, 0.5f, 0.05f, ref rng, new Color(0.85f, 0.85f, 0.85f), new Color(1.06f, 1.06f, 1.06f), new Vector3(1.0f, 0.55f, 0.65f)); break;
                case Piece.Plate: Box(v, t, c, new Vector3(1.0f, 0.06f, 0.8f), 0.9f, ref rng, new Color(0.9f, 0.9f, 0.9f), new Color(1.0f, 1.0f, 1.0f)); break;
                case Piece.Crown: CrownMesh(v, t, c); break;
                case Piece.Limb: Taper(v, t, c, 0.11f, 0.075f, 1.0f, 6, new Color(0.95f, 0.95f, 0.95f), new Color(1.0f, 1.0f, 1.0f), true); break;
                case Piece.Helmet: HelmetMesh(v, t, c); break;
                case Piece.Rifle: RifleMesh(v, t, c); break;
            }
            var m = new Mesh { name = "Debris " + piece, hideFlags = HideFlags.HideAndDontSave };
            m.SetVertices(v); m.SetTriangles(t, 0); m.SetColors(c);
            m.RecalculateNormals();
            // the crown is hinged at its foot: its origin stays at y = 0; everything else is centred on its bounds
            if (piece != Piece.Crown)
            {
                var b = m.bounds; var verts = m.vertices;
                for (int i = 0; i < verts.Length; i++) verts[i] -= b.center;
                m.SetVertices(verts);
            }
            m.RecalculateBounds();
            SmoothNormals(m);
            m.bounds = new Bounds(m.bounds.center, m.bounds.size + Vector3.one * BoundsPad);   // room for the outline hull; Lift takes it off again
            owned.Add(m);
            return m;
        }

        /// <summary>A smoothed normal per vertex position in UV3, so the inverted-hull outline stays whole at hard corners.</summary>
        static void SmoothNormals(Mesh m)
        {
            var verts = m.vertices; var normals = m.normals;
            var sum = new Dictionary<Vector3Int, Vector3>();
            var keys = new Vector3Int[verts.Length];
            for (int i = 0; i < verts.Length; i++)
            {
                keys[i] = new Vector3Int(Mathf.RoundToInt(verts[i].x * 500f), Mathf.RoundToInt(verts[i].y * 500f), Mathf.RoundToInt(verts[i].z * 500f));
                sum.TryGetValue(keys[i], out var n); sum[keys[i]] = n + normals[i];
            }
            var smooth = new List<Vector3>(verts.Length);
            for (int i = 0; i < verts.Length; i++) smooth.Add(sum[keys[i]].normalized);
            m.SetUVs(3, smooth);
        }

        /// <summary>A lumpy sphere: each vertex pushed in or out by up to bump of the radius, the bottom darker than the top.</summary>
        static void Lump(List<Vector3> v, List<int> t, List<Color> c, int segments, int rings, float radius, float bump, ref DebrisRng rng, Color low, Color high, Vector3? squash = null)
        {
            var s = squash ?? Vector3.one;
            int first = v.Count;
            var pushed = new Dictionary<Vector3Int, float>();
            for (int r = 0; r <= rings; r++)
            for (int k = 0; k <= segments; k++)
            {
                float lat = Mathf.PI * r / rings, lon = 2f * Mathf.PI * k / segments;
                var p = new Vector3(Mathf.Sin(lat) * Mathf.Cos(lon), Mathf.Cos(lat), Mathf.Sin(lat) * Mathf.Sin(lon));
                // the seam (k == segments) and the poles share their push with the vertex they coincide with
                var key = new Vector3Int(Mathf.RoundToInt(p.x * 100f), Mathf.RoundToInt(p.y * 100f), Mathf.RoundToInt(p.z * 100f));
                if (!pushed.TryGetValue(key, out float push)) { push = 1f + rng.Range(-bump, bump); pushed[key] = push; }
                v.Add(Vector3.Scale(p * (radius * push), s));
                c.Add(Color.Lerp(low, high, (p.y + 1f) * 0.5f));
            }
            for (int r = 0; r < rings; r++)
            for (int k = 0; k < segments; k++)
            {
                int i = first + r * (segments + 1) + k, j = i + segments + 1;
                t.Add(i); t.Add(i + 1); t.Add(j); t.Add(i + 1); t.Add(j + 1); t.Add(j);
            }
        }

        /// <summary>A box of size, its far end (+z) scaled by taper (a splinter comes to a point), each corner jogged by jitter of the size, with hard faces.</summary>
        static void Box(List<Vector3> v, List<int> t, List<Color> c, Vector3 size, float taper, ref DebrisRng rng, Color low, Color high, float jitter = 0.04f)
        {
            var corner = new Vector3[8];
            for (int i = 0; i < 8; i++)
            {
                float x = (i & 1) == 0 ? -0.5f : 0.5f, y = (i & 2) == 0 ? -0.5f : 0.5f, z = (i & 4) == 0 ? -0.5f : 0.5f;
                float end = z > 0f ? taper : 1f;
                corner[i] = Vector3.Scale(new Vector3(x * end, y * end, z), size) + new Vector3(rng.Range(-jitter, jitter) * size.x, rng.Range(-jitter, jitter) * size.y, rng.Range(-jitter, jitter) * size.z);
            }
            int[][] faces = { new[] { 0, 2, 3, 1 }, new[] { 4, 5, 7, 6 }, new[] { 0, 1, 5, 4 }, new[] { 2, 6, 7, 3 }, new[] { 0, 4, 6, 2 }, new[] { 1, 3, 7, 5 } };
            foreach (var f in faces)
            {
                int first = v.Count;
                foreach (int i in f) { v.Add(corner[i]); c.Add(Color.Lerp(low, high, corner[i].y / Mathf.Max(0.01f, size.y) + 0.5f)); }
                t.Add(first); t.Add(first + 1); t.Add(first + 2); t.Add(first); t.Add(first + 2); t.Add(first + 3);
            }
        }

        /// <summary>A tapered tube along +Y from 0 to height, radius r0 at the foot and r1 at the top, capped.</summary>
        static void Taper(List<Vector3> v, List<int> t, List<Color> c, float r0, float r1, float height, int sides, Color low, Color high, bool caps)
        {
            int first = v.Count;
            for (int ring = 0; ring < 2; ring++)
            {
                float r = ring == 0 ? r0 : r1, y = ring * height;
                for (int k = 0; k <= sides; k++)
                {
                    float a = k * Mathf.PI * 2f / sides;
                    v.Add(new Vector3(Mathf.Cos(a) * r, y, Mathf.Sin(a) * r)); c.Add(ring == 0 ? low : high);
                }
            }
            for (int k = 0; k < sides; k++)
            {
                int i = first + k, j = i + sides + 1;
                t.Add(i); t.Add(j); t.Add(i + 1); t.Add(i + 1); t.Add(j); t.Add(j + 1);
            }
            if (!caps) return;
            int bottom = v.Count; v.Add(new Vector3(0f, 0f, 0f)); c.Add(low);
            int top = v.Count; v.Add(new Vector3(0f, height, 0f)); c.Add(high);
            for (int k = 0; k < sides; k++)
            {
                // clockwise seen from outside (Unity's front face): the bottom cap from below, the top from above
                t.Add(bottom); t.Add(first + k); t.Add(first + k + 1);
                t.Add(top); t.Add(first + sides + 1 + k + 1); t.Add(first + sides + 1 + k);
            }
        }

        /// <summary>The top of a dead tree, 3.6 m from the break with two branch stubs; its foot at the origin (the hinge).</summary>
        static void CrownMesh(List<Vector3> v, List<int> t, List<Color> c)
        {
            var bark = new Color(0.9f, 0.9f, 0.9f); var pale = new Color(1.04f, 1.04f, 1.04f);
            Taper(v, t, c, 0.30f, 0.09f, 3.6f, 6, bark, pale, true);
            Branch(v, t, c, new Vector3(0.12f, 1.4f, 0.05f), Quaternion.Euler(0f, 0f, -58f), 0.09f, 0.03f, 1.3f, bark, pale);
            Branch(v, t, c, new Vector3(-0.05f, 2.4f, 0.10f), Quaternion.Euler(20f, 0f, 55f), 0.07f, 0.025f, 1.0f, bark, pale);
        }

        static void Branch(List<Vector3> v, List<int> t, List<Color> c, Vector3 at, Quaternion turn, float r0, float r1, float length, Color low, Color high)
        {
            var bv = new List<Vector3>(); var bt = new List<int>(); var bc = new List<Color>();
            Taper(bv, bt, bc, r0, r1, length, 5, low, high, true);
            int first = v.Count;
            foreach (var p in bv) v.Add(at + turn * p);
            foreach (int i in bt) t.Add(first + i);
            c.AddRange(bc);
        }

        /// <summary>A helmet: the upper half of a squashed sphere with a brim ring.</summary>
        static void HelmetMesh(List<Vector3> v, List<int> t, List<Color> c)
        {
            var steel = new Color(0.88f, 0.88f, 0.88f); var pale = new Color(1.04f, 1.04f, 1.04f);
            const int sides = 8, rings = 3;
            int first = v.Count;
            for (int r = 0; r <= rings; r++)
            for (int k = 0; k <= sides; k++)
            {
                float lat = 0.5f * Mathf.PI * r / rings, lon = 2f * Mathf.PI * k / sides;
                var p = new Vector3(Mathf.Sin(lat) * Mathf.Cos(lon), Mathf.Cos(lat) * 0.72f, Mathf.Sin(lat) * Mathf.Sin(lon)) * 0.5f;
                v.Add(p); c.Add(Color.Lerp(steel, pale, p.y * 2f + 0.3f));
            }
            for (int r = 0; r < rings; r++)
            for (int k = 0; k < sides; k++)
            {
                int i = first + r * (sides + 1) + k, j = i + sides + 1;
                t.Add(i); t.Add(i + 1); t.Add(j); t.Add(i + 1); t.Add(j + 1); t.Add(j);
            }
            // the brim: a flat ring a little wider than the bowl, both faces
            int brim = v.Count;
            for (int k = 0; k <= sides; k++)
            {
                float a = k * Mathf.PI * 2f / sides;
                v.Add(new Vector3(Mathf.Cos(a), 0f, Mathf.Sin(a)) * 0.5f); c.Add(steel);
                v.Add(new Vector3(Mathf.Cos(a), -0.03f, Mathf.Sin(a)) * 0.66f); c.Add(steel);
            }
            for (int k = 0; k < sides; k++)
            {
                int i = brim + k * 2;
                t.Add(i); t.Add(i + 2); t.Add(i + 1); t.Add(i + 1); t.Add(i + 2); t.Add(i + 3);
                t.Add(i); t.Add(i + 1); t.Add(i + 2); t.Add(i + 1); t.Add(i + 3); t.Add(i + 2);
            }
        }

        /// <summary>A rifle: the barrel and the stock, 1.1 m along +z.</summary>
        static void RifleMesh(List<Vector3> v, List<int> t, List<Color> c)
        {
            var rng = new DebrisRng(Vector3.one, 5u);
            var wood = new Color(0.9f, 0.9f, 0.9f); var dark = new Color(0.55f, 0.55f, 0.55f);
            int first = v.Count;
            Box(v, t, c, new Vector3(0.045f, 0.06f, 0.62f), 1f, ref rng, wood, wood, 0f);
            for (int i = first; i < v.Count; i++) v[i] += new Vector3(0f, 0f, -0.24f);   // the stock, behind
            first = v.Count;
            Box(v, t, c, new Vector3(0.03f, 0.03f, 0.55f), 1f, ref rng, dark, dark, 0f);
            for (int i = first; i < v.Count; i++) v[i] += new Vector3(0f, 0.01f, 0.30f);   // the barrel, ahead
        }
    }
}
