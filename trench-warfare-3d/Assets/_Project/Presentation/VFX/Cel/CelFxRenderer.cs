// Phase: CelFx kit (2026-11-09) - every cel primitive on the field, drawn for nothing a frame. The pattern is
// DebrisRenderer's: a record (CelRecord, 128 bytes) is written ONCE when it is spawned, into a ring per kind in one
// GPU buffer; the kind's vertex shader integrates it from the record and the clock. A frame uploads only the records
// born since the last one and makes one indirect draw per kind that holds a live record, through FrameBudget. A record
// past its life collapses to a point in the shader; a pool whose every record is dead is not drawn at all.
//
// Spawning never allocates: Spawn copies a struct into a NativeArray. The kinds are found once, when the renderer
// starts (CelKind, the extension point). One renderer per scene; CelFx.Renderer makes it on first use.
using System;
using System.Collections.Generic;
using Unity.Collections;
using UnityEngine;
using UnityEngine.Rendering;
using TW.Presentation;

namespace TW.Presentation.VFX.Cel
{
    [DefaultExecutionOrder(520)]
    public sealed class CelFxRenderer : MonoBehaviour
    {
        sealed class Pool
        {
            public CelKind Kind; public Mesh Mesh; public Material Material; public MaterialPropertyBlock Props;
            public int Start, Capacity, Head, Count;
            public float AliveUntil = float.NegativeInfinity;
            public int DirtyLo = int.MaxValue, DirtyHi = -1;
            public int Spawned;   // ever, for the tests and the HUD
        }

        public bool Ready { get; private set; }
        /// <summary>Indirect draws made by the last Submit.</summary>
        public int DrawCalls { get; private set; }
        /// <summary>Records held across every pool (alive or not yet expired out of the ring).</summary>
        public int Held { get; private set; }
        public int KindCount => pools.Length;

        Pool[] pools = Array.Empty<Pool>();
        NativeArray<CelRecord> records;
        GraphicsBuffer buffer, args, palette;
        GraphicsBuffer.IndirectDrawIndexedArgs[] argsData;
        int[] drawOrder = Array.Empty<int>();
        int paletteVersion = -1;
        static readonly Unity.Profiling.ProfilerMarker LateMarker = new Unity.Profiling.ProfilerMarker("CelFx.Late");
        static readonly Bounds Everywhere = new Bounds(Vector3.zero, Vector3.one * 5000f);
        static readonly int RecordsId = Shader.PropertyToID("_CelRecords"), PaletteId = Shader.PropertyToID("_CelPalette"),
            NowId = Shader.PropertyToID("_CelNow"), LodId = Shader.PropertyToID("_CelLod");

        void Awake() { if (CelFx.ExistingRenderer == null || CelFx.ExistingRenderer == this) CelFx.Adopt(this); }

        void OnDestroy()
        {
            CelFx.Release(this);
            Shutdown();
        }

        /// <summary>Build the pools if they are not built. Safe to call in edit mode (the stills do).</summary>
        public void Init()
        {
            if (Ready) return;
            var kinds = FindKinds();
            pools = new Pool[kinds.Count];
            float scale = Mathf.Clamp(FxQuality.Now.Chunks, 0.5f, 1.5f) * Knobs.Get("cel.capacityScale", 1f);
            int total = 0;
            for (int k = 0; k < kinds.Count; k++)
            {
                var kind = kinds[k];
                var shader = kind.FindShader();
                var mesh = shader != null ? kind.BuildMesh() : null;
                var pool = new Pool { Kind = kind, Mesh = mesh, Start = total, Capacity = Mathf.Max(1, Mathf.RoundToInt(kind.Capacity * scale)), Props = new MaterialPropertyBlock() };
                if (shader == null) Debug.LogWarning("CelFxRenderer: the shader of kind " + kind.Name + " is missing; it draws nothing.");
                else
                {
                    pool.Material = new Material(shader) { hideFlags = HideFlags.HideAndDontSave, name = "Cel " + kind.Name };
                    kind.Configure(pool.Material);
                }
                total += pool.Capacity;
                pools[k] = pool;
            }
            records = new NativeArray<CelRecord>(Mathf.Max(1, total), Allocator.Persistent);
            buffer = new GraphicsBuffer(GraphicsBuffer.Target.Structured, Mathf.Max(1, total), CelRecord.Bytes);
            args = new GraphicsBuffer(GraphicsBuffer.Target.IndirectArguments, Mathf.Max(1, pools.Length), GraphicsBuffer.IndirectDrawIndexedArgs.size);
            argsData = new GraphicsBuffer.IndirectDrawIndexedArgs[Mathf.Max(1, pools.Length)];
            palette = new GraphicsBuffer(GraphicsBuffer.Target.Structured, CelPalette.Max * CelPalette.Stride, 16);
            foreach (var pool in pools)
            {
                pool.Props.SetBuffer(RecordsId, buffer);
                pool.Props.SetBuffer(PaletteId, palette);
                if (pool.Material != null) { pool.Material.SetBuffer(RecordsId, buffer); pool.Material.SetBuffer(PaletteId, palette); }
            }
            drawOrder = new int[pools.Length];
            for (int i = 0; i < drawOrder.Length; i++) drawOrder[i] = i;
            Array.Sort(drawOrder, (a, b) => pools[a].Kind.Order != pools[b].Kind.Order ? pools[a].Kind.Order.CompareTo(pools[b].Kind.Order) : a.CompareTo(b));
            paletteVersion = -1;
            Ready = true;
        }

        /// <summary>Free the GPU buffers and the meshes (OnDestroy; edit-mode callers, which get no OnDestroy, call it themselves).</summary>
        public void Shutdown()
        {
            if (records.IsCreated) records.Dispose();
            buffer?.Dispose(); args?.Dispose(); palette?.Dispose();
            buffer = args = palette = null;
            foreach (var p in pools)
            {
                Gone(p.Material); Gone(p.Mesh);
            }
            pools = Array.Empty<Pool>();
            Ready = false;
        }

        static void Gone(UnityEngine.Object o)
        {
            if (o == null) return;
            if (Application.isPlaying) Destroy(o); else DestroyImmediate(o);
        }

        /// <summary>Every kind: the built-ins first (their ids are CelKinds' constants), then every other CelKind in the
        /// TW.* assemblies in name order.</summary>
        public static List<CelKind> FindKinds()
        {
            var list = new List<CelKind> { new StreakKind(), new LobeKind(), new GlowKind() };
            var extra = new List<CelKind>();
            foreach (var asm in AppDomain.CurrentDomain.GetAssemblies())
            {
                string n = asm.GetName().Name;
                if (!n.StartsWith("TW.", StringComparison.Ordinal) || n.StartsWith("TW.Tests", StringComparison.Ordinal)) continue;
                Type[] types;
                try { types = asm.GetTypes(); } catch (System.Reflection.ReflectionTypeLoadException e) { types = e.Types; }
                foreach (var t in types)
                {
                    if (t == null || t.IsAbstract || !typeof(CelKind).IsAssignableFrom(t)) continue;
                    if (t == typeof(StreakKind) || t == typeof(LobeKind) || t == typeof(GlowKind)) continue;
                    if (t.GetConstructor(Type.EmptyTypes) == null) continue;
                    extra.Add((CelKind)Activator.CreateInstance(t));
                }
            }
            extra.Sort((a, b) => string.CompareOrdinal(a.Name, b.Name));
            for (int i = 0; i < extra.Count; i++)
            {
                bool clash = false;
                foreach (var k in list) if (k.Name == extra[i].Name) clash = true;
                if (clash) Debug.LogWarning("CelFxRenderer: two kinds are called " + extra[i].Name + "; the second is ignored.");
                else list.Add(extra[i]);
            }
            return list;
        }

        /// <summary>The id of a kind by name, -1 when there is none (and CelKinds.Chunks / Light for those layer kinds).</summary>
        public int KindId(string name)
        {
            if (name == CelKinds.ChunksName) return CelKinds.Chunks;
            if (name == CelKinds.LightName) return CelKinds.Light;
            Init();
            for (int i = 0; i < pools.Length; i++) if (pools[i].Kind.Name == name) return i;
            return -1;
        }

        public string KindName(int id) => id >= 0 && id < pools.Length ? pools[id].Kind.Name : null;
        public int CapacityOf(int kind) => kind >= 0 && kind < pools.Length ? pools[kind].Capacity : 0;
        public int SpawnedOf(int kind) => kind >= 0 && kind < pools.Length ? pools[kind].Spawned : 0;

        /// <summary>One primitive. r.Born is when it appears (CelFx.Now for at once; later to hold it back). No allocation.</summary>
        public void Spawn(int kind, in CelRecord r)
        {
            if (!Ready) Init();
            if ((uint)kind >= (uint)pools.Length) return;
            var pool = pools[kind];
            if (pool.Material == null) return;
            int i = pool.Head;
            records[pool.Start + i] = r;
            if (i < pool.DirtyLo) pool.DirtyLo = i;
            if (i > pool.DirtyHi) pool.DirtyHi = i;
            pool.Head = (i + 1) % pool.Capacity;
            if (pool.Count < pool.Capacity) pool.Count++;
            pool.Spawned++;
            float end = r.Born + r.Life;
            if (end > pool.AliveUntil) pool.AliveUntil = end;
        }

        void LateUpdate()
        {
            using var perf = LateMarker.Auto();
            CelFx.Pump();
            Submit(CelFx.Now);
        }

        /// <summary>Upload what was born, push the clock and the LOD, and draw every live pool once. LateUpdate calls it;
        /// a still in edit mode calls it itself before it renders.</summary>
        public void Submit(float now)
        {
            if (!Ready) Init();
            Shader.SetGlobalFloat(NowId, now);
            float lod = CelFx.LodBlend(CelFx.Zoom);
            Shader.SetGlobalVector(LodId, new Vector4(lod, CelFx.InkScale(lod), 0f, 0f));
            if (paletteVersion != CelPalette.Version)
            {
                palette.SetData(CelPalette.Rows);
                paletteVersion = CelPalette.Version;
            }
            DrawCalls = 0; Held = 0;
            for (int k = 0; k < pools.Length; k++)
            {
                var pool = pools[k];
                if (pool.DirtyHi >= pool.DirtyLo)
                {
                    int n = pool.DirtyHi - pool.DirtyLo + 1;
                    buffer.SetData(records, pool.Start + pool.DirtyLo, pool.Start + pool.DirtyLo, n);
                    pool.DirtyLo = int.MaxValue; pool.DirtyHi = -1;
                }
                Held += pool.Count;
                argsData[k] = new GraphicsBuffer.IndirectDrawIndexedArgs
                {
                    indexCountPerInstance = pool.Mesh != null ? pool.Mesh.GetIndexCount(0) : 0u, instanceCount = (uint)pool.Count,
                    startIndex = 0, baseVertexIndex = 0, startInstance = (uint)pool.Start
                };
            }
            if (pools.Length > 0) args.SetData(argsData, 0, 0, pools.Length);
            for (int o = 0; o < drawOrder.Length; o++)
            {
                int k = drawOrder[o];
                var pool = pools[k];
                if (pool.Count == 0 || pool.Material == null || now > pool.AliveUntil) continue;
                var rp = new RenderParams(pool.Material) { worldBounds = Everywhere, shadowCastingMode = ShadowCastingMode.Off, receiveShadows = false, matProps = pool.Props };
                FrameBudget.DrawIndirect(rp, pool.Mesh, args, 1, k);
                DrawCalls++;
            }
        }

        /// <summary>Forget every record (a still between shots, a test).</summary>
        public void Clear()
        {
            foreach (var p in pools) { p.Head = 0; p.Count = 0; p.AliveUntil = float.NegativeInfinity; p.DirtyLo = int.MaxValue; p.DirtyHi = -1; }
        }
    }
}
