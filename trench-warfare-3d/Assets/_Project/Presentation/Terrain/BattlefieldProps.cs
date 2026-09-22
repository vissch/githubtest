// Phase: B2 (instanced rendering only; generation lives in BattlefieldComposer)
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Rendering;

namespace TW.Presentation.Terrain
{
    public sealed class BattlefieldProps : MonoBehaviour
    {
        public SimHost Host;
        public int DecorationSeed = 1917;
        public Vector3 PreferredFront = Vector3.right;
        sealed class Batch
        {
            public BattlefieldKit.Module Module;
            Mesh Mesh => Module.Mesh;
            public readonly List<Matrix4x4[]> Pages = new List<Matrix4x4[]>(); public readonly List<int> Counts = new List<int>();
            public readonly List<Bounds> PageBounds = new List<Bounds>();
            readonly Dictionary<Vector2Int, int> spatialPages = new Dictionary<Vector2Int, int>();
            public void Clear() { Counts.Clear(); PageBounds.Clear(); spatialPages.Clear(); }
            public void Add(Matrix4x4 m)
            {
                var key = new Vector2Int(Mathf.FloorToInt(m.m03 / Module.PageSize), Mathf.FloorToInt(m.m23 / Module.PageSize));
                if (!spatialPages.TryGetValue(key, out int page) || Counts[page] == 1023)
                {
                    page = Counts.Count; Counts.Add(0); PageBounds.Add(default);
                    spatialPages[key] = page;
                    if (Pages.Count <= page) Pages.Add(new Matrix4x4[1023]);
                }
                Vector3 e = Mesh.bounds.extents;
                Vector3 a = m.MultiplyVector(new Vector3(e.x, 0f, 0f)), b = m.MultiplyVector(new Vector3(0f, e.y, 0f)), c = m.MultiplyVector(new Vector3(0f, 0f, e.z));
                var instanceBounds = new Bounds(m.MultiplyPoint3x4(Mesh.bounds.center), new Vector3(Mathf.Abs(a.x) + Mathf.Abs(b.x) + Mathf.Abs(c.x), Mathf.Abs(a.y) + Mathf.Abs(b.y) + Mathf.Abs(c.y), Mathf.Abs(a.z) + Mathf.Abs(b.z) + Mathf.Abs(c.z)) * 2f);
                var merged = Counts[page] == 0 ? instanceBounds : PageBounds[page];
                merged.Encapsulate(instanceBounds); PageBounds[page] = merged;
                Pages[page][Counts[page]] = m; Counts[page]++;
            }
        }


        BattlefieldKit kit;
        BattlefieldComposer composer;
        readonly Dictionary<BattlefieldKit.Module, Batch> batches = new Dictionary<BattlefieldKit.Module, Batch>();
        readonly Plane[] planes = new Plane[6];
        bool dirty = true, subscribed;
        public int VisibleInstances { get; private set; }
        public int SubmittedVertices { get; private set; }
        public int DrawCalls { get; private set; }
        public int CompositionCount => composer == null ? 0 : composer.Sites.Count;
        public string PlacementReport => composer?.PlacementReport ?? string.Empty;
        public IReadOnlyList<BattlefieldComposer.Site> Sites => composer?.Sites ?? System.Array.Empty<BattlefieldComposer.Site>();
        public BattlefieldKit Kit => kit;

        public void UseBlueprints(BattlefieldBlueprint[] templates, int seed, Vector3 preferredFront)
        {
            if (kit == null) throw new System.InvalidOperationException("Configure blueprints after BattlefieldProps.Start.");
            DecorationSeed = seed; PreferredFront = preferredFront;
            composer = new BattlefieldComposer(kit, seed, templates, preferredFront); dirty = true;
        }

        void Start()
        {
            kit = new BattlefieldKit(); composer = new BattlefieldComposer(kit, DecorationSeed, preferredFront: PreferredFront);
            foreach (var module in kit.Modules) batches.Add(module, new Batch { Module = module });
        }
        void OnSimEvent(TW.Sim.SimEvent e)
        {
            if (e.Type == TW.Sim.SimEventType.PropChanged || e.Type == TW.Sim.SimEventType.WireBreached || e.Type == TW.Sim.SimEventType.CraterStamp) dirty = true;
        }
        void OnDestroy()
        {
            if (subscribed && Host != null) Host.Events.OnEvent -= OnSimEvent;
            kit?.Dispose();
        }
        void Update()
        {
            if (Host == null || Host.Local == null || kit == null) return;
            if (!subscribed) { Host.Events.OnEvent += OnSimEvent; subscribed = true; }
            if (dirty)
            {
                dirty = false;
                foreach (var batch in batches.Values) batch.Clear();
                composer.Build(Host.Local.Map, GetComponent<GreyboxTerrainView>().Surface, (module, matrix) =>
                {
                    if (!batches.TryGetValue(module, out var batch)) { batch = new Batch { Module = module }; batches.Add(module, batch); }
                    batch.Add(matrix);
                });
            }
            var cam = Camera.main;
            if (cam != null) GeometryUtility.CalculateFrustumPlanes(cam, planes);
            VisibleInstances = SubmittedVertices = DrawCalls = 0;
            Vector3 eye = cam != null ? cam.transform.position : Vector3.zero;
            foreach (var b in batches.Values)
            for (int p = 0; p < b.Counts.Count; p++)
            {
                float reach = b.Module.MaxDistance;
                if (reach < float.PositiveInfinity && (SceneHooks.CloseUp <= 0f || b.PageBounds[p].SqrDistance(eye) > reach * reach)) continue;   // small things: close camera only
                var pageBounds = b.PageBounds[p]; pageBounds.Expand(16f);
                if (cam != null && !GeometryUtility.TestPlanesAABB(planes, pageBounds)) continue;
                var module = b.Module;
                var rp = new RenderParams(module.Material) { worldBounds = pageBounds, shadowCastingMode = module.Shadows ? ShadowCastingMode.On : ShadowCastingMode.Off, receiveShadows = true };
                Graphics.RenderMeshInstanced(rp, module.Mesh, 0, b.Pages[p], b.Counts[p]);
                VisibleInstances += b.Counts[p]; SubmittedVertices += b.Counts[p] * module.Mesh.vertexCount; DrawCalls++;
            }
        }
    }
}
