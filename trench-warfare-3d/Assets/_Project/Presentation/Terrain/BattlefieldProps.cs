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
        /// <summary>Hand edits to the imported props (Resources/Layouts/Battlefield&lt;seed&gt;, written by TW > Env Props); null when there are none.</summary>
        public PropLayout Layout;
        /// <summary>A second camera whose view is drawn as well (the Scene view while props are edited); the counters stay the game camera's.</summary>
        public static Camera EditorCamera;
        /// <summary>Asked for every instance composed: true keeps it out (PropDestruction: what a shell has knocked down stays
        /// down through every recomposition). Keyed by module and placement, never by page and slot, which change.</summary>
        public System.Func<BattlefieldKit.Module, Matrix4x4, bool> Suppress;
        /// <summary>Asked for every instance composed, after Suppress: another module to draw at the same matrix instead
        /// (PropDestruction puts a damaged lining panel's broken twin where the panel stood), or null for the module itself.</summary>
        public System.Func<BattlefieldKit.Module, Matrix4x4, BattlefieldKit.Module> Replace;
        /// <summary>For a masked module (a house), the chunks of the instance at this matrix to hide, a bit each
        /// (PropDestruction: the chunks it has knocked down). Asked for every visible instance as it is submitted.</summary>
        public System.Func<BattlefieldKit.Module, Matrix4x4, HouseKit.ChunkMask> MaskOf;
        readonly Vector4[] mergedMask = new Vector4[1023];
        MaterialPropertyBlock maskBlock;

        sealed class Batch
        {
            public BattlefieldKit.Module Module;
            Mesh Mesh => Module.Mesh;
            public readonly List<Matrix4x4[]> Pages = new List<Matrix4x4[]>(); public readonly List<int> Counts = new List<int>();
            public readonly List<Bounds> PageBounds = new List<Bounds>();
            readonly Dictionary<Vector2Int, int> spatialPages = new Dictionary<Vector2Int, int>();
            public void Clear() { Counts.Clear(); PageBounds.Clear(); spatialPages.Clear(); }
            public int Add(Matrix4x4 m, out int slot)
            {
                var key = new Vector2Int(Mathf.FloorToInt(m.m03 / Module.PageSize), Mathf.FloorToInt(m.m23 / Module.PageSize));
                if (!spatialPages.TryGetValue(key, out int page) || Counts[page] == 1023)
                {
                    page = Counts.Count; Counts.Add(0); PageBounds.Add(default);
                    spatialPages[key] = page;
                    if (Pages.Count <= page) Pages.Add(new Matrix4x4[1023]);
                }
                var instanceBounds = InstanceBounds(m);
                var merged = Counts[page] == 0 ? instanceBounds : PageBounds[page];
                merged.Encapsulate(instanceBounds); PageBounds[page] = merged;
                slot = Counts[page]; Pages[page][slot] = m; Counts[page]++;
                return page;
            }
            Bounds InstanceBounds(Matrix4x4 m)
            {
                Vector3 e = Mesh.bounds.extents;
                Vector3 a = m.MultiplyVector(new Vector3(e.x, 0f, 0f)), b = m.MultiplyVector(new Vector3(0f, e.y, 0f)), c = m.MultiplyVector(new Vector3(0f, 0f, e.z));
                return new Bounds(m.MultiplyPoint3x4(Mesh.bounds.center), new Vector3(Mathf.Abs(a.x) + Mathf.Abs(b.x) + Mathf.Abs(c.x), Mathf.Abs(a.y) + Mathf.Abs(b.y) + Mathf.Abs(c.y), Mathf.Abs(a.z) + Mathf.Abs(b.z) + Mathf.Abs(c.z)) * 2f);
            }
            /// <summary>Moves one instance; its page grows to hold it wherever it went.</summary>
            public void Set(int page, int slot, Matrix4x4 m)
            {
                Pages[page][slot] = m;
                var merged = PageBounds[page]; merged.Encapsulate(InstanceBounds(m)); PageBounds[page] = merged;
            }
            /// <summary>Takes one instance out of sight without reshuffling the page: shrunk to nothing deep under the ground.</summary>
            public void Hide(int page, int slot) => Pages[page][slot] = Matrix4x4.TRS(Pages[page][slot].GetPosition() + Vector3.down * 500f, Quaternion.identity, Vector3.one * 1e-4f);
        }

        /// <summary>One imported prop as placed this composition: what the Scene view picks and the editor moves (EnvPropEditor).</summary>
        public struct Placed
        {
            public BattlefieldKit.Module Module;
            public string Key;
            public bool Added;
            /// <summary>Where it is drawn, and where the composer alone would have put it (the kind's size included).</summary>
            public Matrix4x4 Matrix, Generated;
            internal int Page, Slot;
        }

        BattlefieldKit kit;
        BattlefieldComposer composer;
        BattlefieldBlueprint[] templates;
        TW.Sim.Terrain.MapData map;
        BattlefieldSurface surface;
        readonly Dictionary<BattlefieldKit.Module, Batch> batches = new Dictionary<BattlefieldKit.Module, Batch>();
        readonly Dictionary<string, BattlefieldKit.Module> modulesByName = new Dictionary<string, BattlefieldKit.Module>();
        readonly List<Placed> placed = new List<Placed>();
        readonly Dictionary<string, int> placedByKey = new Dictionary<string, int>(), spots = new Dictionary<string, int>();
        readonly HashSet<string> held = new HashSet<string>();
        readonly Plane[] planes = new Plane[6], editorPlanes = new Plane[6];
        /// <summary>
        /// One buffer, refilled for every module every frame, so the visible pages of a module go to the GPU as one
        /// submission instead of one per 32 m page. Measured 2026-09-22: the props were spending 271 draw calls on
        /// 1,706 instances — six instances a call — because a page is a culling unit and was being used as a batching
        /// unit too. 1023 is the instancing limit of RenderMeshInstanced; a module with more than that submits again.
        /// Allocated once and reused, because the frame budget bans managed allocation during play.
        /// </summary>
        readonly Matrix4x4[] merged = new Matrix4x4[1023];
        bool dirty = true, subscribed;
        public int VisibleInstances { get; private set; }
        public int SubmittedVertices { get; private set; }
        public int DrawCalls { get; private set; }
        public int CompositionCount => composer == null ? 0 : composer.Sites.Count;
        /// <summary>Counts compositions applied. Pages and slots are renumbered by each one, so anything that remembers a
        /// (page, slot) — PropWear's swept squares and knocked props — watches this and throws its own away when it moves.</summary>
        public int Generation { get; private set; }
        public string PlacementReport => composer?.PlacementReport ?? string.Empty;
        public IReadOnlyList<BattlefieldComposer.Site> Sites => composer?.Sites ?? System.Array.Empty<BattlefieldComposer.Site>();
        /// <summary>The scattered lanterns' feet (docs/21 phase 2): NightLights lights those first.</summary>
        public IReadOnlyList<Vector3> LanternPoints => composer != null ? composer.LanternPoints : (IReadOnlyList<Vector3>)System.Array.Empty<Vector3>();
        public BattlefieldKit Kit => kit;
        /// <summary>Every imported prop placed this composition, hand edits applied.</summary>
        public IReadOnlyList<Placed> Editable => placed;

        public void UseBlueprints(BattlefieldBlueprint[] templates, int seed, Vector3 preferredFront)
        {
            if (kit == null) throw new System.InvalidOperationException("Configure blueprints after BattlefieldProps.Start.");
            DecorationSeed = seed; PreferredFront = preferredFront; this.templates = templates;
            Layout = Resources.Load<PropLayout>(PropLayout.ResourcePath(seed));
            Restyle();
        }

        void Start()
        {
            kit = new BattlefieldKit();
            kit.ResolveKeysAndRules();   // every module named for the scale table and carrying its rule (docs/21 phase 1)
            groundFn = Ground;
            SceneHooks.Biplane = () => kit.biplane != null ? (kit.biplane.Mesh, kit.biplane.Material, kit.biplane.Size) : (null, null, Vector3.one);   // the strafe's aircraft (CombatFx.Abilities.cs)
            foreach (var module in kit.Modules)
            {
                batches.Add(module, new Batch { Module = module });
                if (module.Name != null) modulesByName[module.Name] = module;
            }
            if (Layout == null) Layout = Resources.Load<PropLayout>(PropLayout.ResourcePath(DecorationSeed));
            Restyle();
        }

        /// <summary>Gives every kind its look's size and composes afresh: the composer spaces what stands round a prop by
        /// its size, and the blueprints bake it into their footprints. Call after a look changes.</summary>
        public void Restyle()
        {
            // the size the composer spaces by is the look's baseline, brought into the kind's bounds the way every drawn
            // instance will be (Enforce), so spacing, blueprint footprints and Room() agree with what is drawn
            foreach (var module in modulesByName.Values)
            {
                var baseline = Layout?.LookOf(module.Name)?.Baseline ?? Vector3.one;
                module.Size = module.Mesh != null ? AssetScaleTable.Clamp(module.Rule, module.Mesh.bounds.size, baseline) : baseline;
            }
            composer = new BattlefieldComposer(kit, DecorationSeed, templates, PreferredFront);
            composing = null; composed = false;   // a new composer: the next composition is whole, from its first layout
            dirty = true;
        }

        System.Func<float, float, float> groundFn;

        /// <summary>A generated prop drawn in its kind's look (PropLayout.Style): the baseline scale, turn, lean and sink, each
        /// strayed from by the look's range, hashed on the prop's key so the battlefield looks the same every time.</summary>
        Matrix4x4 Styled(BattlefieldKit.Module module, Matrix4x4 m, string key)
        {
            var look = Layout != null ? Layout.LookOf(module.Name) : null;
            return look == null ? m : PropLayout.Style(look, m, key, groundFn);
        }

        /// <summary>The scale rule applied (docs/21 phase 1): a Strict thing drawn outside its bounds against the soldier is
        /// pulled back by one uniform factor, whatever the look, the composer or a hand edit said. Here rather than in
        /// Styled, which returns early for a module with no look, and because hand edits never pass through Styled.</summary>
        Matrix4x4 Enforce(BattlefieldKit.Module module, in Matrix4x4 m)
        {
            if (!module.Rule.Enforce || module.Mesh == null) return m;
            var scale = m.lossyScale;
            var clamped = AssetScaleTable.Clamp(module.Rule, module.Mesh.bounds.size, scale);
            return clamped == scale ? m : Matrix4x4.TRS(m.GetPosition(), m.rotation, clamped);
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

        public BattlefieldKit.Module ModuleNamed(string name) => name != null && modulesByName.TryGetValue(name, out var module) ? module : null;
        public void Recompose() => dirty = true;
        /// <summary>Every drawn instance of a module whose foot is within radius of a point (xz), as (page, slot, matrix).</summary>
        public void Within(BattlefieldKit.Module module, Vector2 centre, float radius, List<(int page, int slot, Matrix4x4 m)> into)
        {
            if (module == null || !batches.TryGetValue(module, out var b)) return;
            float r2 = radius * radius;
            for (int p = 0; p < b.Counts.Count; p++)
            {
                var pb = b.PageBounds[p];
                float dx = Mathf.Max(0f, Mathf.Abs(centre.x - pb.center.x) - pb.extents.x), dz = Mathf.Max(0f, Mathf.Abs(centre.y - pb.center.z) - pb.extents.z);
                if (dx * dx + dz * dz > r2) continue;
                var page = b.Pages[p];
                for (int s = 0, n = b.Counts[p]; s < n; s++)
                {
                    var m = page[s];
                    if (m.m13 < -400f) continue;   // hidden: Batch.Hide sinks it 500 m
                    float ex = m.m03 - centre.x, ez = m.m23 - centre.y;
                    if (ex * ex + ez * ez <= r2) into.Add((p, s, m));
                }
            }
        }
        /// <summary>Takes one instance out of the draw now; Suppress keeps it out of the next composition.</summary>
        /// <summary>Moves one drawn instance where it stands, without touching where it was placed: PropWear knocks a
        /// helmet about as rounds go into it. The next composition puts it back, so nothing has to be undone.</summary>
        public void Move(BattlefieldKit.Module module, int page, int slot, Matrix4x4 m)
        {
            if (module != null && batches.TryGetValue(module, out var b) && page >= 0 && page < b.Counts.Count && slot >= 0 && slot < b.Counts[page]) b.Set(page, slot, m);
        }

        public void Hide(BattlefieldKit.Module module, int page, int slot)
        {
            if (module != null && batches.TryGetValue(module, out var b) && page < b.Counts.Count && slot < b.Counts[page]) b.Hide(page, slot);
        }
        /// <summary>Adds one drawn instance now, outside a composition (PropDestruction puts a damaged twin where a lining
        /// panel stood). The next composition places it again through Replace, so nothing has to be undone.</summary>
        public void AddInstance(BattlefieldKit.Module module, Matrix4x4 m)
        {
            if (module != null) BatchOf(module).Add(m, out _);
        }
        public bool TryGetPlaced(string key, out Placed prop)
        {
            if (placedByKey.TryGetValue(key, out int i)) { prop = placed[i]; return true; }
            prop = default; return false;
        }

        /// <summary>The drawn ground at a point, on the map or on the land beyond it.</summary>
        public float Ground(float x, float z)
        {
            if (map == null || surface == null) return 0f;
            bool inside = x >= 0f && z >= 0f && x <= map.SizeMeters.x && z <= map.SizeMeters.y;
            return inside ? surface.VisualHeight(x, z) : GreyboxTerrainView.SkirtHeight(map, x, z);
        }

        /// <summary>Where a hand edit puts its prop now: on the ground as it is today, at the kind's size.</summary>
        public Matrix4x4 Placement(PropLayout.Edit edit)
        {
            var p = edit.Position;
            var scale = edit.Scale * SizeOf(edit.Module);
            // a hand edit is clamped like anything else: the owner's placements keep their spot and turn, not a 15 m gun
            if (modulesByName.TryGetValue(edit.Module, out var module) && module.Mesh != null) scale = AssetScaleTable.Clamp(module.Rule, module.Mesh.bounds.size, scale);
            return Matrix4x4.TRS(new Vector3(p.x, Ground(p.x, p.z) + p.y, p.z), edit.Rotation, scale);
        }
        float SizeOf(string module) => Layout != null ? Layout.SizeOf(module) : 1f;

        /// <summary>Takes a prop out of the draw while the editor shows it as a GameObject (PropHandle).</summary>
        public void Hold(string key)
        {
            held.Add(key);
            if (placedByKey.TryGetValue(key, out int i))
            {
                batches[placed[i].Module].Hide(placed[i].Page, placed[i].Slot);
                if (placed[i].Module.Sliced != null) dirty = true;   // its building and chunks come out at the next composition
            }
        }

        /// <summary>Puts a held prop back into the draw at a matrix; with none (removed) it stays out and the next composition drops it.</summary>
        public void Release(string key, BattlefieldKit.Module module, Matrix4x4? matrix, bool added)
        {
            held.Remove(key);
            if (matrix == null) return;
            if (placedByKey.TryGetValue(key, out int i))
            {
                var prop = placed[i]; prop.Matrix = matrix.Value; placed[i] = prop;
                batches[prop.Module].Set(prop.Page, prop.Slot, matrix.Value);
                if (prop.Module.Sliced != null) dirty = true;   // its chunks follow at the next composition
            }
            else if (module != null) Put(module, key, added, matrix.Value, null);   // added by hand, or brought back after the composition dropped it
        }

        Batch BatchOf(BattlefieldKit.Module module)
        {
            if (!batches.TryGetValue(module, out var batch)) { batch = new Batch { Module = module }; batches.Add(module, batch); }
            return batch;
        }

        void Put(BattlefieldKit.Module module, string key, bool added, Matrix4x4 generated, PropLayout.Edit edit)
        {
            var matrix = edit != null ? Placement(edit) : generated;
            if (added) generated = matrix;
            var batch = BatchOf(module);
            int page = batch.Add(matrix, out int slot);
            if (held.Contains(key)) batch.Hide(page, slot);
            else if (module.Sliced != null) PutSliced(module.Sliced, matrix);
            placedByKey[key] = placed.Count;
            placed.Add(new Placed { Module = module, Key = key, Added = added, Matrix = matrix, Generated = generated, Page = page, Slot = slot });
        }

        /// <summary>A sliced prop where it stands: its building's whole mesh (the chunks that have gone masked off, MaskOf)
        /// and every chunk still standing, to be found, hit and hidden. Unnamed, so they follow the prop's own matrix.</summary>
        void PutSliced(HouseKit.House house, in Matrix4x4 matrix)
        {
            BatchOf(house.Whole).Add(matrix, out _);
            foreach (var chunk in house.Chunks)
            {
                var at = HouseKit.Place(matrix, chunk);
                if (Suppress == null || !Suppress(chunk.Module, at)) BatchOf(chunk.Module).Add(at, out _);
            }
        }

        // A recomposition in progress: the composer's generators run a step a frame into a staging list
        // (BattlefieldComposer.BuildSteps), and the batches are rebuilt from it in one go when the last has run, so what
        // is drawn is always a whole composition. A crater that lands meanwhile only sets `dirty`: this one finishes and
        // the next starts, so the props converge on the ground as it is, as they did when every crater recomposed at once.
        IEnumerator<bool> composing;
        bool composed, readyToApply;
        readonly List<BattlefieldKit.Module> stagedModules = new List<BattlefieldKit.Module>(8192);
        readonly List<Matrix4x4> stagedMatrices = new List<Matrix4x4>(8192);
        System.Action<BattlefieldKit.Module, Matrix4x4> stage;

        /// <summary>A whole composition at once: the first, and after Restyle.</summary>
        void Compose()
        {
            BeginComposition();
            while (composing.MoveNext()) { }
            ApplyComposition();
        }

        void BeginComposition()
        {
            map = Host.Local.Map; surface = GetComponent<GreyboxTerrainView>().Surface;
            stagedModules.Clear(); stagedMatrices.Clear();
            if (stage == null) stage = (module, matrix) => { stagedModules.Add(module); stagedMatrices.Add(matrix); };
            composing = composer.BuildSteps(map, surface, stage).GetEnumerator();
            readyToApply = false;
        }

        /// <summary>The staged composition into the batches. The composer reads nothing of ours, so emitting after it has
        /// run instead of while it runs places every instance exactly as before.</summary>
        void ApplyComposition()
        {
            Generation++;
            composing = null; readyToApply = false;
            foreach (var batch in batches.Values) batch.Clear();
            placed.Clear(); placedByKey.Clear(); spots.Clear();
            for (int i = 0; i < stagedModules.Count; i++) Emit(stagedModules[i], stagedMatrices[i]);
            if (Layout != null)
                foreach (var edit in Layout.Edits)
                    if (edit.Added && !edit.Removed && modulesByName.TryGetValue(edit.Module, out var module)) Put(module, edit.Key, true, default, edit);
            composed = true;
        }

        void Emit(BattlefieldKit.Module module, Matrix4x4 matrix)
        {
            if (Suppress != null && Suppress(module, matrix)) return;
            if (Replace != null) { var other = Replace(module, matrix); if (other != null) module = other; }
            if (module.Name == null) { BatchOf(module).Add(Enforce(module, matrix), out _); return; }
            // an imported prop: found by kind and spot, so a hand edit (PropLayout) finds it again after every crater
            string key = PropLayout.GeneratedKey(module.Name, matrix.GetPosition());
            spots.TryGetValue(key, out int n); spots[key] = n + 1;
            if (n > 0) key += "#" + n;
            var edit = Layout != null ? Layout.Find(key) : null;
            if (edit != null && edit.Removed) return;
            Put(module, key, false, Enforce(module, Styled(module, matrix, key)), edit);
        }

        void Update()
        {
            using var perf = TW.Sim.PerfMarkers.PropsUpdate.Auto();
            if (Host == null || Host.Local == null || kit == null) return;
            if (!subscribed) { Host.Events.OnEvent += OnSimEvent; subscribed = true; }
            // A whole-map recomposition (28.5 ms in the editor, 2026-09-23) after a crater: a composer step a frame, then the
            // batches rebuilt in a frame of their own, each a HeavyWork turn; started only once the terrain has rescanned the
            // hollows the crater made (the composer reads them). The props stand as they were a few frames longer. The first
            // composition is whole, so the field is dressed from the first frame.
            if (composing != null)
            {
                if (HeavyWork.TryClaim())
                {
                    TW.Sim.PerfMarkers.PropsCompose.Begin();
                    if (readyToApply) ApplyComposition();
                    else if (!composing.MoveNext()) readyToApply = true;
                    TW.Sim.PerfMarkers.PropsCompose.End();
                }
            }
            else if (dirty)
            {
                var terrainView = GetComponent<GreyboxTerrainView>();
                if ((terrainView == null || !terrainView.HollowsPending) && HeavyWork.TryClaim())
                {
                    dirty = false;
                    TW.Sim.PerfMarkers.PropsCompose.Begin();
                    if (!composed) Compose();
                    else { BeginComposition(); if (!composing.MoveNext()) readyToApply = true; }
                    TW.Sim.PerfMarkers.PropsCompose.End();
                }
            }
            var cam = Camera.main;
            if (cam != null) GeometryUtility.CalculateFrustumPlanes(cam, planes);
            var editorCam = EditorCamera != null && EditorCamera != cam ? EditorCamera : null;
            if (editorCam != null) GeometryUtility.CalculateFrustumPlanes(editorCam, editorPlanes);
            VisibleInstances = SubmittedVertices = DrawCalls = 0;
            Vector3 eye = cam != null ? cam.transform.position : Vector3.zero;
            // Pages are culled one at a time, as they must be, and then merged into one submission per module: the
            // page is the right unit to decide what the camera can see and the wrong unit to hand to the GPU.
            foreach (var b in batches.Values)
            {
                var module = b.Module;
                if (!module.Drawn) continue;
                int held = 0; Bounds union = default; bool any = false;
                for (int p = 0; p < b.Counts.Count; p++)
                {
                    float reach = module.MaxDistance; bool small = reach < float.PositiveInfinity;
                    var pageBounds = b.PageBounds[p]; pageBounds.Expand(16f);
                    bool seen = (!small || SceneHooks.CloseUp > 0f && b.PageBounds[p].SqrDistance(eye) <= reach * reach)   // small things: close camera only
                        && (cam == null || GeometryUtility.TestPlanesAABB(planes, pageBounds));
                    bool editorSeen = editorCam != null && (!small || b.PageBounds[p].SqrDistance(editorCam.transform.position) <= reach * reach)
                        && GeometryUtility.TestPlanesAABB(editorPlanes, pageBounds);
                    if (!seen && !editorSeen) continue;
                    int count = b.Counts[p];
                    if (count == 0) continue;
                    if (seen) { VisibleInstances += count; SubmittedVertices += count * module.Mesh.vertexCount; }
                    for (int from = 0; from < count; )
                    {
                        // inside the loop, not above it: a page that spills into a second submission has to widen
                        // that one's bounds too, or everything after a full buffer is drawn with bounds of nothing
                        if (any) union.Encapsulate(pageBounds); else { union = pageBounds; any = true; }
                        int take = Mathf.Min(merged.Length - held, count - from);
                        System.Array.Copy(b.Pages[p], from, merged, held, take);
                        if (module.Masked)
                            for (int k = 0; k < take; k++) mergedMask[held + k] = MaskOf != null ? MaskOf(module, merged[held + k]).Packed : Vector4.zero;
                        held += take; from += take;
                        if (held < merged.Length) continue;
                        Submit(module, union, held); held = 0; any = false;   // full: send it and start the next
                    }
                }
                if (held > 0) Submit(module, union, held);
            }
        }

        /// <summary>One instanced submission of a module, with the bounds of every page that went into it.</summary>
        void Submit(BattlefieldKit.Module module, Bounds bounds, int count)
        {
            var rp = new RenderParams(module.Material) { worldBounds = bounds, shadowCastingMode = module.Shadows ? ShadowCastingMode.On : ShadowCastingMode.Off, receiveShadows = true };
            if (module.Masked)
            {
                // the block is copied when the draw is queued, so one block serves every masked submission
                maskBlock ??= new MaterialPropertyBlock();
                maskBlock.SetVectorArray("_ChunkMask", mergedMask);
                rp.matProps = maskBlock;
            }
            FrameBudget.Draw(rp, module.Mesh, 0, merged, count);
            DrawCalls++;
        }
    }
}
