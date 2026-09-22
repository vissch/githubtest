// Phase: B2 (editor: hand placement of the imported props over the composed battlefield)
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using TW.Presentation.Terrain;
using UnityEditor;
using UnityEngine;
using UnityEngine.Rendering;

namespace TW.Editor
{
    /// <summary>
    /// Hand placement of the imported props (TW > Env Props). In Play, click a prop in the Scene view to select it, then
    /// move, rotate and scale it with the usual tools (W, E, R); Delete removes it, Ctrl+D copies it. Every change goes
    /// straight into the battlefield's PropLayout (Resources/Layouts), so it outlives Play and ships with the game. While
    /// selected the prop is a stand-in GameObject (PropHandle) under "Env Prop Edits"; deselected, it goes back into the
    /// batched draw. The props exist only in Play: the battlefield is composed when the game starts.
    /// </summary>
    [InitializeOnLoad]
    public static class EnvPropEditing
    {
        const string EnabledKey = "TW.EnvProps.Edit";
        const string Folder = "Assets/_Project/Resources/Layouts";

        sealed class Live { public PropHandle Handle; public string Module; public bool Added; public double Born; }
        static readonly Dictionary<string, Live> live = new Dictionary<string, Live>();
        static readonly MethodInfo intersectRayMesh = typeof(HandleUtility).GetMethod("IntersectRayMesh", BindingFlags.Static | BindingFlags.NonPublic | BindingFlags.Public);
        static BattlefieldProps props;
        static PropLayout pending;
        static GameObject root, unpickable;
        static bool leaving;
        static double saveAt = -1;

        public static bool Enabled
        {
            get => EditorPrefs.GetBool(EnabledKey, true);
            set { EditorPrefs.SetBool(EnabledKey, value); if (!value) Stop(); }
        }

        static EnvPropEditing()
        {
            HandleUtility.pickGameObjectCustomPasses += Pick;
            SceneView.duringSceneGui += DrawSelected;
            EditorApplication.update += Tick;
            EditorApplication.playModeStateChanged += state =>
            {
                if (state == PlayModeStateChange.ExitingPlayMode) { leaving = true; Stop(); Save(); }
                else if (state == PlayModeStateChange.EnteredPlayMode || state == PlayModeStateChange.EnteredEditMode) { leaving = false; live.Clear(); root = null; props = null; }
            };
        }

        public static BattlefieldProps Props()
        {
            if (!EditorApplication.isPlaying) return null;
            if (props == null) props = Object.FindFirstObjectByType<BattlefieldProps>();
            return props;
        }

        /// <summary>The battlefield's layout asset, made the first time a prop is edited.</summary>
        public static PropLayout Layout(BattlefieldProps field)
        {
            if (field.Layout != null) return field.Layout;
            string path = Folder + "/Battlefield" + field.DecorationSeed + ".asset";
            var layout = AssetDatabase.LoadAssetAtPath<PropLayout>(path);
            if (layout == null)
            {
                if (!AssetDatabase.IsValidFolder(Folder)) AssetDatabase.CreateFolder("Assets/_Project/Resources", "Layouts");
                layout = ScriptableObject.CreateInstance<PropLayout>(); layout.Seed = field.DecorationSeed;
                AssetDatabase.CreateAsset(layout, path);
            }
            return field.Layout = layout;
        }

        static void Tick()
        {
            if (!EditorApplication.isPlaying || leaving) return;
            var field = Props();
            if (field == null || !Enabled) { if (live.Count > 0 || unpickable != null) Stop(); return; }
            // a custom pick is only asked when Unity's own finds nothing, and the terrain lies under every click
            Unpickable(field.gameObject);
            var view = SceneView.lastActiveSceneView;
            BattlefieldProps.EditorCamera = view != null ? view.camera : null;

            // stand-ins that arrived on their own: a copy (Ctrl+D) is a new prop, an undone delete brings its prop back
            foreach (var handle in PropHandle.All.ToArray()) if (!handle.Registered) Adopt(field, handle);
            foreach (var kv in live.ToArray())
            {
                var entry = kv.Value;
                if (entry.Handle == null) { live.Remove(kv.Key); Removed(field, kv.Key, entry); continue; }
                var t = entry.Handle.transform;
                if (t.hasChanged) { t.hasChanged = false; Commit(field, entry.Handle); }
                if (!Selection.Contains(entry.Handle.gameObject) && EditorApplication.timeSinceStartup - entry.Born > .3) Release(field, kv.Key, entry);
            }
            if (saveAt > 0 && EditorApplication.timeSinceStartup > saveAt) Save();
        }

        /// <summary>Hands every stand-in back to the batched draw and gives the terrain its picking back.</summary>
        static void Stop()
        {
            var field = props;
            foreach (var kv in live.ToArray()) if (kv.Value.Handle != null) Release(field, kv.Key, kv.Value);
            live.Clear();
            Unpickable(null);
            BattlefieldProps.EditorCamera = null;
        }

        static void Unpickable(GameObject terrain)
        {
            if (unpickable == terrain) return;
            if (unpickable != null) SceneVisibilityManager.instance.EnablePicking(unpickable, true);
            unpickable = terrain;
            if (terrain != null) SceneVisibilityManager.instance.DisablePicking(terrain, true);
        }

        static void Save()
        {
            saveAt = -1;
            if (pending != null) AssetDatabase.SaveAssetIfDirty(pending);
        }

        static void Dirty(PropLayout layout)
        {
            EditorUtility.SetDirty(layout); pending = layout;
            saveAt = EditorApplication.timeSinceStartup + 1.0;
        }

        // ---- picking --------------------------------------------------------------------------------------------------

        static GameObject Pick(Camera cam, int layers, Vector2 position, GameObject[] ignore, GameObject[] filter, out int materialIndex)
        {
            materialIndex = -1;
            var e = Event.current;
            // a click makes a stand-in; a pick asked while hovering, dragging or drawing must not
            if (!Enabled || filter != null || e != null && (e.type == EventType.MouseMove || e.type == EventType.MouseDrag || e.type == EventType.Repaint || e.type == EventType.Layout)) return null;
            var field = Props();
            if (field == null) return null;
            var ray = cam.ScreenPointToRay(position);
            float nearest = GroundDistance(field, ray) + .05f;
            // the stand-ins first: TW/Toon has no picking pass, so Unity's own pick does not see them
            GameObject best = null;
            foreach (var entry in live.Values)
            {
                if (entry.Handle == null || ignore != null && ignore.Contains(entry.Handle.gameObject)) continue;
                var mesh = entry.Handle.GetComponent<MeshFilter>().sharedMesh;
                if (Hit(ray, mesh, entry.Handle.transform.localToWorldMatrix, out float t) && t < nearest) { nearest = t; best = entry.Handle.gameObject; }
            }
            var list = field.Editable; int pick = -1;
            for (int i = 0; i < list.Count; i++)
            {
                var prop = list[i];
                if (live.ContainsKey(prop.Key)) continue;
                if (Hit(ray, prop.Module.Mesh, prop.Matrix, out float t) && t < nearest) { nearest = t; pick = i; }
            }
            if (pick >= 0) best = Spawn(field, list[pick]).gameObject;
            return best;
        }

        /// <summary>The ray against the mesh's box, then against the mesh itself (world metres along the ray).</summary>
        static bool Hit(Ray ray, Mesh mesh, Matrix4x4 matrix, out float t)
        {
            var inverse = matrix.inverse;
            if (!Slab(inverse.MultiplyPoint3x4(ray.origin), inverse.MultiplyVector(ray.direction), mesh.bounds, out t)) return false;
            if (intersectRayMesh == null) return true;
            var args = new object[] { ray, mesh, matrix, null };
            if (!(bool)intersectRayMesh.Invoke(null, args)) return false;
            t = ((RaycastHit)args[3]).distance;
            return true;
        }

        static bool Slab(Vector3 origin, Vector3 direction, Bounds box, out float t)
        {
            float near = 0f, far = float.MaxValue; t = 0f;
            Vector3 lo = box.min, hi = box.max;
            for (int a = 0; a < 3; a++)
            {
                if (Mathf.Abs(direction[a]) < 1e-9f) { if (origin[a] < lo[a] || origin[a] > hi[a]) return false; continue; }
                float t1 = (lo[a] - origin[a]) / direction[a], t2 = (hi[a] - origin[a]) / direction[a];
                if (t1 > t2) (t1, t2) = (t2, t1);
                near = Mathf.Max(near, t1); far = Mathf.Min(far, t2);
                if (near > far) return false;
            }
            t = near; return true;
        }

        /// <summary>How far along the ray the drawn ground is, so nothing is picked through a hill.</summary>
        static float GroundDistance(BattlefieldProps field, Ray ray)
        {
            var o = ray.origin;
            if (o.y < field.Ground(o.x, o.z)) return float.PositiveInfinity;
            float t = 0f, step = .25f;
            for (int k = 0; k < 4000 && t < 3000f; k++)
            {
                float next = t + step; var p = ray.GetPoint(next);
                if (p.y < field.Ground(p.x, p.z))
                {
                    float lo = t, hi = next;
                    for (int b = 0; b < 14; b++) { float mid = (lo + hi) * .5f; var q = ray.GetPoint(mid); if (q.y < field.Ground(q.x, q.z)) hi = mid; else lo = mid; }
                    return hi;
                }
                t = next; step = Mathf.Max(.25f, t * .01f);
            }
            return float.PositiveInfinity;
        }

        static void DrawSelected(SceneView view)
        {
            if (live.Count == 0 || Event.current.type != EventType.Repaint) return;
            var keep = Handles.matrix;
            Handles.color = new Color(1f, .62f, .2f, .9f);
            foreach (var entry in live.Values)
            {
                if (entry.Handle == null) continue;
                var bounds = entry.Handle.GetComponent<MeshFilter>().sharedMesh.bounds;
                Handles.matrix = entry.Handle.transform.localToWorldMatrix;
                Handles.DrawWireCube(bounds.center, bounds.size);
            }
            Handles.matrix = keep;
        }

        // ---- stand-ins ------------------------------------------------------------------------------------------------

        static PropHandle Spawn(BattlefieldProps field, BattlefieldProps.Placed prop)
        {
            if (live.TryGetValue(prop.Key, out var entry) && entry.Handle != null) return entry.Handle;
            if (root == null) root = new GameObject("Env Prop Edits");
            var go = new GameObject(prop.Module.Name.Substring(prop.Module.Name.IndexOf('/') + 1));
            go.transform.SetParent(root.transform, false);
            Place(go.transform, prop.Matrix);
            go.AddComponent<MeshFilter>().sharedMesh = prop.Module.Mesh;
            var renderer = go.AddComponent<MeshRenderer>();
            renderer.sharedMaterial = prop.Module.Material;
            renderer.shadowCastingMode = prop.Module.Shadows ? ShadowCastingMode.On : ShadowCastingMode.Off;
            var handle = go.AddComponent<PropHandle>();
            handle.Key = prop.Key; handle.Module = prop.Module.Name; handle.Added = prop.Added;
            Register(field, handle);
            go.transform.hasChanged = false;
            return handle;
        }

        static void Register(BattlefieldProps field, PropHandle handle)
        {
            handle.Registered = true;
            live[handle.Key] = new Live { Handle = handle, Module = handle.Module, Added = handle.Added, Born = EditorApplication.timeSinceStartup };
            field.Hold(handle.Key);
        }

        static void Place(Transform t, Matrix4x4 m)
        {
            t.SetPositionAndRotation(m.GetPosition(), m.rotation);
            t.localScale = m.lossyScale;
        }

        static void Adopt(BattlefieldProps field, PropHandle handle)
        {
            var layout = Layout(field);
            var edit = layout.Find(handle.Key);
            bool undone = edit != null && edit.Removed && !live.ContainsKey(handle.Key);
            if (!undone) { handle.Key = handle.Module + "+" + layout.NextId++; handle.Added = true; }   // a copy: a new prop of the same kind
            Register(field, handle);
            Commit(field, handle);
        }

        static void Commit(BattlefieldProps field, PropHandle handle)
        {
            var layout = Layout(field);
            var edit = layout.Get(handle.Key, handle.Module);
            var t = handle.transform; var p = t.position;
            edit.Added = handle.Added; edit.Removed = false;
            edit.Position = new Vector3(p.x, p.y - field.Ground(p.x, p.z), p.z);
            edit.Rotation = t.rotation;
            edit.Scale = t.lossyScale / layout.SizeOf(handle.Module);
            Dirty(layout);
        }

        static void Release(BattlefieldProps field, string key, Live entry)
        {
            live.Remove(key);
            if (field != null) field.Release(key, field.ModuleNamed(entry.Module), entry.Handle.transform.localToWorldMatrix, entry.Added);
            Object.Destroy(entry.Handle.gameObject);
        }

        static void Removed(BattlefieldProps field, string key, Live entry)
        {
            var layout = Layout(field);
            var edit = layout.Get(key, entry.Module);
            edit.Added = entry.Added; edit.Removed = true;
            Dirty(layout);
            field.Release(key, null, null, entry.Added);
        }

        // ---- window and inspector actions ----------------------------------------------------------------------------

        public static bool IsEdited(PropHandle handle)
        {
            var field = Props();
            return field != null && field.Layout != null && field.Layout.Find(handle.Key) != null;
        }

        public static void ResetToGenerated(PropHandle handle)
        {
            var field = Props();
            if (field == null || handle.Added) return;
            if (field.Layout != null) { field.Layout.Forget(handle.Key); Dirty(field.Layout); }
            if (field.TryGetPlaced(handle.Key, out var prop)) Place(handle.transform, prop.Generated);
            handle.transform.hasChanged = false;
        }

        public static void Remove(PropHandle handle) => Undo.DestroyObjectImmediate(handle.gameObject);

        public static void Add(string module)
        {
            var field = Props(); var kind = field != null ? field.ModuleNamed(module) : null;
            var view = SceneView.lastActiveSceneView;
            if (kind == null || view == null) return;
            var ray = view.camera.ViewportPointToRay(new Vector3(.5f, .5f, 0f));
            float t = GroundDistance(field, ray);
            var at = float.IsInfinity(t) ? view.pivot : ray.GetPoint(t);
            at.y = field.Ground(at.x, at.z);
            var layout = Layout(field);
            var facing = Quaternion.Euler(0f, view.camera.transform.eulerAngles.y + 180f, 0f);   // its front towards the viewer
            var handle = Spawn(field, new BattlefieldProps.Placed { Module = kind, Key = module + "+" + layout.NextId++, Added = true, Matrix = Matrix4x4.TRS(at, facing, layout.LookOf(module)?.Baseline ?? Vector3.one) });   // at its kind's baseline
            Commit(field, handle);
            Selection.activeGameObject = handle.gameObject;
        }

        public static void SetSize(string module, float size)
        {
            var field = Props();
            if (field == null) return;
            var layout = Layout(field);
            float old = layout.SizeOf(module);
            if (Mathf.Approximately(old, size)) return;
            layout.SetSize(module, size); Dirty(layout);
            field.Restyle();
            foreach (var entry in live.Values)
                if (entry.Module == module && entry.Handle != null) { var t = entry.Handle.transform; t.localScale *= size / old; t.hasChanged = false; }
        }

        public static void ClearAll()
        {
            var field = Props();
            if (field == null || field.Layout == null) return;
            foreach (var kv in live.ToArray()) { live.Remove(kv.Key); field.Release(kv.Key, null, null, kv.Value.Added); if (kv.Value.Handle != null) Object.Destroy(kv.Value.Handle.gameObject); }
            field.Layout.Clear(); Dirty(field.Layout);
            field.Restyle();
        }

        /// <summary>A look changed in the window: redraw the battlefield in it.</summary>
        public static void LookChanged(PropLayout layout)
        {
            Dirty(layout);
            Props()?.Restyle();
        }

        // kinds laid on a heading (an aim, a door, a face to the field) stray a little from it; the scatter is turned at random already
        static readonly Dictionary<string, float> headingRange = new Dictionary<string, float>
        {
            { "Siege/MGNest", 6f }, { "Weapons/FieldGun", 6f }, { "Weapons/TankTurret", 10f }, { "Siege/ArmouredStand", 12f },
            { "Siege/SodShelterRuin", 10f }, { "Siege/Pillbox", 10f }, { "Siege/Well", 12f }, { "Weapons/WreckedLimber", 12f },
        };

        /// <summary>
        /// Makes each kind's look from the hand edits (the owner's way of setting them, 2026-09-22): the scale the edited props
        /// were given becomes the kind's baseline, with a range as wide as they differ (10 to 18 %); the depth they were sunk
        /// to, per metre of height, becomes its sink; the lean given to generated props becomes its lean, tipped either way;
        /// and a kind laid on a heading strays a few degrees from it. Kinds without edits keep their look.
        /// </summary>
        public static string LearnLooks()
        {
            var field = Props();
            if (field == null) return "Enter Play first.";
            var layout = Layout(field);
            var report = new System.Text.StringBuilder();
            foreach (var group in layout.Edits.Where(e => !e.Removed).GroupBy(e => e.Module))
            {
                var scales = new List<Vector3>(); var sinks = new List<float>(); var pitches = new List<float>(); var rolls = new List<float>();
                foreach (var edit in group)
                {
                    BattlefieldProps.Placed prop = default;
                    bool generated = !edit.Added && field.TryGetPlaced(edit.Key, out prop);
                    var before = generated ? prop.Generated.lossyScale : Vector3.one;
                    var ratio = new Vector3(edit.Scale.x / before.x, edit.Scale.y / before.y, edit.Scale.z / before.z);
                    if (edit.Added || Mathf.Abs(ratio.x - 1f) > .05f || Mathf.Abs(ratio.y - 1f) > .05f || Mathf.Abs(ratio.z - 1f) > .05f)
                    {
                        scales.Add(edit.Scale);
                        if (edit.Position.y < 0f) sinks.Add(-edit.Position.y / edit.Scale.y);
                    }
                    if (generated)
                    {
                        Vector3 a = prop.Generated.rotation.eulerAngles, b = edit.Rotation.eulerAngles;
                        pitches.Add(Mathf.Abs(Mathf.DeltaAngle(a.x, b.x))); rolls.Add(Mathf.Abs(Mathf.DeltaAngle(a.z, b.z)));
                    }
                }
                var look = layout.GetLook(group.Key);
                var learned = new List<string>();
                if (scales.Count > 0)
                {
                    Vector3 mean = Vector3.zero;
                    foreach (var v in scales) mean += new Vector3(Mathf.Log(v.x), Mathf.Log(v.y), Mathf.Log(v.z));
                    mean /= scales.Count;
                    look.Scale = new Vector3(Mathf.Exp(mean.x), Mathf.Exp(mean.y), Mathf.Exp(mean.z));
                    float spread = 0f;
                    foreach (var v in scales) spread = Mathf.Max(spread, Mathf.Abs(v.x / look.Scale.x - 1f), Mathf.Abs(v.y / look.Scale.y - 1f), Mathf.Abs(v.z / look.Scale.z - 1f));
                    look.ScaleRange = Mathf.Clamp(spread, .10f, .18f);
                    look.Sink = sinks.Count > 0 ? sinks.Average() * look.Scale.y : -1f;
                    learned.Add($"scale {look.Scale.x:0.##}x{look.Scale.y:0.##}x{look.Scale.z:0.##} ±{look.ScaleRange * 100f:0}%" + (look.Sink >= 0f ? $", sunk {look.Sink:0.##} m" : ""));
                }
                if (pitches.Count > 0 && (pitches.Average() > 5f || rolls.Average() > 5f))
                {
                    look.Lean = new Vector2(pitches.Average() > 5f ? pitches.Average() : 0f, rolls.Average() > 5f ? rolls.Average() : 0f);
                    look.LeanRange = .25f;
                    learned.Add($"lean {look.Lean.x:0}° pitch {look.Lean.y:0}° roll ±25%");
                }
                if (learned.Count > 0 && headingRange.TryGetValue(group.Key, out float range)) { look.YawRange = range; learned.Add($"turned ±{range:0}°"); }
                if (learned.Count == 0) { if (look.Scale == Vector3.zero && look.Lean == Vector2.zero && Mathf.Approximately(look.Size, 1f)) layout.Looks.Remove(look); continue; }
                report.AppendLine(group.Key + ": " + string.Join(", ", learned));
            }
            Dirty(layout);
            field.Restyle();
            return report.ToString();
        }

        public static void ViewFromGameCamera()
        {
            var view = SceneView.lastActiveSceneView; var cam = Camera.main;
            if (view == null || cam == null) return;
            view.AlignViewToObject(cam.transform);
            view.Focus();
        }
    }

    public sealed class EnvPropWindow : EditorWindow
    {
        [MenuItem("TW/Env Props")]
        public static void Open() => GetWindow<EnvPropWindow>("Env Props");

        Vector2 scroll;
        int addIndex;
        bool showLooks = true;
        readonly HashSet<string> open = new HashSet<string>();

        void OnInspectorUpdate() => Repaint();

        void OnGUI()
        {
            bool on = EditorGUILayout.ToggleLeft("Pick and edit props in the Scene view", EnvPropEditing.Enabled);
            if (on != EnvPropEditing.Enabled) EnvPropEditing.Enabled = on;
            EditorGUILayout.HelpBox("In Play, click a prop in the Scene view (not the Game view) to select it. Move, rotate and scale with W, E, R; " +
                "Delete removes it, Ctrl+D copies it. Changes save to Resources/Layouts and apply every time the battlefield is built. " +
                "Moving a stump, log or wire obstacle moves only its look: the cover and blocking stay where the map has them.", MessageType.Info);

            var field = EnvPropEditing.Props();
            if (field == null)
            {
                EditorGUILayout.HelpBox("The battlefield is built when the game starts: enter Play mode.", MessageType.None);
                if (!EditorApplication.isPlaying && GUILayout.Button("Play")) EditorApplication.isPlaying = true;
                return;
            }
            if (GUILayout.Button("Scene view: look from the game camera")) EnvPropEditing.ViewFromGameCamera();

            var layout = field.Layout;
            int moved = 0, removed = 0, added = 0;
            if (layout != null) foreach (var e in layout.Edits) { if (e.Removed) removed++; else if (e.Added) added++; else moved++; }
            EditorGUILayout.Space();
            EditorGUILayout.LabelField(layout == null ? "No edits yet" : $"{moved} moved, {added} added, {removed} removed  ({AssetDatabase.GetAssetPath(layout)})", EditorStyles.miniLabel);
            using (new EditorGUILayout.HorizontalScope())
            {
                using (new EditorGUI.DisabledScope(layout == null || layout.Edits.Count == 0))
                    if (GUILayout.Button("Learn looks from my edits") && EditorUtility.DisplayDialog("Learn looks from edits",
                        "Each kind you resized or tilted takes your edits as its baseline size, sink and lean, with a range every prop of that kind varies by. Your edited props stay as they are.", "Learn", "Cancel"))
                        Debug.Log("Env props, looks learned:\n" + EnvPropEditing.LearnLooks());
                using (new EditorGUI.DisabledScope(layout == null || layout.Edits.Count == 0 && layout.Looks.Count == 0))
                    if (GUILayout.Button("Clear all edits") && EditorUtility.DisplayDialog("Clear all prop edits", "Put every prop back where the composer puts it, and every kind back to its own look?", "Clear", "Cancel"))
                        EnvPropEditing.ClearAll();
            }

            var counts = new SortedDictionary<string, int>();
            foreach (var module in field.Kit.Modules) if (module.Name != null) counts[module.Name] = 0;
            foreach (var prop in field.Editable) if (counts.ContainsKey(prop.Module.Name)) counts[prop.Module.Name]++;
            var names = counts.Keys.ToArray();
            if (names.Length == 0) return;

            EditorGUILayout.Space();
            EditorGUILayout.LabelField("Add a prop", EditorStyles.boldLabel);
            using (new EditorGUILayout.HorizontalScope())
            {
                addIndex = EditorGUILayout.Popup(Mathf.Clamp(addIndex, 0, names.Length - 1), names);
                if (GUILayout.Button("Add at Scene view centre", GUILayout.Width(170f))) EnvPropEditing.Add(names[addIndex]);
            }

            EditorGUILayout.Space();
            showLooks = EditorGUILayout.Foldout(showLooks, "Look of every prop of a kind", true);
            if (!showLooks) return;
            scroll = EditorGUILayout.BeginScrollView(scroll);
            foreach (var kv in counts)
            {
                var look = layout != null ? layout.LookOf(kv.Key) : null;
                bool unfolded = EditorGUILayout.Foldout(open.Contains(kv.Key), $"{kv.Key}  ({kv.Value})" + (look != null ? "  *" : ""), true);
                if (unfolded) open.Add(kv.Key); else open.Remove(kv.Key);
                if (!unfolded) continue;
                using (new EditorGUI.IndentLevelScope())
                {
                    if (look == null)
                    {
                        if (GUILayout.Button("Give this kind a look")) { look = EnvPropEditing.Layout(field).GetLook(kv.Key); EnvPropEditing.LookChanged(field.Layout); }
                        continue;
                    }
                    EditorGUI.BeginChangeCheck();
                    look.Size = EditorGUILayout.Slider(new GUIContent("Size", "Multiplies every prop of the kind, hand-edited ones too"), look.Size, .25f, 3f);
                    look.Scale = EditorGUILayout.Vector3Field(new GUIContent("Baseline scale", "Zero keeps the composer's own"), look.Scale);
                    look.ScaleRange = EditorGUILayout.Slider(new GUIContent("  ± size", "Each prop this fraction bigger or smaller"), look.ScaleRange, 0f, .5f);
                    look.Yaw = EditorGUILayout.Slider(new GUIContent("Turn", "Degrees added to the composer's heading"), look.Yaw, -180f, 180f);
                    look.YawRange = EditorGUILayout.Slider(new GUIContent("  ± turn", "Degrees either way per prop"), look.YawRange, 0f, 180f);
                    look.Lean = EditorGUILayout.Vector2Field(new GUIContent("Lean (pitch, roll)", "Degrees, tipped either way per prop"), look.Lean);
                    look.LeanRange = EditorGUILayout.Slider(new GUIContent("  ± lean", "Each prop leans this fraction more or less"), look.LeanRange, 0f, 1f);
                    bool sunk = EditorGUILayout.Toggle(new GUIContent("Own sink", "Off keeps the composer's height"), look.Sink >= 0f);
                    look.Sink = sunk ? Mathf.Max(0f, EditorGUILayout.FloatField(new GUIContent("  metres", "Below the ground at the baseline scale"), Mathf.Max(0f, look.Sink))) : -1f;
                    if (EditorGUI.EndChangeCheck()) EnvPropEditing.LookChanged(layout);
                    if (GUILayout.Button("Remove this look")) { layout.Looks.Remove(look); EnvPropEditing.LookChanged(layout); }
                }
            }
            EditorGUILayout.EndScrollView();
        }
    }

    [CustomEditor(typeof(PropHandle))]
    sealed class PropHandleInspector : UnityEditor.Editor
    {
        public override void OnInspectorGUI()
        {
            var handle = (PropHandle)target;
            EditorGUILayout.LabelField(handle.Module, EditorStyles.boldLabel);
            EditorGUILayout.LabelField(handle.Added ? "Added by hand" : EnvPropEditing.IsEdited(handle) ? "Moved by hand" : "Where the composer put it", EditorStyles.miniLabel);
            var field = EnvPropEditing.Props();
            if (field != null)
            {
                var layout = field.Layout;
                float size = layout != null ? layout.SizeOf(handle.Module) : 1f;
                float next = EditorGUILayout.Slider("Size of the kind", size, .25f, 3f);
                if (!Mathf.Approximately(next, size)) EnvPropEditing.SetSize(handle.Module, next);
            }
            using (new EditorGUILayout.HorizontalScope())
            {
                using (new EditorGUI.DisabledScope(handle.Added || !EnvPropEditing.IsEdited(handle)))
                    if (GUILayout.Button("Reset to generated")) EnvPropEditing.ResetToGenerated(handle);
                if (GUILayout.Button("Remove")) { EnvPropEditing.Remove(handle); GUIUtility.ExitGUI(); }
            }
        }
    }
}
