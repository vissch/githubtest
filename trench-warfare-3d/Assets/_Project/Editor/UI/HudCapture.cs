// Phase: B6 (implemented) — a still that contains the HUD, which no capture in this project had before.
// ScreenCapture returns an empty sky when the editor is driven headlessly, and the IMGUI HUD never appeared in any
// still, so every composition judgement on this game was made on a frame with no interface in it. A UI Toolkit panel
// can render into a RenderTexture (PanelSettings.targetTexture), so this renders the main camera into one texture,
// the HUD panel into another at the same size, composites the two on the CPU and writes a PNG. Runs in Play mode
// from the menu or from `unity command eval` (TW.Editor.HudCapture.Shoot(path)); the file appears a frame later.
// Beside the PNG it writes <png>.json: the capture-pixel rectangle of every named element in Probes that is laid out
// on any document sharing the panel, measured while the panel is on the capture target, so a critic can crop and zoom
// on exactly one component rather than guessing where it is. The first match per name in document draw order wins,
// skipping anything inside a display:none branch; the JSON key order is Probes order, not insertion order.
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.IO;
using UnityEditor;
using UnityEngine;
using UnityEngine.UIElements;
using TW.UI;

namespace TW.Editor
{
    public static class HudCapture
    {
        public const string DefaultPath = "Captures/hud.png";

        /// <summary>Elements whose rectangles go into the sidecar (the first match per name in document draw
        /// order, skipping anything inside a display:none branch; the JSON key order is Probes order).</summary>
        public static readonly string[] Probes =
        {
            "gauges", "gauge-silver", "gauge-men", "gauge-time", "objectives", "minimap-bezel", "speed-bar", "bar",
            "group-infantry", "group-armour", "group-support", "card", "film", "filmline", "tooltip", "hint", "banner", "pause-plate",
            "settings-plate", "tabs", "debrief-plate", "stats-table", "title", "menu-stack", "mission-list", "info-panel",
        };

        // ---- the pointer in the picture --------------------------------------------------------------------------
        // A capture renders the panel and the camera; the operating system's cursor is in neither, so a shot of a
        // hovered card could never show what the pointer is doing. This paints an arrow into the panel itself at a
        // capture pixel, with Painter2D (no sprite), so "rest the pointer on a card" can be seen in the still.
        static VisualElement cursor;

        /// <summary>Draw the pointer arrow at a capture pixel (top-left origin, in a shot <paramref name="width"/>
        /// by <paramref name="height"/>), or take it away with a negative x. Call before Shoot.</summary>
        public static void Cursor(float x, float y, int width = 1920, int height = 1080)
        {
            var hud = Object.FindFirstObjectByType<HudController>(FindObjectsInactive.Include);
            var doc = hud != null ? hud.GetComponent<UIDocument>() : null;
            var root = doc != null ? doc.rootVisualElement : null;
            if (root == null) { Debug.LogWarning("HudCapture.Cursor: no HUD document."); return; }
            if (x < 0f) { cursor?.RemoveFromHierarchy(); cursor = null; return; }
            if (cursor == null || cursor.panel == null)
            {
                cursor = new VisualElement { name = "capture-cursor", pickingMode = PickingMode.Ignore };
                cursor.style.position = Position.Absolute;
                cursor.style.width = 26; cursor.style.height = 34;
                cursor.generateVisualContent += Arrow;
                root.Add(cursor);
            }
            cursor.BringToFront();
            // the HUD root is not at the panel's origin, and an absolute child is placed inside it: take the shot's
            // pixel into panel space, then back into the root's own box, or the arrow lands mid-screen.
            // the panel is not the shot's shape (1200 x 900 against 1920 x 1080), so x and y take their own scale,
            // exactly as Probe does when it writes the rectangles.
            var tree = root.panel.visualTree;
            float sx = tree.layout.width / Mathf.Max(1f, width), sy = tree.layout.height / Mathf.Max(1f, height);
            var box = root.worldBound;
            cursor.style.left = x * sx - box.x; cursor.style.top = y * sy - box.y;
            cursor.MarkDirtyRepaint();
        }

        /// <summary>The classic arrow: white, black-edged, its point on the element's top-left corner.</summary>
        static void Arrow(MeshGenerationContext ctx)
        {
            var p = ctx.painter2D;
            p.BeginPath();
            p.MoveTo(new Vector2(0f, 0f));
            p.LineTo(new Vector2(0f, 24f));
            p.LineTo(new Vector2(6.5f, 18f));
            p.LineTo(new Vector2(11f, 28f));
            p.LineTo(new Vector2(16f, 25.5f));
            p.LineTo(new Vector2(11.5f, 16f));
            p.LineTo(new Vector2(19f, 15f));
            p.ClosePath();
            p.fillColor = Color.white; p.Fill();
            p.strokeColor = new Color(0f, 0f, 0f, 0.9f); p.lineWidth = 2f; p.Stroke();
        }

        [MenuItem("TW/UI/Capture HUD (Play)")]
        public static void Menu() => Shoot(DefaultPath);

        /// <summary>Documents sorted so the one drawn on top (highest sortingOrder) comes first, ties broken by a stable scene/hierarchy key.</summary>
        public static List<UIDocument> InDrawOrder(IEnumerable<UIDocument> docs)
        {
            static string Key(UIDocument doc)
            {
                var parts = new List<string>();
                for (var t = doc.transform; t != null; t = t.parent)
                    parts.Insert(0, t.name + "[" + t.GetSiblingIndex() + "]");
                return doc.gameObject.scene.name + "/" + string.Join("/", parts);
            }
            return docs.Where(d => d != null)
                .OrderByDescending(d => d.sortingOrder)
                .ThenBy(Key, System.StringComparer.Ordinal)
                .ToList();
        }

        /// <summary>True when the element or any of its ancestors is display:none (resolved or inline).</summary>
        public static bool InHiddenBranch(VisualElement e)
        {
            for (var cur = e; cur != null; cur = cur.hierarchy.parent)
            {
                if (cur.resolvedStyle.display == DisplayStyle.None) return true;
                if (cur.style.display.value == DisplayStyle.None) return true;
            }
            return false;
        }

        /// <summary>JSON {"w","h","rects":{name:[x,y,w,h]}}, keys in Probes order first, then any other key ordinal-ascending.</summary>
        public static string RectsJson(int w, int h, IReadOnlyDictionary<string, Rect> rects)
        {
            var keys = new List<string>(Probes.Where(rects.ContainsKey));
            keys.AddRange(rects.Keys.Where(k => System.Array.IndexOf(Probes, k) < 0).OrderBy(k => k, System.StringComparer.Ordinal));
            var sb = new StringBuilder();
            sb.Append("{\"w\":").Append(w).Append(",\"h\":").Append(h).Append(",\"rects\":{");
            bool first = true;
            foreach (var key in keys)
            {
                if (!first) sb.Append(','); first = false;
                var r = rects[key];
                sb.Append('"').Append(key).Append("\":[")
                  .Append(r.x.ToString("0", CultureInfo.InvariantCulture)).Append(',').Append(r.y.ToString("0", CultureInfo.InvariantCulture)).Append(',')
                  .Append(r.width.ToString("0", CultureInfo.InvariantCulture)).Append(',').Append(r.height.ToString("0", CultureInfo.InvariantCulture)).Append(']');
            }
            return sb.Append("}}").ToString();
        }

        /// <summary>Queue a capture; returns the absolute path the PNG will be written to, or null with a reason logged.</summary>
        public static string Shoot(string path, int width = 1920, int height = 1080)
        {
            if (!Application.isPlaying) { Debug.LogWarning("HudCapture: enter Play first."); return null; }
            var hud = Object.FindFirstObjectByType<HudController>(FindObjectsInactive.Include);
            var doc = hud != null ? hud.GetComponent<UIDocument>() : null;
            if (doc == null || doc.panelSettings == null) { Debug.LogWarning("HudCapture: no HudController with a UIDocument in the scene."); return null; }
            var cam = Camera.main;
            if (cam == null) { Debug.LogWarning("HudCapture: no main camera."); return null; }
            if (Busy) { Debug.LogWarning("HudCapture: a capture is already running; wait for it."); return null; }
            string full = Arm(path);
            if (Application.isBatchMode)
            {
                // A batch editor renders and ticks the player loop, but WaitForEndOfFrame never fires in it, so the
                // coroutine below would wait for a frame end that never comes (Tools/assetboard/gamefilm.py draws from
                // EditorApplication.update for the same reason). Same steps, counted in editor ticks instead.
                var br = new GameObject("HudCapture") { hideFlags = HideFlags.HideAndDontSave }.AddComponent<Runner>();
                br.Batch(doc.panelSettings, cam, full, width, height);
                return full;
            }
            var runner = new GameObject("HudCapture") { hideFlags = HideFlags.HideAndDontSave }.AddComponent<Runner>();
            runner.StartCoroutine(runner.Run(doc.panelSettings, cam, full, width, height));
            return full;
        }

        /// <summary>True while a capture holds the HUD panel: a second one would leave the panel on a dead target.</summary>
        public static bool Busy => Runner.Live != null;

        /// <summary>Composites one already-premultiplied UI pixel over a scene pixel. The panel target holds
        /// premultiplied alpha, so the UI channel must not be multiplied by alpha a second time here.</summary>
        internal static Color32 Over(Color32 ui, Color32 scene)
        {
            float a = ui.a / 255f;
            return new Color32(
                (byte)Mathf.Min(255f, ui.r + scene.r * (1f - a)),
                (byte)Mathf.Min(255f, ui.g + scene.g * (1f - a)),
                (byte)Mathf.Min(255f, ui.b + scene.b * (1f - a)),
                255);
        }

        /// <summary>The absolute path of the PNG, with any old PNG and sidecar at it deleted, so the path Shoot
        /// answers can never be read as this capture before it is written.</summary>
        internal static string Arm(string path)
        {
            string full = Path.IsPathRooted(path) ? path : Path.GetFullPath(Path.Combine(Application.dataPath, "..", path));
            try
            {
                if (File.Exists(full)) File.Delete(full);
                if (File.Exists(full + ".json")) File.Delete(full + ".json");
            }
            catch (IOException e) { Debug.LogWarning($"HudCapture: could not clear the old capture at {full}: {e.Message}"); }
            return full;
        }

        // ExecuteAlways: the restore has to happen even when the runner dies outside Play (a domain reload, a test)
        [ExecuteAlways]
        public sealed class Runner : MonoBehaviour
        {
            /// <summary>The one capture that holds the panel, or null. Set in Begin, cleared in Restore.</summary>
            internal static Runner Live;

            PanelSettings held; RenderTexture heldUi, heldScene; RenderTexture oldTarget; bool oldClear; Color oldColor; bool restored;

            /// <summary>Takes the panel over and remembers what to put back; Restore undoes exactly this.</summary>
            public void Begin(PanelSettings p, RenderTexture rtUi, RenderTexture rtScene)
            {
                held = p; heldUi = rtUi; heldScene = rtScene; restored = false;
                oldTarget = p.targetTexture; oldClear = p.clearColor; oldColor = p.colorClearValue;
                p.targetTexture = rtUi; p.clearColor = true; p.colorClearValue = new Color(0f, 0f, 0f, 0f);
                Live = this;
            }

            /// <summary>Puts the shared panel asset back and releases the textures, once, however the run ended.</summary>
            public void Restore()
            {
                if (restored) return;
                restored = true;
                if (held != null) { held.targetTexture = oldTarget; held.clearColor = oldClear; held.colorClearValue = oldColor; }
                if (heldUi != null) { heldUi.Release(); Kill(heldUi); heldUi = null; }
                if (heldScene != null) { heldScene.Release(); Kill(heldScene); heldScene = null; }
                if (Live == this) Live = null;
            }

            /// <summary>Restore also runs out of Play (a test, a domain reload), where Destroy is not allowed.</summary>
            static void Kill(Object o) { if (Application.isPlaying) Destroy(o); else DestroyImmediate(o); }

            void OnDisable() => Restore();   // Play ending, a domain reload or any early exit comes through here
            void OnDestroy() => Restore();

            public IEnumerator Run(PanelSettings panel, Camera cam, string full, int w, int h)
            {
                // 24-bit depth brings an 8-bit stencil: UI Toolkit clips the children of a rounded, overflow-hidden element
                // (every Button and text field in the default theme) with a stencil mask, and on a target without one the
                // mask is drawn as a solid white shape over the control
                var rtUi = new RenderTexture(w, h, 24, RenderTextureFormat.ARGB32) { name = "HudCapture UI" };
                var rtScene = new RenderTexture(w, h, 24, RenderTextureFormat.ARGB32) { name = "HudCapture Scene" };
                Begin(panel, rtUi, rtScene);
                try
                {
                    yield return new WaitForEndOfFrame();     // the panel draws into rtUi during this frame
                    yield return new WaitForEndOfFrame();     // and once more after layout settled for the new size
                    Finish(panel, cam, rtUi, rtScene, full, w, h);
                }
                finally { Restore(); }   // a coroutine stopped between its yields runs this and nothing else
            }

            /// <summary>The capture in a batch editor: take the panel, give it two editor ticks to lay out and draw
            /// into the texture, then finish exactly as the coroutine does.</summary>
            public void Batch(PanelSettings panel, Camera cam, string full, int w, int h)
            {
                var rtUi = new RenderTexture(w, h, 24, RenderTextureFormat.ARGB32) { name = "HudCapture UI" };
                var rtScene = new RenderTexture(w, h, 24, RenderTextureFormat.ARGB32) { name = "HudCapture Scene" };
                Begin(panel, rtUi, rtScene);
                int tick = 0;
                EditorApplication.CallbackFunction step = null;
                step = () =>
                {
                    if (this == null) { EditorApplication.update -= step; return; }   // the runner died; Restore already ran
                    if (++tick < 3) return;   // one tick to lay the panel out at the new size, one to draw it
                    EditorApplication.update -= step;
                    Finish(panel, cam, rtUi, rtScene, full, w, h);
                };
                EditorApplication.update += step;
            }

            /// <summary>Probe the rectangles, render the field, composite the panel over it, write both files, then
            /// hand the panel back and remove the runner.</summary>
            void Finish(PanelSettings panel, Camera cam, RenderTexture rtUi, RenderTexture rtScene, string full, int w, int h)
            {
                string rects = Probe(panel, w, h);
                var camTarget = cam.targetTexture;
                cam.targetTexture = rtScene; cam.Render(); cam.targetTexture = camTarget;
                var scene = Read(rtScene); var uiTex = Read(rtUi);
                Restore();
                var px = scene.GetPixels32(); var upx = uiTex.GetPixels32();
                for (int i = 0; i < px.Length; i++)
                {
                    var u = upx[i]; if (u.a == 0) continue;
                    px[i] = Over(u, px[i]);
                }
                scene.SetPixels32(px); scene.Apply(false, false);
                // what share of the shot is blown out, so a fire-lit frame can be cleared (or not) on its numbers
                long blown = 0;
                for (int i = 0; i < px.Length; i++) if (px[i].r > 250 && px[i].g > 250 && px[i].b > 250) blown++;
                rects = rects.Substring(0, rects.Length - 1) + ",\"blown_frac\":" +
                        ((double)blown / px.Length).ToString("0.00000", CultureInfo.InvariantCulture) + Film() + "}";
                Directory.CreateDirectory(Path.GetDirectoryName(full));
                File.WriteAllBytes(full, scene.EncodeToPNG());
                File.WriteAllText(full + ".json", rects);
                Debug.Log($"HudCapture: wrote {full} ({w}x{h})");
                Kill(scene); Kill(uiTex);
                Kill(gameObject);
            }

            /// <summary>JSON {"w","h","rects":{name:[x,y,w,h]}} in capture pixels, top-left origin; the first
            /// visible trench order cluster is added as "orders".</summary>
            static string Probe(PanelSettings panel, int w, int h)
            {
                var found = new Dictionary<string, Rect>();
                foreach (var doc in InDrawOrder(Object.FindObjectsByType<UIDocument>(FindObjectsSortMode.None)))
                {
                    if (doc.panelSettings != panel || doc.rootVisualElement == null || doc.rootVisualElement.panel == null) continue;
                    var top = doc.rootVisualElement.panel.visualTree;
                    float sx = w / Mathf.Max(1f, top.layout.width), sy = h / Mathf.Max(1f, top.layout.height);
                    void Add(string key, VisualElement e)
                    {
                        if (e == null || found.ContainsKey(key) || InHiddenBranch(e)) return;
                        var b = e.worldBound; if (b.width <= 0f || b.height <= 0f || float.IsNaN(b.x)) return;
                        found[key] = new Rect(b.x * sx, b.y * sy, b.width * sx, b.height * sy);
                    }
                    foreach (var n in Probes) Add(n, doc.rootVisualElement.Q(n));
                    doc.rootVisualElement.Query(className: "hud-orders").ForEach(e => { if (!e.ClassListContains("is-hidden")) Add("orders", e); });
                }
                return RectsJson(w, h, found);
            }

            /// <summary>The film the HUD is playing right now, for the sidecar: its length and loop by construction,
            /// and the frame, the second and the progress this shot actually caught (nothing when no card plays).</summary>
            static string Film()
            {
                var hud = Object.FindFirstObjectByType<HudController>(FindObjectsInactive.Include);
                var player = hud != null ? hud.FilmPlayer : null;
                if (player == null || player.Hovered == null) return ",\"film_playing\":false";
                string Num(float v) => v.ToString("0.000", CultureInfo.InvariantCulture);
                return ",\"film_playing\":" + (player.LastFrame >= 0 ? "true" : "false") +
                       ",\"film_unit\":\"" + player.Hovered.FilmName + "\"" +
                       ",\"film_seconds\":" + Num(CardFilm.Seconds) +
                       ",\"film_fps\":" + Num(CardFilm.Fps) +
                       ",\"film_frames\":" + CardFilm.Frames +
                       ",\"film_loop_at\":" + Num(CardFilm.Seconds) +
                       ",\"film_held\":" + Num(player.Held) +
                       ",\"film_frame\":" + player.LastFrame +
                       ",\"film_progress\":" + Num(player.LastProgress);
            }

            static Texture2D Read(RenderTexture rt)
            {
                var prev = RenderTexture.active;
                RenderTexture.active = rt;
                var t = new Texture2D(rt.width, rt.height, TextureFormat.RGBA32, false);
                t.ReadPixels(new Rect(0, 0, rt.width, rt.height), 0, 0); t.Apply(false, false);
                RenderTexture.active = prev;
                return t;
            }
        }
    }
}
