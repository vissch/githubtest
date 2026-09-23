// Phase: B6 (implemented) — a still that contains the HUD, which no capture in this project had before.
// ScreenCapture returns an empty sky when the editor is driven headlessly, and the IMGUI HUD never appeared in any
// still, so every composition judgement on this game was made on a frame with no interface in it. A UI Toolkit panel
// can render into a RenderTexture (PanelSettings.targetTexture), so this renders the main camera into one texture,
// the HUD panel into another at the same size, composites the two on the CPU and writes a PNG. Runs in Play mode
// from the menu or from `unity command eval` (TW.Editor.HudCapture.Shoot(path)); the file appears a frame later.
// Beside the PNG it writes <png>.json: the capture-pixel rectangle of every named element in Probes that is laid out
// on any document sharing the panel, measured while the panel is on the capture target, so a critic can crop and zoom
// on exactly one component rather than guessing where it is.
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
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

        /// <summary>Elements whose rectangles go into the sidecar (the first match per name per document).</summary>
        public static readonly string[] Probes =
        {
            "gauges", "gauge-silver", "gauge-men", "gauge-time", "objectives", "minimap-bezel", "speed-bar", "bar",
            "group-infantry", "group-armour", "group-support", "card", "tooltip", "banner", "pause-plate",
            "settings-plate", "tabs", "debrief-plate", "stats-table", "title", "menu-stack", "mission-list", "info-panel",
        };

        [MenuItem("TW/UI/Capture HUD (Play)")]
        public static void Menu() => Shoot(DefaultPath);

        /// <summary>Queue a capture; returns the absolute path the PNG will be written to, or null with a reason logged.</summary>
        public static string Shoot(string path, int width = 1920, int height = 1080)
        {
            if (!Application.isPlaying) { Debug.LogWarning("HudCapture: enter Play first."); return null; }
            var hud = Object.FindFirstObjectByType<HudController>(FindObjectsInactive.Include);
            var doc = hud != null ? hud.GetComponent<UIDocument>() : null;
            if (doc == null || doc.panelSettings == null) { Debug.LogWarning("HudCapture: no HudController with a UIDocument in the scene."); return null; }
            var cam = Camera.main;
            if (cam == null) { Debug.LogWarning("HudCapture: no main camera."); return null; }
            string full = Path.IsPathRooted(path) ? path : Path.GetFullPath(Path.Combine(Application.dataPath, "..", path));
            var runner = new GameObject("HudCapture") { hideFlags = HideFlags.HideAndDontSave }.AddComponent<Runner>();
            runner.StartCoroutine(runner.Run(doc.panelSettings, cam, full, width, height));
            return full;
        }

        sealed class Runner : MonoBehaviour
        {
            public IEnumerator Run(PanelSettings panel, Camera cam, string full, int w, int h)
            {
                // 24-bit depth brings an 8-bit stencil: UI Toolkit clips the children of a rounded, overflow-hidden element
                // (every Button and text field in the default theme) with a stencil mask, and on a target without one the
                // mask is drawn as a solid white shape over the control
                var rtUi = new RenderTexture(w, h, 24, RenderTextureFormat.ARGB32) { name = "HudCapture UI" };
                var rtScene = new RenderTexture(w, h, 24, RenderTextureFormat.ARGB32) { name = "HudCapture Scene" };
                var oldTarget = panel.targetTexture; bool oldClear = panel.clearColor; var oldColor = panel.colorClearValue;
                panel.targetTexture = rtUi; panel.clearColor = true; panel.colorClearValue = new Color(0f, 0f, 0f, 0f);
                yield return new WaitForEndOfFrame();     // the panel draws into rtUi during this frame
                yield return new WaitForEndOfFrame();     // and once more after layout settled for the new size
                string rects = Probe(panel, w, h);
                var camTarget = cam.targetTexture;
                cam.targetTexture = rtScene; cam.Render(); cam.targetTexture = camTarget;
                var scene = Read(rtScene); var ui = Read(rtUi);
                panel.targetTexture = oldTarget; panel.clearColor = oldClear; panel.colorClearValue = oldColor;
                var px = scene.GetPixels32(); var upx = ui.GetPixels32();
                for (int i = 0; i < px.Length; i++)
                {
                    var u = upx[i]; if (u.a == 0) continue;
                    float a = u.a / 255f; var s = px[i];
                    px[i] = new Color32((byte)(u.r * a + s.r * (1f - a)), (byte)(u.g * a + s.g * (1f - a)), (byte)(u.b * a + s.b * (1f - a)), 255);
                }
                scene.SetPixels32(px); scene.Apply(false, false);
                Directory.CreateDirectory(Path.GetDirectoryName(full));
                File.WriteAllBytes(full, scene.EncodeToPNG());
                File.WriteAllText(full + ".json", rects);
                Debug.Log($"HudCapture: wrote {full} ({w}x{h})");
                Destroy(scene); Destroy(ui); rtUi.Release(); rtScene.Release(); Destroy(rtUi); Destroy(rtScene);
                Destroy(gameObject);
            }

            /// <summary>JSON {"w","h","rects":{name:[x,y,w,h]}} in capture pixels, top-left origin; the first
            /// visible trench order cluster is added as "orders".</summary>
            static string Probe(PanelSettings panel, int w, int h)
            {
                var found = new Dictionary<string, Rect>();
                foreach (var doc in Object.FindObjectsByType<UIDocument>(FindObjectsSortMode.None))
                {
                    if (doc.panelSettings != panel || doc.rootVisualElement == null || doc.rootVisualElement.panel == null) continue;
                    var top = doc.rootVisualElement.panel.visualTree;
                    float sx = w / Mathf.Max(1f, top.layout.width), sy = h / Mathf.Max(1f, top.layout.height);
                    void Add(string key, VisualElement e)
                    {
                        if (e == null || found.ContainsKey(key) || e.resolvedStyle.display == DisplayStyle.None) return;
                        var b = e.worldBound; if (b.width <= 0f || b.height <= 0f || float.IsNaN(b.x)) return;
                        found[key] = new Rect(b.x * sx, b.y * sy, b.width * sx, b.height * sy);
                    }
                    foreach (var n in Probes) Add(n, doc.rootVisualElement.Q(n));
                    doc.rootVisualElement.Query(className: "hud-orders").ForEach(e => { if (!e.ClassListContains("is-hidden")) Add("orders", e); });
                }
                var sb = new StringBuilder();
                sb.Append("{\"w\":").Append(w).Append(",\"h\":").Append(h).Append(",\"rects\":{");
                bool first = true;
                foreach (var kv in found)
                {
                    if (!first) sb.Append(','); first = false;
                    var r = kv.Value;
                    sb.Append('"').Append(kv.Key).Append("\":[")
                      .Append(r.x.ToString("0", CultureInfo.InvariantCulture)).Append(',').Append(r.y.ToString("0", CultureInfo.InvariantCulture)).Append(',')
                      .Append(r.width.ToString("0", CultureInfo.InvariantCulture)).Append(',').Append(r.height.ToString("0", CultureInfo.InvariantCulture)).Append(']');
                }
                return sb.Append("}}").ToString();
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
