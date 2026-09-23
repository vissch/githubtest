// Phase: B6 (implemented) — a still that contains the HUD, which no capture in this project had before.
// ScreenCapture returns an empty sky when the editor is driven headlessly, and the IMGUI HUD never appeared in any
// still, so every composition judgement on this game was made on a frame with no interface in it. A UI Toolkit panel
// can render into a RenderTexture (PanelSettings.targetTexture), so this renders the main camera into one texture,
// the HUD panel into another at the same size, composites the two on the CPU and writes a PNG. Runs in Play mode
// from the menu or from `unity command eval` (TW.Editor.HudCapture.Shoot(path)); the file appears a frame later.
using System.Collections;
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
                var rtUi = new RenderTexture(w, h, 0, RenderTextureFormat.ARGB32) { name = "HudCapture UI" };
                var rtScene = new RenderTexture(w, h, 24, RenderTextureFormat.ARGB32) { name = "HudCapture Scene" };
                var oldTarget = panel.targetTexture; bool oldClear = panel.clearColor; var oldColor = panel.colorClearValue;
                panel.targetTexture = rtUi; panel.clearColor = true; panel.colorClearValue = new Color(0f, 0f, 0f, 0f);
                yield return new WaitForEndOfFrame();     // the panel draws into rtUi during this frame
                yield return new WaitForEndOfFrame();     // and once more after layout settled for the new size
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
                Debug.Log($"HudCapture: wrote {full} ({w}x{h})");
                Destroy(scene); Destroy(ui); rtUi.Release(); rtScene.Release(); Destroy(rtUi); Destroy(rtScene);
                Destroy(gameObject);
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
