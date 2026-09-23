// Phase: B6 (implemented) — puts the Toolkit HUD into any scene that has a SimHost and no HUD yet.
// The committed GreyboxCorridor carries the HUD object, but SimHost.Restart() reloads the scene, fresh clones may
// rebuild it, and a test scene has neither; this runs after every scene load and adds the UIDocument + HudController
// when the flag is on. The same "add if missing" habit DebugOverlay.Start has for the IMGUI pieces.
using UnityEngine;
using UnityEngine.SceneManagement;
using UnityEngine.UIElements;
using TW.Presentation;

namespace TW.UI
{
    public static class HudBootstrap
    {
        public const string PanelResource = "UI/DustFrontPanel";
        public const string HudResource = "Hud/BattleHud";
        /// <summary>Tests set this to keep the bootstrap out of their scenes.</summary>
        public static bool Disabled;

        [RuntimeInitializeOnLoadMethod(RuntimeInitializeLoadType.AfterSceneLoad)]
        static void Init()
        {
            SceneManager.sceneLoaded -= OnSceneLoaded;
            SceneManager.sceneLoaded += OnSceneLoaded;
            Ensure();
        }

        static void OnSceneLoaded(Scene s, LoadSceneMode m) => Ensure();

        /// <summary>The HUD object for the scene's SimHost, created if none exists. Null when there is no host.</summary>
        public static HudController Ensure()
        {
            if (Disabled || !Application.isPlaying) return null;
            var existing = Object.FindFirstObjectByType<HudController>(FindObjectsInactive.Include);
            if (existing != null) return existing;
            // created whether or not the flag is on: with the flag off it hides itself and only listens for F9
            var host = Object.FindFirstObjectByType<SimHost>();
            if (host == null) return null;
            return Create(host);
        }

        public static HudController Create(SimHost host)
        {
            var panel = Resources.Load<PanelSettings>(PanelResource);
            var tree = Resources.Load<VisualTreeAsset>(HudResource);
            if (panel == null || tree == null)
            {
                Debug.LogWarning($"HudBootstrap: missing {(panel == null ? PanelResource : HudResource)} in Resources; run TW/UI/Build All UI Assets.");
                return null;
            }
            var go = new GameObject("HUD");
            var doc = go.AddComponent<UIDocument>();
            doc.panelSettings = panel;
            doc.visualTreeAsset = tree;
            doc.sortingOrder = 0;
            var hud = go.AddComponent<HudController>();
            hud.Host = host;
            return hud;
        }
    }
}
