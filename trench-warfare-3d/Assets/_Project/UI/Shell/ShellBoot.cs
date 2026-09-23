// Phase: B6 (implemented) — puts the one ShellRoot into the game and keeps it through every scene load.
// A DontDestroyOnLoad object survives SimHost.Restart()'s single-mode LoadScene, so the debug restart presets keep
// working untouched and pressing Play straight in GreyboxCorridor still gives a playable game (the shell boots into
// its in-match state). Loads and applies settings.json first. Skipped for test scenes and when a test disables it.
using UnityEngine;
using UnityEngine.SceneManagement;
using UnityEngine.UIElements;
using TW.Presentation;

namespace TW.UI
{
    public static class ShellBoot
    {
        public static bool Disabled;
        public const string RootName = "ShellRoot";
        static bool settingsApplied;

        [RuntimeInitializeOnLoadMethod(RuntimeInitializeLoadType.AfterSceneLoad)]
        static void Init()
        {
            if (Disabled) return;
            string scene = SceneManager.GetActiveScene().name;
            if (scene != MatchLaunch.BattleScene && scene != MatchLaunch.MenuScene && scene != "Bootstrap") return;
            EnsureRoot();
        }

        public static ShellRouter EnsureRoot()
        {
            if (!settingsApplied)
            {
                SettingsStore.Load();
                SettingsApplier.ApplyAll(SettingsStore.Current, video: !Application.isEditor);
                settingsApplied = true;
            }
            if (ShellRouter.Instance != null) return ShellRouter.Instance;
            var assets = ShellAssets.Load();
            if (assets == null || assets.Panel == null) { Debug.LogWarning("ShellBoot: ShellAssets.asset missing; run TW/UI/Build Shell Assets."); return null; }
            var go = new GameObject(RootName);
            Object.DontDestroyOnLoad(go);
            var doc = go.AddComponent<UIDocument>();
            doc.panelSettings = assets.Panel;
            doc.sortingOrder = 100;
            var router = go.AddComponent<ShellRouter>();
            router.Assets = assets;
            return router;
        }
    }
}
