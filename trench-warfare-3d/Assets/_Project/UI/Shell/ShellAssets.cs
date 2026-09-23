// Phase: B6 (implemented) — the one asset the shell loads: which UXML is which screen, the panel, the missions.
// Assets/_Project/UI/Resources/ShellAssets.asset, built by TW/UI/Build Shell Assets (ShellAssetsBuilder).
using UnityEngine;
using UnityEngine.UIElements;

namespace TW.UI
{
    public sealed class ShellAssets : ScriptableObject
    {
        public const string Resource = "ShellAssets";
        public PanelSettings Panel;
        public VisualTreeAsset MainMenu, MissionSelect, PauseMenu, Settings, Debrief;
        public Texture2D Backdrop;
        public MissionCatalog Catalog;

        public static ShellAssets Load() => Resources.Load<ShellAssets>(Resource);
    }
}
