// Phase: B6 (implemented) — the seam between the camera assembly and TW.UI.
// TW.Presentation.Camera must never reference TW.UI (DebugOverlay bootstraps the IMGUI BattleHud), and TW.UI needs
// nothing from here but two facts: which HUD is on, and whether the mouse is over interface chrome. Both live in this
// static so the camera, the click mask in TestPanel and the reticle in CombatFx can ask without knowing who answers.
// Pattern: SceneHooks (RenderGround.cs) and IZoomSource.
using UnityEngine;

namespace TW.Presentation
{
    public static class HudBridge
    {
        const string Key = "tw.hud.toolkit";

        /// <summary>
        /// UI Toolkit HUD on, IMGUI BattleHud off. Read once by whoever bootstraps the HUD and every frame by the
        /// OnGUI paths it replaces. Default 0 until the Toolkit HUD passes the two-developer playtest; F9 flips it.
        /// </summary>
        public static bool UseToolkitHud
        {
            get => PlayerPrefs.GetInt(Key, DefaultToolkit) != 0;
            set { PlayerPrefs.SetInt(Key, value ? 1 : 0); PlayerPrefs.Save(); }
        }

        /// <summary>The compiled-in default: 0 = IMGUI, 1 = Toolkit. Flipped to 1 in step A4 of the UI plan.</summary>
        public const int DefaultToolkit = 1;

        /// <summary>
        /// Is this mouse position (bottom-left origin, as Mouse.current.position reports it) over HUD chrome? The
        /// Toolkit HUD registers itself here in OnEnable and clears itself in OnDisable; null means no HUD is up and
        /// the caller should fall back to its own geometry (BattleHud.BarHeight / MinimapRect) or to "not over UI".
        /// </summary>
        public static System.Func<Vector2, bool> PointerOverUi;

        /// <summary>Convenience for the polling sites: false when nothing has registered.</summary>
        public static bool IsPointerOverUi(Vector2 mouse) => PointerOverUi != null && PointerOverUi(mouse);
    }
}
