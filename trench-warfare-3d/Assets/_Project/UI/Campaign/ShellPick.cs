// Phase: B6 / docs/21 phase 6 (implemented) — is the mouse over one of this screen's plates? The 3D views behind the
// campaign screens ask HudBridge.IsPointerOverUi before they pick a building or a pin; the screen answers from its
// own tree: over the UI when the panel's pick lands inside it (the containers ignore picking, so the clear parts of
// the screen let the click through). False without a panel (an EditMode test).
using UnityEngine;
using UnityEngine.UIElements;

namespace TW.UI
{
    public static class ShellPick
    {
        /// <summary>Mouse in screen space (bottom-left origin, as Mouse.current reports it).</summary>
        public static bool Over(VisualElement root, Vector2 mouse)
        {
            var panel = root?.panel;
            if (panel == null) return false;
            Vector2 p = RuntimePanelUtils.ScreenToPanel(panel, new Vector2(mouse.x, Screen.height - mouse.y));
            var hit = panel.Pick(p);
            while (hit != null)
            {
                if (hit == root) return true;
                hit = hit.parent;
            }
            return false;
        }
    }
}
