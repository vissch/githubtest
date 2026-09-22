// Phase: B6 (implemented) — the statics that outlive a scene load, put back before the next scene starts.
// A Single LoadScene destroys every object but not a static. The owners clear their own (SimHost.OnDestroy,
// VATRenderer, GreyboxTerrainView, TankRenderer, Storm's Thaw); these are the ones nobody owns: the debug
// bombardment override the TestPanel presets set, the engine time scale Storm's lightning may be holding, and
// the shell's own input focus. ShellRouter calls this on every sceneLoaded and on Quit to menu.
using UnityEngine;

namespace TW.Presentation
{
    public static class SceneStatics
    {
        public static void Reset()
        {
            SimHost.BombardmentOverride = -1f;
            Time.timeScale = 1f;
            InputFocus.Reset();
            HudBridge.PointerOverUi = null;
        }
    }
}
