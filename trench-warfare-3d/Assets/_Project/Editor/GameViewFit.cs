// Phase: B1 (implemented)
// On entering Play mode, puts the Game view's zoom slider back to its minimum. On a scaled display the slider can sit
// above it (2x on a 150 % screen), which magnifies the picture and pushes the HUD's edges (silver, deploy bar, minimap)
// out of sight. Editor only; uses reflection because GameView is internal.
using System.Reflection;
using UnityEditor;

namespace TW.Editor
{
    [InitializeOnLoad]
    public static class GameViewFit
    {
        static GameViewFit() { EditorApplication.playModeStateChanged += s => { if (s == PlayModeStateChange.EnteredPlayMode) Fit(); }; }

        [MenuItem("TW/Fit Game View")]
        public static void Fit()
        {
            var type = System.Type.GetType("UnityEditor.GameView,UnityEditor");
            if (type == null) return;
            const BindingFlags flags = BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public;
            var min = type.GetProperty("minScale", flags);
            var snap = type.GetMethod("SnapZoom", flags);
            if (min == null || snap == null) return;
            foreach (var view in UnityEngine.Resources.FindObjectsOfTypeAll(type))
            {
                snap.Invoke(view, new object[] { (float)min.GetValue(view) });
                ((EditorWindow)view).Repaint();
            }
        }
    }
}
