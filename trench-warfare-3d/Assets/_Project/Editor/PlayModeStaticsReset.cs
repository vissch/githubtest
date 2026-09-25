// Phase: tooling (2026-09-25) — when the editor leaves Play, put back the statics the session left behind.
// The project reloads the domain on entering Play but not on leaving it (EditorSettings), which is fast but means
// every static a match wrote is still there afterwards. An EditMode test run in the same editor then sees the last
// frame of that match: BlastReactionTests failed in a live editor and passed in the batch gate for exactly this
// reason. SceneStatics.ResetSession is the list; this only calls it at the right moment, after every object of the
// match has been destroyed (EnteredEditMode, not ExitingPlayMode, so no OnDisable runs after it and re-sets anything).
using UnityEditor;
using TW.Presentation;

namespace TW.Editor
{
    [InitializeOnLoad]
    public static class PlayModeStaticsReset
    {
        static PlayModeStaticsReset()
        {
            EditorApplication.playModeStateChanged -= OnChange;
            EditorApplication.playModeStateChanged += OnChange;
        }

        static void OnChange(PlayModeStateChange change)
        {
            if (change == PlayModeStateChange.EnteredEditMode) SceneStatics.ResetSession();
        }
    }
}
