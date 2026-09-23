// Phase: P0 (implemented) — Bootstrap scene: loads the first playable scene. The campaign shell (A7) replaces this.
using UnityEngine;
using UnityEngine.SceneManagement;

namespace TW.Presentation
{
    public sealed class BootstrapLoader : MonoBehaviour
    {
        public string SceneName = "GreyboxCorridor";
        /// <summary>Tooling (TW.Perf.PerfBench): a scene to load instead, set before the first scene loads.</summary>
        public static string Override;
        void Start() => SceneManager.LoadScene(string.IsNullOrEmpty(Override) ? SceneName : Override);
    }
}
