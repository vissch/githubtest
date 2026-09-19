// Phase: P0 (implemented) — Bootstrap scene: loads the first playable scene. The campaign shell (A7) replaces this.
using UnityEngine;
using UnityEngine.SceneManagement;

namespace TW.Presentation
{
    public sealed class BootstrapLoader : MonoBehaviour
    {
        public string SceneName = "GreyboxCorridor";
        void Start() => SceneManager.LoadScene(SceneName);
    }
}
