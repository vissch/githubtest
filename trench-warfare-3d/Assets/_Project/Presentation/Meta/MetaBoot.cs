// Phase: B6 / docs/21 phase 6 (implemented) — installs the campaign views' factories into MetaServices when the player
// starts (before the first scene), so a screen in TW.UI can ask for a Home Front or a map without referencing this
// assembly. Each factory makes a GameObject in the current scene; the scene's end takes it, and SceneStatics forgets
// the reference (MetaServices.Reset). A test calls Install itself.
using UnityEngine;

namespace TW.Presentation.Meta
{
    public static class MetaBoot
    {
        [RuntimeInitializeOnLoadMethod(RuntimeInitializeLoadType.BeforeSceneLoad)]
        public static void Install()
        {
            MetaServices.MakeHomeFront = () => new GameObject("HomeFront").AddComponent<HomeFrontDiorama>();
            MetaServices.MakeMap = () => new GameObject("StrategicMap").AddComponent<StrategicMapView>();
        }
    }
}
