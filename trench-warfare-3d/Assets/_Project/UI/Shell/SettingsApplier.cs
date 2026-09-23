// Phase: B6 (implemented) — puts a GameSettings into effect: screen, quality, volume, UI scale, camera, keys.
// Lives in TW.UI because the camera and the shake are in the Camera assembly, which Presentation.Core cannot see.
// Video is applied only when asked (a settings Apply) and never in the editor; the camera part is re-applied on every
// scene load because TacticalCamera is a scene object and would otherwise come back with its defaults.
using UnityEngine;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.UI
{
    public static class SettingsApplier
    {
        public const int ReferenceWidth = 1920, ReferenceHeight = 1080;

        public static void ApplyAll(GameSettings s, bool video)
        {
            if (video) ApplyVideo(s);
            ApplyAudio(s);
            ApplyInterface(s);
            ApplyCamera(s);
            ApplyKeys(s);
        }

        public static void ApplyVideo(GameSettings s)
        {
            if (s.Video.QualityLevel >= 0 && s.Video.QualityLevel < QualitySettings.names.Length && QualitySettings.GetQualityLevel() != s.Video.QualityLevel)
                QualitySettings.SetQualityLevel(s.Video.QualityLevel, true);
            QualitySettings.vSyncCount = s.Video.VSync ? 1 : 0;   // after the quality level: levels carry their own vSync
            if (Application.isEditor) return;
            int w = s.Video.Width > 0 ? s.Video.Width : Screen.currentResolution.width;
            int h = s.Video.Height > 0 ? s.Video.Height : Screen.currentResolution.height;
            var mode = (FullScreenMode)Mathf.Clamp(s.Video.FullscreenMode, 0, 3);
            if (Screen.width != w || Screen.height != h || Screen.fullScreenMode != mode) Screen.SetResolution(w, h, mode);
        }

        public static void ApplyAudio(GameSettings s)
        {
            AudioListener.volume = s.Audio.Master;
            AudioLevels.Master = s.Audio.Master; AudioLevels.Ambience = s.Audio.Ambience; AudioLevels.Sfx = s.Audio.Sfx; AudioLevels.Music = s.Audio.Music;
        }

        /// <summary>UI scale: the panel scales with screen height against a reference; shrinking the reference grows the UI.</summary>
        public static void ApplyInterface(GameSettings s)
        {
            var panel = Resources.Load<PanelSettings>(HudBootstrap.PanelResource);
            if (panel != null)
            {
                float k = Mathf.Clamp(s.Interface.UiScale, 0.75f, 1.5f);
                var want = new Vector2Int(Mathf.RoundToInt(ReferenceWidth / k), Mathf.RoundToInt(ReferenceHeight / k));
                if (panel.referenceResolution != want) panel.referenceResolution = want;
            }
            var hud = Object.FindFirstObjectByType<HudController>(FindObjectsInactive.Include);
            if (hud != null && hud.Refs != null)
            {
                hud.Refs.MinimapBezel.EnableInClassList("hud-bezel--round", s.Interface.RoundRadar);
                hud.Refs.MinimapBezel.EnableInClassList("hud-bezel--square", !s.Interface.RoundRadar);
            }
        }

        public static void ApplyCamera(GameSettings s)
        {
            var cam = Object.FindFirstObjectByType<TacticalCamera>();
            if (cam != null)
            {
                cam.PanSpeed = s.Camera.PanSpeed;
                if (!Application.isEditor) cam.EdgeScroll = s.Camera.EdgeScroll;   // the editor forces it off (TacticalCamera.Start)
                cam.ZoomMin = s.Camera.ZoomMin; cam.ZoomMax = s.Camera.ZoomMax;
                cam.Zoom = Mathf.Clamp(cam.Zoom, cam.ZoomMin, cam.ZoomMax);
            }
            CameraShake.Strength = s.Camera.Shake;
        }

        public static void ApplyKeys(GameSettings s)
        {
            s.Bindings.Normalise();
            KeyMap.Current = s.Bindings.Clone();
        }
    }
}
