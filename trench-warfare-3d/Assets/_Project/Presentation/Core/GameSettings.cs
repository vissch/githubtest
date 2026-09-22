// Phase: B6 (implemented) — what the settings screen edits and settings.json holds.
// One serialisable tree with a Version so a later build can migrate an old file; JsonUtility, so unknown fields are
// ignored and missing ones keep their defaults. Applying it to the engine is SettingsApplier's job (TW.UI, because
// the camera and the shake live in the Camera assembly which this one cannot reference).
using System;
using UnityEngine;

namespace TW.Presentation
{
    [Serializable]
    public sealed class GameSettings
    {
        public const int CurrentVersion = 1;
        public int Version = CurrentVersion;
        public VideoSettings Video = new VideoSettings();
        public AudioSettings Audio = new AudioSettings();
        public InterfaceSettings Interface = new InterfaceSettings();
        public CameraSettings Camera = new CameraSettings();
        public KeyMap.Bindings Bindings = KeyMap.Defaults();

        [Serializable]
        public sealed class VideoSettings
        {
            public int Width, Height;            // 0 = native
            public int RefreshRateHz;            // 0 = current
            public int FullscreenMode = 1;       // FullScreenMode: 0 exclusive, 1 fullscreen window, 2 maximised, 3 windowed
            public bool VSync = true;
            public int QualityLevel = -1;        // -1 = leave the project default
        }

        [Serializable]
        public sealed class AudioSettings
        {
            [Range(0f, 1f)] public float Master = 1f;
            [Range(0f, 1f)] public float Ambience = 0.8f;
            [Range(0f, 1f)] public float Sfx = 1f;
            [Range(0f, 1f)] public float Music = 0.8f;
        }

        [Serializable]
        public sealed class InterfaceSettings
        {
            [Range(0.75f, 1.5f)] public float UiScale = 1f;
            public bool Tooltips = true;
            public bool RoundRadar = false;      // the Dust Front bezel over the square minimap
        }

        [Serializable]
        public sealed class CameraSettings
        {
            public float PanSpeed = 60f;         // TacticalCamera.PanSpeed
            public bool EdgeScroll = true;       // TacticalCamera.EdgeScroll (the editor forces it off)
            public float ZoomMin = 6f, ZoomMax = 600f;
            [Range(0f, 2f)] public float Shake = 1f;   // CameraShake.Strength
            [Range(0f, 1f)] public float Gore = 1f;    // DebrisRenderer.Gore, once committed
        }

        public static GameSettings Defaults() => new GameSettings();

        /// <summary>Bring an older or hand-edited file up to this build's shape.</summary>
        public void Migrate()
        {
            Video ??= new VideoSettings();
            Audio ??= new AudioSettings();
            Interface ??= new InterfaceSettings();
            Camera ??= new CameraSettings();
            Bindings ??= KeyMap.Defaults();
            Bindings.Normalise();
            // fill unbound actions from defaults so a file from a build with fewer actions still has keys for the new ones
            var d = KeyMap.Defaults();
            for (int i = 0; i < KeyMap.ActionCount; i++)
                if (Bindings.Primary[i] == UnityEngine.InputSystem.Key.None && Bindings.Secondary[i] == UnityEngine.InputSystem.Key.None)
                { Bindings.Primary[i] = d.Primary[i]; Bindings.Secondary[i] = d.Secondary[i]; }
            Audio.Master = Mathf.Clamp01(Audio.Master); Audio.Ambience = Mathf.Clamp01(Audio.Ambience);
            Audio.Sfx = Mathf.Clamp01(Audio.Sfx); Audio.Music = Mathf.Clamp01(Audio.Music);
            Interface.UiScale = Mathf.Clamp(Interface.UiScale, 0.75f, 1.5f);
            Camera.ZoomMin = Mathf.Clamp(Camera.ZoomMin, 2f, 60f);
            Camera.ZoomMax = Mathf.Clamp(Camera.ZoomMax, Camera.ZoomMin + 10f, 2000f);
            Version = CurrentVersion;
        }

        public GameSettings Clone()
        {
            var c = JsonUtility.FromJson<GameSettings>(JsonUtility.ToJson(this));
            c.Migrate();
            return c;
        }

        public string ToJson() => JsonUtility.ToJson(this, true);

        public static GameSettings FromJson(string json)
        {
            GameSettings s = null;
            try { if (!string.IsNullOrWhiteSpace(json)) s = JsonUtility.FromJson<GameSettings>(json); }
            catch (Exception) { s = null; }
            s ??= Defaults();
            s.Migrate();
            return s;
        }
    }
}
