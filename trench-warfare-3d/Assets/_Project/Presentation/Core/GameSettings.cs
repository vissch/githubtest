// Phase: B6 (implemented) — what the settings screen edits and settings.json holds.
// One serialisable tree with a Version so a later build can migrate an old file; JsonUtility, so unknown fields are
// ignored and missing ones keep their defaults. Applying it to the engine is SettingsApplier's job (TW.UI, because
// the camera and the shake live in the Camera assembly which this one cannot reference).
using System;
using System.Collections.Generic;
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
            /// <summary>
            /// Muted by default (owner, 2026-09-23). SettingsApplier sends this straight to AudioListener.volume,
            /// so it silences everything at once. Only Master is zeroed: the three buses below keep their mix, so
            /// a player who turns the game up in Settings > Audio gets a balanced sound rather than a flat one.
            /// A saved settings.json wins over this, so it only affects a player who has never set a level.
            /// </summary>
            [Range(0f, 1f)] public float Master = 0f;
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
            [Range(0f, 1f)] public float Gore = 1f;    // DebrisRenderer.Gore: 0 = no gore lumps, no limbs
        }

        /// <summary>What each settings slider allows, by its name in Settings.uxml, and the field it edits. Migrate
        /// clamps a loaded file into these and SettingsScreen gives each slider its range from here, so a value
        /// the screen cannot show cannot be loaded either. GameSettingsTests holds Settings.uxml and the [Range]
        /// attributes above to this table.</summary>
        public static readonly IReadOnlyList<(string Slider, string Field, float Min, float Max)> Sliders = new[]
        {
            ("slider-pan-speed", "Camera.PanSpeed", 20f, 200f),
            ("slider-zoom-min", "Camera.ZoomMin", 2f, 30f),
            ("slider-zoom-max", "Camera.ZoomMax", 60f, 2000f),
            ("slider-shake", "Camera.Shake", 0f, 2f),
            ("slider-gore", "Camera.Gore", 0f, 1f),
            ("slider-master", "Audio.Master", 0f, 1f),
            ("slider-ambience", "Audio.Ambience", 0f, 1f),
            ("slider-sfx", "Audio.Sfx", 0f, 1f),
            ("slider-music", "Audio.Music", 0f, 1f),
            ("slider-ui-scale", "Interface.UiScale", 0.75f, 1.5f),
        };

        /// <summary>The (min, max) of a slider in <see cref="Sliders"/>.</summary>
        public static Vector2 RangeOf(string slider)
        {
            foreach (var s in Sliders) if (s.Slider == slider) return new Vector2(s.Min, s.Max);
            throw new ArgumentException($"no settings slider named {slider}");
        }

        static float Clamp(float v, string slider)
        {
            var r = RangeOf(slider);
            return Mathf.Clamp(v, r.x, r.y);
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
            Audio.Master = Clamp(Audio.Master, "slider-master"); Audio.Ambience = Clamp(Audio.Ambience, "slider-ambience");
            Audio.Sfx = Clamp(Audio.Sfx, "slider-sfx"); Audio.Music = Clamp(Audio.Music, "slider-music");
            Interface.UiScale = Clamp(Interface.UiScale, "slider-ui-scale");
            Camera.PanSpeed = Clamp(Camera.PanSpeed, "slider-pan-speed");
            Camera.Shake = Clamp(Camera.Shake, "slider-shake");
            Camera.Gore = Clamp(Camera.Gore, "slider-gore");
            Camera.ZoomMin = Clamp(Camera.ZoomMin, "slider-zoom-min");
            var far = RangeOf("slider-zoom-max");
            Camera.ZoomMax = Mathf.Clamp(Camera.ZoomMax, Mathf.Max(far.x, Camera.ZoomMin + 10f), far.y);
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
