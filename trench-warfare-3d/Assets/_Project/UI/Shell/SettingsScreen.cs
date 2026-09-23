// Phase: B6 (implemented) — Settings: Controls | Video | Audio | Interface, edited as a draft, applied on APPLY.
// Controls is one row per rebindable action with a primary and a secondary key-cap; click a cap and the next key
// pressed is bound (KeyMap.Listen), Esc cancels, and a key already bound elsewhere is taken from that action (the cap
// shows the conflict first). Video reads the monitor's modes and the project's quality levels; the UI scale slider
// previews live and is put back on BACK if not applied.
using System;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.InputSystem;
using UnityEngine.UIElements;
using TW.Presentation;

namespace TW.UI
{
    public sealed class SettingsScreen : ShellScreen
    {
        public static readonly string[] RequiredNames =
        {
            "settings-plate", "tabs", "tab-controls", "tab-video", "tab-audio", "tab-interface", "pages", "page-controls", "page-video", "page-audio", "page-interface",
            "controls-list", "slider-pan-speed", "toggle-edge-scroll", "slider-zoom-min", "slider-zoom-max", "slider-shake", "slider-gore", "btn-controls-defaults",
            "dropdown-resolution", "dropdown-fullscreen", "toggle-vsync", "dropdown-quality",
            "slider-master", "slider-ambience", "slider-sfx", "slider-music", "audio-note",
            "slider-ui-scale", "toggle-tooltips", "toggle-round-radar",
            "btn-apply", "btn-defaults", "btn-back",
        };
        static readonly string[] Tabs = { "controls", "video", "audio", "interface" };
        static readonly string[] FullscreenNames = { "EXCLUSIVE FULLSCREEN", "FULLSCREEN WINDOW", "MAXIMISED WINDOW", "WINDOWED" };

        GameSettings draft;
        float appliedScale;
        /// <summary>What the SOUND IS OFF warning replaces. Captured from the UXML in OnBind, BEFORE the first
        /// BuildAudio, because Refill() runs BuildAudio again and would otherwise capture the warning itself.</summary>
        string audioNote;
        const string SoundOffNote = "SOUND IS OFF. DRAG MASTER UP TO HEAR THE GAME.";
        IDisposable listening; Button listeningCap;
        readonly List<Resolution> resolutions = new List<Resolution>();
        readonly Dictionary<GameAction, (Button primary, Button secondary)> caps = new Dictionary<GameAction, (Button, Button)>();

        public override VisualTreeAsset Tree(ShellAssets a) => a?.Settings;

        protected override void OnBind()
        {
            draft = SettingsStore.Current.Clone();
            appliedScale = SettingsStore.Current.Interface.UiScale;
            audioNote = Root?.Q<UnityEngine.UIElements.Label>("audio-note")?.text;
            foreach (var t in Tabs) { string tab = t; Btn("tab-" + tab, () => ShowTab(tab)); }
            ShowTab("controls");
            BuildControls();
            BuildVideo();
            BuildAudio();
            BuildInterface();
            Btn("btn-apply", Apply);
            // DEFAULTS keeps the audio levels. Otherwise a player who turned the sound up, then pressed DEFAULTS
            // for some unrelated reason, silently lost it again — and pressing DEFAULTS is exactly what someone
            // does when they think something is wrong.
            Btn("btn-defaults", () => { var audio = draft.Audio; draft = GameSettings.Defaults(); draft.Audio = audio; Refill(); });
            Btn("btn-back", Back);
        }

        protected override void OnUnbind()
        {
            listening?.Dispose(); listening = null; InputFocus.Listening = false;
            if (!Mathf.Approximately(appliedScale, PreviewScale)) { var s = SettingsStore.Current.Clone(); s.Interface.UiScale = appliedScale; SettingsApplier.ApplyInterface(s); }
            // The audio preview goes back if they left without applying. After APPLY, SettingsStore.Current IS
            // the draft, so this is a no-op; without it a previewed volume would outlive the screen that
            // previewed it and there would be no way to explain why the game got loud.
            SettingsApplier.ApplyAudio(SettingsStore.Current);
        }

        float PreviewScale => draft != null ? draft.Interface.UiScale : appliedScale;

        void ShowTab(string tab)
        {
            foreach (var t in Tabs)
            {
                Root.Q("page-" + t)?.EnableInClassList("tw-hidden", t != tab);
                Root.Q("tab-" + t)?.EnableInClassList("tw-tab--active", t == tab);
            }
        }

        void Refill()
        {
            Root.Q("controls-list")?.Clear(); caps.Clear();
            BuildControls(); BuildVideo(); BuildAudio(); BuildInterface();
        }

        // ---- controls -------------------------------------------------------------------------------------------------
        void BuildControls()
        {
            var list = Root.Q("controls-list");
            if (list != null && list.childCount == 0)
            {
                bool alt = false;
                for (int i = 0; i < KeyMap.ActionCount; i++)
                {
                    var a = (GameAction)i;
                    if (!KeyMap.Rebindable(a)) continue;
                    var row = new VisualElement(); row.AddToClassList("tw-row"); if (alt) row.AddToClassList("tw-row--alt"); alt = !alt;
                    var label = new Label(KeyMap.Label(a)); label.AddToClassList("tw-row__label"); row.Add(label);
                    var ctl = new VisualElement(); ctl.AddToClassList("tw-row__control"); row.Add(ctl);
                    var p = Cap(a, false); var s = Cap(a, true);
                    ctl.Add(p); ctl.Add(s);
                    caps[a] = (p, s);
                    list.Add(row);
                }
            }
            foreach (var kv in caps) { RefreshCap(kv.Key, false); RefreshCap(kv.Key, true); }
            Slider("slider-pan-speed", draft.Camera.PanSpeed, v => draft.Camera.PanSpeed = v);
            Toggle("toggle-edge-scroll", draft.Camera.EdgeScroll, v => draft.Camera.EdgeScroll = v);
            Slider("slider-zoom-min", draft.Camera.ZoomMin, v => draft.Camera.ZoomMin = v);
            Slider("slider-zoom-max", draft.Camera.ZoomMax, v => draft.Camera.ZoomMax = v);
            Slider("slider-shake", draft.Camera.Shake, v => draft.Camera.Shake = v);
            Slider("slider-gore", draft.Camera.Gore, v => draft.Camera.Gore = v);
            Btn("btn-controls-defaults", () => { draft.Bindings = KeyMap.Defaults(); foreach (var kv in caps) { RefreshCap(kv.Key, false); RefreshCap(kv.Key, true); } });
        }

        Button Cap(GameAction a, bool secondary)
        {
            var b = new Button { name = $"key-{a}{(secondary ? "-alt" : "")}" };
            b.AddToClassList("tw-keycap"); b.focusable = false;
            b.clicked += () => Listen(a, secondary, b);
            return b;
        }

        void RefreshCap(GameAction a, bool secondary)
        {
            if (!caps.TryGetValue(a, out var pair)) return;
            var b = secondary ? pair.secondary : pair.primary;
            var key = secondary ? draft.Bindings.Secondary[(int)a] : draft.Bindings.Primary[(int)a];
            b.text = key == Key.None ? (secondary ? "-" : "NONE") : KeyMap.Display(key);
            b.EnableInClassList("tw-keycap--listening", false);
            b.EnableInClassList("tw-keycap--conflict", false);
        }

        void Listen(GameAction a, bool secondary, Button cap)
        {
            listening?.Dispose();
            if (listeningCap != null) RefreshCap(a, secondary);
            listeningCap = cap;
            cap.text = "PRESS A KEY"; cap.EnableInClassList("tw-keycap--listening", true);
            Router?.Clock?.Add(MatchClock.Hold.Modal);
            listening = KeyMap.Listen(key =>
            {
                listening = null; listeningCap = null;
                Router?.Clock?.Remove(MatchClock.Hold.Modal);
                // take the key from whoever had it
                for (int i = 0; i < KeyMap.ActionCount; i++)
                {
                    if (i == (int)a) continue;
                    if (draft.Bindings.Primary[i] == key) { draft.Bindings.Primary[i] = Key.None; RefreshCap((GameAction)i, false); Flash((GameAction)i, false); }
                    if (draft.Bindings.Secondary[i] == key) { draft.Bindings.Secondary[i] = Key.None; RefreshCap((GameAction)i, true); Flash((GameAction)i, true); }
                }
                if (secondary) draft.Bindings.Secondary[(int)a] = key; else draft.Bindings.Primary[(int)a] = key;
                RefreshCap(a, secondary);
            }, () => { listening = null; listeningCap = null; Router?.Clock?.Remove(MatchClock.Hold.Modal); RefreshCap(a, secondary); });
        }

        void Flash(GameAction a, bool secondary)
        {
            if (caps.TryGetValue(a, out var pair)) (secondary ? pair.secondary : pair.primary).EnableInClassList("tw-keycap--conflict", true);
        }

        // ---- video -------------------------------------------------------------------------------------------------------
        void BuildVideo()
        {
            var res = Root.Q<DropdownField>("dropdown-resolution");
            if (res != null)
            {
                resolutions.Clear(); var choices = new List<string>();
                foreach (var r in Screen.resolutions)
                {
                    string label = $"{r.width} x {r.height}  {Mathf.RoundToInt((float)r.refreshRateRatio.value)} HZ";
                    if (choices.Contains(label)) continue;
                    choices.Add(label); resolutions.Add(r);
                }
                if (choices.Count == 0) { choices.Add($"{Screen.width} x {Screen.height}"); resolutions.Add(Screen.currentResolution); }
                res.choices = choices;
                int cur = 0;
                for (int i = 0; i < resolutions.Count; i++) if (resolutions[i].width == (draft.Video.Width > 0 ? draft.Video.Width : Screen.width) && resolutions[i].height == (draft.Video.Height > 0 ? draft.Video.Height : Screen.height)) cur = i;
                res.index = cur;
                res.RegisterValueChangedCallback(_ => { var r = resolutions[Mathf.Clamp(res.index, 0, resolutions.Count - 1)]; draft.Video.Width = r.width; draft.Video.Height = r.height; draft.Video.RefreshRateHz = Mathf.RoundToInt((float)r.refreshRateRatio.value); });
            }
            var fs = Root.Q<DropdownField>("dropdown-fullscreen");
            if (fs != null)
            {
                fs.choices = new List<string>(FullscreenNames);
                fs.index = Mathf.Clamp(draft.Video.FullscreenMode, 0, 3);
                fs.RegisterValueChangedCallback(_ => draft.Video.FullscreenMode = fs.index);
            }
            Toggle("toggle-vsync", draft.Video.VSync, v => draft.Video.VSync = v);
            var q = Root.Q<DropdownField>("dropdown-quality");
            if (q != null)
            {
                var names = new List<string>(); foreach (var n in QualitySettings.names) names.Add(n.ToUpperInvariant());
                q.choices = names;
                q.index = Mathf.Clamp(draft.Video.QualityLevel >= 0 ? draft.Video.QualityLevel : QualitySettings.GetQualityLevel(), 0, Mathf.Max(0, names.Count - 1));
                q.RegisterValueChangedCallback(_ => draft.Video.QualityLevel = q.index);
            }
        }

        // ---- audio, interface ---------------------------------------------------------------------------------------------
        /// <summary>
        /// The audio sliders apply LIVE, the way BuildInterface's UI-scale slider already does.
        ///
        /// They did not, and this panel is the one that tells the player "SOUND IS OFF. DRAG MASTER UP TO HEAR
        /// THE GAME" on every fresh install. Dragging master up wrote to the draft and stopped there, so the
        /// game stayed silent until APPLY was pressed - and when a control appears to do nothing the next move
        /// is Escape, which is Back(), which pops without saving. The player followed the only instruction the
        /// game gives about its own silence, heard nothing, and lost the change on the way out.
        ///
        /// Audio is the page where the effect IS the feedback: a volume cannot be previewed by reading a number.
        /// The preview is taken back in OnUnbind if they leave without applying, as the UI-scale one is.
        /// </summary>
        void BuildAudio()
        {
            Slider("slider-master", draft.Audio.Master, v => { draft.Audio.Master = v; SoundChanged(); });
            Slider("slider-ambience", draft.Audio.Ambience, v => { draft.Audio.Ambience = v; SoundChanged(); });
            Slider("slider-sfx", draft.Audio.Sfx, v => { draft.Audio.Sfx = v; SoundChanged(); });
            Slider("slider-music", draft.Audio.Music, v => { draft.Audio.Music = v; SoundChanged(); });
            RefreshAudioNote();
        }

        /// <summary>Put the draft's levels into the engine, and keep the warning honest about them.</summary>
        void SoundChanged() { SettingsApplier.ApplyAudio(draft); RefreshAudioNote(); }

        /// <summary>
        /// A master at zero with no readout is indistinguishable from broken audio, so the panel says so. It is
        /// recomputed on every change because it used to be written ONCE while the tab was built: it still read
        /// SOUND IS OFF after the player had turned the sound on, and it never appeared at all for a player who
        /// turned it off, since the old guard could only ever write the warning and never clear it.
        /// </summary>
        void RefreshAudioNote()
        {
            var note = Root?.Q<UnityEngine.UIElements.Label>("audio-note");
            if (note == null || draft == null) return;
            note.text = draft.Audio.Master <= 0f ? SoundOffNote : (audioNote ?? string.Empty);
        }

        void BuildInterface()
        {
            Slider("slider-ui-scale", draft.Interface.UiScale, v => { draft.Interface.UiScale = v; SettingsApplier.ApplyInterface(draft); });
            Toggle("toggle-tooltips", draft.Interface.Tooltips, v => draft.Interface.Tooltips = v);
            Toggle("toggle-round-radar", draft.Interface.RoundRadar, v => draft.Interface.RoundRadar = v);
        }

        void Slider(string name, float value, Action<float> onChange)
        {
            var s = Root.Q<Slider>(name);
            if (s == null) return;
            s.SetValueWithoutNotify(value);
            s.RegisterValueChangedCallback(e => onChange(e.newValue));
        }

        void Toggle(string name, bool value, Action<bool> onChange)
        {
            var t = Root.Q<Toggle>(name);
            if (t == null) return;
            t.SetValueWithoutNotify(value);
            t.RegisterValueChangedCallback(e => onChange(e.newValue));
        }

        void Apply()
        {
            draft.Migrate();
            SettingsStore.Save(draft.Clone());
            SettingsApplier.ApplyAll(SettingsStore.Current, video: true);
            appliedScale = SettingsStore.Current.Interface.UiScale;
        }

        void Back() => Router?.Pop();

        public override void OnEscape()
        {
            if (listening != null) { listening.Dispose(); listening = null; if (listeningCap != null) { foreach (var kv in caps) { RefreshCap(kv.Key, false); RefreshCap(kv.Key, true); } listeningCap = null; } Router?.Clock?.Remove(MatchClock.Hold.Modal); return; }
            Back();
        }
    }
}
