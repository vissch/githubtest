// Phase: B6 (implemented) — a deploy card plays its unit as a tiny film (the owner's idea, accepted 2026-10-08, built
// in treatment A, 2026-10-09). Rest the pointer on a card and its picture comes alive: three seconds of the unit
// doing its job, played in the card's own window under the key, cost and name, with a 2 px bone line counting the
// loop. The frames are not video: Tools/cardfilm.py bakes 10 fps x 3 s = 30 frames of 160 x 160 into one
// 960 x 800 sheet per unit under UI/Resources/CardFilms/, and this slices that sheet with Sprite.Create.
// A card with no sheet (every support card, and the units no film exists for) stays exactly as it is today.
// All the maths is static and pure so CardFilmTests can hold it without a panel, a sim or a frame.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UIElements;

namespace TW.UI
{
    public static class CardFilm
    {
        /// <summary>The sheet's shape, as Tools/cardfilm.py bakes it: 30 frames of 160 px in a 6 x 5 grid.</summary>
        public const int Frames = 30, Cols = 6, Rows = 5, Side = 160;
        public const float Fps = 10f;
        /// <summary>The goal's three seconds. Frames / Fps, by construction.</summary>
        public const float Seconds = Frames / Fps;
        public const string FolderPath = "CardFilms/";
        /// <summary>A frame's worth of float slack. 0.35 + 1/10 in floats is a hair under 0.45 and 0.35 + 3 a hair
        /// under 3.35, so without it the clock names the frame before the one it means and the line reads full.</summary>
        const float Slack = 1e-4f;

        // Sprite.Create every frame of every hovered card would allocate once a frame; both caches live for the
        // session (four sheets of ~3 MB, 30 sprites each), and Resources.Load returns the same texture anyway.
        static readonly Dictionary<string, Texture2D> sheets = new Dictionary<string, Texture2D>();
        static readonly Dictionary<string, Sprite[]> slices = new Dictionary<string, Sprite[]>();

        /// <summary>
        /// Which frame a card held for <paramref name="held"/> seconds shows: -1 before the plate's delay (the film and
        /// the tooltip open together), then 0..Frames-1, looping. The epsilon keeps frame boundaries on the frame the
        /// clock names: 0.35 + 1/10 in floats is a hair under 0.45.
        /// </summary>
        public static int Frame(float held, float delaySeconds = HudLayout.TooltipDelaySeconds)
        {
            if (held < delaySeconds) return -1;
            int f = (int)((held - delaySeconds) * Fps + Slack) % Frames;
            return f < 0 ? 0 : f;
        }

        /// <summary>How far through the loop the film is, 0..1, for the bone line under it. 0 before the delay.</summary>
        public static float Progress(float held, float delaySeconds = HudLayout.TooltipDelaySeconds)
        {
            if (held < delaySeconds) return 0f;
            float t = ((held - delaySeconds) * Fps + Slack) % Frames;
            return Mathf.Clamp01(t / Frames);
        }

        /// <summary>The sheet of a portrait name (HudText.PortraitName), or null when no film was baked for it.</summary>
        public static Texture2D Sheet(string portrait)
        {
            if (string.IsNullOrEmpty(portrait)) return null;
            if (sheets.TryGetValue(portrait, out var t)) return t;
            t = Resources.Load<Texture2D>(FolderPath + portrait);
            sheets[portrait] = t;
            return t;
        }

        /// <summary>One frame of a unit's film as a sprite, or null with no sheet or a frame out of the loop.</summary>
        public static Sprite Of(string portrait, int frame)
        {
            if (frame < 0 || frame >= Frames) return null;
            var sheet = Sheet(portrait);
            if (sheet == null) return null;
            if (!slices.TryGetValue(portrait, out var cut)) slices[portrait] = cut = new Sprite[Frames];
            if (cut[frame] != null) return cut[frame];
            // UV space counts up from the bottom; the sheet is pasted top row first.
            float x = (frame % Cols) * Side, y = sheet.height - (frame / Cols + 1) * Side;
            var s = Sprite.Create(sheet, new Rect(x, y, Side, Side), new Vector2(0.5f, 0.5f), 100f, 0, SpriteMeshType.FullRect);
            s.hideFlags = HideFlags.HideAndDontSave;
            cut[frame] = s;
            return s;
        }

        /// <summary>Forget the cached sheets and slices (a test that reloads Resources, a live reload, Play ending:
        /// the sprites belong to the domain that made them, so nothing may carry them into the next one).</summary>
        public static void ClearCache() { sheets.Clear(); slices.Clear(); }

        static CardFilm() => TW.Presentation.SceneStatics.Register(nameof(CardFilm), ClearCache);
    }

    /// <summary>
    /// The player: one hovered card at a time, its enter time on UNSCALED time so the film plays during a tactical
    /// pause, and one Update a frame that sets the film element's background and the line's width or hides both.
    /// </summary>
    public sealed class CardFilmPlayer
    {
        /// <summary>The class a card wears while its film plays, so the skin can thicken the cost plate over it.</summary>
        public const string PlayingClass = "is-filming";

        readonly System.Func<bool> allowed;
        readonly float delay;
        CardRefs hovered;
        float since;
        int lastFrame = -1;

        /// <summary><paramref name="allowed"/> is the tooltips setting: the film and the plate open together.</summary>
        public CardFilmPlayer(System.Func<bool> allowed = null, float delaySeconds = HudLayout.TooltipDelaySeconds)
        {
            this.allowed = allowed; delay = delaySeconds;
        }

        /// <summary>The card the pointer rests on, or null.</summary>
        public CardRefs Hovered => hovered;

        /// <summary>How long that card has been held, in unscaled seconds; -1 with no card.</summary>
        public float Held => hovered == null ? -1f : Time.unscaledTime - since;

        /// <summary>The frame last shown (0..Frames-1), or -1 when nothing is playing. What a capture writes down.</summary>
        public int LastFrame => lastFrame;

        /// <summary>How far through the loop the last shown frame is, 0..1.</summary>
        public float LastProgress => lastFrame < 0 ? 0f : CardFilm.Progress(Held, delay);

        /// <summary>Register one card's pointer events. Entering a card replaces the hovered one, so only one plays.</summary>
        public void Attach(CardRefs c)
        {
            if (c == null || c.Root == null) return;
            c.Root.RegisterCallback<PointerEnterEvent>(_ => Enter(c));
            c.Root.RegisterCallback<PointerLeaveEvent>(_ => Leave(c));
        }

        public void Enter(CardRefs c)
        {
            if (c == null || hovered == c) return;
            Hide(hovered);
            hovered = c; since = Time.unscaledTime;
        }

        public void Leave(CardRefs c)
        {
            if (hovered != c) return;
            Hide(c); hovered = null;
        }

        public void Clear() { Hide(hovered); hovered = null; }

        /// <summary>Show one card at an exact second of its film, for a capture or a PlayMode test.</summary>
        public void Preview(CardRefs c, float second)
        {
            if (c == null) return;
            Hide(hovered);
            hovered = c; since = Time.unscaledTime - delay - second;
            Update();
        }

        /// <summary>Once a frame, after the tooltip's own Update.</summary>
        public void Update()
        {
            var c = hovered;
            if (c == null) return;
            if (c.Film == null || c.Root == null) { Hide(c); return; }
            // locked and match-over cards show their lock and their grey, not a film; a poor or cooling one plays
            // under its existing shade, because the picture is what the card is, not whether it can be bought.
            if (c.LastLocked || c.LastOver || (allowed != null && !allowed())) { Hide(c); return; }
            float held = Time.unscaledTime - since;
            int frame = CardFilm.Frame(held, delay);
            var sprite = frame < 0 ? null : CardFilm.Of(c.FilmName, frame);
            if (sprite == null) { Hide(c); return; }
            lastFrame = frame;
            c.Root.AddToClassList(PlayingClass);
            c.Film.style.backgroundImage = new StyleBackground(sprite);
            c.Film.style.display = DisplayStyle.Flex;
            if (c.FilmLine != null)
            {
                c.FilmLine.style.display = DisplayStyle.Flex;
                c.FilmLine.style.width = Length.Percent(100f * CardFilm.Progress(held, delay));
            }
        }

        void Hide(CardRefs c)
        {
            lastFrame = -1;
            if (c == null) return;
            c.Root?.RemoveFromClassList(PlayingClass);
            if (c.Film == null) return;
            c.Film.style.display = DisplayStyle.None;
            if (c.FilmLine != null) c.FilmLine.style.display = DisplayStyle.None;
        }
    }
}
