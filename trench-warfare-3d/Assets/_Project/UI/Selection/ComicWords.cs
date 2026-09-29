// Phase: A5d (2026-09-29, the owner's night look) — comic sound words, rare (the owner: "keep rare comic sound words").
// The owner's edits of a game screenshot letter a "CRACK" into the night in pale blue with a dark rim. Here a word pops
// over the field at a notable moment: CRACK where a bullet passes a man close (NearMiss), CLANG where a shell bounces off
// a hull (VehicleArmourHit, stopped), KRUMP where a big shell bursts (Explosion, radius 6 m or more), KA-BOOM where a
// machine cooks off (VehicleCookOff). Rare by rule, not by luck: one word at most every fx.comicGap seconds (8), the
// weightiest of the moments since the last word, never more than MaxLive on screen, and only where the camera sees it
// (inside the frame's margin, nearer than MaxViewM). fx.comicWords 0 turns them off. No glow: the owner's edit had a
// magenta one, and the owner kept the letters without it. Drawn on the HUD's markers layer like DeathMarks, pooled.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.Sim;

namespace TW.UI
{
    public sealed class ComicWords : System.IDisposable
    {
        public const float PopSeconds = 0.16f, HoldSeconds = 1.05f, FadeSeconds = 0.35f, RisePx = 14f;
        public const float DefaultGap = 8f, MaxViewM = 140f, Margin = 0.08f, BigBurstM = 6f;
        public const int MaxLive = 2;
        /// <summary>Metres above the ground a word hangs: clear of the skulls DeathMarks puts at head height (2.1 m), which
        /// covered a letter at 2.2 (Play, 2026-09-29).</summary>
        public const float NearMissLiftM = 4.2f, LiftM = 5.2f;

        /// <summary>A moment worth a word, weightiest last.</summary>
        public enum Moment : byte { None, NearMiss, BigBurst, Ricochet, CookOff }

        /// <summary>The word each moment gets.</summary>
        public static string WordFor(Moment m) => m switch
        {
            Moment.NearMiss => "CRACK!",
            Moment.BigBurst => "KRUMP!",
            Moment.Ricochet => "CLANG!",
            Moment.CookOff => "KA-BOOM!",
            _ => null,
        };

        /// <summary>Which moment an event is, if any.</summary>
        public static Moment Classify(SimEvent e) => e.Type switch
        {
            SimEventType.NearMiss => Moment.NearMiss,
            SimEventType.Explosion when e.Scalar >= BigBurstM && e.Dir.y < 1.5f => Moment.BigBurst,   // a shell or masonry, not a cook-off (Dir.y 2)
            SimEventType.VehicleArmourHit when e.Scalar < 0f => Moment.Ricochet,                     // negative: the plate stopped it
            SimEventType.VehicleCookOff => Moment.CookOff,
            _ => Moment.None,
        };

        /// <summary>True when a point at this viewport position and distance is one the player can see a word at.</summary>
        public static bool Seen(Vector3 viewport, float distance) =>
            viewport.z > 0f && distance <= MaxViewM && viewport.x > Margin && viewport.x < 1f - Margin && viewport.y > Margin && viewport.y < 1f - Margin;

        /// <summary>The rule that keeps them rare: moments offered since the last word, the weightiest kept; a word may go
        /// out once Gap seconds have passed and fewer than MaxLive are showing. Pure, for the tests.</summary>
        public sealed class Picker
        {
            public float Gap = DefaultGap;
            float last = -1e9f;
            Moment best; Vector3 bestAt;

            public void Offer(Moment m, Vector3 at) { if (m > best) { best = m; bestAt = at; } }

            /// <summary>The word to show now, if any; the waiting moment is dropped either way once the gap has passed,
            /// so a word is always about something that just happened.</summary>
            public bool Take(float now, int live, out Moment m, out Vector3 at)
            {
                m = Moment.None; at = default;
                if (best == Moment.None) return false;
                if (now - last < Gap) { best = Moment.None; return false; }   // too soon: forget it
                if (live >= MaxLive) { best = Moment.None; return false; }
                m = best; at = bestAt; best = Moment.None; last = now;
                return true;
            }
        }

        sealed class Word { public VisualElement Root; public Label Text; public Vector3 World; public float Born, Tilt; public bool On; }

        readonly SimHost host;
        readonly VisualElement layer;
        readonly System.Func<Vector2, Vector2> toHud;
        readonly List<Word> live = new List<Word>(MaxLive), spare = new List<Word>(MaxLive);
        readonly Picker picker = new Picker();
        bool subscribed, on = true; int knobs = -1; Camera lastCam;

        public ComicWords(VisualElement root, SimHost host, System.Func<Vector2, Vector2> toHud)
        {
            this.host = host; this.toHud = toHud;
            layer = root?.Q("markers-layer");
            if (host != null && host.Events != null) { host.Events.OnEvent += OnEvent; subscribed = true; }
        }

        public void Dispose()
        {
            if (subscribed && host != null && host.Events != null) host.Events.OnEvent -= OnEvent;
            subscribed = false;
            foreach (var w in live) w.Root.RemoveFromHierarchy();
            foreach (var w in spare) w.Root.RemoveFromHierarchy();
            live.Clear(); spare.Clear();
        }

        /// <summary>Words showing now (for tests and captures).</summary>
        public int Count => live.Count;
        /// <summary>Words shown since the game began, and the last one (for captures: a still is taken when it changes).</summary>
        public static int Shown; public static string LastWord;
        static ComicWords() => SceneStatics.Register(nameof(ComicWords), () => { Shown = 0; LastWord = null; });

        void OnEvent(SimEvent e)
        {
            if (!on || layer == null) return;
            var m = Classify(e);
            if (m == Moment.None) return;
            Vector3 p = e.Pos;
            if (host != null && host.Local != null) p.y = RenderGround.Sample(host.Local.Map, p.x, p.z) + (m == Moment.NearMiss ? NearMissLiftM : LiftM);
            if (lastCam != null && !Seen(lastCam.WorldToViewportPoint(p), Vector3.Distance(lastCam.transform.position, p))) return;
            picker.Offer(m, p);
        }

        /// <summary>Once per frame: maybe one new word; place, pop, hold and fade the words showing.</summary>
        public void Tick(Camera cam, bool visible)
        {
            if (knobs != Knobs.Generation)
            {
                knobs = Knobs.Generation;
                on = Knobs.Get("fx.comicWords", true);
                picker.Gap = Mathf.Max(0.5f, Knobs.Get("fx.comicGap", DefaultGap));
            }
            lastCam = cam;
            float now = Time.unscaledTime;
            if (on && layer != null && picker.Take(now, live.Count, out var m, out var at)) Add(m, at, now);
            float life = PopSeconds + HoldSeconds + FadeSeconds;
            for (int i = live.Count - 1; i >= 0; i--)
            {
                var w = live[i];
                float age = now - w.Born;
                if (age >= life || !on) { Recycle(i); continue; }
                if (!visible || cam == null) { Show(w, false); continue; }
                Vector3 sp = cam.WorldToScreenPoint(w.World);
                if (sp.z <= cam.nearClipPlane) { Show(w, false); continue; }
                Show(w, true);
                Vector2 hud = toHud(sp);
                float pop = age < PopSeconds ? Mathf.Lerp(0.35f, 1.25f, age / PopSeconds) : Mathf.Lerp(1.25f, 1f, Mathf.Clamp01((age - PopSeconds) / 0.10f));
                float rise = RisePx * Mathf.Clamp01(age / life);
                float fade = 1f - Mathf.Clamp01((age - PopSeconds - HoldSeconds) / FadeSeconds);
                w.Root.transform.position = new Vector3(hud.x, hud.y - rise, 0f);
                w.Root.transform.scale = new Vector3(pop, pop, 1f);
                w.Root.transform.rotation = Quaternion.Euler(0f, 0f, w.Tilt);
                w.Root.style.opacity = fade;
            }
        }

        void Add(Moment m, Vector3 at, float now)
        {
            var w = spare.Count > 0 ? PopSpare() : Make();
            w.World = at; w.Born = now;
            w.Tilt = ((Mathf.Abs(at.x * 12.9898f + at.z * 78.233f) % 1f) - 0.5f) * 14f;   // a hand-lettered lean, -7..7 degrees
            w.Text.text = WordFor(m);
            Shown++; LastWord = w.Text.text;
            w.Root.EnableInClassList("hud-comic--big", m >= Moment.Ricochet);
            live.Add(w);
        }

        static void Show(Word w, bool show) { if (w.On == show) return; w.On = show; w.Root.style.display = show ? DisplayStyle.Flex : DisplayStyle.None; }

        Word Make()
        {
            var w = new Word();
            w.Root = new VisualElement { pickingMode = PickingMode.Ignore, usageHints = UsageHints.DynamicTransform }; w.Root.AddToClassList("hud-comic");
            w.Text = new Label { pickingMode = PickingMode.Ignore }; w.Text.AddToClassList("hud-comic__text");
            w.Root.Add(w.Text);
            layer.Add(w.Root);
            w.On = true;
            return w;
        }

        Word PopSpare() { var w = spare[spare.Count - 1]; spare.RemoveAt(spare.Count - 1); return w; }

        void Recycle(int i) { var w = live[i]; live.RemoveAt(i); Show(w, false); spare.Add(w); }
    }
}
