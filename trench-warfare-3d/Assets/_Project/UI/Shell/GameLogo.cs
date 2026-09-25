// Phase: B6 (implemented) — the game's logo, DIESELFRONT / TOADSTEEL (owner, 2026-09-25), wherever the game names itself.
// The art was cut into layers by Seedream 5 Pro Layerize (background removed; the cog's window and the small cog in it
// rebuilt from the original) and ships in Resources/Logo, back to front: window, cog_top, gear (the big cog, whole, so
// it can turn), plate, side_left, side_right, cog_left, cog_right, word_top (DIESELFRONT), word_bottom (TOADSTEEL), with
// layout.json placing each in the logo's box; plus logo_full (all of it, flat) and wordmark (DIESELFRONT alone). A screen marks where it wants one and GameLogo.Fill (ShellScreen.Bind) puts it there:
//   .tw-logo--animated   the layers: the plates drop in (with the logo's soft shadow) while the big cog spins up behind
//                        them, DIESELFRONT slams in, then TOADSTEEL, each landing with a small punch of the whole logo
//                        and a kick of the cog; then the cog turns slowly, the words breathe, and a glint sweeps across
//                        the letter faces (glint_top_N / glint_bottom_N, baked to the faces only) every GlintPeriod;
//   .tw-logo             the whole logo, flat (pause, debrief);
//   .tw-wordmark         DIESELFRONT alone, for strips and headers.
// The element's width comes from USS; the height follows the art's aspect. Nothing here if the art is missing.
using System;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UIElements;

namespace TW.UI
{
    public static class GameLogo
    {
        public const string Folder = "Logo";
        public const string Name = "DIESELFRONT: TOADSTEEL";

        [Serializable] class Layer { public string name; public int x, y, w, h; }
        [Serializable] class Layout { public int w, h, glintFrames; public Layer[] layers; }

        static Layout layout;
        static readonly Dictionary<string, Texture2D> textures = new Dictionary<string, Texture2D>();

        static Texture2D Tex(string name)
        {
            if (!textures.TryGetValue(name, out var t) || t == null) textures[name] = t = Resources.Load<Texture2D>(Folder + "/" + name);
            return t;
        }

        static Layout GetLayout()
        {
            if (layout != null) return layout;
            var json = Resources.Load<TextAsset>(Folder + "/layout");
            if (json != null) layout = JsonUtility.FromJson<Layout>(json.text);
            return layout;
        }

        /// <summary>Fill every logo placeholder under root.</summary>
        public static void Fill(VisualElement root)
        {
            if (root == null) return;
            root.Query(className: "tw-logo--animated").ForEach(Animated);
            root.Query(className: "tw-logo").ForEach(e => Flat(e, "logo_full"));
            root.Query(className: "tw-wordmark").ForEach(e => Flat(e, "wordmark"));
        }

        /// <summary>A single image, scaled to the element's width with the art's aspect.</summary>
        static void Flat(VisualElement e, string image)
        {
            var t = Tex(image);
            if (t == null) { e.style.display = DisplayStyle.None; return; }
            e.pickingMode = PickingMode.Ignore;
            e.style.backgroundImage = new StyleBackground(t);
            e.style.unityBackgroundScaleMode = ScaleMode.ScaleToFit;
            KeepAspect(e, t.height / (float)t.width);
        }

        static void KeepAspect(VisualElement e, float hOverW)
        {
            void Fit() { float w = e.resolvedStyle.width; if (!float.IsNaN(w) && w > 0f) e.style.height = w * hOverW; }
            e.RegisterCallback<GeometryChangedEvent>(_ => Fit());
            Fit();
        }

        // ---- the layered, animated logo -------------------------------------------------------------------------------
        public const float FrameIn = 0.35f, WordIn = 0.26f, TopAt = 0.28f, BottomAt = 0.5f, IdleAt = 1.0f;
        public const float GearSpinIn = 0.9f, GearFromDeg = -120f, GearIdleDegPerSecond = 3f;
        public const float PunchSeconds = 0.18f, PunchScale = 0.025f, KickDeg = 7f;
        public const float GlintAt = 1.15f, GlintSweep = 0.55f, GlintLag = 0.18f, GlintPeriod = 7f;

        static void Animated(VisualElement e)
        {
            var L = GetLayout();
            if (L == null || L.layers == null || L.w <= 0) { Flat(e, "logo_full"); return; }
            e.Clear();
            e.pickingMode = PickingMode.Ignore;
            KeepAspect(e, L.h / (float)L.w);
            var group = new List<VisualElement>();
            VisualElement top = null, bottom = null, gear = null;
            var glintTop = GlintFrames("top", L.glintFrames); var glintBottom = GlintFrames("bottom", L.glintFrames);
            VisualElement topGlint = null, bottomGlint = null;
            foreach (var l in L.layers)
            {
                var t = Tex(l.name);
                if (t == null) continue;
                var c = new VisualElement { pickingMode = PickingMode.Ignore, usageHints = UsageHints.DynamicTransform };
                c.style.position = Position.Absolute;
                c.style.left = Length.Percent(100f * l.x / L.w); c.style.top = Length.Percent(100f * l.y / L.h);
                c.style.width = Length.Percent(100f * l.w / L.w); c.style.height = Length.Percent(100f * l.h / L.h);
                c.style.backgroundImage = new StyleBackground(t);
                c.style.unityBackgroundScaleMode = ScaleMode.StretchToFill;
                e.Add(c);
                if (l.name == "word_top") { top = c; topGlint = GlintLayer(c); }
                else if (l.name == "word_bottom") { bottom = c; bottomGlint = GlintLayer(c); }
                else if (l.name == "gear") gear = c; else group.Add(c);
            }
            float born = Time.unscaledTime;
            var parts = new Parts { Root = e, Group = group, Gear = gear, Top = top, Bottom = bottom, TopGlint = topGlint, BottomGlint = bottomGlint, GlintTop = glintTop, GlintBottom = glintBottom };
            var job = e.schedule.Execute(() => Step(Time.unscaledTime - born, parts)).Every(16);
            e.RegisterCallback<DetachFromPanelEvent>(_ => job.Pause());
            e.RegisterCallback<AttachToPanelEvent>(_ => { born = Time.unscaledTime; job.Resume(); });
            Step(0f, parts);
        }

        sealed class Parts
        {
            public VisualElement Root, Gear, Top, Bottom, TopGlint, BottomGlint;
            public List<VisualElement> Group;
            public Texture2D[] GlintTop, GlintBottom;
            public int ShownTop = -2, ShownBottom = -2;
        }

        static Texture2D[] GlintFrames(string word, int n)
        {
            if (n <= 0) return null;
            var f = new Texture2D[n];
            for (int i = 0; i < n; i++) if ((f[i] = Tex($"glint_{word}_{i}")) == null) return null;
            return f;
        }

        /// <summary>The glint over a word: a child filling it, so it follows the word's slam and breath.</summary>
        static VisualElement GlintLayer(VisualElement word)
        {
            var g = new VisualElement { pickingMode = PickingMode.Ignore };
            g.style.position = Position.Absolute; g.style.left = 0; g.style.top = 0; g.style.right = 0; g.style.bottom = 0;
            g.style.unityBackgroundScaleMode = ScaleMode.StretchToFill;
            g.style.display = DisplayStyle.None;
            word.Add(g);
            return g;
        }

        /// <summary>Which glint frame shows at t (the sweep's start is `at`, repeating every GlintPeriod), or -1 for none.</summary>
        public static int GlintFrame(float t, float at, int frames)
        {
            if (frames <= 0 || t < at) return -1;
            float u = Mathf.Repeat(t - at + 1e-4f, GlintPeriod) - 1e-4f;   // float slop: 8.15 - 1.15 is 6.9999995, not 7
            if (u >= GlintSweep) return -1;
            return Mathf.Min(frames - 1, (int)(u / GlintSweep * frames));
        }

        /// <summary>The punch of a word landing, x seconds after it lands (0 before and after).</summary>
        public static float Punch(float x) => x <= 0f || x >= PunchSeconds ? 0f : Mathf.Sin(Mathf.PI * x / PunchSeconds) * PunchScale;

        /// <summary>The cog's kick from a word landing: a quick shove forward that settles.</summary>
        public static float Kick(float x) => x <= 0f ? 0f : KickDeg * Mathf.Exp(-x * 5f) * (1f - Mathf.Exp(-x * 30f));

        /// <summary>The big cog's angle at t: spun in from GearFromDeg with an ease-out, then turning on slowly for ever.</summary>
        public static float GearAngle(float t)
        {
            if (t < GearSpinIn) { float k = Mathf.Clamp01(t / GearSpinIn); return GearFromDeg * (1f - k) * (1f - k) * (1f - k); }
            return (t - GearSpinIn) * GearIdleDegPerSecond % 360f;
        }

        static void ShowGlint(VisualElement g, Texture2D[] frames, int f, ref int shown)
        {
            if (g == null || frames == null || f == shown) return;
            shown = f;
            g.style.display = f < 0 ? DisplayStyle.None : DisplayStyle.Flex;
            if (f >= 0) g.style.backgroundImage = new StyleBackground(frames[f]);
        }

        /// <summary>The logo's pose at t seconds after it appeared (public for tests and captures).</summary>
        public static void Pose(float t, out float frameY, out float frameAlpha, out float topScale, out float topAlpha, out float bottomScale, out float bottomAlpha, out float bob)
        {
            float f = Mathf.Clamp01(t / FrameIn);
            float ease = 1f - (1f - f) * (1f - f) * (1f - f);
            frameY = Mathf.Lerp(-6f, 0f, ease); frameAlpha = f;
            Slam(t - TopAt, out topScale, out topAlpha);
            Slam(t - BottomAt, out bottomScale, out bottomAlpha);
            bob = t < IdleAt ? 0f : Mathf.Sin((t - IdleAt) * 1.7f);
        }

        /// <summary>A word lands: in from 1.6x, a little under 1 on impact, settling at 1.</summary>
        static void Slam(float t, out float scale, out float alpha)
        {
            if (t <= 0f) { scale = 1.6f; alpha = 0f; return; }
            float k = Mathf.Clamp01(t / WordIn);
            alpha = Mathf.Clamp01(k * 2.5f);
            if (k < 1f) scale = Mathf.Lerp(1.6f, 0.94f, k * k);
            else scale = Mathf.Lerp(0.94f, 1f, Mathf.Clamp01((t - WordIn) / 0.12f));
        }

        static void Step(float t, Parts p)
        {
            Pose(t, out float fy, out float fa, out float ts, out float ta, out float bs, out float ba, out float bob);
            float topLand = t - (TopAt + WordIn), bottomLand = t - (BottomAt + WordIn);
            float punch = 1f + Punch(topLand) + Punch(bottomLand);
            p.Root.style.scale = new Scale(new Vector3(punch, punch, 1f));
            // the plates drop as one: pixels of the logo's height (a percent translate is of each layer's own size)
            float h = p.Root.resolvedStyle.height, dy = float.IsNaN(h) ? 0f : fy * 0.01f * h;
            foreach (var g in p.Group) { g.style.translate = new Translate(0, dy, 0); g.style.opacity = fa; }
            if (p.Gear != null) { p.Gear.style.rotate = new Rotate(GearAngle(t) + Kick(topLand) + Kick(bottomLand)); p.Gear.style.opacity = fa; }
            ShowGlint(p.TopGlint, p.GlintTop, GlintFrame(t, GlintAt, p.GlintTop?.Length ?? 0), ref p.ShownTop);
            ShowGlint(p.BottomGlint, p.GlintBottom, GlintFrame(t, GlintAt + GlintLag, p.GlintBottom?.Length ?? 0), ref p.ShownBottom);
            var gear = p.Gear; var top = p.Top; var bottom = p.Bottom;
            float breathe = 1f + 0.012f * bob;
            if (top != null) { top.style.scale = new Scale(new Vector3(ts * breathe, ts * breathe, 1f)); top.style.opacity = ta; top.style.translate = new Translate(0, Length.Percent(-1.2f * bob), 0); }
            if (bottom != null) { bottom.style.scale = new Scale(new Vector3(bs, bs, 1f)); bottom.style.opacity = ba; bottom.style.translate = new Translate(0, Length.Percent(1.2f * bob), 0); }
        }
    }
}
