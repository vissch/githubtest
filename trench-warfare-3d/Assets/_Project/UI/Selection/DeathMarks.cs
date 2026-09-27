// Phase: B6 (implemented) — a skull where someone dies (owner, 2026-09-25): on every Death event a small skull pops up
// over the man as he was drawn, rises and fades. Bone white when the enemy loses a man, alarm red when we do. Deaths
// close together (MergeM, MergeSeconds) join the skull already there and it counts them ("x4"), so a barrage reads as
// a few counted skulls rather than a wall of them. Drawn on the HUD's markers layer (under every panel) like the
// selection marks, pooled, moved by transform. The skull is painted here from signed distances, white with a dark rim
// for snow and mud alike, and tinted per side; an artist's icon can replace SkullTexture later.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.Sim;

namespace TW.UI
{
    public sealed class DeathMarks : System.IDisposable
    {
        public const float LifeSeconds = 2.6f, FadeSeconds = 0.9f, PopSeconds = 0.18f, RisePx = 34f, SizePx = 30f;
        public const float MergeM = 5f, MergeSeconds = 0.9f, HeadM = 2.1f;
        public const int MaxSkulls = 48, TexPx = 64;
        public static readonly Color Theirs = new Color32(0xE8, 0xE1, 0xD2, 255);   // an enemy fell: bone
        public static readonly Color Ours = new Color32(0xE0, 0x2B, 0x2B, 255);     // one of ours fell: alarm red

        sealed class Skull
        {
            public VisualElement Root, Icon; public Label Count;
            public Vector3 World; public bool Ours; public int N, ShownN = -1; public float Born, Last; public bool On;
        }

        readonly SimHost host;
        readonly VisualElement layer;
        readonly System.Func<Vector2, Vector2> toHud;
        readonly Texture2D tex;
        readonly List<Skull> live = new List<Skull>(MaxSkulls), spare = new List<Skull>(MaxSkulls);
        bool subscribed;

        public DeathMarks(VisualElement root, SimHost host, System.Func<Vector2, Vector2> toHud)
        {
            this.host = host; this.toHud = toHud;
            layer = root?.Q("markers-layer");
            tex = SkullTexture(TexPx);
            if (host != null && host.Events != null) { host.Events.OnEvent += OnEvent; subscribed = true; }
        }

        public void Dispose()
        {
            if (subscribed && host != null && host.Events != null) host.Events.OnEvent -= OnEvent;
            subscribed = false;
            foreach (var s in live) s.Root.RemoveFromHierarchy();
            foreach (var s in spare) s.Root.RemoveFromHierarchy();
            live.Clear(); spare.Clear();
            if (tex != null) { if (Application.isPlaying) Object.Destroy(tex); else Object.DestroyImmediate(tex); }
        }

        /// <summary>Skulls showing now, and how many deaths skull i counts (for tests and captures).</summary>
        public int Count => live.Count;
        public int CountOf(int i) => live[i].N;

        void OnEvent(SimEvent e)
        {
            if (e.Type != SimEventType.Death || layer == null || host.Local == null) return;
            var w = host.Local.World;
            if (e.A < 0 || e.A >= w.HighWater) return;
            // events reach us once a frame: at 2x-8x a later tick may already have deployed a new man into his slot, and
            // then the slot describes the new man; his side is then the killer's enemy, his place the event's
            bool reused = w.IsAlive(e.A);
            int killer = e.B;
            bool ours = reused && killer >= 0 && killer < w.HighWater ? (w.Team[killer] & 1) != 0 : (w.Team[e.A] & 1) == 0;
            // where he was drawn (the corpse is laid there too), at head height
            Vector3 p = host.Presenter != null && !reused ? (Vector3)host.Presenter.Drawn(e.A) : (Vector3)e.Pos;
            p.y = RenderGround.Sample(host.Local.Map, p.x, p.z) + HeadM;
            Add(p, ours, Time.unscaledTime);
        }

        /// <summary>A death at p: joins a skull of the same side close by and recent, or makes a new one.</summary>
        public void Add(Vector3 p, bool ours, float now)
        {
            foreach (var s in live)
                if (s.Ours == ours && now - s.Last <= MergeSeconds && (s.World - p).sqrMagnitude <= MergeM * MergeM)
                {
                    s.N++; s.Last = now; s.Born = Mathf.Max(s.Born, now - PopSeconds);   // it pops again and lives on from now
                    s.World = Vector3.Lerp(s.World, p, 1f / s.N);
                    return;
                }
            if (live.Count >= MaxSkulls) Recycle(0);   // the oldest makes room
            var k = spare.Count > 0 ? Pop() : Make();
            k.World = p; k.Ours = ours; k.N = 1; k.Born = k.Last = now;
            k.Icon.style.unityBackgroundImageTintColor = ours ? Ours : Theirs;
            k.Root.EnableInClassList("hud-skull--ours", ours);
            live.Add(k);
        }

        /// <summary>Once per frame: place, pop, rise and fade every skull; retire the finished.</summary>
        public void Tick(Camera cam, bool visible)
        {
            float now = Time.unscaledTime;
            for (int i = live.Count - 1; i >= 0; i--)
            {
                var s = live[i];
                float age = now - s.Born;
                if (age >= LifeSeconds) { Recycle(i); continue; }
                if (!visible || cam == null) { Show(s, false); continue; }
                Vector3 sp = cam.WorldToScreenPoint(s.World);
                if (sp.z <= cam.nearClipPlane) { Show(s, false); continue; }
                Show(s, true);
                Vector2 at = toHud(sp);
                float pop = age < PopSeconds ? Mathf.Lerp(0.5f, 1.15f, age / PopSeconds) : Mathf.Lerp(1.15f, 1f, Mathf.Clamp01((age - PopSeconds) / 0.12f));
                float rise = RisePx * Mathf.SmoothStep(0f, 1f, age / LifeSeconds);
                float fade = 1f - Mathf.Clamp01((age - (LifeSeconds - FadeSeconds)) / FadeSeconds);
                s.Root.transform.position = new Vector3(at.x, at.y - rise, 0f);
                s.Root.transform.scale = new Vector3(pop, pop, 1f);
                s.Root.style.opacity = fade;
                if (s.N != s.ShownN)
                {
                    s.ShownN = s.N;
                    s.Count.text = s.N > 1 ? Times(s.N) : "";
                    s.Count.style.display = s.N > 1 ? DisplayStyle.Flex : DisplayStyle.None;
                }
            }
        }

        static void Show(Skull s, bool on) { if (s.On == on) return; s.On = on; s.Root.style.display = on ? DisplayStyle.Flex : DisplayStyle.None; }

        Skull Make()
        {
            var k = new Skull();
            k.Root = new VisualElement { pickingMode = PickingMode.Ignore, usageHints = UsageHints.DynamicTransform }; k.Root.AddToClassList("hud-skull");
            k.Icon = new VisualElement { pickingMode = PickingMode.Ignore }; k.Icon.AddToClassList("hud-skull__icon");
            k.Icon.style.backgroundImage = new StyleBackground(tex);
            k.Count = new Label { pickingMode = PickingMode.Ignore }; k.Count.AddToClassList("hud-skull__count");
            k.Root.Add(k.Icon); k.Root.Add(k.Count);
            layer.Add(k.Root);
            k.On = true;
            return k;
        }

        Skull Pop() { var k = spare[spare.Count - 1]; spare.RemoveAt(spare.Count - 1); k.ShownN = -1; return k; }

        void Recycle(int i) { var k = live[i]; live.RemoveAt(i); Show(k, false); spare.Add(k); }

        static readonly string[] timesCache = new string[256];
        static string Times(int n) => n < timesCache.Length ? timesCache[n] ??= "×" + n : "×" + n;

        /// <summary>
        /// The skull: a cranium and a jaw, two eye sockets, a nose and three tooth gaps cut out; white with a soft dark rim,
        /// on clear. Texture rows run bottom-up, so +v is up.
        /// </summary>
        public static Texture2D SkullTexture(int n)
        {
            var t = new Texture2D(n, n, TextureFormat.RGBA32, true) { wrapMode = TextureWrapMode.Clamp, filterMode = FilterMode.Trilinear, hideFlags = HideFlags.HideAndDontSave };
            var px = new Color32[n * n];
            float aa = 1.2f / n, rim = 2.2f / n;
            for (int y = 0; y < n; y++) for (int x = 0; x < n; x++)
            {
                float u = (x + 0.5f) / n - 0.5f, v = (y + 0.5f) / n - 0.5f;
                // the head's silhouette with its rim; inside it, bone except in the sockets, which are dark, not holes
                float head = HeadSdf(u, v), holes = HolesSdf(u, v);
                float inHead = Mathf.Clamp01(0.5f - head / aa);
                float bone = inHead * Mathf.Clamp01(0.5f + holes / aa);
                float shadow = Mathf.Clamp01(0.5f - (head - rim) / aa) * 0.75f;
                float a = inHead + shadow * (1f - inHead);
                byte c = (byte)Mathf.RoundToInt(a > 0f ? 255f * bone / a : 0f);
                px[y * n + x] = new Color32(c, c, c, (byte)Mathf.RoundToInt(a * 255f));
            }
            t.SetPixels32(px); t.Apply(true, true);
            return t;
        }

        /// <summary>Signed distance to the skull's silhouette (negative inside), in a unit box centred on 0 with +v up.</summary>
        public static float HeadSdf(float u, float v)
        {
            float cranium = new Vector2(u, v - 0.07f).magnitude - 0.31f;
            float jaw = Box(u, v + 0.22f, 0.17f, 0.11f) - 0.04f;
            return SmoothMin(cranium, jaw, 0.06f);
        }

        /// <summary>Signed distance to the dark parts inside it: the eye sockets, the nose and three gaps between the teeth.</summary>
        public static float HolesSdf(float u, float v)
        {
            float eyes = Mathf.Min(new Vector2(u - 0.13f, v + 0.005f).magnitude - 0.095f, new Vector2(u + 0.13f, v + 0.005f).magnitude - 0.095f);
            float nose = Box(u, v + 0.11f, 0.028f, 0.035f) - 0.012f;
            float teeth = 1e9f;
            for (int k = -1; k <= 1; k++) teeth = Mathf.Min(teeth, Box(u - k * 0.075f, v + 0.29f, 0.013f, 0.05f));
            return Mathf.Min(eyes, Mathf.Min(nose, teeth));
        }

        static float Box(float x, float y, float hx, float hy)
        {
            float dx = Mathf.Abs(x) - hx, dy = Mathf.Abs(y) - hy;
            return new Vector2(Mathf.Max(dx, 0f), Mathf.Max(dy, 0f)).magnitude + Mathf.Min(Mathf.Max(dx, dy), 0f);
        }

        static float SmoothMin(float a, float b, float k)
        {
            float h = Mathf.Clamp01(0.5f + 0.5f * (b - a) / k);
            return Mathf.Lerp(b, a, h) - k * h * (1f - h);
        }
    }
}
