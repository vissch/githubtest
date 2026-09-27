// Phase: B6 (implemented) — what selection looks like on the field, as Dust Front draws it (its developer's GIFs,
// 2026-09-23 research): under each selected unit a flat hexagon of two brackets with gaps at two corners, squashed by
// the camera's pitch as if lying on the ground; above it a short health bar, dark track and red fill.
// Drawn on the HUD, not in the world (round 15 capture: world markers sank under the duckboards and behind the trench
// walls, and URP Unlit cannot switch its depth test off; a world bar also thinned to a hair when zoomed out). So each
// mark is a pooled pair of HUD elements placed from the projected feet and head every frame: always visible, crisp at
// any zoom, the bracket's size from the unit's projected footprint (never smaller than MinHexPx), the bar a fixed
// width. Kinds: ours (off-white), theirs (rust), hover (faint), danger (alarm red: ours under our own strike, or pinned
// men who will stay when over the top is hovered), go (amber: the men over the top would send).
// Moved by transform only, so nothing here re-lays-out the HUD; hidden marks stay in the pool.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UIElements;

namespace TW.UI
{
    public sealed class SelectionMarkers : System.IDisposable
    {
        public enum Kind : byte { Ours, Theirs, Hover, Danger, Go }

        public static readonly Color Ours = new Color32(0xE8, 0xE1, 0xD2, 255);      // --tw-text-bright
        public static readonly Color Theirs = new Color32(0xC8, 0x5A, 0x3E, 255);    // --tw-enemy, lifted a little to read at 2 px
        public static readonly Color Hover = new Color32(0xE8, 0xE1, 0xD2, 120);
        public static readonly Color Danger = new Color32(0xE0, 0x2B, 0x2B, 255);    // --tw-alarm
        public static readonly Color Go = new Color32(0xF2, 0xA6, 0x48, 255);        // --tw-accent-bright: would go over the top
        public const int HexTexPx = 128;
        public const float HexBoxPx = 64f, MinHexPx = 26f, MaxHexPx = 220f, ManBarPx = 30f, VehicleBarPx = 56f, BarHeightPx = 6f, BarGapPx = 5f;
        public const float HeadM = 1.9f;   // the drawn man's helmet over his feet, per unit of figure scale (CombatFx puts the chest at 1.2)

        sealed class Mark
        {
            public VisualElement Root, Hex, Bar, Fill;
            public Kind Kind = (Kind)255; public int Pct = -1; public bool BarOn = true, Vehicle, On = true, Enemy;
        }

        readonly VisualElement layer;
        readonly System.Func<Vector2, Vector2> toHud;
        readonly Texture2D hex;
        readonly List<Mark> pool = new List<Mark>(64);
        int used;
        Camera cam;

        /// <param name="toHud">Screen px (bottom-left origin) to the HUD's layout px, as SelectionController.ToHud.</param>
        public SelectionMarkers(VisualElement root, System.Func<Vector2, Vector2> toHud)
        {
            layer = root?.Q("markers-layer");
            this.toHud = toHud;
            hex = HexBrackets(HexTexPx);
            if (layer != null) layer.pickingMode = PickingMode.Ignore;
        }

        public void Dispose()
        {
            layer?.Clear(); pool.Clear();
            if (hex != null) Object.Destroy(hex);
        }

        public void Begin(Camera c) { cam = c; used = 0; }

        /// <summary>One mark this frame: its bracket, and a bar at hp (0..1) unless hp is negative.</summary>
        public void Add(ScreenUnit u, Kind kind, float hp)
        {
            if (layer == null || cam == null) return;
            // feet and head in the world: the body centre the picker projects, less half a figure, and a helmet over it
            float scale = u.Vehicle ? 1f : u.Size / 1.1f;   // UnitPicker: Size = 1.1 x figure scale for a man
            float body = u.Vehicle ? UnitPicker.VehicleBodyM : UnitPicker.ManBodyM * scale;
            Vector3 feet = new Vector3(u.World.x, u.World.y - body, u.World.z);
            Vector3 head = u.Vehicle ? feet + Vector3.up * 2.6f : feet + Vector3.up * (HeadM * scale);
            Vector3 sf = cam.WorldToScreenPoint(feet);
            if (sf.z <= cam.nearClipPlane) return;
            // the hexagon lies on the ground: its screen width from the camera's right, its height from the ground ahead
            float r = u.Size * 0.5f;
            Vector3 right = cam.transform.right; right.y = 0f; right = right.sqrMagnitude > 1e-6f ? right.normalized : Vector3.right;
            Vector3 ahead = Vector3.Cross(right, Vector3.up);
            Vector2 pf = toHud(sf);
            Vector2 pr = toHud(cam.WorldToScreenPoint(feet + right * r)), pa = toHud(cam.WorldToScreenPoint(feet + ahead * r));
            float w = 2f * (pr - pf).magnitude, h = 2f * Mathf.Abs((pa - pf).y);
            h = Mathf.Max(h, w * 0.25f);   // never flatter than a quarter, or it vanishes when the camera lies low
            float grow = w < MinHexPx ? MinHexPx / Mathf.Max(1f, w) : w > MaxHexPx ? MaxHexPx / w : 1f;
            w *= grow; h *= grow;

            var m = Take();
            m.Root.transform.position = new Vector3(pf.x, pf.y, 0f);
            m.Hex.transform.scale = new Vector3(w / HexBoxPx, h / HexBoxPx, 1f);
            if (m.Kind != kind)
            {
                m.Kind = kind;
                m.Hex.style.unityBackgroundImageTintColor = kind == Kind.Ours ? Ours : kind == Kind.Theirs ? Theirs : kind == Kind.Hover ? Hover : kind == Kind.Go ? Go : Danger;
            }
            if (m.Enemy != !u.Ours) { m.Enemy = !u.Ours; m.Fill.EnableInClassList("hud-mark__fill--enemy", m.Enemy); }
            bool bar = hp >= 0f;
            if (bar != m.BarOn) { m.BarOn = bar; m.Bar.style.display = bar ? DisplayStyle.Flex : DisplayStyle.None; }
            if (!bar) return;
            if (u.Vehicle != m.Vehicle) { m.Vehicle = u.Vehicle; m.Bar.style.width = u.Vehicle ? VehicleBarPx : ManBarPx; }
            int pct = Mathf.Clamp(Mathf.CeilToInt(hp * 100f), 0, 100);
            if (pct != m.Pct) { m.Pct = pct; m.Fill.style.width = Length.Percent(pct); }
            // over the head, or over the bracket's far edge if the camera looks so steeply down that the head is lower
            Vector2 ph = toHud(cam.WorldToScreenPoint(head));
            float top = Mathf.Min(ph.y, pf.y - h * 0.5f) - BarGapPx - BarHeightPx - pf.y;
            m.Bar.transform.position = new Vector3(-(u.Vehicle ? VehicleBarPx : ManBarPx) * 0.5f + (ph.x - pf.x), top, 0f);
        }

        /// <summary>Hide whatever this frame did not use.</summary>
        public void End()
        {
            for (int i = used; i < pool.Count; i++)
            {
                var m = pool[i];
                if (m.On) { m.On = false; m.Root.style.display = DisplayStyle.None; }
            }
        }

        Mark Take()
        {
            if (used < pool.Count)
            {
                var m = pool[used++];
                if (!m.On) { m.On = true; m.Root.style.display = DisplayStyle.Flex; }
                return m;
            }
            var n = new Mark();
            n.Root = New("hud-mark", layer);
            n.Hex = New("hud-mark__hex", n.Root);
            n.Hex.style.backgroundImage = new StyleBackground(hex);
            n.Bar = New("hud-mark__bar", n.Root);
            n.Fill = New("hud-mark__fill", n.Bar);
            pool.Add(n); used++;
            return n;
        }

        static VisualElement New(string cls, VisualElement parent)
        {
            var e = new VisualElement { pickingMode = PickingMode.Ignore, usageHints = UsageHints.DynamicTransform };
            e.AddToClassList(cls); parent.Add(e); return e;
        }

        /// <summary>
        /// The bracket texture: a flat-topped hexagon outline, white with a soft dark rim (so it reads on snow and on
        /// mud alike), with gaps at the top-right and bottom-left corners so it reads as two brackets.
        /// </summary>
        public static Texture2D HexBrackets(int n)
        {
            var t = new Texture2D(n, n, TextureFormat.RGBA32, true) { wrapMode = TextureWrapMode.Clamp, filterMode = FilterMode.Trilinear, hideFlags = HideFlags.HideAndDontSave };
            var px = new Color32[n * n];
            float r = 0.42f, thick = 1f / 22f, rim = 1f / 24f;
            for (int y = 0; y < n; y++) for (int x = 0; x < n; x++)
            {
                float u = (x + 0.5f) / n - 0.5f, v = (y + 0.5f) / n - 0.5f;
                float d = Mathf.Abs(HexSdf(u, v, r));
                float ink = Mathf.Clamp01((thick - d) * n * 0.9f);
                float shadow = Mathf.Clamp01((thick + rim - d) * n * 0.35f) * 0.6f;
                float ang = Mathf.Atan2(v, u) * Mathf.Rad2Deg; if (ang < 0f) ang += 360f;
                float gap = Mathf.Min(Mathf.Abs(Mathf.DeltaAngle(ang, 60f)), Mathf.Abs(Mathf.DeltaAngle(ang, 240f)));
                float keep = Mathf.Clamp01((gap - 14f) / 3f);   // the two gaps, with soft ends
                ink *= keep; shadow *= keep;
                // white over a dark rim: colour is white where inked, black in the rim; alpha is the union
                float a = ink + shadow * (1f - ink);
                byte c = (byte)Mathf.RoundToInt(a > 0f ? 255f * ink / a : 0f);
                px[y * n + x] = new Color32(c, c, c, (byte)Mathf.RoundToInt(a * 255f));
            }
            t.SetPixels32(px); t.Apply(true, true);
            return t;
        }

        /// <summary>Signed distance to a flat-topped regular hexagon of circumradius r (negative inside).</summary>
        public static float HexSdf(float x, float y, float r)
        {
            // Inigo Quilez's hexagon (flat top and bottom at +-apothem, vertices at 0, 60, ... degrees); the apothem is r cos 30
            const float k0 = -0.8660254f, k1 = 0.5f, k2 = 0.57735027f;
            float px = Mathf.Abs(x), py = Mathf.Abs(y);
            float dot = 2f * Mathf.Min(k0 * px + k1 * py, 0f);
            px -= dot * k0; py -= dot * k1;
            float a = r * 0.8660254f;
            px -= Mathf.Clamp(px, -k2 * a, k2 * a); py -= a;
            return new Vector2(px, py).magnitude * Mathf.Sign(py);
        }
    }
}
