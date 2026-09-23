// Phase: B6 (implemented) — the four orders on every trench the player owns, anchored where the trench is on screen.
// Port of BattleHud.cs 409-469: [fall back][lock] gap [hold fire][over the top] with the garrison count beneath, one
// cluster per owned trench, anchored on the trench below the middle of the view so it follows pan and zoom, culled
// where neighbours would overlap (the trench nearest the enemy keeps its buttons) and where it would run off the
// screen. Clusters are pooled at build (one per trench in the map) and moved by transform, which invalidates no layout.
using System;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Sim;
using TW.Sim.Nav;

namespace TW.UI
{
    public sealed class TrenchOrderCluster
    {
        sealed class Cluster
        {
            public VisualElement Holder, Root;
            public Button Fallback, Lock, HoldFire, Advance;
            public VisualElement LockIcon, FireIcon;
            public Label Garrison;
            public int Trench;
            public bool Shown = true, LastLocked, LastHeld, LastManned = true;
            public int LastCount = -1;
            public float LastX = float.NaN, LastY;
        }

        readonly SimHost host;
        readonly TacticalCamera cam;
        readonly HudTooltip tooltip;
        readonly VisualElement layer;
        readonly List<Cluster> pool = new List<Cluster>();
        readonly List<Vector3> anchors = new List<Vector3>(8);   // x, y = panel, z = trench index
        readonly Action<CommandType, int, int> issue;
        public bool Interactive = true;

        public TrenchOrderCluster(VisualElement ordersLayer, VisualTreeAsset template, SimHost host, TacticalCamera cam, HudTooltip tooltip, Action<CommandType, int, int> issue)
        {
            layer = ordersLayer; this.host = host; this.cam = cam; this.tooltip = tooltip; this.issue = issue;
            int n = host.Local.Fields.Trenches.Length;
            for (int t = 0; t < n; t++) pool.Add(Make(template, t));
        }

        Cluster Make(VisualTreeAsset template, int trench)
        {
            var c = new Cluster { Trench = trench };
            c.Holder = template != null ? template.Instantiate() : Fallback();
            c.Holder.style.position = Position.Absolute; c.Holder.style.left = 0; c.Holder.style.top = 0;
            c.Holder.pickingMode = PickingMode.Ignore;
            layer.Add(c.Holder);
            c.Root = c.Holder.Q("orders") ?? c.Holder;
            c.Fallback = c.Holder.Q<Button>("fallback"); c.Lock = c.Holder.Q<Button>("lock");
            c.HoldFire = c.Holder.Q<Button>("holdfire"); c.Advance = c.Holder.Q<Button>("advance");
            c.LockIcon = c.Holder.Q("lock-icon"); c.FireIcon = c.Holder.Q("holdfire-icon");
            c.Garrison = c.Holder.Q<Label>("garrison");
            foreach (var b in new[] { c.Fallback, c.Lock, c.HoldFire, c.Advance }) if (b != null) b.focusable = false;
            int t = trench;
            c.Fallback.clicked += () => issue(CommandType.TrenchFallback, t, 0);
            c.Advance.clicked += () => issue(CommandType.TrenchAdvance, t, 0);
            c.Lock.clicked += () => issue(CommandType.TrenchLock, t, host.Local.Fields.Trenches[t].Locked != 0 ? 0 : 1);
            c.HoldFire.clicked += () => issue(CommandType.TrenchHoldFire, t, host.Local.Fields.Trenches[t].HoldFire != 0 ? 0 : 1);
            if (tooltip != null)
            {
                tooltip.Attach(c.Fallback, "FALL BACK", HudText.FallbackTip);
                tooltip.Attach(c.Advance, "OVER THE TOP", HudText.AdvanceTip);
                tooltip.Attach(c.Lock, () => host.Local.Fields.Trenches[t].Locked != 0 ? "LOCKED" : "OPEN", () => host.Local.Fields.Trenches[t].Locked != 0 ? HudText.LockedTip : HudText.OpenTip);
                tooltip.Attach(c.HoldFire, () => host.Local.Fields.Trenches[t].HoldFire != 0 ? "HOLDING FIRE" : "FIRING AT WILL", () => host.Local.Fields.Trenches[t].HoldFire != 0 ? HudText.HoldingTip : HudText.FiringTip);
            }
            Hide(c);
            return c;
        }

        static VisualElement Fallback()
        {
            var holder = new VisualElement();
            var root = new VisualElement { name = "orders", pickingMode = PickingMode.Ignore }; root.AddToClassList("hud-orders"); holder.Add(root);
            var row = new VisualElement { name = "row", pickingMode = PickingMode.Ignore }; row.AddToClassList("hud-orders__row"); root.Add(row);
            Button B(string n, string ico) { var b = new Button { name = n }; b.AddToClassList("tw-btn"); b.AddToClassList("tw-btn--icon"); b.AddToClassList("hud-orders__btn"); var i = new VisualElement { name = n + "-icon", pickingMode = PickingMode.Ignore }; i.AddToClassList("tw-icon"); i.AddToClassList(ico); b.Add(i); row.Add(b); return b; }
            B("fallback", "tw-ico-fallback"); B("lock", "tw-ico-lock-open");
            var gap = new VisualElement { name = "gap", pickingMode = PickingMode.Ignore }; gap.AddToClassList("hud-orders__gap"); row.Add(gap);
            B("holdfire", "tw-ico-fireatwill"); B("advance", "tw-ico-overthetop");
            var g = new Label { name = "garrison", pickingMode = PickingMode.Ignore }; g.AddToClassList("tw-badge"); g.AddToClassList("hud-orders__count"); root.Add(g);
            return holder;
        }

        static void Hide(Cluster c) { if (!c.Shown) return; c.Holder.style.display = DisplayStyle.None; c.Shown = false; }
        static void Show(Cluster c) { if (c.Shown) return; c.Holder.style.display = DisplayStyle.Flex; c.Shown = true; }

        public void HideAll() { foreach (var c in pool) Hide(c); }

        /// <summary>Called once per frame after the camera has moved (execution order), with the panel to convert into.</summary>
        public void Refresh(IPanel panel, Camera unityCam, float panelW, float panelH, bool over)
        {
            if (panel == null || unityCam == null || over || !Interactive) { HideAll(); return; }
            var fields = host.Local.Fields;
            var map = host.Local.Map;
            float focusX = cam != null ? cam.Focus.x : map.SizeMeters.x * 0.5f;
            const float b = HudLayout.OrderBtnPx;
            float half = HudLayout.ClusterWidthPx * 0.5f;
            // zoomed out, neighbouring trenches' buttons would overlap: the trench nearest the enemy keeps its buttons
            anchors.Clear();
            for (int t = fields.Trenches.Length - 1; t >= 0; t--)
            {
                if (fields.Trenches[t].OwnerTeam != 0 || map.Trenches[t].CellCount == 0) continue;
                float z = map.NavCellCenter(map.TrenchCells[map.Trenches[t].CellStart]).z + 1f;
                Vector3 s = unityCam.WorldToScreenPoint(new Vector3(focusX + 9f, 0f, z));
                if (s.z <= 0f) continue;
                Vector2 p = RuntimePanelUtils.ScreenToPanel(panel, new Vector2(s.x, Screen.height - s.y)) / HudLayout.HudScale;   // panel -> HUD space
                anchors.Add(new Vector3(p.x, p.y, t));
            }
            anchors.Sort((p, q) => q.x.CompareTo(p.x));   // the enemy is on screen-right
            for (int k = anchors.Count - 1; k >= 0; k--)
                for (int j = 0; j < k; j++)
                    if (Mathf.Abs(anchors[j].x - anchors[k].x) < HudLayout.ClusterOverlapPx) { anchors.RemoveAt(k); break; }

            for (int t = 0; t < pool.Count; t++)
            {
                var c = pool[t];
                int a = -1;
                for (int k = 0; k < anchors.Count; k++) if ((int)anchors[k].z == t) { a = k; break; }
                if (a < 0) { Hide(c); continue; }
                float px = anchors[a].x, py = anchors[a].y;
                if (px < half || px > panelW - half) { Hide(c); continue; }
                float by = Mathf.Clamp(py, HudLayout.ClusterTopMinPx, panelH - HudLayout.BarHeightPx - b - HudLayout.ClusterBottomMarginPx);
                float x = px - half;
                if (float.IsNaN(c.LastX) || Mathf.Abs(x - c.LastX) > 0.25f || Mathf.Abs(by - c.LastY) > 0.25f)
                {
                    c.Holder.transform.position = new Vector3(x, by, 0f);
                    c.LastX = x; c.LastY = by;
                }
                var ts = fields.Trenches[t];
                bool locked = ts.Locked != 0, held = ts.HoldFire != 0, manned = ts.GarrisonCount > 0;
                if (locked != c.LastLocked || float.IsNaN(c.LastX) || !c.Shown)
                {
                    c.LockIcon?.EnableInClassList("tw-ico-lock-closed", locked); c.LockIcon?.EnableInClassList("tw-ico-lock-open", !locked);
                    c.Lock.EnableInClassList("tw-btn--on", locked); c.LastLocked = locked;
                }
                if (held != c.LastHeld || !c.Shown)
                {
                    c.FireIcon?.EnableInClassList("tw-ico-holdfire", held); c.FireIcon?.EnableInClassList("tw-ico-fireatwill", !held);
                    c.HoldFire.EnableInClassList("tw-btn--on", held); c.LastHeld = held;
                }
                if (manned != c.LastManned || !c.Shown) { c.Fallback.SetEnabled(manned); c.Advance.SetEnabled(manned); c.LastManned = manned; }
                if (ts.GarrisonCount != c.LastCount) { c.Garrison.text = MenText(ts.GarrisonCount); c.LastCount = ts.GarrisonCount; }
                Show(c);
            }
        }

        static readonly string[] menCache = new string[512];
        static string MenText(int n)
        {
            if (n < 0 || n >= menCache.Length) return n + " MEN";
            return menCache[n] ??= n + " MEN";
        }
    }
}
