// Phase: B6 (implemented) — the four orders on every trench the player owns, anchored where the trench is on screen.
// Port of BattleHud.cs 409-469: [fall back][lock] gap [categories][over the top] with the garrison count beneath, one
// cluster per owned trench, anchored on the trench below the middle of the view so it follows pan and zoom, culled
// where neighbours would overlap (the trench nearest the enemy keeps its buttons) and where it would run off the
// screen. Clusters are pooled at build (one per trench in the map) and moved by transform, which invalidates no layout.
// The men badge carries the garrison's morale (GarrisonStats: amber shaken, red half pinned) and answers the cursor:
// resting on it shows the garrison's card (SelectionController reads HoveredTrench); the over-the-top tooltip counts
// the pinned men who will stay behind.
// The troop-category chips (owner, 2026-09-24; they took the hold-fire button's place): one per infantry type in the
// trench, face and count, a red pip when some are pinned, a bright rim when that category of this trench is selected.
// Click selects that category, Shift+click adds or drops it, a double click also goes there. With a selection scoped
// to the trench (TrenchScope), over the top sends only those categories (TrenchSelectAdvance). Hold fire is the small
// tag beside the men badge now (click or F).
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
            public Button Fallback, Lock, Advance;
            public VisualElement LockIcon, HoldTag, Cats;
            public Label Garrison;
            public readonly List<CatChip> Chips = new List<CatChip>(4);
            public int Trench, LastMask = -1;
            public bool Shown = true, LastLocked, LastHeld = true, LastManned = true;
            public GarrisonStats.Morale LastMorale;
            public int LastCount = -1;
            public float LastX = float.NaN, LastY;
        }

        sealed class CatChip
        {
            public VisualElement Root, Face, Pip; public Label Count;
            public int Archetype = -1, Shown = -1, Pinned = -1; public Mood Mood = (Mood)255; public bool Selected, On = true;
        }

        public const int MaxChips = 8;
        /// <summary>A chip and its gap; the chips took the place of the hold-fire button and its gap.</summary>
        public const float ChipPitchPx = 44f + HudLayout.OrderGapPx, ReplacedPx = HudLayout.OrderBtnPx + HudLayout.OrderGapPx;

        /// <summary>How far the chips push a trench's cluster past the fixed ClusterWidthPx.</summary>
        float ExtraWidth(int t)
        {
            if (garrison == null || t >= garrison.Trenches) return 0f;
            int n = 0;
            for (int a = 0; a <= TrenchScope.MaxMaskArchetype && n < MaxChips; a++) if (garrison.TypeCount(t, a) > 0) n++;
            return Mathf.Max(0f, n * ChipPitchPx - ReplacedPx);
        }

        /// <summary>What the chips and the advance ask of the selection (SelectionController answers).</summary>
        public interface ISelection
        {
            /// <summary>The mask over the top should send for trench t: 0 = everyone (no scoped selection, or it covers all).</summary>
            int AdvanceMask(int trench);
            bool CategorySelected(int trench, int archetype);
            void SelectCategory(int trench, int archetype, bool toggle, bool focus);
        }

        /// <summary>Set by the HUD once the selection exists.</summary>
        public ISelection Selection;
        /// <summary>The category chip under the cursor (trench, archetype), or (-1, -1): the hover card shows that category.</summary>
        public int HoveredCatTrench { get; private set; } = -1;
        public int HoveredCatArchetype { get; private set; } = -1;
        /// <summary>The trench whose over-the-top button is under the cursor, or -1: the field shows who would go.</summary>
        public int HoveredAdvanceTrench { get; private set; } = -1;

        readonly SimHost host;
        readonly TacticalCamera cam;
        readonly HudTooltip tooltip;
        readonly VisualElement layer;
        readonly List<Cluster> pool = new List<Cluster>();
        readonly List<Vector3> anchors = new List<Vector3>(8);   // x, y = panel, z = trench index
        readonly Action<CommandType, int, int> issue;
        readonly GarrisonStats garrison;
        public bool Interactive = true;
        /// <summary>The trench whose men badge is under the cursor, or -1.</summary>
        public int HoveredTrench { get; private set; } = -1;

        public TrenchOrderCluster(VisualElement ordersLayer, VisualTreeAsset template, SimHost host, TacticalCamera cam, HudTooltip tooltip, Action<CommandType, int, int> issue, GarrisonStats garrison = null)
        {
            layer = ordersLayer; this.host = host; this.cam = cam; this.tooltip = tooltip; this.issue = issue; this.garrison = garrison;
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
            c.Advance = c.Holder.Q<Button>("advance");
            c.LockIcon = c.Holder.Q("lock-icon"); c.HoldTag = c.Holder.Q("holdfire-tag"); c.Cats = c.Holder.Q("categories");
            c.Garrison = c.Holder.Q<Label>("garrison");
            if (c.Garrison != null)
            {
                c.Garrison.pickingMode = PickingMode.Position;   // the cursor may rest on it for the garrison card
                c.Garrison.RegisterCallback<PointerEnterEvent>(_ => HoveredTrench = trench);
                c.Garrison.RegisterCallback<PointerLeaveEvent>(_ => { if (HoveredTrench == trench) HoveredTrench = -1; });
            }
            foreach (var b in new[] { c.Fallback, c.Lock, c.Advance }) if (b != null) b.focusable = false;
            int t = trench;
            c.Fallback.clicked += () => issue(CommandType.TrenchFallback, t, 0);
            c.Advance.clicked += () =>
            {
                int mask = Selection != null ? Selection.AdvanceMask(t) : 0;
                if (mask != 0) issue(CommandType.TrenchSelectAdvance, t, mask); else issue(CommandType.TrenchAdvance, t, 0);
            };
            c.Lock.clicked += () => issue(CommandType.TrenchLock, t, host.Local.Fields.Trenches[t].Locked != 0 ? 0 : 1);
            c.Advance.RegisterCallback<PointerEnterEvent>(_ => HoveredAdvanceTrench = t);
            c.Advance.RegisterCallback<PointerLeaveEvent>(_ => { if (HoveredAdvanceTrench == t) HoveredAdvanceTrench = -1; });
            if (c.HoldTag != null)
            {
                c.HoldTag.pickingMode = PickingMode.Position;
                c.HoldTag.RegisterCallback<ClickEvent>(_ => issue(CommandType.TrenchHoldFire, t, host.Local.Fields.Trenches[t].HoldFire != 0 ? 0 : 1));
            }
            if (tooltip != null)
            {
                tooltip.Attach(c.Fallback, "FALL BACK", HudText.FallbackTip);
                tooltip.Attach(c.Advance, () => AdvanceHeading(t), () => AdvanceText(t));
                tooltip.Attach(c.Lock, () => host.Local.Fields.Trenches[t].Locked != 0 ? "LOCKED" : "OPEN", () => host.Local.Fields.Trenches[t].Locked != 0 ? HudText.LockedTip : HudText.OpenTip);
                if (c.HoldTag != null)
                    tooltip.Attach(c.HoldTag, () => host.Local.Fields.Trenches[t].HoldFire != 0 ? "HOLDING FIRE" : "FIRING AT WILL",
                                   () => (host.Local.Fields.Trenches[t].HoldFire != 0 ? HudText.HoldingTip : HudText.FiringTip) + ". Click or F to switch");
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
            var cats = new VisualElement { name = "categories", pickingMode = PickingMode.Ignore }; cats.AddToClassList("hud-orders__cats"); row.Add(cats);
            B("advance", "tw-ico-overthetop");
            var countRow = new VisualElement { name = "count-row", pickingMode = PickingMode.Ignore }; countRow.AddToClassList("hud-orders__count-row"); root.Add(countRow);
            var g = new Label { name = "garrison", pickingMode = PickingMode.Ignore }; g.AddToClassList("tw-badge"); g.AddToClassList("hud-orders__count"); countRow.Add(g);
            var tag = new VisualElement { name = "holdfire-tag" }; tag.AddToClassList("tw-icon"); tag.AddToClassList("tw-ico-holdfire"); tag.AddToClassList("hud-orders__holdtag"); countRow.Add(tag);
            return holder;
        }

        void Hide(Cluster c)
        {
            if (!c.Shown) return;
            c.Holder.style.display = DisplayStyle.None; c.Shown = false;
            if (HoveredTrench == c.Trench) HoveredTrench = -1;   // a hidden badge gets no PointerLeave
            if (HoveredCatTrench == c.Trench) { HoveredCatTrench = -1; HoveredCatArchetype = -1; }
            if (HoveredAdvanceTrench == c.Trench) HoveredAdvanceTrench = -1;
        }

        // ---- the troop-category chips -------------------------------------------------------------------------------
        CatChip MakeChip(Cluster c)
        {
            var k = new CatChip();
            k.Root = new VisualElement { focusable = false }; k.Root.AddToClassList("hud-cat-chip");
            k.Face = new VisualElement { pickingMode = PickingMode.Ignore }; k.Face.AddToClassList("hud-cat-chip__face"); k.Root.Add(k.Face);
            k.Count = new Label { pickingMode = PickingMode.Ignore }; k.Count.AddToClassList("hud-cat-chip__count"); k.Root.Add(k.Count);
            k.Pip = new VisualElement { pickingMode = PickingMode.Ignore }; k.Pip.AddToClassList("hud-cat-chip__pip"); k.Root.Add(k.Pip);
            int t = c.Trench;
            k.Root.RegisterCallback<ClickEvent>(e =>
            {
                if (k.Archetype < 0 || Selection == null) return;
                Selection.SelectCategory(t, k.Archetype, e.shiftKey, e.clickCount >= 2);
                e.StopPropagation();
            });
            k.Root.RegisterCallback<PointerEnterEvent>(_ => { HoveredCatTrench = t; HoveredCatArchetype = k.Archetype; });
            k.Root.RegisterCallback<PointerLeaveEvent>(_ => { if (HoveredCatTrench == t && HoveredCatArchetype == k.Archetype) { HoveredCatTrench = -1; HoveredCatArchetype = -1; } });
            c.Cats.Add(k.Root);
            c.Chips.Add(k);
            return k;
        }

        void RefreshChips(Cluster c)
        {
            if (c.Cats == null || garrison == null || c.Trench >= garrison.Trenches) return;
            int t = c.Trench, used = 0;
            for (int a = 0; a <= TrenchScope.MaxMaskArchetype && used < MaxChips; a++)
            {
                int n = garrison.TypeCount(t, a);
                if (n == 0) continue;
                var k = used < c.Chips.Count ? c.Chips[used] : MakeChip(c);
                used++;
                if (!k.On) { k.On = true; k.Root.style.display = DisplayStyle.Flex; }
                float hp = garrison.TypeMeanHp(t, a);
                var mood = hp > 0.6f ? Mood.Neutral : hp > 0.3f ? Mood.Wounded : Mood.Critical;
                if (k.Archetype != a || k.Mood != mood)
                {
                    if (k.Archetype != a && HoveredCatTrench == t && HoveredCatArchetype == k.Archetype) HoveredCatArchetype = a;   // the chip under the cursor now shows another type
                    k.Archetype = a; k.Mood = mood;
                    string name = UnitArt.NameOf((byte)a);
                    var tex = UnitArt.HasFace(name) ? UnitArt.State(name, mood) : UnitArt.Full(name);
                    k.Face.style.backgroundImage = tex != null ? new StyleBackground(tex) : new StyleBackground(StyleKeyword.None);
                }
                if (n != k.Shown) { k.Shown = n; k.Count.text = CountText(n); }
                int pinned = garrison.TypePinned(t, a);
                if ((pinned > 0) != (k.Pinned > 0)) k.Root.EnableInClassList("hud-cat-chip--pinned", pinned > 0);
                k.Pinned = pinned;
                bool sel = Selection != null && Selection.CategorySelected(t, a);
                if (sel != k.Selected) { k.Selected = sel; k.Root.EnableInClassList("hud-cat-chip--selected", sel); }
            }
            for (int i = used; i < c.Chips.Count; i++)
            {
                var k = c.Chips[i];
                if (k.On)
                {
                    k.On = false; k.Root.style.display = DisplayStyle.None;
                    if (HoveredCatTrench == t && HoveredCatArchetype == k.Archetype) { HoveredCatTrench = -1; HoveredCatArchetype = -1; }
                }
                k.Archetype = -1;
            }
        }

        static readonly string[] countCache = new string[256];
        static string CountText(int n) => n >= 0 && n < countCache.Length ? countCache[n] ??= n.ToString() : n.ToString();

        string AdvanceHeading(int t) => Selection != null && Selection.AdvanceMask(t) != 0 ? "OVER THE TOP: ONLY THE SELECTED" : "OVER THE TOP";

        /// <summary>Who a masked advance sends: the whole categories (the sim moves by type), less the pinned.</summary>
        string MaskedText(int t, int mask)
        {
            var names = new System.Text.StringBuilder();
            int go = 0, stay = 0;
            for (int a = 0; a <= TrenchScope.MaxMaskArchetype; a++)
            {
                if ((mask & (1 << a)) == 0) continue;
                int n = garrison.TypeCount(t, a), p = garrison.TypePinned(t, a);
                if (n == 0) continue;
                if (names.Length > 0) names.Append(", ");
                names.Append(HudText.Name((byte)a));
                go += n - p; stay += p;
            }
            return "Only " + names + " go, every one of them in this trench: " + go + " men" + (stay > 0 ? ", " + stay + " pinned stay" : "");
        }

        string AdvanceText(int t)
        {
            if (garrison == null || t >= garrison.Trenches) return HudText.AdvanceTip;
            int mask = Selection != null ? Selection.AdvanceMask(t) : 0;
            if (mask != 0) return MaskedText(t, mask);
            int pinned = garrison.CountOf(t, UnitState.Pinned), men = garrison.Men(t);
            if (pinned == 0) return HudText.AdvanceTip;
            return HudText.AdvanceTip + ". " + (pinned >= men ? "All " + men + " are pinned: nobody will go" : pinned + " of " + men + " are pinned and will stay");
        }
        static void Show(Cluster c) { if (c.Shown) return; c.Holder.style.display = DisplayStyle.Flex; c.Shown = true; }

        public void HideAll() { foreach (var c in pool) Hide(c); }

        /// <summary>Called once per frame after the camera has moved (execution order), with the panel to convert into.</summary>
        public void Refresh(IPanel panel, Camera unityCam, float panelW, float panelH, bool over)
        {
            if (panel == null || unityCam == null || over || !Interactive) { HideAll(); return; }
            var fields = host.Local.Fields;
            garrison?.Refresh(host.Local.World, fields.Trenches.Length);
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
                    if (Mathf.Abs(anchors[j].x - anchors[k].x) < HudLayout.ClusterOverlapPx + ExtraWidth((int)anchors[k].z)) { anchors.RemoveAt(k); break; }   // k is left of j: its chips reach towards j

            for (int t = 0; t < pool.Count; t++)
            {
                var c = pool[t];
                int a = -1;
                for (int k = 0; k < anchors.Count; k++) if ((int)anchors[k].z == t) { a = k; break; }
                if (a < 0) { Hide(c); continue; }
                float px = anchors[a].x, py = anchors[a].y;
                if (px < half || px > panelW - half - ExtraWidth(t)) { Hide(c); continue; }
                float by = Mathf.Clamp(py, HudLayout.ClusterTopMinPx, panelH - HudLayout.BarHeightPx - HudLayout.BarBottomMarginPx - b - HudLayout.ClusterBottomMarginPx);
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
                    c.HoldTag?.EnableInClassList("hud-orders__holdtag--on", held); c.LastHeld = held;
                }
                RefreshChips(c);
                int advMask = Selection != null ? Selection.AdvanceMask(t) : 0;
                if (advMask != c.LastMask) { c.LastMask = advMask; c.Advance.EnableInClassList("hud-orders__btn--partial", advMask != 0); }
                if (manned != c.LastManned || !c.Shown)
                {
                    c.Fallback.SetEnabled(manned); c.Advance.SetEnabled(manned); c.LastManned = manned;
                    c.Root.EnableInClassList("is-empty", !manned);     // round 11: an empty trench's cluster steps back until hovered
                }
                if (ts.GarrisonCount != c.LastCount) { c.Garrison.text = MenText(ts.GarrisonCount); c.LastCount = ts.GarrisonCount; }
                var morale = garrison != null && t < garrison.Trenches ? garrison.MoraleOf(t) : GarrisonStats.Morale.Steady;
                if (morale != c.LastMorale)
                {
                    c.Garrison.EnableInClassList("hud-orders__count--shaken", morale == GarrisonStats.Morale.Shaken);
                    c.Garrison.EnableInClassList("hud-orders__count--pinned", morale == GarrisonStats.Morale.Pinned);
                    c.LastMorale = morale;
                }
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
