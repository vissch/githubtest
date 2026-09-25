// Phase: B6 (implemented) — the selection in the HUD, under the minimap and speed bar as Dust Front has it:
// - one unit: its card — name plate, its face in the mood its health puts it in (the six condition faces), the side,
//   HP as numbers and a bar, its speed;
// - a few (up to TileLimit): a grid of portrait tiles, three to a row, each with its own HP bar; click one to select it
//   alone, Shift+click to drop it;
// - many: one tile per type with the count and the type's mean HP, because a hundred riflemen as a hundred tiles says
//   nothing; click to keep only that type, Shift+click to drop it.
// And the control groups as small chips bottom-left (digit and size), click to recall, a second click to centre. A chip
// carries its group's alert (GroupAlerts): flashing red when the group is being cut down, amber when half of it is
// pinned, grey when it is gone; a click on an alerting chip recalls the group and goes there at once.
// Redrawn when the selection changes; the HP figures refresh a few times a second.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.Sim;

namespace TW.UI
{
    public sealed class SelectionPanel
    {
        public const int TileLimit = 12, Columns = 3;
        public const float RefreshSeconds = 0.2f;

        readonly SelectionController sel;
        readonly SimHost host;
        readonly VisualElement panel, single, grid, portrait, hpFill, groups;
        readonly Label name, side, hp, stats, header;
        readonly Label ordersLine;   // "G: MG, ASSAULT OF TRENCH 2 GO OVER THE TOP (5)" when the selection is a trench's categories
        int shownOrdersKey = int.MinValue;
        int drawnVersion = -1; float nextHp;
        public const float FlashHz = 2.5f;
        public readonly GroupAlerts Alerts = new GroupAlerts();
        sealed class Chip { public Button Root; public Label Size; public int Shown = -1; public GroupAlerts.Alert Alert; public bool Visible, Flash; }
        readonly Chip[] chips = new Chip[SelectionModel.GroupCount];
        readonly List<(VisualElement fill, UnitHandle h)> tileBars = new List<(VisualElement, UnitHandle)>();
        readonly List<(VisualElement fill, byte archetype)> typeBars = new List<(VisualElement, byte)>();

        public SelectionPanel(VisualElement root, SelectionController sel, SimHost host)
        {
            this.sel = sel; this.host = host;
            panel = root?.Q("selection"); single = root?.Q("sel-single"); grid = root?.Q("sel-grid");
            portrait = root?.Q("sel-portrait"); hpFill = root?.Q("sel-hp-fill"); groups = root?.Q("groups");
            name = root?.Q<Label>("sel-name"); side = root?.Q<Label>("sel-side"); hp = root?.Q<Label>("sel-hp");
            stats = root?.Q<Label>("sel-stats"); header = root?.Q<Label>("sel-header");
            if (panel != null)
            {
                panel.style.display = DisplayStyle.None;
                ordersLine = new Label { pickingMode = PickingMode.Ignore }; ordersLine.AddToClassList("hud-selection__orders");
                ordersLine.style.display = DisplayStyle.None;
                panel.Add(ordersLine);
            }
            if (groups != null) groups.style.display = DisplayStyle.None;
        }

        SimWorld World => host != null && host.Local != null ? host.Local.World : null;

        public void Refresh()
        {
            var w = World; if (w == null || panel == null) return;
            var m = sel.Model;
            if (m.Version != drawnVersion) { drawnVersion = m.Version; Rebuild(w); nextHp = 0f; }
            if (Time.unscaledTime >= nextHp) { nextHp = Time.unscaledTime + RefreshSeconds; RefreshHp(w); RefreshOrders(); }
            RefreshGroups();
        }

        void Rebuild(SimWorld w)
        {
            var m = sel.Model;
            panel.style.display = m.Count > 0 ? DisplayStyle.Flex : DisplayStyle.None;
            if (m.Count == 0) return;
            bool one = m.Count == 1;
            single.style.display = one ? DisplayStyle.Flex : DisplayStyle.None;
            grid.style.display = one ? DisplayStyle.None : DisplayStyle.Flex;
            tileBars.Clear(); typeBars.Clear(); grid.Clear();
            if (one)
            {
                var h = m.Items[0];
                byte a = w.Archetype[h.Slot];
                bool ours = (w.Team[h.Slot] & 1) == 0;
                name.text = HudText.Name(a).ToUpperInvariant();
                side.text = ours ? ((w.Flags[h.Slot] & (uint)UnitFlags.Vehicle) != 0 ? "ARMOUR" : "INFANTRY") : "ENEMY";
                side.EnableInClassList("hud-selection__side--enemy", !ours);
                header.text = "";
                return;
            }
            if (m.Count <= TileLimit)
            {
                header.text = m.Count + " SELECTED";
                foreach (var h in m.Items) grid.Add(Tile(w, h));
            }
            else
            {
                var counts = new SortedDictionary<byte, int>();
                foreach (var h in m.Items) { byte a = w.Archetype[h.Slot]; counts.TryGetValue(a, out int c); counts[a] = c + 1; }
                header.text = m.Count + " SELECTED  ·  " + counts.Count + (counts.Count == 1 ? " TYPE" : " TYPES");
                foreach (var kv in counts) grid.Add(TypeTile(kv.Key, kv.Value));
            }
        }

        VisualElement Tile(SimWorld w, UnitHandle h)
        {
            byte a = w.Archetype[h.Slot];
            var t = TileShell(a, null, out var fill);
            t.RegisterCallback<ClickEvent>(e => sel.SelectOnly(h, e.shiftKey));
            tileBars.Add((fill, h));
            return t;
        }

        VisualElement TypeTile(byte archetype, int count)
        {
            var t = TileShell(archetype, "×" + count, out var fill);
            t.RegisterCallback<ClickEvent>(e => sel.KeepType(archetype, e.shiftKey));
            typeBars.Add((fill, archetype));
            return t;
        }

        static VisualElement TileShell(byte archetype, string count, out VisualElement fill)
        {
            var t = new VisualElement { focusable = false }; t.AddToClassList("tw-window"); t.AddToClassList("hud-selection__tile");
            var art = new VisualElement { pickingMode = PickingMode.Ignore }; art.AddToClassList("hud-selection__tile-art");
            string n = UnitArt.NameOf(archetype);
            var tex = UnitArt.HasFace(n) ? UnitArt.State(n, Mood.Neutral) : UnitArt.Full(n);
            if (tex != null) art.style.backgroundImage = new StyleBackground(tex);
            t.Add(art);
            if (count != null) { var c = new Label(count) { pickingMode = PickingMode.Ignore }; c.AddToClassList("hud-selection__tile-count"); t.Add(c); }
            var track = new VisualElement { pickingMode = PickingMode.Ignore }; track.AddToClassList("hud-selection__tile-hp");
            fill = new VisualElement { pickingMode = PickingMode.Ignore }; fill.AddToClassList("hud-selection__tile-hp-fill");
            track.Add(fill); t.Add(track);
            t.tooltip = HudText.Name(archetype);
            return t;
        }

        void RefreshHp(SimWorld w)
        {
            var m = sel.Model;
            if (m.Count == 1)
            {
                var h = m.Items[0];
                float cur = w.Hp[h.Slot], max = Mathf.Max(1f, w.MaxHp[h.Slot]), k = Mathf.Clamp01(cur / max);
                hp.text = $"HP {Mathf.CeilToInt(cur)} / {Mathf.CeilToInt(max)}";
                hpFill.style.width = Length.Percent(k * 100f);
                var st = UnitStatus.Of(w, h.Slot);
                stats.text = UnitStatus.Word(st) + $"  ·  SPEED {w.Speed[h.Slot]:0.0} M/S";
                stats.EnableInClassList("hud-hover__state--warn", UnitStatus.Tone(st) == StateTone.Warn);
                stats.EnableInClassList("hud-hover__state--alarm", UnitStatus.Tone(st) == StateTone.Alarm);
                string n = UnitArt.NameOf(w.Archetype[h.Slot]);
                var mood = k > 0.6f ? Mood.Neutral : k > 0.3f ? Mood.Wounded : Mood.Critical;
                var tex = UnitArt.HasFace(n) ? UnitArt.State(n, mood) : UnitArt.Full(n);
                portrait.style.backgroundImage = tex != null ? new StyleBackground(tex) : new StyleBackground(StyleKeyword.None);
                return;
            }
            foreach (var (fill, h) in tileBars)
                if (w.IsAlive(h.Slot)) fill.style.width = Length.Percent(Mathf.Clamp01(w.Hp[h.Slot] / Mathf.Max(1f, w.MaxHp[h.Slot])) * 100f);
            if (typeBars.Count > 0)
            {
                foreach (var (fill, a) in typeBars)
                {
                    float sum = 0f; int n = 0;
                    foreach (var h in m.Items) if (w.Archetype[h.Slot] == a) { sum += Mathf.Clamp01(w.Hp[h.Slot] / Mathf.Max(1f, w.MaxHp[h.Slot])); n++; }
                    fill.style.width = Length.Percent(n > 0 ? sum / n * 100f : 0f);
                }
            }
        }

        /// <summary>When every selected man garrisons one trench, what G would do: who goes, whole categories, pinned stay.</summary>
        void RefreshOrders()
        {
            if (ordersLine == null) return;
            var g = sel.Garrison;
            bool scoped = sel.ScopedTrench(out int t, out int raw) && g != null && t < g.Trenches;
            int eff = scoped ? TrenchScope.Effective(raw, TrenchScope.Present(g, t)) : 0;
            int go = 0, stay = 0;
            if (scoped)
                for (int a = 0; a <= TrenchScope.MaxMaskArchetype; a++)
                    if ((raw & (1 << a)) != 0 || eff == 0 && g.TypeCount(t, a) > 0) { go += g.TypeCount(t, a) - g.TypePinned(t, a); stay += g.TypePinned(t, a); }
            int key = scoped ? ((t * 31 + eff) * 1000 + go) * 1000 + stay : -1;
            if (key == shownOrdersKey) return;
            shownOrdersKey = key;
            ordersLine.style.display = scoped ? DisplayStyle.Flex : DisplayStyle.None;
            if (!scoped) return;
            var who = new System.Text.StringBuilder();
            if (eff == 0) who.Append("ALL OF TRENCH ").Append(t + 1);
            else
            {
                for (int a = 0; a <= TrenchScope.MaxMaskArchetype; a++)
                    if ((eff & (1 << a)) != 0 && g.TypeCount(t, a) > 0) { if (who.Length > 0) who.Append(", "); who.Append(HudText.Name((byte)a).ToUpperInvariant()); }
                who.Append(" OF TRENCH ").Append(t + 1);
            }
            ordersLine.text = KeyMap.Display(KeyMap.Primary(GameAction.Advance)) + ": " + who + " GO OVER THE TOP (" + go + ")" + (stay > 0 ? "  ·  " + stay + " PINNED STAY" : "");
            ordersLine.EnableInClassList("hud-selection__orders--warn", stay > 0);
        }

        // ---- the control-group chips ----------------------------------------------------------------------------
        void RefreshGroups()
        {
            if (groups == null) return;
            var w = World; var m = sel.Model;
            float now = Time.unscaledTime;
            bool flashOn = Mathf.Repeat(now * FlashHz, 1f) < 0.5f;
            bool any = false;
            for (int g = 0; g < SelectionModel.GroupCount; g++)
            {
                int n = m.GroupSize(g), pinned = 0;
                if (w != null)
                {
                    var members = m.Group(g);   // indexed: a foreach over the interface would allocate every frame
                    for (int i = 0; i < members.Count; i++) if (UnitStatus.Of(w, members[i].Slot) == UnitState.Pinned) pinned++;
                }
                Alerts.Observe(g, m.GroupStamp(g), n, pinned, now);
                var alert = Alerts.Of(g, now);
                bool visible = n > 0 || alert == GroupAlerts.Alert.Lost;
                var c = chips[g] ??= MakeChip(g);
                if (visible != c.Visible) { c.Visible = visible; c.Root.style.display = visible ? DisplayStyle.Flex : DisplayStyle.None; }
                if (!visible) continue;
                any = true;
                if (n != c.Shown) { c.Shown = n; c.Size.text = n.ToString(); }
                if (alert != c.Alert)
                {
                    c.Alert = alert;
                    c.Root.EnableInClassList("hud-group-chip--pinned", alert == GroupAlerts.Alert.Pinned);
                    c.Root.EnableInClassList("hud-group-chip--hit", alert == GroupAlerts.Alert.Hit);
                    c.Root.EnableInClassList("hud-group-chip--lost", alert == GroupAlerts.Alert.Lost);
                }
                bool flash = alert == GroupAlerts.Alert.Hit && flashOn;
                if (flash != c.Flash) { c.Flash = flash; c.Root.EnableInClassList("hud-group-chip--flash", flash); }
            }
            bool show = any;
            if (groups.style.display != (show ? DisplayStyle.Flex : DisplayStyle.None)) groups.style.display = show ? DisplayStyle.Flex : DisplayStyle.None;
        }

        Chip MakeChip(int g)
        {
            var c = new Chip();
            c.Root = new Button(() => Click(g)) { focusable = false };
            c.Root.AddToClassList("tw-btn"); c.Root.AddToClassList("hud-group-chip");
            var digit = new Label(SelectionModel.KeyDigit(g).ToString()) { pickingMode = PickingMode.Ignore }; digit.AddToClassList("hud-group-chip__digit");
            c.Size = new Label { pickingMode = PickingMode.Ignore }; c.Size.AddToClassList("hud-group-chip__size");
            var pip = new VisualElement { pickingMode = PickingMode.Ignore }; pip.AddToClassList("hud-group-chip__pip");
            c.Root.Add(digit); c.Root.Add(c.Size); c.Root.Add(pip);
            c.Root.style.display = DisplayStyle.None;
            groups.Add(c.Root);   // in group order, so the digits read 1..9, 0 whichever are showing
            return c;
        }

        void Click(int g)
        {
            var alert = Alerts.Of(g, Time.unscaledTime);
            if (alert == GroupAlerts.Alert.Lost) return;           // nobody to go to
            sel.RecallGroup(g);
            if (alert != GroupAlerts.Alert.None) sel.FocusSelection();   // in trouble: take me there now
        }
    }
}
