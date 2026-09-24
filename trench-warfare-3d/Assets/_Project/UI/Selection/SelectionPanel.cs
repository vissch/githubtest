// Phase: B6 (implemented) — the selection in the HUD, under the minimap and speed bar as Dust Front has it:
// - one unit: its card — name plate, its face in the mood its health puts it in (the six condition faces), the side,
//   HP as numbers and a bar, its speed;
// - a few (up to TileLimit): a grid of portrait tiles, three to a row, each with its own HP bar; click one to select it
//   alone, Shift+click to drop it;
// - many: one tile per type with the count and the type's mean HP, because a hundred riflemen as a hundred tiles says
//   nothing; click to keep only that type, Shift+click to drop it.
// And the control groups as small chips bottom-left (digit and size), click to recall, a second click to centre.
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
        int drawnVersion = -1, drawnGroupsHash = -1; float nextHp;
        readonly List<(VisualElement fill, UnitHandle h)> tileBars = new List<(VisualElement, UnitHandle)>();
        readonly List<(VisualElement fill, byte archetype)> typeBars = new List<(VisualElement, byte)>();

        public SelectionPanel(VisualElement root, SelectionController sel, SimHost host)
        {
            this.sel = sel; this.host = host;
            panel = root?.Q("selection"); single = root?.Q("sel-single"); grid = root?.Q("sel-grid");
            portrait = root?.Q("sel-portrait"); hpFill = root?.Q("sel-hp-fill"); groups = root?.Q("groups");
            name = root?.Q<Label>("sel-name"); side = root?.Q<Label>("sel-side"); hp = root?.Q<Label>("sel-hp");
            stats = root?.Q<Label>("sel-stats"); header = root?.Q<Label>("sel-header");
            if (panel != null) panel.style.display = DisplayStyle.None;
            if (groups != null) groups.style.display = DisplayStyle.None;
        }

        SimWorld World => host != null && host.Local != null ? host.Local.World : null;

        public void Refresh()
        {
            var w = World; if (w == null || panel == null) return;
            var m = sel.Model;
            if (m.Version != drawnVersion) { drawnVersion = m.Version; Rebuild(w); nextHp = 0f; }
            if (Time.unscaledTime >= nextHp) { nextHp = Time.unscaledTime + RefreshSeconds; RefreshHp(w); }
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

        // ---- the control-group chips ----------------------------------------------------------------------------
        void RefreshGroups()
        {
            if (groups == null) return;
            var m = sel.Model;
            int hash = 17;
            for (int g = 0; g < SelectionModel.GroupCount; g++) hash = hash * 31 + m.GroupSize(g);
            if (hash == drawnGroupsHash) return;
            drawnGroupsHash = hash;
            groups.Clear();
            bool any = false;
            for (int g = 0; g < SelectionModel.GroupCount; g++)
            {
                int n = m.GroupSize(g);
                if (n == 0) continue;
                any = true;
                int group = g;
                var chip = new Button(() => sel.RecallGroup(group)) { focusable = false };
                chip.AddToClassList("tw-btn"); chip.AddToClassList("hud-group-chip");
                var digit = new Label(SelectionModel.KeyDigit(g).ToString()) { pickingMode = PickingMode.Ignore }; digit.AddToClassList("hud-group-chip__digit");
                var size = new Label(n.ToString()) { pickingMode = PickingMode.Ignore }; size.AddToClassList("hud-group-chip__size");
                chip.Add(digit); chip.Add(size);
                chip.tooltip = $"GROUP {SelectionModel.KeyDigit(g)}: SHIFT+{SelectionModel.KeyDigit(g)} TO SELECT, TWICE TO GO THERE";
                groups.Add(chip);
            }
            groups.style.display = any ? DisplayStyle.Flex : DisplayStyle.None;
        }
    }
}
