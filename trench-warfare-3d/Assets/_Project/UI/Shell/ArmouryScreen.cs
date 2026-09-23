// Phase: B6 (implemented) — the armoury (owner, 2026-09-23: "a unit upgrade screen where we can see the cutouts of
// the artworks"). Every unit a side can field, as its painted cutout; choosing one shows it large with its role, the
// roster's numbers (cost, hp, speed, deploy cooldown), the six condition faces the speaker strip uses on the
// battlefield, and three rank slots. There is no upgrade rule in the sim yet, so the ranks are shown locked and say
// so, rather than pretending to buy something. Numbers come from RosterEntry.FillDefault for both sides.
using System.Collections.Generic;
using Unity.Collections;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Sim;

namespace TW.UI
{
    public sealed class ArmouryScreen : ShellScreen
    {
        public const string TreePath = "Shell/Armoury";
        public static readonly string[] RequiredNames = { "armoury-screen", "infantry-grid", "machine-grid", "info-title", "info-role", "info-art", "info-body", "info-stats", "states-row", "states-note", "ranks-row", "btn-back" };
        public static readonly string[] Ranks = { "RANK I", "RANK II", "RANK III" };
        public override bool HidesHud => true;

        readonly Dictionary<byte, RosterEntry> entries = new Dictionary<byte, RosterEntry>();
        readonly List<Button> tiles = new List<Button>();
        public byte Selected { get; private set; }

        public override VisualTreeAsset Tree(ShellAssets a) => Resources.Load<VisualTreeAsset>(TreePath);

        /// <summary>Archetypes either side can deploy, infantry first, each once.</summary>
        public static List<byte> Units(Dictionary<byte, RosterEntry> into = null)
        {
            var roster = new NativeArray<RosterEntry>(RosterEntry.SlotCount * 2, Allocator.Temp);
            RosterEntry.FillDefault(roster, 0); RosterEntry.FillDefault(roster, RosterEntry.SlotCount);
            var list = new List<byte>();
            for (int s = 0; s < roster.Length; s++)
            {
                byte a = roster[s].Archetype;
                if (list.Contains(a)) continue;
                list.Add(a); if (into != null) into[a] = roster[s];
            }
            roster.Dispose();
            list.Sort((x, y) => { bool vx = into != null && into[x].IsVehicle, vy = into != null && into[y].IsVehicle; return vx != vy ? (vx ? 1 : -1) : x.CompareTo(y); });
            return list;
        }

        protected override void OnBind()
        {
            entries.Clear(); tiles.Clear();
            var infantry = Root.Q("infantry-grid"); var machines = Root.Q("machine-grid");
            var units = Units(entries);
            foreach (var a in units)
            {
                var e = entries[a];
                var tile = new Button { name = "tile-" + UnitArt.NameOf(a) }; tile.AddToClassList("tw-list-row"); tile.AddToClassList("armoury-tile"); tile.focusable = false;
                var art = new VisualElement { pickingMode = PickingMode.Ignore }; art.AddToClassList("armoury-tile__art");
                var tex = UnitArt.Full(UnitArt.NameOf(a)); if (tex != null) art.style.backgroundImage = new StyleBackground(tex);
                var name = new Label(HudText.Name(a).ToUpperInvariant()) { pickingMode = PickingMode.Ignore }; name.AddToClassList("armoury-tile__name");
                var cost = new Label(e.Cost.ToString()) { pickingMode = PickingMode.Ignore }; cost.AddToClassList("armoury-tile__cost");
                tile.Add(art); tile.Add(name); tile.Add(cost);
                byte pick = a; tile.clicked += () => Select(pick);
                (e.IsVehicle ? machines : infantry)?.Add(tile);
                tiles.Add(tile);
            }
            var ranks = Root.Q("ranks-row");
            if (ranks != null)
            {
                ranks.Clear();
                foreach (var r in Ranks)
                {
                    var slot = new VisualElement(); slot.AddToClassList("tw-window"); slot.AddToClassList("armoury-rank"); slot.SetEnabled(false);
                    var lk = new VisualElement { pickingMode = PickingMode.Ignore }; lk.AddToClassList("tw-icon"); lk.AddToClassList("tw-ico-lock-closed"); lk.AddToClassList("armoury-rank__lock");
                    var l = new Label(r) { pickingMode = PickingMode.Ignore }; l.AddToClassList("tw-caps"); l.AddToClassList("armoury-rank__label");
                    slot.Add(lk); slot.Add(l); ranks.Add(slot);
                }
            }
            Btn("btn-back", () => Router?.Pop());
            if (units.Count > 0) Select(units[0]);
        }

        public void Select(byte archetype)
        {
            Selected = archetype;
            string art = UnitArt.NameOf(archetype);
            foreach (var t in tiles) t.EnableInClassList("tw-list-row--selected", t.name == "tile-" + art);
            SetText("info-title", HudText.Name(archetype).ToUpperInvariant());
            bool vehicle = entries.TryGetValue(archetype, out var e) && e.IsVehicle;
            SetText("info-role", vehicle ? "MACHINE" : "INFANTRY");
            SetText("info-body", HudText.Tip(archetype));
            if (entries.ContainsKey(archetype))
            {
                float cd = e.CooldownTicks * SimConfig.Default.TickSeconds;
                SetText("info-stats", $"COST {e.Cost} SILVER\nHP {e.Hp:0}\nSPEED {e.Speed:0.0} M/S" + (cd > 0f ? $"\nREDEPLOY {cd:0} S" : ""));
            }
            var big = Root.Q("info-art"); var tex = UnitArt.Full(art);
            if (big != null) big.style.backgroundImage = tex != null ? new StyleBackground(tex) : new StyleBackground(StyleKeyword.None);
            var row = Root.Q("states-row");
            if (row != null)
            {
                row.Clear();
                if (UnitArt.HasFace(art))
                    for (int m = 0; m < UnitArt.StateNames.Length; m++)
                    {
                        var cell = new VisualElement { pickingMode = PickingMode.Ignore }; cell.AddToClassList("tw-window"); cell.AddToClassList("armoury-state");
                        var st = UnitArt.State(art, (Mood)m); if (st != null) cell.style.backgroundImage = new StyleBackground(st);
                        var cap = new Label(UnitArt.StateNames[m].ToUpperInvariant()) { pickingMode = PickingMode.Ignore }; cap.AddToClassList("armoury-state__label");
                        cell.Add(cap); row.Add(cell);
                    }
            }
            SetText("states-note", UnitArt.HasFace(art) ? "HOW THEY REPORT ON THE BATTLEFIELD: TALKING, ORDERS, VICTORY, WOUNDED, CRITICAL" : "NO FACE, NO REPORTS: THE SERGEANT SPEAKS FOR THIS MACHINE");
        }
    }
}
