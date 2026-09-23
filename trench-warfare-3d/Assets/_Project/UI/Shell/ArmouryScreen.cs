// Phase: B6 (implemented) — the armoury (owner, 2026-09-23: "a unit upgrade screen where we can see the cutouts of
// the artworks"). Every unit a side can field, as its painted cutout; choosing one shows it large with its role, the
// roster's numbers (cost, hp, speed, deploy cooldown) as a label/value grid, three rank slots, and the six condition
// faces the speaker strip uses on the battlefield. There is no upgrade rule in the sim yet, so the ranks are shown
// locked and say so, rather than pretending to buy something. Numbers come from RosterEntry.FillDefault for both
// sides; a unit only the enemy fields shows an ENEMY chip where our units show their cost.
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
        public const string RankLocked = "NOT IN RULES YET";
        public override bool HidesHud => true;

        readonly Dictionary<byte, RosterEntry> entries = new Dictionary<byte, RosterEntry>();
        readonly HashSet<byte> ours = new HashSet<byte>();
        readonly List<Button> tiles = new List<Button>();
        public byte Selected { get; private set; }

        public override VisualTreeAsset Tree(ShellAssets a) => Resources.Load<VisualTreeAsset>(TreePath);

        /// <summary>Archetypes either side can deploy, infantry first, each once; ourSide collects team 0's.</summary>
        public static List<byte> Units(Dictionary<byte, RosterEntry> into = null, HashSet<byte> ourSide = null)
        {
            var roster = new NativeArray<RosterEntry>(RosterEntry.SlotCount * 2, Allocator.Temp);
            RosterEntry.FillDefault(roster, 0); RosterEntry.FillDefault(roster, RosterEntry.SlotCount);
            var list = new List<byte>();
            for (int s = 0; s < roster.Length; s++)
            {
                byte a = roster[s].Archetype;
                if (s < RosterEntry.SlotCount) ourSide?.Add(a);
                if (list.Contains(a)) continue;
                list.Add(a); if (into != null) into[a] = roster[s];
            }
            roster.Dispose();
            list.Sort((x, y) => { bool vx = into != null && into[x].IsVehicle, vy = into != null && into[y].IsVehicle; return vx != vy ? (vx ? 1 : -1) : x.CompareTo(y); });
            return list;
        }

        protected override void OnBind()
        {
            entries.Clear(); tiles.Clear(); ours.Clear();
            var infantry = Root.Q("infantry-grid"); var machines = Root.Q("machine-grid");
            var units = Units(entries, ours);
            foreach (var a in units)
            {
                var e = entries[a];
                var tile = new Button { name = "tile-" + UnitArt.NameOf(a) }; tile.AddToClassList("tw-list-row"); tile.AddToClassList("armoury-tile"); tile.focusable = false;
                var art = new VisualElement { pickingMode = PickingMode.Ignore }; art.AddToClassList("armoury-tile__art");
                var tex = UnitArt.Full(UnitArt.NameOf(a)); if (tex != null) art.style.backgroundImage = new StyleBackground(tex);
                var name = new Label(HudText.Name(a).ToUpperInvariant()) { pickingMode = PickingMode.Ignore }; name.AddToClassList("armoury-tile__name");
                bool enemy = !ours.Contains(a);
                var cost = new Label(enemy ? "ENEMY" : e.Cost.ToString()) { pickingMode = PickingMode.Ignore };
                cost.AddToClassList(enemy ? "armoury-tile__chip" : "armoury-tile__cost");
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
                    var slot = new VisualElement(); slot.AddToClassList("tw-window"); slot.AddToClassList("armoury-rank");
                    var lk = new VisualElement { pickingMode = PickingMode.Ignore }; lk.AddToClassList("tw-icon"); lk.AddToClassList("tw-ico-lock-closed"); lk.AddToClassList("armoury-rank__lock");
                    var text = new VisualElement { pickingMode = PickingMode.Ignore }; text.AddToClassList("armoury-rank__text");
                    var l = new Label(r) { pickingMode = PickingMode.Ignore }; l.AddToClassList("tw-caps"); l.AddToClassList("armoury-rank__label");
                    var why = new Label(RankLocked) { pickingMode = PickingMode.Ignore }; why.AddToClassList("armoury-rank__why");
                    text.Add(l); text.Add(why); slot.Add(lk); slot.Add(text); ranks.Add(slot);
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
            SetText("info-role", (vehicle ? "ARMOUR" : "INFANTRY") + (ours.Contains(archetype) ? "" : "  ·  ENEMY SIDE"));
            SetText("info-body", HudText.Tip(archetype));
            var stats = Root.Q("info-stats");
            if (stats != null)
            {
                stats.Clear();
                if (entries.ContainsKey(archetype))
                {
                    float cd = e.CooldownTicks * SimConfig.Default.TickSeconds;
                    Stat(stats, "COST", e.Cost + " SILVER", true);
                    Stat(stats, "HP", e.Hp.ToString("0"));
                    Stat(stats, "SPEED", e.Speed.ToString("0.0") + " M/S");
                    if (cd > 0f) Stat(stats, "REDEPLOY", cd.ToString("0") + " S");
                }
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
                        var cell = new VisualElement { pickingMode = PickingMode.Ignore }; cell.AddToClassList("armoury-state");
                        var face = new VisualElement { pickingMode = PickingMode.Ignore }; face.AddToClassList("tw-window"); face.AddToClassList("armoury-state__face");
                        var st = UnitArt.State(art, (Mood)m); if (st != null) face.style.backgroundImage = new StyleBackground(st);
                        var cap = new Label(UnitArt.StateNames[m].ToUpperInvariant()) { pickingMode = PickingMode.Ignore }; cap.AddToClassList("armoury-state__label");
                        cell.Add(face); cell.Add(cap); row.Add(cell);
                    }
            }
            // the tiles carry their own names; the note only explains an empty row
            bool face6 = UnitArt.HasFace(art);
            SetText("states-note", face6 ? "" : "NO FACE, NO REPORTS: THE SERGEANT SPEAKS FOR THIS MACHINE");
            Root.Q("states-note")?.EnableInClassList("armoury-note--hidden", face6);
        }

        /// <summary>One label/value row of the stats grid: labels dim, values bone, the cost amber.</summary>
        static void Stat(VisualElement grid, string label, string value, bool cost = false)
        {
            var row = new VisualElement { pickingMode = PickingMode.Ignore }; row.AddToClassList("armoury-stat");
            var l = new Label(label) { pickingMode = PickingMode.Ignore }; l.AddToClassList("armoury-stat__label");
            var v = new Label(value) { pickingMode = PickingMode.Ignore }; v.AddToClassList("armoury-stat__value");
            if (cost) v.AddToClassList("armoury-stat__value--cost");
            row.Add(l); row.Add(v); grid.Add(row);
        }
    }
}
