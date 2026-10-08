// Phase: tooling (2026-09-28) — the Proving Ground's launch screen (owner, 2026-09-28: the test level with every
// unit). The ground, its seed, the bombardment, the purse and the scripted enemy on the left, with the ten each side
// brings; on the right every unit the tables define as a tile with the state it is in (BUILT, PROTOTYPE, STAND-IN) and
// the ideas that are words only (IDEA: shown, never picked). A tile goes into the first empty slot of the ten being
// picked, a slot empties when clicked, and TO THE GROUND starts an endless match (ProvingGround.Request) over which the
// router opens the panel (ProvingGroundPanel). What was chosen is kept for the next visit.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.Sim;

namespace TW.UI
{
    public sealed class ProvingGroundLaunchScreen : ShellScreen
    {
        public const string TreePath = "Shell/ProvingGround";
        public static readonly string[] RequiredNames =
        {
            "proving-screen", "ground-tabs", "seed-field", "btn-seed-random", "bombardment-tabs", "silver-tabs", "ai-tabs", "ai-blurb",
            "btn-side-ours", "btn-side-theirs", "btn-defaults", "btn-clear", "ten-ours", "ten-theirs", "ten-note",
            "unit-scroll", "infantry-grid", "machine-grid", "idea-grid", "unit-tip", "btn-back", "btn-start",
        };
        public override bool HidesHud => true;

        public const byte Empty = 255;
        public static readonly string[] GroundNames = { "SHELLED WOOD", "WINTER LINE", "THE LANDING", "THE NARROWS" };
        public static readonly Ground[] Grounds = { Ground.ShelledForest, Ground.WinterLine, Ground.Landing, Ground.Narrows };
        public static readonly string[] BombardmentNames = { "QUIET", "LIGHT", "HEAVY", "DRUMFIRE" };
        public static readonly float[] Bombardments = { 0f, 8f, 30f, 70f };
        public static readonly int[] Silvers = { 300, 1000, 5000, 20000 };
        public static readonly string[] AiBlurbs =
        {
            "No opponent of its own: only the waves you send come at you.",
            "A slow enemy that attacks late and fields no armour or support.",
            "Deploys every two seconds, attacks at eight men, shells you when it can.",
            "Fast deploys, early attacks, tanks whenever it can afford them.",
        };
        public const string EmptyNote = "AN EMPTY SLOT TAKES THE UNIT ITS FACTION PUTS THERE.";
        public const string FullNote = "THE TEN IS FULL: CLICK A SLOT TO EMPTY IT.";

        /// <summary>What the screen was left at, for the next visit (and a restart from the menu).</summary>
        sealed class Choice
        {
            public int Ground, Bombardment = 1, Silver = 2, Ai, Side;
            public uint Seed = 1917;
            public byte[] Ours = ProvingGround.DefaultTen(FactionId.Iron), Theirs = ProvingGround.DefaultTen(FactionId.Brass);
        }
        static Choice kept;
        Choice c;
        // kept across scene loads on purpose (a restart, a return to the menu); forgotten when the Play session ends
        static ProvingGroundLaunchScreen() => SceneStatics.Register(nameof(ProvingGroundLaunchScreen), () => kept = null);

        readonly List<Button> groundTabs = new List<Button>(), bombardmentTabs = new List<Button>(), silverTabs = new List<Button>(), aiTabs = new List<Button>();
        readonly List<Button>[] slots = { new List<Button>(), new List<Button>() };

        public byte[] Ours => c.Ours;
        public byte[] Theirs => c.Theirs;
        public int Side => c.Side;

        /// <param name="fresh">Tests: start from the defaults whatever an earlier screen left.</param>
        public ProvingGroundLaunchScreen(bool fresh = false) { c = fresh || kept == null ? new Choice() : kept; if (!fresh) kept = c; }

        public override VisualTreeAsset Tree(ShellAssets a) => Resources.Load<VisualTreeAsset>(TreePath);

        protected override void OnBind()
        {
            Tabs("ground-tabs", "tab-ground-", GroundNames, groundTabs, i => { c.Ground = i; Refresh(); });
            Tabs("bombardment-tabs", "tab-bombardment-", BombardmentNames, bombardmentTabs, i => { c.Bombardment = i; Refresh(); });
            var silverNames = new string[Silvers.Length]; for (int i = 0; i < Silvers.Length; i++) silverNames[i] = Silvers[i].ToString();
            Tabs("silver-tabs", "tab-silver-", silverNames, silverTabs, i => { c.Silver = i; Refresh(); });
            Tabs("ai-tabs", "tab-ai-", ProvingGround.AiNames, aiTabs, i => { c.Ai = i; Refresh(); });

            var seed = Root.Q<IntegerField>("seed-field");
            seed?.SetValueWithoutNotify((int)c.Seed);
            seed?.RegisterValueChangedCallback(e => c.Seed = (uint)Mathf.Max(0, e.newValue));
            Btn("btn-seed-random", () => { c.Seed = (uint)Random.Range(1, 99999); seed?.SetValueWithoutNotify((int)c.Seed); });

            Btn("btn-side-ours", () => SetSide(0));
            Btn("btn-side-theirs", () => SetSide(1));
            Btn("btn-defaults", Defaults);
            Btn("btn-clear", Clear);
            Btn("btn-back", () => Router?.Pop());
            Btn("btn-start", () => Router?.StartMission(BuildRequest()));

            for (int side = 0; side < 2; side++)
            {
                var row = Root.Q(side == 0 ? "ten-ours" : "ten-theirs");
                slots[side].Clear();
                if (row == null) continue;
                row.Clear();
                for (int s = 0; s < RosterEntry.SlotCount; s++)
                {
                    var b = new Button { name = $"slot-{side}-{s}" }; b.AddToClassList("tw-list-row"); b.AddToClassList("pg-slot"); b.focusable = false;
                    var key = new Label(HudText.Hotkey(s)) { pickingMode = PickingMode.Ignore }; key.AddToClassList("pg-slot__key");
                    var name = new Label("") { name = "name", pickingMode = PickingMode.Ignore }; name.AddToClassList("pg-slot__name");
                    b.Add(key); b.Add(name);
                    int sd = side, sl = s; b.clicked += () => ClearSlot(sd, sl);
                    row.Add(b); slots[side].Add(b);
                }
            }

            var infantry = Root.Q("infantry-grid"); var machines = Root.Q("machine-grid"); var ideas = Root.Q("idea-grid");
            infantry?.Clear(); machines?.Clear(); ideas?.Clear();
            foreach (var u in ProvingGround.Catalogue())
            {
                var tile = Tile("tile-" + u.Archetype, u.Name, UnitArt.NameOf(u.Archetype), u.Status);
                var cost = new Label(u.Cost.ToString()) { pickingMode = PickingMode.Ignore }; cost.AddToClassList("armoury-tile__cost");
                tile.Add(cost);
                var unit = u;
                tile.clicked += () => Pick(unit.Archetype);
                tile.RegisterCallback<MouseEnterEvent>(_ => SetText("unit-tip", Describe(unit)));
                (u.Machine ? machines : infantry)?.Add(tile);
            }
            for (int i = 0; i < ProvingGround.Ideas.Length; i++)
            {
                var idea = ProvingGround.Ideas[i];
                var tile = Tile("idea-" + i, idea.Name, null, UnitStage.Idea);
                tile.AddToClassList("pg-tile--idea");
                tile.RegisterCallback<MouseEnterEvent>(_ => SetText("unit-tip", idea.Name.ToUpperInvariant() + "\nAN IDEA. IT WAITS FOR: " + idea.Needs.ToUpperInvariant()));
                tile.clicked += () => SetText("unit-tip", idea.Name.ToUpperInvariant() + " IS AN IDEA AND CANNOT BE FIELDED YET. IT WAITS FOR: " + idea.Needs.ToUpperInvariant());
                ideas?.Add(tile);
            }
            SetText("unit-tip", "CLICK A UNIT TO PUT IT IN THE TEN BEING PICKED.");
            Refresh();
        }

        public static string Describe(ProvingGround.Unit u) =>
            $"{u.Name.ToUpperInvariant()}  ·  {ProvingGround.StatusNames[(int)u.Status]}  ·  COST {u.Cost}  ·  HP {u.Hp:0}  ·  {u.Speed:0.0} M/S\n{u.Tip}";

        public static string ChipClass(UnitStage s) =>
            s == UnitStage.Prototype ? "pg-chip--prototype" : s == UnitStage.StandIn ? "pg-chip--standin" : s == UnitStage.Idea ? "pg-chip--idea" : "pg-chip--built";

        public static Label Chip(UnitStage s)
        {
            var chip = new Label(ProvingGround.StatusNames[(int)s]) { pickingMode = PickingMode.Ignore };
            chip.AddToClassList("pg-chip"); chip.AddToClassList(ChipClass(s));
            return chip;
        }

        static Button Tile(string id, string name, string art, UnitStage status)
        {
            var tile = new Button { name = id }; tile.AddToClassList("tw-list-row"); tile.AddToClassList("armoury-tile"); tile.focusable = false;
            var pic = new VisualElement { pickingMode = PickingMode.Ignore }; pic.AddToClassList("armoury-tile__art");
            var tex = art != null ? UnitArt.Full(art) : null; if (tex != null) pic.style.backgroundImage = new StyleBackground(tex);
            var label = new Label(name.ToUpperInvariant()) { pickingMode = PickingMode.Ignore }; label.AddToClassList("armoury-tile__name");
            tile.Add(pic); tile.Add(label); tile.Add(Chip(status));
            return tile;
        }

        void Tabs(string container, string prefix, string[] names, List<Button> into, System.Action<int> pick)
        {
            var row = Root.Q(container); into.Clear();
            if (row == null) return;
            row.Clear();
            for (int i = 0; i < names.Length; i++)
            {
                int idx = i;
                var b = new Button { name = prefix + i, text = names[i] }; b.AddToClassList("tw-tab"); b.focusable = false;
                b.clicked += () => pick(idx);
                row.Add(b); into.Add(b);
            }
        }

        static void Mark(List<Button> tabs, int active) { for (int i = 0; i < tabs.Count; i++) tabs[i].EnableInClassList("tw-tab--active", i == active); }

        byte[] Ten(int side) => side == 0 ? c.Ours : c.Theirs;

        public void SetSide(int side) { c.Side = side == 0 ? 0 : 1; Refresh(); }
        public void SetGround(int i) { c.Ground = Mathf.Clamp(i, 0, Grounds.Length - 1); Refresh(); }
        public void SetAi(int i) { c.Ai = Mathf.Clamp(i, 0, ProvingGround.AiNames.Length - 1); Refresh(); }

        /// <summary>Put a unit in the first empty slot of the ten being picked; false (and a note) when it is full.</summary>
        public bool Pick(byte archetype)
        {
            if (ProvingGround.Line(archetype).Hp <= 0f) return false;
            var ten = Ten(c.Side);
            int at = System.Array.IndexOf(ten, Empty);
            if (at < 0) { SetText("ten-note", FullNote); return false; }
            ten[at] = archetype;
            Refresh();
            return true;
        }

        public void ClearSlot(int side, int slot)
        {
            var ten = Ten(side);
            if (slot < 0 || slot >= ten.Length) return;
            ten[slot] = Empty; c.Side = side == 0 ? 0 : 1;
            Refresh();
        }

        /// <summary>The ten being picked, as its faction hands it out.</summary>
        public void Defaults()
        {
            var d = ProvingGround.DefaultTen(c.Side == 0 ? FactionId.Iron : FactionId.Brass);
            System.Array.Copy(d, Ten(c.Side), d.Length);
            Refresh();
        }

        public void Clear()
        {
            var ten = Ten(c.Side);
            for (int i = 0; i < ten.Length; i++) ten[i] = Empty;
            Refresh();
        }

        /// <summary>A ten as the request carries it: slot for slot, an empty slot as the unit its faction puts there
        /// (the sim fills a loadout in order, so dropping the empty ones would move every later unit up a key).</summary>
        public static byte[] Filled(byte[] ten, FactionId faction)
        {
            var d = ProvingGround.DefaultTen(faction);
            var l = new byte[d.Length];
            for (int i = 0; i < l.Length; i++) l[i] = i < ten.Length && ten[i] != Empty ? ten[i] : d[i];
            return l;
        }

        public MatchLaunch.Request BuildRequest() =>
            ProvingGround.Request(Grounds[Mathf.Clamp(c.Ground, 0, Grounds.Length - 1)], c.Seed, Bombardments[Mathf.Clamp(c.Bombardment, 0, Bombardments.Length - 1)],
                Silvers[Mathf.Clamp(c.Silver, 0, Silvers.Length - 1)], Filled(c.Ours, FactionId.Iron), Filled(c.Theirs, FactionId.Brass), c.Ai);

        void Refresh()
        {
            if (Root == null) return;
            Mark(groundTabs, c.Ground); Mark(bombardmentTabs, c.Bombardment); Mark(silverTabs, c.Silver); Mark(aiTabs, c.Ai);
            SetText("ai-blurb", AiBlurbs[Mathf.Clamp(c.Ai, 0, AiBlurbs.Length - 1)]);
            Root.Q<Button>("btn-side-ours")?.EnableInClassList("tw-tab--active", c.Side == 0);
            Root.Q<Button>("btn-side-theirs")?.EnableInClassList("tw-tab--active", c.Side == 1);
            for (int side = 0; side < 2; side++)
            {
                var ten = Ten(side);
                for (int s = 0; s < slots[side].Count && s < ten.Length; s++)
                {
                    var b = slots[side][s]; bool empty = ten[s] == Empty;
                    var name = b.Q<Label>("name"); if (name != null) name.text = empty ? "-" : HudText.Name(ten[s]).ToUpperInvariant();
                    b.EnableInClassList("pg-slot--empty", empty);
                    b.EnableInClassList("pg-slot--active", side == c.Side);
                }
            }
            SetText("ten-note", EmptyNote);
        }
    }
}
