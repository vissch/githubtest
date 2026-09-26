// Phase: B6 / docs/21 phase 6 (implemented) — the Home Front screen: the faction's buildings (FactionBuildings) as a
// list over the diorama (IHomeFrontView through MetaServices), the chosen one's stage, EXPAND with its cost or the
// reason it cannot, and one row per upgrade line with tier pips and BUY. Every purchase mutates the profile, saves
// it and tells the diorama to raise the building. IRON / BRASS tabs switch the city. Binds without a router or a
// view (the tests); a profile handed in is never written to disk.
using System;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Presentation;

namespace TW.UI
{
    public sealed class HomeFrontScreen : ShellScreen
    {
        public const string TreePath = "Shell/HomeFront";
        public static readonly string[] RequiredNames = { "homefront-screen", "gold-readout", "gold-value", "faction-tabs", "building-list", "building-info", "info-title", "info-blurb", "stage-label", "btn-expand", "expand-cost", "lines-list", "btn-back", "btn-front" };
        public static readonly string[] FactionNames = { "IRON", "BRASS" };
        public static readonly string[] StageNames = { "GROUND FLOOR", "STAGE I", "STAGE II", "STAGE III" };
        public override bool HidesHud => true;

        readonly CampaignProfile given;
        CampaignProfile profile; bool persist;
        IHomeFrontView view;
        Func<Vector2, bool> mine;
        readonly List<Button> rows = new List<Button>(), tabs = new List<Button>();
        public byte Faction { get; private set; }
        public string Selected { get; private set; }

        public HomeFrontScreen(CampaignProfile profile = null) { given = profile; }

        public override VisualTreeAsset Tree(ShellAssets a) => Resources.Load<VisualTreeAsset>(TreePath);

        protected override void OnBind()
        {
            profile = given ?? ProfileStore.Current; persist = given == null;
            Faction = (byte)Mathf.Clamp(profile.Faction, 0, FactionNames.Length - 1);
            var tabRoot = Root.Q("faction-tabs");
            tabs.Clear();
            if (tabRoot != null)
            {
                tabRoot.Clear();
                for (byte f = 0; f < FactionNames.Length; f++)
                {
                    byte pick = f;
                    var b = new Button { name = "tab-faction-" + f, text = FactionNames[f] }; b.AddToClassList("tw-tab"); b.focusable = false;
                    b.clicked += () => SetFaction(pick);
                    tabRoot.Add(b); tabs.Add(b);
                }
            }
            Btn("btn-expand", Expand);
            Btn("btn-back", () => Router?.Pop());
            Btn("btn-front", () => SwapTo(() => new StrategicMapScreen(given)));
            mine = m => ShellPick.Over(Root, m);
            HudBridge.PointerOverUi = mine;
            BuildList();
            Attach3D();
        }

        protected override void OnUnbind() { Detach3D(); if (HudBridge.PointerOverUi == mine) HudBridge.PointerOverUi = null; }
        public override void OnCovered() { Detach3D(); if (HudBridge.PointerOverUi == mine) HudBridge.PointerOverUi = null; }
        public override void OnUncovered() { profile = given ?? ProfileStore.Current; HudBridge.PointerOverUi = mine; BuildList(); Attach3D(); }

        void Attach3D()
        {
            if (!Application.isPlaying) return;
            view = MetaServices.EnsureHomeFront();
            if (view == null) return;
            view.Picked += Select;
            view.Show(Faction, FactionBuildings.Views(profile, Faction));
            if (Selected != null) view.Highlight(Selected);
        }

        void Detach3D()
        {
            if (view == null) return;
            view.Picked -= Select;
            view.Hide();
            view = null;
        }

        /// <summary>The tab is the choice: the profile's faction is saved at once (Save honours the test switch), so
        /// the map, the staging screen and the next session open on it.</summary>
        public void SetFaction(byte f)
        {
            if (f >= FactionNames.Length || f == Faction) return;
            Faction = f; profile.Faction = f; Save();
            Selected = null;
            BuildList();
            if (view != null) { view.Show(Faction, FactionBuildings.Views(profile, Faction)); if (Selected != null) view.Highlight(Selected); }
        }

        void BuildList()
        {
            for (int i = 0; i < tabs.Count; i++) tabs[i].EnableInClassList("tw-tab--active", i == Faction);
            var list = Root.Q("building-list");
            rows.Clear();
            var all = FactionBuildings.Of(Faction);
            if (list != null)
            {
                list.Clear();
                foreach (var b in all)
                {
                    var row = new Button { name = "row-" + b.Id }; row.AddToClassList("tw-list-row"); row.focusable = false;
                    var t = new Label(b.Name) { pickingMode = PickingMode.Ignore }; t.AddToClassList("tw-list-row__title"); row.Add(t);
                    var s = new Label(StageNames[Mathf.Clamp(profile.StageOf(Faction, b.Id), 0, StageNames.Length - 1)]) { pickingMode = PickingMode.Ignore }; s.AddToClassList("tw-list-row__sub"); row.Add(s);
                    string id = b.Id; row.clicked += () => Select(id);
                    list.Add(row); rows.Add(row);
                }
            }
            if (Selected == null || FactionBuildings.Find(Faction, Selected) == null) Selected = all.Length > 0 ? all[0].Id : null;
            if (Selected != null) Select(Selected);
        }

        public void Select(string id)
        {
            var b = FactionBuildings.Find(Faction, id);
            if (b == null) return;
            Selected = id;
            view?.Highlight(id);
            foreach (var r in rows) r.EnableInClassList("tw-list-row--selected", r.name == "row-" + id);
            RefreshInfo(b);
        }

        void RefreshInfo(Building b)
        {
            SetText("gold-value", profile.Gold.ToString());
            SetText("info-title", b.Name);
            SetText("info-blurb", b.Blurb);
            int stage = Mathf.Clamp(profile.StageOf(Faction, b.Id), 0, b.Stages.Length - 1);
            SetText("stage-label", StageNames[Mathf.Min(stage, StageNames.Length - 1)] + "  ·  " + b.Stages[stage].Blurb);
            bool can = FactionBuildings.CanExpand(profile, b, out var why);
            Root.Q<Button>("btn-expand")?.SetEnabled(can);
            SetText("expand-cost", can ? b.Stages[stage + 1].Cost + " GOLD" : why);
            var lines = Root.Q("lines-list");
            if (lines == null) return;
            lines.Clear();
            for (int i = 0; i < b.Lines.Length; i++)
            {
                var l = b.Lines[i]; int tier = profile.TierOf(Faction, b.Id, i);
                var row = new VisualElement { name = "line-" + i }; row.AddToClassList("campaign-line");
                var name = new Label(l.Name) { pickingMode = PickingMode.Ignore, tooltip = l.Blurb }; name.AddToClassList("tw-caps"); name.AddToClassList("campaign-line__name"); row.Add(name);
                var pips = new VisualElement { pickingMode = PickingMode.Ignore }; pips.AddToClassList("campaign-pips");
                for (int t = 1; t <= l.MaxTier; t++)
                {
                    var pip = new VisualElement { pickingMode = PickingMode.Ignore }; pip.AddToClassList("campaign-pip");
                    if (t <= tier) pip.AddToClassList("campaign-pip--on");
                    pips.Add(pip);
                }
                row.Add(pips);
                bool ok = FactionBuildings.CanBuy(profile, b, i, out var reason);
                var whyLabel = new Label(ok ? l.Cost(tier + 1) + " GOLD" : reason) { pickingMode = PickingMode.Ignore }; whyLabel.AddToClassList("tw-caption"); whyLabel.AddToClassList("campaign-line__why"); row.Add(whyLabel);
                var buy = new Button { name = "btn-buy-" + i, text = l.Kind == LineKind.AbilityUnlock ? "UNLOCK" : "BUY", tooltip = l.Blurb };
                buy.AddToClassList("tw-btn"); buy.AddToClassList("tw-btn--small"); buy.focusable = false; buy.SetEnabled(ok);
                int line = i; buy.clicked += () => Buy(b, line);
                row.Add(buy);
                lines.Add(row);
            }
        }

        public void Expand()
        {
            var b = FactionBuildings.Find(Faction, Selected);
            if (b == null || !FactionBuildings.Expand(profile, b)) return;
            Save();
            view?.SetStage(FactionBuildings.View(profile, b), true);
            BuildList();
        }

        public void Buy(Building b, int line)
        {
            if (!FactionBuildings.Buy(profile, b, line)) return;
            Save();
            RefreshInfo(b);
        }

        void Save() { if (persist) ProfileStore.Save(profile); }
    }
}
