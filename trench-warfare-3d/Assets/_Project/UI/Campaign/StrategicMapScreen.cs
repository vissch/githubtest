// Phase: B6 / docs/21 phase 6 (implemented) — the strategic map screen: the country nodes (CampaignGraph) as a list
// that mirrors the pins on the continent (IStrategicMapView through MetaServices), the chosen one's blurb, enemy and
// progress, its missions in order (won / next / locked), and SELECT MISSION to the staging screen. Picking a pin
// selects its row; selecting a row focuses the map. Binds without a router or a view (the tests).
using System;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Presentation;

namespace TW.UI
{
    public sealed class StrategicMapScreen : ShellScreen
    {
        public const string TreePath = "Shell/StrategicMap";
        public static readonly string[] RequiredNames = { "map-screen", "gold-readout", "gold-value", "node-list", "node-info", "info-title", "info-blurb", "info-enemy", "info-progress", "mission-list", "btn-select", "btn-home", "btn-back" };
        public static readonly string[] StateChips = { "LOCKED", "OPEN", "HERE", "WON" };
        public static readonly string[] MissionChips = { "LOCKED", "NEXT", "WON", "AGAIN" };
        public override bool HidesHud => true;

        readonly CampaignProfile given;
        CampaignProfile profile;
        IStrategicMapView view;
        Func<Vector2, bool> mine;
        readonly List<Button> rows = new List<Button>(), missionRows = new List<Button>();
        public string Selected { get; private set; }
        public int Mission { get; private set; }

        public StrategicMapScreen(CampaignProfile profile = null) { given = profile; }

        public override VisualTreeAsset Tree(ShellAssets a) => Resources.Load<VisualTreeAsset>(TreePath);

        protected override void OnBind()
        {
            profile = given ?? ProfileStore.Current;
            Btn("btn-back", () => Router?.Pop());
            Btn("btn-home", () => SwapTo(() => new HomeFrontScreen(given)));
            Btn("btn-select", () => { if (Selected != null && CanFightSelected) Router?.Push(new StagingScreen(given, Selected, Mission)); });
            mine = m => ShellPick.Over(Root, m);
            HudBridge.PointerOverUi = mine;
            Selected = null;
            BuildList();
            Attach3D();
        }

        protected override void OnUnbind() { Detach3D(); if (HudBridge.PointerOverUi == mine) HudBridge.PointerOverUi = null; }
        public override void OnCovered() { Detach3D(); if (HudBridge.PointerOverUi == mine) HudBridge.PointerOverUi = null; }
        public override void OnUncovered() { profile = given ?? ProfileStore.Current; HudBridge.PointerOverUi = mine; BuildList(); Attach3D(); }

        bool CanFightSelected
        {
            get { var n = CampaignGraph.Find(Selected); return n != null && CampaignGraph.CanFight(n, Mission, profile); }
        }

        void Attach3D()
        {
            if (!Application.isPlaying) return;
            view = MetaServices.EnsureMap();
            if (view == null) return;
            view.Picked += Select;
            view.Show(CampaignGraph.Views(profile, Selected), CampaignGraph.FrontLine);
            if (Selected != null) view.Focus(Selected, false);
        }

        void Detach3D()
        {
            if (view == null) return;
            view.Picked -= Select;
            view.Hide();
            view = null;
        }

        /// <summary>The node the screen opens on: the last one fought if it is still open, else the first open one.</summary>
        public static string Opening(CampaignProfile p)
        {
            var last = CampaignGraph.Find(p.LastNode);
            if (last != null && CampaignGraph.IsAvailable(last, p)) return last.Id;
            foreach (var id in CampaignGraph.FrontLine)
            {
                var n = CampaignGraph.Find(id);
                if (n != null && CampaignGraph.StateOf(n, p) == NodeState.Available) return n.Id;
            }
            return CampaignGraph.Nodes.Length > 0 ? CampaignGraph.Nodes[0].Id : null;
        }

        void BuildList()
        {
            SetText("gold-value", profile.Gold.ToString());
            var list = Root.Q("node-list");
            rows.Clear();
            if (list != null)
            {
                list.Clear();
                foreach (var id in CampaignGraph.FrontLine)
                {
                    var n = CampaignGraph.Find(id); if (n == null) continue;
                    var row = new Button { name = "row-" + n.Id }; row.AddToClassList("tw-list-row"); row.focusable = false;
                    var t = new Label(n.Name) { pickingMode = PickingMode.Ignore }; t.AddToClassList("tw-list-row__title"); row.Add(t);
                    var state = CampaignGraph.StateOf(n, profile, Selected);
                    var s = new Label(StateChips[(int)state] + "  ·  " + CampaignGraph.Done(n, profile) + " / " + n.Missions.Length) { pickingMode = PickingMode.Ignore }; s.AddToClassList("tw-list-row__sub"); row.Add(s);
                    string pick = n.Id; row.clicked += () => Select(pick);
                    list.Add(row); rows.Add(row);
                }
            }
            Select(Selected ?? Opening(profile));
        }

        public void Select(string id)
        {
            var n = CampaignGraph.Find(id);
            if (n == null) return;
            Selected = id;
            Mission = CampaignGraph.NextMission(n, profile);
            foreach (var r in rows) r.EnableInClassList("tw-list-row--selected", r.name == "row-" + id);
            if (view != null)
            {
                foreach (var other in CampaignGraph.Nodes) view.SetState(other.Id, CampaignGraph.StateOf(other, profile, Selected));
                view.Focus(id, true);
            }
            SetText("info-title", n.Name);
            SetText("info-blurb", n.Blurb);
            SetText("info-enemy", "ENEMY  ·  " + HomeFrontScreen.FactionNames[Mathf.Clamp(n.EnemyFaction, 0, HomeFrontScreen.FactionNames.Length - 1)] + (n.Finale ? "  ·  THE LAST BATTLE" : ""));
            var state = CampaignGraph.StateOf(n, profile, Selected);
            SetText("info-progress", CampaignGraph.Done(n, profile) + " / " + n.Missions.Length + " MISSIONS WON  ·  " + (state == NodeState.Locked ? "NEEDS " + string.Join(" AND ", Names(n.Prerequisites)) : StateChips[(int)state]));
            BuildMissions(n);
        }

        static string[] Names(string[] ids)
        {
            var names = new string[ids.Length];
            for (int i = 0; i < ids.Length; i++) names[i] = CampaignGraph.Find(ids[i])?.Name ?? ids[i].ToUpperInvariant();
            return names;
        }

        void BuildMissions(CampaignNode n)
        {
            var list = Root.Q("mission-list");
            missionRows.Clear();
            if (list != null)
            {
                list.Clear();
                for (int i = 0; i < n.Missions.Length; i++)
                {
                    var m = n.Missions[i];
                    bool won = profile.IsComplete(n.Id, i), can = CampaignGraph.CanFight(n, i, profile);
                    string chip = won ? (can ? MissionChips[3] : MissionChips[2]) : can ? MissionChips[1] : MissionChips[0];
                    var row = new Button { name = "mission-" + i }; row.AddToClassList("tw-list-row"); row.focusable = false; row.SetEnabled(can);
                    var t = new Label((i + 1) + ".  " + m.Title) { pickingMode = PickingMode.Ignore }; t.AddToClassList("tw-list-row__title"); row.Add(t);
                    var s = new Label(chip + "  ·  " + (won ? "NO GOLD AGAIN" : CampaignGraph.Reward(n, i) + " GOLD")) { pickingMode = PickingMode.Ignore }; s.AddToClassList("tw-list-row__sub"); row.Add(s);
                    int idx = i; row.clicked += () => SelectMission(idx);
                    list.Add(row); missionRows.Add(row);
                }
            }
            SelectMission(Mission);
        }

        public void SelectMission(int index)
        {
            var n = CampaignGraph.Find(Selected);
            if (n == null || index < 0 || index >= n.Missions.Length) return;
            Mission = index;
            for (int i = 0; i < missionRows.Count; i++) missionRows[i].EnableInClassList("tw-list-row--selected", i == index);
            Root.Q<Button>("btn-select")?.SetEnabled(CanFightSelected);
        }
    }
}
