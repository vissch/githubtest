// Phase: B6 (implemented) — the objectives list top-left (Dust Front's red-bulleted list) and the centre banner.
// One row per sector objective: a bullet in the owner's colour (amber while someone is capturing it), its name and
// the capture progress; rows are built once and rebound only when owner, capturer or percent change. The banner
// takes the sim's events straight from the pump (a capture, an incoming barrage, the end of the match), the same
// three CombatFx's OnGUI banner reported.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.UI
{
    public sealed class ObjectiveTracker
    {
        sealed class Row
        {
            public VisualElement Holder, Bullet; public Label Name, Progress;
            public byte LastOwner = 254, LastCapturing = 254; public int LastPct = -1;
        }

        readonly HudRefs refs;
        readonly SimHost host;
        readonly List<Row> rows = new List<Row>();
        float bannerUntil = -1f;
        bool bannerShown;

        public ObjectiveTracker(HudRefs refs, VisualTreeAsset rowTemplate, SimHost host)
        {
            this.refs = refs; this.host = host;
            var map = host.Local.Map;
            int n = map.Objectives.IsCreated ? map.Objectives.Length : 0;
            for (int o = 0; o < n; o++)
            {
                var def = map.Objectives[o];
                var r = new Row();
                r.Holder = rowTemplate != null ? rowTemplate.Instantiate() : RowFallback();
                r.Holder.pickingMode = PickingMode.Ignore;
                refs.ObjectivesList.Add(r.Holder);
                r.Bullet = r.Holder.Q("bullet"); r.Name = r.Holder.Q<Label>("objective-name"); r.Progress = r.Holder.Q<Label>("objective-progress");
                r.Name.text = NameOf(def);
                rows.Add(r);
            }
            refs.Objectives.EnableInClassList("is-empty", n == 0);
            host.Events.OnEvent += OnEvent;
        }

        public void Dispose() { if (host != null) host.Events.OnEvent -= OnEvent; }

        static string NameOf(in ObjectiveDef def)
        {
            string kind = def.Kind.ToString().ToUpperInvariant();
            return def.SideTeam == 0 ? "OUR " + kind : "ENEMY " + kind;
        }

        static VisualElement RowFallback()
        {
            var h = new VisualElement { name = "objective", pickingMode = PickingMode.Ignore }; h.AddToClassList("hud-objective");
            var b = new VisualElement { name = "bullet", pickingMode = PickingMode.Ignore }; b.AddToClassList("tw-bullet"); b.AddToClassList("tw-bullet--neutral"); h.Add(b);
            var n = new Label { name = "objective-name", pickingMode = PickingMode.Ignore }; n.AddToClassList("tw-caps"); n.AddToClassList("hud-objective__name"); h.Add(n);
            var p = new Label { name = "objective-progress", pickingMode = PickingMode.Ignore }; p.AddToClassList("tw-mono"); p.AddToClassList("hud-objective__progress"); h.Add(p);
            return h;
        }

        public void Refresh()
        {
            var sectors = host.Local.Sectors;
            if (sectors != null && sectors.States.IsCreated)
            {
                var map = host.Local.Map;
                int n = Mathf.Min(rows.Count, sectors.States.Length);
                for (int o = 0; o < n; o++)
                {
                    var st = sectors.States[o]; var def = map.Objectives[o]; var r = rows[o];
                    int pct = st.CaptureProgressTicks > 0 ? 100 * st.CaptureProgressTicks / Mathf.Max(1, def.CaptureTicks) : 0;
                    if (st.Owner != r.LastOwner || st.CapturingTeam != r.LastCapturing)
                    {
                        bool capturing = st.CapturingTeam != 255 && st.CaptureProgressTicks > 0;
                        r.Bullet.EnableInClassList("tw-bullet--mine", !capturing && st.Owner == 0);
                        r.Bullet.EnableInClassList("tw-bullet--theirs", !capturing && st.Owner == 1);
                        r.Bullet.EnableInClassList("tw-bullet--neutral", !capturing && st.Owner > 1);
                        r.Bullet.EnableInClassList("tw-bullet--capturing", capturing);
                        r.LastOwner = st.Owner; r.LastCapturing = st.CapturingTeam;
                    }
                    if (pct != r.LastPct) { r.Progress.text = pct > 0 ? PctText(pct) : ""; r.LastPct = pct; }
                }
            }
            // banner timeout
            if (bannerShown && Time.unscaledTime >= bannerUntil) { refs.Banner.EnableInClassList("is-visible", false); bannerShown = false; }
        }

        static readonly string[] pct = new string[101];
        static string PctText(int p) { p = Mathf.Clamp(p, 0, 100); return pct[p] ??= p + "%"; }

        void OnEvent(SimEvent ev)
        {
            switch (ev.Type)
            {
                case SimEventType.TrenchCaptured: Banner(HudText.CapturedBanner(ev.A, ev.B == 0), HudLayout.BannerSeconds, ev.B == 0 ? "tw-banner--victory" : "tw-banner--defeat"); break;
                case SimEventType.AbilityFired:
                    if (ev.B != 0 && (ev.A == (int)OffMapAbilityId.HeBarrage || ev.A == (int)OffMapAbilityId.CreepingBarrage)) Banner(HudText.IncomingBanner, 4f, "tw-banner--defeat");
                    break;
                case SimEventType.MatchEnded: Banner(ev.A == 0 ? HudText.VictoryBanner : HudText.DefeatBanner, 3600f, ev.A == 0 ? "tw-banner--victory" : "tw-banner--defeat"); break;
            }
        }

        public void Banner(string text, float seconds, string cls)
        {
            refs.BannerText.text = text;
            refs.BannerText.EnableInClassList("tw-banner--victory", cls == "tw-banner--victory");
            refs.BannerText.EnableInClassList("tw-banner--defeat", cls == "tw-banner--defeat");
            refs.Banner.EnableInClassList("is-visible", true);
            bannerUntil = Time.unscaledTime + seconds; bannerShown = true;
        }
    }
}
