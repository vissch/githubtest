// Phase: B6 (implemented) — the debrief: VICTORY or DEFEAT, the numbers for both sides, and where to go next.
// Reads a MatchReport (MatchStats) once at bind. View Field hides the plate but keeps the match held; Esc brings it
// back. Save Replay waits for the recorder to be wired into SimHost (step B7). A campaign mission (CampaignSession)
// pays its gold on a win, once: the session's Awarded latch guards RESTART and a re-bind, and a mission already won
// pays nothing (docs/21 phase 6); CONTINUE then goes back to the map.
// A match the Proving Ground's panel wrote into (ProvingGround.Tooled) pays nothing and records nothing on a win:
// the row says so (owner, 2026-10-07: F8 stays in every match, and a match in which the panel was used pays no gold).
using UnityEngine.UIElements;
using TW.Presentation;

namespace TW.UI
{
    public sealed class DebriefScreen : ShellScreen
    {
        public static readonly string[] Rows = { "men-lost", "vehicles-lost", "men-fielded", "kills", "shots", "accuracy", "silver-earned", "silver-left", "trenches-taken", "trenches-held", "objectives-held", "abilities-fired" };
        public static readonly string[] RequiredNames = { "debrief-plate", "result", "result-sub", "stats-table", "duration", "seed-line", "gold-row", "gold-earned", "gold-total", "btn-replay", "btn-restart", "btn-view-field", "btn-continue", "view-hint" };
        public const string ToTheMap = "TO THE MAP", NoGoldForADefeat = "NO GOLD FOR A DEFEAT", NoGoldForADraw = "NO GOLD FOR A DRAW", NoGoldAgain = "ALREADY WON: NO GOLD AGAIN", NoGoldAfterTools = "TEST PANEL USED: NO GOLD";
        public override bool HidesHud => !viewing;

        readonly MatchReport report;
        VisualElement plate, screen, logo, hint; bool viewing;

        public DebriefScreen(MatchReport report) { this.report = report ?? new MatchReport(); }

        public override VisualTreeAsset Tree(ShellAssets a) => a?.Debrief;

        protected override void OnBind()
        {
            plate = Root.Q("debrief-plate");
            screen = Root.Q("debrief-screen"); logo = Root.Q("debrief-logo"); hint = Root.Q("view-hint");   // the screen classes sit on the named child, not on Root (the TemplateContainer)
            bool won = report.Winner == 0;
            var result = Root.Q<Label>("result");
            if (result != null) { result.text = report.Winner < 0 ? "DRAWN" : won ? "VICTORY" : "DEFEAT"; result.EnableInClassList("tw-accent", won); result.EnableInClassList("tw-alarm", !won); }
            Root.Q("stamp")?.EnableInClassList("tw-stamp--victory", won);
            Root.Q("stamp")?.EnableInClassList("tw-stamp--defeat", !won && report.Winner >= 0);
            SetText("result-sub", string.IsNullOrEmpty(report.EndReason) ? "" : report.EndReason + " AT " + MatchReport.Clock(report.DurationSeconds));
            SetText("duration", "DURATION " + MatchReport.Clock(report.DurationSeconds));
            SetText("seed-line", $"{(string.IsNullOrEmpty(report.Title) ? "SHELLED WOOD" : report.Title)}   {(string.IsNullOrEmpty(report.Difficulty) ? "" : report.Difficulty + "   ")}SEED {report.Seed}");
            Fill("men-lost", report.MenLost); Fill("vehicles-lost", report.VehiclesLost); Fill("men-fielded", report.MenFielded);
            Fill("kills", report.Kills); Fill("shots", report.Shots);
            Cell("accuracy", 0, $"{report.Accuracy(0):0}%"); Cell("accuracy", 1, $"{report.Accuracy(1):0}%");
            Cell("silver-earned", 0, Earned(0)); Cell("silver-earned", 1, Earned(1));
            Fill("silver-left", report.SilverNow); Fill("trenches-taken", report.TrenchesTaken); Fill("trenches-held", report.TrenchesHeld);
            Fill("objectives-held", report.ObjectivesHeld); Fill("abilities-fired", report.AbilitiesFired);
            var replay = Btn("btn-replay", null); replay?.SetEnabled(false);
            Btn("btn-restart", () => Router?.RestartMatch());
            Btn("btn-view-field", ViewField);
            Btn("btn-continue", () => { if (CampaignSession.Active) CampaignSession.ResumeMap = true; Router?.QuitToMenu(); });
            Award();
        }

        /// <summary>A campaign win pays the mission's gold once and records it; the row says what happened.</summary>
        void Award()
        {
            var row = Root.Q("gold-row");
            if (!CampaignSession.Active) { row?.EnableInClassList("tw-hidden", true); return; }
            var profile = ProfileStore.Current;
            bool won = report.Winner == 0;
            bool tooled = ProvingGround.Tooled;   // the Proving Ground's panel wrote into this match: it pays nothing
            int earned = 0; bool paid = false;
            if (won && !tooled && !CampaignSession.Awarded)
            {
                var node = CampaignGraph.Find(CampaignSession.NodeId);
                if (node != null && profile.Complete(node.Id, CampaignSession.MissionIndex)) { earned = CampaignGraph.Reward(node, CampaignSession.MissionIndex); profile.Gold += earned; paid = true; }
                profile.LastNode = CampaignSession.NodeId;
                CampaignSession.Awarded = true; CampaignSession.LastAward = earned; CampaignSession.AwardedFor = report;
                ProfileStore.Save(profile);
            }
            else if (won && !tooled) { earned = CampaignSession.LastAward; paid = earned > 0 && ReferenceEquals(CampaignSession.AwardedFor, report); }   // only this match's debrief shows what it paid
            row?.EnableInClassList("tw-hidden", false);
            SetText("gold-earned", !won ? (report.Winner < 0 ? NoGoldForADraw : NoGoldForADefeat) : tooled ? NoGoldAfterTools : paid ? "GOLD " + earned.ToString("+#;-#;0") : NoGoldAgain);
            SetText("gold-total", "WAR CHEST " + profile.Gold + " GOLD");
            var cont = Root.Q<Button>("btn-continue");
            if (cont != null) cont.text = ToTheMap;
        }

        string Earned(int t) => (report.SilverNow[t] - report.SilverStart[t]).ToString("+#;-#;0");
        void Fill(string row, int[] v) { Cell(row, 0, v[0].ToString()); Cell(row, 1, v[1].ToString()); }
        void Cell(string row, int team, string text) => SetText($"{row}-{(team == 0 ? "you" : "enemy")}", text);

        /// <summary>VIEW THE FIELD: one named entry point for the button and for the PlayMode test.</summary>
        public void ViewField() => SetViewing(true);

        void SetViewing(bool on)
        {
            viewing = on;
            plate?.EnableInClassList("tw-hidden", on);
            // The fill and the vignette are classes of debrief-screen; Root is the TemplateContainer around it, so a
            // class toggled there reached nothing and the opaque tw-screen fill stayed over the match.
            screen?.EnableInClassList("shell-screen--clear", on);
            screen?.EnableInClassList("tw-dim", !on);
            logo?.EnableInClassList("tw-hidden", on);
            hint?.EnableInClassList("tw-hidden", !on);
            Router?.Hud?.SetVisible(on);
        }

        public override void OnEscape() { if (viewing) SetViewing(false); }
    }
}
