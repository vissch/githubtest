// Phase: B6 (implemented) — the debrief: VICTORY or DEFEAT, the numbers for both sides, and where to go next.
// Reads a MatchReport (MatchStats) once at bind. View Field hides the plate but keeps the match held; Esc brings it
// back. Save Replay waits for the recorder to be wired into SimHost (step B7).
using UnityEngine.UIElements;
using TW.Presentation;

namespace TW.UI
{
    public sealed class DebriefScreen : ShellScreen
    {
        public static readonly string[] Rows = { "men-lost", "vehicles-lost", "men-fielded", "kills", "shots", "accuracy", "silver-earned", "silver-left", "trenches-taken", "trenches-held", "objectives-held", "abilities-fired" };
        public static readonly string[] RequiredNames = { "debrief-plate", "result", "result-sub", "stats-table", "duration", "seed-line", "btn-replay", "btn-restart", "btn-view-field", "btn-continue" };
        public override bool HidesHud => !viewing;

        readonly MatchReport report;
        VisualElement plate; bool viewing;

        public DebriefScreen(MatchReport report) { this.report = report ?? new MatchReport(); }

        public override VisualTreeAsset Tree(ShellAssets a) => a?.Debrief;

        protected override void OnBind()
        {
            plate = Root.Q("debrief-plate");
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
            Btn("btn-view-field", () => SetViewing(true));
            Btn("btn-continue", () => Router?.QuitToMenu());
        }

        string Earned(int t) => (report.SilverNow[t] - report.SilverStart[t]).ToString("+#;-#;0");
        void Fill(string row, int[] v) { Cell(row, 0, v[0].ToString()); Cell(row, 1, v[1].ToString()); }
        void Cell(string row, int team, string text) => SetText($"{row}-{(team == 0 ? "you" : "enemy")}", text);

        void SetViewing(bool on)
        {
            viewing = on;
            plate?.EnableInClassList("tw-hidden", on);
            Root.EnableInClassList("tw-dim", !on);
            Router?.Hud?.SetVisible(on);
        }

        public override void OnEscape() { if (viewing) SetViewing(false); }
    }
}
