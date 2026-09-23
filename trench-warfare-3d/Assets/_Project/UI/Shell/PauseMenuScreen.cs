// Phase: B6 (implemented) — the Esc menu over a match: Resume, Settings, Restart, Surrender, Quit to menu.
// Dims the field and holds the sim (MatchClock.Hold.Menu via the router); the tactical pause is separate and stays
// as it was. Restart and Quit ask once; Surrender ends the match through the sim (CommandType.Surrender) so the
// debrief path runs for real.
using UnityEngine.UIElements;
using TW.Presentation;

namespace TW.UI
{
    public sealed class PauseMenuScreen : ShellScreen
    {
        public static readonly string[] RequiredNames = { "pause-plate", "pause-title", "btn-resume", "btn-settings", "btn-restart", "btn-surrender", "btn-quit-menu", "confirm", "confirm-text", "btn-confirm-yes", "btn-confirm-no", "pause-hint" };
        VisualElement confirm; Label confirmText; System.Action pending;

        public override VisualTreeAsset Tree(ShellAssets a) => a?.PauseMenu;

        protected override void OnBind()
        {
            confirm = Root.Q("confirm"); confirmText = Root.Q<Label>("confirm-text");
            confirm?.EnableInClassList("tw-hidden", true);
            Btn("btn-resume", () => Router?.Pop());
            Btn("btn-settings", () => Router?.Push(new SettingsScreen()));
            Btn("btn-restart", () => Ask("RESTART THE MISSION FROM THE START?", () => Router?.RestartMatch()));
            Btn("btn-surrender", () => Ask("SURRENDER THE SECTOR?", () => Router?.Surrender()));
            Btn("btn-quit-menu", () => Ask("QUIT TO THE MAIN MENU? THE MATCH IS LOST.", () => Router?.QuitToMenu()));
            Btn("btn-confirm-yes", () => { var p = pending; pending = null; confirm?.EnableInClassList("tw-hidden", true); p?.Invoke(); });
            Btn("btn-confirm-no", () => { pending = null; confirm?.EnableInClassList("tw-hidden", true); });
            SetText("pause-hint", $"{KeyMap.Display(KeyMap.Primary(GameAction.TacticalPause))}  TACTICAL PAUSE      {KeyMap.Display(KeyMap.Primary(GameAction.Menu))}  RESUME      {KeyMap.Display(KeyMap.Primary(GameAction.HudToggle))}  OLD / NEW HUD");
        }

        void Ask(string text, System.Action action)
        {
            pending = action;
            if (confirmText != null) confirmText.text = text;
            confirm?.EnableInClassList("tw-hidden", false);
        }

        public override void OnEscape()
        {
            if (pending != null) { pending = null; confirm?.EnableInClassList("tw-hidden", true); return; }
            Router?.Pop();
        }
    }
}
