// Phase: B6 (implemented) — the main menu: a stack of riveted plates over a dark field. SKIRMISH goes to the mission
// select; ARMOURY shows every unit's artwork; CAMPAIGN waits for the A7 campaign map; SETTINGS and QUIT do what they say.
using UnityEngine;
using UnityEngine.UIElements;

namespace TW.UI
{
    public sealed class MainMenuScreen : ShellScreen
    {
        public static readonly string[] RequiredNames = { "backdrop", "title", "subtitle", "menu-stack", "btn-skirmish", "btn-armoury", "btn-campaign", "btn-settings", "btn-quit", "version" };
        public override bool HidesHud => true;

        public override VisualTreeAsset Tree(ShellAssets a) => a?.MainMenu;

        protected override void OnBind()
        {
            var backdrop = Root.Q("backdrop");
            var tex = Router != null && Router.Assets != null ? Router.Assets.Backdrop : null;
            if (backdrop != null && tex != null) backdrop.style.backgroundImage = new StyleBackground(tex);
            Btn("btn-skirmish", () => Router?.Push(new MissionSelectScreen()));
            Btn("btn-armoury", () => Router?.Push(new ArmouryScreen()));
            var campaign = Btn("btn-campaign", null); campaign?.SetEnabled(false);
            Btn("btn-settings", () => Router?.Push(new SettingsScreen()));
            Btn("btn-quit", () => Router?.QuitGame());
            SetText("version", $"TRENCH WARFARE 3D   BUILD {Application.version}   UNITY {Application.unityVersion}");
        }

        public override void OnEscape() { }   // the root screen stays
    }
}
