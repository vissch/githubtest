// Phase: B6 (implemented) — the main menu: a stack of riveted plates over a dark field. SKIRMISH goes to the mission
// select; ARMOURY shows every unit's artwork; CAMPAIGN opens the strategic map and HOME FRONT the city (docs/21
// phase 6); SETTINGS and QUIT do what they say. A debrief that asked for the map (CampaignSession.ResumeMap) gets it
// pushed over the menu as soon as the menu binds.
// The backdrop is the key art (owner, 2026-09-25) with the title and menu on its dark left, over a shade that fades from
// near-black at the left edge to nothing past the middle, and a foot that darkens the bottom for the version line. USS
// has no gradients, so both are small textures painted here once and stretched.
using UnityEngine;
using UnityEngine.UIElements;
using TW.Presentation;

namespace TW.UI
{
    public sealed class MainMenuScreen : ShellScreen
    {
        public static readonly string[] RequiredNames = { "backdrop", "logo", "title", "subtitle", "menu-stack", "btn-skirmish", "btn-armoury", "btn-campaign", "btn-homefront", "btn-settings", "btn-quit", "version" };
        public override bool HidesHud => true;

        public override VisualTreeAsset Tree(ShellAssets a) => a?.MainMenu;

        protected override void OnBind()
        {
            var backdrop = Root.Q("backdrop");
            var tex = Router != null && Router.Assets != null ? Router.Assets.Backdrop : null;
            if (backdrop != null && tex != null) backdrop.style.backgroundImage = new StyleBackground(tex);
            var shade = Root.Q("keyart-shade");
            if (shade != null) shade.style.backgroundImage = new StyleBackground(shadeTex ??= Shade(256, 1, true, 0.88f, 0.58f));
            var foot = Root.Q("keyart-foot");
            if (foot != null) foot.style.backgroundImage = new StyleBackground(footTex ??= Shade(1, 128, false, 0.75f, 1f));
            Btn("btn-skirmish", () => Router?.Push(new MissionSelectScreen()));
            Btn("btn-armoury", () => Router?.Push(new ArmouryScreen()));
            Btn("btn-campaign", () => Router?.Push(new StrategicMapScreen()));
            Btn("btn-homefront", () => Router?.Push(new HomeFrontScreen()));
            Btn("btn-settings", () => Router?.Push(new SettingsScreen()));
            Btn("btn-quit", () => Router?.QuitGame());
            SetText("version", $"{GameLogo.Name}   BUILD {Application.version}   UNITY {Application.unityVersion}");
            if (CampaignSession.ResumeMap && Router != null) { CampaignSession.ResumeMap = false; Router.Push(new StrategicMapScreen()); }
        }

        public override void OnEscape() { }   // the root screen stays

        static Texture2D shadeTex, footTex;
        // painted once per Play session: ??= does not see a texture Unity destroyed when the last session ended
        static MainMenuScreen() => TW.Presentation.SceneStatics.Register(nameof(MainMenuScreen), () => { shadeTex = null; footTex = null; });

        /// <summary>
        /// A black ramp: alpha `strongest` at the dark end falling (smoothstep) to 0 at `reach` of the way across; along x
        /// from the left, or along y from the bottom. Stretched over its element, so a few pixels are enough.
        /// </summary>
        static Texture2D Shade(int w, int h, bool alongX, float strongest, float reach)
        {
            var t = new Texture2D(w, h, TextureFormat.RGBA32, false) { wrapMode = TextureWrapMode.Clamp, filterMode = FilterMode.Bilinear, hideFlags = HideFlags.HideAndDontSave };
            var px = new Color32[w * h];
            for (int y = 0; y < h; y++) for (int x = 0; x < w; x++)
            {
                float u = alongX ? (x + 0.5f) / w : (y + 0.5f) / h;   // texture row 0 is the bottom
                float k = 1f - Mathf.SmoothStep(0f, 1f, Mathf.Clamp01(u / reach));
                px[y * w + x] = new Color32(6, 7, 8, (byte)Mathf.RoundToInt(255f * strongest * k));
            }
            t.SetPixels32(px); t.Apply(false, true);
            return t;
        }
    }
}
