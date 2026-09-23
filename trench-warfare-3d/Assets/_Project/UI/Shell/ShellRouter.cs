// Phase: B6 (implemented) — the screens over the game: a stack on one UIDocument that outlives every scene load.
// ShellBoot puts one ShellRoot (DontDestroyOnLoad) in the game; this component owns its UIDocument at sorting order
// 100 on the same PanelSettings as the HUD, so shell plates sit above HUD elements in one panel and panel.Pick sees
// them as one interface. On every scene load it rebinds to the scene's SimHost (clock, stats, camera settings) or,
// with no host, shows the main menu. Esc opens the pause menu in a match unless an armed ability consumed it; a
// finished match brings the debrief a beat after the capture reads on screen. Screens are plain classes over a
// VisualElement (ShellScreen), so EditMode tests bind them with no panel.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Sim.Match;

namespace TW.UI
{
    [DefaultExecutionOrder(500)]   // after TestPanel.Update, so an Esc it consumed is visible
    [RequireComponent(typeof(UIDocument))]
    public sealed class ShellRouter : MonoBehaviour
    {
        public static ShellRouter Instance { get; private set; }
        public const float EndDelaySeconds = 2.5f;

        public ShellAssets Assets;
        public SimHost Host { get; private set; }
        public MatchClock Clock { get; private set; }
        public MatchStats Stats { get; private set; }
        public HudController Hud => hud != null ? hud : (hud = FindFirstObjectByType<HudController>(FindObjectsInactive.Include));
        public bool InMatch => Host != null;
        public int Depth => stack.Count;
        public ShellScreen Top => stack.Count > 0 ? stack[stack.Count - 1] : null;

        UIDocument doc;
        VisualElement root;
        HudController hud;
        readonly List<ShellScreen> stack = new List<ShellScreen>();
        bool debriefShown; float endedAt = -1f;

        void Awake()
        {
            if (Instance != null && Instance != this) { Destroy(gameObject); return; }
            Instance = this;
            doc = GetComponent<UIDocument>();
            if (Assets == null) Assets = ShellAssets.Load();
            SceneManager.sceneLoaded += OnSceneLoaded;
        }

        void OnDestroy()
        {
            SceneManager.sceneLoaded -= OnSceneLoaded;
            if (Instance == this) Instance = null;
        }

        void Start() => Bind(SceneManager.GetActiveScene());

        void OnSceneLoaded(Scene s, LoadSceneMode mode)
        {
            SceneStatics.Reset();
            ClearStack();
            Bind(s);
        }

        void Bind(Scene s)
        {
            root = doc.rootVisualElement;
            hud = null;
            Host = FindFirstObjectByType<SimHost>();
            debriefShown = false; endedAt = -1f;
            var settings = SettingsStore.Current;
            if (Host != null)
            {
                Clock = MatchClock.For(Host);
                Stats = MatchStats.Attach(Host);
                SettingsApplier.ApplyCamera(settings);
                SettingsApplier.ApplyInterface(settings);
            }
            else
            {
                Clock = null; Stats = null;
                if (Assets != null && Assets.MainMenu != null) Push(new MainMenuScreen());
            }
        }

        // ---- the stack --------------------------------------------------------------------------------------------
        public void Push(ShellScreen screen)
        {
            var tree = screen.Tree(Assets);
            if (tree == null) { Debug.LogWarning($"ShellRouter: no UXML for {screen.GetType().Name}; run TW/UI/Build Shell Assets."); return; }
            var ve = tree.Instantiate();
            ve.style.position = Position.Absolute; ve.style.left = 0; ve.style.top = 0; ve.style.right = 0; ve.style.bottom = 0;
            ve.pickingMode = PickingMode.Ignore;
            root.Add(ve);
            if (Top != null) Top.OnCovered();
            stack.Add(screen);
            screen.Bind(ve, this);
            ApplyHolds();
        }

        public void Pop()
        {
            if (stack.Count == 0) return;
            var s = stack[stack.Count - 1];
            stack.RemoveAt(stack.Count - 1);
            s.Unbind();
            s.Root?.RemoveFromHierarchy();
            Top?.OnUncovered();
            ApplyHolds();
        }

        public void Replace(ShellScreen screen) { Pop(); Push(screen); }

        public void ClearStack() { while (stack.Count > 0) Pop(); }

        void ApplyHolds()
        {
            bool modal = false, hideHud = false;
            foreach (var s in stack) { modal |= s.Modal; hideHud |= s.HidesHud; }
            InputFocus.Modal = modal;
            if (Clock != null)
            {
                if (modal) Clock.Add(MatchClock.Hold.Menu); else Clock.Remove(MatchClock.Hold.Menu);
                if (debriefShown) Clock.Add(MatchClock.Hold.Debrief);
            }
            Hud?.SetVisible(!hideHud);
        }

        // ---- per frame ----------------------------------------------------------------------------------------------
        void Update()
        {
            for (int i = 0; i < stack.Count; i++) stack[i].Tick();
            // Esc: the top screen's business, else the pause menu (unless an armed ability just used it)
            if (!InputFocus.Listening && KeyMap.DownRaw(GameAction.Menu) && !InputFocus.EscapeConsumed)
            {
                if (Top != null) Top.OnEscape();
                else if (Host != null && !debriefShown)
                {
                    var panel = Camera.main != null ? Camera.main.GetComponent<TestPanel>() : null;
                    if (panel == null || panel.Armed == OffMapAbilityId.None) Push(new PauseMenuScreen());
                }
            }
            // the debrief, a beat after the end
            if (Host != null && Host.Local != null && !debriefShown && Host.Local.World.WinnerTeam >= 0)
            {
                if (endedAt < 0f) endedAt = Time.unscaledTime;
                else if (Time.unscaledTime - endedAt >= EndDelaySeconds) ShowDebrief();
            }
        }

        public void ShowDebrief(string reason = null)
        {
            if (debriefShown || Host == null) return;
            debriefShown = true;
            ClearStack();
            var report = Stats != null ? Stats.Report(MatchLaunch.Running, reason) : new MatchReport();
            Push(new DebriefScreen(report));
            ApplyHolds();
        }

        // ---- actions the screens call ---------------------------------------------------------------------------------
        public void RestartMatch() { if (Host != null) Host.Restart(); }
        public void QuitToMenu() => MatchLaunch.QuitToMenu();
        public void StartMission(MatchLaunch.Request r) => MatchLaunch.Start(r);
        public void Surrender()
        {
            if (Host == null || Host.Local == null) return;
            var w = Host.Local.World;
            Host.Issue(new TW.Sim.SimCommand { Tick = w.Tick, Player = 0, Type = TW.Sim.CommandType.Surrender });
            ClearStack();
        }
        public void QuitGame()
        {
#if UNITY_EDITOR
            UnityEditor.EditorApplication.isPlaying = false;
#else
            Application.Quit();
#endif
        }
    }
}
