// Phase: B6 (implemented) — the screens over the game: a stack on one UIDocument that outlives every scene load.
// ShellBoot puts one ShellRoot (DontDestroyOnLoad) in the game; this component owns its UIDocument at sorting order
// 100 on the same PanelSettings as the HUD, so shell plates sit above HUD elements in one panel and panel.Pick sees
// them as one interface. On every scene load it rebinds to the scene's SimHost (clock, stats, camera settings) or,
// with no host, shows the main menu. Esc opens the pause menu in a match unless an armed ability consumed it; a
// finished match brings the debrief a beat after the capture reads on screen. Screens are plain classes over a
// VisualElement (ShellScreen), so EditMode tests bind them with no panel.
// The Proving Ground (2026-09-28): a match started from its launch screen gets its panel pushed when the scene binds;
// F8 opens the panel over any match and folds it when it is up. The panel is an Overlay: Esc passes it by.
// The feedback capture (2026-10-07): F10, anywhere the shell is, writes the game as it is and takes the picture
// (FeedbackCapture), and a frame later puts up the box for his words (FeedbackScreen), which holds the match.
// A capture that could not be written, and one that was saved, say so in a line low on the screen (Notice): the line
// is no screen of the stack, so it holds nothing and outlives the box it speaks for.
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
        /// <summary>The screen under the top one, or null.</summary>
        public ShellScreen Under => stack.Count > 1 ? stack[stack.Count - 2] : null;

        UIDocument doc;
        VisualElement root;
        HudController hud;
        readonly List<ShellScreen> stack = new List<ShellScreen>();
        bool debriefShown; float endedAt = -1f;
        /// <summary>Takes the picture of a feedback capture. A field, so a test stands in for it; a batch run has no screen
        /// and is asked for nothing.</summary>
        public System.Action<string> Shoot = path => { if (!Application.isBatchMode) ScreenCapture.CaptureScreenshot(path); };
        FeedbackRecord captured; string capturedIn;   // written this frame: its box comes up on the next, so the box is not in the picture
        int boxClosedFrame = -1;                      // the frame a box went away: the same frame's F10 is not a new capture
        VisualElement notice; float noticeUntil; bool going;
        public const string NoticeTree = "Shell/Notice";
        public const float NoticeSeconds = 2.5f;
        public const string NotCaptured = "THE CAPTURE COULD NOT BE SAVED: ";
        /// <summary>The line on screen now, or null.</summary>
        public string NoticeText { get; private set; }

        void Awake()
        {
            if (Instance != null && Instance != this) { Destroy(gameObject); return; }
            Instance = this;
            doc = GetComponent<UIDocument>();
            if (Assets == null) Assets = ShellAssets.Load();
            SceneManager.sceneLoaded += OnSceneLoaded;
            Application.logMessageReceived += FeedbackCapture.Heard;
        }

        void OnDestroy()
        {
            SceneManager.sceneLoaded -= OnSceneLoaded;
            if (Instance == this) { Application.logMessageReceived -= FeedbackCapture.Heard; Instance = null; }   // a second router that destroyed itself in Awake never subscribed
            going = true;
            // the game quits, or Play ends, with a capture open: it is closed with what he had typed
            DropCaptured();
            foreach (var s in stack) if (s is FeedbackScreen open) open.Abandon();
        }

        void Start() => Bind(SceneManager.GetActiveScene());

        void OnSceneLoaded(Scene s, LoadSceneMode mode)
        {
            SceneStatics.Reset();
            DropCaptured();
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
                if (MatchLaunch.Running != null && MatchLaunch.Running.ProvingGround) Push(new ProvingGroundPanel());
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

        /// <summary>Pops down to the nearest screen of a kind below the top, if there is one (true), so a link back to it
        /// from further up the stack does not stack another copy.</summary>
        public bool PopTo<T>() where T : ShellScreen
        {
            int at = -1;
            for (int i = stack.Count - 2; i >= 0; i--) if (stack[i] is T) { at = i; break; }
            if (at < 0) return false;
            while (stack.Count - 1 > at) Pop();
            return true;
        }

        public void Pop()
        {
            if (stack.Count == 0) return;
            var s = stack[stack.Count - 1];
            stack.RemoveAt(stack.Count - 1);
            if (s is FeedbackScreen) boxClosedFrame = Time.frameCount;
            s.Root?.RemoveFromHierarchy();   // before Unbind, which clears Root: the other way round no popped screen ever
            s.Unbind();                      // left the panel, so the main menu stayed drawn over every match in a player
            Top?.OnUncovered();
            ApplyHolds();
        }

        /// <summary>Take one screen off the stack wherever it is in it (a box that closes itself must not pop a screen
        /// that came up over it).</summary>
        public void Remove(ShellScreen screen)
        {
            int at = stack.IndexOf(screen);
            if (at < 0) return;
            if (at == stack.Count - 1) { Pop(); return; }
            stack.RemoveAt(at);
            if (screen is FeedbackScreen) boxClosedFrame = Time.frameCount;
            screen.Root?.RemoveFromHierarchy();
            screen.Unbind();
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
            FeedbackCapture.Frame(Time.unscaledDeltaTime);
            if (NoticeText != null && Time.unscaledTime >= noticeUntil) ClearNotice();
            if (capturedIn != null) OpenFeedbackBox();
            for (int i = 0; i < stack.Count; i++) stack[i].Tick();
            // Esc: the top screen's business, else the pause menu (unless an armed ability just used it)
            if (!InputFocus.Listening && KeyMap.DownRaw(GameAction.Menu) && !InputFocus.EscapeConsumed)
            {
                if (Top != null && !Top.Overlay) Top.OnEscape();
                else if (Host != null && !debriefShown)
                {
                    var panel = Camera.main != null ? Camera.main.GetComponent<TestPanel>() : null;
                    if (panel == null || panel.Armed == OffMapAbilityId.None) Push(new PauseMenuScreen());
                }
            }
            if (Host != null && !debriefShown && !InputFocus.Listening && !InputFocus.Modal && ProvingKeyDown()) ToggleProvingGround();
            // F10, read raw: it works over a menu, the pause screen and the debrief too. Not while a key is being rebound,
            // nor on the frame a rebind took this press as its key. With the box up the key closes it only when it is no
            // key he could be typing (rebound to a letter, the letter is his words').
            if (FeedbackKeyActs(KeyMap.DownRaw(GameAction.Feedback), FunctionKeyDown(GameAction.Feedback))) Feedback();
            // the debrief, a beat after the end
            if (Host != null && Host.Local != null && !debriefShown && Host.Local.World.WinnerTeam >= 0)
            {
                if (endedAt < 0f) endedAt = Time.unscaledTime;
                else if (Time.unscaledTime - endedAt >= EndDelaySeconds) ShowDebrief();
            }
        }

        /// <summary>Whether this frame's press of the feedback key (`down`) is the command. Not while a rebind waits for its
        /// key, and not on the frame a rebind ended with a key (InputFocus.CaptureConsumed): that press was the key to
        /// bind. With the box up (Typing) only a key he could not be typing counts (`functionKeyDown`).</summary>
        public static bool FeedbackKeyActs(bool down, bool functionKeyDown)
            => !InputFocus.Listening && !InputFocus.CaptureConsumed && down && (!InputFocus.Typing || functionKeyDown);

        static bool FunctionKeyDown(GameAction a)
        {
            var kb = UnityEngine.InputSystem.Keyboard.current;
            if (kb == null) return false;
            var p = KeyMap.Primary(a); var s = KeyMap.Secondary(a);
            return (FeedbackScreen.ClosesTheBox(p) && kb[p].wasPressedThisFrame) || (FeedbackScreen.ClosesTheBox(s) && kb[s].wasPressedThisFrame);
        }

        /// <summary>F8, not a GameAction: a test tool's key is not the player's to rebind, and an action added to the list
        /// would show in the settings' controls page.</summary>
        static bool ProvingKeyDown()
        {
            var kb = UnityEngine.InputSystem.Keyboard.current;
            return kb != null && kb[UnityEngine.InputSystem.Key.F8].wasPressedThisFrame;
        }

        /// <summary>The Proving Ground's panel over this match: opened if it is not up, folded or unfolded if it is.</summary>
        public void ToggleProvingGround()
        {
            if (Host == null) return;
            foreach (var s in stack)
                if (s is ProvingGroundPanel panel) { panel.Fold(!panel.Folded); return; }
            if (Top == null) Push(new ProvingGroundPanel());
        }

        /// <summary>F10. With the box up: save his words and close it. Otherwise capture: the game as it is goes to disk now
        /// (the state file and, at the end of this frame, the screen with the HUD on it), and the box for his words comes
        /// up on the next frame. Its being modal is what holds the match; nothing is held on this frame, so the picture
        /// shows the HUD as he saw it and not a PAUSED plate. On a menu there is no match and the file says so.
        /// A box that went away on this frame (Esc and F10 pressed together) is not answered with a new capture.
        /// A capture that cannot be written says so on screen, and leaves no folder the board would wait on.</summary>
        public void Feedback()
        {
            if (capturedIn != null || boxClosedFrame == Time.frameCount) return;
            for (int i = stack.Count - 1; i >= 0; i--)
                if (stack[i] is FeedbackScreen open) { open.Close(true); return; }
            ClearNotice();   // the line about the last capture is not part of this one's picture
            string folder = null;
            try
            {
                var record = FeedbackCapture.Gather(Host, Clock, Stats, Camera.main, System.DateTime.Now, Host != null ? Hud : null);
                folder = FeedbackCapture.Claim(record);
                FeedbackCapture.Save(record, folder);
                Shoot?.Invoke(System.IO.Path.Combine(folder, FeedbackCapture.ShotName));
                captured = record; capturedIn = folder;
            }
            catch (System.Exception e)
            {
                Debug.LogWarning($"ShellRouter: the feedback capture was not written: {e.Message}");
                if (folder != null) FeedbackCapture.Withdraw(folder);   // with a state file it stands, without words; without one it is taken back
                Notice(NotCaptured + e.Message.ToUpperInvariant());
            }
        }

        /// <summary>A line low on the screen for a few seconds: what just happened to a capture. Not a screen of the
        /// stack: it holds nothing, takes no click and is still there when the box it speaks for has gone.</summary>
        public void Notice(string text, float seconds = NoticeSeconds)
        {
            if (going || root == null || string.IsNullOrEmpty(text)) return;
            ClearNotice();
            NoticeText = text; noticeUntil = Time.unscaledTime + seconds;
            var tree = Resources.Load<VisualTreeAsset>(NoticeTree);
            if (tree == null) { Debug.LogWarning("ShellRouter: no UXML for the notice line (Resources/" + NoticeTree + "): " + text); return; }
            notice = tree.Instantiate();
            notice.style.position = Position.Absolute; notice.style.left = 0; notice.style.top = 0; notice.style.right = 0; notice.style.bottom = 0;
            notice.pickingMode = PickingMode.Ignore;
            var label = notice.Q<Label>("notice-text");
            if (label != null) label.text = text;
            root.Add(notice);
        }

        void ClearNotice()
        {
            notice?.RemoveFromHierarchy();
            notice = null; NoticeText = null;
        }

        void OpenFeedbackBox()
        {
            var record = captured; string folder = capturedIn;
            captured = null; capturedIn = null;
            Push(new FeedbackScreen(record, folder));
            if (!(Top is FeedbackScreen)) FeedbackCapture.Done(folder);   // no UXML for the box: the capture stands without words
        }

        /// <summary>A capture whose box never came up (the scene went away first) is closed as it is.</summary>
        void DropCaptured()
        {
            if (capturedIn == null) return;
            FeedbackCapture.Done(capturedIn);
            captured = null; capturedIn = null;
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
