// Phase: B6 (implemented) — the battle HUD in UI Toolkit: the playing interface, styled after Dust Front.
// One UIDocument on the HUD object; this component builds the tree from BattleHud.uxml once the sim exists, wires
// every button to SimHost.Issue (the same lockstep door the IMGUI HUD used), and refreshes the labels once a frame
// from the sim's numbers, touching only what changed. It registers the pointer mask on HudBridge so the click that
// arms a barrage on a card is never also the click that fires it, and it steps down for the old IMGUI HUD when
// HudBridge.UseToolkitHud is off (F9 flips it during the flag window).
// Runs after TacticalCamera and CameraShake (order 11000) so the trench order clusters land on the shaken frame.
using UnityEngine;
using UnityEngine.InputSystem;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Sim;
using TW.Sim.Match;

namespace TW.UI
{
    [DefaultExecutionOrder(11000)]
    [RequireComponent(typeof(UIDocument))]
    public sealed class HudController : MonoBehaviour
    {
        public SimHost Host;
        public TestPanel Panel;
        public TacticalCamera Cam;
        public MatchClock Clock { get; private set; }
        public HudRefs Refs => refs;
        /// <summary>False while a shell screen is up: the HUD keeps drawing but takes no orders.</summary>
        public bool Interactive => !InputFocus.Modal;

        UIDocument doc;
        HudRefs refs;
        HudMinimap minimap;
        TrenchOrderCluster clusters;
        ObjectiveTracker objectives;
        HudDialogue dialogue;
        HudCommentary commentary;
        public HudDialogue Dialogue => dialogue;
        SelectionController selection;
        SelectionPanel selectionPanel;
        /// <summary>What the player has selected on the field (inspecting and groups; the trenches keep the orders).</summary>
        public SelectionController Selection => selection;
        HudTooltip tooltip;
        HudHotkeys hotkeys;
        BattleHud legacy;
        bool built, visible = true, flagOn;
        int pickFrame = -1; bool pickHit;
        float nextLegacyCheck;
        static readonly ProfilerMarkerScope refreshMarker = new ProfilerMarkerScope("TW.Hud.Refresh");

        void OnEnable()
        {
            doc = GetComponent<UIDocument>();
            if (Host == null) Host = FindFirstObjectByType<SimHost>();
            var main = Camera.main;
            if (main != null)
            {
                if (Panel == null) Panel = main.GetComponent<TestPanel>();
                if (Cam == null) Cam = main.GetComponent<TacticalCamera>();
            }
            ApplyFlag();
        }

        void OnDisable()
        {
            if (HudBridge.PointerOverUi == PointerOverHud) HudBridge.PointerOverUi = null;
            objectives?.Dispose(); objectives = null;
            commentary?.Dispose(); commentary = null;
            minimap?.Dispose(); minimap = null;
            selection?.Dispose(); selection = null; selectionPanel = null;
            built = false;   // OnEnable after this (a live reload of the sheets, a toggle) builds again from a fresh tree
            if (legacy != null && !flagOn) legacy.enabled = true;
        }

        /// <summary>Read HudBridge.UseToolkitHud and step in or out accordingly.</summary>
        public void ApplyFlag()
        {
            flagOn = HudBridge.UseToolkitHud;
            HudHotkeys.LegacyOverlayActive = !flagOn;   // DebugOverlay handles 1-5 / Space / Backspace / L only while the IMGUI HUD is the HUD
            var root = doc != null ? doc.rootVisualElement : null;
            if (root != null) root.style.display = flagOn && visible ? DisplayStyle.Flex : DisplayStyle.None;
            if (flagOn) HudBridge.PointerOverUi = PointerOverHud;
            else if (HudBridge.PointerOverUi == PointerOverHud) HudBridge.PointerOverUi = null;
            if (legacy != null) legacy.enabled = !flagOn;
            if (!flagOn && Panel != null) BattleHud.MinimapRect = default;
        }

        public void SetVisible(bool on)
        {
            visible = on;
            var root = doc != null ? doc.rootVisualElement : null;
            if (root != null) root.style.display = flagOn && visible ? DisplayStyle.Flex : DisplayStyle.None;
        }

        // ---- build --------------------------------------------------------------------------------------------------
        bool TryBuild()
        {
            if (Host == null || Host.Local == null || doc == null || doc.rootVisualElement == null) return false;
            var root = doc.rootVisualElement;
            if (refs != null)
            {
                // a rebuild: let go of the old pieces and start from the document's own tree, so cards are not added twice
                objectives?.Dispose(); objectives = null;
                commentary?.Dispose(); commentary = null;
                minimap?.Dispose(); minimap = null;
                selection?.Dispose(); selection = null; selectionPanel = null;
                if (doc.visualTreeAsset != null) { root.Clear(); doc.visualTreeAsset.CloneTree(root); }
            }
            var w = Host.Local.World;
            var roster = new RosterEntry[RosterEntry.SlotCount];
            for (int s = 0; s < roster.Length; s++) roster[s] = w.Roster[s];   // player 0's slots start at 0 (SimWorld.cs:48)
            var costs = new int[HudView.SupportAbilities.Length];
            for (int i = 0; i < costs.Length; i++) costs[i] = OffMapAbilitySystem.TryGetStats((int)HudView.SupportAbilities[i], out var st) ? st.Cost : 0;
            refs = HudView.Build(root, Resources.Load<VisualTreeAsset>("Hud/UnitCard"), roster, costs);
            Clock = MatchClock.For(Host);
            tooltip = new HudTooltip(refs.Tooltip, refs.TooltipTitle, refs.TooltipBody, refs.TooltipCost);
            minimap = new HudMinimap(refs, Host, Cam);
            var garrison = new GarrisonStats();
            clusters = new TrenchOrderCluster(refs.OrdersLayer, Resources.Load<VisualTreeAsset>("Hud/TrenchOrders"), Host, Cam, tooltip, Order, garrison);
            objectives = new ObjectiveTracker(refs, Resources.Load<VisualTreeAsset>("Hud/ObjectiveRow"), Host);
            dialogue = new HudDialogue(root);
            commentary = new HudCommentary(Host, dialogue, SettingsStore.Current.Interface.Tooltips);   // the tips follow the tooltips setting
            selection = new SelectionController(Host, Cam, root, () => Panel, garrison) { Trenches = clusters };
            selectionPanel = new SelectionPanel(root, selection, Host);
            hotkeys = new HudHotkeys(this);
            Wire();
            refs.MinimapBezel.EnableInClassList("hud-bezel--round", SettingsStore.Current.Interface.RoundRadar);
            refs.MinimapBezel.EnableInClassList("hud-bezel--square", !SettingsStore.Current.Interface.RoundRadar);
            built = true;
            return true;
        }

        void Wire()
        {
            foreach (var c in refs.Cards)
            {
                var card = c;
                if (card.IsSupport) card.Root.clicked += () => ToggleArm(card.Ability);
                else card.Root.clicked += () => Deploy(card.Slot);
                tooltip.Attach(card.Root, () => card.Title, () => card.Tip, () => CostLine(card));
            }
            for (int k = 0; k < refs.SpeedButtons.Length; k++)
            {
                int idx = k; var b = refs.SpeedButtons[k];
                if (b == null) continue;
                b.clicked += () =>
                {
                    if (Clock == null) return;
                    if (idx == 0) Clock.Toggle(MatchClock.Hold.Tactical);
                    else { Clock.SetSpeed(HudView.SpeedOfIndex[idx]); Clock.Remove(MatchClock.Hold.Tactical); }
                };
                tooltip.Attach(b, idx == 0 ? "PAUSE" : "SPEED " + HudView.SpeedText(idx), idx == 0 ? HudText.PauseTip : HudText.SpeedTip);
            }
            // the gauges explain their side figures (income, the enemy's count, PAUSED)
            var root = doc.rootVisualElement;
            foreach (var (name, heading, text) in new[] { ("gauge-silver", "SILVER", HudText.SilverGaugeTip), ("gauge-men", "MEN", HudText.MenGaugeTip), ("gauge-time", "TIME", HudText.TimeGaugeTip) })
            {
                var g = root.Q(name);
                if (g == null) continue;
                g.pickingMode = PickingMode.Position;
                tooltip.Attach(g, heading, text);
            }
        }

        string CostLine(CardRefs c)
        {
            string key = c.IsSupport ? HudText.SupportHotkey(c.Ability == OffMapAbilityId.ChlorineGas ? 1 : 0) : HudText.Hotkey(c.Slot);
            return $"{c.BaseCost} SILVER   KEY {key}";
        }

        // ---- commands ----------------------------------------------------------------------------------------------
        public void Deploy(int slot)
        {
            if (!Interactive || Host?.Local == null) return;
            var w = Host.Local.World;
            if (w.WinnerTeam >= 0 || slot < 0 || slot >= RosterEntry.SlotCount) return;
            if (w.SlotUnlocked[slot] == 0 || w.SlotCooldown[slot] > 0 || w.Silver[0] < w.Roster[slot].Cost) return;
            Host.Issue(SimCommand.Deploy(w.Tick, 0, slot));
        }

        public void ToggleArm(OffMapAbilityId id)
        {
            if (!Interactive || Panel == null || Host?.Local == null) return;
            if (Panel.Armed == id) { Panel.Arm(OffMapAbilityId.None); return; }
            var w = Host.Local.World;
            if (w.WinnerTeam >= 0 || !OffMapAbilitySystem.TryGetStats((int)id, out var st)) return;
            if (Host.Local.Abilities == null || Host.Local.Abilities.CooldownOf(0, id) > 0 || w.Silver[0] < st.Cost) return;
            Panel.Arm(id);
        }

        void Order(CommandType type, int trench, int b)
        {
            if (!Interactive || Host?.Local == null || Host.Local.World.WinnerTeam >= 0) return;
            Host.Issue(new SimCommand { Tick = Host.Local.World.Tick, Player = 0, Type = type, A = trench, B = b });
        }

        /// <summary>An order to the player's front trench (the hotkeys' target, as DebugOverlay's Space and Backspace).</summary>
        public void OrderFront(CommandType type)
        {
            if (Host?.Local == null) return;
            short t = Host.Local.Fields.FrontTrench(0);
            if (t >= 0) Order(type, t, 0);
        }

        public void ToggleFront(CommandType type)
        {
            if (Host?.Local == null) return;
            short t = Host.Local.Fields.FrontTrench(0);
            if (t < 0) return;
            var ts = Host.Local.Fields.Trenches[t];
            int b = type == CommandType.TrenchLock ? (ts.Locked != 0 ? 0 : 1) : type == CommandType.TrenchHoldFire ? (ts.HoldFire != 0 ? 0 : 1) : 0;
            Order(type, t, b);
        }

        // ---- the click mask ----------------------------------------------------------------------------------------
        /// <summary>Is this mouse position (bottom-left origin) over HUD chrome? Once per frame; layout layers ignore picking.</summary>
        bool PointerOverHud(Vector2 mouse)
        {
            if (Time.frameCount == pickFrame) return pickHit;
            pickFrame = Time.frameCount;
            pickHit = false;
            if (!flagOn || !visible || doc == null) return false;
            var root = doc.rootVisualElement;
            var panel = root?.panel;
            if (panel == null) return false;
            Vector2 p = RuntimePanelUtils.ScreenToPanel(panel, new Vector2(mouse.x, Screen.height - mouse.y));   // panel space is top-left origin
            var hit = panel.Pick(p);
            pickHit = hit != null && hit != root;
            return pickHit;
        }

        // ---- per frame ---------------------------------------------------------------------------------------------
        void LateUpdate()
        {
            using var perf = TW.Sim.PerfMarkers.HudLate.Auto();
            if (!flagOn)
            {
                if (KeyMap.DownRaw(GameAction.HudToggle)) { HudBridge.UseToolkitHud = true; ApplyFlag(); }
                return;
            }
            if (built && refs.Bar.panel == null) built = false;   // the document rebuilt its tree under us (live reload)
            if (!built && !TryBuild()) return;
            using (refreshMarker.Auto())
            {
                Refresh();
            }
        }

        void Refresh()
        {
            var w = Host.Local.World;
            bool over = w.WinnerTeam >= 0;
            float tickSeconds = w.Config.TickSeconds;
            hotkeys.Update();
            LegacyInterim();

            // gauges
            minimap.Refresh();
            dialogue?.Tick(Time.unscaledDeltaTime);
            selection?.Tick(Interactive);
            selectionPanel?.Refresh();
            bool paused = Clock != null ? Clock.Paused : Host.TimeScale <= 0f;
            float speed = Clock != null ? Clock.Speed : Host.TimeScale;
            HudView.BindGauges(refs, w.Silver[0], Host.SilverPerSecond, minimap.MyMen, minimap.TheirMen, Mathf.FloorToInt(w.Tick * tickSeconds), speed, paused);

            // cards
            int silver = w.Silver[0];
            var abilities = Host.Local.Abilities;
            var armed = Panel != null ? Panel.Armed : OffMapAbilityId.None;
            for (int i = 0; i < refs.Cards.Count; i++)
            {
                var c = refs.Cards[i];
                if (!c.IsSupport)
                {
                    int s = c.Slot;
                    var e = w.Roster[s];
                    HudView.BindCard(c, silver, w.SlotCooldown[s], Mathf.Max(1, e.CooldownTicks), w.SlotUnlocked[s] != 0, over, tickSeconds, 0);
                }
                else
                {
                    int cd = abilities != null ? abilities.CooldownOf(0, c.Ability) : 0;
                    int total = OffMapAbilitySystem.TryGetStats((int)c.Ability, out var st) ? Mathf.Max(1, st.CooldownTicks) : 1;
                    HudView.BindSupportCard(c, silver, cd, total, armed == c.Ability, over, tickSeconds);
                }
            }
            HudView.ShowHint(refs, armed != OffMapAbilityId.None ? (Panel != null && Panel.Aim.IsLine ? HudText.AimLineHint : HudText.AimHint) : null);

            // the rest
            var root = doc.rootVisualElement;
            clusters.Refresh(root.panel, Camera.main, refs.Root.resolvedStyle.width, refs.Root.resolvedStyle.height, over);   // HUD space (HudScale)
            objectives.Refresh();
            tooltip.Update();
        }

        /// <summary>
        /// Until DebugOverlay and TestPanel read HudBridge (step A5): switch the IMGUI HUD off ourselves and keep its
        /// static MinimapRect pointing at OUR minimap, so TestPanel.TryGroundPoint's old mask still refuses a ground
        /// click under it. The bottom 100 px it also refuses is where our bar is.
        /// </summary>
        void LegacyInterim()
        {
            if (legacy == null && Time.unscaledTime >= nextLegacyCheck)
            {
                nextLegacyCheck = Time.unscaledTime + 0.5f;
                var main = Camera.main;
                legacy = main != null ? main.GetComponent<BattleHud>() : null;
            }
            if (legacy != null && legacy.enabled) legacy.enabled = false;
            var root = doc.rootVisualElement;
            float h = root.resolvedStyle.height;
            if (h > 0f && refs.Minimap.panel != null)
            {
                float k = Screen.height / h;   // panel units to screen pixels (the panel is matched on height)
                var wb = refs.MinimapBezel.worldBound;
                BattleHud.MinimapRect = new Rect(wb.x * k, wb.y * k, wb.width * k, wb.height * k);
            }
        }
    }

    /// <summary>A profiler marker that compiles away nothing but stays cheap; wraps Unity.Profiling.ProfilerMarker.</summary>
    public readonly struct ProfilerMarkerScope
    {
        readonly Unity.Profiling.ProfilerMarker marker;
        public ProfilerMarkerScope(string name) { marker = new Unity.Profiling.ProfilerMarker(name); }
        public Unity.Profiling.ProfilerMarker.AutoScope Auto() => marker.Auto();
    }
}
