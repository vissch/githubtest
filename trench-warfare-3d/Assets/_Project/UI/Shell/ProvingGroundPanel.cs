// Phase: tooling (2026-09-28) — the Proving Ground's panel: docked at the right of a running match, never modal, so
// the battle plays on under it and the HUD stays live. Three pages:
//  UNITS  every unit the match's table defines, with its state (BUILT, PROTOTYPE, STAND-IN): OURS and THEIRS put 1, 5
//         or 10 of it at that side's rally point, +W adds as many to your own wave; the ideas are listed and greyed;
//  WAVES  the ready-made waves and your own: SEND now or REPEAT on the timer, placed at the enemy's rally or sent up
//         THROUGH THEIR SLOTS by command;
//  MATCH  silver, the clock, clearing the field, the sappers' orders (every sapper of a side lays ahead of himself),
//         the enemy's barrage and gas on your front trench, the scripted enemy's level,
//         restart, end (the debrief), quit.
// The router pushes it when the match was started from the Proving Ground's launch screen and F8 opens it over any
// match; F8 or its button folds it to its title. Esc is not its business (Overlay): the pause menu opens over it.
// Everything it does goes through the director (Presentation/Core/ProvingGround.cs); a test binds it over a director
// of its own with no router.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;

namespace TW.UI
{
    public sealed class ProvingGroundPanel : ShellScreen
    {
        public const string TreePath = "Shell/ProvingGroundPanel";
        public static readonly string[] RequiredNames =
        {
            "pg-dock", "pg-title", "btn-fold", "pg-body", "tab-units", "tab-waves", "tab-match", "page-units", "page-waves", "page-match",
            "count-tabs", "unit-list", "unit-tip", "btn-through-slots", "every-tabs", "slots-note", "wave-list", "custom-text",
            "btn-custom-send", "btn-custom-repeat", "btn-custom-clear", "timer-text", "btn-timer-stop", "btn-enemy-barrage", "btn-enemy-gas",
            "ai-tabs", "btn-silver-ours", "btn-silver-theirs", "speed-tabs", "btn-clear-ours", "btn-clear-theirs", "field-text",
            "btn-sappers-mine", "btn-sappers-wire", "btn-sappers-theirs",
            "btn-restart", "btn-end", "btn-quit-menu", "match-note", "status",
        };
        public override bool Modal => false;
        public override bool HidesHud => false;
        public override bool Overlay => true;

        public static readonly int[] Counts = { 1, 5, 10 };
        public static readonly float[] Everys = { 15f, 30f, 60f, 120f };
        public static readonly string[] Pages = { "units", "waves", "match" };
        public const int SilverGift = 1000;
        public const string EndReason = "THE PROVING GROUND WAS CLOSED";
        public const string PlacedNote = "WAVES ARE PLACED AT THEIR RALLY POINT: ANY UNIT, NO SILVER.";
        public const string SlotsNote = "WAVES GO UP THROUGH THEIR SLOTS BY COMMAND: ONLY UNITS OF THEIR TEN.";

        /// <summary>Your own wave survives a restart of the match (the scene reloads and the panel is rebuilt).</summary>
        static ProvingGround.Wave custom = NewCustom();
        static ProvingGround.Wave NewCustom() => new ProvingGround.Wave { Name = "YOUR WAVE", Blurb = "Built on the units page." };
        // kept across scene loads on purpose (a restart); emptied when the Play session ends
        static ProvingGroundPanel() => SceneStatics.Register(nameof(ProvingGroundPanel), () => custom = NewCustom());

        ProvingGround ground;
        List<ProvingGround.Wave> presets;
        readonly List<Button> pageTabs = new List<Button>(), countTabs = new List<Button>(), everyTabs = new List<Button>(), aiTabs = new List<Button>(), speedTabs = new List<Button>();
        VisualElement dock;
        Label status, timer, field;
        Button stop;
        string shown;
        float lastLive = -1f;
        /// <summary>The live lines are rewritten this often, not every frame: they are strings.</summary>
        public const float LiveEverySeconds = 0.25f;

        public int Page { get; private set; }
        public int CountIndex { get; private set; } = 1;
        public int EveryIndex { get; private set; } = 1;
        public int Ai { get; private set; } = -1;
        public bool Folded { get; private set; }
        public int Count => Counts[CountIndex];
        public ProvingGround Director => ground;
        public ProvingGround.Wave Custom => custom;

        /// <param name="director">Tests: a director over a match of their own. Null: the router's match.</param>
        public ProvingGroundPanel(ProvingGround director = null) { ground = director; }

        public override VisualTreeAsset Tree(ShellAssets a) => Resources.Load<VisualTreeAsset>(TreePath);

        protected override void OnBind()
        {
            var host = Router != null ? Router.Host : null;
            if (ground == null && host != null) ground = ProvingGround.For(host);
            dock = Root.Q("pg-dock"); status = Root.Q<Label>("status"); timer = Root.Q<Label>("timer-text"); field = Root.Q<Label>("field-text");
            stop = Root.Q<Button>("btn-timer-stop");
            presets = ProvingGround.Presets();
            if (host != null) Ai = AiOf(host);

            Btn("btn-fold", () => Fold(!Folded));
            pageTabs.Clear();
            for (int i = 0; i < Pages.Length; i++) { int idx = i; var b = Btn("tab-" + Pages[i], () => Show(idx)); if (b != null) pageTabs.Add(b); }

            Tabs("count-tabs", "tab-count-", Counts.Length, i => "x" + Counts[i], countTabs, i => { CountIndex = i; Refresh(); });
            Tabs("every-tabs", "tab-every-", Everys.Length, i => Everys[i].ToString("0") + " S", everyTabs, i => { EveryIndex = i; Refresh(); });
            Tabs("ai-tabs", "tab-ai-", ProvingGround.AiNames.Length, i => ProvingGround.AiNames[i], aiTabs, SetAi);
            Tabs("speed-tabs", "tab-speed-", MatchClock.Speeds.Length + 1, i => i == 0 ? "PAUSE" : "x" + MatchClock.Speeds[i - 1].ToString("0"), speedTabs, SetSpeed);

            BuildUnits(host != null && host.Local != null ? host.Local.World : null);
            BuildWaves();

            Btn("btn-through-slots", () => { if (ground != null) ground.ThroughSlots = !ground.ThroughSlots; Refresh(); });
            Btn("btn-custom-send", () => Send(custom));
            Btn("btn-custom-repeat", () => Repeat(custom));
            Btn("btn-custom-clear", () => { custom.Squads.Clear(); Refresh(); });
            Btn("btn-timer-stop", () => { ground?.Unschedule(); Refresh(); });
            Btn("btn-enemy-barrage", () => { ground?.EnemySupport(OffMapAbilityId.HeBarrage); Refresh(); });
            Btn("btn-enemy-gas", () => { ground?.EnemySupport(OffMapAbilityId.ChlorineGas); Refresh(); });
            Btn("btn-silver-ours", () => { ground?.GiveSilver(0, SilverGift); Refresh(); });
            Btn("btn-silver-theirs", () => { ground?.GiveSilver(1, SilverGift); Refresh(); });
            Btn("btn-clear-ours", () => { ground?.Clear(0); Refresh(); });
            Btn("btn-clear-theirs", () => { ground?.Clear(1); Refresh(); });
            Btn("btn-sappers-mine", () => { ground?.OrderSappers(0, TW.Sim.Units.UnitAbilityId.LayMine); Refresh(); });
            Btn("btn-sappers-wire", () => { ground?.OrderSappers(0, TW.Sim.Units.UnitAbilityId.LayTripwire); Refresh(); });
            Btn("btn-sappers-theirs", () => { ground?.OrderSappers(1, TW.Sim.Units.UnitAbilityId.LayMine); Refresh(); });
            Btn("btn-restart", () => Router?.RestartMatch());
            Btn("btn-end", () => Router?.ShowDebrief(EndReason));
            Btn("btn-quit-menu", () => Router?.QuitToMenu());
            bool endless = host != null && host.Local != null && host.Local.World.Config.Endless;
            SetText("match-note", endless ? "ENDLESS: HOLDING EVERY OBJECTIVE NAMES NO WINNER. END MATCH SHOWS THE DEBRIEF."
                : "THIS MATCH CAN BE WON AND LOST: IT WAS NOT STARTED FROM THE PROVING GROUND.");
            SetText("unit-tip", "OURS AND THEIRS PLACE THE UNIT AT THAT SIDE'S RALLY POINT. +W ADDS IT TO YOUR OWN WAVE.");
            Show(Page);
        }

        /// <summary>Which preset the host's knobs are, or -1 when they are none of them (a mission's own difficulty).</summary>
        static int AiOf(SimHost h)
        {
            if (!h.ScriptedPeer) return 0;
            for (int i = 1; i < ProvingGround.AiPresets.Length; i++)
            {
                var p = ProvingGround.AiPresets[i];
                if (p.DeployEveryTicks == h.PeerDeployEveryTicks && p.AttackGarrison == h.PeerAttackGarrison && p.Tanks == h.PeerDeploysTanks && p.Support == h.PeerUsesSupport) return i;
            }
            return -1;
        }

        void Tabs(string container, string prefix, int n, System.Func<int, string> text, List<Button> into, System.Action<int> pick)
        {
            var row = Root.Q(container); into.Clear();
            if (row == null) return;
            row.Clear();
            for (int i = 0; i < n; i++)
            {
                int idx = i;
                var b = new Button { name = prefix + i, text = text(i) }; b.AddToClassList("tw-tab"); b.focusable = false;
                b.clicked += () => pick(idx);
                row.Add(b); into.Add(b);
            }
        }

        static Button Small(string name, string text, System.Action click)
        {
            var b = new Button { name = name, text = text }; b.AddToClassList("tw-btn"); b.AddToClassList("tw-btn--small"); b.focusable = false;
            if (click != null) b.clicked += click;
            return b;
        }

        static VisualElement Row(string name, string title, string sub, out Label subLabel)
        {
            var row = new VisualElement { name = name }; row.AddToClassList("pg-row");
            var text = new VisualElement { pickingMode = PickingMode.Ignore }; text.AddToClassList("pg-row__text");
            var t = new Label(title) { pickingMode = PickingMode.Ignore }; t.AddToClassList("pg-row__name");
            subLabel = new Label(sub) { pickingMode = PickingMode.Ignore }; subLabel.AddToClassList("pg-row__sub");
            text.Add(t); text.Add(subLabel); row.Add(text);
            return row;
        }

        void BuildUnits(SimWorld world)
        {
            var list = Root.Q<ScrollView>("unit-list");
            if (list == null) return;
            list.Clear();
            bool machines = false, first = true;
            foreach (var u in ProvingGround.Catalogue(world))
            {
                if (first || u.Machine != machines)
                {
                    var head = new Label(u.Machine ? "MACHINES" : "INFANTRY") { pickingMode = PickingMode.Ignore }; head.AddToClassList("tw-caption"); head.AddToClassList("pg-section");
                    list.Add(head); machines = u.Machine; first = false;
                }
                var unit = u;
                var row = Row("unit-" + u.Archetype, $"{u.Archetype,2}  {u.Name.ToUpperInvariant()}", $"COST {u.Cost}   HP {u.Hp:0}   {u.Speed:0.0} M/S", out _);
                row.Add(ProvingGroundLaunchScreen.Chip(u.Status));
                row.Add(Small($"btn-ours-{u.Archetype}", "OURS", () => Spawn(0, unit.Archetype)));
                row.Add(Small($"btn-theirs-{u.Archetype}", "THEIRS", () => Spawn(1, unit.Archetype)));
                row.Add(Small($"btn-wave-{u.Archetype}", "+W", () => AddToWave(unit.Archetype)));
                row.RegisterCallback<MouseEnterEvent>(_ => SetText("unit-tip", ProvingGroundLaunchScreen.Describe(unit)));
                list.Add(row);
            }
            var ideas = new Label("IDEAS: NOT IN THE SIM YET") { pickingMode = PickingMode.Ignore }; ideas.AddToClassList("tw-caption"); ideas.AddToClassList("pg-section");
            list.Add(ideas);
            for (int i = 0; i < ProvingGround.Ideas.Length; i++)
            {
                var idea = ProvingGround.Ideas[i];
                var row = Row("idea-" + i, idea.Name.ToUpperInvariant(), "WAITS FOR: " + idea.Needs.ToUpperInvariant(), out _);
                row.AddToClassList("pg-row--idea");
                row.Add(ProvingGroundLaunchScreen.Chip(UnitStage.Idea));
                list.Add(row);
            }
        }

        void BuildWaves()
        {
            var list = Root.Q<ScrollView>("wave-list");
            if (list == null) return;
            list.Clear();
            for (int i = 0; i < presets.Count; i++)
            {
                var wave = presets[i];
                var row = Row("wave-" + i, $"{wave.Name}  ({wave.Units})", wave.Describe(), out _);
                row.AddToClassList("pg-row--wave");
                row.Add(Small("btn-send-" + i, "SEND", () => Send(wave)));
                row.Add(Small("btn-repeat-" + i, "REPEAT", () => Repeat(wave)));
                row.RegisterCallback<MouseEnterEvent>(_ => SetText("slots-note", wave.Blurb.ToUpperInvariant()));
                list.Add(row);
            }
        }

        // ---- what the buttons do (public: a test presses them without a panel) -----------------------------------------
        public void Show(int page)
        {
            Page = Mathf.Clamp(page, 0, Pages.Length - 1);
            for (int i = 0; i < Pages.Length; i++) Root?.Q("page-" + Pages[i])?.EnableInClassList("pg-page--hidden", i != Page);
            Refresh();
        }

        public void Fold(bool folded)
        {
            Folded = folded;
            dock?.EnableInClassList("pg-dock--collapsed", Folded);
        }

        public int Spawn(int team, byte archetype)
        {
            int n = ground != null ? ground.Spawn(team, archetype, Count) : 0;
            Refresh();
            return n;
        }

        public void AddToWave(byte archetype)
        {
            custom.Add(archetype, Count);
            Refresh();
        }

        public int Send(ProvingGround.Wave wave)
        {
            int n = ground != null ? ground.Send(wave) : 0;
            Refresh();
            return n;
        }

        public void Repeat(ProvingGround.Wave wave)
        {
            ground?.Schedule(wave, Everys[EveryIndex]);
            Refresh();
        }

        public void SetAi(int i)
        {
            Ai = Mathf.Clamp(i, 0, ProvingGround.AiNames.Length - 1);
            ProvingGround.ApplyAi(Ai, Router != null ? Router.Host : null);
            Refresh();
        }

        /// <summary>0 is the tactical pause; 1.. are MatchClock.Speeds.</summary>
        public void SetSpeed(int i)
        {
            var clock = Router != null ? Router.Clock : null;
            if (clock != null)
            {
                if (i == 0) clock.Toggle(MatchClock.Hold.Tactical);
                else { clock.Remove(MatchClock.Hold.Tactical); clock.SetSpeed(MatchClock.Speeds[Mathf.Clamp(i - 1, 0, MatchClock.Speeds.Length - 1)]); }
            }
            Refresh();
        }

        static void Mark(List<Button> tabs, int active) { for (int i = 0; i < tabs.Count; i++) tabs[i].EnableInClassList("tw-tab--active", i == active); }

        void Refresh()
        {
            if (Root == null) return;
            Mark(pageTabs, Page); Mark(countTabs, CountIndex); Mark(everyTabs, EveryIndex); Mark(aiTabs, Ai);
            var clock = Router != null ? Router.Clock : null;
            int speed = -1;
            if (clock != null)
            {
                if (clock.Has(MatchClock.Hold.Tactical)) speed = 0;
                else for (int i = 0; i < MatchClock.Speeds.Length; i++) if (Mathf.Approximately(MatchClock.Speeds[i], clock.Speed)) speed = i + 1;
            }
            Mark(speedTabs, speed);
            bool slots = ground != null && ground.ThroughSlots;
            Root.Q<Button>("btn-through-slots")?.EnableInClassList("tw-btn--on", slots);
            SetText("slots-note", slots ? SlotsNote : PlacedNote);
            SetText("custom-text", custom.Empty ? "EMPTY" : $"{custom.Units}: {custom.Describe()}");
            Root.Q<Button>("btn-custom-send")?.SetEnabled(!custom.Empty);
            Root.Q<Button>("btn-custom-repeat")?.SetEnabled(!custom.Empty);
            Live();
        }

        /// <summary>The lines that change while the match runs: the timer, the head count, the last thing done.</summary>
        void Live()
        {
            string t = ground == null || ground.Scheduled == null ? "NO WAVE ON THE TIMER"
                : $"{ground.Scheduled.Name} EVERY {ground.EveryTicks * TickSeconds:0} S: NEXT IN {Mathf.CeilToInt(ground.SecondsToNext)} S";
            if (timer != null && timer.text != t) timer.text = t;
            stop?.SetEnabled(ground != null && ground.Scheduled != null);
            var host = Router != null ? Router.Host : null;
            if (field != null && host != null && host.Local != null)
            {
                var w = host.Local.World; int ours = 0, theirs = 0;
                for (int i = 0; i < w.HighWater; i++) if (w.IsAlive(i)) { if (w.Team[i] == 0) ours++; else theirs++; }
                string f = $"ON THE FIELD: {ours} OURS, {theirs} THEIRS OF {w.Config.MaxSlots}.  SILVER {w.Silver[0]} / {w.Silver[1]}";
                if (field.text != f) field.text = f;
            }
            string s = ground == null ? "NO MATCH" : ground.Pending > 0 ? $"{ground.Last}  ({ground.Pending} STILL TO DEPLOY)" : ground.Last;
            if (status != null && s != shown) { shown = s; status.text = s; }
        }

        float TickSeconds
        {
            get
            {
                var host = Router != null ? Router.Host : null;
                return host != null && host.Local != null ? host.Local.World.Config.TickSeconds : SimConfig.Default.TickSeconds;
            }
        }

        public override void Tick()
        {
            ground?.Tick();
            float now = Time.unscaledTime;
            if (Root == null || (lastLive >= 0f && now - lastLive < LiveEverySeconds)) return;
            lastLive = now;
            Live();
        }
    }
}
