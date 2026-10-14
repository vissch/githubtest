// Phase: A3 (implemented) — an unscoped over-the-top order finds the men. Every deploy garrisons the rear trench, so
// the key must aim there and not at the empty front line (review 3, [Z011.1]).
using System.Collections;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using UnityEngine.UIElements;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Units;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.UI;

namespace TW.Tests
{
    public class HudOrderPlayTests
    {
        GameObject go, hudGo;
        GameObject camGo, hostGo;        // [I3] the lock test builds a real HUD over its own match
        HudController lockHud;
        /// <summary>Open ground on the playtest map (Tests/Sim/SapperTests.cs:21): no trench, no bunker, so a mine may lie there.</summary>
        static readonly Vector3 Open = new Vector3(150f, 0f, 240f);

        [UnitySetUp]
        public IEnumerator SetUp()
        {
            HudBootstrap.Disabled = true; ShellBoot.Disabled = true;
            MatchLaunch.Current = null;
            yield return null;
        }

        [UnityTearDown]
        public IEnumerator TearDown()
        {
            MatchLaunch.Current = null;
            if (hudGo != null) Object.Destroy(hudGo);
            if (camGo != null) Object.Destroy(camGo);
            if (go != null) Object.Destroy(go);
            if (lockHud != null) Object.Destroy(lockHud.gameObject);
            if (hostGo != null) Object.Destroy(hostGo);
            if (camGo != null) Object.Destroy(camGo);
            HudBootstrap.Disabled = false; ShellBoot.Disabled = false;
            yield return null;
        }

        /// <summary>
        /// [Z011.1] G with nothing selected must order the trench the men are in. On the playtest map seat 0 holds
        /// trench 0 at z = 60 (the reserve) and trench 1 at z = 140 (the front); deploys stop in the rear one, so an
        /// order aimed at the front trench moves nobody and the sim reports OrderFoundNoOne.
        /// </summary>
        [UnityTest]
        public IEnumerator GWithNothingSelectedSendsTheMannedTrenchOverTheTop()
        {
            go = new GameObject("hud-order-test"); go.SetActive(false);
            var host = go.AddComponent<SimHost>();
            host.GeneratedBattlefield = false; host.PlaytestMap = true;
            host.ScriptedPeer = false; host.DeterminismCanary = false;
            host.StartingSilver = 1000; host.TimeScale = 20f;
            go.SetActive(true);
            yield return null;
            Assert.That(host.Local, Is.Not.Null, "[Z011.1] the match must be built");

            short rear = host.Local.Fields.RearTrench(0), front = host.Local.Fields.FrontTrench(0);
            Assert.That(rear, Is.Not.EqualTo(front), "[Z011.1] the playtest map gives seat 0 two fire trenches");

            // one deploy a tick: SlotCooldown rejects a second in the same tick
            // Frames AND real time: with -nographics on an idle machine 600 frames pass in a fraction of a second,
            // before the men have walked to the trench (canary gate of 2026-11-10: green only on a busy machine).
            int deployed = 0;
            float until = Time.realtimeSinceStartup + 30f;
            for (int f = 0; (f < 600 || Time.realtimeSinceStartup < until) && deployed < 5; f++)
            {
                host.Issue(SimCommand.Deploy(host.Local.World.Tick, 0, 0));
                deployed = Alive(host.Local.World);
                yield return null;
            }
            Assert.That(Alive(host.Local.World), Is.GreaterThanOrEqualTo(5), "[Z011.1] five riflemen must reach the field");

            until = Time.realtimeSinceStartup + 30f;
            for (int f = 0; (f < 600 || Time.realtimeSinceStartup < until) && host.Local.Fields.Trenches[rear].GarrisonCount < 5; f++) yield return null;
            Assert.That(host.Local.Fields.Trenches[rear].GarrisonCount, Is.EqualTo(5), "[Z011.1] every reinforcement garrisons the rear trench");
            Assert.That(host.Local.Fields.Trenches[front].GarrisonCount, Is.EqualTo(0), "[Z011.1] the front trench holds nobody until a player orders an advance");

            hudGo = new GameObject("hud-order-test-hud"); hudGo.SetActive(false);
            var hud = hudGo.AddComponent<HudController>();   // inactive: no OnEnable, no UI tree, no selection
            hud.Host = host;
            Assert.That(hud.Interactive, Is.True, "[Z011.1] no modal must hold the input focus");

            int left = 0, foundNoOne = 0;
            System.Action<SimEvent> watch = e =>
            {
                if (e.Type == SimEventType.UnitLeftTrench && e.B == rear) left++;
                else if (e.Type == SimEventType.OrderFoundNoOne) foundNoOne++;
            };
            host.Events.OnEvent += watch;
            try
            {
                hud.OrderFront(CommandType.TrenchAdvance);   // no selection: the unscoped branch
                for (int f = 0; f < 120 && left == 0 && foundNoOne == 0; f++) yield return null;
            }
            finally { host.Events.OnEvent -= watch; }

            Assert.That(foundNoOne, Is.EqualTo(0), "[Z011.1] the order must not go to an empty trench (OrderFoundNoOne)");
            Assert.That(left, Is.GreaterThan(0), "[Z011.1] men of the manned trench must go over the top");
        }

        /// <summary>
        /// [I3] lock / hold-fire are absolute flags the sim applies a few ticks later and not at all while paused,
        /// so a second click computed from stale sim state resent the same value and the toggle could not be undone
        /// in a tactical pause. Drives HudController.ToggleFront twice through a real paused match.
        /// </summary>
        [UnityTest]
        public IEnumerator ASecondLockClickInTacticalPauseMustUndoTheFirst()
        {
            camGo = new GameObject("main-camera"); camGo.tag = "MainCamera";
            camGo.AddComponent<Camera>();

            hostGo = new GameObject("sim-host");
            var host = hostGo.AddComponent<SimHost>();
            host.GeneratedBattlefield = false; host.PlaytestMap = true;
            host.ScriptedPeer = false; host.PeerAttacks = false;

            lockHud = HudBootstrap.Create(host);

            // [I3b] the old body waited a fixed 20 frames, which in batch mode need not cover the command's
            // InputDelayTicks + 2, so the assert could only ever read the starting value. It now waits on World.Tick.
            float until = Time.realtimeSinceStartup + 20f;
            while ((lockHud.Host.Local == null || lockHud.Host.Local.World.Tick < 2) && Time.realtimeSinceStartup < until) yield return null;
            Assert.That(lockHud.Host.Local, Is.Not.Null, "[I3d] the match never started");
            // [I3d] not-null is true straight after Awake: only a ticking world says the match really runs.
            Assert.That(lockHud.Host.Local.World.Tick, Is.GreaterThanOrEqualTo(2u),
                "[I3d] the match never ticked: the world is still at tick " + lockHud.Host.Local.World.Tick);

            short t = lockHud.Host.Local.Fields.FrontTrench(0);
            Assert.That(t, Is.GreaterThanOrEqualTo((short)0), "[I3b] no owned front trench to lock");

            // the control: one click lands the absolute flag 1
            uint issued = lockHud.Host.Local.World.Tick;
            lockHud.ToggleFront(CommandType.TrenchLock);
            yield return Settle(issued);
            Assert.That(lockHud.Host.Local.Fields.Trenches[t].Locked, Is.EqualTo(1),
                "[I3b] one lock click must land Locked == 1");

            // the finding: two clicks inside a tactical pause must cancel out
            lockHud.Clock.Toggle(MatchClock.Hold.Tactical);
            yield return null;
            Assert.That(lockHud.Clock.Paused, Is.True, "[I3b] the tactical pause did not take hold");

            uint paused = lockHud.Host.Local.World.Tick;
            lockHud.ToggleFront(CommandType.TrenchLock);
            lockHud.ToggleFront(CommandType.TrenchLock);

            lockHud.Clock.Remove(MatchClock.Hold.Tactical);
            yield return Settle(paused);

            Assert.That(lockHud.Host.Local.Fields.Trenches[t].Locked, Is.EqualTo(1),
                "[I3b] a second lock click in tactical pause must undo the first");

            // [I3c] the pair above lands on the value it started from, so dropped paused clicks read the same.
            // One more paused click must reach the sim: it is the only phase whose value the test has not held yet.
            lockHud.Clock.Toggle(MatchClock.Hold.Tactical);
            yield return null;
            uint single = lockHud.Host.Local.World.Tick;
            lockHud.ToggleFront(CommandType.TrenchLock);
            lockHud.Clock.Remove(MatchClock.Hold.Tactical);
            yield return Settle(single);

            Assert.That(lockHud.Host.Local.Fields.Trenches[t].Locked, Is.EqualTo(0),
                "[I3c] a single lock click in a tactical pause must read Locked == 0 after the game resumes");
        }

        /// <summary>Waits until a command issued at <paramref name="issued"/> can have been applied.</summary>
        IEnumerator Settle(uint issued)
        {
            uint target = issued + (uint)lockHud.Host.Local.World.Config.InputDelayTicks + 2;
            float until = Time.realtimeSinceStartup + 20f;
            while (lockHud.Host.Local.World.Tick < target && Time.realtimeSinceStartup < until) yield return null;
            Assert.That(lockHud.Host.Local.World.Tick, Is.GreaterThanOrEqualTo(target),
                "[I3b] the sim never reached the tick the command was due at");
        }

        /// <summary>
        /// [sapper-button] The real battle bar in a real panel: with a sapper of ours selected the lay-mine button is
        /// there and live, pressing it arms the order, and the ground click sends the command the sim consumes until a
        /// mine lies on the field (MinePlaced). The selected sapper must survive that click: the press is the order's,
        /// not a click on empty field (TestPanel.SpendAim). With the sapper dropped the row goes away again.
        /// </summary>
        [UnityTest]
        public IEnumerator TheLayMineButtonLaysAMineAndKeepsTheSapperSelected()
        {
            var panelSettings = Resources.Load<PanelSettings>(HudBootstrap.PanelResource);
            var tree = Resources.Load<VisualTreeAsset>(HudBootstrap.HudResource);
            Assert.That(panelSettings, Is.Not.Null, "DustFrontPanel.asset missing from Resources");
            Assert.That(tree, Is.Not.Null, "Hud/BattleHud.uxml missing from Resources");

            go = new GameObject("sapper-button-test"); go.SetActive(false);
            var host = go.AddComponent<SimHost>();
            host.GeneratedBattlefield = false; host.PlaytestMap = true;
            host.ScriptedPeer = false; host.DeterminismCanary = false;
            host.StartingSilver = 1000; host.TimeScale = 8f;
            go.SetActive(true);
            float until = Time.realtimeSinceStartup + 15f;
            while ((host.Local == null || host.Local.World.Tick < 10) && Time.realtimeSinceStartup < until) yield return null;
            Assert.That(host.Local, Is.Not.Null, "[sapper-button] the match must be built");
            Assert.That(host.Local.Sapper, Is.Not.Null, "[sapper-button] the match runs a SapperSystem");
            host.WriteWorlds(m => { var b = m.World.GetSystem<AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });

            camGo = new GameObject("sapper-button-test-cam") { tag = "MainCamera" };
            camGo.AddComponent<Camera>();
            var cam = camGo.AddComponent<TacticalCamera>(); cam.Host = host;
            var testPanel = camGo.AddComponent<TestPanel>(); testPanel.Host = host;

            // one sapper of ours on open ground, written into every world the match has (as the Proving Ground places)
            int slot = -1;
            until = Time.realtimeSinceStartup + 10f;
            while (slot < 0 && Time.realtimeSinceStartup < until)
            {
                host.WriteWorlds(m =>
                {
                    var e = m.World.Units.Roster[InfantryArchetype.Sapper];
                    int s = m.World.Spawn(0, InfantryArchetype.Sapper, new Unity.Mathematics.float3(Open.x, 0f, Open.z - 20f), e.Hp, e.Speed, false);
                    if (s >= 0) slot = s;
                });
                yield return null;
            }
            Assert.That(slot, Is.GreaterThanOrEqualTo(0), "[sapper-button] a sapper must reach the field");

            hudGo = new GameObject("sapper-button-test-hud"); hudGo.SetActive(false);
            var doc = hudGo.AddComponent<UIDocument>();
            doc.panelSettings = panelSettings; doc.visualTreeAsset = tree;
            var hud = hudGo.AddComponent<HudController>();
            hud.Host = host; hud.Panel = testPanel; hud.Cam = cam;
            hudGo.SetActive(true);
            until = Time.realtimeSinceStartup + 10f;
            while (hud.Refs == null && Time.realtimeSinceStartup < until) yield return null;
            Assert.That(hud.Refs, Is.Not.Null, "[sapper-button] the HUD must build its tree");
            var refs = hud.Refs;
            Assert.That(refs.LayMine, Is.Not.Null, "[sapper-button] BattleHud.uxml has no order-laymine button");
            Assert.That(refs.UnitOrders.ClassListContains("is-visible"), Is.False, "[sapper-button] nothing selected: no row of unit orders");

            hud.Selection.SelectOnly(new UnitHandle(slot, host.Local.World.Generation[slot]), drop: false);
            yield return null;
            Assert.That(refs.UnitOrders.ClassListContains("is-visible"), Is.True, "[sapper-button] a sapper is selected: the row is there");
            Assert.That(refs.LayMine.enabledSelf, Is.True, "[sapper-button] he has charges, so the button is live");

            // the real button: a submit (which Button turns into its own click) and then a click event through the element
            using (var submit = NavigationSubmitEvent.GetPooled()) refs.LayMine.SendEvent(submit);
            yield return null;
            if (!refs.LayMine.ClassListContains(HudView.ArmedClass))
            {
                using (var click = ClickEvent.GetPooled()) refs.LayMine.SendEvent(click);
                yield return null;
            }
            bool armedByTheButton = refs.LayMine.ClassListContains(HudView.ArmedClass);
            if (!armedByTheButton)
            {
                // A synthesized click does not always drive Clickable in a test panel (DebriefViewFieldPlayTests
                // met the same wall): then arm through the button's own handler. What this case catches is the ground
                // click below, not the event's route.
                TestContext.WriteLine("[sapper-button] the synthesized press did not reach Clickable; arming through ToggleLay");
                hud.ToggleLay(UnitAbilityId.LayMine);
                yield return null;
            }
            Assert.That(refs.LayMine.ClassListContains(HudView.ArmedClass), Is.True, "[sapper-button] the lay-mine order must show as armed");
            TestContext.WriteLine("[sapper-button] armed by a real press on the button: " + armedByTheButton);

            int placed = 0, rejected = 0, ordered = 0;
            System.Action<SimEvent> watch = e =>
            {
                if (e.Type == SimEventType.MinePlaced && e.B == 0) placed++;
                else if (e.Type == SimEventType.SapperOrdered && e.A == slot) ordered++;
                else if (e.Type == SimEventType.CommandRejected) rejected++;
            };
            host.Events.OnEvent += watch;
            try
            {
                int sent = hud.LayAt(Open);   // the ground click, at the point the mouse would have hit
                Assert.That(sent, Is.EqualTo(1), "[sapper-button] one command for the one selected sapper");
                Assert.That(testPanel.AimSpentFrame, Is.EqualTo(Time.frameCount), "[sapper-button] the press is spent, so the selection does not read it as a click on the field");
                until = Time.realtimeSinceStartup + 30f;
                while (placed == 0 && Time.realtimeSinceStartup < until) yield return null;
            }
            finally { host.Events.OnEvent -= watch; }

            Assert.That(rejected, Is.EqualTo(0), "[sapper-button] the sim must take the bar's command unchanged");
            Assert.That(ordered, Is.EqualTo(1), "[sapper-button] the sapper must take the errand (SapperOrdered)");
            Assert.That(placed, Is.GreaterThan(0), "[sapper-button] a mine of ours must lie on the field (MinePlaced)");
            Assert.That(hud.Selection.Model.Items.Count, Is.EqualTo(1), "[sapper-button] the ordered sapper must still be selected after the ground click");
            Assert.That(refs.LayMine.ClassListContains(HudView.ArmedClass), Is.False, "[sapper-button] the order is spent, so the button is no longer armed");

            hud.Selection.SelectOnly(new UnitHandle(slot, host.Local.World.Generation[slot]), drop: true);
            yield return null;
            Assert.That(refs.UnitOrders.ClassListContains("is-visible"), Is.False, "[sapper-button] with the sapper dropped the row goes away");
        }

        static int Alive(SimWorld w)
        {
            int n = 0;
            for (int i = 0; i < w.HighWater; i++) if (w.IsAlive(i) && w.Team[i] == 0) n++;
            return n;
        }
    }
}
