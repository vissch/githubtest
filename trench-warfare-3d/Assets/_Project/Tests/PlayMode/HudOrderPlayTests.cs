// Phase: B6 (implemented) — [I3] lock / hold-fire are absolute flags the sim applies a few ticks later and not at
// all while paused, so a second click computed from stale sim state resent the same value and the toggle could not
// be undone in a tactical pause. Drives HudController.ToggleFront twice through a real paused match.
using System.Collections;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using TW.Presentation;
using TW.Sim;
using TW.UI;

namespace TW.Tests
{
    public class HudOrderPlayTests
    {
        GameObject camGo, hostGo;
        HudController hud;

        [UnitySetUp]
        public IEnumerator SetUp()
        {
            HudBootstrap.Disabled = true; ShellBoot.Disabled = true;

            camGo = new GameObject("main-camera"); camGo.tag = "MainCamera";
            camGo.AddComponent<Camera>();

            hostGo = new GameObject("sim-host");
            var host = hostGo.AddComponent<SimHost>();
            host.GeneratedBattlefield = false; host.PlaytestMap = true;
            host.ScriptedPeer = false; host.PeerAttacks = false;

            hud = HudBootstrap.Create(host);
            yield return null;
        }

        [UnityTearDown]
        public IEnumerator TearDown()
        {
            if (hud != null) Object.Destroy(hud.gameObject);
            if (hostGo != null) Object.Destroy(hostGo);
            if (camGo != null) Object.Destroy(camGo);
            HudBootstrap.Disabled = false; ShellBoot.Disabled = false;
            yield return null;
        }

        [UnityTest]
        public IEnumerator ASecondLockClickInTacticalPauseMustUndoTheFirst()
        {
            // [I3b] the old body waited a fixed 20 frames, which in batch mode need not cover the command's
            // InputDelayTicks + 2, so the assert could only ever read the starting value. It now waits on World.Tick.
            float until = Time.realtimeSinceStartup + 20f;
            while ((hud.Host.Local == null || hud.Host.Local.World.Tick < 2) && Time.realtimeSinceStartup < until) yield return null;
            Assert.That(hud.Host.Local, Is.Not.Null, "[I3b] the match started");

            short t = hud.Host.Local.Fields.FrontTrench(0);
            Assert.That(t, Is.GreaterThanOrEqualTo((short)0), "[I3b] no owned front trench to lock");

            // the control: one click lands the absolute flag 1
            uint issued = hud.Host.Local.World.Tick;
            hud.ToggleFront(CommandType.TrenchLock);
            yield return Settle(issued);
            Assert.That(hud.Host.Local.Fields.Trenches[t].Locked, Is.EqualTo(1),
                "[I3b] one lock click must land Locked == 1");

            // the finding: two clicks inside a tactical pause must cancel out
            hud.Clock.Toggle(MatchClock.Hold.Tactical);
            yield return null;
            Assert.That(hud.Clock.Paused, Is.True, "[I3b] the tactical pause did not take hold");

            uint paused = hud.Host.Local.World.Tick;
            hud.ToggleFront(CommandType.TrenchLock);
            hud.ToggleFront(CommandType.TrenchLock);

            hud.Clock.Remove(MatchClock.Hold.Tactical);
            yield return Settle(paused);

            Assert.That(hud.Host.Local.Fields.Trenches[t].Locked, Is.EqualTo(1),
                "[I3b] a second lock click in tactical pause must undo the first");
        }

        /// <summary>Waits until a command issued at <paramref name="issued"/> can have been applied.</summary>
        IEnumerator Settle(uint issued)
        {
            uint target = issued + (uint)hud.Host.Local.World.Config.InputDelayTicks + 2;
            float until = Time.realtimeSinceStartup + 20f;
            while (hud.Host.Local.World.Tick < target && Time.realtimeSinceStartup < until) yield return null;
            Assert.That(hud.Host.Local.World.Tick, Is.GreaterThanOrEqualTo(target),
                "[I3b] the sim never reached the tick the command was due at");
        }
    }
}
