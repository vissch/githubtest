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
            yield return null; yield return null;
            short t = hud.Host.Local.Fields.FrontTrench(0);
            Assert.That(t, Is.GreaterThanOrEqualTo((short)0), "no owned front trench to lock");

            hud.Clock.Toggle(MatchClock.Hold.Tactical);
            yield return null;
            Assert.That(hud.Clock.Paused, Is.True, "the tactical pause did not take hold");

            hud.ToggleFront(CommandType.TrenchLock);
            hud.ToggleFront(CommandType.TrenchLock);

            hud.Clock.Remove(MatchClock.Hold.Tactical);
            for (int i = 0; i < 20; i++) yield return null;

            Assert.That(hud.Host.Local.Fields.Trenches[t].Locked, Is.EqualTo(0),
                "[I3] a second lock click in tactical pause must undo the first");
        }
    }
}
