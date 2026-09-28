// Phase: tooling (the gym, 2026-09-28) — the gym stages for real on a running host: a quiet battle, a man spawned through
// WriteWorlds and pinned is drawn in the pinned clip, and an HE barrage called through the gym is accepted by the sim
// (its AbilityFired comes back through the event pump), with no log errors on the way. CanaryFixture runs every PlayMode
// host with the canary over a lossy link, so a write can be refused while one world waits: the test retries a moment.
// GymDirector exists only in the editor and development builds, and so does this test.
#if UNITY_EDITOR || DEVELOPMENT_BUILD
using System.Collections;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using TW.Perf;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;
using TW.UI;

namespace TW.Tests
{
    public class GymPlayTests
    {
        GameObject go;

        [UnitySetUp] public IEnumerator SetUp() { HudBootstrap.Disabled = true; ShellBoot.Disabled = true; yield return null; }

        [UnityTearDown]
        public IEnumerator TearDown()
        {
            if (go != null) Object.Destroy(go);
            HudBootstrap.Disabled = false; ShellBoot.Disabled = false;
            yield return null;
        }

        [UnityTest]
        public IEnumerator TheGymPinsAClipAndCallsABarrage()
        {
            go = new GameObject("gym-test"); go.SetActive(false);
            var host = go.AddComponent<SimHost>();
            host.GeneratedBattlefield = false; host.PlaytestMap = false;
            go.SetActive(true);
            float until = Time.realtimeSinceStartup + 10f;
            while ((host.Local == null || host.Local.World.Tick < 20) && Time.realtimeSinceStartup < until) yield return null;
            Assert.That(host.Local, Is.Not.Null, "the match started");

            var d = GymDirector.Attach(host);
            until = Time.realtimeSinceStartup + 5f;
            while (!d.Quiet() && Time.realtimeSinceStartup < until) yield return null;
            Assert.IsTrue(d.Quiet(), "quiet: the worlds were aligned");
            var r = d.Begin(new GymEntry { Tab = GymTab.Abilities, Id = (int)OffMapAbilityId.HeBarrage, Name = "HeBarrage" });
            int man = -1;
            until = Time.realtimeSinceStartup + 5f;
            while ((man = d.PlayClip(0, Clip.KneelAimedIdle, d.Stage)) < 0 && Time.realtimeSinceStartup < until) yield return null;
            Assert.GreaterOrEqual(man, 0, "spawned through WriteWorlds");
            bool called = false;
            until = Time.realtimeSinceStartup + 5f;
            while (!(called = d.Ability(OffMapAbilityId.HeBarrage, d.Stage + new Vector2(0f, 40f))) && Time.realtimeSinceStartup < until) yield return null;
            Assert.IsTrue(called, "the top-up was written and the order issued");

            until = Time.realtimeSinceStartup + 6f;
            while (r.Count(SimEventType.AbilityFired) == 0 && Time.realtimeSinceStartup < until) yield return null;
            d.End();
            Assert.Greater(r.Count(SimEventType.AbilityFired), 0, "the sim accepted the barrage");
            Assert.AreEqual(0, r.Rejects, "and rejected nothing");
            Assert.AreEqual(Clip.KneelAimedIdle, host.Animation.State[man].Clip, "the pinned man is drawn in the pinned clip");
            Assert.AreEqual(0, r.Errors, string.Join("\n", r.Log));
            d.Clear();
        }
    }
}
#endif
