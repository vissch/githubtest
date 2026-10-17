// Phase: B6 (implemented) — the AnimT seed Presenter.Interpolate passes to the stance fallback (UseAnimationController
// false) must run at the sim's speed, not the wall's: a hold freezes it, a fast-forward speeds it.
using System.Collections;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using TW.Presentation;

namespace TW.Tests
{
    public class AnimClockPlayTests
    {
        GameObject go;

        [UnitySetUp]
        public IEnumerator SetUp()
        {
            Time.captureDeltaTime = 1f / 50f;
            yield return null;
        }

        [UnityTearDown]
        public IEnumerator TearDown()
        {
            Time.captureDeltaTime = 0f;   // PerfBench and DeathStills own this static too: never leave it set
            if (go != null) Object.Destroy(go);
            yield return null;
        }

        SimHost MakeHost()
        {
            go = new GameObject("anim-clock-test");
            go.SetActive(false);
            var host = go.AddComponent<SimHost>();
            host.GeneratedBattlefield = false;
            host.PlaytestMap = false;
            host.ScriptedPeer = false;
            host.PeerAttacks = false;
            go.SetActive(true);
            return host;
        }

        [UnityTest]
        public IEnumerator AnimTimeIsFrozenWhileTheClockHolds()
        {
            var host = MakeHost();
            host.UseAnimationController = false;
            var clock = MatchClock.For(host);
            clock.Add(MatchClock.Hold.Tactical);
            yield return null;
            yield return null;
            Assert.That(host.TimeScale, Is.EqualTo(0f), "a broken hold cannot pass this test");
            float first = host.AnimTime;
            yield return null;
            yield return null;
            yield return null;
            Assert.That(host.AnimTime, Is.EqualTo(first), "a held match must not keep walking");
        }

        [UnityTest]
        public IEnumerator AnimTimeRunsAtTimeScale()
        {
            var host = MakeHost();
            host.UseAnimationController = false;
            host.TimeScale = 4f;
            yield return null;
            float start = host.AnimTime;
            float wall = 0f;
            for (int i = 0; i < 5; i++)
            {
                yield return null;
                wall += Time.deltaTime;
            }
            float grew = host.AnimTime - start;
            Assert.That(grew, Is.EqualTo(4f * wall).Within(0.01f));
        }
    }
}
