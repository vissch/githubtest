// Phase: B6 (implemented) — a mission's request reaches the sim: SimHost.Awake applies MatchLaunch.Current before
// it builds the match, so the world's config carries the request's silver. The one line in Awake is what this proves.
using System.Collections;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using TW.Presentation;
using TW.UI;

namespace TW.Tests
{
    public class MatchLaunchPlayTests
    {
        GameObject go;

        [UnitySetUp] public IEnumerator SetUp() { HudBootstrap.Disabled = true; ShellBoot.Disabled = true; yield return null; }

        [UnityTearDown]
        public IEnumerator TearDown()
        {
            MatchLaunch.Current = null;
            if (go != null) Object.Destroy(go);
            HudBootstrap.Disabled = false; ShellBoot.Disabled = false;
            yield return null;
        }

        [UnityTest]
        public IEnumerator TheRequestIsInTheWorldsConfig()
        {
            MatchLaunch.Current = new MatchLaunch.Request { StartingSilver = 999, SilverPerSecond = 5f, GeneratedBattlefield = false, PlaytestMap = false, PeerDeployEveryTicks = 77 };
            go = new GameObject("launch-test"); go.SetActive(false);
            var host = go.AddComponent<SimHost>();
            host.GeneratedBattlefield = true;   // the request must override this
            go.SetActive(true);
            yield return null;
            Assert.That(host.Local, Is.Not.Null);
            Assert.That(host.Local.World.Config.StartingSilver, Is.EqualTo(999), "SimHost.Awake must apply MatchLaunch.Current before NewMatch");
            Assert.That(host.Local.World.Silver[0], Is.EqualTo(999));
            Assert.That(host.Local.World.Config.SilverPerSecond, Is.EqualTo(5f));
            Assert.That(host.PeerDeployEveryTicks, Is.EqualTo(77));
            Assert.That(host.GeneratedBattlefield, Is.False, "the cheap greybox, as the request asked");
            Assert.That(MatchLaunch.Running, Is.SameAs(MatchLaunch.Current));
        }

        [UnityTest]
        public IEnumerator NoRequestLeavesTheSceneValuesAlone()
        {
            MatchLaunch.Current = null;
            go = new GameObject("launch-test-2"); go.SetActive(false);
            var host = go.AddComponent<SimHost>();
            host.StartingSilver = 123; host.GeneratedBattlefield = false; host.PlaytestMap = false;
            go.SetActive(true);
            yield return null;
            Assert.That(host.Local.World.Config.StartingSilver, Is.EqualTo(123));
            Assert.That(MatchLaunch.Running, Is.Null);
        }
    }
}
