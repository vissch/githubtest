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
        /// <summary>
        /// UNITY runs the launch hook, not a test pretending to.
        ///
        /// The EditMode test SomethingPutsTheVolumeOnTheListenerAtLaunch reaches ApplyAudioOnLaunch by reflection
        /// and invokes it. That proves the method works; it cannot prove anything calls it. The one thing that
        /// makes a shipped build silent is that Unity runs it BeforeSceneLoad, and nothing observed that.
        ///
        /// The failure it leaves room for is ordinary: a RuntimeInitializeOnLoadMethod that throws does not stop
        /// the game - Unity logs it and carries on - so an exception anywhere on that path leaves
        /// AudioListener.volume at UNITY'S default of 1 while SettingsStore.Current says 0. A game shipping at
        /// full volume with every audio test green. The EditMode test cannot see it, because invoking the method
        /// itself would surface the throw.
        ///
        /// In PlayMode Unity really has run the hook by the time this executes, so comparing the listener against
        /// the settings in force catches a hook that was deleted, one that threw, and one that ran too late.
        ///
        /// ON WHAT THIS PROVES: a value check is only a proof when the value differs from what you would get
        /// without the hook, and here it does - the shipped default is 0 and Unity's own default is 1, so on a
        /// machine with no settings.json the listener can only be at 0 because something put it there. If a
        /// player has SAVED a master of 1 it degrades to a consistency check. Saying so is better than claiming
        /// a proof that depends on the tester's own settings file.
        /// </summary>
        [UnityTest]
        public IEnumerator UnityItselfPutsThePlayersVolumeOnTheListenerBeforeTheFirstScene()
        {
            yield return null;   // one frame, so anything that applies settings on load has certainly run

            var inForce = SettingsStore.Current;
            Assert.IsNotNull(inForce, "SettingsStore.Current is never null; something replaced it with one");
            Assert.AreEqual(inForce.Audio.Master, AudioListener.volume, 1e-5f,
                $"Unity entered play with the listener at {AudioListener.volume} while the settings in force say "
                + $"{inForce.Audio.Master}. Either nothing ran SettingsApplier.ApplyAudioOnLaunch - in which case a "
                + "build ships at Unity's default of 1 however muted the settings are - or it ran and threw, which "
                + "Unity logs and swallows.");

            // the three buses come off the same call, so they fail together and are worth naming separately
            Assert.AreEqual(inForce.Audio.Ambience, AudioLevels.Ambience, 1e-5f, "the ambience bus was not applied at launch");
            Assert.AreEqual(inForce.Audio.Sfx, AudioLevels.Sfx, 1e-5f, "the sfx bus was not applied at launch");
            Assert.AreEqual(inForce.Audio.Music, AudioLevels.Music, 1e-5f, "the music bus was not applied at launch");
        }

    }
}
