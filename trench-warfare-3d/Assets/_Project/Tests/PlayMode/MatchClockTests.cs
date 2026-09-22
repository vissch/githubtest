// Phase: B6 (implemented) — the clock's holds compose, its speed is what the sim sees, and it adopts writes it did
// not make. Runs against a real SimHost on the cheap greybox so the write lands where SimHost.Update reads it.
using System.Collections;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using TW.Presentation;

namespace TW.Tests
{
    public class MatchClockTests
    {
        GameObject go;

        [UnityTearDown]
        public IEnumerator TearDown()
        {
            if (go != null) Object.Destroy(go);
            yield return null;
        }

        SimHost MakeHost()
        {
            go = new GameObject("clock-test");
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
        public IEnumerator HoldsComposeAndReleaseInAnyOrder()
        {
            var host = MakeHost();
            var clock = MatchClock.For(host);
            yield return null;
            Assert.That(host.TimeScale, Is.EqualTo(1f));

            clock.Add(MatchClock.Hold.Menu);
            yield return null;
            Assert.That(host.TimeScale, Is.EqualTo(0f), "a menu holds the sim");

            clock.Add(MatchClock.Hold.Tactical);
            clock.Remove(MatchClock.Hold.Menu);
            yield return null;
            Assert.That(host.TimeScale, Is.EqualTo(0f), "the tactical pause still holds after the menu closed");
            Assert.That(clock.Has(MatchClock.Hold.Tactical));

            clock.Remove(MatchClock.Hold.Tactical);
            yield return null;
            Assert.That(host.TimeScale, Is.EqualTo(1f));
            Assert.That(clock.Paused, Is.False);
        }

        [UnityTest]
        public IEnumerator SpeedIsKeptAcrossAHold()
        {
            var host = MakeHost();
            var clock = MatchClock.For(host);
            clock.SetSpeed(4f);
            yield return null;
            Assert.That(host.TimeScale, Is.EqualTo(4f));
            clock.Add(MatchClock.Hold.Debrief);
            yield return null;
            Assert.That(host.TimeScale, Is.EqualTo(0f));
            clock.Remove(MatchClock.Hold.Debrief);
            yield return null;
            Assert.That(host.TimeScale, Is.EqualTo(4f), "the speed chosen before the hold comes back");
            clock.StepSpeed(+1); Assert.That(clock.Speed, Is.EqualTo(8f));
            clock.StepSpeed(+1); Assert.That(clock.Speed, Is.EqualTo(8f), "clamps at the top");
            clock.StepSpeed(-3); Assert.That(clock.Speed, Is.EqualTo(1f), "clamps at the bottom");
        }

        [UnityTest]
        public IEnumerator ExternalWritesAreAdopted()
        {
            var host = MakeHost();
            var clock = MatchClock.For(host);
            yield return null;
            host.TimeScale = 2f;            // the IMGUI speed button, not yet migrated
            yield return null;
            Assert.That(clock.Speed, Is.EqualTo(2f), "a foreign speed write becomes the clock's speed");
            Assert.That(host.TimeScale, Is.EqualTo(2f));
            host.TimeScale = 0f;            // the IMGUI pause button
            yield return null;
            Assert.That(clock.Has(MatchClock.Hold.Tactical), "a foreign write to zero is a tactical pause");
            Assert.That(host.TimeScale, Is.EqualTo(0f));
            host.TimeScale = 1f;
            yield return null;
            Assert.That(clock.Paused, Is.False);
            Assert.That(clock.Speed, Is.EqualTo(1f));
        }

        [UnityTest]
        public IEnumerator ChangedFiresOnHoldsAndSpeedOnly()
        {
            var host = MakeHost();
            var clock = MatchClock.For(host);
            int fired = 0;
            clock.Changed += () => fired++;
            yield return null; yield return null;
            Assert.That(fired, Is.EqualTo(0), "a steady clock is silent");
            clock.Add(MatchClock.Hold.Menu); clock.Add(MatchClock.Hold.Menu);
            Assert.That(fired, Is.EqualTo(1), "adding a hold twice fires once");
            clock.SetSpeed(2f); clock.SetSpeed(2f);
            Assert.That(fired, Is.EqualTo(2));
            clock.Remove(MatchClock.Hold.Menu);
            Assert.That(fired, Is.EqualTo(3));
        }

        [UnityTest]
        public IEnumerator ForReturnsTheSameClock()
        {
            var host = MakeHost();
            yield return null;
            var a = MatchClock.For(host); var b = MatchClock.For(host);
            Assert.That(a, Is.SameAs(b));
            Assert.That(a.Host, Is.SameAs(host));
            Assert.That(MatchClock.For(null), Is.Null);
        }
    }
}
