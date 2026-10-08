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
        bool? canaryWas;

        // CanaryFixture turns the canary on for the whole PlayMode run. Two worlds over a lossy loopback make
        // StepOnce fail, so ticks per frame are not the arithmetic the time tests below count on: pin it off here.
        [SetUp]
        public void SetUp()
        {
            canaryWas = SimHost.CanaryOverride;
            SimHost.CanaryOverride = false;
        }

        [UnityTearDown]
        public IEnumerator TearDown()
        {
            Time.captureDeltaTime = 0f;   // left set, every later PlayMode test runs on a fake frame time
            SimHost.CanaryOverride = canaryWas;
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
            Time.captureDeltaTime = 0.05f;   // SimConfig.Default.TickSeconds: one frame is one tick, so the tick asserts below are not frame-rate luck
            var host = MakeHost();
            var clock = MatchClock.For(host);
            yield return null;
            Assert.That(host.TimeScale, Is.EqualTo(1f));

            clock.Add(MatchClock.Hold.Menu);
            yield return null;
            Assert.That(host.TimeScale, Is.EqualTo(0f), "a menu holds the sim");
            uint held = host.Local.World.Tick;
            yield return null; yield return null;
            Assert.That(host.Local.World.Tick, Is.EqualTo(held), "a menu holds the sim's ticks, not only its TimeScale");

            clock.Add(MatchClock.Hold.Tactical);
            clock.Remove(MatchClock.Hold.Menu);
            yield return null;
            Assert.That(host.TimeScale, Is.EqualTo(0f), "the tactical pause still holds after the menu closed");
            Assert.That(clock.Has(MatchClock.Hold.Tactical));

            clock.Remove(MatchClock.Hold.Tactical);
            yield return null; yield return null;
            Assert.That(host.TimeScale, Is.EqualTo(1f));
            Assert.That(clock.Paused, Is.False);
            Assert.That(host.Local.World.Tick, Is.GreaterThan(held), "the sim steps again once the holds are gone");
        }

        [UnityTest]
        public IEnumerator SpeedIsKeptAcrossAHold()
        {
            Time.captureDeltaTime = 0.05f;   // SimConfig.Default.TickSeconds: one frame is one tick, so the tick asserts below are not frame-rate luck
            var host = MakeHost();
            var clock = MatchClock.For(host);
            clock.SetSpeed(4f);
            yield return null;
            Assert.That(host.TimeScale, Is.EqualTo(4f));
            clock.Add(MatchClock.Hold.Debrief);
            yield return null;
            Assert.That(host.TimeScale, Is.EqualTo(0f));
            uint held = host.Local.World.Tick;
            yield return null; yield return null;
            Assert.That(host.Local.World.Tick, Is.EqualTo(held), "the debrief holds the sim's ticks too");
            clock.Remove(MatchClock.Hold.Debrief);
            yield return null; yield return null;
            Assert.That(host.TimeScale, Is.EqualTo(4f), "the speed chosen before the hold comes back");
            Assert.That(host.Local.World.Tick, Is.GreaterThan(held), "and the sim steps again");
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

        // [M1] The host owed itself no sim time after a slow frame: the accumulator was uncapped, so the frames after
        // it kept paying the guard's full eight ticks. TickRate is 20, so one 1 s frame owes 20 ticks and the guard
        // pays 8; the frames after it must step at most one tick of their own (two, counting the clamp's leftover).
        [UnityTest]
        public IEnumerator ASlowFrameIsNotPaidBackOverTheNextFrames()
        {
            var host = MakeHost();
            var clock = MatchClock.For(host);
            yield return null;

            Time.captureDeltaTime = 1f;      // one frame is a whole second of sim: 20 ticks owed, 8 payable
            yield return null;
            Time.captureDeltaTime = 0.05f;   // SimConfig.Default.TickSeconds: one tick per frame from here on
            yield return null;
            uint before = host.Local.World.Tick;
            yield return null;
            uint stepped = host.Local.World.Tick - before;
            Assert.That(stepped, Is.LessThanOrEqualTo(2u),
                "[M1] a frame after a slow one steps its own tick, not the backlog the guard could not pay (stepped " + stepped + ")");
        }

        // [T37] The hold tests only read the TimeScale field they had just written. With a time debt standing, a
        // paused host kept stepping the backlog: the sim ran on while the game said it was paused.
        [UnityTest]
        public IEnumerator AHoldStopsTheSimEvenWithATimeDebt()
        {
            var host = MakeHost();
            var clock = MatchClock.For(host);
            yield return null;

            Time.captureDeltaTime = 1f;      // build the debt the guard cannot pay in one frame
            yield return null;
            Time.captureDeltaTime = 0.05f;

            clock.Add(MatchClock.Hold.Menu);
            uint held = host.Local.World.Tick;
            yield return null; yield return null;
            Assert.That(host.TimeScale, Is.EqualTo(0f));
            Assert.That(host.Local.World.Tick, Is.EqualTo(held),
                "[T37] a hold stops the sim's ticks even when the host still owes time (stepped "
                + (host.Local.World.Tick - held) + ")");

            clock.Remove(MatchClock.Hold.Menu);
            yield return null; yield return null;
            Assert.That(host.Local.World.Tick, Is.GreaterThan(held), "[T37] and the sim runs again on release");
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
