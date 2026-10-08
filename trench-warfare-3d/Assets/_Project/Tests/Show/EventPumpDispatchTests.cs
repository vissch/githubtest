// Phase: review fix (2026-10-08) — one throwing subscriber must not break the event pump for everybody else.
// [M2] Dispatch used to clear Frame only after the loop and caught nothing: a handler that threw re-delivered every
// earlier event on the next frame, never delivered the later ones, and let Frame grow without limit.
using System;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using TW.Presentation;
using TW.Sim;

namespace TW.Tests
{
    public class EventPumpDispatchTests
    {
        [TearDown] public void TearDown() => LogAssert.ignoreFailingMessages = false;

        // [M2] a throwing subscriber costs its own call and nothing more.
        [Test]
        public void AThrowingSubscriberDoesNotRepeatOrBlockEvents()
        {
            LogAssert.ignoreFailingMessages = true;   // the fix logs the exception instead of letting it escape
            var pump = new EventPump();
            int a = 0, b = 0;
            pump.OnEvent += _ => { a++; throw new InvalidOperationException("[M2] a subscriber throws"); };
            pump.OnEvent += _ => b++;

            pump.Frame.Add(new SimEvent { Tick = 1, Type = SimEventType.Shot });
            pump.Frame.Add(new SimEvent { Tick = 1, Type = SimEventType.Shot });

            Assert.DoesNotThrow(() => pump.Dispatch(), "[M2] a throwing subscriber must not throw out of Dispatch");
            Assert.That(pump.Frame.Count, Is.EqualTo(0), "[M2] the frame is cleared even when a subscriber threw");
            Assert.That(a, Is.EqualTo(2), "[M2] the throwing subscriber is still offered the later event");
            Assert.That(b, Is.EqualTo(2), "[M2] the subscriber behind the throwing one hears both events");

            pump.Dispatch();
            Assert.That(a, Is.EqualTo(2), "[M2] no event is delivered a second time");
            Assert.That(b, Is.EqualTo(2), "[M2] no event is delivered a second time");
        }
    }
}
