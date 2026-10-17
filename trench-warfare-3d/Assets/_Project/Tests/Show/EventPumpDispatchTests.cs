// Phase: review-fix (2026-10-17)
using System;
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Mathematics;
using TW.Sim;
using TW.Presentation;

namespace TW.Tests
{
    public class EventPumpDispatchTests
    {
        static SimConfig.WorldInit BareInit() => new SimConfig.WorldInit
        {
            SpawnA = new float3(60f, 0f, 10f),
            SpawnB = new float3(60f, 0f, 200f),
        };

        [TearDown]
        public void TearDown() => EventPump.ProfileSubscribers = false;

        // [found-clear-eventpump-frame-in-a-finally-not-after-20261017-0]
        [Test]
        public void ASubscriberThatThrowsDoesNotLeaveTheFrameToBeDispatchedTwice()
        {
            using var w = new SimWorld(SimConfig.Default, BareInit());
            var pump = new EventPump();
            var seen = new List<int>();
            bool thrown = false;
            pump.OnEvent += e => seen.Add(e.A);
            pump.OnEvent += e =>
            {
                if (!thrown) { thrown = true; throw new InvalidOperationException("boom"); }
            };

            w.Events.Clear();
            w.Events.Add(1, SimEventType.Shot, 7);
            pump.Collect(w);
            Assert.Throws<InvalidOperationException>(() => pump.Dispatch());
            Assert.That(pump.Frame.Count, Is.EqualTo(0),
                "[found-clear-eventpump-frame-in-a-finally-not-after-20261017-0] a handler that threw left this frame's events in Frame");

            w.Events.Clear();
            w.Events.Add(2, SimEventType.Shot, 8);
            pump.Collect(w);
            pump.Dispatch();

            Assert.That(seen, Is.EqualTo(new List<int> { 7, 8 }),
                "[found-clear-eventpump-frame-in-a-finally-not-after-20261017-0] an event was dispatched more than once after a subscriber threw");
            Assert.That(pump.Frame.Count, Is.EqualTo(0),
                "[found-clear-eventpump-frame-in-a-finally-not-after-20261017-0] Frame was not empty after the second Dispatch");
        }

        // [found-clear-eventpump-frame-in-a-finally-not-after-20261017-0]
        [Test]
        public void AThrowUnderProfileSubscribersStillClearsTheFrame()
        {
            EventPump.ProfileSubscribers = true;
            using var w = new SimWorld(SimConfig.Default, BareInit());
            var pump = new EventPump();
            var seen = new List<int>();
            bool thrown = false;
            pump.OnEvent += e => seen.Add(e.A);
            pump.OnEvent += e =>
            {
                if (!thrown) { thrown = true; throw new InvalidOperationException("boom"); }
            };

            w.Events.Clear();
            w.Events.Add(1, SimEventType.Shot, 7);
            pump.Collect(w);
            Assert.Throws<InvalidOperationException>(() => pump.Dispatch());
            Assert.That(pump.Frame.Count, Is.EqualTo(0),
                "[found-clear-eventpump-frame-in-a-finally-not-after-20261017-0] a handler that threw under ProfileSubscribers left this frame's events in Frame");

            w.Events.Clear();
            w.Events.Add(2, SimEventType.Shot, 8);
            pump.Collect(w);
            pump.Dispatch();

            Assert.That(seen, Is.EqualTo(new List<int> { 7, 8 }),
                "[found-clear-eventpump-frame-in-a-finally-not-after-20261017-0] an event was dispatched more than once under ProfileSubscribers after a subscriber threw");
            Assert.That(pump.Frame.Count, Is.EqualTo(0),
                "[found-clear-eventpump-frame-in-a-finally-not-after-20261017-0] Frame was not empty after the second Dispatch under ProfileSubscribers");
        }
    }
}
