// Phase: tooling (2026-10-14) - what the debug overlay can say about a frame's events. Dispatch empties Frame before
// OnGUI runs, so the count the overlay prints has to be remembered by the pump, not read off the list.
using NUnit.Framework;
using TW.Presentation;
using TW.Sim;

namespace TW.Tests
{
    public class EventPumpTests
    {
        [Test]   // [M7] the overlay printed Frame.Count, which Dispatch had already cleared: always 0
        public void TheCountOfTheLastDispatchSurvivesTheClear()
        {
            var pump = new EventPump();
            pump.Frame.Add(new SimEvent { Tick = 1, Type = SimEventType.Shot });
            pump.Frame.Add(new SimEvent { Tick = 1, Type = SimEventType.Shot });
            pump.Frame.Add(new SimEvent { Tick = 1, Type = SimEventType.Hit });
            pump.Dispatch();
            Assert.AreEqual(0, pump.Frame.Count, "[M7] Dispatch clears the frame, which is why the overlay read 0");
            Assert.AreEqual(3, pump.LastDispatched, "[M7] events/frame always prints 0");
        }
    }
}
