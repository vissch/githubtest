// Phase: P0 (implemented)
// Copies the sim's per-tick events into a managed list consumed once per frame by VFX, audio, UI and ragdolls.
using System;
using System.Collections.Generic;
using TW.Sim;

namespace TW.Presentation
{
    public sealed class EventPump
    {
        public readonly List<SimEvent> Frame = new List<SimEvent>(1024);
        public int OverrunTotal;
        public event Action<SimEvent> OnEvent;

        public void Collect(SimWorld w)
        {
            var ev = w.Events.Events;
            for (int i = 0; i < ev.Length; i++) Frame.Add(ev[i]);
            OverrunTotal += w.Events.Overrun;
        }

        /// <summary>Dispatch and clear. Call once per render frame after all ticks for the frame were stepped.</summary>
        public void Dispatch()
        {
            if (OnEvent != null) for (int i = 0; i < Frame.Count; i++) OnEvent(Frame[i]);
            Frame.Clear();
        }
    }
}
