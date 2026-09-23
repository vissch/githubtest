// Phase: P0 (implemented)
// Copies the sim's per-tick events into a managed list consumed once per frame by VFX, audio, UI and ragdolls.
// Subscribers are held as an array rather than a multicast delegate so each can be timed under its own profiler marker
// (TW.Events.To.<Type>, only while ProfileSubscribers is on). Semantics are the delegate's: subscription order, `-=`
// removes the last matching handler, and a handler added during Dispatch takes effect from the next event.
using System;
using System.Collections.Generic;
using Unity.Profiling;
using TW.Sim;

namespace TW.Presentation
{
    public sealed class EventPump
    {
        public readonly List<SimEvent> Frame = new List<SimEvent>(1024);
        public int OverrunTotal;
        /// <summary>Time every subscriber under its own marker. Off by default: two marker calls per event per subscriber.</summary>
        public static bool ProfileSubscribers;
        Action<SimEvent>[] handlers = new Action<SimEvent>[0];
        ProfilerMarker[] handlerMarkers = new ProfilerMarker[0];
        string[] handlerNames = new string[0];

        public event Action<SimEvent> OnEvent
        {
            add
            {
                if (value == null) return;
                int n = handlers.Length;
                var h = new Action<SimEvent>[n + 1]; var m = new ProfilerMarker[n + 1]; var s = new string[n + 1];
                Array.Copy(handlers, h, n); Array.Copy(handlerMarkers, m, n); Array.Copy(handlerNames, s, n);
                h[n] = value; s[n] = "TW.Events.To." + OwnerName(value); m[n] = new ProfilerMarker(s[n]);
                handlers = h; handlerMarkers = m; handlerNames = s;
            }
            remove
            {
                if (value == null) return;
                int at = -1;
                for (int k = handlers.Length - 1; k >= 0; k--) if (handlers[k] == value) { at = k; break; }
                if (at < 0) return;
                int n = handlers.Length;
                var h = new Action<SimEvent>[n - 1]; var m = new ProfilerMarker[n - 1]; var s = new string[n - 1];
                for (int k = 0, j = 0; k < n; k++) { if (k == at) continue; h[j] = handlers[k]; m[j] = handlerMarkers[k]; s[j] = handlerNames[k]; j++; }
                handlers = h; handlerMarkers = m; handlerNames = s;
            }
        }

        public int SubscriberCount => handlers.Length;
        /// <summary>The profiler marker name subscriber `k` is timed under.</summary>
        public string SubscriberMarker(int k) => handlerNames[k];

        static string OwnerName(Action<SimEvent> d)
        {
            var t = d.Method.DeclaringType;
            while (t != null && t.Name.StartsWith("<", StringComparison.Ordinal)) t = t.DeclaringType;   // a lambda's closure class
            return t != null ? t.Name : "?";
        }

        public void Collect(SimWorld w)
        {
            var ev = w.Events.Events;
            for (int i = 0; i < ev.Length; i++) Frame.Add(ev[i]);
            OverrunTotal += w.Events.Overrun;
        }

        /// <summary>Dispatch and clear. Call once per render frame after all ticks for the frame were stepped.</summary>
        public void Dispatch()
        {
            using var dispatch = PerfMarkers.EventsDispatch.Auto();
            for (int i = 0; i < Frame.Count; i++)
            {
                var e = Frame[i];
                var h = handlers;   // re-read per event: a handler added mid-dispatch hears the next event, as with a delegate
                if (ProfileSubscribers)
                {
                    var m = handlerMarkers;
                    for (int k = 0; k < h.Length; k++) { m[k].Begin(); h[k](e); m[k].End(); }
                }
                else for (int k = 0; k < h.Length; k++) h[k](e);
            }
            Frame.Clear();
        }
    }
}
