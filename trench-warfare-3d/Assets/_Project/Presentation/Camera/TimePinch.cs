// Phase: punch-quarter-second (implemented) — the slow-mo punch when a heap of men fountains apart (DeathGag.Fountain).
// Concept doc: docs/design/idea-a-quarter-second-slow-mo-punch-when-a-he-concept.md, option A: 0.200 s hold + 0.080 s
// ramp = 0.280 s. The owner, 2026-10-11: "trim the follow-through after the hit so it lands under the quarter
// second". Built here with the ramp trimmed to 0.040 s (0.240 s total, strictly under 0.250 s). Single-player only:
// like Storm's lightning freeze, this slows the sim with the world (concept doc, note 3).
//
// Storm.Freeze/Thaw (Presentation/Terrain/Storm.cs) also owns Time.timeScale, for a lightning strike. The rule, in
// this one place: never Fire() while the world is already held at (near) zero, and if Time.timeScale is not what
// this component wrote last frame, something else took the field — drop the window and write nothing more.
using UnityEngine;

namespace TW.Presentation.Tactical
{
    /// <summary>The punch's timing curve: 0.25x through the hold, then a lerp back to 1x across the ramp. Pure data,
    /// no Unity state, so a test can measure it directly.</summary>
    public static class PunchPinch
    {
        public const float SlowScale = 0.25f;
        public const float HoldSeconds = 0.200f;
        public const float RampSeconds = 0.040f;
        public static float Duration => HoldSeconds + RampSeconds;

        public static float ScaleAt(float since)
        {
            if (since <= HoldSeconds) return SlowScale;
            if (since >= Duration) return 1f;
            return Mathf.Lerp(SlowScale, 1f, (since - HoldSeconds) / RampSeconds);
        }
    }

    /// <summary>Drives Time.timeScale through PunchPinch's curve for one window, added to the host's GameObject on
    /// first use.</summary>
    public sealed class TimePinch : MonoBehaviour
    {
        float since = -1f;
        float lastWritten = -1f;

        public static TimePinch For(GameObject host)
        {
            if (host == null) return null;
            var p = host.GetComponent<TimePinch>();
            if (p == null) p = host.AddComponent<TimePinch>();
            return p;
        }

        /// <summary>Starts one pinch window; ignored while one is already running, so a heap of many deaths in one
        /// frame still reads as a single punch.</summary>
        public void Fire()
        {
            if (since >= 0f) return;
            if (Time.timeScale <= 0.001f) return;   // the world is already held (e.g. Storm's strike): never steal it
            since = 0f;
            lastWritten = Time.timeScale = PunchPinch.ScaleAt(0f);
        }

        void Update()
        {
            if (since < 0f) return;
            if (!Mathf.Approximately(Time.timeScale, lastWritten))
            {
                // somebody else wrote the field under us: drop the window and leave their write alone
                since = -1f;
                return;
            }
            since += Time.unscaledDeltaTime;
            if (since >= PunchPinch.Duration)
            {
                lastWritten = Time.timeScale = 1f;
                since = -1f;
                return;
            }
            lastWritten = Time.timeScale = PunchPinch.ScaleAt(since);
        }
    }
}
