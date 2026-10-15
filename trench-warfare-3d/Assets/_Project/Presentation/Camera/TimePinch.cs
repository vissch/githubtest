// Phase: punch-quarter-second (implemented) — the slow-mo punch when a heap of men fountains apart (DeathGag.Fountain).
// Concept doc: docs/design/idea-a-quarter-second-slow-mo-punch-when-a-he-concept.md, option A: 0.200 s hold + 0.080 s
// ramp = 0.280 s. The owner, 2026-10-11: "trim the follow-through after the hit so it lands under the quarter
// second". Built here with the ramp trimmed to 0.040 s (0.240 s total, strictly under 0.250 s). Single-player only:
// like Storm's lightning freeze, this slows the sim with the world (concept doc, note 3).
//
// Phase: punch-quarter-second-b (implemented) — a lightning strike mid-punch used to leave the game at 0.25x
// forever (Storm.Freeze saved the punch's own scale as its "before", Storm.Thaw restored it, and nothing ever set
// 1 again). The decision now lives in PunchClock, a pure, Unity-free struct: the punch always yields to any other
// writer of Time.timeScale and always hands back the scale that was there *before* the punch fired, never its own
// 0.25 or a value in between. Storm no longer reads the punch's own scale as its "before" (ScaleBehindThePunch).
// Fire() takes the lockstep flag and does nothing in a lockstep match: a local time-scale change there would desync
// the two worlds.
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

    /// <summary>The punch's decision, with no Unity call in it so a test can drive it frame by frame: when to fire,
    /// what to write each step, and what to hand back when something else (a freeze, a pause, another writer, the
    /// component going away) interrupts the window. Never ends a window at anything but <see cref="Before"/>.</summary>
    public struct PunchClock
    {
        public const float Eps = 1e-4f;

        /// <summary>Seconds since the punch fired; -1 when no window is running.</summary>
        public float Since;
        /// <summary>The scale this clock last asked for (so it can tell its own write back from a stranger's).</summary>
        public float Written;
        /// <summary>The scale to end the window at: whatever Time.timeScale was before the punch fired.</summary>
        public float Before;
        /// <summary>True while another writer (a freeze, or the field held at ~0) owns the field; the window waits.</summary>
        public bool Parked;

        public bool Running => Since >= 0f;

        /// <summary>Starts one pinch window. False (and no write) in a lockstep match, while one is already running,
        /// or while the world is already held at (near) zero (e.g. Storm's strike): never steal the field.</summary>
        public bool TryFire(float scaleNow, bool lockstep, out float write)
        {
            write = 0f;
            if (lockstep || Running || scaleNow <= 0.001f) return false;
            Before = scaleNow > 0.001f ? scaleNow : 1f;
            Since = 0f;
            Parked = false;
            write = Written = PunchPinch.SlowScale;
            return true;
        }

        /// <summary>One frame's decision. False means: write nothing. Call only while <see cref="Running"/>.</summary>
        public bool Step(float unscaledStep, float scaleNow, bool frozen, out float write)
        {
            write = 0f;
            if (!Running) return false;
            if (frozen || scaleNow <= 0.001f)
            {
                // another writer holds the field (or is about to): wait, don't add time, don't write
                Parked = true;
                return false;
            }
            if (Parked)
            {
                // the field came back from the other writer
                Parked = false;
                if (Mathf.Abs(scaleNow - Written) <= Eps)
                {
                    // it handed our own scale back (e.g. the old Storm.Thaw): take the window back
                }
                else
                {
                    // it is not our value: that writer owns the field now, the window is not ours to continue
                    End();
                    return false;
                }
            }
            if (Mathf.Abs(scaleNow - Written) > Eps)
            {
                // somebody wrote the field under us, outside a park: drop the window, leave their write alone
                End();
                return false;
            }
            Since += unscaledStep;
            if (Since >= PunchPinch.Duration)
            {
                write = Before;   // never 1f outright: a match running faster than 1x gets its own speed back
                End();
                return true;
            }
            write = Written = PunchPinch.ScaleAt(Since);
            return true;
        }

        /// <summary>For OnDisable/OnDestroy: end the window now, handing the field back if it is still ours.</summary>
        public bool Release(float scaleNow, out float write)
        {
            write = 0f;
            bool ours = Running && Mathf.Abs(scaleNow - Written) <= Eps;
            End();
            if (!ours) return false;
            write = Before;
            return true;
        }

        /// <summary>What the scale was behind the punch: <see cref="Before"/> while running, else the scale now.</summary>
        public float Baseline(float scaleNow) => Running ? Before : scaleNow;

        void End()
        {
            Since = -1f;
            Parked = false;
            Written = -1f;
        }
    }

    /// <summary>Drives Time.timeScale through PunchPinch's curve for one window, added to the host's GameObject on
    /// first use. The decision itself lives in PunchClock; this is the thin Unity shell over it. ExecuteAlways so
    /// OnDisable/OnDestroy hand the scale back even when a test (or the editor) destroys it outside Play.</summary>
    [ExecuteAlways]
    public sealed class TimePinch : MonoBehaviour
    {
        PunchClock clock = new PunchClock { Since = -1f, Written = -1f };
        static TimePinch active;

        static TimePinch() => SceneStatics.Register(nameof(TimePinch), () => active = null);

        public static TimePinch For(GameObject host)
        {
            if (host == null) return null;
            var p = host.GetComponent<TimePinch>();
            if (p == null) p = host.AddComponent<TimePinch>();
            return p;
        }

        /// <summary>What the scale was before any running punch; the scale now when none is running. For a freeze
        /// (Storm) to read instead of the punch's own in-flight scale.</summary>
        public static float ScaleBehindThePunch(float scaleNow) => active != null ? active.clock.Baseline(scaleNow) : scaleNow;

        /// <summary>Starts one pinch window; ignored while one is already running (so a heap of many deaths in one
        /// frame still reads as a single punch) or while the match runs on lockstep (a local time-scale change there
        /// would desync the two worlds).</summary>
        public void Fire(bool lockstepMatch)
        {
            if (clock.TryFire(Time.timeScale, lockstepMatch, out var write))
            {
                Time.timeScale = write;
                active = this;
            }
        }

        void Update()
        {
            if (clock.Step(Time.unscaledDeltaTime, Time.timeScale, Time.timeScale <= 0.001f, out var write))
                Time.timeScale = write;
            if (!clock.Running) active = null;
        }

        void OnDisable() => ReleaseNow();
        void OnDestroy() => ReleaseNow();

        void ReleaseNow()
        {
            if (clock.Release(Time.timeScale, out var write)) Time.timeScale = write;
            if (active == this) active = null;
        }
    }
}
