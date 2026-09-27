// Phase: C72 (AOSA, juice J03) - the per-shot log of an image run.
// A blind critic counting tracer births off 32 held frames could only say "medium-low": bundles of parallel tracers,
// volleys and trunks hide which streak is new. The presentation already knows every shot exactly: CombatFx gives each
// tracer its birth time (the event's frame plus ShotStagger's delay), and the held clock gives every captured frame its
// time. So in an image run (PerfBench, shot_tick) CombatFx writes each shot it shows here, and the report sorts them
// into the held frame each was first drawn on, with the shooter's side, trench line and screen start.
// Zero cost otherwise: CombatFx reads On (a static bool) once a shot, and nothing is allocated until Begin, which only
// an image run calls. Read-only: nothing here is read by the sim or by any drawing, and it draws no random numbers.
using UnityEngine;

namespace TW.Presentation
{
    public static class ShotLog
    {
        /// <summary>One shot as CombatFx showed it. Born and Arrived are Time.time values (the tracer's own Born, and the
        /// frame its event arrived on); X/Z the shooter's sim position; Garrison his TrenchId (-1 none).</summary>
        public struct Entry
        {
            public float Born, Arrived, Life;
            public uint Tick;
            public int Shooter;
            public byte Team;
            public short Garrison;
            public float X, Z;
            public Vector3 From, To;
        }

        /// <summary>True only while an image run is logging. The one thing CombatFx reads per shot otherwise.</summary>
        public static bool On;
        /// <summary>Shots of earlier ticks are not kept (whole ticks, so a kept tick has all its shots).</summary>
        public static uint FromTick;
        static Entry[] entries;
        public static int Count { get; private set; }
        /// <summary>Shots not kept because the buffer was full (the report says so).</summary>
        public static int Dropped { get; private set; }

        /// <summary>Allocates the buffer and starts logging. PerfBench calls it in image runs only.</summary>
        public static void Begin(int capacity, uint fromTick)
        {
            entries = new Entry[Mathf.Max(1, capacity)];
            Count = 0; Dropped = 0; FromTick = fromTick;
            On = true;
        }

        public static void Stop() => On = false;

        /// <summary>Stops and lets the buffer go.</summary>
        // an image run's log must not carry into the next Play in the same editor
        static ShotLog() => SceneStatics.Register(nameof(ShotLog), Clear);

        public static void Clear() { On = false; entries = null; Count = 0; Dropped = 0; FromTick = 0; }

        public static void Add(in Entry e)
        {
            if (!On || entries == null || e.Tick < FromTick) return;
            if (Count >= entries.Length) { Dropped++; return; }
            entries[Count++] = e;
        }

        public static Entry Get(int i) => entries[i];

        // ------------------------------------------------------------------------------ pure: where a shot falls
        /// <summary>BirthFrame: first drawn before frame 0 and pruned by then.</summary>
        public const int Gone = -1;
        /// <summary>BirthFrame: first drawn before frame 0 and still drawn on it (a tracer in flight at frame 0).</summary>
        public const int InFlight = -2;
        /// <summary>BirthFrame: first drawn after the last held frame.</summary>
        public const int After = -3;

        /// <summary>The held frame (0..frames-1) a tracer born at `born` is first drawn on, or Gone, InFlight or After.
        /// CombatFx draws a tracer on a frame when `now >= Born` and keeps it while `Born >= now - life` (its Prune), and
        /// its Update runs after SimHost dispatched the frame's events, so a shot shown with no delay is drawn on the frame
        /// its event arrives. frameTimes are the Time.time of each captured frame; the frame before frame 0 is heldStep
        /// earlier (the held clock).</summary>
        public static int BirthFrame(float born, float[] frameTimes, int frames, float heldStep, float life)
        {
            if (frameTimes == null || frames <= 0) return After;
            if (born > frameTimes[frames - 1]) return After;
            int k = 0;
            while (k < frames && frameTimes[k] < born) k++;
            if (k > 0) return k;
            float first = frameTimes[0];
            if (born > (float)(first - heldStep)) return 0;
            return born >= (float)(first - life) ? InFlight : Gone;
        }

        // ------------------------------------------------------------------------------ pure: which line fired it
        /// <summary>Cells of MapData.TrenchCells (ordered along the trench, 2 m nav cells) to a line section.</summary>
        public const int SectionCells = 15;
        /// <summary>Metres of the grid that sorts shooters standing outside any trench.</summary>
        public const float OpenGridMetres = 20f;

        /// <summary>The firing line a shot belongs to: side, the trench the shooter stands in and the section of it
        /// (along = his cell's index in that trench's TrenchCells), or, out of any trench, a cell of a coarse grid.
        /// "0:t3.2" is team 0, trench 3, section 2; "1:o2.11" is team 1 in the open, grid cell (2, 11).</summary>
        public static string Line(byte team, int trench, int along, float x, float z)
        {
            if (trench >= 0 && along >= 0) return team + ":t" + trench + "." + (along / SectionCells);
            return team + ":o" + Mathf.FloorToInt(x / OpenGridMetres) + "." + Mathf.FloorToInt(z / OpenGridMetres);
        }
    }
}
