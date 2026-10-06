// Phase: tooling (the gym's time strip, 2026-10-06) — the arithmetic behind "can the gym see a missing effect".
//
// Why. The first filming (unit-look/baseline) gave every entry one still per zoom band, three of them so far out
// that the whole small stage was a dot, and the gym flagged 0 of 350: nobody could tell "nothing was drawn" from
// "the still missed it". An effect lives for a second or two; a single frame at one moment is a coin toss.
//
// So for the tabs whose entries are EVENTS IN TIME (deaths, abilities, events, a unit's own fire) the sheet stops
// being six zooms of one moment and becomes one close zoom at five moments: a 'before' frame taken before the
// entry is staged, then four across its life. Clips and Scenes keep their bands — a clip is a pose, and a scene is
// a whole stage you want the overviews of.
//
// With a 'before' frame the gym can MEASURE: CaptureRig.Diff gives changed_frac per frame, and an entry that
// expects something drawn and never moves more pixels than an idle stage does (rain, flicker) is flagged. The
// floor is measured at run time, not guessed — Editor/Gym.cs shoots two idle pairs before the catalogue and
// writes the number into summary.json — because it depends on the weather the run is filmed in.
//
// Pure arithmetic, no Unity objects, so GymStripTests can check both ways of failing without entering Play. It
// lives in TW.Perf rather than TW.Editor because Tests/Show's asmdef references TW.Perf.
using System.Collections.Generic;

namespace TW.Perf
{
    public static class GymStrip
    {
        /// <summary>The zoom every strip frame is shot at: GymCatalogue.Bands[1] (T2, 16 m), close enough that a
        /// man is a figure and wide enough to hold a shell burst.</summary>
        public const float Zoom = 16f;

        /// <summary>Frames in a strip: the 'before' frame plus Moments().</summary>
        public const int Frames = 5;

        /// <summary>Which tabs are filmed as a strip in time rather than as a ladder of zooms.</summary>
        public static bool Strips(GymTab tab) =>
            tab == GymTab.Deaths || tab == GymTab.Abilities || tab == GymTab.Events || tab == GymTab.Units;

        /// <summary>Does this entry promise something new on screen? Only those can be flagged "nothing drawn":
        /// a Rejected ability is supposed to draw nothing, and Covered/Excluded are never photographed at all.</summary>
        public static bool Expects(GymEntry e) =>
            Strips(e.Tab) && (e.Expect == GymExpect.Fires || e.Expect == GymExpect.FactionSeat || e.Expect == GymExpect.Preview);

        /// <summary>
        /// When to photograph, in game seconds after the entry is staged: before it has got going, on the way up,
        /// at the moment the old run photographed (`life`, the gym's existing wait, so the baseline's frame is
        /// still one of these), and after it should have finished. Ascending, at least 0.25 s apart, at least four.
        /// </summary>
        public static float[] Moments(float life)
        {
            if (life < 0f) life = 0f;
            var raw = new[] { 0.12f * life, 0.45f * life, life, life + 2f };
            var outp = new float[raw.Length];
            float prev = -1f;
            for (int i = 0; i < raw.Length; i++)
            {
                float t = raw[i] < 0f ? 0f : raw[i];
                if (t < prev + 0.25f) t = prev + 0.25f;
                outp[i] = t; prev = t;
            }
            return outp;
        }

        /// <summary>
        /// How much of the frame must change before we believe something was drawn. Three times the measured noise
        /// floor (rain and flicker move pixels on an idle stage), and never under 0.4 % of the frame — a burst that
        /// touches fewer pixels than that is invisible to a player anyway.
        /// </summary>
        public static float Threshold(float floor)
        {
            if (!(floor > 0f)) floor = 0f;
            float t = 3f * floor;
            return t > 0.004f ? t : 0.004f;
        }

        /// <summary>
        /// The flag line, or null when at least one strip frame differs from the 'before' frame by more than
        /// Threshold(floor). `changed` is CaptureRig.Diff's changed_frac for each strip frame; an empty list says
        /// nothing was measured, which is not evidence of nothing drawn.
        /// </summary>
        public static string NothingDrawn(IList<float> changed, float floor)
        {
            if (changed == null || changed.Count == 0) return null;
            float best = 0f; bool any = false;
            for (int i = 0; i < changed.Count; i++)
            {
                float v = changed[i];
                if (float.IsNaN(v)) continue;
                any = true;
                if (v > best) best = v;
            }
            if (!any) return null;
            float th = Threshold(floor);
            if (best > th) return null;
            return "nothing drawn: the strip's biggest change is " + best.ToString("0.0000", System.Globalization.CultureInfo.InvariantCulture)
                 + " of the frame, under the threshold " + th.ToString("0.0000", System.Globalization.CultureInfo.InvariantCulture)
                 + " (noise floor " + floor.ToString("0.0000", System.Globalization.CultureInfo.InvariantCulture) + ")";
        }


        // ----------------------------------------------------------------- framing the subject (look-04, 2026-10-06)
        // Round 2's strips could not be trusted: Deaths/Shot.jpg held no man in any cell and Events/VehicleDestroyed
        // showed the same whole tank five times. One close zoom of the whole stage is not a picture OF the victim, so
        // the strip now frames the SUBJECT - the man, the machine, the unit - and the zoom is worked out from how tall
        // he is. The rig's frame is 1.1547 * Zoom metres tall whatever the fov (CaptureRig.Rig.Pose keeps the ground
        // distance * tan30 / tan(fov/2)), so the share of the cell a subject fills is his height / (1.1547 * Zoom),
        // foreshortened by the camera's pitch.

        /// <summary>The zoom band a strip may be shot at. ZoomMin is the tactical camera's own floor.</summary>
        public const float ZoomMin = 6f, ZoomMax = 16f;

        /// <summary>The share of the cell's height a subject should fill.</summary>
        public const float Share = 0.33f;

        /// <summary>How tall the subject stands, in metres: a man, a tank hull, a walker (the crabsplit and tanksplit
        /// bakes x VehicleSize). Guesses from the bakes, not measured - subject_height_frac in the sidecar is the
        /// proof, and a wrong one shows up in the first run.</summary>
        public static float SubjectHeight(bool vehicle, bool walker) => walker ? 6.5f : vehicle ? 4.4f : 2.0f;

        /// <summary>The zoom that makes a subject `heightM` tall fill `share` of the cell, clamped to the band.</summary>
        public static float ZoomFor(float heightM, float pitchDeg, float share = Share)
        {
            if (!(heightM > 0f)) heightM = 2f;
            if (!(share > 0f)) share = Share;
            double tan30 = System.Math.Tan(30.0 * System.Math.PI / 180.0);
            double z = heightM * System.Math.Cos(pitchDeg * System.Math.PI / 180.0) / (share * 2.0 * tan30);
            if (z < ZoomMin) z = ZoomMin;
            if (z > ZoomMax) z = ZoomMax;
            return (float)z;
        }

        /// <summary>An aircraft flies at PlaneLow, 25 m up, outside an 18 m frame: its strip is shot wide and aimed up.</summary>
        public const float FlyerZoom = 40f, FlyerAimY = 15f;

        /// <summary>The abilities whose subject is in the air.</summary>
        public static bool Flyer(GymEntry e) =>
            e.Tab == GymTab.Abilities && (e.Id == 5 || e.Id == 8 || e.Id == 10 || e.Id == 12);   // BomberRun, ReconFlight, StrafeRun, ParaDrop

        /// <summary>Is the ability's aiming disc, the selection marker and the HUD drawn in a gym shot? Only when the
        /// run asked for it (hud=1): an overlay repaints a tenth of the frame and the measurement cannot tell it from
        /// the effect it is meant to isolate.</summary>
        public static bool ShowsOverlays(GymEntry e, bool hudOption) => hudOption;

        /// <summary>
        /// The flag for an entry whose STAGING never made its event happen in the sim: a preview replayed into the
        /// effects, a victim who lived, an ability the sim refused, a machine that was neither destroyed nor set
        /// alight. Such an entry is not judged - it says nothing about what the game draws. Null when the sim did it.
        /// </summary>
        public static string NotStaged(GymEntry e, bool previewOnly, bool victimStaged, bool victimDied,
                                      bool abilityFired, bool machineChanged)
        {
            if (previewOnly) return "not staged: the event was replayed into the effects only; nothing happened in the sim";
            if (e.Tab == GymTab.Deaths && victimStaged && !victimDied) return "not staged: the victim lived, so there is no death to photograph";
            if (e.Tab == GymTab.Abilities && e.Expect != GymExpect.Rejected && !abilityFired) return "not staged: the sim refused the ability, so nothing was fired";
            if (e.Tab == GymTab.Events && !machineChanged) return "not staged: the machine was neither destroyed nor set alight in the world";
            return null;
        }

        /// <summary>The strip frames' file-name suffixes, in order, starting with the 'before' frame.</summary>
        public static readonly string[] Names = { "0before", "1start", "2peak", "3end", "4after" };
    }
}
