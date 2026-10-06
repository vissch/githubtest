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
        /// How much of the frame must change before we believe something was drawn: the measured noise floor plus
        /// 1 % of the frame, but never more than three times the floor and never under 0.4 % of it.
        ///
        /// Why not three times the floor alone (look-08). A strip is now shot at the zoom the SUBJECT needs, and at
        /// 6 m a close frame repaints 0.2466 of itself on its own - three times that is 0.74, a line no effect on a
        /// stage can cross, so every close entry was flagged and the flag said nothing. Round 3's Deaths/Shot moved
        /// 0.2725 of the frame, clearly more than idle, and was flagged all the same. A % of the frame OVER the
        /// floor is a line that scales: at a tiny floor 3x is still the tighter of the two and nothing changes, at a
        /// close floor it is reachable - and an entry that truly draws nothing sits AT the floor and still fails.
        /// </summary>
        public static float Threshold(float floor)
        {
            if (!(floor > 0f)) floor = 0f;
            float t = 3f * floor;
            float over = floor + 0.01f;
            if (over < t) t = over;
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
        public static float ZoomFor(float heightM, float pitchDeg, float share = Share, float min = ZoomMin)
        {
            if (!(heightM > 0f)) heightM = 2f;
            if (!(share > 0f)) share = Share;
            if (!(min > 0f)) min = ZoomMin;
            double tan30 = System.Math.Tan(30.0 * System.Math.PI / 180.0);
            double z = heightM * System.Math.Cos(pitchDeg * System.Math.PI / 180.0) / (share * 2.0 * tan30);
            if (z < min) z = min;
            if (z > ZoomMax) z = ZoomMax;
            return (float)z;
        }

        // ------------------------------------------------------------- the camera's side (look-07, 2026-10-06)
        // round3/FLAGS.md said no infantryman shows a weapon and that Rifle, MG, Sniper and Officer are one
        // silhouette. Every picture it read was taken from ONE place: the Units tab spawns a man at yaw 30 and
        // every strip frame is shot at yaw 30, pitch 25 - from BEHIND him, at a third of the cell's height, with
        // his own body between the camera and the rifle in his right hand. A claim about what a man carries needs
        // the camera in front of him and beside him, and the man big enough in the frame to see it.
        //
        // So `views=front,side` adds a pass of the same five moments from each named side. The yaw a view wants is
        // a WORLD yaw (CaptureRig.Rig.YawPin is set to 0 for the shot, so shot.Yaw is absolute), worked out from the
        // man's own drawn facing: a camera at his facing + 180 looks him in the face. Pitch drops to the tactical
        // camera's own floor (8) because a view from above hides a rifle held level, and the share of the cell
        // doubles to a half, which needs a zoom near 3.4 m - below ZoomMin, hence ZoomFor's `min` and
        // CaptureRig.ZoomFloor. Std is exactly today's shot and stays the first view of every run.

        /// <summary>Which side a strip pass is shot from. Std is the historical shot: yaw 30, pitch 25, Share.</summary>
        public enum GymView { Std, Front, Side, Back }

        /// <summary>The zoom floor a close view may ask for. Half the cell's height for a 2 m man needs ~3.4 m, and
        /// ZoomMin is 6. Below ZoomMin the camera is in its own close band (CloseFov), which the player can reach
        /// only part way - an honest view of the game's own close look, but not the tactical one.</summary>
        public const float CloseZoomFloor = 2.5f;

        /// <summary>`views=front,side`. Null, empty or all-unknown gives { Std } - exactly today's run.</summary>
        public static GymView[] ParseViews(string spec)
        {
            var list = new List<GymView> { GymView.Std };
            if (!string.IsNullOrEmpty(spec))
                foreach (var part in spec.Split(','))
                {
                    var t = part.Trim().ToLowerInvariant();
                    GymView v;
                    if (t == "front") v = GymView.Front;
                    else if (t == "side") v = GymView.Side;
                    else if (t == "back") v = GymView.Back;
                    else if (t == "std" || t == "standard") v = GymView.Std;
                    else continue;
                    if (!list.Contains(v)) list.Add(v);
                }
            return list.ToArray();
        }

        /// <summary>The WORLD yaw the camera is placed at for a view of a subject facing `subjectYawDeg`, or NaN for
        /// Std (keep whatever the run already does). Front looks him in the face, Back is over his shoulder.</summary>
        public static float ViewYaw(GymView v, float subjectYawDeg)
        {
            if (v == GymView.Std) return float.NaN;
            float y = subjectYawDeg + (v == GymView.Front ? 180f : v == GymView.Side ? 90f : 0f);
            y %= 360f;
            if (y < 0f) y += 360f;
            return y;
        }

        /// <summary>A close view is shot almost level (8 is TacticalCamera.PitchMin): from 25 degrees up, a rifle
        /// held level is seen end-on and the man's helmet covers his arms.</summary>
        public static float ViewPitch(GymView v) => v == GymView.Std ? 25f : 8f;

        /// <summary>How much of the cell's height the man should fill: a half for the close views, so what he
        /// carries is bigger than a few pixels.</summary>
        public static float ViewShare(GymView v) => v == GymView.Std ? Share : 0.50f;

        /// <summary>What goes on the file-name stem. Std adds nothing, so its names never change.</summary>
        public static string ViewSuffix(GymView v) => v == GymView.Std ? "" : v.ToString().ToLowerInvariant();

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
        /// Is the cyan contact ring under a machine (TankRenderer's team-coloured disc and its rider pips) drawn in a
        /// gym shot? Only when the run asked for it (hud=1) or the entry is ABOUT the ring itself.
        ///
        /// Why (look-08). round3 flagged it on every machine - Events/Vehicle*, Units/Maw, Breaker, Pincer, Banner,
        /// Skimmer: a bright cyan bracket lying across the hull and the ground, a tenth of a close frame, drawn in
        /// the world (so Overlays' UIDocument sweep never touched it) and the loudest thing in a picture that is
        /// supposed to be of the machine. It is an instrument, like the aiming disc, so a strip hides it.
        /// </summary>
        public static bool ShowsSelection(GymEntry e, bool hudOption)
        {
            if (hudOption) return true;
            if (!Strips(e.Tab)) return true;        // a clip or a scene is the game as it is played
            return AboutSelection(e);
        }

        /// <summary>Is this entry about the ring/marker itself, so hiding it would hide the subject of the picture?</summary>
        public static bool AboutSelection(GymEntry e)
        {
            string n = e.Name;
            if (string.IsNullOrEmpty(n)) return false;
            return n.IndexOf("Select", System.StringComparison.OrdinalIgnoreCase) >= 0
                || n.IndexOf("Marker", System.StringComparison.OrdinalIgnoreCase) >= 0
                || n.IndexOf("Ring", System.StringComparison.OrdinalIgnoreCase) >= 0;
        }

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
