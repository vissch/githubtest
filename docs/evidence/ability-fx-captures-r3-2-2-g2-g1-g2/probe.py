#!/usr/bin/env python3
"""Does the effect actually show up in the frame, and is it big enough on the ground to be an effect?

Harsh critic r1 fix 3 (the owner, 2026-10-19): "the probe never finds the effect in the frame". So this probe asks
the code nothing. It reads the pixels of the stills AbilityFxStills wrote and the pixels of the no-effect plates
taken at the same pose and the same sim tick, and reports, per still, the share of pixels whose Rec.709 luma moved
by more than 0.08 - the effect's area in the frame - and how many pixels that is.

    python docs/evidence/ability-fx-captures-r3-2-2-g2-g1-g2/probe.py

--dir defaults to this script's own folder, so the bare command above works from anywhere.

TWENTY-FOUR ROWS (g2-g1-g2, this lane, 2026-10-23; the owner: "repeat smoke, barrage and splash. Each is one still,
so the floor has no spread to sit outside."). Every subject - smoke, barrage, splash and the drop - is now shot
THREE times a band, each rep its own editor run on its own sim tick, so this probe can also ask of smoke, barrage
and splash what the g2-g1 probe already asked of the drop: that a build's own three captures land on one sim tick
and do not drift against each other.

EACH SUBJECT NOW HAS ITS OWN SPREAD BAR (g2-g1-g2, this lane, 2026-10-23). Smoke, barrage and splash used to be
measured only against the ground footprint floor, a single still with no spread to sit outside. Now that each is
three captures, every row is held against max(spread bar, ground footprint): the spread bar is 0.98 x the smallest
of that subject and band's three captures of this lane's own tree, the same arithmetic and the same 2% margin the
drop already used (g2-g1: a 0.03% spread, sixty times under the margin). `floor_kind` says which of the two bars
was the stricter one for that row. Measured from this lane's own g2-g1-g2 probe.json:

    smoke    T1   r1 0.165423  r2 0.165423  r3 0.165423   smallest 0.165423  x 0.98 -> 0.1621
    smoke    O240 r1 0.032550  r2 0.032547  r3 0.032547   smallest 0.032547  x 0.98 -> 0.0318
    barrage  T1   r1 0.216957  r2 0.219558  r3 0.217415   smallest 0.216957  x 0.98 -> 0.2126
    barrage  O240 r1 0.015934  r2 0.015985  r3 0.015951   smallest 0.015934  x 0.98 -> 0.0156
    splash   T1   r1 0.090132  r2 0.090132  r3 0.090132   smallest 0.090132  x 0.98 -> 0.0883
    splash   O240 r1 0.003141  r2 0.003141  r3 0.003141   smallest 0.003141  x 0.98 -> 0.0030
    drop     T1   r1 0.136472  r2 0.136510  r3 0.136480   smallest 0.136472  x 0.98 -> 0.1337
    drop     O240 r1 0.013163  r2 0.013165  r3 0.013173   smallest 0.013163  x 0.98 -> 0.0129

On these numbers the spread bar is the stricter one in all twelve footprint-subject cases (e.g. splash T1's spread
bar 0.0883 against its 0.0709 footprint; splash O240's 0.0030 against its 0.0012), so the 400 m^2 footprint rule
stays on the page without ever deciding - it is still computed and still checked, in case a future capture shrinks
the frame enough to flip which bar wins. The drop keeps its own fixed bar, unchanged from g2-g1: its capture is a
point event whose look is not a patch of churned ground, so the footprint question is never asked of it.

The footprint floor (R0 C1, a measuring choice taken by the coordinator 2026-10-19, not the owner; the row in
docs/reference/decisions.md): a fixed share of the frame is not a fair test of a point effect, because at the
overview the same share means a feature the size of a village. So the floor is a GROUND FOOTPRINT, capped at the
old share of the frame:

    effect_area_frac >= min(0.10, 400 / frame_area_m2)   and   effect_px >= 150

400 m^2 is about 20 x 20 m: a shell's crown of water and foam, honestly. The cap keeps the close bands as strict as
they were. The pixel count stops a tiny frame passing on a share alone.

SPLASH MUST BE A BLUE COLUMN (g2-g1-g2, this lane, 2026-10-23; the owner: "splash_T1 is one blue column"). Of the
pixels the diff counts as moved, this probe reports the mean RGB shift (`shift_rgb`) and fails a splash row whose
mean blue shift is not more than twice its mean red shift. The factor is the gap between the subjects, not a number
read back out of the code: measured on this lane's own captures, splash shifts R23.5 G37.7 B71.2 at T1 (B/R 3.0) and
R23.2 G40.0 B78.1 at O240 (3.4), where the barrage shifts R71.2 G49.6 B42.1 (earth, B/R 0.6) and the smoke shifts
R38.1 G36.5 B32.9 (0.9). The check passes the splash with room and would catch a splash that came back as dust or
earth.

HOW frame_area_m2 IS MEASURED, from the still's own sidecar (fov, aspect, cam_height_m, cam_pitch), no code read:

    vh = fov / 2                          the vertical half angle
    hh = atan(aspect * tan(vh))           the horizontal half angle
    d  = cam_height_m / sin(pitch)        the slant range to the aim point at the frame's centre
    frame_area_m2 = 4 * d^2 * tan(hh) * tan(vh) / sin(pitch)

Exit 1 when a row fails, and say which.
"""
import argparse
import json
import math
import os
import sys

import numpy as np
from PIL import Image

FOOTPRINT_SUBJECTS = ["smoke", "barrage", "splash"]
BANDS = ["T1", "O240"]
REPS = ["r1", "r2", "r3"]
DROP_BAR = {"T1": 0.1337, "O240": 0.0129}   # see the docstring: 0.98 x the smallest of three captures a band
# the spread bar for the three footprint subjects: 0.98 x the smallest of that subject/band's three captures
BAR = {
    "smoke": {"T1": 0.1621, "O240": 0.0318},
    "barrage": {"T1": 0.2126, "O240": 0.0156},
    "splash": {"T1": 0.0883, "O240": 0.0030},
}
DIFF = 0.08             # how far a pixel's luma must move to count as the effect
MIN_GROUND_M2 = 400.0   # the ground an effect must cover: about 20 x 20 m
FRAC_CAP = 0.10         # never stricter than the old share-of-frame floor
MIN_PX = 150            # and never fewer changed pixels than this
MAX_POSE = 0.5
SPLASH_BLUE_FACTOR = 2.0   # a splash row's mean blue shift must be more than this many times its mean red shift
ROWS_WANTED = 24


def floor_for(area):
    """The share of the frame the effect must cover at this band. See the module docstring."""
    return min(FRAC_CAP, MIN_GROUND_M2 / area)


def frame_area_m2(side):
    """The ground area the frame covers at the scale of its centre, from the sidecar's optics. 0 when it cannot be told."""
    fov = float(side.get("fov", 0.0))
    aspect = float(side.get("aspect", 0.0))
    height = float(side.get("cam_height_m", 0.0))
    pitch = float(side.get("cam_pitch", 0.0))
    if fov <= 0.0 or aspect <= 0.0 or height <= 0.0:
        return 0.0
    sp = math.sin(math.radians(pitch))
    if sp <= 0.01:                       # a level or rising camera frames no bounded ground
        return 0.0
    vh = math.radians(fov) / 2.0
    hh = math.atan(aspect * math.tan(vh))
    d = height / sp
    return 4.0 * d * d * math.tan(hh) * math.tan(vh) / sp


def luma(path):
    """Rec.709 luma as an int16 array and its size. The matrix is the one CaptureRig uses, not PIL's default."""
    im = Image.open(path).convert("RGB").convert("L", (0.2126, 0.7152, 0.0722, 0.0))
    return np.asarray(im, dtype=np.int16), im.size[0], im.size[1]


def rgb(path):
    """The RGB pixels as an int16 array."""
    return np.asarray(Image.open(path).convert("RGB"), dtype=np.int16)


def moved(still_l, plate_l):
    """How many pixels moved by more than DIFF between the plate and the still, their share of the frame, and the mask."""
    cut = int(round(DIFF * 255.0))
    mask = np.abs(still_l - plate_l) > cut
    px = int(mask.sum())
    return px, px / float(mask.size), mask


def mean_shift_rgb(still_rgb, plate_rgb, mask):
    """The mean per-channel absolute shift of the moved pixels, or (0, 0, 0) when none moved."""
    if not mask.any():
        return (0.0, 0.0, 0.0)
    diff = np.abs(still_rgb - plate_rgb)
    means = diff[mask].mean(axis=0)
    return tuple(round(float(v), 1) for v in means)


def sidecar(path):
    side = os.path.splitext(path)[0] + ".json"
    if not os.path.exists(side):
        return {}
    with open(side, encoding="utf-8") as fh:
        return json.load(fh)


def wanted():
    """Every row: its name, its band and the stem of its PNG pair. Every subject is now three reps a band."""
    out = []
    for subject in FOOTPRINT_SUBJECTS + ["drop"]:
        for band in BANDS:
            for rep in REPS:
                out.append((subject + "_" + rep, band, "%s_%s_%s" % (subject, band, rep)))
    return out


def main():
    here = os.path.dirname(os.path.abspath(__file__))
    ap = argparse.ArgumentParser()
    ap.add_argument("--dir", default=here, help="the folder with the stills, the plates and their sidecars")
    ap.add_argument("--tick-tol", type=int, default=2,
                    help="how many sim ticks a still and its plate may be apart (0 = exactly the same tick)")
    ap.add_argument("--out", default=None, help="where probe.json goes (default <dir>/probe.json)")
    a = ap.parse_args()
    out = a.out or os.path.join(a.dir, "probe.json")

    rows, fails = [], []
    subject_ticks = {}        # base subject -> band -> {sim_tick: [reps]}: the three reps must share one tick
    for subject, band, stem in wanted():
        who = "%s %s" % (subject, band)
        still = os.path.join(a.dir, stem + ".png")
        plate = os.path.join(a.dir, stem + "_plate.png")
        if not os.path.exists(still) or not os.path.exists(plate):
            fails.append("%s: missing %s" % (who, "still" if not os.path.exists(still) else "plate"))
            continue
        sv, sw, sh = luma(still)
        pv, pw, ph = luma(plate)
        if (sw, sh) != (pw, ph):
            fails.append("%s: still is %dx%d, plate is %dx%d" % (who, sw, sh, pw, ph))
            continue
        px, frac, mask = moved(sv, pv)

        ss, sp = sidecar(still), sidecar(plate)
        tick, ptick = int(ss.get("sim_tick", -1)), int(sp.get("sim_tick", -1))
        delta = abs(tick - ptick) if tick >= 0 and ptick >= 0 else -1
        pose = float(ss.get("pose_error_m", 9.0))
        ppose = float(sp.get("pose_error_m", 9.0))
        area = frame_area_m2(ss)
        base, rep = subject.rsplit("_", 1)
        is_drop = base == "drop"
        footprint = floor_for(area) if area > 0 else None
        if is_drop:
            bar, floor_kind = DROP_BAR[band], "fixed_bar"
        elif footprint is None:
            bar, floor_kind = BAR[base][band], "spread_bar"
        elif BAR[base][band] >= footprint:
            bar, floor_kind = BAR[base][band], "spread_bar"
        else:
            bar, floor_kind = footprint, "ground_footprint"

        shift_rgb = mean_shift_rgb(rgb(still), rgb(plate), mask) if base == "splash" else None

        rows.append({"subject": subject, "band": band, "effect_area_frac": round(frac, 6),
                     "frame_area_m2": round(area, 1), "effect_px": px,
                     "effect_ground_m2": round(frac * area, 1),
                     "floor_frac": round(bar, 6) if bar is not None else None,
                     "floor_kind": floor_kind,
                     "pose_error_m": round(max(pose, ppose), 4), "sim_tick": tick, "plate_sim_tick": ptick,
                     "tick_delta": delta, "shift_rgb": list(shift_rgb) if shift_rgb is not None else None})

        subject_ticks.setdefault(base, {}).setdefault(band, {}).setdefault(tick, []).append(rep)

        if is_drop:
            if bar is not None and frac < bar:
                fails.append("%s: effect_area_frac %.6f below the fixed bar %.4f (the drop's own drawing shrank)"
                             % (who, frac, bar))
        elif area <= 0.0:
            fails.append("%s: the sidecar has no fov/aspect/cam_height_m, so the framed ground cannot be measured" % who)
        elif frac < bar:
            fails.append("%s: effect_area_frac %.4f below the %s %.4f (%.0f m^2 of ground framed; the effect covers "
                         "%.0f m^2)"
                         % (who, frac, floor_kind, bar, area, frac * area))
        if px < MIN_PX:
            fails.append("%s: only %d changed pixels, fewer than %d (nothing to see)" % (who, px, MIN_PX))
        if max(pose, ppose) >= MAX_POSE:
            fails.append("%s: pose_error_m %.3f is %.1f or more (the camera was not where it was asked to be)"
                         % (who, max(pose, ppose), MAX_POSE))
        if delta < 0:
            fails.append("%s: no sim_tick in a sidecar, so the pair cannot be shown to be the same moment" % who)
        elif delta > a.tick_tol:
            fails.append("%s: still is on tick %d, plate on %d (%d apart, tolerance %d)"
                         % (who, tick, ptick, delta, a.tick_tol))
        if shift_rgb is not None and shift_rgb[2] <= SPLASH_BLUE_FACTOR * shift_rgb[0]:
            fails.append("%s: mean blue shift %.1f is not more than %.0fx the mean red shift %.1f (not a blue column)"
                         % (who, shift_rgb[2], SPLASH_BLUE_FACTOR, shift_rgb[0]))

    # The three reps of a band are three editor runs of the same script, so they stand on the same sim tick. More than
    # one tick means the reps were blocks of one run and are not three captures of one moment.
    for base, bands in subject_ticks.items():
        for band, seen in bands.items():
            if len(seen) > 1:
                fails.append("%s %s: the three reps stand on %d sim ticks (%s), where one was wanted"
                             % (base, band, len(seen), ", ".join("%d %s" % (t, "+".join(seen[t])) for t in sorted(seen))))

    if len(rows) < ROWS_WANTED:
        fails.append("only %d rows: %d were asked for" % (len(rows), ROWS_WANTED))

    doc = {"rows": rows, "diff_threshold": DIFF, "min_ground_m2": MIN_GROUND_M2, "frac_cap": FRAC_CAP,
           "min_effect_px": MIN_PX, "drop_bar": DROP_BAR, "spread_bar": BAR,
           "splash_blue_factor": SPLASH_BLUE_FACTOR, "tick_tolerance": a.tick_tol,
           "subject_ticks": {b: {bd: sorted(seen) for bd, seen in bands.items()} for b, bands in subject_ticks.items()},
           "failures": fails}
    with open(out, "w", encoding="utf-8") as fh:
        json.dump(doc, fh, indent=2)
        fh.write("\n")

    for r in rows:
        print("%-9s %-5s frac %.6f (bar %.6f, %-16s)  px %-7d  framed %9.0f m^2  effect %7.0f m^2  pose %.3f  ticks %d/%d  rgb %s"
              % (r["subject"], r["band"], r["effect_area_frac"], r["floor_frac"], r["floor_kind"],
                 r["effect_px"], r["frame_area_m2"], r["effect_ground_m2"], r["pose_error_m"],
                 r["sim_tick"], r["plate_sim_tick"], r["shift_rgb"]))
    print("rows %d  footprint bar = min(%.2f, %.0f / frame_area_m2); drop bar T1 %.4f O240 %.4f; effect_px >= %d; "
          "splash blue > %.0fx red" % (len(rows), FRAC_CAP, MIN_GROUND_M2, DROP_BAR["T1"], DROP_BAR["O240"], MIN_PX,
                                        SPLASH_BLUE_FACTOR))
    for f in fails:
        print("FAIL " + f)
    return 1 if fails else 0


if __name__ == "__main__":
    sys.exit(main())
